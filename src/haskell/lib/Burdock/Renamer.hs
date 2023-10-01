
{-

Renamer is the first pass on the syntax, it's parsed syntax ast ->
parsed syntax ast. After than, the syntax is desugared further and
converted to interpretersyntax

It will process and check prelude statements

it directs how the prelude statements to be desugared to load-module
and the module value at the end which represents the provided
things. All the prelude statements are removed in this pass

Every referenced module gets a single unique "local canonical name"

Every reference to an imported identifier in the module source is
changed to use this canonical name - whether or not it is qualified
in the original source. This includes types and variant patterns.

Every no arg variant in a pattern match is disambiguated from a
pattern binding

TODO: maybe every use of a variable in an expression is rewritten to
  explicitly derefence the variable

TODO: this code could also disambiguate qualified names from record
field access. Not sure if it's worth it with the current implementation,
qualified names during interpretation are implemented as record field
access

TODO: this code instead of just doing load-module, could do explicit
  lists of what it needs from each module, then the loaded value
  would only have these and nothing else, to reduce unneeded pointers

Something it doesn't currently do is rewrite bindings to be unique within a
module - even when shadowing happens

The code also performs the following static checks:

checks there are no inconsistencies (such as multiple definitions for
the same binding) or references to unknown things in the prelude
statements

checks every identifier is recognised - including locally defined ones

checks that shadow is used when needed

checks the number of args in variant pattern matches (will move to the
type checker when that exists, it's trivial to do now and a small
benefit)

checks any attempt to assign is to a var binding

At the end, the user code will get the information to create the
desugar provides runtime module value, and the overall module metadata
which is passed when desugaring modules which import the module that
has just been renamed

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Burdock.Renamer
    (StaticError(..)
    ,prettyStaticError
    ,prettyStaticErrors

    ,renameModule
    ,renameScript

    ,ModuleMetadata
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

import Burdock.RenamerEnv
    (StaticError(..)
    ,prettyStaticError
    ,prettyStaticErrors

    ,ModuleMetadata
    ,RenamerEnv
    ,makeRenamerEnv

    ,provide
    ,provideFrom
    ,bImport
    ,include
    ,includeFrom
    ,importFrom
    ,queryLoadModules
    ,applyProvides

    ,createBinding
    ,createType
    ,createVar 
    
    ,renameIdentifier
    ,renameType
    ,renameBinding
    ,renameAssign
    )

import qualified Burdock.RenamerEnv as R

import qualified Burdock.Syntax as S

import Data.Text (Text)

import Control.Monad.Reader
    (ReaderT
    ,runReaderT
    )

import Control.Monad.Reader.Class
    (ask
    ,local
    )

import Control.Monad.Writer
    (Writer
    ,runWriter
    ,tell
    )

import Control.Arrow (first, second)

------------------------------------------------------------------------------

type Renamer = ReaderT RenamerEnv (Writer [StaticError])

renameModule :: ModuleMetadata -> [(Text, ModuleMetadata)] -> S.Script -> Either [StaticError] (ModuleMetadata, S.Script)
renameModule tmpHack ctx (S.Script stmts) =
    errToEither $ runRenamer tmpHack ctx $ do
        (re, stmts') <- rewritePreludeStmts stmts
        -- get the module value info to create the make-module last statement
        rs <- liftErrs $ applyProvides re
        let ls = makeModuleValue $ snd rs
        -- return the final module metadata along with the renamed source
        pure ((fst rs, S.Script (stmts' ++ [ls])))
  where
    makeModuleValue rs =
        let n = Nothing
            r = flip map rs $ \(as,b) -> (b, toIdenExpr $ map (n,) as)
        in S.StmtExpr n $ S.App n (S.Iden n "make-module") [S.RecordSel n r]

renameScript :: ModuleMetadata
             -> [(Text, ModuleMetadata)]
             -> S.Script
             -> Either [StaticError] (ModuleMetadata, S.Script)
renameScript tmpHack ctx (S.Script stmts) =
    errToEither $ runRenamer tmpHack ctx $ do
        (re, stmts') <- rewritePreludeStmts stmts
        -- todo: use a new function which puts in a default provide all
        -- the motivation to return module metadata from this is for a
        -- user of this function to get info on the top level bindings in this script
        rs <- liftErrs $ applyProvides re
        pure ((fst rs, S.Script stmts'))
    
    -- errToEither $ runRenamer tmpHack ctx (S.Script . snd <$> rewritePreludeStmts stmts)

-- tmpHack used to shoehorn in definitions from pre module initRuntime, bootstrap and internals hacks
runRenamer :: ModuleMetadata -> [(Text, ModuleMetadata)] -> Renamer a -> (a, [StaticError])
runRenamer tmpHack ctx f = runWriter $ flip runReaderT (makeRenamerEnv tmpHack ctx) f

callWithEnv :: (RenamerEnv -> ([StaticError], b)) -> Renamer b
callWithEnv f = liftErrs =<< f <$> ask

liftErrs :: ([StaticError], a) -> Renamer a
liftErrs (es, a) = tell es >> pure a

errToEither :: (a, [StaticError]) -> Either [StaticError] a
errToEither (a, []) = Right a
errToEither (_, e) = Left e

------------------------------------------------------------------------------

rewritePreludeStmts :: [S.Stmt] -> Renamer (RenamerEnv, [S.Stmt])

-- catch prelude statements here

rewritePreludeStmts (S.Provide sp pis : ss) = do
    ctx <- callWithEnv $ provide sp pis
    local (const ctx) (rewritePreludeStmts ss)

rewritePreludeStmts (S.Import sp (S.ImportSpecial "file" [nm]) al : ss) = do
    ctx <- callWithEnv $ bImport sp nm al
    local (const ctx) (rewritePreludeStmts ss)

rewritePreludeStmts (S.IncludeFrom sp al pis : ss) = do
    ctx <- callWithEnv $ includeFrom sp al pis
    local (const ctx) (rewritePreludeStmts ss)

rewritePreludeStmts (S.Include sp (S.ImportSpecial "file" [nm]) : ss) = do
    ctx <- callWithEnv $ include sp nm
    local (const ctx) (rewritePreludeStmts ss)

rewritePreludeStmts (S.ImportFrom sp (S.ImportSpecial "file" [nm]) pis : ss) = do
    ctx <- callWithEnv $ importFrom sp nm pis
    local (const ctx) (rewritePreludeStmts ss)

rewritePreludeStmts (S.ProvideFrom sp al pis : ss) = do
    ctx <- callWithEnv $ provideFrom sp al pis
    local (const ctx) (rewritePreludeStmts ss)

-- not a prelude statement? fall through to the regular statement handling
-- after outputting the needed load-modules

rewritePreludeStmts ss = do
    lms <- callWithEnv queryLoadModules
    second (map (uncurry mlm) lms ++) <$> rewriteStmts ss
  where
    mlm nm al = S.LetDecl n (S.NameBinding n al) (S.App n (S.Iden n "load-module") [S.Text n nm])
    n = Nothing

---------------------------------------

rewriteStmts :: [S.Stmt] -> Renamer (RenamerEnv, [S.Stmt])
rewriteStmts [] = (,[]) <$> ask

rewriteStmts (S.StmtExpr sp e : ss) = do
    e' <- rewriteExpr e
    second (S.StmtExpr sp e':) <$> rewriteStmts ss

rewriteStmts (S.When sp e bdy : ss) = do
    e' <- rewriteExpr e
    bdy' <- snd <$> rewriteStmts bdy
    second (S.When sp e' bdy' :) <$> rewriteStmts ss

rewriteStmts (S.LetDecl sp b e : ss) = do
    (b', rn) <- rewriteBinding b
    e' <- rewriteExpr e
    let st = S.LetDecl sp b' e'
    second (st:) <$> rn (rewriteStmts ss)

rewriteStmts (S.VarDecl sp (S.SimpleBinding sp' sh nm ann) e : ss) = do
    ctx <- callWithEnv $ createVar (sh == S.Shadow) sp' nm
    e' <- rewriteExpr e
    ann' <- maybe (pure Nothing) (\a -> Just <$> rewriteAnn a) ann
    let st = S.VarDecl sp (S.SimpleBinding sp' sh nm ann') e'
    second (st:) <$> local (const ctx) (rewriteStmts ss)

rewriteStmts (s@(S.DataDecl sp nm ps vs _ _) : ss) = do
    -- todo: check the type parameters - they go into scope
    --   and if other parts of the decl use type params
    --   some of them need to match or they are unrecognised identifier
    -- rename the variantdecls: check for name conflicts
    --    simple bindings: name conflicts don't apply
    --      except with each other in the same variantdecl
    --    rename the maybe ann
    -- rename the methods: check for name conflicts
    --   make them recursive too
    -- rename the test block
    let vs' = flip map vs $ \(S.VariantDecl _sp vnm _ _) -> (sp,vnm)
    ctx <- callWithEnv $ createType sp nm (length ps) vs'
    second (s:) <$> local (const ctx) (rewriteStmts ss)

{-
renaming mutually recursive decls:
gather all adjacent rec and fun decls
create the bindings for all their names
then do each of the bodies in turn
the only slight issue, is you might prefer to have a redefinition error
on the second lexical occurance of a binding, in this system, when
a function arg conflicts with a later function name, you'll get this
order reversed. Could do a hack to patch this up

todo: there must be a much cleaner way to implement this function
-}
rewriteStmts ss | (rs,ss') <- getRecs ss
                , not (null rs) = do
    let doBs [] = doDs (map snd rs)
        doBs ((sp,nm):bs) = do
            ctx <- callWithEnv $ createBinding False sp nm
            local (const ctx) $ doBs bs
        doDs [] = rewriteStmts ss'
        doDs (d:ds) = do
            st <- rewriteFunRec d
            second (st:) <$> doDs ds
    doBs (map fst rs)
  where
    rewriteFunRec (S.FunDecl sp nm fh mdoc bdy tsts) = do
        -- todo: rename the type in the nm
        -- rename the test block
        (fh', rn) <- rewriteHeader fh
        -- check the body with the function name in scope
        rn $ do
            bdy' <- snd <$> rewriteStmts bdy
            pure (S.FunDecl sp nm fh' mdoc bdy' tsts)
    rewriteFunRec (S.RecDecl sp (S.NameBinding fsp fnm) e) = do
        e' <- rewriteExpr e
        pure $ S.RecDecl sp (S.NameBinding fsp fnm) e'
    rewriteFunRec s = error $ "internal error: wrong kind of statement: " <> show s
    getRec st@(S.FunDecl _ (S.SimpleBinding fsp _ fnm _) _ _ _ _) = Just ((fsp,fnm),st)
    -- todo: maybe need to match some other kinds of names in a rec?
    -- possibly typed? definitely shadow
    getRec st@(S.RecDecl _ (S.NameBinding fsp fnm) _) = Just ((fsp,fnm),st)
    getRec _ = Nothing
    --getRecs :: [S.Stmt] -> ([((S.SourcePosition, Text),S.Stmt)], [S.Stmt])
    getRecs (s1:ss1) | Just s1' <- getRec s1 = first (s1' :) $ getRecs ss1
                     | otherwise = ([],ss1)
    getRecs [] = ([],[])

-- todo: typestmt
--   adds a new name
--   checks the type parameters are in scope?
-- todo: contract - rename the ann
-- todo: ffitypestmt - this will probably change

rewriteStmts (S.SetVar sp tgt e : ss)
    | Just tgt' <- getIdenList tgt = do
          tgt'' <- callWithEnv $ renameAssign sp $ map snd tgt'
          e' <- rewriteExpr e
          let st = S.SetVar sp (toIdenExpr $ map (sp,) tgt'') e'
          second (st:) <$> rewriteStmts ss
      -- todo: fix this error to use tell and return the original
    | otherwise = error $ "bad target of assign " <> show tgt

-- todo: setref, rename the expressions
-- check: rename the body
-- provide from - returning to this later, along with module provide items
-- use package: will probably change

rewriteStmts (s:_) = error $ "unsupported stmt syntax " <> show s

---------------------------------------

rewriteExpr :: S.Expr -> Renamer S.Expr

rewriteExpr x@(S.Num{}) = pure x

rewriteExpr x@(S.Text{}) = pure x

-- covers iden and iden only dotexprs
rewriteExpr e | Just is <- getIdenList e = do
    nis <- callWithEnv $ renameIdentifier is
    pure $ toIdenExpr nis

rewriteExpr (S.Parens sp e) = S.Parens sp <$> rewriteExpr e

rewriteExpr (S.If sp ts els) = do
    ts' <- mapM rewriteB ts
    els' <- case els of
        Nothing -> pure Nothing
        Just eb -> Just <$> (snd <$> rewriteStmts eb)
    pure $ S.If sp ts' els'
  where
    rewriteB (e,bdy) = (,) <$> rewriteExpr e <*> (snd <$> rewriteStmts bdy)
        
-- todo: ask

rewriteExpr (S.App sp f as) = do
    f' <- rewriteExpr f
    as' <- mapM rewriteExpr as
    pure $ S.App sp f' as'

-- todo: instexpr

-- because this is later desugared to e0.op'(e1)
-- op cannot be checked until a type checker is implemented
-- but op will partially be checked in the later desugaring statically
-- if it matches a possible operator
rewriteExpr (S.BinOp sp e0 op e1) = do
    e0' <- rewriteExpr e0
    e1' <- rewriteExpr e1
    pure $ S.BinOp sp e0' op e1'

rewriteExpr (S.UnaryMinus sp e) = S.UnaryMinus sp <$> rewriteExpr e

rewriteExpr (S.Lam sp fh bdy) = do
    (fh', rn) <- rewriteHeader fh
    rn $ do
        bdy' <- snd <$> rewriteStmts bdy
        pure (S.Lam sp fh' bdy')

-- todo: curly lam

-- todo: let
-- todo: letrec

rewriteExpr (S.Block sp ss) =
    (S.Block sp . snd) <$> rewriteStmts ss

-- todo: general dotexpr

rewriteExpr (S.Cases sp e ma ts el) = do
    e' <- rewriteExpr e
    ma' <- maybe (pure Nothing) (\a -> Just <$> rewriteAnn a) ma
    ts' <- mapM rewriteThen ts
    el' <- case el of
        Nothing -> pure Nothing
        Just elx -> Just . snd <$> rewriteStmts elx
    pure $ S.Cases sp e' ma' ts' el'
  where
    rewriteThen (bm, wh, bdy) = do
        (bm', rn) <- rewriteBinding bm
        rn $ do
            wh' <- case wh of
                Nothing -> pure wh
                Just whx -> Just <$> rewriteExpr whx
            bdy' <- snd <$> rewriteStmts bdy
            pure (bm', wh', bdy')

rewriteExpr (S.TupleSel sp es) = S.TupleSel sp <$> mapM rewriteExpr es
    
-- todo: recordsel
-- todo: extend
-- todo: tablesel
-- todo: tupleget
-- todo: construct

rewriteExpr (S.AssertTypeCompat sp e ann) = do
    ann' <- rewriteAnn ann
    pure $ S.AssertTypeCompat sp e ann'

-- todo: typelet

rewriteExpr x@(S.Template{}) = pure x

-- todo: unboxref
-- todo: receive

-- todo: for
rewriteExpr (S.MethodExpr sp m) = S.MethodExpr sp <$> rewriteMethod m

rewriteExpr e = error $ "unsupported expr syntax: " <> show e

rewriteMethod :: S.Method -> Renamer S.Method
rewriteMethod (S.Method fh bdy) = do
    (fh',rn) <- rewriteHeader fh
    rn $ do
        bdy' <- snd <$> rewriteStmts bdy
        pure (S.Method fh' bdy')

-- todo: rewrite ps (types), ma (ann)
-- where does functionheader appear? when does it need to check or introduce
--   type names
rewriteHeader :: S.FunHeader -> Renamer (S.FunHeader, Renamer a -> Renamer a)
rewriteHeader (S.FunHeader ps bs ma) = do
    (bs', rn) <- rewriteBindings bs
    pure (S.FunHeader ps bs' ma, rn)

rewriteAnn :: S.Ann -> Renamer S.Ann
rewriteAnn (S.TName tsp tnm) = do
    (_,nt) <- callWithEnv $ renameType tsp tnm
    pure (S.TName tsp nt)
-- todo: the rest of the Ann variations
-- I think there are two contexts: one where unrecognised type names
--   are treated as introduced type parameters
--   and one where the local type parameters have to have already
--   been declared in the local env
rewriteAnn x = error $ "unsupported ann syntax: " <> show x

rewriteBinding :: S.Binding -> Renamer (S.Binding, Renamer a -> Renamer a)
rewriteBinding = \case
    S.NameBinding sp nm -> do
        p1 <- callWithEnv $ renameBinding (R.NameBinding sp nm)
        case p1 of
            R.NameBinding {} -> do
                let f1 f = do
                        ctx <- callWithEnv (createBinding False sp nm)
                        local (const ctx) f
                pure (S.NameBinding sp nm, f1)
            R.VariantBinding _ nmx _ -> do
                pure (S.VariantBinding sp nmx [], id)
    b@(S.ShadowBinding sp nm) -> do
        let f1 f = do
                ctx <- callWithEnv (createBinding True sp nm)
                local (const ctx) f
        pure (b, f1)
    b@(S.VariantBinding sp nm bs) -> do
        p1 <- callWithEnv $ renameBinding (R.VariantBinding sp nm (length bs))
        case p1 of
            R.NameBinding {} -> do
                tell [UnrecognisedIdentifier sp nm]
                pure (b,id)
            R.VariantBinding _ nmx _ -> do
                (bs',rn) <- rewriteBindings bs
                -- todo: recurse on the bs
                pure (S.VariantBinding sp nmx bs', rn)
    S.TypedBinding sp b' a -> do
        a' <- rewriteAnn a
        (b'',rn) <- rewriteBinding b'
        pure (S.TypedBinding sp b'' a', rn)
    S.TupleBinding sp bs -> do
        (bs',rn) <- rewriteBindings bs
        pure (S.TupleBinding sp bs', rn)
    S.AsBinding sp b' sh nm -> do
        (b'',rn) <- rewriteBinding b'
        let f1 f = rn $ do
                ctx <- callWithEnv (createBinding (sh == S.Shadow) sp nm)
                local (const ctx) f
        pure (S.AsBinding sp b'' sh nm, f1)
    b@(S.WildcardBinding {}) -> pure (b, id)
    b@(S.NumberLitBinding {}) -> pure (b, id)
    b@(S.StringLitBinding {}) -> pure (b, id)

-- this renames the bindings "in parallel", so they don't see each other
-- then applies the bindings in serial
-- maybe there are situations where you want to rename in serial too?
rewriteBindings :: [S.Binding] -> Renamer ([S.Binding], Renamer a -> Renamer a)
rewriteBindings [] = pure ([], id)
rewriteBindings (b:bs) = do
    (b',rn) <- rewriteBinding b
    (bs',rn') <- rewriteBindings bs
    pure (b':bs', rn' . rn)

---------------------------------------

-- quick functions to convert sequences of dotexpr into [(sourcepos,Text)]
-- and vice versa

getIdenList :: S.Expr -> Maybe [(S.SourcePosition, Text)]
getIdenList (S.Iden sp n) = Just [(sp,n)]
getIdenList (S.DotExpr sp' e n) | Just is <- getIdenList e
    = Just $ is ++ [(sp',n)]
getIdenList _ = Nothing

toIdenExpr :: [(S.SourcePosition, Text)] -> S.Expr
toIdenExpr xs = toIdenList' $ reverse xs
  where
    toIdenList' [] = error $ "empty iden list"
    toIdenList' [(sp, n)] = S.Iden sp n
    toIdenList' ((sp', n') : ys) = S.DotExpr sp' (toIdenList' ys) n'
