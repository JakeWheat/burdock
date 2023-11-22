
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Burdock.RenameAst
    (rename
    ,getImportSources
    ,ModuleID
    ,ModuleMetadata 

    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error, show)
--import Burdock.Utils (trace)

import Data.Text (Text)
--import qualified Data.Text.Lazy as L

import qualified Burdock.Syntax as S
import Burdock.StaticError (StaticError(..))

--import qualified Burdock.Pretty as P

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..))

import qualified Burdock.Rename as R

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

getImportSources :: S.Script -> [S.ImportSource]
getImportSources (S.Script ss) =
    concatMap getImportSourceInfo $ rewriteUseContext ss
  where
    getImportSourceInfo (S.Import _ s _) = [s]
    getImportSourceInfo (S.Include _ s) = [s]
    getImportSourceInfo (S.ImportFrom _ s _) = [s]
    getImportSourceInfo _x = []

------------------------------------------------------------------------------

-- todo: fix this so there's one place in the source that says the
-- name of the default use context
-- and support arbitrary use context names
rewriteUseContext :: [S.Stmt] -> [S.Stmt]
rewriteUseContext = \case
    [] -> importInterpreter : includeBurdock2023 : []
    (S.UseContext _ (S.ImportName ["_bootstrap-interpreter"]) : ss) -> ss
    (S.UseContext _ (S.ImportName ["_base"]) : ss) -> importInterpreter : ss
    (S.UseContext _ (S.ImportName ["burdock2023"]) : ss) -> importInterpreter : includeBurdock2023 : ss
    ss -> importInterpreter : includeBurdock2023 : ss
  where
    importInterpreter = S.Import n (S.ImportSpecial "haskell" ["_interpreter"]) "_interpreter"
    includeBurdock2023 = S.Include n (S.ImportSpecial "haskell" ["burdock2023"])
    n = Nothing

------------------------------------------------------------------------------

type Renamer = ReaderT R.RenamerEnv (Writer [StaticError])

runRenamer :: Text
           ->  [(S.ImportSource, ModuleID)]
           -> [(ModuleID, ModuleMetadata)]
           -> Renamer a
           -> (a, [StaticError])
runRenamer modID isCtx ctx f = runWriter $ flip runReaderT (R.makeRenamerEnv modID isCtx ctx) f

rename :: Text
       -> [(S.ImportSource, ModuleID)]
       -> [(ModuleID, ModuleMetadata)]
       -> S.Script
       -> Either [StaticError] (ModuleMetadata, S.Script)
rename modID is ctx (S.Script stmts) = 
    errToEither $ runRenamer modID is ctx $ do
        (re, stmts') <- rewritePreludeStmts $ rewriteUseContext stmts
        (mm,ret) <- liftErrs $ R.applyProvides re
        let rscr = case ret of
                       Nothing -> stmts'
                       Just rs -> stmts' ++ [makeModuleValue rs]
        pure (mm, S.Script rscr)
  where
    makeModuleValue rs =
        let n = Nothing
            r = flip map rs $ \(as,b) -> (b, toIdenExpr $ map (n,) as)
        in S.StmtExpr n $ S.App n (S.DotExpr n (S.Iden n "_interpreter") "make-module") [S.RecordSel n r]

errToEither :: (a, [StaticError]) -> Either [StaticError] a
errToEither (a, []) = Right a
errToEither (_, e) = Left e

liftErrs :: ([StaticError], a) -> Renamer a
liftErrs (es, a) = tell es >> pure a

callWithEnv :: (R.RenamerEnv -> ([StaticError], b)) -> Renamer b
callWithEnv f = liftErrs =<< f <$> ask

------------------------------------------------------------------------------

rewritePreludeStmts :: [S.Stmt] -> Renamer (R.RenamerEnv, [S.Stmt])

rewritePreludeStmts (S.Provide sp pis : ss) = do
    ctx <- callWithEnv $ R.provide sp pis
    local (const ctx) (rewritePreludeStmts ss)

rewritePreludeStmts (S.Import sp is al : ss) = do
    ctx <- callWithEnv $ R.bImport sp is al
    local (const ctx) (rewritePreludeStmts ss)

rewritePreludeStmts (S.IncludeFrom sp al pis : ss) = do
    ctx <- callWithEnv $ R.includeFrom sp al pis
    local (const ctx) (rewritePreludeStmts ss)

rewritePreludeStmts (S.Include sp is : ss) = do
    ctx <- callWithEnv $ R.include sp is
    local (const ctx) (rewritePreludeStmts ss)

rewritePreludeStmts (S.ImportFrom sp is pis : ss) = do
    ctx <- callWithEnv $ R.importFrom sp is pis
    local (const ctx) (rewritePreludeStmts ss)

rewritePreludeStmts (S.ProvideFrom sp al pis : ss) = do
    ctx <- callWithEnv $ R.provideFrom sp al pis
    local (const ctx) (rewritePreludeStmts ss)

-- non prelude statement - output the desugared imports then
-- go to regular processing
rewritePreludeStmts ss = do
    (re, lms) <- callWithEnv R.queryLoadModules
    local (const re) $ do
        let f (ModuleID nm as) alias = S.Import Nothing (S.ImportSpecial nm as) alias
        second (map (uncurry f) lms ++) <$> rewriteStmts ss

------------------------------------------------------------------------------

rewriteStmts :: [S.Stmt] -> Renamer (R.RenamerEnv, [S.Stmt])
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
    ctx <- callWithEnv $ R.createVar (sh == S.Shadow) sp' nm
    e' <- rewriteExpr e
    ann' <- maybe (pure Nothing) (\a -> Just <$> rewriteAnn a) ann
    let st = S.VarDecl sp (S.SimpleBinding sp' sh nm ann') e'
    second (st:) <$> local (const ctx) (rewriteStmts ss)

{-

data decl, getting the scoping right
in the bodies of the variant definitions
and in any method definitions
all the variants, the type, and the is-type and is-variant functions
are in scope
I don't think anything else needs to be done

-}
rewriteStmts (S.DataDecl sp nm ps vs ms whr : ss) = do

    -- todo: check the type parameters - they should not conflict
    -- with in scope types, but will automatically shadow any other
    -- name such as identifier or module
    -- they go into scope for checking the bits of the data decl

    -- create entries for: type, variant names, is-type, is-variant
    let vs' = flip map vs $ \(S.VariantDecl _sp vnm _ _) -> (sp,vnm)
    ctx <- callWithEnv $ R.createType sp nm (length ps) vs'
    local (const ctx) $ do
        let isBs = (False, sp,"is-" <> nm) : flip map vs' (\(vsp,vnm) -> (False, vsp,"is-" <> vnm))
        ctx1 <- callWithEnv $ R.createBindings isBs
        -- rename the components
        -- todo: check for name conflicts
        local (const ctx1) $ do
            ms' <- flip mapM ms $ \(mnm,m) -> (mnm,) <$> rewriteMethod m
            whr' <- case whr of
                Nothing -> pure Nothing
                Just bdy -> Just . snd <$> rewriteStmts bdy
            let st = S.DataDecl sp nm ps vs ms' whr'
            second (st:) <$> rewriteStmts ss

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
            ctx <- callWithEnv $ R.createBinding False sp nm
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
    getRecs ssall@(s1:ss1) | Just s1' <- getRec s1 = first (s1' :) $ getRecs ss1
                     | otherwise = ([],ssall)
    getRecs [] = ([],[])

-- todo: typestmt
--   adds a new name
--   checks the type parameters are in scope?
-- todo: contract - rename the ann
-- todo: ffitypestmt - this will probably change

rewriteStmts (S.SetVar sp tgt e : ss)
    | Just tgt' <- getIdenList tgt = do
          tgt'' <- callWithEnv $ R.renameAssign sp $ map snd tgt'
          e' <- rewriteExpr e
          let st = S.SetVar sp (toIdenExpr $ map (sp,) tgt'') e'
          second (st:) <$> rewriteStmts ss
      -- todo: fix this error to use tell and return the original
    | otherwise = error $ "bad target of assign " <> show tgt

-- todo: setref, rename the expressions

rewriteStmts (S.Check sp mnm bdy : ss) = do
    bdy' <- snd <$> rewriteStmts bdy
    let s = S.Check sp mnm bdy'
    second (s:) <$> rewriteStmts ss

-- provide from - returning to this later, along with module provide items
-- use package: will probably change

rewriteStmts (s:_) = error $ "unsupported stmt syntax " <> show s

------------------------------------------------------------------------------

rewriteExpr :: S.Expr -> Renamer S.Expr
rewriteExpr x@(S.Num{}) = pure x

rewriteExpr x@(S.Text{}) = pure x

-- covers iden and iden only dotexprs
rewriteExpr e | Just is <- getIdenList e = do
    nis <- callWithEnv $ R.renameIdentifier is
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
rewriteExpr (S.Let sp bs bdy) = do
    let dob acc [] = do
            bdy' <- snd <$> rewriteStmts bdy
            pure $ S.Let sp (reverse acc) bdy'
        dob acc ((b,e):bs') = do
            (b',rn) <- rewriteBinding b
            e' <- rewriteExpr e
            rn $ dob ((b',e'):acc) bs'
    dob [] bs
-- todo: letrec

rewriteExpr (S.Block sp ss) =
    (S.Block sp . snd) <$> rewriteStmts ss

-- make sure this comes after the getidens version above
rewriteExpr (S.DotExpr sp e nm) =
    (\e1 -> S.DotExpr sp e1 nm) <$> rewriteExpr e
    

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
rewriteExpr (S.RecordSel sp fs) =
    S.RecordSel sp <$> mapM rf fs
  where
    rf (nm,e) = (nm,) <$> rewriteExpr e

-- todo: extend
-- todo: tablesel
-- todo: tupleget
rewriteExpr (S.TupleGet sp e i) =
    (\e' -> S.TupleGet sp e' i) <$> rewriteExpr e

rewriteExpr (S.Construct sp nm es) = do
    nm' <- callWithEnv $ R.renameIdentifier $ map (sp,) nm
    S.Construct sp (map snd nm') <$> mapM rewriteExpr es

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

------------------------------------------------------------------------------

rewriteMethod :: S.Method -> Renamer S.Method
rewriteMethod (S.Method fh bdy) = do
    (fh',rn) <- rewriteHeader fh
    rn $ do
        bdy' <- snd <$> rewriteStmts bdy
        pure (S.Method fh' bdy')

rewriteHeader :: S.FunHeader -> Renamer (S.FunHeader, Renamer a -> Renamer a)
rewriteHeader (S.FunHeader ps bs ma) = do
    (bs', rn) <- rewriteBindings bs
    pure (S.FunHeader ps bs' ma, rn)

rewriteAnn :: S.Ann -> Renamer S.Ann
rewriteAnn (S.TName tsp tnm) = do
    (_,nt) <- callWithEnv $ R.renameType tsp tnm
    pure (S.TName tsp nt)
-- todo: the rest of the Ann variations
-- I think there are two contexts: one where unrecognised type names
--   are treated as introduced type parameters
--   and one where the local type parameters have to have already
--   been declared in the local env
rewriteAnn x = error $ "unsupported ann syntax: " <> show x

------------------------------------------------------------------------------

rewriteBinding :: S.Binding -> Renamer (S.Binding, Renamer a -> Renamer a)
rewriteBinding = \case
    S.NameBinding sp nm -> do
        p1 <- callWithEnv $ R.renameBinding (R.NameBinding sp nm)
        case p1 of
            R.NameBinding {} -> do
                let f1 f = do
                        ctx <- callWithEnv (R.createBinding False sp nm)
                        local (const ctx) f
                pure (S.NameBinding sp nm, f1)
            R.VariantBinding _ nmx _ -> do
                pure (S.VariantBinding sp nmx [], id)
    b@(S.ShadowBinding sp nm) -> do
        let f1 f = do
                ctx <- callWithEnv (R.createBinding True sp nm)
                local (const ctx) f
        pure (b, f1)
    b@(S.VariantBinding sp nm bs) -> do
        p1 <- callWithEnv $ R.renameBinding (R.VariantBinding sp nm (length bs))
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
                ctx <- callWithEnv (R.createBinding (sh == S.Shadow) sp nm)
                local (const ctx) f
        pure (S.AsBinding sp b'' sh nm, f1)
    b@(S.WildcardBinding {}) -> pure (b, id)
    b@(S.NumberLitBinding {}) -> pure (b, id)
    b@(S.StringLitBinding {}) -> pure (b, id)

-- this renames the bindings "in parallel", so they don't see each other
-- (but should conflict if they shadow)
-- then applies the bindings in serial
-- maybe there are situations where you want to rename in serial too?
rewriteBindings :: [S.Binding] -> Renamer ([S.Binding], Renamer a -> Renamer a)
rewriteBindings [] = pure ([], id)
rewriteBindings (b:bs) = do
    (b',rn) <- rewriteBinding b
    (bs',rn') <- rewriteBindings bs
    pure (b':bs', rn' . rn)

------------------------------------------------------------------------------

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
