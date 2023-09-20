
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
module Burdock.Renamer
    (StaticError(..)
    ,renameModule
    ,renameScript
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

import Burdock.RenamerEnv
    (StaticError(..)

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

    ,addLocalBinding
    ,renameIdentifier
    ,renameType
    ,renamePattern
    ,renameAssign
    )


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

import Control.Arrow (second)

------------------------------------------------------------------------------

type Renamer = ReaderT RenamerEnv (Writer [StaticError])

renameModule :: [(Text, ModuleMetadata)] -> S.Script -> Either [StaticError] (ModuleMetadata, S.Script)
renameModule ctx (S.Script stmts) =
    errToEither $ runRenamer ctx $ do
            (re, stmts') <- rewritePreludeStmts stmts
            -- get the module value info to create the make-module last statement
            rs <- wrapIt $ applyProvides re
            let ls = makeModuleValue $ snd rs
            -- return the final module metadata along with the renamed source
            pure ((fst rs, S.Script (stmts' ++ [ls])))
  where
    makeModuleValue rs =
        let n = Nothing
            r = flip map rs $ \(a,b) -> (b, S.Iden n a)
        in S.StmtExpr n $ S.App n (S.Iden n "make-module") [S.RecordSel n r]


renameScript :: [(Text, ModuleMetadata)] -> S.Script -> Either [StaticError] S.Script
renameScript ctx (S.Script stmts) =
    errToEither $ runRenamer ctx (S.Script . snd <$> rewritePreludeStmts stmts)

runRenamer :: [(Text, ModuleMetadata)] -> Renamer a -> (a, [StaticError])
runRenamer ctx f = runWriter $ flip runReaderT (makeRenamerEnv ctx) f

wrapIt :: ([StaticError], a) -> Renamer a
wrapIt (es, a) = tell es >> pure a

errToEither :: (a, [StaticError]) -> Either [StaticError] a
errToEither (a, []) = Right a
errToEither (_, e) = Left e

------------------------------------------------------------------------------

rewritePreludeStmts :: [S.Stmt] -> Renamer (RenamerEnv, [S.Stmt])

-- catch prelude statements here

-- not a prelude statement? fall through to the regular statement handling

rewritePreludeStmts ss = rewriteStmts ss

---------------------------------------

rewriteStmts :: [S.Stmt] -> Renamer (RenamerEnv, [S.Stmt])
rewriteStmts [] = (,[]) <$> ask

rewriteStmts (S.StmtExpr sp e : ss) = do
    e' <- rewriteExpr e
    second (S.StmtExpr sp e':) <$> rewriteStmts ss

rewriteStmts (st@(S.LetDecl _ (S.NameBinding sp nm) _) : ss) = do
    ctx <- wrapIt =<< addLocalBinding False (sp,nm) <$> ask
    second (st:) <$> local (const ctx) (rewriteStmts ss)
rewriteStmts (s:_) = error $ "unsupported syntax " <> show s

---------------------------------------

rewriteExpr :: S.Expr -> Renamer S.Expr
rewriteExpr e | Just is <- getIdenList e = do
    nis <- wrapIt =<< renameIdentifier is <$> ask
    pure $ toIdenExpr nis
rewriteExpr e = error $ "unsupported syntax: " <> show e

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
