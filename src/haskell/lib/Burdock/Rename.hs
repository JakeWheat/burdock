
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Rename
    (rename
    ,getImportSources
    ,ModuleID
    ,ModuleMetadata 

    ) where

import Prelude hiding (error, show, putStrLn)
--import Burdock.Utils (error, show)

import Data.Text (Text)

import qualified Burdock.Syntax as S
import Burdock.StaticError (StaticError(..))

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..))

getImportSources :: S.Script -> [S.ImportSource]
getImportSources (S.Script ss) = concatMap getImportSourceInfo ss
  where
    getImportSourceInfo (S.StmtExpr _ (S.Block _ ss')) = concatMap getImportSourceInfo ss'
    getImportSourceInfo (S.Import _ s _) = [s]
    getImportSourceInfo (S.Include _ s) = [s]
    getImportSourceInfo (S.ImportFrom _ s _) = [s]
    getImportSourceInfo _x = []

rename :: Text
       -> [(S.ImportSource, ModuleID)]
       -> [(ModuleID, ModuleMetadata)]
       -> S.Script
       -> Either [StaticError] (ModuleMetadata, S.Script)
rename _fn _ism _ctx (S.Script scr) =

{-
temp algo:
if there's a use context empty
  -> skip it
else add the default imports
then:
if there's a provide: *, type *, data * end
  -> turn on module mode, which for now adds a make module value
     to the end of the source
any other prelude statement is ignored

then keep the rest of the source as is

-}
    
    Right (ModuleMetadata, (S.Script $ checkUseContext scr))

  where
    checkUseContext (S.UseContext _ (S.ImportName ["empty"]) : ss) = checkProvideAll ss
    -- todo: change to load _interpreter, and load burdock2023
    checkUseContext ss = 
        ltm "_bootstrap"
        : ltm "_bootstrap-either"
        : ltm "_bootstrap-list"
        : ltm "global"
        : S.StmtExpr n (app "include-all" [S.Iden n "global"])
        : checkProvideAll ss

    checkProvideAll (S.Provide _ [S.ProvideAll _,S.ProvideTypeAll _,S.ProvideDataAll _] : ss) =
        let rs = concat $ map getBinds ss
        in ss ++ [makeModuleValue rs]
    checkProvideAll ss = ss

    makeModuleValue rs =
        let sp = Nothing
            r = flip map rs $ \r1 -> (r1, S.Iden sp r1)
        in S.StmtExpr sp $ S.App sp (S.DotExpr sp (S.Iden sp "_bootstrap") "make-module") [S.RecordSel sp r]

    getBinds (S.LetDecl _ b _) = getBs b
    getBinds (S.VarDecl _ (S.SimpleBinding _ _ nm _) _) = [nm]
    getBinds (S.DataDecl _ nm _ vars _ _) =
        -- a data decl produces:
        -- a _type-X, an is-X for the type,
        -- a _variant-X, is-X, X for each variant
        let varNames = flip map vars $ \(S.VariantDecl _ vnm _ _) -> vnm
        in concat
            [["_type-" <> nm, "is-" <> nm]
            ,map ("is-" <>) varNames
            ,map ("_variant-" <>) varNames
            ,varNames]
    -- todo: rec, fun, type
    getBinds _ = []
    getBs (S.NameBinding _ nm) = [nm]
    getBs (S.ShadowBinding _ nm) = [nm]
    getBs (S.VariantBinding _ _ bs) = concat $ map getBs bs
    getBs (S.TypedBinding _ b _) = getBs b
    getBs (S.TupleBinding _ bs) = concat $ map getBs bs
    getBs (S.AsBinding _ b _ nm) = nm : getBs b
    getBs (S.WildcardBinding {}) = []
    getBs (S.NumberLitBinding {}) = []
    getBs (S.StringLitBinding {}) = []

    ltm nm = lt nm (app "load-module" [S.Text n "haskell", S.Text n nm])
    lt nm e = S.LetDecl n (S.NameBinding n nm) e
    app nm es = S.App n (S.DotExpr n (S.Iden n "_bootstrap") nm) es
    n = Nothing
