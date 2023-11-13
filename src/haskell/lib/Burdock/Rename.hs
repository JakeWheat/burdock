
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Rename
    (renameScript
    ,renameModule

    ,ModuleID
    ,ModuleMetadata 

    ) where

import Data.Text (Text)

import qualified Burdock.Syntax as S
import Burdock.StaticError (StaticError(..))

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..))

renameScript :: Text
             -> [(S.ImportSource, ModuleID)]
             -> [(ModuleID, ModuleMetadata)]
             -> S.Script
             -> Either [StaticError] (ModuleMetadata, S.Script)
renameScript _fn _ism _ctx scr =
    Right (ModuleMetadata, scr)

{-
The bool is to turn provideAll on as the default,
this is for working with burdock source fragments from ffi code,
which will usually want provide: *, type *, data * end as the default
(this provideAll doesn't also do module *)
-}
renameModule :: Bool
             -> Text
             -> [(S.ImportSource, ModuleID)]
             -> [(ModuleID, ModuleMetadata)]
             -> S.Script
             -> Either [StaticError] (ModuleMetadata, S.Script)
renameModule _provideAll _fn _ _ (S.Script ss) =
    -- quick hack - just generate the make-module call at the end
    let rs = concat $ map getBinds ss
    in Right (ModuleMetadata, S.Script (ss ++ [makeModuleValue rs]))
  where
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
