

{-

This is the core renamer code which is the logic without any syntax.

Renamer is the first pass on the syntax, it's parsed syntax ast ->
parsed syntax ast. After than, the syntax is desugared further and
converted to interpretersyntax

It processes and checks prelude statements

it directs how the prelude statements to be desugared to load-module
and the module value at the end which represents the provided
things. All the prelude statements are removed in this pass

Every referenced module gets a single unique "local canonical name"

Every reference to an imported identifier in the module source is
changed to use this canonical name - whether or not it is qualified
in the original source. This includes types and variant patterns.

Every no arg variant in a pattern match is disambiguated from a
pattern binding, so later stages can trust a namepattern never
matches a 0 arg constructor

TODO: maybe every use of a variable in an expression is rewritten to
  explicitly derefence the variable?

TODO: this code could also disambiguate qualified names from record
field access. Not sure if it's worth it with the current implementation,
qualified names during interpretation are implemented as record field
access. This could be useful if want to closure capture module members,
instead of just the whole module which is what happens now

TODO: this code instead of just doing load-module, could do explicit
  lists of what it needs from each module, then the loaded value
  would only have these and nothing else, to reduce unneeded pointers

Something it doesn't currently do is rewrite bindings to be unique within a
module - even when shadowing happens. A fresh name generator is planned,
which will also make this relatively easy if it's desired.

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
{-# LANGUAGE LambdaCase #-}
module Burdock.Rename
    (RenamerEnv
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
    ,createBindings
    ,createType
    ,createVar 
    
    ,renameIdentifier
    ,renameType
    ,RenameBinding(..)
    ,renameBinding
    ,renameAssign
    ,renameTypeName
    ,renameVariantPattern

    ,canonicalizeModuleName
    ,intMod
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error, show)
--import Burdock.Utils (trace, traceit)

import Burdock.StaticError (StaticError(..))

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..)
    ,BindingMeta(..)
    )

-- OK, so it cheats and uses a little bit of syntax. Could be all proper and
-- just mirror these few small items, then write conversion functions, but
-- it doesn't seem worth it at this time
import qualified Burdock.Syntax as S
    (ProvideItem(..)
    ,ImportSource(..)
    ,SourcePosition
    )

import Data.Text (Text)
import qualified  Data.Text as T

import Control.Arrow (first)

import Burdock.RenameTypes
    (RenamerEnv(..)
    ,reImportSources
    ,reModuleMetadatas
    ,reProvides
    ,reProvideFroms
    ,reImports
    ,reIncludes
    ,reIncludeFroms
    ,reImportFroms
    ,reBindings
    ,reCurrentOriginIDName
    ,reLoadModules
    
    ,RenamerBindingEntry(..)
    --,beRenamedName
    --,beSourcePos
    --,beBm
    --,beOrigin
    --,beIsLocal
    )

import Lens.Micro
    (set
    ,over
    )
import Lens.Micro.Extras
    (view)

--import Data.Maybe (mapMaybe)

import Data.List (nub)

--import Text.Show.Pretty (ppShow)

------------------------------------------------------------------------------

makeRenamerEnv :: Text
               -> [(S.ImportSource, ModuleID)]
               -> [(ModuleID, ModuleMetadata)]
               -> RenamerEnv
makeRenamerEnv mnm is ctx = RenamerEnv is ctx [] [] [] [] [] [] [] mnm []

------------------------------------------------------------------------------

-- probably need to revisit this approach if start using a fresh names system
canonicalizeModuleName :: Text -> [Text] -> Text
canonicalizeModuleName pnm as = "_module-" <> pnm <> "-" <> T.intercalate "." as

intMod :: Text
intMod = canonicalizeModuleName "haskell" ["_interpreter"]

------------------------------------------------------------------------------

provide :: S.SourcePosition -> [S.ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
provide sp pis re =
    ([], over reProvides ((sp,pis):) re)
         
provideFrom :: S.SourcePosition -> [Text] -> [S.ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
provideFrom sp nm pis re =
    ([],over reProvideFroms ((sp,nm,pis):) re)

bImport :: S.SourcePosition -> S.ImportSource -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
bImport sp is alias re =
    ([], over reImports ((sp,is,alias):) re)

include :: S.SourcePosition -> S.ImportSource -> RenamerEnv -> ([StaticError], RenamerEnv)
include sp is re =
    ([],over reIncludes ((sp,is):) re)

includeFrom :: S.SourcePosition -> [Text] -> [S.ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
includeFrom sp mal pis re =
    ([], over reIncludeFroms ((sp,mal,pis):) re)

importFrom :: S.SourcePosition -> S.ImportSource -> [S.ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
importFrom = error $ "import from not implemented yet"

-- get the load-modules info needed for the desugaring
-- the result is id for the module, local alias for the module
-- so (a,b) here should become b = load-module("a")
-- this should be called before processing the first non prelude statement

-- maybe change the name, this is the call that processes all the prelude statements
-- there should be a switch so that after this, a subsequence prelude statement
-- causes an error

queryLoadModules :: RenamerEnv -> ([StaticError], (RenamerEnv,[(ModuleID,Text)]))
queryLoadModules re =
    let loadModules =
            -- todo: sort the imports by order they first appear in the source
            -- so they load in the order the user wrote them, which will make
            -- troubleshooting less confusing
            -- this means interleaving these statements, and taking into account
            -- transforms that need to preserve the source order in the positions
            let is1 = reverse $ map (\(_,i,_) -> i) (view reImports re)
                is2 = reverse $ map snd (view reIncludes re)
                is3 = reverse $ map (\(_,i,_) -> i) (view reImportFroms re)
                is = nub [ mid
                     | i <- is1 ++ is2 ++ is3
                     , (isx,mid) <- view reImportSources re
                     , i == isx]
            in flip map is $ \mid@(ModuleID p as) ->
                    -- todo: revisit how the bootstrap temporarily
                    -- masquerades as the _interpreter module
                    if as == ["_bootstrap"]
                    then (mid, canonicalizeModuleName "haskell" ["_interpreter"])
                    else (mid, canonicalizeModuleName p as)
        importBindings =
            -- create the binding entries
            map (\(userAl,canonicalAl, (nm, sp, bm, oid)) ->
            ([userAl,nm], RenamerBindingEntry [canonicalAl,nm] sp bm oid False))
            -- unnestb
            $ concatMap ( \(a,b,c) -> map (a,b,) c)
            -- join imports, import sources, module metadatas and load modules
            [(userAl,canonicalAl,mms)
            | (_,is1,userAl) <- view reImports re
            , (is2,mid2) <- view reImportSources re
            , (mid3,ModuleMetadata mms) <- view reModuleMetadatas re
            , (mid4,canonicalAl) <- loadModules
            , is1 == is2
            , mid2 == mid3
            , mid3 == mid4]
        includeBindings =
            map (\(canonicalAl, (nm, sp, bm, oid)) ->
            ([nm], RenamerBindingEntry [canonicalAl,nm] sp bm oid False))
            -- unnest
            $ concatMap ( \(a,b) -> map (a,) b)
            -- join includes with import sources, module metadata and load modules
            [(canonicalAl, mms)
            | (_,is1) <- view reIncludes re
            , (is2,mid2) <- view reImportSources re
            , (mid3,ModuleMetadata mms) <- view reModuleMetadatas re
            , (mid4,canonicalAl) <- loadModules
            , is1 == is2
            , mid2 == mid3
            , mid3 == mid4
            ]

        bs = importBindings ++ includeBindings
    in ([], (set reBindings bs $ set reLoadModules loadModules re, loadModules))

-- this applies the provides and returns the final module metadata
-- for this module, plus the record for the desugared module value
-- it's module name, alias, so
-- [([a],b),([c],d)] should become {b:a, d:c}
-- it will check for name clashes and unrecognised identifiers
-- if it returns nothing, it means there were no provide statements
-- this is different to having an empty provide, that will return an empty
-- module value, without it, it will return the value of the last statement in the user script
applyProvides :: RenamerEnv -> ([StaticError], (ModuleMetadata, Maybe [([Text],Text)]))
applyProvides re =
    -- check if there are any provides or provide froms
    if null (view reProvides re) && null (view reProvideFroms re)
    then ([], (ModuleMetadata [], Nothing))
    else let (mm1,rc1)= unzip
                  [((nm1,sp,bm,oid), (nm, nm1))
                  | _ <- view reProvides re
                  , (_,RenamerBindingEntry nm sp bm oid True) <- view reBindings re
                  , let nm1 = last nm]
             (mm2, rc2) = unzip
                  $ map (\(canonicalAl, meta@(nm, _sp, _bm, _oid)) ->
                             (meta, ([canonicalAl,nm], nm)))
                  -- unnest
                  $ concatMap ( \(a,b) -> map (a,) b)
                  {-$ traceit (T.pack $ ppShow
                             {-("apply provides"
                            ,"provide froms", reverse $ view reProvideFroms re
                            ,"imports" , view reImports re
                            ,"import sources", view reImportSources re
                            ,"module metadatas", view reModuleMetadatas re
                            ,"load modules", view reLoadModules re
                            )-}
                            ("provide froms", reverse $ view reProvideFroms re
                            ,"==============================================="
                            ,"join to imports"
                            , [(al,is,al1)
                              | (_,al,_) <- reverse $ view reProvideFroms re
                              , (_,is,al1) <- view reImports re
                              , al == [al1]]
                            ,"==============================================="
                            ,"join to import sources"
                            , [(al,is,al1, mid2)
                              | (_,al,_) <- reverse $ view reProvideFroms re
                              , (_,is,al1) <- view reImports re
                              , (is2,mid2) <- view reImportSources re
                              , al == [al1]
                              , is == is2
                              ]
                            ,"==============================================="
                            ,"join to module metas"
                            , [(al,is,al1, mid2, mm)
                              | (_,al,_) <- reverse $ view reProvideFroms re
                              , (_,is,al1) <- view reImports re
                              , (is2,mid2) <- view reImportSources re
                              , (mid3, ModuleMetadata mm) <- view reModuleMetadatas re
                              , al == [al1]
                              , is == is2
                              , mid2 == mid3
                              ]
                            ,"==============================================="
                            ,"join to loadmodules"
                            , [(al,is,al1, mid2, mm)
                              | (_,al,_) <- reverse $ view reProvideFroms re
                              , (_,is,al1) <- view reImports re
                              , (is2,mid2) <- view reImportSources re
                              , (mid3, ModuleMetadata mm) <- view reModuleMetadatas re
                              , al == [al1]
                              , is == is2
                              , mid2 == mid3
                              ]
                            ,"==============================================="
                            )
                            )-}
                  [ (canonicalAl, metas)
                  | (_,al,_) <- reverse $ view reProvideFroms re
                  , (_,is,al1) <- view reImports re
                  , (is2,mid2) <- view reImportSources re
                  , (mid3, ModuleMetadata metas) <- view reModuleMetadatas re
                  , (mid4,canonicalAl) <- view reLoadModules re
                  , al == [al1]
                  , is == is2
                  , mid2 == mid3
                  , mid3 == mid4
                  ]
                  
             mm = mm1 ++ mm2
             rc = rc1 ++ rc2
         in ([], (ModuleMetadata mm, Just rc))

------------------------------------------------------------------------------

createBinding :: Bool -> S.SourcePosition -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
createBinding _shadow sp i re =
    let b = RenamerBindingEntry [i] sp BEIdentifier (view reCurrentOriginIDName re,sp) True
    in ([],over reBindings (([i],b):) re)

createBindings :: [(Bool, S.SourcePosition, Text)] -> RenamerEnv -> ([StaticError], RenamerEnv)
createBindings [] re = ([],re)
createBindings ((sh,sp,nm):bs) re =
    let (es,re') = createBinding sh sp nm re
    in first (es++) $ createBindings bs re'

createVar :: Bool -> S.SourcePosition -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
createVar _shadow sp i re =
    let b = RenamerBindingEntry [i] sp BEVariable (view reCurrentOriginIDName re,sp) True
    in ([],over reBindings (([i],b):) re)

createType :: S.SourcePosition
             -> Text
             -> Int
             -> [(S.SourcePosition, Text)] -- variant names
             -> RenamerEnv
             -> ([StaticError], RenamerEnv)
createType sp i numParams vs re =
    -- create _type-x, is-x, for variants: is-x, _variant-x, x
    let renamedTypeName = renameTypeName i
        oid = (view reCurrentOriginIDName re,sp)
        makeSimpleEntry nm = ([nm], RenamerBindingEntry [nm] sp BEIdentifier oid True)
        bs = [([renamedTypeName], RenamerBindingEntry [renamedTypeName] sp (BEType numParams) oid True)
             ,makeSimpleEntry ("is-" <> i)]
             ++ concat (flip map vs (\(_vsp,vnm) ->
                 [makeSimpleEntry vnm
                 ,makeSimpleEntry ("is-" <> vnm)
                 -- todo: need to track the number of args for each variant
                 ,(["_variant-" <> vnm], RenamerBindingEntry ["_variant-" <> vnm] sp (BEVariant 1) oid True)]))
        -- todo: add the data entry for data provide item export
    in ([],over reBindings (bs ++) re)

renameIdentifier :: [(S.SourcePosition, Text)] -> RenamerEnv -> ([StaticError], [(S.SourcePosition,Text)])
-- need to figure out a better way to handle this - at least namespace it,
-- plus it should be a regular value you can assign and pass around and stuff
renameIdentifier x@[(_, "list")] _ = ([], x)
-- similar comments, but this isn't namespaced, and it cannot be assigned
-- and passed around
renameIdentifier x@[(_, "run-task")] _ = ([], x)

renameIdentifier x@((sp,_):_) re =
    case lookup (map snd x) (view reBindings re) of
        Nothing ->
            -- field access - check the prefix
            case x of
                [_] -> --trace "1" (x, view reBindings re)
                     ([UnrecognisedIdentifier sp (map snd x)], x)
                _ -> case renameIdentifier (init x) re of
                    ([], ys) -> ([], ys ++ [last x])
                    -- if the prefix isn't found, return the original error
                    -- later might want to do something different depending on the error?
                    _ -> --trace "2" x
                        ([UnrecognisedIdentifier sp (map snd x)], x)
        -- todo: if this is a type or module alias, then error
        Just (RenamerBindingEntry cn _ _ _ _) -> ([], map (sp,) cn)
        Just (RenamerBindingAmbiguous _) -> error $ show sp <> "ambiguous identifier"
renameIdentifier [] _ = error $ "internal error: empty identifier"

-- same as renameIdentifier, but for type names
renameType :: S.SourcePosition -> [Text] -> RenamerEnv -> ([StaticError], (S.SourcePosition,[Text]))
renameType sp x _re = ([],(sp,renameQTypeName x))

-- renameBinding:
--
-- disambiguate a zero arg pattern to see if it's a variant or not
-- rename patterns to the canonical name
-- check the number of args to variant patterns
-- this is only used on namebinding, variantbinding
-- if the name is not a variant, this function will also do the addLocalBinding stuff
-- for patterns which irrefutably introduce bindings, use addLocalBinding:
--   shadowbinding, asbinding

data RenameBinding
    = NameBinding S.SourcePosition Text
    | VariantBinding S.SourcePosition [Text] Int

-- see if the binding matches a variant, if so, rename it
-- doesn't check for identifier renamed, for this function,
-- you create bindings in a separate step (todo: reconsider this inconsistency)
renameBinding :: RenameBinding -> RenamerEnv -> ([StaticError], RenameBinding)
renameBinding b re =
    case b of
        NameBinding sp nm ->
            case lookup [renameVariantPattern nm] (view reBindings re) of
                -- todo: check number of args, if it isn't 0, it should be
                -- an error
                Just (RenamerBindingEntry cn _ (BEVariant numP) _ _) ->
                    ([], VariantBinding sp cn numP)
                Nothing -> ([], b)
                -- error: should be caught when attempt to apply the binding
                Just _ -> ([], b)
        VariantBinding sp nm _numP ->
            -- todo: check number of args match
            case lookup (renameQVariantPattern nm) (view reBindings re) of
                Just (RenamerBindingEntry cn _ (BEVariant numP) _ _) ->
                    ([], VariantBinding sp cn numP)
                -- temp hack
                --Just (RenamerBindingEntry cn _ _ _ _) ->
                --    ([], VariantBinding sp cn 42)
                Just x -> error $ show x
                _ -> --trace "didn't find: " (nm,renameQVariantPattern nm, view reBindings re)
                    ([UnrecognisedIdentifier sp nm], b)

-- check if the target of the assign is a variable
-- plus rename the target if it needs it
renameAssign :: S.SourcePosition -> [Text] -> RenamerEnv -> ([StaticError], [Text])
renameAssign _sp nm _re = ([], nm)

{-
want to have constructors and types with the same name
so they can't straightforwardly be in the same namespace
pyret just puts them in completely different namespaces
as a hack, rename Type to _typeinfo-Type
then it sort of works as if they are in different namespaces
-}
renameTypeName :: Text -> Text
renameTypeName = ("_type-" <>)

renameQTypeName :: [Text] -> [Text]
renameQTypeName x = if null x
    then error $ "empty qtype name"
    else init x ++ [renameTypeName $ last x]

{-
a variant is an identifier, which returns the constant or function
to make a value of that variant
it is also something that allows you to pattern match on the variant
in this system, it will lookup a typeinfo/variant name pair
the renamer will leave variant expressions pointing to the renamed
  variant
but patterns will point to _patterninfo-Variant
when providing names, if they are variants, you export/import and rename
  the corresponding _patterninfo- binding too
-}

renameVariantPattern :: Text -> Text
renameVariantPattern = ("_variant-" <>)

renameQVariantPattern :: [Text] -> [Text]
renameQVariantPattern x = if null x
    then error $ "empty qvariantname"
    else init x ++ [renameVariantPattern $ last x]
