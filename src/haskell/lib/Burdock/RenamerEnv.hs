

-- The renamer env is the syntax unware metadata handling part
-- of the renamer process, the bit that actually does the renaming
-- on syntax is in the Renamer module, which also explains what it's doing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module Burdock.RenamerEnv
    (StaticError(..)
    ,prettyStaticError
    ,prettyStaticErrors

    ,ModuleMetadata
    ,ModuleID(..)
    ,RenamerEnv
    ,makeRenamerEnv
    ,rePath

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

    ,RenameBinding(..)
    ,renameIdentifier
    ,renameType
    ,renameBinding
    ,renameAssign
    ,renameTypeName
    ,renameQTypeName

    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

-- could try to create a custom data type that's the same as these imported
-- bits of syntax to isolate the syntax from this module, but it seems like
-- overkill unless there's some specific reason to do it completely correctly
import Burdock.Syntax
    (ProvideItem(..)
    ,ImportSource(..)
    )

import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (mapMaybe)
import Data.Data (Data)
import Burdock.Syntax (SourcePosition)

import Control.Monad.State
    (StateT
    ,runStateT
    )

import Control.Monad.State.Class
    (--get
    --,put
    --,
    state
    )

import Control.Monad.Writer
    (Writer
    ,runWriter
    ,tell
    )

import Control.Monad.Except
    (ExceptT
    ,runExceptT
    ,throwError
    )

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )

import Control.Arrow (first)

------------------------------------------------------------------------------

{-

The way some of the more complex functions here work:
they may produce errors, then continue, and produce a result,
and a modified RenamerEnv - this is to allow the system to
produce as much errors as possible on a single run

Sometimes, a function wants to bail early on an error though.

So the design is: have a Writer for the errors,
  have a state to track the renamerenv as it gets updated
  and also have exceptt to report an error and bail
  immediately, preserving the current state and other errors so far
-}

type RenamerEnvM = ExceptT [StaticError] (StateT RenamerEnv (Writer [StaticError]))

runRenamerEnv :: RenamerEnv -> RenamerEnvM () -> ([StaticError], RenamerEnv)
runRenamerEnv re f =
    case runRenamerEnv' re f of
        ((Left es, re1), es') -> (es ++ es', re1)
        ((Right (), re1), es') -> (es', re1)
  where
    runRenamerEnv' :: RenamerEnv
                   -> RenamerEnvM a
                   -> ((Either [StaticError] a, RenamerEnv), [StaticError])
    runRenamerEnv' r' f' = runWriter $ flip runStateT r' $ runExceptT f'

------------------------------------------------------------------------------

-- todo: this will move to the desugarer or a shared module, since
-- there are later passes which can produce static errors too
data StaticError
    = UnrecognisedIdentifier SourcePosition [Text]
    -- current error def, original def
    | IdentifierRedefined SourcePosition SourcePosition Text
    -- current error pos, target def pos
    | AssignToNonVar SourcePosition SourcePosition [Text]
    deriving (Eq,Show, Data)

prettyStaticError :: StaticError -> Text
prettyStaticError = \case
    UnrecognisedIdentifier _ [] -> "internal error with unrecognised identifier"
    UnrecognisedIdentifier pos is -> doMsg pos $ icd is <> " not found"
    IdentifierRedefined csp osp i ->
        T.unlines
        [doMsg csp $ "identifier redefined: " <> i
        ,doMsg osp "previous definition here"]
    AssignToNonVar csp osp is ->
        T.unlines
        [doMsg csp $ "assign to non var: " <> icd is
        ,doMsg osp "target defined here"]
  where
    doMsg pos msg = case pos of
        Nothing -> "<unknown>:" <> msg
        Just (fn,l,c) -> fn <> ":" <> show l <> ":" <> show c <> ": " <> msg
    icd = T.intercalate "."

-- todo: sort by their primary source position
-- if there's no source position, should they come last?
prettyStaticErrors :: [StaticError] -> Text
prettyStaticErrors = T.unlines . map prettyStaticError

------------------------------------------------------------------------------

-- main types

-- represents the env needed to rename within a module, for prelude
-- and regular statements
-- there's a lot of redundancy here, this is aimed to make it
-- easier to use it

data ModuleID
    = ModuleID
    {mPlugin :: Text
    ,mArgs :: [Text]}
    deriving (Eq, Show)

moduleIDCanonicalName :: ModuleID -> Text
moduleIDCanonicalName m = "_" <> T.intercalate "_" ("module" : mPlugin m : mArgs m)

moduleIDShow :: ModuleID -> Text
moduleIDShow m = mPlugin m <> "(" <> T.intercalate "," (mArgs m) <> ")"

data RenamerEnv
    = RenamerEnv
    { -- available modules to import
     reImportSources :: [(ImportSource,ModuleID)]
      --  the key is the identifier of the module
      -- this is the name and args of the import source
    ,reCtx :: [(ModuleID, ModuleMetadata)]
    -- all the local bindings so far, this is used to process the provide items
    -- and create a module metadata for the current module at the end of renaming
    ,reLocalBindings :: [(Text, (SourcePosition, BindingMeta))]
    -- tally of all the load modules needed in the renamed source
    -- it's the module id that matches the key in reCtx, and the local canonical
    -- alias for that module
    ,reLoadModules :: [(ModuleID,Text)]
    -- modules brought into user scope using a prelude statement, the key is the user
    -- alias. this is only used by other prelude statements
    -- the value is the canonical name and the module metadata
    ,reAliasedModules :: [(Text, (Text, ModuleMetadata))]
    -- running tally of the provide items in prelude statements seen so far
    ,reProvideItems :: [ProvideItem]
    -- all the identifiers in scope that can be used in user code
    -- the key is what the user writes, and the value is the renamed
    -- qiden, and the metadata for that entry
    -- this mirrors the reLocalBindings, and includes all the aliased
    -- and included identifiers from prelude statements that are in scope
    ,reBindings :: [([Text], ([Text], SourcePosition, BindingMeta))]
    ,rePath :: Text -- path of the current module, used for desugaring relative
      -- paths in prelude statements
    }
    deriving Show

makeRenamerEnv :: Text
               -> ModuleMetadata
               -> [(ImportSource, ModuleID)]
               -> [(ModuleID, ModuleMetadata)]
               -> RenamerEnv
makeRenamerEnv fn (ModuleMetadata tmpHack) isCtx ctx =
    let b = flip map tmpHack $ \(nm,(sp,bm)) ->
            ([nm],([nm], sp, bm))
    in RenamerEnv isCtx ctx [] [] [] [] b fn

------------------------------------------------------------------------------

-- prelude handling

-- todo: an additional step is to read the source, and determine what modules are
-- referenced. Then the calling code will find these modules and get their
-- metadata, before doing the renaming for this module

-- todo: check that there are no inconsistencies in the provide items so far
-- defer this to apply provides if it's easier to check everything onne time
provide :: SourcePosition -> [ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
provide _sp pis re = ([], re {reProvideItems = reProvideItems re ++ pis})

-- provide items from an imported module
provideFrom :: SourcePosition -> Text -> [ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
provideFrom = error $ "provide from not implemented yet"

-- load the module, and include all the bindings in it with the alias
bImport :: SourcePosition -> ImportSource -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
bImport sp is alias re =
    -- todo: make a better canonical alias
    let mid = maybe (error $ "unmatched import source: " <> show is) id $ lookup is $ reImportSources re
        canonicalAlias = moduleIDCanonicalName mid
        moduleMetadata = case lookup mid (reCtx re) of
            -- todo: this should show the original import source that the user used
            -- maybe also the desugared one is useful for debugging
            Nothing -> error $ "renamerenv bimport unrecognised module: " <> moduleIDShow mid -- todo return staticerror
            Just m -> m
        ps = flip map (mmBindings moduleMetadata) $ \(i, (_sp, be)) ->
             ([alias,i], ([canonicalAlias,i], sp, be))
    in ([], re
       {reLoadModules = (mid,canonicalAlias) : reLoadModules re
       ,reAliasedModules = (alias, (canonicalAlias, moduleMetadata)) : reAliasedModules re
       ,reBindings = ps ++ reBindings re})

include :: SourcePosition -> ImportSource -> RenamerEnv -> ([StaticError], RenamerEnv)
include sp is re =
    runRenamerEnv re $ do
    let mid = maybe (error $ "unmatched import source: " <> show is) id $ lookup is $ reImportSources re
        canonicalAlias = moduleIDCanonicalName mid
        moduleMetadata = case lookup mid (reCtx re) of
            Nothing -> error $ "renamerenv include unrecognised module: " <> moduleIDShow mid -- todo return staticerror
                       <> " " <> show (map fst (reCtx re))
            Just m -> m
    state (\re1 -> ((), re1 {reLoadModules = (mid,canonicalAlias) : reLoadModules re}))
    flip mapM_ (mmBindings moduleMetadata) $ \(i,(_sp,be)) ->
        case lookup [i] (reBindings re) of
            Nothing -> addOne ([i], ([canonicalAlias,i], sp, be))
            Just (_,sp'',_) -> tell [IdentifierRedefined sp sp'' i]
  where
    addOne :: ([Text], ([Text], SourcePosition, BindingMeta)) -> RenamerEnvM ()
    addOne e = state (\re1 -> ((), re1 {reBindings = e : reBindings re1}))


includeFrom :: SourcePosition -> Text -> [ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
includeFrom sp mal pis re =
    runRenamerEnv re $ do
    (canonicalAlias,modMeta) <- maybe (throwError
                  [error $ "module alias not found: " <> mal]) pure
        $ lookup mal (reAliasedModules re)
    flip mapM_ pis $ \case
        ProvideAll sp' ->
            flip mapM_ (mmBindings modMeta) $ \(i,(_sp,be)) ->
            case lookup [i] (reBindings re) of
                Nothing -> addOne ([i], ([canonicalAlias,i], sp, be))
                -- the issue with this, is you could have a large overlap
                -- from two include from .. : * end statements
                -- and you want to combine all the errors for one * into
                -- one error message for the user, not have like separate 50 errors
                -- one for each name clash. I think this should only happen
                -- with *, so it's one error per syntactic element the user
                -- writes. need a new StaticError variant for this
                Just (_,sp'',_) -> tell [IdentifierRedefined sp' sp'' i]
        ProvideHiding sp' hs ->
            flip mapM_ (mmBindings modMeta) $ \(i,(sp'',be)) ->
            if i `elem` hs
            then pure ()
            else case lookup [i] (reBindings re) of
                Nothing -> addOne ([i], ([canonicalAlias,i], sp', be))
                Just (_,sp''',_) -> tell [IdentifierRedefined sp'' sp''' i]
        ProvideName sp' [i] ->
            case lookup i (mmBindings modMeta) of
                Nothing -> tell [UnrecognisedIdentifier sp' [i]]
                Just (sp'',be) -> addOne ([i], ([canonicalAlias,i], sp'', be))
        ProvideAlias sp' [i] nm ->
            case lookup i (mmBindings modMeta) of
                Nothing -> tell [UnrecognisedIdentifier sp' [i]]
                Just (sp'',be) -> addOne ([nm], ([canonicalAlias,i], sp'', be))
            
        x -> error $ "unsupported include from provide item: " <> show x
  where
    addOne :: ([Text], ([Text], SourcePosition, BindingMeta)) -> RenamerEnvM ()
    addOne e = state (\re1 -> ((), re1 {reBindings = e : reBindings re1}))

importFrom :: SourcePosition -> ImportSource -> [ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
importFrom = error $ "import from not implemented yet"

-- get the load-modules info needed for the desugaring
-- the result is id for the module, local alias for the module
-- so (a,b) here should become b = load-module("a")
-- this should be called before processing the first non prelude statement
queryLoadModules :: RenamerEnv -> ([StaticError], [(ModuleID,Text)])
queryLoadModules re = ([], reLoadModules re)

-- this applies the provides and returns the final module metadata
-- for this module, plus the record for the desugared module value
-- it's module name, alias, so
-- [([a],b),([c],d)] should become {b:a, d:c}
-- it will check for name clashes and unrecognised identifiers
applyProvides :: RenamerEnv -> ([StaticError], (ModuleMetadata, [([Text],Text)]))
applyProvides re =
    let -- return the provided items in the order they were seen
        -- when using *
        localBindings = reverse $ reLocalBindings re
        ps :: [((Text,(SourcePosition, BindingMeta)), [Text])]
        ps = flip concatMap (reProvideItems re) $ \case
            ProvideAll _ -> map (\e@(nm,_) -> (e, [nm]))
                $ filter (\(_, (_,x)) -> case x of
                                 BEType {} -> False
                                 _ -> True)
                  localBindings
            ProvideName _ as -> case lookup as (reBindings re) of
                Nothing -> error $ "identifier not found: " <> show as
                Just (cn,sp,be) -> [((last as,(sp,be)), cn)]
            ProvideAlias _ as n -> case lookup as (reBindings re) of
                Nothing -> error $ "identifier not found: " <> show as
                Just (cn,sp,be) -> [((n,(sp,be)), cn)]
            ProvideHiding _ nms ->
                map (\e@(nm,_) -> (e, [nm]))
                $ filter ((`notElem` nms) . fst)
                $ localBindings
            ProvideType _ nm ->
                let tinfoname = renameQTypeName nm
                in case lookup tinfoname (reBindings re) of
                    Nothing -> error $ "identifier not found: " <> show nm
                    Just (cn,sp,be@(BEType{})) -> [((renameTypeName $ last nm,(sp,be)), cn)]
                    Just {} -> error $ "internal: type provide item names non type: " <> show nm
                    -- todo: can look up without the renameqtypename and give a more helpful
                    -- error message in this case
            ProvideTypeAlias _ nm al ->
                let tinfoname = renameQTypeName nm
                in case lookup tinfoname (reBindings re) of
                    Nothing -> error $ "identifier not found: " <> show nm
                    Just (cn,sp,be@(BEType{})) -> [((renameTypeName al,(sp,be)), cn)]
                    Just {} -> error $ "internal: type provide item names non type: " <> show nm
            ProvideTypeAll _ ->
                flip mapMaybe localBindings $ \case
                    (nm, (sp, be@(BEType{}))) -> Just ((nm, (sp,be)), [nm])
                    _ -> Nothing
            x -> error $ "unsupported provide item " <> show x
        (ms,rs) = unzip $ flip map ps $ \(a@(nm,_), cn) -> (a,(cn,nm))
    in ([], (ModuleMetadata ms, rs))

------------------------------------------------------------------------------

-- renaming

-- adds a local binding, user should scope these, the top level
-- ones will be used in applyProvides
-- it will check shadow is used if it needs to be
createBinding :: Bool -> SourcePosition -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
createBinding shadow sp i re =
    -- check if shadow is needed:
    case lookup [i] (reBindings re) of
        Just (_,sp',_) | not shadow -> ([IdentifierRedefined sp sp' i], re)
        _ -> ([], re {reBindings = ([i],([i], sp, BEIdentifier)) : reBindings re
                     ,reLocalBindings = (i,(sp,BEIdentifier)) : reLocalBindings re})

createBindings :: [(Bool, SourcePosition, Text)] -> RenamerEnv -> ([StaticError], RenamerEnv)
createBindings [] re = ([],re)
createBindings ((sh,sp,nm):bs) re =
    let (es,re') = createBinding sh sp nm re
    in first (es++) $ createBindings bs re'

createVar :: Bool -> SourcePosition -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
createVar shadow sp i re =
    -- check if shadow is needed:
    case lookup [i] (reBindings re) of
        Just (_,sp',_) | not shadow -> ([IdentifierRedefined sp sp' i], re)
        _ -> ([], re {reBindings = ([i],([i], sp, BEVariable)) : reBindings re
                     ,reLocalBindings = (i,(sp,BEVariable)) : reLocalBindings re})

createType :: SourcePosition
             -> Text
             -> Int
             -> [(SourcePosition, Text)] -- variant names
             -> RenamerEnv
             -> ([StaticError], RenamerEnv)
createType sp i numParams vs re =
    case lookup [i] (reBindings re) of
        Just (_,sp',_) -> ([IdentifierRedefined sp sp' i], re)
        _ ->
            let renamedTypeName = renameTypeName i
                bs = (renamedTypeName,(sp,BEType numParams))
                      : flip map vs (\(vsp,vnm) -> (vnm, (vsp,BEVariant 0)))
            in ([]
               ,re {reBindings = flip map bs (\(nm,(sp',be)) -> ([nm],([nm], sp', be))) ++ reBindings re
                   ,reLocalBindings = bs ++ reLocalBindings re})

{-

TODO:
maintaining source positions for later error messages and stack traces:
if there's a field access suffix, these elements keep the source positions
they had when passed in
if it's a local identifier -> it isn't changed, the source position isn't changed
if it's a reference to a imported identifier, then all the renamed components
take the source position of the first element in what was passed in
could try to preserve more of the source positions in this case

-}

renameIdentifier :: [(SourcePosition, Text)] -> RenamerEnv -> ([StaticError], [(SourcePosition,Text)])
renameIdentifier x@((sp,_):_) re =
    case lookup (map snd x) (reBindings re) of
        Nothing ->
            -- field access - check the prefix
            case x of
                [_] -> ([UnrecognisedIdentifier sp (map snd x)], x)
                _ -> case renameIdentifier (init x) re of
                    ([], ys) -> ([], ys ++ [last x])
                    -- if the prefix isn't found, return the original error
                    -- later might want to do something different depending on the error?
                    _ -> ([UnrecognisedIdentifier sp (map snd x)], x)
        -- todo: if this is a type or module alias, then error
        Just (cn, _, _) -> ([], map (sp,) cn)
renameIdentifier [] _ = error $ "internal error: empty identifier"

-- same as renameIdentifier, but for type names
renameType :: SourcePosition -> [Text] -> RenamerEnv -> ([StaticError], (SourcePosition,[Text]))
renameType sp x re =
    case lookup (renameQTypeName x) (reBindings re) of
        Nothing -> ([UnrecognisedIdentifier sp x], (sp,x))
        -- todo: check if type
        Just (cn, _, _) -> ([], (sp, cn))

-- disambiguate a zero arg pattern to see if it's a variant
-- or not
-- rename patterns to the canonical name
-- check the number of args to variant patterns
-- this is only used on namebinding, variantbinding
-- if the name is not a variant, this function will also do the addLocalBinding stuff
-- for patterns which irrefutably introduce bindings, use addLocalBinding:
--   shadowbinding, asbinding

data RenameBinding
    = NameBinding SourcePosition Text
    | VariantBinding SourcePosition [Text] Int

-- see if the binding matches a variant, if so, rename it
-- doesn't check for identifier renamed, for this function,
-- you create bindings in a separate step (todo: reconsider this inconsistency)
renameBinding :: RenameBinding -> RenamerEnv -> ([StaticError], RenameBinding)
renameBinding b re =
    case b of
        NameBinding sp nm ->
            case lookup [nm] (reBindings re) of
                -- todo: check number of args, if it isn't 0, it should be
                -- an error
                Just (cn, _, BEVariant numP) -> ([], VariantBinding sp cn numP)
                Nothing -> ([], b)
                -- error: should be caught when attempt to apply the binding
                Just (_, _, _) -> ([], b)
        VariantBinding sp nm _numP ->
            -- todo: check number of args match
            case lookup nm (reBindings re) of
                Just (cn, _, BEVariant numP) -> ([], VariantBinding sp cn numP)
                _ -> ([UnrecognisedIdentifier sp nm], b)

-- check if the target of the assign is a variable
-- plus rename the target if it needs it
renameAssign :: SourcePosition -> [Text] -> RenamerEnv -> ([StaticError], [Text])
renameAssign sp nm re = 
    case lookup nm (reBindings re) of
        Nothing -> ([UnrecognisedIdentifier sp nm], nm)
        -- todo: check if type
        Just (cn, _, BEVariable) -> ([], cn)
        Just (_, sp1, _) -> ([AssignToNonVar sp sp1 nm], nm)


{-
want to have constructors and types with the same name
so they can't straightforwardly be in the same namespace
pyret just puts them in completely different namespaces
as a hack, rename Type to _typeinfo-Type
then it sort of works as if they are in different namespaces
-}
renameTypeName :: Text -> Text
renameTypeName = ("_typeinfo-" <>)

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

--renameVariantPattern :: Text -> Text
--renameVariantPattern = ("_patterninfo-" <>)
