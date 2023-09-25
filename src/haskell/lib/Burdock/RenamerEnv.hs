

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

    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

-- could try to create a custom data type that's the same as provide
-- items to isolate the syntax from this module, but it seems like
-- overkill unless there's some specific reason to do it completely correctly
import Burdock.Syntax (ProvideItem(..))

import Data.Text (Text)
import qualified Data.Text as T

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
    deriving (Eq,Show, Data)

prettyStaticError :: StaticError -> Text
prettyStaticError = \case
    UnrecognisedIdentifier _ [] -> "internal error with unrecognised identifier"
    UnrecognisedIdentifier pos is -> doMsg pos $ icd is <> " not found"
    IdentifierRedefined csp osp i ->
        T.unlines
        [doMsg csp $ "identifier redefined: " <> i
        ,doMsg osp "previous definition here"]
  where
    doMsg pos msg = case pos of
        Nothing -> msg
        Just (fn,l,c) -> fn <> ":" <> show l <> ":" <> show c <> ": " <> msg
    icd = T.intercalate "."

-- todo: sort by their primary source position
-- if there's no source position, should they come last?
prettyStaticErrors :: [StaticError] -> Text
prettyStaticErrors = T.unlines . map prettyStaticError

------------------------------------------------------------------------------

-- main types

data BindingMeta
    = BEIdentifier
    --  | BEVariant Int
    --  | BEVariable
    --  | BEType Int
    --  | BEModuleAlias
    -- todo: add module provide items
    --   add error for flagging ambiguous identifier on use
    --   when the ambiguity is introduced with two * provide items


-- represents the bindings available in a module
data ModuleMetadata
    = ModuleMetadata
      {mmBindings :: [(Text,(SourcePosition, BindingMeta))]}


-- represents the env needed to rename within a module, for prelude
-- and regular statements
-- there's a lot of redundancy here, this is aimed to make it
-- easier to use it

data RenamerEnv
    = RenamerEnv
    { -- available modules to import, the key is the identifier of the module
      -- for burdock modules for now, this will be the unique path to the module source

     reCtx :: [(Text, ModuleMetadata)]
    -- all the local bindings so far, this is used to process the provide items
    -- and create a module metadata for the current module at the end of renaming
    ,reLocalBindings :: [(Text, (SourcePosition, BindingMeta))]
    -- tally of all the load modules needed in the renamed source
    -- it's the module id that matches the key in reCtx, and the local canonical
    -- alias for that module
    ,reLoadModules :: [(Text,Text)]
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
    }

makeRenamerEnv :: [(Text, ModuleMetadata)] -> RenamerEnv
makeRenamerEnv ctx = RenamerEnv ctx [] [] [] [] []

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
provideFrom :: RenamerEnv -> ([StaticError], RenamerEnv)
provideFrom = undefined

-- load the module, and include all the bindings in it with the alias
bImport :: SourcePosition -> Text -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
bImport sp nm alias re =
    -- todo: make a better canonical alias
    let canonicalAlias = "_module-" <> nm
        moduleMetadata = case lookup nm (reCtx re) of
            Nothing -> error $ "unrecognised module: " <> nm -- todo return staticerror
            Just m -> m
        ps = flip map (mmBindings moduleMetadata) $ \(i, (_sp, be)) ->
             ([alias,i], ([canonicalAlias,i], sp, be))
    in ([], re
       {reLoadModules = (nm,canonicalAlias) : reLoadModules re
       ,reAliasedModules = (alias, (canonicalAlias, moduleMetadata)) : reAliasedModules re
       ,reBindings = ps ++ reBindings re})

include :: RenamerEnv -> ([StaticError], RenamerEnv)
include = undefined

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

importFrom :: RenamerEnv -> ([StaticError], RenamerEnv)
importFrom = undefined

-- get the load-modules info needed for the desugaring
-- the result is id for the module, local alias for the module
-- so (a,b) here should become b = load-module("a")
-- this should be called before processing the first non prelude statement
queryLoadModules :: RenamerEnv -> ([StaticError], [(Text,Text)])
queryLoadModules re = ([], reLoadModules re)

-- this applies the provides and returns the final module metadata
-- for this module, plus the record for the desugared module value
-- it's module name, alias, so
-- [([a],b),([c],d)] should become {b:a, d:c}
-- it will check for name clashes and unrecognised identifiers
applyProvides :: RenamerEnv -> ([StaticError], (ModuleMetadata, [([Text],Text)]))
applyProvides re =
    let -- todo: no provides doesn't default to this, it defaults
        -- to providing nothing
        provideItems = if null (reProvideItems re)
                       then [ProvideAll Nothing]
                       else reProvideItems re
        -- return the provided items in the order they were seen
        localBindings = reverse $ reLocalBindings re
        ps :: [((Text,(SourcePosition, BindingMeta)), [Text])]
        ps = flip concatMap provideItems $ \case
            -- todo: filter by type of binding
            ProvideAll _ -> map (\e@(nm,_) -> (e, [nm])) localBindings
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
            x -> error $ "unsupported provide item " <> show x
        (ms,rs) = unzip $ flip map ps $ \(a@(nm,_), cn) -> (a,(cn,nm))
    in ([], (ModuleMetadata ms, rs))

------------------------------------------------------------------------------

-- renaming

-- adds a local binding, user should scope these, the top level
-- ones will be used in applyProvides
-- it will check shadow is used if it needs to be
addLocalBinding :: Bool -> SourcePosition -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
addLocalBinding shadow sp i re =
    -- check if shadow is needed:
    case lookup [i] (reBindings re) of
        Just (_,sp',_) | not shadow -> ([IdentifierRedefined sp sp' i], re)
        _ -> ([], re {reBindings = ([i],([i], sp, BEIdentifier)) : reBindings re
                     ,reLocalBindings = (i,(sp,BEIdentifier)) : reLocalBindings re
                     })

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
renameType :: [(SourcePosition, Text)] -> RenamerEnv -> ([StaticError], [(SourcePosition,Text)])
renameType = undefined

-- disambiguate a zero arg pattern to see if it's a variant
-- or not
-- rename patterns to the canonical name
-- check the number of args to variant patterns
-- this is only used on namebinding, variantbinding
-- if the name is not a variant, this function will also do the addLocalBinding stuff
-- for patterns which irrefutably introduce bindings, use addLocalBinding:
--   shadowbinding, asbinding
renamePattern :: [Text] -- name components
              -> Maybe Int -- number of args if explicit variant pattern
                 -- if this amount doesn't match the definition of the
                 -- variant, this function will produce an error
                 -- if it's Nothing, it will check that it's either
                 -- a zero arg variant, or an actual name introducing binding
              -> RenamerEnv
              -- return: left means it's a namebinding
              -- right means it's a variant binding (possibly 0 arg)
              -> ([StaticError], (Either Text [Text]))
renamePattern = undefined

-- check if the target of the assign is a variable
-- plus rename the target if it needs it
renameAssign :: [(SourcePosition,Text)] -> RenamerEnv -> ([StaticError], [(SourcePosition,Text)])
renameAssign = undefined
