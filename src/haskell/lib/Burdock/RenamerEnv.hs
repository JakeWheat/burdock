

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

import Data.Maybe
    (mapMaybe
    )

import Data.Tuple (swap)

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

prettyStaticErrors :: [StaticError] -> Text
prettyStaticErrors = T.unlines . map prettyStaticError

------------------------------------------------------------------------------

-- main types

-- This represents a renamed module metadata, it contains all the
-- information to be able to rename a module which imports this module

data ModuleMetadata
    = ModuleMetadata
    {mmBindings :: [Text]}
    deriving Show

-- this is the env that is used during renaming a single module
data RenamerEnv
    = RenamerEnv
    { -- the list of modules available to be imported
     reCtx :: [(Text, ModuleMetadata)]
     -- imported module id, canonical alias in this module
     -- the id is the same as the one in reCtx
    ,reLoadModules :: [(Text,Text)]
    -- user alias, canonical name
    ,reAliasedModules :: [(Text,Text)]
    -- what the user writes, what it will be rewritten to
    ,reBindings :: [([Text], BindingEntry)]}
    deriving Show
data BindingEntry
    = BindingEntry
    {beName :: [Text]
    -- source position of definition if local
    -- or the prelude statement that introduces it if not
    ,beSourcePosition :: SourcePosition}
    deriving Show

makeRenamerEnv :: [(Text, ModuleMetadata)] -> RenamerEnv
makeRenamerEnv ctx =
    RenamerEnv ctx [] [] []


------------------------------------------------------------------------------

-- prelude handling

-- todo: an additional step is to read the source, and determine what modules are
-- referenced. Then the calling code will find these modules and get their
-- metadata, before doing the renaming for this module

provide :: RenamerEnv -> ([StaticError], RenamerEnv)
provide = undefined

provideFrom :: RenamerEnv -> ([StaticError], RenamerEnv)
provideFrom = undefined

bImport :: SourcePosition -> Text -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
bImport sp nm alias re =
    -- todo: make a better canonical alias
    let canonicalAlias = "_module-" <> nm
        ps = case lookup nm (reCtx re) of
            Nothing -> error $ "unrecognised module: " <> nm -- todo return staticerror
            Just m -> flip map (mmBindings m)
                      $ \n -> ([alias,n]
                              ,BindingEntry [canonicalAlias,n] sp)
    in ([], re
       {reLoadModules = (nm,canonicalAlias) : reLoadModules re
       ,reAliasedModules = (alias, canonicalAlias) : reAliasedModules re
       ,reBindings = ps ++ reBindings re})

include :: RenamerEnv -> ([StaticError], RenamerEnv)
include = undefined

includeFrom :: SourcePosition -> Text -> [ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
includeFrom sp mal _pis re = runRenamerEnv re $ do
    -- todo: turn these errors into staticerrors
    canonicalAlias <- maybe (throwError [error $ "alias not found: " <> mal]) pure
        $ lookup mal (reAliasedModules re)
    modId <- maybe (throwError
                  [error $ "internal error: canonical alias not found in reLoadModules: " <> canonicalAlias]) pure
        $ lookup canonicalAlias (map swap $ reLoadModules re)
    modMeta <- maybe (throwError
                  [error $ "internal error: module metadata not found: " <> modId]) pure
        $ lookup modId (reCtx re)
    flip mapM_ (mmBindings modMeta) $ \i -> do
        case lookup [i] (reBindings re) of
            Nothing -> addOne ([i], BindingEntry [canonicalAlias,i] sp)
            -- the issue with this, is you could have a large overlap
            -- from two include from .. : * end statements
            -- and you want to combine all the errors for one * into
            -- one error message for the user, not have like separate 50 errors
            -- one for each name clash. I think this should only happen
            -- with *, so it's one error per syntactic element the user
            -- writes. need a new StaticError variant for this
            Just p ->
                tell [IdentifierRedefined sp (beSourcePosition p) i]
        
  where
    addOne :: ([Text], BindingEntry) -> RenamerEnvM ()
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
-- [(a,b),(c,d)] should become {b:a, d:c}
applyProvides :: RenamerEnv -> ([StaticError], (ModuleMetadata, [(Text,Text)]))
applyProvides re =
    -- todo: find a more robust way to track local bindings
    -- come back to this when doing provide items
    let localBinds = flip mapMaybe (reBindings re) $ \case
            ([a],b) | [a] == beName b -> Just a
            _ -> Nothing
    -- return the provided items in the order they were seen
    in ([], (ModuleMetadata localBinds, reverse $ map (\a -> (a,a)) localBinds))

------------------------------------------------------------------------------

-- renaming

-- adds a local binding, user should scope these, the top level
-- ones will be used in applyProvides
-- it will check shadow is used if it needs to be
addLocalBinding :: Bool -> SourcePosition -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
addLocalBinding shadow sp i re =
    -- check if shadow is needed:
    case lookup [i] (reBindings re) of
        Just be | not shadow -> ([IdentifierRedefined sp (beSourcePosition be) i], re)
        _ -> ([], re {reBindings = (([i],BindingEntry [i] sp): reBindings re)})

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
        Just r -> ([],map (sp,) $ beName r)

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
