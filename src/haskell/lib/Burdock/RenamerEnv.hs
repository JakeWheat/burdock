

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

import Data.Text (Text)
import qualified Data.Text as T

import Data.Data (Data)
import Burdock.Syntax (SourcePosition)

import Data.Maybe (mapMaybe)

-- todo: this will move to the desugarer or a shared module, since
-- there are later passes which can produce static errors too
data StaticError
    = UnrecognisedIdentifier [(SourcePosition,Text)]
    deriving (Eq,Show, Data)

prettyStaticError :: StaticError -> Text
prettyStaticError = \case
    UnrecognisedIdentifier [] -> "internal error with unrecognised identifier"
    UnrecognisedIdentifier is@((pos,_):_) -> doMsg pos $ icd (map snd is) <> " not found"
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
    {reCtx :: [(Text, ModuleMetadata)]
     -- imported module id, canonical alias in this module
    ,reLoadModules :: [(Text,Text)]
    -- what the user writes, what it will be rewritten to
    ,reBindings :: [([Text], [Text])]}
makeRenamerEnv :: [(Text, ModuleMetadata)] -> RenamerEnv
makeRenamerEnv ctx =
    RenamerEnv ctx [] []


------------------------------------------------------------------------------

-- prelude handling

-- todo: an additional step is to read the source, and determine what modules are
-- referenced. Then the calling code will find these modules and get their
-- metadata, before doing the renaming for this module

provide :: RenamerEnv -> ([StaticError], RenamerEnv)
provide = undefined

provideFrom :: RenamerEnv -> ([StaticError], RenamerEnv)
provideFrom = undefined

bImport :: Text -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
bImport nm alias re =
    -- todo: make a better canonical alias
    let canonicalAlias = "_module-" <> nm
        ps = case lookup nm (reCtx re) of
            Nothing -> error $ "unrecognised module: " <> nm -- todo return staticerror
            Just m -> flip map (mmBindings m) $ \n -> ([alias,n], [canonicalAlias,n])
    in ([], re
       {reLoadModules = (nm,canonicalAlias) : reLoadModules re
       ,reBindings = ps ++ reBindings re})

include :: RenamerEnv -> ([StaticError], RenamerEnv)
include = undefined

includeFrom :: RenamerEnv -> ([StaticError], RenamerEnv)
includeFrom = undefined

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
            ([a],[b]) | a == b -> Just a
            _ -> Nothing
    -- return the provided items in the order they were seen
    in ([], (ModuleMetadata localBinds, reverse $ map (\a -> (a,a)) localBinds))

------------------------------------------------------------------------------

-- renaming

-- adds a local binding, user should scope these, the top level
-- ones will be used in applyProvides
-- it will check shadow is used if it needs to be
addLocalBinding :: Bool -> (SourcePosition,Text) -> RenamerEnv -> ([StaticError], RenamerEnv)
addLocalBinding _shadow (_,i) re =
    ([], re {reBindings = (([i],[i]): reBindings re)})

-- lookup the identifier
-- see if it exists
-- see if it should be renamed, if so, what do

renameIdentifier :: [(SourcePosition, Text)] -> RenamerEnv -> ([StaticError], [(SourcePosition,Text)])
renameIdentifier x re =
    case lookup (map snd x) (reBindings re) of
        Nothing -> ([UnrecognisedIdentifier x], x)
        Just r -> ([],map (Nothing,) r)

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
