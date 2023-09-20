

-- The renamer env is the syntax unware metadata handling part
-- of the renamer process, the bit that actually does the renaming
-- on syntax is in the Renamer module, which also explains what it's doing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Burdock.RenamerEnv
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

    ) where

import Prelude hiding (error, putStrLn, show)
--import Burdock.Utils (error, show)

import Data.Text (Text)

import Data.Data (Data)
import Burdock.Syntax (SourcePosition)

-- todo: this will move to the desugarer or a shared module, since
-- there are later passes which can produce static errors too
data StaticError
    = UnrecognisedIdentifier [(SourcePosition,Text)]
    deriving (Eq,Show, Data)

------------------------------------------------------------------------------

-- main types

-- This represents a renamed module metadata, it contains all the
-- information to be able to rename a module which imports this module

data ModuleMetadata = ModuleMetadata
    deriving Show

-- this is the env that is used during renaming a single module
data RenamerEnv
    = RenamerEnv
    {reEnv :: [Text]}
makeRenamerEnv :: [(Text, ModuleMetadata)] -> RenamerEnv
makeRenamerEnv _ =
    RenamerEnv []


------------------------------------------------------------------------------

-- prelude handling

-- todo: an additional step is to read the source, and determine what modules are
-- referenced. Then the calling code will find these modules and get their
-- metadata, before doing the renaming for this module

provide :: RenamerEnv -> ([StaticError], RenamerEnv)
provide = undefined

provideFrom :: RenamerEnv -> ([StaticError], RenamerEnv)
provideFrom = undefined

bImport :: RenamerEnv -> ([StaticError], RenamerEnv)
bImport = undefined

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
queryLoadModules = undefined

-- this applies the provides and returns the final module metadata
-- for this module, plus the record for the desugared module value
-- it's module name, alias, so
-- [(a,b),(c,d)] should become {b:a, d:c}
applyProvides :: RenamerEnv -> ([StaticError], (ModuleMetadata, [(Text,Text)]))
applyProvides = undefined

------------------------------------------------------------------------------

-- renaming

-- adds a local binding, user should scope these, the top level
-- ones will be used in applyProvides
-- it will check shadow is used if it needs to be
addLocalBinding :: Bool -> (SourcePosition,Text) -> RenamerEnv -> ([StaticError], RenamerEnv)
addLocalBinding _shadow (_,i) re =
    ([], re {reEnv = (i: reEnv re)})

-- lookup the identifier
-- see if it exists
-- see if it should be renamed, if so, what do

renameIdentifier :: [(SourcePosition, Text)] -> RenamerEnv -> ([StaticError], [(SourcePosition,Text)])
renameIdentifier x@[(_,i)] re =
    if i `elem` (reEnv re)
    then ([],x)
    else ([UnrecognisedIdentifier x], x)

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
