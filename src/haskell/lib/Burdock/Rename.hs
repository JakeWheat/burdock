

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
    ,renameBinding
    ,renameAssign
    ,renameTypeName
    ,renameVariantPattern

    ) where
    
import Burdock.StaticError (StaticError(..))

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..))

-- OK, so it cheats and uses a little bit of syntax. Could be all proper and
-- just mirror these few small items, then write conversion functions, but
-- it doesn't seem worth it at this time
import qualified Burdock.Syntax as S
    (ProvideItem(..)
    ,ImportSource(..)
    ,SourcePosition
    )

import Data.Text (Text)

------------------------------------------------------------------------------

data RenamerEnv
    = RenamerEnv

makeRenamerEnv :: Text
               -> [(S.ImportSource, ModuleID)]
               -> [(ModuleID, ModuleMetadata)]
               -> RenamerEnv
makeRenamerEnv fn isCtx ctx = undefined

provide :: S.SourcePosition -> [S.ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
provide _sp _pis _re = undefined

provideFrom :: S.SourcePosition -> Text -> [S.ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
provideFrom = error $ "provide from not implemented yet"

bImport :: S.SourcePosition -> S.ImportSource -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
bImport _sp _is _alias _re = undefined

include :: S.SourcePosition -> S.ImportSource -> RenamerEnv -> ([StaticError], RenamerEnv)
include _sp _is _re = undefined

includeFrom :: S.SourcePosition -> Text -> [S.ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
includeFrom _sp _mal _pis _re = undefined

importFrom :: S.SourcePosition -> S.ImportSource -> [S.ProvideItem] -> RenamerEnv -> ([StaticError], RenamerEnv)
importFrom = error $ "import from not implemented yet"

-- get the load-modules info needed for the desugaring
-- the result is id for the module, local alias for the module
-- so (a,b) here should become b = load-module("a")
-- this should be called before processing the first non prelude statement
queryLoadModules :: RenamerEnv -> ([StaticError], [(ModuleID,Text)])
queryLoadModules _re = undefined

-- this applies the provides and returns the final module metadata
-- for this module, plus the record for the desugared module value
-- it's module name, alias, so
-- [([a],b),([c],d)] should become {b:a, d:c}
-- it will check for name clashes and unrecognised identifiers
-- if it returns nothing, it means there were no provide statements
-- this is different to having an empty provide, that will return an empty
-- module value, without it, it will return the value of the last statement in the user script
applyProvides :: RenamerEnv -> ([StaticError], (ModuleMetadata, Maybe [([Text],Text)]))
applyProvides _re = undefined

------------------------------------------------------------------------------

createBinding :: Bool -> S.SourcePosition -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
createBinding shadow sp i re = undefined

createBindings :: [(Bool, S.SourcePosition, Text)] -> RenamerEnv -> ([StaticError], RenamerEnv)
createBindings _ re = undefined

createVar :: Bool -> S.SourcePosition -> Text -> RenamerEnv -> ([StaticError], RenamerEnv)
createVar shadow sp i re = undefined

createType :: S.SourcePosition
             -> Text
             -> Int
             -> [(S.SourcePosition, Text)] -- variant names
             -> RenamerEnv
             -> ([StaticError], RenamerEnv)
createType sp i numParams vs re = undefined

renameIdentifier :: [(S.SourcePosition, Text)] -> RenamerEnv -> ([StaticError], [(S.SourcePosition,Text)])
renameIdentifier x@((sp,_):_) re = undefined

-- same as renameIdentifier, but for type names
renameType :: S.SourcePosition -> [Text] -> RenamerEnv -> ([StaticError], (S.SourcePosition,[Text]))
renameType sp x re = undefined

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
renameBinding b re = undefined

-- check if the target of the assign is a variable
-- plus rename the target if it needs it
renameAssign :: S.SourcePosition -> [Text] -> RenamerEnv -> ([StaticError], [Text])
renameAssign sp nm re = undefined

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
