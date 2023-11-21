
module Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..)
    ,BindingMeta(..)
    ,OriginID
    ) where

import Data.Text (Text)
import Burdock.Syntax (SourcePosition)

-- the internal canonical name for a module, corresponds roughly to
-- an import special in the syntax
data ModuleID
    = ModuleID
    {mPlugin :: Text
    ,mArgs :: [Text]}
    deriving (Eq, Show)

-- used to allow two references to the same ultimate item in prelude
-- statements to be deduplicated, instead of cause a shadow error
-- a bit hacky at the moment, a canonical module id and a source position
type OriginID = (Text, SourcePosition)

-- represents the bindings available in a module when it's used from
-- some other code
data ModuleMetadata
    = ModuleMetadata
    {mmBindings :: [(Text,SourcePosition, BindingMeta, OriginID)]}
    deriving Show

{-

handles several things: entries in a module metadata
entries for looking up identifiers, types, variants, variables
during renaming
but also to represent module and data provide items, they don't
exactly belong here but it's nice and simple to shoehorn them in
it's slightly trick - maybe this means it should be split:
the data entries only say how a data provide item should be expanded
these subentries also have to appear
but for modules - in a nested module metadata you only have
the entries under the modules
during renaming, you also have separate entries for each item

-}
data BindingMeta
    = BEIdentifier
    | BEVariant Int
    | BEVariable
    | BEType Int
    | BEModule Text ModuleMetadata
    | BEData [Text]
    deriving Show
