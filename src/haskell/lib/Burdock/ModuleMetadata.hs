

-- currently, the runtime needs this, it should be abstract to the
-- runtime but transparent to the renamer system

module Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    ,tempEmptyModuleMetadata
    ) where
    

import Burdock.Syntax (SourcePosition)
import Data.Text (Text)

data BindingMeta
    = BEIdentifier
    | BEVariant Int
    | BEVariable
    | BEType Int
    --  | BEModuleAlias
    -- todo: add module provide items
    --   add error for flagging ambiguous identifier on use
    --   when the ambiguity is introduced with two * provide items
    deriving Show

-- represents the bindings available in a module
data ModuleMetadata
    = ModuleMetadata
      {mmBindings :: [(Text,(SourcePosition, BindingMeta))]}
    deriving Show

tempEmptyModuleMetadata :: ModuleMetadata
tempEmptyModuleMetadata = ModuleMetadata []

