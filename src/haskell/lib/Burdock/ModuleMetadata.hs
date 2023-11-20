
module Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..)
    ,BindingMeta(..)
    ) where

import Data.Text (Text)

data ModuleID
    = ModuleID
    {mPlugin :: Text
    ,mArgs :: [Text]}
    deriving (Eq, Show)

data ModuleMetadata = ModuleMetadata

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
