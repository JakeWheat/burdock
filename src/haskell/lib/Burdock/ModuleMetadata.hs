
module Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..)
    ) where

import Data.Text (Text)

data ModuleID
    = ModuleID
    {mPlugin :: Text
    ,mArgs :: [Text]}
    deriving (Eq, Show)

data ModuleMetadata = ModuleMetadata
