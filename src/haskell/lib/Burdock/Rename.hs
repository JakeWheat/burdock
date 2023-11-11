
module Burdock.Rename
    (renameScript

    ,ModuleID
    ,ModuleMetadata 

    ) where

import Data.Text (Text)

import qualified Burdock.Syntax as S
import Burdock.StaticError (StaticError(..))

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..)
    )

renameScript :: Text
             -> [(S.ImportSource, ModuleID)]
             -> [(ModuleID, ModuleMetadata)]
             -> S.Script
             -> Either [StaticError] (ModuleMetadata, S.Script)
renameScript _fn _ism _ctx scr =
    Right (ModuleMetadata, scr)
