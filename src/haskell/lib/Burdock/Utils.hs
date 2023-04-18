
module Burdock.Utils
    (error
    ,show
    ) where

import Prelude hiding (error, show)
import qualified Prelude as P

import Data.Text as T


error :: T.Text -> a
error = P.error . T.unpack

show :: Show a => a -> T.Text
show = T.pack . P.show

