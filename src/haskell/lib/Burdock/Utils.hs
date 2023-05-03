
module Burdock.Utils
    (error
    ,show
    ,trace
    ) where

import Prelude hiding (error, show)
import qualified Prelude as P

import Data.Text as T

import qualified Debug.Trace as DT

error :: T.Text -> a
error = P.error . T.unpack

show :: Show a => a -> T.Text
show = T.pack . P.show

trace :: Show a => T.Text -> a -> b -> b
trace msg val e = DT.trace (T.unpack (msg <> show val)) e
