
module Burdock.Utils
    (error
    ,show
    ,_traceit
    ) where

import Prelude hiding (error, show)
import qualified Prelude as P

import Data.Text as T

import Debug.Trace (trace)

error :: T.Text -> a
error = P.error . T.unpack

show :: Show a => a -> T.Text
show = T.pack . P.show

_traceit :: Show a => T.Text -> a -> b -> b
_traceit msg val e = trace (T.unpack (msg <> show val)) e
