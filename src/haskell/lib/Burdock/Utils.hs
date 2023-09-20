
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Utils
    (error
    ,show
    ,trace
    ,traceit
    ,emacsShowPos
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

traceit :: Show a => T.Text -> a -> a
traceit msg val = DT.trace (T.unpack (msg <> show val)) val

emacsShowPos :: Maybe (T.Text, Int, Int) -> T.Text
emacsShowPos Nothing = "unknown:"
emacsShowPos (Just (nm,l,c)) = nm <> ":" <> show l <> ":" <> show c <> ":"
