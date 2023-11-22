
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Utils
    (error
    ,show
    ,trace
    ,traceit
    ,emacsShowPos
    ,catchAsText
    ) where

import Prelude hiding (error, show)
import qualified Prelude as P

import Data.Text as T

import qualified Debug.Trace as DT

import GHC.Stack (HasCallStack)

import Control.Exception.Safe
    (catch
    ,SomeException
    ,MonadCatch
    )
import Text.Show.Pretty (ppShow)

error :: HasCallStack => T.Text -> a
error = P.error . T.unpack

show :: Show a => a -> T.Text
show = T.pack . P.show

trace :: Show a => T.Text -> a -> b -> b
trace msg val e = DT.trace (T.unpack (msg <> (T.pack $ ppShow val))) e

traceit :: Show a => T.Text -> a -> a
traceit msg val = DT.trace (T.unpack (msg <> (T.pack $ ppShow val))) val

emacsShowPos :: Maybe (T.Text, Int, Int) -> T.Text
emacsShowPos Nothing = "unknown:"
emacsShowPos (Just (nm,l,c)) = nm <> ":" <> show l <> ":" <> show c <> ":"

catchAsText :: MonadCatch m => m a -> (Text -> m a) -> m a
catchAsText f h = catch f $ \(e :: SomeException) -> h (show e)
