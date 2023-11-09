
{-

quick fake test runner to bootstrap the code

-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (show)
import Data.Text.IO (putStrLn)

main :: IO ()
main = putStrLn "hello"
