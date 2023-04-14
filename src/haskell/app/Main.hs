
{-

quick fake test runner to bootstrap the code

-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.RawString.QQ as R
--import Text.Show.Pretty (ppShow)

import Burdock.Parse (parseScript)
--import qualified Burdock.Syntax as S

import Burdock.Interpreter
    (interpBurdock)

import Control.Monad
    (void)



mySrc :: Text
mySrc = [R.r|

check:
  1 + 2 is 3
  #a = 3
  #a + 1 is 4
  #ffi-demo(5) is 10
end
         
         |]

main :: IO ()
main = do
    let ast = either error id $ parseScript "" $ T.unpack mySrc
    void $ interpBurdock ast
    --runInterp InterpreterState $ interpStmts ast
    --putStrLn $ ppShow ast
    --putStrLn "Hello, Haskell!"
    pure ()



