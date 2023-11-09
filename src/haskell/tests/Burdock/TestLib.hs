
-- temp testing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.TestLib
    (TestTree(..)
    ,toTasty
    ) where
import Prelude hiding (error, putStrLn, show)

import qualified Test.Tasty as Tst
import qualified Test.Tasty.HUnit as Tst

import qualified Data.Text as T
import Data.Text (Text)

data TestTree
    = TestGroup Text [TestTree]
    | TestCase Text (IO ())

{-
if the burdock implementation gets the wrong regression, it might be
really difficult to debug via the burdock testing system, so
alternatively be able to run the tasty tests directly via a haskell
main
-}

toTasty :: TestTree -> Tst.TestTree
toTasty (TestGroup nm ts) = Tst.testGroup (T.unpack nm) $ map toTasty ts
toTasty (TestCase nm t) = Tst.testCase (T.unpack nm) t
