{-

run tests implemented in haskell from the burdock testing system

todo: should there be a way to compile burdock without the tests embedded?
maybe come back after more work on the haskell ffi

-}

{-# LANGUAGE OverloadedStrings #-}
module Burdock.Tests
    (allTests
    ,TestTree(..)
    ,allTestsTasty
    ) where

import qualified Test.Tasty as Tst
import qualified Test.Tasty.HUnit as Tst

import Data.Text (Text)
import qualified Data.Text as T

-- holy fucking shit is tasty a pure shitload of walls

data TestTree
    = TestGroup Text [TestTree]
    | TestCase Text (IO ())


toTasty :: TestTree -> Tst.TestTree
toTasty (TestGroup nm ts) = Tst.testGroup (T.unpack nm) $ map toTasty ts
toTasty (TestCase nm t) = Tst.testCase (T.unpack nm) t


{-
if the burdock implementation gets the wrong regression, it might be
really difficult to debug via the burdock testing system, so
alternatively be able to run the tasty tests directly via a haskell
main
-}

allTestsTasty :: Tst.TestTree
allTestsTasty = toTasty allTests
        
allTests :: TestTree
allTests = TestGroup "allTests" [t3]

t3 :: TestTree
t3 = TestCase "t3" $ do
    Tst.assertEqual "test" (1 :: Int) 1
    Tst.assertEqual "test2" (2 :: Int) 2
