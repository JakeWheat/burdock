{-

run tests implemented in haskell from the burdock testing system

todo: should there be a way to compile burdock without the tests embedded?
maybe come back after more work on the haskell ffi

-}

{-# LANGUAGE OverloadedStrings #-}
module Burdock.Tests
    (allTests
    ) where

import qualified Test.Tasty.HUnit as Tst

import Burdock.TestLib
    (TestTree(..)
    )

import qualified Burdock.ParseTests (testData)

allTests :: TestTree
allTests =
    TestGroup "allTests"
    [t3
    ,Burdock.ParseTests.testData
    ]

t3 :: TestTree
t3 = TestCase "t3" $ do
    Tst.assertEqual "test" (1 :: Int) 1
    Tst.assertEqual "test2" (2 :: Int) 2
