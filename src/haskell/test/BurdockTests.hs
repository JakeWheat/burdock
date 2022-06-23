
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Burdock.TestData
import Burdock.Parse
import Burdock.Pretty
import Burdock.Interpreter

import qualified PyWrapTests

import Burdock.Syntax (SourcePosition)
import Data.Generics.Uniplate.Data (transformBi)
import Data.Data (Data)

--import Control.Concurrent.Async (withAsync, wait)

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

-- todo: how to structure the tests better?
--import qualified PythonFFI

main :: IO ()
main = do
    setNumCapabilities =<< getNumProcessors
    -- avoid bound thread, possible that it makes a performance difference
    pts <- PyWrapTests.tests
    at <- makeTests testdata
    T.defaultMain $ T.testGroup "group" [at
                                        ,pts]
                  


makeTests :: TestTree -> IO T.TestTree
makeTests (TestGroup nm ts) = T.testGroup nm <$> mapM makeTests ts

makeTests (ExprParseTest src ex) = pure $ makeParseTest parseExpr prettyExpr src ex
makeTests (StmtParseTest src ex) = pure $ makeParseTest parseStmt prettyStmt src ex
makeTests (ScriptParseTest src ex) = pure $ makeParseTest parseScript prettyScript src ex
makeTests (LiterateParseTest src ex) = pure $ makeRtParseTest parseLiterateScript parseScript src ex

------------------------------------------------------------------------------

makeParseTest :: (Eq a, Show a, Data a) =>
                 (FilePath -> String -> Either String a)
              -> (a -> String)
              -> String
              -> a
              -> T.TestTree
makeParseTest parse pretty src expected = T.testCase src $ do
    let rmsp = transformBi (\(_ :: SourcePosition) -> Nothing)
    let got = either error rmsp $ parse "" src
    T.assertEqual "parse" expected got
    let printed = pretty got
    let roundtrip = either error rmsp $ parse "" printed
    T.assertEqual "parse pretty roundtrip" expected roundtrip

makeRtParseTest :: (Eq a, Show a, Data a) =>
                 (FilePath -> String -> Either String a)
              -> (FilePath -> String -> Either String a)
              -> String
              -> String
              -> T.TestTree
makeRtParseTest parse parseEx src expected = T.testCase src $ do
    let rmsp = transformBi (\(_ :: SourcePosition) -> Nothing)
    let got = either error rmsp $ parse "" src
        expected' = either error rmsp $ parseEx "" expected
    T.assertEqual "parse" expected' got

