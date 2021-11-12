
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Burdock.TestData
import Burdock.Parse
import Burdock.Pretty
import Burdock.Interpreter
import qualified Burdock.HsOccasionalTests as HsOccasionalTests
import Burdock.Syntax (SourcePosition)
import Data.Generics.Uniplate.Data (transformBi)
import Data.Data (Data)

--import Control.Monad (forM_, forM)
import Control.Exception.Safe (catch
                              ,SomeException)


import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

main :: IO ()
main = do
    setNumCapabilities =<< getNumProcessors
    at <- makeTests testdata
    T.defaultMain $ T.testGroup "group" [at
                                        ,HsOccasionalTests.tests]


makeTests :: TestTree -> IO T.TestTree
makeTests (TestGroup nm ts) = T.testGroup nm <$> mapM makeTests ts

makeTests (ExprParseTest src ex) = pure $ makeParseTest parseExpr prettyExpr src ex
makeTests (StmtParseTest src ex) = pure $ makeParseTest parseStmt prettyStmt src ex
makeTests (ScriptParseTest src ex) = pure $ makeParseTest parseScript prettyScript src ex
makeTests (InterpreterTestsFile fn) = makeInterpreterFileTest fn

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

makeInterpreterFileTest :: FilePath -> IO T.TestTree
makeInterpreterFileTest fn = catch makeIt $ \ex -> do
    pure $ T.testCase fn $ T.assertFailure $ show (ex :: SomeException)
  where
    makeIt = do
        src <- readFile fn
        h <- newHandle
        _ <- runScript h Nothing []
             "_system.modules._internals.set-auto-print-test-results(false)\n\
             \_system.modules._internals.set-auto-run-tests(true)"
        _ <- runScript h Nothing [] src
        trs <- getTestResults h
        
        let ts = flip map trs $ \(modName, cbs) ->
                T.testGroup modName $ flip map cbs $ \(CheckBlockResult cnm cts) ->
                T.testGroup cnm $ flip map cts $ \case
                    TestPass nm -> T.testCase nm $ T.assertBool "" True
                    TestFail nm msg -> T.testCase nm $ T.assertBool msg False
        pure $ T.testGroup fn ts
