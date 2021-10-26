

import Burdock.TestData
import Burdock.Parse
import Burdock.Pretty
import Burdock.Interpreter

--import Control.Monad (forM_, forM)
import Control.Exception.Safe (catch
                              ,SomeException)

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

main :: IO ()
main = do
    at <- makeTests testdata
    T.defaultMain at


makeTests :: TestTree -> IO T.TestTree
makeTests (TestGroup nm ts) = T.testGroup nm <$> mapM makeTests ts

makeTests (ExprParseTest src ex) = pure $ makeParseTest parseExpr prettyExpr src ex
makeTests (ScriptParseTest src ex) = pure $ makeParseTest parseScript prettyScript src ex
makeTests (InterpreterTests _nm src) = makeInterpreterTest src

------------------------------------------------------------------------------

makeParseTest :: (Eq a, Show a) =>
                 (FilePath -> String -> Either String a)
              -> (a -> String)
              -> String
              -> a
              -> T.TestTree
makeParseTest parse pretty src expected = T.testCase src $ do
    let got = either error id $ parse "" src
    T.assertEqual "parse" expected got
    let printed = pretty got
    let roundtrip = either error id $ parse "" printed
    T.assertEqual "parse pretty roundtrip" expected roundtrip

makeInterpreterTest :: String -> IO T.TestTree
makeInterpreterTest src = catch makeIt $ \ex ->
    pure $ T.testCase (take 10 src) $ T.assertFailure $ show (ex :: SomeException)
  where
    makeIt = do
        h <- newHandle
        trs <- runScriptWithTests h Nothing [] src
        let ts = flip map trs $ \(TestResult nm ps) -> T.testCase nm $ T.assertBool "" ps
        pure $ T.testGroup (take 10 src) ts

