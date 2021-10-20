

import Burdock.TestData
import Burdock.Parse
import Burdock.Pretty
import Burdock.Interpreter

import Control.Monad (forM_)
import Control.Exception.Safe (catch
                              ,SomeException)

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

main :: IO ()
main = do
    at <- allTests
    T.defaultMain $ T.testGroup "all_tests" [at]


allTests :: IO T.TestTree
allTests = do
    interpreterTests'' <- interpreterTests'
    pure $ T.testGroup "Burdock.Tests" [exprParseTests'
                                       ,scriptParseTests'
                                       ,interpreterTests''
                                       ]

-- todo: mix all the test data together in a big data only tree
-- then apply the make tests in one pass over the tree
-- so that the tree structure of the tests is part of the testdata
-- and not here
exprParseTests' :: T.TestTree
exprParseTests' =
    T.testGroup "exprParseTests" $
    map (uncurry $ makeParseTest parseExpr prettyExpr) exprParseTests

scriptParseTests' :: T.TestTree
scriptParseTests' =
    T.testGroup "scriptParseTests" $
    map (uncurry $ makeParseTest parseScript prettyScript) scriptParseTests

interpreterTests' :: IO T.TestTree
interpreterTests' =
    T.testGroup "interpreterTests" <$> mapM makeInterpreterTest interpreterTests


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
    pure $ T.testCase src $ T.assertFailure $ show (ex :: SomeException)
  where
    makeIt = do
        let ast = either error id $ parseScript "" src
        trs <- executeScriptWithTests ast
        pure $ T.testCase src $ forM_ trs $ \(TestResult nm ps) -> T.assertBool nm ps
