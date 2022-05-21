
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Burdock.TestData
import Burdock.Parse
import Burdock.Pretty
import Burdock.Interpreter
import qualified Burdock.HsConcurrencyTests as HsConcurrencyTests

import qualified PyWrapTests

import Burdock.Syntax (SourcePosition)
import Data.Generics.Uniplate.Data (transformBi)
import Data.Data (Data)

import Control.Monad (forM)
import Control.Exception.Safe (catch
                              ,SomeException)

--import Control.Concurrent.Async (withAsync, wait)
import Control.Exception.Safe (bracket)

import Data.List (isSuffixOf)

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

-- todo: how to structure the tests better?
import qualified FFITypesTest
import qualified PythonFFI
import System.Directory (doesDirectoryExist, listDirectory)
import Data.Maybe (catMaybes)
import System.FilePath ((</>))

import System.FilePath.Glob
    (CompOptions(..)
    ,globDir1
    ,compileWith
    ,compDefault
    )

{-

-}

main :: IO ()
main = do
    setNumCapabilities =<< getNumProcessors
    -- avoid bound thread, possible that it makes a performance difference
    pts <- PyWrapTests.tests
    at <- makeTests testdata
    T.defaultMain $ T.testGroup "group" [at
                                        ,HsConcurrencyTests.tests
                                        ,pts]
                  


makeTests :: TestTree -> IO T.TestTree
makeTests (TestGroup nm ts) = T.testGroup nm <$> mapM makeTests ts

makeTests (ExprParseTest src ex) = pure $ makeParseTest parseExpr prettyExpr src ex
makeTests (StmtParseTest src ex) = pure $ makeParseTest parseStmt prettyStmt src ex
makeTests (ScriptParseTest src ex) = pure $ makeParseTest parseScript prettyScript src ex
makeTests (LiterateParseTest src ex) = pure $ makeRtParseTest parseLiterateScript parseScript src ex
makeTests (InterpreterTestsFile fn) = makeInterpreterFileTest fn

makeTests (InterpreterTestsDir dir) = do
   fs1 <- globDir1 (compileWith compDefault {recursiveWildcards = True} "**/*.bur") dir 
   fs2 <- globDir1 (compileWith compDefault {recursiveWildcards = True} "**/*.rst") dir
   let fs = fs1 ++ fs2
   --putStrLn $ dir ++ ": " ++ show fs
   T.testGroup dir . catMaybes <$> (forM fs $ \f -> 
       if ".bur" `isSuffixOf` f || ".rst" `isSuffixOf` f
       then Just <$> makeInterpreterFileTest f
       else pure Nothing)

makeTests (InterpreterTestsOptionalDir dir) = do
    x <- doesDirectoryExist dir
    if x
       then do
           fs <- listDirectory dir
           T.testGroup dir . catMaybes <$> (forM fs $ \f -> 
               if ".rst" `isSuffixOf` f
               then Just <$> makeInterpreterFileTest (dir </> f)
               else pure Nothing)
        else pure $ T.testGroup "" []

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


makeInterpreterFileTest :: FilePath -> IO T.TestTree
makeInterpreterFileTest fn = catch makeIt $ \ex -> do
    pure $ T.testCase fn $ T.assertFailure $ show (ex :: SomeException)
  where
    makeIt = bracket newHandle closeHandle $ \h -> do
        addPackages h
        src <- readFile fn
        _ <- runScript h Nothing []
             "_system.modules._internals.set-auto-print-test-results(false)\n\
             \_system.modules._internals.set-auto-run-tests(true)"
        _ <- (if "rst" `isSuffixOf` fn then runLiterateScript else runScript) h (Just fn) [] src
        trs <- getTestResults h
        
        let ts = flip map trs $ \(modName, cbs) ->
                T.testGroup modName $ flip map cbs $ \(CheckBlockResult cnm cts) ->
                T.testGroup cnm $ flip map cts $ \case
                    TestPass nm -> T.testCase nm $ T.assertBool "" True
                    TestFail nm msg -> T.testCase nm $ T.assertBool msg False
        pure $ T.testGroup fn ts
    -- todo: a bit better
    addPackages h = do
        addFFIPackage h "packages/ffitypes-test" FFITypesTest.ffiTypesFFIPackage
        addFFIPackage h "packages/python" PythonFFI.pythonFFIPackage
