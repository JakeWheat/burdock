
-- temp testing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.TestLib
    (runHUnitTests
    ,TestTree(..)
    ,toTasty
    ) where
import Prelude hiding (error, putStrLn, show)
import Data.Text.IO (putStrLn)
import Burdock.Utils (show)

import GHC.Stack
    (srcLocFile
    ,srcLocStartLine
    )

import qualified Test.Tasty as Tst
import qualified Test.Tasty.HUnit as Tst

import Burdock.RuntimeBootstrap
    (Runtime
    ,liftIO
    ,Value
    ,app
    ,makeString
    ,getMember
    )

import Control.Exception.Safe (catch
                              ,SomeException
                              )

import Control.Monad (void)

import qualified Data.Text as T
import Data.Text (Text)
import Burdock.HaskellModulePlugin
    (importModule
    ,ImportSource(..))

------------------------------------------------------------------------------

-- holy fucking shit is tasty a pure shitload of walls

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

data TestRunClosure
    = TestRunClosure
    {logAndPrintResult :: Value
    ,testPass :: Value
    ,testFail :: Value
    }

-- run the hunit tests as if they are burdock tests
-- this should integrate with the formatting of results, and selective
-- running of tests in the future

runHUnitTests :: TestTree -> Runtime ()
runHUnitTests allTests = do
    testing <- importModule $ ImportName ["testing"]
    {-trc <- TestRunClosure
        <$> (maybe (error "log-and-print-result not found") id <$> lookupBinding "log-and-print-result")
        <*> (maybe (error "test-pass not found") id <$> lookupBinding "test-pass")
        <*> (maybe (error "test-fail not found") id <$> lookupBinding "test-fail")-}
    trc <- TestRunClosure
           <$> getMember testing "log-and-print-result"
           <*> getMember testing "test-pass"
           <*> getMember testing "test-fail"
    runTestTree trc allTests

runTestTree :: TestRunClosure -> TestTree -> Runtime ()
runTestTree cbs (TestGroup nm ts) = do
    liftIO $ putStrLn $ nm <> ":"
    mapM_ (runTestTree cbs) ts

runTestTree cbs (TestCase nm tst) = do
    res <- flip catch catchFailure $ flip catch catchHunitFailure $ do
        liftIO $ tst
        makeTestPass nm
    void $ app Nothing (logAndPrintResult cbs) [res]
    pure ()
  where
    catchHunitFailure (Tst.HUnitFailure l msg) =
        let msg' = prependLocation l (T.pack msg)
        in makeTestFail nm msg'
    catchFailure (e :: SomeException) =
        makeTestFail nm (show e)
    makeTestPass :: Text -> Runtime Value
    makeTestPass t = do
        t' <- makeString t
        app Nothing (testPass cbs) [t']
    makeTestFail :: Text -> Text -> Runtime Value
    makeTestFail t msg  = do
        t' <- makeString t
        msg' <- makeString msg
        app Nothing (testFail cbs) [t', msg']
    prependLocation mbloc s =
        case mbloc of
            Nothing -> s
            Just loc -> T.pack (srcLocFile loc) <> ":" <> show (srcLocStartLine loc) <> ":\n" <> s
