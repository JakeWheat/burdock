
-- temp testing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.TestWrap
    (doStuff) where
import Prelude hiding (error, putStrLn, show)
import Data.Text.IO (putStrLn)
import Burdock.Utils (show,error)

--import qualified Test.Tasty as Tst
import qualified Test.Tasty.HUnit as Tst

import Burdock.Runtime
    (Runtime
    ,liftIO
    ,Value
    ,lookupBinding
    ,app
    ,makeValue
    )

import Burdock.Tests
    (allTests
    ,TestTree(..))

import Control.Exception.Safe (catch
                              ,SomeException
                              )

import Control.Monad (void)

import qualified Data.Text as T
import Data.Text (Text)

data TestRunClosure
    = TestRunClosure
    {logAndPrintResult :: Value
    ,testPass :: Value
    ,testFail :: Value
    }

doStuff :: Runtime ()
doStuff = do
    trc <- TestRunClosure
        <$> (maybe (error "log-and-print-result not found") id <$> lookupBinding "log-and-print-result")
        <*> (maybe (error "test-pass not found") id <$> lookupBinding "test-pass")
        <*> (maybe (error "test-fail not found") id <$> lookupBinding "test-fail")
    runTestTree trc allTests


runTestTree :: TestRunClosure -> TestTree -> Runtime ()
runTestTree cbs (TestGroup nm ts) = do
    liftIO $ putStrLn $ nm <> ":"
    mapM_ (runTestTree cbs) ts

runTestTree cbs (TestCase nm tst) = do
    res <- flip catch catchFailure $ flip catch catchHunitFailure $ do
        liftIO $ tst
        makeTestPass nm
        -- app Nothing logResult res
        -- app Nothing formatTest res
        --liftIO $ putStrLn $ "PASS: " <> nm
    void $ app Nothing (logAndPrintResult cbs) [res]
    pure ()
  where
    catchHunitFailure (Tst.HUnitFailure _ msg) =
        --liftIO $ putStrLn $ "FAIL: " <> nm <> " " <> T.pack msg
        makeTestFail nm (T.pack msg)
    catchFailure (e :: SomeException) =
        makeTestFail nm (show e)
        --liftIO $ putStrLn $ "FAIL: " <> nm <> " " <> show e
    makeTestPass :: Text -> Runtime Value
    makeTestPass t =
        app Nothing (testPass cbs) [makeValue "string" t]
    makeTestFail :: Text -> Text -> Runtime Value
    makeTestFail t msg  =
        app Nothing (testFail cbs) [makeValue "string" t, makeValue "string" msg]
