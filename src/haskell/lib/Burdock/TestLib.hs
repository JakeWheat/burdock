
-- temp testing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.TestLib
    (runHUnitTests
    ) where
import Prelude hiding (error, putStrLn, show)
import Data.Text.IO (putStrLn)
import Burdock.Utils (show,error)

import GHC.Stack
    (srcLocFile
    ,srcLocStartLine
    )

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

runHUnitTests :: Runtime ()
runHUnitTests = do
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
    void $ app Nothing (logAndPrintResult cbs) [res]
    pure ()
  where
    catchHunitFailure (Tst.HUnitFailure l msg) =
        let msg' = prependLocation l (T.pack msg)
        in makeTestFail nm msg'
    catchFailure (e :: SomeException) =
        makeTestFail nm (show e)
    makeTestPass :: Text -> Runtime Value
    makeTestPass t =
        app Nothing (testPass cbs) [makeValue "string" t]
    makeTestFail :: Text -> Text -> Runtime Value
    makeTestFail t msg  =
        app Nothing (testFail cbs) [makeValue "string" t, makeValue "string" msg]
    prependLocation mbloc s =
        case mbloc of
            Nothing -> s
            Just loc -> T.pack (srcLocFile loc) <> ":" <> show (srcLocStartLine loc) <> ":\n" <> s
