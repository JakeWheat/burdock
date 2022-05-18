
import Control.Concurrent (setNumCapabilities)
import GHC.Conc (getNumProcessors)
import Control.Concurrent.Async (withAsync, wait)

import qualified Test.Tasty as T

import qualified PyWrapTests

main :: IO ()
main = do
    setNumCapabilities =<< getNumProcessors
    pts <- PyWrapTests.tests
    _ <- withAsync (T.defaultMain $ pts) wait
    pure ()
