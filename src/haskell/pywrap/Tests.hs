
import Control.Concurrent (setNumCapabilities)
import GHC.Conc (getNumProcessors)

import qualified Test.Tasty as T

import qualified PyWrapTests

main :: IO ()
main = do
    setNumCapabilities =<< getNumProcessors
    pts <- PyWrapTests.tests
    T.defaultMain pts

