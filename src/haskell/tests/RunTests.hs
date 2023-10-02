
import Burdock.Tests (allTests)
import Burdock.TestLib (toTasty)
import Test.Tasty (defaultMain)


main :: IO ()
main = defaultMain $ toTasty allTests
