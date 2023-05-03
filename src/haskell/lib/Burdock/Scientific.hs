
module Burdock.Scientific
    (Scientific
    ,extractInt
    ,showScientific
    ,divideScientific
    ) where

import Data.Scientific
import qualified Data.Text as T


extractInt :: Scientific -> Maybe Int
extractInt n = toBoundedInteger n

showScientific :: Scientific -> T.Text
showScientific n = T.pack $ case extractInt n of
                             Just x -> show x
                             Nothing ->  show n

-- ...

divideScientific :: Scientific -> Scientific -> Scientific
divideScientific a b =
   let a' :: Double
       a' = Data.Scientific.toRealFloat a
       b' :: Double
       b' = Data.Scientific.toRealFloat b
   in fromFloatDigits (a' / b')
  
