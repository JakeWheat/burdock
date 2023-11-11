
module Burdock.StaticError
    (StaticError(..)
    ,prettyStaticError
    ,prettyStaticErrors
    ) where

import Data.Text (Text)
    
data StaticError = StaticError

prettyStaticError :: StaticError -> Text
prettyStaticError = undefined

prettyStaticErrors :: [StaticError] -> Text
prettyStaticErrors = undefined

