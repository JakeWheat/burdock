

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
import qualified Text.RawString.QQ as R

import Data.List (intercalate)

import System.Environment

main :: IO ()
main = do

    a <- getArgs
    let (ofn,fns) = case a of
            (ofn':fns') -> (ofn',fns')
            _ -> error "please pass output filename"
    
    let rdFile fn = (fn,) <$> readFile fn
    fs <- mapM rdFile fns
    
    let makeEntry (nm,txt) = "    (" ++ show nm ++ ", [R.r|" ++ txt ++ "|])"
    
    writeFile ofn $ [R.r| {-# LANGUAGE QuasiQuotes #-}
module Burdock.GeneratedBuiltins where

import qualified Text.RawString.QQ as R


builtins :: [(String,String)]
builtins = [
|] ++ intercalate ",\n" (map makeEntry fs) ++ "\n    ]"
