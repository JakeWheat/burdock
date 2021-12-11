

module FFITypesTest (ffiTypesFFIPackage) where

import Data.Dynamic
    (Dynamic
    ,toDyn
    ,fromDynamic
    --,Typeable
    )

import Burdock.Interpreter
    (Interpreter
    ,Value(..)
    ,FFIPackage(..)
    ,FFITypeInfo(..)
    )

ffiTypesFFIPackage :: FFIPackage
ffiTypesFFIPackage = FFIPackage
    {ffiPackageTypes =
     [("haskell-string", FFITypeInfo (Just stringEquals) (Just stringLT) (Just stringToRepr))]
    ,ffiPackageFunctions = 
     [("make-haskell-string", makeHaskellString)
     ,("unmake-haskell-string", unmakeHaskellString)]
    }

stringEquals :: Dynamic -> Dynamic -> Interpreter Bool
stringEquals a b =
    case (fromDynamic a, fromDynamic b) of
        (Just a', Just b') -> e a' b'
        _ -> error $ "expected two haskell-strings, got " ++ show (a,b)
  where
    e :: String -> String -> Interpreter Bool
    e x y = pure $ (==) x y

stringLT :: Dynamic -> Dynamic -> Interpreter Bool
stringLT a b =
    case (fromDynamic a, fromDynamic b) of
        (Just a', Just b') -> e a' b'
        _ -> error $ "expected two haskell-strings, got " ++ show (a,b)
  where
    e :: String -> String -> Interpreter Bool
    e x y = pure $ (<) x y

stringToRepr :: Dynamic -> Interpreter String
stringToRepr a = case fromDynamic a of
    Just x -> pure x
    Nothing -> error $ "expected string, got " ++ show a

makeHaskellString :: [Value] -> Interpreter Value
makeHaskellString [TextV t]
    = pure $ FFIValue "haskell-string" $ toDyn t
makeHaskellString _ = error "bad args to makeHaskellString"

unmakeHaskellString :: [Value] -> Interpreter Value
unmakeHaskellString [FFIValue "haskell-string" v]
    | Just v' <- fromDynamic v
    = pure $ TextV v'
unmakeHaskellString _ = error "bad args to unmakeHaskellString"
