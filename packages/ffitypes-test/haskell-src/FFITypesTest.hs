

module FFITypesTest (ffiTypesFFIPackage) where

import Data.Dynamic
    (Dynamic
    ,fromDynamic
    --,Typeable
    )

import Burdock.Interpreter
    (Interpreter
    ,Value(..)
    ,FFIPackage(..)
    ,FFITypeInfo(..)
    ,makeFFIValue
    ,unmakeFFIValue
    )

ffiTypesFFIPackage :: FFIPackage
ffiTypesFFIPackage = FFIPackage
    {ffiPackageTypes =
     [("haskell-string", FFITypeInfo (Just stringEquals) (Just stringLT) (Just stringToRepr))]
    ,ffiPackageFunctions = 
     [("make-haskell-string", makeHaskellString)
     ,("unmake-haskell-string", unmakeHaskellString)
     ,("haskell-string-length", haskellStringLength)
     ]
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
    = makeFFIValue "haskell-string" t
makeHaskellString _ = error "bad args to makeHaskellString"

unmakeHaskellString :: [Value] -> Interpreter Value
unmakeHaskellString [v] = do
    v' <- unmakeFFIValue "haskell-string" v
    case v' of
        Nothing -> error "bad args to unmakeHaskellString"
        Just v'' -> pure $ TextV v''
unmakeHaskellString _ = error "bad args to unmakeHaskellString"


haskellStringLength :: [Value] -> Interpreter Value
haskellStringLength [v] = do
    v' <- unmakeFFIValue "haskell-string" v
    case v' of
        Nothing -> error "bad args to haskellStringLength"
        Just (v'' :: String) -> pure $ NumV $ fromIntegral $ length v''
haskellStringLength _ = error "bad args to haskellStringLength"
