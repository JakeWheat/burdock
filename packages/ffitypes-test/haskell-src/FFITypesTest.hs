{-


creating an ffi type

ffi types are held in a dynamic in haskell
when you create an ffi package, you create a list of types,
  and a list of ffi functions
the type is a name, and an ffitypeinfo, which holds the member callback function
this is when someone does ffival.member on your ffi value
  this callback function will be called with [Value] as the args
    the actual args will be [TextV field-name, ffi-value]
the ffi value can support the following standard operations:
  ._equals
  ._lessthan
  ._torepr - if missing, a default one will be based on show for dynamic
  ._app -> this allows the ffi-value to be used like a function

currently, the method is tedious:
  the member returns a funv value, which points to an ffi function
  so for each of the above (and any other methods), you have to
    create a big method value binding things and calling
      an ffi function
    and you have to create each ffi function that implements that method
    and add a entry for it in the ffipackage

-}

{-# LANGUAGE ScopedTypeVariables #-}
module FFITypesTest
    (ffiTypesFFIPackage
    ) where

import Data.Dynamic
    ({-Dynamic
    ,-}fromDynamic
    --,Typeable
    )

import Burdock.Interpreter
    (Interpreter
    ,Value(..)
    ,FFIPackage(..)
    ,FFITypeInfo(..)
    ,makeFFIValue
    ,unmakeFFIValue
    ,fromBList
    ,makeFFIMemberFunction
    ,ffiSingleArgMethod
    ,ffiNoArgMethod
    ,ffiNoArgValue
    ,FMD(..)
    ,ffiMemberDispatcher
    )

ffiTypesFFIPackage :: FFIPackage
ffiTypesFFIPackage = FFIPackage
    {ffiPackageTypes =
     [("haskell-string"
      , FFITypeInfo (makeFFIMemberFunction "_member-haskell-string"))]
    ,ffiPackageFunctions = 
     [("make-haskell-string", makeHaskellString)
     ,("unmake-haskell-string", unmakeHaskellString)
     ,("haskell-string-length", haskellStringLength)
     ,("_member-haskell-string", ffiMemberDispatcher haskellStringFMD)
     ,("_haskell-string-equals", haskellStringEquals)
     ,("_haskell-string-lessthan", haskellStringLessThan)
     ,("_haskell-string-torepr", haskellStringToRepr)
     ,("_haskell-string-app", haskellStringApp)
     ,("_haskell-string-generic-member", haskellStringGenericMember)]}

haskellStringFMD :: FMD
haskellStringFMD = FMD
    {fmdLkp = [("_equals", ffiSingleArgMethod "_haskell-string-equals")
              ,("_lessthan", ffiSingleArgMethod "_haskell-string-lessthan")
              ,("_torepr", ffiNoArgMethod "_haskell-string-torepr")
              ,("_app", ffiSingleArgMethod "_haskell-string-app")]
    ,fmdFallback = Just $ ffiNoArgValue "_haskell-string-generic-member"}

haskellStringGenericMember :: [Value] -> Interpreter Value
haskellStringGenericMember [TextV fld, v] = do
    v' <- maybe (error $ "expected string, got " ++ show v) id
          <$> unmakeFFIValue "haskell-string" v
    pure $ TextV $ v' ++ "." ++ fld
haskellStringGenericMember _ = error "bad args to haskellStringGenericMember"

haskellStringEquals :: [Value] -> Interpreter Value
haskellStringEquals [FFIValue _ a, FFIValue _ b] =
    case (fromDynamic a, fromDynamic b) of
        (Just a', Just b') -> BoolV <$> e a' b'
        _ -> error $ "expected two haskell-strings, got " ++ show (a,b)
  where
    e :: String -> String -> Interpreter Bool
    e x y = pure $ (==) x y
haskellStringEquals _ = error "bad args to haskellStringEquals"

haskellStringLessThan :: [Value] -> Interpreter Value
haskellStringLessThan [FFIValue _ a, FFIValue _ b] =
    case (fromDynamic a, fromDynamic b) of
        (Just a', Just b') -> BoolV <$> e a' b'
        _ -> error $ "expected two haskell-strings, got " ++ show (a,b)
  where
    e :: String -> String -> Interpreter Bool
    e x y = pure $ (<) x y
haskellStringLessThan _ = error "bad args to haskellStringLessThan"

haskellStringToRepr :: [Value] -> Interpreter Value
haskellStringToRepr [FFIValue _ a] =
    case fromDynamic a of
        Just (a' :: String) -> pure $ TextV a'
        _ -> error $ "expected haskell-string, got " ++ show a
haskellStringToRepr _ = error "bad args to haskellStringToRepr"

haskellStringApp :: [Value] -> Interpreter Value
haskellStringApp [FFIValue _ a, b] =
    case fromDynamic a of
        Just (a' :: String) -> do
            let Just bs = fromBList b
            pure $ TextV $ "app called on " ++ a' ++ (if null bs then "" else (" " ++ show bs))
        _ -> error $ "expected haskell-string, got " ++ show a
haskellStringApp _ = error "bad args to haskellStringApp"

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
