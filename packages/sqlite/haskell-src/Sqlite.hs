
module Sqlite (sqlitePackage) where

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

sqlitePackage :: FFIPackage
sqlitePackage = FFIPackage
    {ffiPackageTypes =
     [
     ]
    ,ffiPackageFunctions =
     [
     ]
    }
