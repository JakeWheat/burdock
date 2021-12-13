
module Sqlite (sqlitePackage) where

import qualified Database.SQLite.Simple as S

import Data.IORef
    (IORef
    ,newIORef
    ,readIORef
    ,writeIORef)
import Data.String (fromString)
import Control.Monad.Reader (liftIO)
import Data.Dynamic
    (toDyn
    ,fromDynamic
    )

-- todo: turn this into a proper api for ffi code to use
import Burdock.Interpreter
    (Interpreter
    ,Value(..)
    ,FFIPackage(..)
    ,FFITypeInfo(..)
    ,nothing
    ,fromBList
    ,makeBList
    ,app
    )
-- todo: export the functions to work with these with the ffi api
import Burdock.Scientific as Sc

sqlitePackage :: FFIPackage
sqlitePackage = FFIPackage
    {ffiPackageTypes =
     [("sqlite-handle", FFITypeInfo Nothing Nothing Nothing)
     ,("sqlite-result-handle", FFITypeInfo Nothing Nothing Nothing)
     ]
    ,ffiPackageFunctions =
     [("sqlite-open", bOpen)
     ,("sqlite-command", bCommand)
     ,("sqlite-query", bQuery)
     ,("sqlite-next-row", bNextRow)
     ,("sqlite-close-result", bCloseResultHandle)
     ,("sqlite-close", bCloseHandle)
     ]
    }

bOpen :: [Value] -> Interpreter Value
bOpen [TextV fn] = do
    h <- liftIO $ S.open fn
    pure $ FFIValue "sqlite-handle" $ toDyn h
bOpen _ = error "bad args to sqlite open"

bCommand :: [Value] -> Interpreter Value
bCommand [FFIValue "sqlite-handle" h', TextV cmd, args']
    | Just args <- mapM bSqlValueToSQLData =<< fromBList args'
    , Just conn <- fromDynamic h'
    = do
      liftIO $ S.execute conn (fromString cmd) args
      pure nothing
bCommand _ = error "bad args to sqlite command"

bQuery :: [Value] -> Interpreter Value
bQuery [FFIValue "sqlite-handle" h', TextV cmd, args']
    | Just args <- mapM bSqlValueToSQLData =<< fromBList args'
    , Just conn <- fromDynamic h'
    = do
      r <- liftIO $ S.query conn (fromString cmd) args
      (rs :: ResultHandle) <- liftIO $ newIORef r
      pure $ FFIValue "sqlite-result-handle" $ toDyn rs
bQuery _ = error "bad args to sqlite query"

bNextRow :: [Value] -> Interpreter Value
bNextRow [none, somef, mkSqlNumber, FFIValue "sqlite-result-handle" rs']
    | Just (rs :: ResultHandle) <- fromDynamic rs'
    = do
      vr <- liftIO $ readIORef rs
      case vr of
          [] -> pure none
          (v:vs) -> do
              liftIO $ writeIORef rs vs
              v' <- mapM (sqlDataToBValue mkSqlNumber) v
              let lst = makeBList v'
              app somef [lst]
bNextRow _ = error "bad args to sqlite next-row"

-- tg is currently SimpleTypeInfo {tiID = ["_system","modules","sqlite.sqlite","Value"]}
-- how should ffi code check these tags correctly?
bSqlValueToSQLData :: Value -> Maybe S.SQLData
bSqlValueToSQLData (VariantV _tg "sql-number" [(_,NumV n)]) =
    case Sc.extractInt n of
        Just n' -> Just $ S.SQLInteger $ fromIntegral n'
        Nothing -> Nothing
bSqlValueToSQLData _ = Nothing

type ResultHandle = (IORef [[S.SQLData]])

bCloseResultHandle :: [Value] -> Interpreter Value
bCloseResultHandle [FFIValue "sqlite-result-handle" rs']
    | Just (_ :: ResultHandle) <- fromDynamic rs' =
          pure nothing
bCloseResultHandle _ = error "bad args to sqlite close-result"

bCloseHandle :: [Value] -> Interpreter Value
bCloseHandle [FFIValue "sqlite-handle" rs']
    | Just conn <- fromDynamic rs' = do
          liftIO $ S.close conn
          pure nothing
bCloseHandle _ = error "bad args to sqlite close"

sqlDataToBValue :: Value -> S.SQLData -> Interpreter Value
sqlDataToBValue mkSqlNumber (S.SQLInteger n) =
    app mkSqlNumber [NumV $ fromIntegral n]
