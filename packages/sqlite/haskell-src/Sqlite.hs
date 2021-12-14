
module Sqlite (sqlitePackage) where

import qualified Database.SQLite.Simple as S

import Data.IORef
    (IORef
    ,newIORef
    ,readIORef
    ,writeIORef)
import Data.String (fromString)
import Control.Monad.Reader (liftIO)

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
    ,makeFFIValue
    ,unmakeFFIValue
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
     ,("sqlite-testing", sqliteTesting)
     ]
    }

bOpen :: [Value] -> Interpreter Value
bOpen [TextV fn] = do
    h <- liftIO $ S.open fn
    makeFFIValue "sqlite-handle" h
bOpen _ = error "bad args to sqlite open"

bCommand :: [Value] -> Interpreter Value
bCommand [h, TextV cmd, args']
    | Just args <- mapM bSqlValueToSQLData =<< fromBList args'
    = do
      conn <- maybe (error "bad args to sqlite command") id
              <$> unmakeFFIValue "sqlite-handle" h
      liftIO $ S.execute conn (fromString cmd) args
      pure nothing
bCommand _ = error "bad args to sqlite command"

bQuery :: [Value] -> Interpreter Value
bQuery [h, TextV cmd, args']
    | Just args <- mapM bSqlValueToSQLData =<< fromBList args'
    = do
      conn <- maybe (error "bad args to sqlite query") id
              <$> unmakeFFIValue "sqlite-handle" h
      r <- liftIO $ S.query conn (fromString cmd) args
      (rs :: ResultHandle) <- liftIO $ newIORef r
      makeFFIValue "sqlite-result-handle" rs
bQuery _ = error "bad args to sqlite query"

bNextRow :: [Value] -> Interpreter Value
bNextRow [none, somef, mkSqlNumber, rs']
    = do
      rs <- maybe (error "bad args to sqlite next-row") id
              <$> unmakeFFIValue "sqlite-result-handle" rs'
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
bCloseResultHandle [rs'] = do
      (_rs :: ResultHandle) <- maybe (error "bad args to sqlite close-result") id
              <$> unmakeFFIValue "sqlite-result-handle" rs'
      pure nothing
bCloseResultHandle _ = error "bad args to sqlite close-result"

bCloseHandle :: [Value] -> Interpreter Value
bCloseHandle [conn'] = do
    conn <- maybe (error "bad args to sqlite close") id
            <$> unmakeFFIValue "sqlite-handle" conn'
    liftIO $ S.close conn
    pure nothing
bCloseHandle _ = error "bad args to sqlite close"

sqlDataToBValue :: Value -> S.SQLData -> Interpreter Value
sqlDataToBValue mkSqlNumber (S.SQLInteger n) =
    app mkSqlNumber [NumV $ fromIntegral n]

sqliteTesting :: [Value] -> Interpreter Value
sqliteTesting _ = do
    liftIO $ putStrLn "sqlite testing"
    conn <- liftIO $ S.open ":memory:"
    liftIO $ S.execute_ conn (fromString "create table t1(a int, b text, c bool, d date)")
    _r :: [[S.SQLData]] <- liftIO $ S.query_ conn (fromString "select * from t1")
    pure nothing
