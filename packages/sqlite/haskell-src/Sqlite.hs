
module Sqlite (sqlitePackage) where



--import           Control.Applicative
--import qualified Data.Text as T
--import qualified Data.ByteString as B
import qualified Database.SQLite.Simple as S
--import Database.SQLite.Simple (NamedParam((:=)))
--import qualified Database.SQLite.Simple.FromRow as F

import Data.IORef
    (IORef
    ,newIORef
    ,readIORef
    ,writeIORef)

--import Data.Int (Int64)

import Data.String (fromString)

import Control.Monad (forM_)

import Control.Monad.Reader (liftIO)


import Data.Dynamic
    (--Dynamic
     toDyn
    ,fromDynamic
    --,Typeable
    )

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

--import Debug.Trace (trace)

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

data SqliteHandle = SqliteHandle S.Connection

type SqliteValue = S.SQLData

{-data SqliteValue
    = SQLInteger !Int64
    | SQLFloat !Double	 
    | SQLText !T.Text	 
    | SQLBlob !B.ByteString	 
    | SQLNull-}


------------------------------------------------------------------------------

-- burdock wrappers
-- todo: don't need two layers of wrappers

bOpen :: [Value] -> Interpreter Value
bOpen [TextV str] = do
    h <- liftIO $ open str
    pure $ FFIValue "sqlite-handle" $ toDyn h
bOpen _ = error "bad args to sqlite open"

bCommand :: [Value] -> Interpreter Value
bCommand [FFIValue "sqlite-handle" h', TextV cmd, args']
    | Just args <- mapM bSqlValueToSQLData =<< fromBList args'
    , Just h <- fromDynamic h'
    = do
      --liftIO $ putStrLn $ "ARGS: " ++ show args
      liftIO $ command h cmd args
      pure nothing
bCommand _ = error "bad args to sqlite command"

bQuery :: [Value] -> Interpreter Value
bQuery [FFIValue "sqlite-handle" h', TextV cmd, args']
    | Just args <- mapM bSqlValueToSQLData =<< fromBList args'
    , Just h <- fromDynamic h'
    = do
      rs <- liftIO $ query h cmd args
      pure $ FFIValue "sqlite-result-handle" $ toDyn rs
bQuery _ = error "bad args to sqlite query"

bNextRow :: [Value] -> Interpreter Value
bNextRow [none, somef, mkSqlNumber, FFIValue "sqlite-result-handle" rs']
    | Just rs <- fromDynamic rs'
    = do
      r <- liftIO $ nextRow rs
      case r of
          Nothing -> pure none
          Just vs -> do
              vs' <- mapM (sqlDataToBValue mkSqlNumber) vs
              let lst = makeBList vs'
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

bCloseResultHandle :: [Value] -> Interpreter Value
bCloseResultHandle [FFIValue "sqlite-result-handle" rs']
    | Just rs <- fromDynamic rs' = do
          liftIO $ closeResult rs
          pure nothing
bCloseResultHandle _ = error "bad args to sqlite close-result"

bCloseHandle :: [Value] -> Interpreter Value
bCloseHandle [FFIValue "sqlite-handle" rs']
    | Just rs <- fromDynamic rs' = do
          liftIO $ close rs
          pure nothing
bCloseHandle _ = error "bad args to sqlite close"



sqlDataToBValue :: Value -> S.SQLData -> Interpreter Value
sqlDataToBValue mkSqlNumber (S.SQLInteger n) =
    app mkSqlNumber [NumV $ fromIntegral n]

------------------------------------------------------------------------------

-- sqlite lib wrappers

-- todo: errors if the file doesn't exist
open :: FilePath -> IO SqliteHandle
open fn = SqliteHandle <$> S.open fn

-- creates an new db, errors if the file exists
_create :: FilePath -> IO SqliteHandle
_create = undefined

-- could also add direct access to the usually bad idea sqlite
-- behaviour of "create or open"

close :: SqliteHandle -> IO ()
close (SqliteHandle conn) = S.close conn

command :: SqliteHandle -> String -> [SqliteValue] -> IO ()
command (SqliteHandle conn) cmd args =
  S.execute conn (fromString cmd) args

-- prep and rerun

command_ :: SqliteHandle -> String -> IO ()
command_ h c = command h c []

data ResultHandle = ResultHandle (IORef [[SqliteValue]])

nextRow :: ResultHandle -> IO (Maybe [SqliteValue])
nextRow (ResultHandle rr) = do
    vr <- readIORef rr
    case vr of
        [] -> pure Nothing
        (v:vs) -> do
            writeIORef rr vs
            pure $ Just v

closeResult :: ResultHandle -> IO ()
closeResult _ = pure ()

query :: SqliteHandle -> String -> [SqliteValue] -> IO ResultHandle
query (SqliteHandle conn) q args = do
  r <- S.query conn (fromString q) args  --  :: IO [[SqliteValue]]
  ResultHandle <$> newIORef r

query_ :: SqliteHandle -> String -> IO ResultHandle
query_ h q = query h q []

-- not quite sure about the result type
_queryAll :: SqliteHandle -> String -> [SqliteValue] -> IO ([String], [[SqliteValue]])
_queryAll = undefined

_queryAll_ :: SqliteHandle -> String -> IO ([String], [[SqliteValue]])
_queryAll_ = undefined


_main :: IO ()
_main = do
    --_t2
    putStrLn "\n--------------\n"
    h <- open ":memory:"
    command_ h "create table t1(a int)"
    forM_ [1,2,3,4] $ \i->
        command h "insert into t1 values (?)" [S.SQLInteger i]
    rs <- query_ h "select * from t1"
    let lp = do
            v <- nextRow rs
            case v of
                Nothing -> pure ()
                Just row -> do
                    putStrLn $ show row
                    lp
    lp
    closeResult rs
    close h

{-
_t2 :: IO ()
_t2 = do
  conn <- S.open "test.db"
  S.execute conn (fromString "INSERT INTO test (str) VALUES (?)") (S.Only ("test string 2" :: String))
  S.execute conn (fromString "INSERT INTO test (id, str) VALUES (?,?)") (TestField 13 (fromString "test string 3"))
  rowId <- S.lastInsertRowId conn
  S.executeNamed conn (fromString "UPDATE test SET str = :str WHERE id = :id") [fromString ":str" := (fromString "updated str" :: T.Text), fromString ":id" := rowId]
  r <- S.query_ conn (fromString "SELECT * from test") :: IO [TestField]
  mapM_ print r
  S.execute conn (fromString "DELETE FROM test WHERE id = ?") (S.Only rowId)
  S.close conn
--main :: IO ()
--main = do
  --T.defaultMain $ T.testGroup "group" []
-}
