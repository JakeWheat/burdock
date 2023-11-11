
{-

built in ffi module used to bootstrap the interpreter/handle

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.System
    (burdockSystemModule
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error, show)
import Data.Text.IO (putStrLn)

import Control.Arrow (first, second)

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import Data.Dynamic
    (toDyn
    ,fromDynamic
    ,Typeable
    )

import Data.IORef
    (IORef
    ,readIORef
    ,modifyIORef
    ,newIORef
    )

import qualified Burdock.Runtime as R
import Burdock.Runtime (Value)
import Burdock.Scientific (extractInt, Scientific)

------------------------------------------------------------------------------

burdockSystemModule :: R.Runtime [(Text, Value)]
burdockSystemModule = do

    -- create ffitypetag burdock type
    ffitagInBurdock <- makeFFIType "ffi-tag"
        [ToRepr $ \v -> pure $ "<" <> R.tyName v <> ">"
        -- to be fixed to use the unique type id
        ,Equals $ \v w -> pure $ R.tyName v == R.tyName w
        ]
    -- todo: put the above in the runtime, so it's always available
    -- then it'll just get bound to a name in burdock here

    -- create number type
    burdockNumber <- makeFFIType "number1"
        -- have to put the type in somewhere
        [ToRepr $ \(v :: Scientific) -> pure $ show v
        ,Compare $ (pure .) . compare
        -- arith
        ,Arith $ ArithM
        -- this is idiomatic so it's actually OK to write like this:
         {aAdd = (pure .) . (+)
         ,aSub = (pure .) . (-)
         ,aMult = (pure .) . (*)
         ,aDiv = (pure .) . (/)
         }
        ]

    -- is-number -> implement via is-ffi-type function with closure
    
    tg <- myMakeValueTag

    testLog <- liftIO $ newIORef (0,0)

    pure [("_type-ffitag", R.FFIValue ffitagInBurdock $ toDyn ffitagInBurdock)
         ,("_type-number", R.FFIValue ffitagInBurdock $ toDyn burdockNumber)
         
         ,("run-binary-test", R.Fun (bRunBinaryTest testLog))
         ,("demo-make-val", R.Fun (demoMakeVal tg))
         ,("print", R.Fun bPrint)
         ,("get-test-passes", R.Fun (getTestVal testLog 0))
         ,("get-test-failures", R.Fun (getTestVal testLog 1))
         ]

{-

planning:
ffitypetag
datadecltag
number
string
boolean + true/false pattern matching (will try to implement as datadecl)

the above need is-X variations

+ types, is-X and other support for: tuple, record, module, box?, function, method

nothing

list
either

run-task + variations

raise
load-module

data decl support, roughly
haskell-list ffitype

make-datadecl-tag, make haskell list, make variant, is type, is
variant, check variants equal, show variant

this is roughly the core of the _system module that's needed to run
the language itself

-}
    
------------------------------------------------------------------------------

runBinaryTest :: IORef (Int,Int)
              -> Text
              -> R.Value
              -> R.Value
              -> R.Value
              -> Text
              -> R.Runtime ()
runBinaryTest tally msg v0 v1 op opFailString = do
    -- todo: get the original source positions in here
    v0' <- R.runTask $ R.app Nothing v0 []
    v1' <- R.runTask $ R.app Nothing v1 []
    case (v0',v1') of
        (Right v0'', Right v1'') -> do
            -- todo: have to run-task the call to op
            r <- R.app Nothing op [v0'', v1'']
            case r of
                R.Boolean True -> liftIO $ do
                    liftIO $ modifyIORef tally (second (+1))
                    liftIO $ putStrLn $ "PASS: " <> msg
                R.Boolean False -> do
                    liftIO $ modifyIORef tally (first (+1))
                    -- todo: have to runtask the call to showValue
                    sv0 <- R.showValue v0''
                    sv1 <- R.showValue v1''
                    liftIO $ putStrLn $ T.unlines
                        ["FAIL: " <> msg
                        ,sv0
                        ,opFailString
                        ,sv1]
                _ -> error $ "non bool from test predicate: " <> R.debugShowValue r
        (Left er0, Left er1) ->
            liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"LHS raised: " <> er0
                 ,"RHS raised: " <> er1]
        (Left er0, Right {}) ->
            liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"LHS raised: " <> er0]
        (Right {}, Left er1) ->
            liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"RHS raised: " <> er1]

getTestVal :: IORef (Int,Int) -> Int -> [Value] -> R.Runtime Value
getTestVal = undefined

bRunBinaryTest :: IORef (Int,Int) -> [Value] -> R.Runtime Value
bRunBinaryTest tally [R.BText msg
                     ,v0
                     ,v1
                     ,op
                     ,R.BText opFailString] = do
    runBinaryTest tally msg v0 v1 op opFailString
    pure R.BNothing
    
bRunBinaryTest _ _ = error $ "bad args to bRunBinaryTest"

------------------------------------------------------------------------------

data MyType = MyType Int
    deriving (Show,Eq,Ord)

myMakeValueTag :: R.Runtime R.FFITypeTag
myMakeValueTag =
    makeFFIType "my-type"
        [ToRepr $ \(v :: MyType) -> pure $ show v
        --,Equals $ \(v :: MyType) w -> pure $ v == w
        ,Compare $ \(v :: MyType) w -> pure $ compare v w
        -- arith
        ,Arith $ ArithM
         {aAdd = \(MyType a) (MyType b) -> pure $ MyType (a + b)
         ,aSub = \(MyType a) (MyType b) -> pure $ MyType (a - b)
         ,aMult = \(MyType a) (MyType b) -> pure $ MyType (a * b)
         ,aDiv = \(MyType a) (MyType b) -> pure $ MyType (a `div` b)
         }
        ]

demoMakeVal :: R.FFITypeTag -> [Value] -> R.Runtime Value
demoMakeVal tg [R.Number n] = do
    -- tg <- myMakeValueTag
    pure $ R.FFIValue tg $ toDyn $ MyType $ maybe (error "balls") id $ extractInt n

--------------------------------------

bPrint :: [Value] -> R.Runtime Value
bPrint [R.BText t] = do
    liftIO $ putStrLn t
    pure R.BNothing
bPrint [v] = do
    f <- R.getMember Nothing v "_torepr"
    -- todo: how do functions get a source position from syntax
    -- if that's how they are called?
    r <- R.app Nothing f []
    case r of
        R.BText t -> liftIO $ putStrLn t
        _ -> error $ "non text returned from x._torepr()" <> R.debugShowValue v <> " " <> R.debugShowValue r
    pure R.BNothing

------------------------------------------------------------------------------

-- this bit to move, probably to Runtime
-- todo: add some more helpers - for generic functions
--   for other kinds of methods
--   for fields on ffi types
--   for the other ffi behaviour: assign, app, others?

makeFFIType :: Typeable a => Text -> [FFIValueEntry a] -> R.Runtime R.FFITypeTag
makeFFIType nm meths = do
    -- todo: need to generate a type id from the runtime first
    -- then use this when extracting values to make sure they're the
    -- right type
    let meths' :: [(Text, Value -> R.Runtime Value)]
        meths' = concat $ flip map meths $ \case
           ToRepr f -> [("_torepr",\case
               R.FFIValue _ v
                   | Just v' <- fromDynamic v ->
                     pure $ R.Fun $ \[] -> R.BText <$> f v')]
           Equals f -> [("_equals", \case
               R.FFIValue _ v
                   | Just v' <- fromDynamic v ->
                     pure $ R.Fun $ \case
                         [R.FFIValue _ w] | Just w' <- fromDynamic w -> R.Boolean <$> f v' w'
                         [_] -> pure $ R.Boolean False)]
           Compare f ->
               let mk1 nm fn = (nm, \case
                    R.FFIValue _ v
                        | Just v' <- fromDynamic v ->
                          pure $ R.Fun $ \case
                              [R.FFIValue _ w] | Just w' <- fromDynamic w -> R.Boolean . fn <$> f v' w')
               in [("_equals", \case
                    R.FFIValue _ v
                        | Just v' <- fromDynamic v ->
                          pure $ R.Fun $ \case
                              [R.FFIValue _ w] | Just w' <- fromDynamic w -> R.Boolean . (==EQ) <$> f v' w'
                              [_] -> pure $ R.Boolean False)
                  ,mk1 "_lessthan" (==LT)
                  ,mk1 "_lessequal" (`elem` [LT,EQ])
                  ,mk1 "_greaterequal" (`elem` [GT,EQ])
                  ,mk1 "_greaterthan" (==GT)
                  ]
           Arith (ArithM myadd mysub mymul mydiv) ->
               let mk1 nm fn = (nm, \case
                    R.FFIValue _ v
                        | Just v' <- fromDynamic v ->
                          pure $ R.Fun $ \case
                              [R.FFIValue _ w] | Just w' <- fromDynamic w -> R.FFIValue tg . toDyn <$> fn v' w')
               in [mk1 "_plus" myadd
                  ,mk1 "_minus" mysub
                  ,mk1 "_times" mymul
                  ,mk1 "_divide" mydiv
                  ]
        memFn nm = case lookup nm meths' of
            Nothing -> error $ "field not found:" <> nm
            Just x -> x
        -- really relying on haskell laziness here
        tg = R.FFITypeTag {R.tyName = nm
                          ,R.tyMemberFn = memFn}
    pure tg

data FFIValueEntry a
    = ToRepr (a -> R.Runtime Text)
    | Equals (a -> a -> R.Runtime Bool)
    | Compare (a -> a -> R.Runtime Ordering)
    | Arith (Arith a)

data Arith a =
    ArithM
    {aAdd :: a -> a -> R.Runtime a
    ,aSub :: a -> a -> R.Runtime a
    ,aMult :: a -> a -> R.Runtime a
    ,aDiv :: a -> a -> R.Runtime a
    }
