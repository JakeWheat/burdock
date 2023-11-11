
{-# LANGUAGE OverloadedStrings #-}
module Burdock.Burdock
    (liftIO
    ,createHandle
    ,runScript
    -- todo: make abstract
    ,Value(..)
    -- what should user code do in these situations:
    ,R.debugShowValue
    
    ,Handle
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error)

import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Control.Monad.IO.Class (liftIO)

import Burdock.Runtime (Value)
import qualified Burdock.Parse as P
import qualified Burdock.Rename as N
import qualified Burdock.Desugar as D
import qualified Burdock.Interpreter as I
import qualified Burdock.Runtime as R
import Burdock.StaticError (prettyStaticErrors)
import Burdock.System (burdockSystemModule)
    
------------------------------------------------------------------------------

data Handle
    = Handle
    {hRuntimeState :: R.RuntimeState
    }

createHandle :: IO Handle
createHandle = do
    st <- R.makeRuntimeState
    R.runRuntime st $ do
        sm <- burdockSystemModule
        R.addBinding "_system" (R.Module sm)
    pure $ Handle st

runScript :: Handle -> (Maybe Text) -> L.Text -> IO Value
runScript h fn' src = do
    let fn = maybe "<unknown>" id fn'
        -- parse src
        ast = either error id $ P.parseScript fn src
        -- rename src
        (_,rast) = either (error . prettyStaticErrors) id $ N.renameScript fn [] [] ast
        -- desugar src
        iast = either (error . prettyStaticErrors) id $ D.desugarScript fn rast
        -- interpret src
    R.runRuntime (hRuntimeState h) $ I.interpBurdock iast
