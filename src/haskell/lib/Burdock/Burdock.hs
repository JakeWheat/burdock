
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Burdock.Burdock
    (liftIO
    ,createHandle
    ,runScript

    -- todo: make abstract
    ,Value(..)
    ,R.makeFFIValue
    ,R.extractFFIValue
    ,hRuntimeState

    -- what should user code do in these situations:
    ,R.debugShowValue
    ,R.getFFITypeInfoTypeInfo

    ,desugarScript
    ,desugarModule
    ,desugarFragment
    
    ,Handle
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error)

import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Control.Monad.IO.Class (liftIO)

import Burdock.Runtime (Value)
import qualified Burdock.Parse as P
import qualified Burdock.Syntax as S
import qualified Burdock.Rename as N
import qualified Burdock.Desugar as D
import qualified Burdock.Interpreter as I
import qualified Burdock.Runtime as R
import Burdock.StaticError (prettyStaticErrors)
import Burdock.Bootstrap (burdockBootstrapModule)
import qualified Burdock.PrettyInterpreter as I
import Burdock.StaticError (StaticError)

import Burdock.ModuleMetadata
    (ModuleMetadata
    ,ModuleID)

import qualified Text.RawString.QQ as R

------------------------------------------------------------------------------

data Handle
    = Handle
    {hRuntimeState :: R.RuntimeState
    }

createHandle :: IO Handle
createHandle = do
    st <- R.makeRuntimeState
    R.runRuntime st $ do
        sm <- burdockBootstrapModule
        R.addBinding "_bootstrap" (R.Module sm)
        em <- runFragment Nothing testScript
        R.addBinding "_bootstrap-either" em
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

runFragment :: (Maybe Text) -> L.Text -> R.Runtime Value
runFragment fn' src = do
    let fn = maybe "<unknown>" id fn'
        -- parse src
        ast = either error id $ P.parseScript fn src
        -- rename src
        (_,rast) = either (error . prettyStaticErrors) id $ N.renameModule True fn [] [] ast
        -- desugar src
        iast = either (error . prettyStaticErrors) id $ D.desugarScript fn rast
        -- interpret src
    I.interpBurdock iast

desugarScript :: Handle -> (Maybe Text) -> L.Text -> IO L.Text
desugarScript h fn' src = desugarIt N.renameScript h fn' src

desugarModule :: Handle -> (Maybe Text) -> L.Text -> IO L.Text
desugarModule h fn' src = desugarIt (N.renameModule False) h fn' src

-- fragment is a module, except if there are no provides, it defaults to
-- provide *, type *, data *
desugarFragment :: Handle -> (Maybe Text) -> L.Text -> IO L.Text
desugarFragment h fn' src = desugarIt (N.renameModule True) h fn' src

type RenameF = Text
             -> [(S.ImportSource, ModuleID)]
             -> [(ModuleID, ModuleMetadata)]
             -> S.Script
             -> Either [StaticError] (ModuleMetadata, S.Script)

desugarIt :: RenameF -> Handle -> (Maybe Text) -> L.Text -> IO L.Text
desugarIt renameF _h fn' src = do
    let fn = maybe "<unknown>" id fn'
        -- parse src
        ast = either error id $ P.parseScript fn src
        -- rename src
        (_,rast) = either (error . prettyStaticErrors) id $ renameF fn [] [] ast
        -- desugar src
        iast = either (error . prettyStaticErrors) id $ D.desugarScript fn rast
        -- interpret src
    pure $ I.prettyStmts iast

testScript :: L.Text
testScript = [R.r|

data either:
  | right(a)
  | left(b)
end

|]
