
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
        eitherV <- runFragment Nothing eitherScript
        R.addBinding "_bootstrap-either" eitherV
        listV <- runFragment Nothing listScript
        listV' <- addModuleItems listV [("make-burdock-list", R.Fun makeBurdockList)]
        R.addBinding "_bootstrap-list" listV'
        nothingV <- runFragment Nothing nothingScript
        R.addBinding "_bootstrap-nothing" nothingV
        -- temp before renamer/modules/prelude added
        R.addBinding "true" $ R.Boolean True
        R.addBinding "false" $ R.Boolean False
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

addModuleItems :: Value -> [(Text, Value)] -> R.Runtime Value
addModuleItems (R.Module is) es = pure $ R.Module $ is ++ es
addModuleItems x _ = error $ "wrong arg to addModuleItems: " <> R.debugShowValue x

eitherScript :: L.Text
eitherScript = [R.r|

data either:
  | right(a)
  | left(b)
end

|]

listScript :: L.Text
listScript = [R.r|

data list:
  | link(first, rest)
  | empty
sharing:
  method _torepr(self):
    fun intercalate-items(l):
      cases l:
        | empty => ""
        | link(x, y) =>
          cases y:
            | empty => torepr(x)
            | else => torepr(x) + ", " + intercalate-items(y)
          end
      end
    end
    cases self:
      | empty => "[list: ]"
      | link(x,y) => "[list: "
          + intercalate-items(self) + "]"
    end
  end,
  method _plus(self,b):
    cases self:
      | empty => b
      | link(x,y) => link(x, y + b)
    end
  end,
  method length(self):
    cases self:
      | empty => 0
      | link(_,b) => 1 + b.length()
    end
  end ,
  method map(self,f):
    cases self:
      | empty => empty
      | link(a,b) => link(f(a), b.map(f))
    end
  end
end

|]


makeBurdockList :: [Value] -> R.Runtime Value
makeBurdockList us = do
    link <- getList "link"
    empty <- getList "empty"
    f link empty us
  where
    f _ empty [] = pure empty
    f link empty (v:vs) = do
        vs' <- f link empty vs
        R.app Nothing link [v, vs']
    getList nm = do
        Just b <- R.lookupBinding "_bootstrap-list"
        R.getMember Nothing b nm

    
nothingScript :: L.Text
nothingScript = "data Nothing: nothing end"
