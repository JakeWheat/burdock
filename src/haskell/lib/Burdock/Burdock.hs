
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
    
    ,Handle
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error)

import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Control.Monad.IO.Class (liftIO)

import Burdock.Runtime (Value)
import qualified Burdock.Parse as P
--import qualified Burdock.Syntax as S
import qualified Burdock.Rename as N
import qualified Burdock.Desugar as D
import qualified Burdock.Interpreter as I
import qualified Burdock.InterpreterSyntax as I
import qualified Burdock.Runtime as R
import Burdock.StaticError (StaticError, prettyStaticErrors)
import Burdock.Bootstrap (burdockBootstrapModule)
import qualified Burdock.PrettyInterpreter as I
--import Burdock.StaticError (StaticError)

--import Burdock.ModuleMetadata
--    (ModuleMetadata
--    ,ModuleID)

import qualified Text.RawString.QQ as R

------------------------------------------------------------------------------

data Handle
    = Handle
    {hRuntimeState :: R.RuntimeState
    }

createHandle :: IO Handle
createHandle = do
    -- current arbitrary design decision is to handle all the bootstrapping
    -- from here, and not e.g. bootstrap partially internally within Runtime,
    -- then across one or more other bootstrap participating modules
    st <- R.makeRuntimeState
    R.runRuntime st $ do
        hp <- R.haskellModulePlugin
        R.addModulePlugin "haskell" $ R.hmmModulePlugin hp
        -- temp hack
        let quickAddModule nm m =
                let hm = R.HaskellModule (pure R.ModuleMetadata) m
                in R.addHaskellModule nm hm hp
        -- the plan: the bootstrap modules are what's needed to run all of the
        -- interpeter. _bootstrap is needed to interpret the list burdock source,
        -- then the list type is needed for construct, and the either type
        -- for run-task, etc. (to be reviewed in the future)
        quickAddModule "_bootstrap" (R.Module <$> burdockBootstrapModule)
        quickAddModule "_bootstrap-either" $ runScript' (Just "<bootstrap-either>") eitherScript
        quickAddModule "_bootstrap-list" $ do
            listV <- runScript' (Just "<bootstrap-list>") listScript
            addModuleItems listV [("make-burdock-list", R.Fun makeBurdockList)]
        -- hack for global, this will become a regular burdock module when that's
        -- implemented
        quickAddModule "global" $ runScript' (Just "<global>") globalSource
    pure $ Handle st

{-
todo:
new plan is to create an _interpreter module from _bootstrap *
then the renamer will add this only
rename global to burdock2023 (later global will be a smaller module, like pyret's)
-}

quickImportModule :: Text -> R.Runtime ()
quickImportModule nm =
    R.addBinding nm =<< R.getModuleValue Nothing (R.RuntimeImportSource "haskell" [nm])

globalSource :: L.Text
globalSource = [R.r|

# stand in for the future global module
# this is the module that's auto included into any script by default

# todo: there should be another default module with all the stuff in
# and global should be a limited module following pyret

use context empty

provide: *, type *, data * end

_bootstrap-either = _bootstrap.load-module("haskell", "_bootstrap-either")
_bootstrap-list = _bootstrap.load-module("haskell", "_bootstrap-list")

raise = _bootstrap.raise
print = _bootstrap.print
tostring = _bootstrap.tostring
true = _bootstrap.true
false = _bootstrap.false
_type-list = _bootstrap-list._type-list
_variant-link = _bootstrap-list._variant-link
_variant-empty = _bootstrap-list._variant-empty
is-list = _bootstrap-list.is-list
is-link = _bootstrap-list.is-link
is-empty = _bootstrap-list.is-empty
link = _bootstrap-list.link
empty = _bootstrap-list.empty

|]

runScript :: Handle -> (Maybe Text) -> L.Text -> IO Value
runScript h fn src = R.runRuntime (hRuntimeState h) $ runScript' fn src

runScript' :: (Maybe Text) -> L.Text -> R.Runtime Value
runScript' fn src = do
    let iast = either (error . prettyStaticErrors) id $ compileScript fn src
    R.withScope $ do
        -- todo: move this to the renamer when it can handle it
        quickImportModule "_bootstrap"
        I.interpBurdock iast

-- wrapper: will include recursively loading in the future
desugarScript :: Handle -> (Maybe Text) -> L.Text -> IO L.Text
desugarScript _ fn src = pure $ either (L.fromStrict . error . prettyStaticErrors) I.prettyStmts $ compileScript fn src

compileScript :: (Maybe Text) -> L.Text -> Either [StaticError] [I.Stmt]
compileScript fn' src =
    let fn = maybe "<unknown>" id fn'
        -- parse src
        ast = either error id $ P.parseScript fn src
        -- rename src
        (_,rast) = either (error . prettyStaticErrors) id $ N.rename fn [] [] ast
        -- desugar src
        iast = either (error . prettyStaticErrors) id $ D.desugarScript fn rast
    in pure iast


addModuleItems :: Value -> [(Text, Value)] -> R.Runtime Value
addModuleItems (R.Module is) es = pure $ R.Module $ is ++ es
addModuleItems x _ = error $ "wrong arg to addModuleItems: " <> R.debugShowValue x

eitherScript :: L.Text
eitherScript = [R.r|

use context empty

provide: *, type *, data * end

data either:
  | right(a)
  | left(b)
end

|]

listScript :: L.Text
listScript = [R.r|

use context empty

provide: *, type *, data * end

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

-- this function exists to bootstrap construct syntax
-- probably it's main justification is to make the desugared interpreter
-- syntax, and the code that generates it readable, so it's a bit of a
-- luxury
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
