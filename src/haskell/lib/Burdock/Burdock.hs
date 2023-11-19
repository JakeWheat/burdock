
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
import Burdock.Utils (error, show)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Control.Monad.IO.Class (liftIO)

import Burdock.Runtime (Value)
import qualified Burdock.Parse as P
--import qualified Burdock.Syntax as S
import qualified Burdock.Rename as N
import qualified Burdock.Desugar as D
import qualified Burdock.Interpreter as I
import qualified Burdock.InterpreterSyntax as I
import qualified Burdock.Runtime as R
import Burdock.StaticError
    (--StaticError
    --,
        prettyStaticErrors)
import Burdock.Bootstrap (burdockBootstrapModule)
import qualified Burdock.PrettyInterpreter as I
--import Burdock.StaticError (StaticError)

import Burdock.ModuleMetadata
    (ModuleMetadata
    ,ModuleID(..)
    )

import qualified Text.RawString.QQ as R

import Data.IORef
    (IORef
    ,newIORef
    ,readIORef
    ,modifyIORef
    ,writeIORef
    )

import qualified System.FilePath as F
    (takeDirectory
    ,(</>)
    )
import qualified System.Directory as F
    (canonicalizePath
    ,getCurrentDirectory
    )

------------------------------------------------------------------------------

burdockPluginName :: Text
burdockPluginName = "file"

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
        bp <- burdockModulePlugin
        R.addModulePlugin burdockPluginName bp
    pure $ Handle st

{-
todo:
new plan is to create an _interpreter module from _bootstrap *
then the renamer will add this only
rename global to burdock2023 (later global will be a smaller module, like pyret's)
-}

quickImportModule :: Text -> R.Runtime ()
quickImportModule nm =
    R.addBinding nm =<< R.getModuleValue (R.ModuleID "haskell" [nm])

fileNameToModuleID :: Text -> ModuleID
fileNameToModuleID nm = ModuleID burdockPluginName [nm]

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
runScript h fn src = do
    putStrLn $ "got: " <> show fn
    R.runRuntime (hRuntimeState h) $ runScript' fn src

runScript' :: (Maybe Text) -> L.Text -> R.Runtime Value
runScript' fn src = do
    let mid = fileNameToModuleID (maybe "<unknown>" id fn)
    (_,iast) <- recurseAndCompileScript mid src
    R.withScope $ do
        -- todo: move this to the renamer when it can handle it
        quickImportModule "_bootstrap"
        I.interpBurdock iast

-- wrapper: will include recursively loading in the future
desugarScript :: Handle -> (Maybe Text) -> L.Text -> IO L.Text
desugarScript h fn src = do
    let mid = fileNameToModuleID (maybe "<unknown>" id fn)
    R.runRuntime (hRuntimeState h) $ do
        x <- recurseAndCompileScript mid src
        pure $ I.prettyStmts $ snd x

-- quick hack - will use the future ffi + fragments api
addModuleItems :: Value -> [(Text, Value)] -> R.Runtime Value
addModuleItems (R.Module is) es = pure $ R.Module $ is ++ es
addModuleItems x _ = error $ "wrong arg to addModuleItems: " <> R.debugShowValue x

getModuleIDFilename :: ModuleID -> Text
getModuleIDFilename (ModuleID _ [fn]) = fn
getModuleIDFilename (ModuleID _ as) = error $ "bad args to burdock moduleid " <> show as

loadAndCompileScript :: ModuleID -> R.Runtime (ModuleMetadata, [I.Stmt])
loadAndCompileScript mid = do
    src <- liftIO $ L.readFile (T.unpack $ getModuleIDFilename mid)
    recurseAndCompileScript mid src

recurseAndCompileScript :: ModuleID -> L.Text -> R.Runtime (ModuleMetadata, [I.Stmt])
recurseAndCompileScript mid src = do
    let fn = getModuleIDFilename mid
        ast = either error id $ P.parseScript fn src
        deps = N.getImportSources ast

    ism <- flip mapM deps $ \x -> (x,) <$> R.lookupImportSource (Just mid) x

    ctx <- flip mapM ism $ \(_,dmid) -> (dmid,) <$> R.getModuleMetadata dmid
    let (md,rast) = either (error . prettyStaticErrors) id $ N.rename fn ism ctx ast
        iast = either (error . prettyStaticErrors) id $ D.desugarScript fn rast
    pure $ (md,iast)


------------------------------------------------------------------------------

-- burdock module plugin
-- loads burdock modules from relative and absolute fs paths
-- can get the metadata for a module without running it - so you can
-- static check pure burdock source bases before executing any of it

data BurdockModulePluginCache
    = BurdockModulePluginCache
    {bmCache :: IORef [(ModuleID, ((ModuleMetadata, [I.Stmt]), Maybe Value))]}

addCompiledModule :: ModuleID -> (ModuleMetadata, [I.Stmt]) -> BurdockModulePluginCache -> R.Runtime ()
addCompiledModule nm md bm = liftIO $ modifyIORef (bmCache bm) ((nm,(md,Nothing)):)

addExecutedModule :: ModuleID -> Value -> BurdockModulePluginCache -> R.Runtime ()
addExecutedModule nm mdv bm = do
    bmv <- liftIO $ readIORef (bmCache bm)
    case lookup nm bmv of
        Nothing -> error $ "add executed module but no compiled module: " <> show nm
        Just (_,Just {}) -> error $ "add executed module attempted replace: " <> show nm
        Just (c,Nothing) -> liftIO $ writeIORef (bmCache bm) $ updateMap bmv nm (c,Just mdv)

updateMap :: Eq k => [(k, v)] -> k -> v -> [(k, v)]
updateMap l k v = (k, v) : filter (\a -> (fst a) /= k) l

-- get the module metadata and interpreter script from the cache
-- if it isn't there, load it, compile it, add it to the cache and return it
getCompiledModule :: ModuleID -> BurdockModulePluginCache -> R.Runtime (ModuleMetadata, [I.Stmt])
getCompiledModule fn bm = do
    bmv <- liftIO $ readIORef (bmCache bm)
    case lookup fn bmv of
        Just (m,_) -> pure m
        Nothing -> do
            md <- loadAndCompileScript fn
            addCompiledModule fn md bm
            pure md

-- the the module value from the cache
-- if it isn't there, get the compiled module info, run it, add it to the cache
-- and return. The getcompiledmodule will also load, compile and cache this it if it
-- isn't in the cache either
getModuleValue :: ModuleID -> BurdockModulePluginCache -> R.Runtime Value
getModuleValue fn bm = do
    bmv <- liftIO $ readIORef (bmCache bm)
    case lookup fn bmv of
        Just (_,Just v) -> pure v
        _ -> do
            (_,iast) <- getCompiledModule fn bm
            v <- R.withScope $ do
                -- todo: move this to the renamer when it can handle it
                quickImportModule "_bootstrap"
                I.interpBurdock iast
            addExecutedModule fn v bm
            pure v


(</>) :: Text -> Text -> Text
a </> b = T.pack ((F.</>) (T.unpack a) (T.unpack b))

canonicalizePath :: Text -> IO Text
canonicalizePath a = T.pack <$> F.canonicalizePath (T.unpack a)

getCurrentDirectory :: IO Text
getCurrentDirectory = T.pack <$> F.getCurrentDirectory

takeDirectory :: Text -> Text
takeDirectory a = T.pack (F.takeDirectory (T.unpack a))

burdockModulePlugin :: R.Runtime R.ModulePlugin
burdockModulePlugin = do
    bmc <- BurdockModulePluginCache <$> liftIO (newIORef [])

    let canonicaliseModuleID mctx (ModuleID nm as) =
            liftIO $ case as of
                [fn] -> do
                    b <- case mctx of
                        Nothing -> liftIO $ getCurrentDirectory
                        Just m -> pure $ takeDirectory $ getModuleIDFilename m
                    (\x -> ModuleID nm [x]) <$> canonicalizePath (b </> fn)
                _ -> error $ "bad args to burdock module id: " <> show as
                
    
    let gmd is = fst <$> getCompiledModule is bmc
        gmv is = getModuleValue is bmc
    pure $ R.ModulePlugin canonicaliseModuleID gmd gmv

------------------------------------------------------------------------------

-- burdock bootstrap scripts - bits implemented in burdock that are needed
-- by some of the interpreter features.

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
