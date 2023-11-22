
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
--import Data.Text.IO (putStrLn)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Control.Monad.IO.Class (liftIO)

import Burdock.Runtime (Value)
import qualified Burdock.Parse as P
--import qualified Burdock.Syntax as S
import qualified Burdock.RenameAst as N
import qualified Burdock.Desugar as D
import qualified Burdock.Interpret as I
import qualified Burdock.InterpreterSyntax as I
import qualified Burdock.Runtime as R
import Burdock.StaticError
    (--StaticError
    --,
        prettyStaticErrors)
import Burdock.FFIModules.Bootstrap (burdockBootstrapModule)
import Burdock.FFIModules.Internals (burdockInternalsModule)
import qualified Burdock.InterpreterPretty as I
--import Burdock.StaticError (StaticError)

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,ModuleID(..)
    ,BindingMeta(..)
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

{-

current arbitrary design decision is to handle all the bootstrapping
from here, and not e.g. bootstrap partially internally within Runtime,
then across one or more other bootstrap participating modules

The bootstrap process mainly has to deal with the fact that the interpreter needs
list and either types, and especially list would be very tedious to implement in ffi.

So the system bootstraps by loading the internal _bootstrap module
this allows most of the interpreter to work, as long as it's imported
under the _interpreter alias.

this is used to create the either and list types from burdock source
these are needed for the remaining language features:
list for construct
either for run-task

then these modules are combined into the _interpreter module, which
is automatically imported by the renamer outside of bootstrapping

currently this file contains 4 burdock source fragments:
bootstrap-either
bootstrap-list
_interpreter
burdock2023

in theory, all of them could just be .bur files in the builtins directory,
  which might be a better way to do it?

-}

createHandle :: IO Handle
createHandle = do
    st <- R.makeRuntimeState
    R.runRuntime st $ do
        hp <- R.haskellModulePlugin
        R.addModulePlugin "haskell" $ R.hmmModulePlugin hp
        let bootstrapLoadModule nm (mm, mv) = do
                -- the system designed to load modules only if they are used
                -- in this case, we definitely want them loaded, so force loading now
                let hm = R.HaskellModule (pure mm) (pure mv)
                R.addHaskellModule nm hm hp
        bootstrapLoadModule "_bootstrap" =<< burdockBootstrapModule
        bootstrapLoadModule "_bootstrap-either" =<< runScript' (Just "<bootstrap-either>") eitherScript
        bootstrapLoadModule "_bootstrap-list" =<< do
            (ModuleMetadata mm1, listV) <- runScript' (Just "<bootstrap-list>") listScript
            -- todo: closure capture helper fns
            llink <- R.getMember Nothing listV "link"
            lempty <- R.getMember Nothing listV "empty"
            let mbl = makeBurdockList llink lempty
            mv <- addModuleItems listV [("make-burdock-list", R.Fun mbl)]
            let mm2 = ModuleMetadata $ ("make-burdock-list", Nothing, BEIdentifier, ("<bootstrap-list>",Nothing)) : mm1
            -- todo: add the make-burdock-list metadata entry
            pure (mm2, mv)
        bootstrapLoadModule "_interpreter" =<< runScript' (Just "<interpreter>") interpreterSupportScript
        -- system now bootstrapped to the point where can use regular _interpreter module
        -- so use context base is working
        bp <- burdockModulePlugin
        R.addModulePlugin burdockPluginName bp
        bootstrapLoadModule "_internals" =<< burdockInternalsModule
        bootstrapLoadModule "burdock2023" =<< runScript' (Just "<burdock2023>") burdock2023Source
        -- system now bootstrapped to be able to use default use context burdock2023

    pure $ Handle st

_quickImportModule :: Text -> R.Runtime ()
_quickImportModule nm =
    R.addBinding nm =<< R.getModuleValue (R.ModuleID "haskell" [nm])

fileNameToModuleID :: Text -> ModuleID
fileNameToModuleID nm = ModuleID burdockPluginName [nm]

runScript :: Handle -> (Maybe Text) -> L.Text -> IO Value
runScript h fn src = R.runRuntime (hRuntimeState h) (snd <$> runScript' fn src)

runScript' :: (Maybe Text) -> L.Text -> R.Runtime (ModuleMetadata, Value)
runScript' fn src = do
    let mid = fileNameToModuleID (maybe "<unknown>" id fn)
    (mm,iast) <- recurseAndCompileScript mid src
    {-liftIO $ L.putStrLn $ L.unlines
        ["===================================="
        ,L.fromStrict (maybe "unk" id fn)
        ,I.prettyStmts iast
        ,"===================================="]-}
    (mm,) <$> R.withScope (I.interpBurdock iast)

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
            v <- R.withScope $ I.interpBurdock iast
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
    let canonicalizeModuleID mctx (ModuleID nm as) =
            liftIO $ case as of
                [fn] -> do
                    b <- case mctx of
                        Nothing -> liftIO $ getCurrentDirectory
                        Just m -> pure $ takeDirectory $ getModuleIDFilename m
                    (\x -> ModuleID nm [x]) <$> canonicalizePath (b </> fn)
                _ -> error $ "bad args to burdock module id: " <> show as
    let gmd is = fst <$> getCompiledModule is bmc
        gmv is = getModuleValue is bmc
    pure $ R.ModulePlugin canonicalizeModuleID gmd gmv

------------------------------------------------------------------------------

-- burdock bootstrap scripts - bits implemented in burdock that are needed
-- by some of the interpreter features.

interpreterSupportScript :: L.Text
interpreterSupportScript = [R.r|

# this is all the stuff under the _interpreter module which is
# what the interpreter itself needs to run everything

# context _bootstrap-interpreter is used to load modules before the completed
# _interpreter module is ready
use context _bootstrap-interpreter

# load the modules which will reexport to create the interpreter module

# need the bootstrap in scope as _interpreter?
import haskell("_bootstrap") as _interpreter
import haskell("_bootstrap-either") as _bootstrap-either
import haskell("_bootstrap-list") as _bootstrap-list

provide from _interpreter:
  *, type *, data *
end

provide from _bootstrap-either:
  *, type *, data *
end

provide from _bootstrap-list:
  *, type *, data *
end

|]

burdock2023Source :: L.Text
burdock2023Source = [R.r|

# start of the default convenience module that's auto included into any
# script by default

use context _base

# bring the _interpreter namespace into scope for user use
import haskell("_interpreter") as _interpreter
provide from _interpreter:

  # todo: needs extras for boolean
  true,
  false,

  data list,
  data either,

  # todo:needs extras for nothing
  nothing,

  raise,
  not
end

import haskell("_internals") as _internals
provide from _internals:
  print,
  tostring
end

|]

eitherScript :: L.Text
eitherScript = [R.r|

use context _bootstrap-interpreter

provide: *, type *, data * end

import haskell("_bootstrap") as _interpreter

data either:
  | right(a)
  | left(b)
end

|]

listScript :: L.Text
listScript = [R.r|

use context _bootstrap-interpreter

provide: *, type *, data * end

import haskell("_bootstrap") as _interpreter

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
            | empty => x.torepr()
            | else => x.torepr() + ", " + intercalate-items(y)
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
makeBurdockList :: Value -> Value -> ([Value] -> R.Runtime Value)
makeBurdockList link empty us = f us
  where
    f [] = pure empty
    f (v:vs) = do
        vs' <- f vs
        R.app Nothing link [v, vs']
