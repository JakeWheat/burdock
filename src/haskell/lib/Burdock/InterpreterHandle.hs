
{-
Wrapper to provide a user api to the embedded language

also implements the burdock language plugin to the runtime

this is also where the recursive loading of modules is implemented

adds some debug support

-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Burdock.InterpreterHandle
    (createHandle
    ,runRuntime
    ,runScript
    ,getModuleValue
    ,getTestResults
    ,liftIO
    ,runTask
    ,debugShowValue
    ,extractValue
    ,DumpMode(..)
    ,dumpSource
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)
import Data.Text.IO (putStrLn)

import qualified Burdock.InterpreterSyntax as I
import Burdock.RuntimeBootstrap
    (Runtime
    ,RuntimeState
    --,emptyRuntimeState
    ,getRuntimeState
    ,runRuntime
    ,liftIO
    ,createBootstrapHandle

    ,setTempEnvStage
    ,getTempEnvStage
    ,lookupBinding
    ,lookupType
    ,setBootstrapRecTup
    ,BootstrapValues(..)

    ,FFITypeTag
    
    ,Value

    ,withScope
    ,addModulePlugin
    ,ModulePlugin(..)
    ,RuntimeImportSource(..)
    ,lookupImportSource
    ,BurdockImportSource(..)
    ,getModuleMetadata
    ,getModuleValue
    ,getTestResults
    ,runTask
    ,debugShowValue
    ,extractValue
    ,setTestModule
    )

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import Burdock.Parse (parseScript)
import Burdock.Desugar
    (desugarScript
    ,desugarModule
    ,getImportSources
    ,ModuleID(..)
    )

import qualified Burdock.Syntax as S

import Control.Monad
    (when
    ,forM
    )

import Data.IORef
    (IORef
    ,newIORef
    ,readIORef
    ,modifyIORef
    )

import Text.Show.Pretty (ppShow)
import Burdock.InterpreterPretty (prettyStmts)

import System.Directory
    (canonicalizePath
    ,getCurrentDirectory
    )
import System.FilePath
    (takeDirectory
    ,(</>)
    )

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(BEIdentifier)
    )

import Burdock.Pretty (prettyScript)

import Burdock.Renamer
    (renameScript
    ,renameModule
    ,prettyStaticErrors
    )

-- temp
import Burdock.DemoHaskellModule (demoHaskellModule)
import Burdock.TestHelpers (testHelpers)
import Burdock.HaskellModulePlugin
    (addHaskellModule
    ,importModule
    ,ImportSource(..)
    )

import Burdock.Interpreter
    (interpBurdock
    )

import qualified Text.RawString.QQ as R

------------------------------------------------------------------------------

debugPrintUserScript :: Bool
debugPrintUserScript = False

debugPrintBootstrap :: Bool
debugPrintBootstrap = False

debugPrintInternals :: Bool
debugPrintInternals = False

------------------------------------------------------------------------------

createHandle :: IO RuntimeState
createHandle = do
    -- get the handle with the bootstrap ffi set up
    (st,hp) <- createBootstrapHandle
    runRuntime st $ do
        -- add the burdock module handler, then run the initial burdock scripts
        ModuleMetadata bootstrapMetadata' _ <- getTempEnvStage
        -- hack cos list construct is implemented in desugar atm
        let bootstrapMetadata = ModuleMetadata (("list", (Nothing, BEIdentifier)) : bootstrapMetadata') []
        
        mp <- burdockModulePlugin
        addModulePlugin "file" mp

        (bmm,_) <- runScript' debugPrintBootstrap (Just "_bootstrap") bootstrap

        let lkpf f = maybe (error $ "_bootstrap " <> f <> " not found") id <$> lookupBinding f
            typeTag t = do
                v <- maybe (error $ "_bootstrap " <> t  <> " not found") id <$> lookupType t
                case extractValue v of
                    Just (t' :: FFITypeTag) -> pure t'
                    Nothing -> error $ "_bootstrap type " <> t <> " looked up to: " <> debugShowValue v

        setBootstrapRecTup =<< BootstrapValues
            <$> lkpf "empty"
            <*> lkpf "link"
            <*> lkpf "nothing"
            <*> typeTag "string"
        
        -- temp: add the bootstrap ffi + bootstrap burdock metadata
        let tempCombineModuleMetadata (ModuleMetadata a ads) (ModuleMetadata b bds) = ModuleMetadata (a ++ b) (ads ++ bds)
        setTempEnvStage $ tempCombineModuleMetadata bootstrapMetadata bmm
        (imm,_) <- runScript' debugPrintInternals (Just "_internals") internals

        -- temp: add metadata
        setTempEnvStage $ tempCombineModuleMetadata (tempCombineModuleMetadata bootstrapMetadata bmm) imm

        addHaskellModule "demo-haskell-module" demoHaskellModule hp
        addHaskellModule "test-helpers" testHelpers hp

        -- todo: figure out how to only load the testing module when it is used
        testing <- importModule $ ImportName ["testing"]
        setTestModule testing

            -- todo: tests in the internals?
        getRuntimeState

runScript :: Maybe T.Text -> L.Text -> Runtime Value
runScript fn src = snd <$> runScript' debugPrintUserScript fn src

--------------------------------------

-- quick hack functions to dump renamed or desugared source

data DumpMode
    = DumpRenameScript
    | DumpRenameModule
    | DumpDesugarScript
    | DumpDesugarModule

prepDump :: Maybe Text
         -> L.Text
         -> (Text
             -> S.Script
             -> ModuleMetadata
             -> [(S.ImportSource, ModuleID)]
             -> [(ModuleID, ModuleMetadata)]
             -> Runtime a)
         -> Runtime a
prepDump fn' src res = do
    let fn = maybe "unnamed" id fn'
        ast = either error id $ parseScript fn src
    ms <- recurseMetadata (Just fn) ast
    tmpHack <- getTempEnvStage
    let (ism,ms') = unwrapMetadata ms
    res fn ast tmpHack ism ms'

dumpSource :: DumpMode -> Maybe T.Text -> L.Text -> Runtime L.Text
dumpSource DumpRenameScript fn' src =
    prepDump fn' src $ \fn ast tmpHack ism ms' ->
    case renameScript fn tmpHack ism ms' ast of
        Left  e -> error $ prettyStaticErrors e
        Right (_,res) -> pure $ prettyScript res

dumpSource DumpRenameModule fn' src =
    prepDump fn' src $ \fn ast tmpHack ism ms' ->
    case renameModule fn tmpHack ism ms' ast of
        Left  e -> error $ prettyStaticErrors e
        Right (_,res) -> pure $ prettyScript res

dumpSource DumpDesugarScript fn' src =
    prepDump fn' src $ \fn ast tmpHack ism ms' -> do
    let (_,dast) = desugarScript tmpHack fn ism ms' ast
    pure $ prettyStmts dast

dumpSource DumpDesugarModule fn' src =
    prepDump fn' src $ \fn ast tmpHack ism ms' -> do
    let (_,dast) = desugarModule tmpHack fn ism ms' ast
    pure $ prettyStmts dast
    
------------------------------------------------------------------------------

runScript' :: Bool -> Maybe T.Text -> L.Text -> Runtime (ModuleMetadata, Value)
runScript' debugPrint fn' src = do
    -- filename either comes from bootstrap or from user
    fn <- T.pack <$> liftIO (canonicalizePath $ maybe "unnamed" T.unpack fn')
    let ast = either error id $ parseScript fn src
    ms <- recurseMetadata (Just fn) ast
    --liftIO $ putStrLn $ "desugar script"
    tmpHack <- getTempEnvStage
    let (ism,ms') = unwrapMetadata ms
    let (mm,dast) = desugarScript tmpHack fn ism ms' ast
    when False $ liftIO $ putStrLn $ T.pack $ ppShow dast
    when debugPrint $ liftIO $ L.putStrLn $ prettyStmts dast
    (mm,) <$> interpBurdock dast

loadAndDesugarModule :: Text -> Runtime (ModuleMetadata, [I.Stmt])
loadAndDesugarModule fn = do
    --liftIO $ putStrLn $ "load " <>  fn
    src <- liftIO $ L.readFile (T.unpack fn)
    let ast = either error id $ parseScript fn src
    ms <- recurseMetadata (Just fn) ast
    --liftIO $ putStrLn $ "desugar " <> fn
    tmpHack <- getTempEnvStage
    let (ism,ms') = unwrapMetadata ms
    pure $ desugarModule tmpHack fn ism ms' ast

recurseMetadata :: Maybe Text -> S.Script -> Runtime [(S.ImportSource, ModuleID, ModuleMetadata)]
recurseMetadata ctx ast = do
    let deps = getImportSources ast
    is <- flip mapM deps $ \x -> case x of
        S.ImportName nm -> (x,) <$> lookupImportSource (BurdockImportName nm)
        S.ImportSpecial p as -> (x,) <$> lookupImportSource (BurdockImportSpecial p as)
    forM is $ \x -> case x of
        (bis, ris) ->
            (bis, ModuleID (risImportSourceName ris) (risArgs ris),)
            <$> getModuleMetadata ctx ris

unwrapMetadata :: [(S.ImportSource, ModuleID, ModuleMetadata)]
               -> ([(S.ImportSource, ModuleID)], [(ModuleID, ModuleMetadata)])
unwrapMetadata = unzip . map (\(is,mid,mm) -> ((is,mid),(mid,mm)))


data BurdockModulePluginCache
    = BurdockModulePluginCache
    {cacheCompiled :: IORef [(Text, (ModuleMetadata, [I.Stmt]))]
    ,cacheModuleValues :: IORef [(Text, Value)]
    }

burdockModulePlugin :: Runtime ModulePlugin
burdockModulePlugin = do
    let nx = liftIO $ newIORef []
    pc <- BurdockModulePluginCache <$> nx <*> nx
    let --resolveModulePath :: Maybe Text -> Text -> Runtime Text
        resolveModulePath ctx fn = do
            ctx' <- case ctx of
                Nothing -> liftIO $ getCurrentDirectory
                Just x -> pure $ takeDirectory $ T.unpack x
            liftIO (T.pack <$> (canonicalizePath (ctx' </> T.unpack fn)))
            
        compileCacheModule fn = do
            v <- liftIO $ readIORef (cacheCompiled pc)
            case lookup fn v of
                Just v' -> pure v'
                Nothing -> do
                    vs <- loadAndDesugarModule fn
                    liftIO $ modifyIORef (cacheCompiled pc) ((fn,vs):)
                    pure vs
        getMetadata' ctx ri = do
            fn <- resolveModulePath ctx $ getRiFile ri
            (m,_) <- compileCacheModule fn
            pure m
        getModuleValue' ctx ri = do
            fn <- resolveModulePath ctx $ getRiFile ri
            v <- liftIO $ readIORef (cacheModuleValues pc)
            case lookup fn v of
                Just v' -> pure v'
                Nothing -> do
                    (_,dast) <- compileCacheModule fn
                    v' <- withScope $ interpBurdock dast
                    liftIO $ modifyIORef (cacheModuleValues pc) ((fn,v'):)
                    pure v'
    pure $ ModulePlugin getMetadata' getModuleValue'
  where
    getRiFile = \case 
        RuntimeImportSource _ [fn] -> fn
        RuntimeImportSource _ as -> error $ "bad args to burdock module source: " <> show as
        

------------------------------------------------------------------------------

-- temp hack, before modules implemented, have bootstrap burdock
-- then internals burdock, that are run in a handle when it's created
-- before any user code

bootstrap :: L.Text
bootstrap = [R.r|

provide:
  *
end

data Nothing: nothing end

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

---------------------------------------

internals :: L.Text
internals = [R.r|

provide:
  *
end

##################
# built in stuff
           
data Either:
  | left(v)
  | right(v)
end

_run-task-fixup = lam(x):
    cases x:
      | left(es) => left(es.exception)
      | _ => x
    end
  end


##################
# more built in stuff

#repeat :: (n :: Number, e :: a) -> List<a>
fun repeat(n,e):
  if n > 0:
    link(e, repeat(n - 1, e))
  else:
    empty
  end
end

# temp
fun make-module(m):
  m
end

  |]
