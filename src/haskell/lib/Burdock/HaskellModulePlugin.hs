
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Burdock.HaskellModulePlugin
    (haskellModulePlugin
    ,addHaskellModule
    ,hmmModulePlugin
    ,HaskellModule(..)
    ,makeHaskellModule
    ,importModule
    ,S.ImportSource(..)
    ,ModuleMember(..)
    ,opaque
    ,makeFFIType
    ,HaskellModuleManager
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

import qualified Burdock.Syntax as S (ImportSource (..))

import Burdock.Runtime
    (Runtime
    ,ModulePlugin(..)
    ,ModuleMetadata
    ,Value
    ,RuntimeImportSource(..)
    ,liftIO
    ,makeRecord
    ,lookupImportSource
    ,BurdockImportSource(..)
    ,getModuleValue
    ,FFITypeTag
    ,tyName
    ,ffiTypeTagToValue
    ,makeFFIType
    )

import Data.Text (Text)
import Data.IORef
    (IORef
    ,newIORef
    ,readIORef
    ,modifyIORef)

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )

data HaskellModuleManager
    = HaskellModuleManager
    {hmmModulePlugin :: ModulePlugin
    ,hmmModules :: IORef [(Text, HaskellModule)]
    }

data HaskellModule
    = HaskellModule
    {hmGetMetadata :: Runtime ModuleMetadata
    ,hmGetModuleValue :: Runtime Value
    }

haskellModulePlugin :: Runtime HaskellModuleManager
haskellModulePlugin = do
    modulesRef <- liftIO $ newIORef []
    let lookupPlugin ris = do
            modules <- liftIO $ readIORef modulesRef
            let mnm = case ris of
                    RuntimeImportSource _ [mnmcunt] -> mnmcunt
                    _ -> error "unsupported haskell import source " <> show ris
            pure $ maybe (error $ "haskell module not found: " <> mnm) id
                $ lookup mnm modules

    let x = ModulePlugin
            {mpGetMetadata = \_ ris -> do
                r <- lookupPlugin ris
                hmGetMetadata r
            ,mpGetModuleValue = \_ ris -> do
                r <- lookupPlugin ris
                hmGetModuleValue r
            }
    pure $ HaskellModuleManager x modulesRef

addHaskellModule :: Text -> Runtime HaskellModule -> HaskellModuleManager -> Runtime ()
addHaskellModule nm hm hmm = do
    -- todo: figure out how to defer this to the first time the module is used
    hm' <- hm
    liftIO $ modifyIORef (hmmModules hmm) ((nm,hm'):)

data ModuleMember
    = Identifier Text Value
    | Type FFITypeTag

makeHaskellModule :: [ModuleMember] -> Runtime HaskellModule
makeHaskellModule bs = do
    let ms = flip map bs $ \case
            Identifier nm _ -> (nm, (Nothing, BEIdentifier))
            Type tg -> (tyName tg, (Nothing, BEType 0))
    m <- makeRecord =<< flip mapM bs (\case
            Identifier nm v -> pure (nm,v)
            Type tg -> do
                v <- ffiTypeTagToValue tg
                pure (tyName tg, v))
    pure $ HaskellModule (pure (ModuleMetadata ms [])) (pure m)

importModule :: S.ImportSource -> Runtime Value
importModule is = do
    ris <- lookupImportSource $ case is of
           S.ImportName nm -> BurdockImportName nm
           S.ImportSpecial p as -> BurdockImportSpecial p as
    getModuleValue Nothing ris

opaque :: Text -> Value -> Runtime Value
opaque _ _  = error $ "opaque value"
