

{-# LANGUAGE OverloadedStrings #-}
module Burdock.RuntimeBootstrap
    (createBootstrapHandle
    ,Value(..)

    ,ModuleMetadata
    ,ModulePlugin(..)
    ,RuntimeImportSource(..)

    ,getModuleMetadata
    ,getModuleValue
    ,addModulePlugin
    ,lookupImportSource
    ,BurdockImportSource(..)
    
    ,runRuntime
    ,Runtime
    ,liftIO
    ,Scientific

    ,debugShowValue

    ,RuntimeState
    ,emptyRuntimeState
    ,getRuntimeState
    ,addFFIType
    ,ffiTypeTagToValue
    ,makeFFIType
    
    ,setBootstrapRecTup
    ,BootstrapValues(..)
    
    ,withCallstackEntry

    ,makeFunctionValue
    ,makeValue
    ,extractValue

    ,DataDeclTypeTag(..)
    ,FFITypeTag
    ,tyName
    ,Env

    ,captureClosure
    
    ,nothingValue
    ,makeBurdockList
    ,extractBurdockList

    ,makeRecord
    
    ,makeTuple
    ,extractTuple

    ,makeVariant
    ,makeValueName
    ,makeString
    ,makeBool
    ,makeNumber

    ,variantName
    ,variantTag
    ,variantFields
    ,variantValueFields

    ,getFFITypeTag
    ,makeDataDeclTag
    ,dataDeclTagsEqual

    ,runTask
    ,throwValue

    ,getCallStack
    
    ,withScope
    ,withNewEnv
    ,addBinding
    ,lookupBinding

    ,createBurdockRunner

    ,getMember
    ,app

    ,addTestFail
    ,addTestPass
    ,getTestResults
    ,setTestModule
    ,doIsTest
    ,doIsNotTest

    --,ffimethodapp
    -- temp
    ,getTempEnvStage
    ,setTempEnvStage
    ) where

import Prelude hiding (error, putStrLn, show)

import Burdock.Runtime

import Burdock.HaskellModulePlugin
    (haskellModulePlugin
    --,addHaskellModule
    ,hmmModulePlugin
    ,HaskellModuleManager
    )

import Burdock.DefaultRuntime
    (initRuntime
    --,internals
    --,bootstrap
    )


createBootstrapHandle :: IO (RuntimeState, HaskellModuleManager)
createBootstrapHandle = do
    st <- emptyRuntimeState
    hp <- runRuntime st $ do
        hp <- haskellModulePlugin
        addModulePlugin "haskell" $ hmmModulePlugin hp

        tmpHackMetadata <- initRuntime

        setTempEnvStage tmpHackMetadata
        pure hp
    pure (st,hp)
