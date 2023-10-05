


{-# LANGUAGE OverloadedStrings #-}
module Burdock.DemoHaskellModule
    (createModule
    ) where

import Burdock.Runtime
    (Runtime
    ,makeRecord
    ,makeNumber
    )

import Burdock.HaskellModulePlugin
    (HaskellModule(..)
    )

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )
    
createModule :: Runtime HaskellModule
createModule = do
     {-
    addBinding "a" $ makeNumber 2
    finishModule-}
    nm <- makeNumber 2
    let bs = [("a", (Nothing, BEIdentifier))]
    m <- makeRecord [("a", nm)]
    pure $ HaskellModule (pure (ModuleMetadata bs)) (pure m)
