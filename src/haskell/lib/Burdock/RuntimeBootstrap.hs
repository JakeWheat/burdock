

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    ,tupleGet

    ,variantName
    ,variantTag
    ,variantFields
    ,variantValueFields

    ,makeDataDeclTag
    ,dataDeclTagsEqual

    ,runTask
    ,throwValue

    ,getCallStack
    
    ,withScope
    ,withNewEnv
    ,addBinding
    ,lookupBinding
    ,lookupType

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

    ,initRuntime
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

import Burdock.Runtime
    (RuntimeState
    ,Runtime
    ,emptyRuntimeState
    ,runRuntime
    ,getRuntimeState

    ,setTempEnvStage
    ,setBootstrapRecTup
    ,BootstrapValues(..)
    ,getTempEnvStage
    
    ,debugShowValue
    
    ,addModulePlugin
    ,RuntimeImportSource(..)
    ,ModulePlugin(..)
    ,getModuleMetadata
    ,lookupImportSource
    ,BurdockImportSource(..)

    ,addFFIType
    ,addBinding
    ,getMember
    ,throwValue
    ,getModuleValue
    ,app
    ,withCallstackEntry
    ,captureClosure
    ,createBurdockRunner
    ,lookupType
    ,lookupBinding
    ,withNewEnv
    ,withScope
    ,tupleGet

    ,getCallStack
    ,runTask
    

    ,Value(..)
    ,DataDeclTypeTag
    ,dataDeclTagsEqual
    ,dtyName
    ,tyName
    ,FFITypeTag
    ,Env

    ,ffiTypeTagToValue
    ,makeFFIType
    
    ,makeDataDeclTag
    
    ,makeBool
    ,makeFunctionValue
    ,makeValue
    ,extractValue
    ,makeValueName
    ,makeString
    ,extractBurdockList
    ,makeBurdockList
    ,nothingValue
    ,makeRecord
    ,makeTuple
    ,makeNumber
    ,extractTuple

    ,doIsTest
    ,doIsNotTest
    ,setTestModule
    ,getTestResults
    ,addTestPass
    ,addTestFail
    
    ,makeVariant
    ,variantTag
    ,variantName
    ,variantValueFields
    ,variantFields

    ,liftIO
    )

import Burdock.HaskellModulePlugin
    (haskellModulePlugin
    --,addHaskellModule
    ,hmmModulePlugin
    ,HaskellModuleManager
    )

import Data.Text (Text)
import qualified Data.Text as T

import Burdock.Scientific
    (showScientific
    ,Scientific
    )

import Control.Monad
    (when
    ,forM
    ,void
    )

import Data.Dynamic
    (Typeable
    )

import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )

import Data.IORef
    (newIORef
    ,modifyIORef
    ,readIORef)

import Burdock.RenamerEnv (renameTypeName)

import Burdock.RuntimeBootstrapExtras
    (addRuntimeBootstrapExtras
    ,binaryMemberLax)

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

------------------------------------------------------------------------------

-- temp hack again - before modules and proper ffi implemented,
-- make a bunch of quick haskell ffi functions available to burdock code
-- this includes part of the build in language

initRuntime :: Runtime ModuleMetadata
initRuntime = do

    hackMM <- liftIO $ newIORef [("run-task", (Nothing,BEIdentifier))
                                ,("run-task-cs", (Nothing,BEIdentifier))
                                ,("run-task-cs-async", (Nothing,BEIdentifier))
                                -- hack for boolean literal patterns
                                -- todo: renamer should convert boolean literal
                                --   patterns into a booleanliteralpattern variant
                                ,("true", (Nothing, BEVariant 0))
                                ,("false", (Nothing, BEVariant 0))
                                ]
    let addFFIType' nm ty = do
            liftIO $ modifyIORef hackMM ((renameTypeName nm, (Nothing, BEType 0)) : )
            addFFIType nm ty
        addBinding' nm f = do
            liftIO $ modifyIORef hackMM ((nm, (Nothing, BEIdentifier)) : )
            addBinding nm f

    -- todo: divide this up into stuff that's needed to execute bootstrap
    -- and maybe internals
    -- stuff which is part of a built in module or needed to run scripts
    -- which use the whole language and built in modules only
    -- and other bits, used just for testing?

    void $ addFFIType' "ffitypetag" ffitypetagFFI
    void $ addFFIType' "datadecltag" datadecltagFFI

    void $ addFFIType' "number" scientificFFI
    void $ addFFIType' "string" stringFFI
    booleanType <- addFFIType' "boolean" booleanFFI

    void $ addFFIType' "haskell-list" ffitypetagFFI
    -- hack for the construct hack for haskell-list
    addBinding' "haskell-list" =<< makeBool True

    addBinding' "make-datadecltag" =<< makeFunctionValue makeDataDeclTag'
    addBinding' "make-burdock-list" =<< makeFunctionValue myMakeBurdockList
    addBinding' "make-haskell-list" =<< makeFunctionValue makeHaskellList
    addBinding' "make-variant" =<< makeFunctionValue myMakeVariant
    addBinding' "is-type" =<< makeFunctionValue myIsType
    addBinding' "is-variant" =<< makeFunctionValue myIsVariant
    addBinding' "check-variants-equal" =<< makeFunctionValue checkVariantsEqual
    addBinding' "raise" =<< makeFunctionValue raise

    -- loading a module needs the interpreter, but the interpreter depends on this
    -- module
    addBinding' "load-module" =<< makeFunctionValue myLoadModule

    -- should true be a built in value (built into the runtime), or an ffi
    -- value, or a agdt?
    
    addBinding' "true" (makeValue booleanType True)
    addBinding' "false" (makeValue booleanType False)

    addBinding' "show-variant" =<< makeFunctionValue showVariant

    -- these are things which should be moved to a regular haskell ffi
    -- module
    addRuntimeBootstrapExtras addFFIType' addBinding'

    --addBinding' "make-bytestring" =<< makeFunctionValue makeBytestring
    --addBinding' "get-bytestring-byte" =<< makeFunctionValue getBytestringByte

    -- well hacky, use reverse to shoehorn in the true and false variants
    -- before the binding
    -- all this mess will be fixed after the module system and basic haskell
    -- ffi modules are implemented for user code, before replacing the system
    -- bootstrap with the new approach
    flip ModuleMetadata [] <$> reverse <$> liftIO (readIORef hackMM)

------------------------------------------------------------------------------
 
-- some helper functions mainly for operators

unaryMember :: Typeable a => Text -> Text -> Text -> (a -> Text) -> Value -> [Value] -> Runtime Value
unaryMember burName inType outType f v as =
    case as of
        [] -> do
            let v1 = maybe (error $ "u not a " <> inType <> " " <> debugShowValue v) id $ extractValue v
            makeValueName outType $ f v1
        _ -> error $ "bad args to " <> inType <> " " <> burName


binaryMember :: (Typeable a, Typeable b) =>
    Text -> Text -> Text -> (a -> a -> b) -> Value -> [Value] -> Runtime Value
binaryMember burName inType outType f v1 as =
    case as of
        [v2] -> do
            let n1 = maybe (error $ "b0 not a " <> inType <> " " <> debugShowValue v1) id $ extractValue v1
                n2 = maybe (error $ "b1 not a " <> inType <> " " <> debugShowValue v2) id $ extractValue v2
            makeValueName outType $ f n1 n2
        _ -> error $ "bad args to " <> inType <> " " <> burName


------------------------------------------------------------------------------

ffitypetagFFI :: Text -> Value -> Runtime Value
ffitypetagFFI "_torepr" v = makeFunctionValue $ \case
    [] -> do
        let y = maybe (error $ "toreprnot a ffitypetag " <> debugShowValue v) id $ extractValue v
        makeString $ tyName y
    xs -> error $ "unsupported args to torepr: " <> T.intercalate "," (map debugShowValue xs)
          <> debugShowValue v
ffitypetagFFI m _ = error $ "unsupported field on ffitypetag: " <> m

datadecltagFFI :: Text -> Value -> Runtime Value
datadecltagFFI "_torepr" v = makeFunctionValue $ \case
    [] -> do
        let y = maybe (error $ "toreprnot a datadecltag" <> debugShowValue v) id $ extractValue v
        makeString $ dtyName y
    xs -> error $ "unsupported args to torepr: " <> T.intercalate "," (map debugShowValue xs)
          <> debugShowValue v
datadecltagFFI m _ = error $ "unsupported field on datadecltag: " <> m

makeDataDeclTag' :: [Value] -> Runtime Value
makeDataDeclTag' [v] | Just v' <- extractValue v = do
    makeDataDeclTag v'
makeDataDeclTag' _ = error $ "bad args to makeDataDeclTag'"

------------------------------------------------------------------------------

-- built in types methods

scientificFFI :: Text -> Value -> Runtime Value
scientificFFI "_torepr" v =
    makeFunctionValue $ unaryMember "_torepr" "number" "string" showScientific v
scientificFFI "_plus" v1 =
    makeFunctionValue $ binaryMember "_plus" "number" "number" ((+) :: Scientific -> Scientific -> Scientific) v1
scientificFFI "_minus" v1 = do
    makeFunctionValue $ binaryMember "_plus" "number" "number" ((-) :: Scientific -> Scientific -> Scientific) v1
scientificFFI "_times" v1 = do
    makeFunctionValue $ binaryMember "_plus" "number" "number" ((*) :: Scientific -> Scientific -> Scientific) v1
scientificFFI "_equals" v1 = do
    makeFunctionValue $ binaryMemberLax "_equals" "number" "boolean" ((==) :: Scientific -> Scientific -> Bool) v1
scientificFFI "_lessequal" v1 = do
    makeFunctionValue $ binaryMember "_lessequal" "number" "boolean" ((<=) :: Scientific -> Scientific -> Bool) v1
scientificFFI "_lessthan" v1 = do
    makeFunctionValue $ binaryMember "_lessthan" "number" "boolean" ((<) :: Scientific -> Scientific -> Bool) v1
scientificFFI "_greaterthan" v1 = do
    makeFunctionValue $ binaryMember "_greaterthan" "number" "boolean" ((>) :: Scientific -> Scientific -> Bool) v1
scientificFFI "_greaterequal" v1 = do
    makeFunctionValue $ binaryMember "_greaterequal" "number" "boolean" ((>=) :: Scientific -> Scientific -> Bool) v1
scientificFFI m _ = error $ "unsupported field on number: " <> m


stringFFI :: Text -> Value -> Runtime Value
stringFFI "_plus" v1 =
    makeFunctionValue $ binaryMember "_plus" "string" "string" ((<>) :: Text -> Text -> Text) v1
stringFFI "_equals" v1 =
    makeFunctionValue $ binaryMemberLax "_equals" "string" "boolean" ((==) :: Text -> Text -> Bool) v1
stringFFI "_torepr" v1 = do
    makeFunctionValue $ unaryMember "_torepr" "string" "string" (show :: (Text -> Text)) v1
stringFFI m _ = error $ "unsupported field on string: " <> m

booleanFFI :: Text -> Value -> Runtime Value
booleanFFI "_torepr" v1 = do
    makeFunctionValue $ unaryMember "_torepr" "boolean" "string" disp v1
  where
    disp n1 = if n1 then ("true" :: Text) else "false"
booleanFFI "_equals" v1 = do
    makeFunctionValue $ binaryMemberLax "_equals" "boolean" "boolean" ((==) :: Bool -> Bool -> Bool) v1
booleanFFI m _ = error $ "unsupported field on boolean: " <> m


------------------------------------------------------------------------------

-- specific language support for agdt

makeHaskellList :: [Value] -> Runtime Value
makeHaskellList vs = makeValueName "haskell-list" vs

myMakeVariant :: [Value] -> Runtime Value
myMakeVariant [tg, nm, flds, es] = do
    let tg' :: DataDeclTypeTag
        tg' = maybe (error $ "bad args to make variant, first arg is not DataDeclTypeTag")
              id $ extractValue tg
        t :: Text
        t = maybe (error $ "bad args to make variant, second arg is not text")
            id $ extractValue nm
        flds' :: [Value]
        flds' = maybe (error $ "bad args to make variant, third arg is not haskell list")
                id $ extractValue flds
        flds'' :: [Text]
        flds'' = flip map flds' $ \f -> maybe (error $ "bad args to make variant, third arg has non string in list")
                 id $ extractValue f
        es' :: [Value]
        es' = maybe (error $ "bad args to make variant, fourth arg is not list")
                id $ extractValue es
                                        
    when (length es' /= length flds') $ error $ "wrong number of args to create variant " <> t
    -- todo: need the type tag to put in here, later it will be abstract
    -- and it will be used in lots of these auxiliary functions
    makeVariant tg' t $ zip flds'' es'
myMakeVariant _ = error $ "bad args to makeVariant"

myIsVariant :: [Value] -> Runtime Value
myIsVariant [tg, nm, x] = do
    let tg' :: DataDeclTypeTag
        tg' = maybe (error $ "bad args to make variant, first arg is not DataDeclTypeTag")
              id $ extractValue tg
        nm' :: Text
        nm' = maybe (error $ "bad args to is variant, second arg is not text")
            id $ extractValue nm
    tgx <- variantTag x
    x' <- variantName x
    case (tgx, x') of
        (Just tgx', Just t) -> makeBool $ dataDeclTagsEqual tg' tgx' && nm' == t
        _ -> makeBool False
myIsVariant _ = error $ "bad args to myIsVariant"

myIsType :: [Value] -> Runtime Value
myIsType [tg, x] = do
    let tg' :: DataDeclTypeTag
        tg' = maybe (error $ "bad args to is type, first arg is not datadecltag")
            id $ extractValue tg
    x' <- variantTag x
    case x' of
        Nothing -> makeBool False
        Just t -> makeBool $ dataDeclTagsEqual tg' t
myIsType _ = error $ "bad args to myIsType"


-- todo: decide if the flds should be passed, or this function should
-- work them out. Currently, the passed fields are ignored and this
-- function works them out using the hack auxiliary variantValueFields
checkVariantsEqual :: [Value] -> Runtime Value
checkVariantsEqual [a, b@(VariantV {})] = do
    at <- variantName a
    bt <- variantName b
    vfldsa <- variantValueFields a
    vfldsb <- variantValueFields b
    -- hack for records:
    case (at,bt,vfldsa,vfldsb) of
        (Just at', Just bt',Just vfldsa', Just vfldsb') ->
            if at' == bt' &&
               map fst vfldsa' == map fst vfldsb'
            then do
                --liftIO $ putStrLn $ "fields equal:" <> show at'
                fsEq <- forM (map fst vfldsa') $ \f -> do
                    va <- getMember a f
                    vb <- getMember b f
                    eqx <- getMember va "_equals"
                    x <- app Nothing eqx [vb]
                    pure x
                -- and together all the fsEq
                let andEm [] = makeBool True
                    andEm (v:vs) = case extractValue v of
                        Just True -> andEm vs
                        Just False -> makeBool False
                        _ -> error $ "non boolean returned from _equals method"
                andEm fsEq
            else makeBool False
        _ -> error $ "checkVariantsEqual error: " <> debugShowValue a <> " " <> debugShowValue b
checkVariantsEqual [_,_] = makeBool False
checkVariantsEqual _ = error $ "bad args to checkVariantsEqual"

showVariant :: [Value] -> Runtime Value
showVariant [x] = do
    at <- variantName x
    bt <- variantValueFields x
    let trm e = do
            f <- getMember e "_torepr"
            app Nothing f []
    case (at,bt) of
        (Just n, Just fs) -> do
            let fs' = map snd fs
            fs'' <- mapM trm fs'
            let (es :: [Maybe Text]) = map extractValue fs''
            let es' :: [Text]
                es' = maybe (error "showvariant non string from torepr") id $ sequence es
            --liftIO $ putStrLn $ show es'
            if null es'
                then makeString n
                else makeString $ n <> "(" <> T.intercalate ", " es' <> ")"
        _ -> error $ "show variant called on non variant " <> debugShowValue x
    
showVariant _ = error $ "bad args to showVariant"

------------------------------------------------------------------------------

-- module system support

myLoadModule  :: [Value] -> Runtime Value
myLoadModule [ctx,nm,as]
    | Just (ctx' :: Text)  <- extractValue ctx
    , Just (nm' :: Text) <- extractValue nm
    , Just as' <- extractBurdockList as
    , Just (as'' :: [Text]) <- mapM extractValue as' = do
    getModuleValue (Just ctx') $ RuntimeImportSource nm' as''
myLoadModule _ = error $ "bad args to myLoadModule"

------------------------------------------------------------------------------

-- simple utility functions

myMakeBurdockList :: [Value] -> Runtime Value
myMakeBurdockList vs = makeBurdockList vs

raise :: [Value] -> Runtime Value
raise [x] = throwValue x
raise _ = error $ "bad args to raise"



