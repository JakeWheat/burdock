
{-

built in ffi module used to bootstrap the interpreter/handle

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Bootstrap
    (burdockBootstrapModule
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error, show)
import Data.Text.IO (putStrLn)

import Control.Arrow (first, second)

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import Data.IORef
    (IORef
    ,readIORef
    ,modifyIORef
    ,newIORef
    )

import qualified Burdock.Runtime as R
import Burdock.Runtime (Value)
import Burdock.Scientific (Scientific, showScientific)

------------------------------------------------------------------------------

burdockBootstrapModule :: R.Runtime [(Text, Value)]
burdockBootstrapModule = R.withScope $ do

    --liftIO $ putStrLn "**********************LOAD BOOSTRAP"
    -- get the ffitypetag burdock type
    -- this is used to make other ffi types
    -- and it's added to a binding in burdock
    ffiTypeInfo <- R.getFFITypeInfoTypeInfo

    dataDeclTagTI <- makeDataDeclTagType
    variantTagTI <- makeVariantTagType

    burdockNumberTI <- makeNumberType

    haskellListTI <- makeHaskellListType

    tupleDeclTag <- R.newDataDeclTag "tuple"
    tupleVariantTag <- pure $ R.VariantTag tupleDeclTag "tuple"
    recordDeclTag <- R.newDataDeclTag "record"
    recordVariantTag <- pure $ R.VariantTag recordDeclTag "record"

    testLog <- liftIO $ newIORef (0,0)

    -- wrap direct ffi type infos as burdock ffi values so they can
    -- be bound in a burdock namespace
    burdockFFITag <- R.makeFFIValue ffiTypeInfo ffiTypeInfo
    burdockNumberTag <- R.makeFFIValue ffiTypeInfo burdockNumberTI
    haskellListTag <- R.makeFFIValue ffiTypeInfo haskellListTI
    variantTag <- R.makeFFIValue ffiTypeInfo variantTagTI
    tupleTypeFFITag <- R.makeFFIValue dataDeclTagTI tupleDeclTag
    tupleVariantFFITag <- R.makeFFIValue variantTagTI tupleVariantTag
    recordTypeFFITag <- R.makeFFIValue dataDeclTagTI recordDeclTag
    recordVariantFFITag <- R.makeFFIValue variantTagTI recordVariantTag

{-
TODO: review these

quite a lot of these functions (and values) could become specialised syntax
would this make the code simpler? e.g. if you added all the data decl support
functions to I.Expr, then you'd remove a layer of redirection
which is easier to read and maintain?
produce a version of the code where you try to make as much pure syntax as possible
and move the rest into runtime itself too
so you can compare both and see which is nicer

-}
    
    pure [

          -- core language types
          ("_type-ffitag", burdockFFITag)
         ,("_type-number", burdockNumberTag)
         ,("_type-haskell-list", haskellListTag)
         ,("_type-variant-tag", variantTag)

         ,("_type-tuple", tupleTypeFFITag)
         ,("_variant-tuple", tupleVariantFFITag)
         ,("_type-record", recordTypeFFITag)
         ,("_variant-record", recordVariantFFITag)

         ,("show-tuple", R.Fun showTuple)
         ,("show-record", R.Fun showRecord)

         ,("true", R.Boolean True)
         ,("false", R.Boolean False)
         
         ,("nothing", R.BNothing)
         ,("raise", R.Fun bRaise)
         ,("haskell-error", R.Fun haskellError)

         ,("not", R.Fun bNot)

         -- data decl support
         ,("make-data-decl-tag", R.Fun (makeDataDeclTag dataDeclTagTI))
         ,("is-type", R.Fun $ isType dataDeclTagTI)
         ,("make-variant-tag", R.Fun (makeVariantTag dataDeclTagTI variantTagTI))
         ,("is-variant", R.Fun (isVariant variantTagTI))
         ,("make-variant", R.Fun (makeVariant variantTagTI haskellListTI))
         ,("variants-equal", R.Fun (variantsEqual haskellListTI))
         ,("show-variant", R.Fun (showVariant haskellListTI))
         ,("make-haskell-list", R.Fun (makeHaskellList haskellListTI))

         ,("make-module", R.Fun makeModule) -- rename to make module value?
         -- temp hack, will be handled in the renamer
         ,("include-all", R.Fun includeAll)

          -- todo: the below should be moved to other modules/namespaces
          -- test framework plugin
         ,("run-binary-test", R.Fun (bRunBinaryTest testLog))
         ,("get-test-passes", R.Fun (getTestVal testLog 0 burdockNumberTI))
         ,("get-test-failures", R.Fun (getTestVal testLog 1 burdockNumberTI))

          -- misc
         ,("tostring", R.Fun bToString)
         ,("print", R.Fun bPrint)
         ]

{-

planning:
ffitypetag
datadecltag
number
string
boolean + true/false pattern matching (will try to implement as datadecl)

the above need is-X variations

+ types, is-X and other support for: tuple, record, module, box?, function, method

nothing

list
either

run-task + variations

raise

data decl support, roughly
haskell-list ffitype

make-datadecl-tag, make haskell list, make variant, is type, is
variant, check variants equal, show variant

this is roughly the core of the _bootstrap module that's needed to run
the language itself

-}

------------------------------------------------------------------------------

makeNumberType :: R.Runtime R.FFITypeInfo
makeNumberType = do
    R.makeFFIType ["_bootstrap", "number"]
        [R.ToRepr $ \(v :: Scientific) -> pure $ showScientific v
        ,R.Compare $ (pure .) . compare
        ,R.Arith
            {R.aAdd = (pure .) . (+)
            ,R.aSub = (pure .) . (-)
            ,R.aMult = (pure .) . (*)
            ,R.aDiv = (pure .) . (/)}]

-- a haskell list is a list of burdock values in a haskell list
-- this is used to bootstrap datadecls
makeHaskellListType :: R.Runtime R.FFITypeInfo
makeHaskellListType = do
    R.makeFFIType ["_bootstrap", "haskell-list"]
        [R.ToRepr $ \(_v :: [Value]) -> pure $ "<haskell-list>"
        ,R.Equals $ \_ _  -> pure False]

makeDataDeclTagType :: R.Runtime R.FFITypeInfo
makeDataDeclTagType = do
    R.makeFFIType ["_bootstrap", "data-decl-tag"]
        [R.ToRepr $ \(R.DataDeclTag _ nm) -> pure $ "<data-decl-tag: " <> nm <> ">"
        ,R.Equals $ (pure .) . (==)]

makeVariantTagType :: R.Runtime R.FFITypeInfo
makeVariantTagType = do
    R.makeFFIType ["_bootstrap", "variant-tag"]
        [R.ToRepr $ \(R.VariantTag (R.DataDeclTag _ dnm) vnm) -> pure $ "<variant-tag: " <> dnm <> "." <> vnm <> ">"
        ,R.Equals $ (pure .) . (==)]

------------------------------------------------------------------------------

runBinaryTest :: IORef (Int,Int)
              -> Text
              -> R.Value
              -> R.Value
              -> R.Value
              -> Text
              -> R.Runtime ()
runBinaryTest tally msg lv0 lv1 op opFailString = do
    -- todo: get the original source positions in here
    v0 <- R.runTask $ R.app Nothing lv0 []
    v1 <- R.runTask $ R.app Nothing lv1 []
    case (v0,v1) of
        (Right v0', Right v1') -> do
            expressionsOK v0' v1'
        (Left er0, Left er1) -> do
            liftIO $ modifyIORef tally (second (+1))
            er0s <- safeToRepr er0
            er1s <- safeToRepr er1
            liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"LHS raised: " <> er0s
                 ,"RHS raised: " <> er1s]
        (Left er0, Right {}) -> do
            liftIO $ modifyIORef tally (second (+1))
            er0s <- safeToRepr er0
            liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"LHS raised: " <> er0s]
        (Right {}, Left er1) -> do
            liftIO $ modifyIORef tally (second (+1))
            er1s <- safeToRepr er1
            liftIO $ putStrLn $ T.unlines
                 ["FAIL: " <> msg
                 ,"RHS raised: " <> er1s]
  where
    expressionsOK v0 v1 = do
        r <- R.runTask $ R.app Nothing op [v0, v1]
        case r of
            Right rx -> predicateOK rx v0 v1
            Left er -> do
                liftIO $ modifyIORef tally (second (+1))
                ers <- safeToRepr er
                liftIO $ putStrLn $ T.unlines
                    ["FAIL: " <> msg
                    ,"predicate raised: " <> ers]
    predicateOK r v0 v1 = 
         case r of
            R.Boolean True -> liftIO $ do
                liftIO $ modifyIORef tally (first (+1))
                liftIO $ putStrLn $ "PASS: " <> msg
            R.Boolean False -> do
                liftIO $ modifyIORef tally (second (+1))
                sv0 <- safeToRepr v0
                sv1 <- safeToRepr v1
                liftIO $ putStrLn $ T.unlines
                    ["FAIL: " <> msg
                    ,sv0
                    ,opFailString
                    ,sv1]
            _ -> do
                liftIO $ modifyIORef tally (second (+1))
                er <- safeToRepr r
                error $ "non bool from test predicate: " <> er -- R.debugShowValue r
    safeToRepr v = do
        r <- R.runTask $ do
            tr <- R.getMember Nothing v "_torepr"
            R.app Nothing tr []
        case r of
            Left e -> case e of
                -- todo: another fucking call to safetorepr
                R.BString e' -> pure $ e' <> " : " <> R.debugShowValue v
                e1 -> pure $ R.debugShowValue e1
            Right (R.BString t) -> pure t
            Right x -> pure $ R.debugShowValue v <> " torepr: " <> R.debugShowValue x

getTestVal :: IORef (Int,Int) -> Int -> R.FFITypeInfo -> [Value] -> R.Runtime Value
getTestVal v i nti = \case
    [] -> do
        (a,b) :: (Int,Int) <- liftIO $ readIORef v
        if i == 0
            then R.makeFFIValue nti $ ((fromIntegral a) :: Scientific)
            else R.makeFFIValue nti $ ((fromIntegral b) :: Scientific)
    _ -> error $ "bad args to getTestVal"

bRunBinaryTest :: IORef (Int,Int) -> [Value] -> R.Runtime Value
bRunBinaryTest tally [R.BString msg
                     ,v0
                     ,v1
                     ,op
                     ,R.BString opFailString] = do
    runBinaryTest tally msg v0 v1 op opFailString
    pure R.BNothing
bRunBinaryTest _ _ = error $ "bad args to bRunBinaryTest"

------------------------------------------------------------------------------

-- data decl support

makeHaskellList :: R.FFITypeInfo -> [Value] -> R.Runtime Value
makeHaskellList hti as = R.makeFFIValue hti as

--_make-data-decl-tag(nm)
makeDataDeclTag :: R.FFITypeInfo -> [Value] -> R.Runtime Value
makeDataDeclTag ddti [R.BString nm] = do
    t <- R.newDataDeclTag nm
    R.makeFFIValue ddti t
makeDataDeclTag _ _ = error $ "bad args to makeDataDeclTag"

--_is-type(tag, x)
isType :: R.FFITypeInfo -> [Value] -> R.Runtime Value
isType ddti [edti, R.Variant (R.VariantTag dt _) _] = do
    Right edti' <- R.extractFFIValue ddti edti
    pure $ R.Boolean $ edti' == dt
isType _ _ = error $ "bad args to isType"

--_make-variant-tag(dtag,nm)
makeVariantTag :: R.FFITypeInfo -> R.FFITypeInfo -> [Value] -> R.Runtime Value
makeVariantTag ddti vti [dt, R.BString nm] = do
    Right dti <- R.extractFFIValue ddti dt
    R.makeFFIValue vti $ R.VariantTag dti nm
makeVariantTag _ _ _ = error $ "bad args to makeVariantTag"

--_is-variant(vtag,x)
isVariant :: R.FFITypeInfo -> [Value] -> R.Runtime Value
isVariant vvti [wvtv, R.Variant vt _] = do
    Right wantedTag <- R.extractFFIValue vvti wvtv
    pure $ R.Boolean $ wantedTag == vt
isVariant _ _ = error $ "bad args to isVariant"

--_make-variant(vtag, fieldnames, *as)
makeVariant :: R.FFITypeInfo -> R.FFITypeInfo -> [Value] -> R.Runtime Value
makeVariant vvti hlti [vt, fs, as] = do
    Right (varTag :: R.VariantTag) <- R.extractFFIValue vvti vt
    Right fs' <- R.extractFFIValue hlti fs
    let fs'' = flip map fs' $ \case
            R.BString t -> t
            x -> error $ "non string in make variant call: " <> R.debugShowValue x
    Right as' <- R.extractFFIValue hlti as
    pure $ R.Variant varTag $ zip fs'' as'
makeVariant _ _ _ = error $ "bad args to makeVariant"

-- variants-equal(fs :: [String],v0,v1)
variantsEqual :: R.FFITypeInfo -> [Value] -> R.Runtime Value
variantsEqual hlti [fs, v0, v1] = do
    Right fs' <- R.extractFFIValue hlti fs
    let fs'' :: [Text]
        fs'' = flip map fs' $ \case
            R.BString t -> t
            x -> error $ "non string in make variant call: " <> R.debugShowValue x
    case (v0,v1) of
        (R.Variant tg0 fs0, R.Variant tg1 fs1)
            | tg0 == tg1
            -> do
              let checkit fn = case (lookup fn fs0, lookup fn fs1) of
                       (Just a, Just b) -> do
                           eq <- R.getMember Nothing a "_equals"
                           R.app Nothing eq [b]
                           --undefined --call a.equals(b)
                       _ -> pure $ R.Boolean False
              res <-mapM checkit fs''
              let tot :: Bool
                  tot = and $ flip map res $ \case
                                     R.Boolean True -> True
                                     _ -> False
              pure $ R.Boolean tot
        _ -> pure $ R.Boolean False
variantsEqual _ as = error $ "bad args to variantsEqual" <> show (length as)

showVariant :: R.FFITypeInfo -> [Value] -> R.Runtime Value
showVariant hlti [fs, v0] = do
    Right fs' <- R.extractFFIValue hlti fs
    let fs'' :: [Text]
        fs'' = flip map fs' $ \case
            R.BString t -> t
            x -> error $ "non string in make variant call: " <> R.debugShowValue x
    case v0 of
        R.Variant (R.VariantTag _ nm) _ | null fs'' -> do
            pure $ R.BString $ nm 
        R.Variant (R.VariantTag _ nm) vfs -> do
            let tr fnm = do
                    let a = maybe (error "what?") id $ lookup fnm vfs
                    tr1 <- R.getMember Nothing a "_torepr"
                    x <- R.app Nothing tr1 []
                    case x of
                        R.BString s -> pure s
                        x1 -> error $ "wrong return type from torepr" <> R.debugShowValue x1
            vs <- mapM tr fs''
            pure $ R.BString $ nm <> "(" <> T.intercalate "," vs <> ")"
        _ -> error "bad arg to showVariant"

showVariant _ _as = error $ "bad args to showVariant"


showTuple :: [Value] -> R.Runtime Value
-- todo: check the tag
showTuple [R.Variant _ vs] = do
    vs' <- mapM (tor . snd) $ filter ((`notElem` ["_equals", "_torepr"]) . fst) vs
    pure $ R.BString $ "{" <> T.intercalate ";" vs' <> "}"
  where
    tor v = do
        t <- R.getMember Nothing v "_torepr"
        v1 <- R.app Nothing t []
        case v1 of
            R.BString s -> pure s
            _ -> error $ "wrong type from torepr 388"
showTuple _ = error "bad args to showTuple"

showRecord :: [Value] -> R.Runtime Value
showRecord [R.Variant _ vs] = do
    vs' <- mapM tor $ filter ((`notElem` ["_equals", "_torepr"]) . fst) vs
    pure $ R.BString $ "{" <> T.intercalate "," vs' <> "}"
  where
    tor (nm,v) = do
        t <- R.getMember Nothing v "_torepr"
        v1 <- R.app Nothing t []
        case v1 of
            R.BString s -> pure $ nm <> ":" <> s
            _ -> error $ "wrong type from torepr 388"
showRecord _ = error "bad args to showRecord"

------------------------------------------------------------------------------

bRaise :: [Value] -> R.Runtime Value
bRaise [v] = R.throwM $ R.ValueException v
bRaise _ = error "bad args to bRaise" 


haskellError :: [Value] -> R.Runtime Value
haskellError [R.BString v] = error v
haskellError _ = error "bad args to haskellError"

makeModule :: [Value] -> R.Runtime Value
-- todo: check the tag
makeModule [R.Variant _ fs] = pure $ R.Module fs
makeModule _ = error "bad args to makeModule"

includeAll :: [Value] -> R.Runtime Value
-- todo: check the tag
includeAll [R.Module fs] = do
    flip mapM_ fs $ \(n,v) -> R.addBinding n v
    pure R.BNothing
includeAll _ = error "bad args to includeAll"

------------------------------------------------------------------------------

-- todo: print should use tostring on a single value
--   so it needs to closure capture the tostring function
bPrint :: [Value] -> R.Runtime Value
bPrint [R.BString t] = do
    liftIO $ putStrLn t
    pure R.BNothing
bPrint [v] = do
    f <- R.getMember Nothing v "_torepr"
    -- todo: how do functions get a source position from syntax
    -- if that's how they are called?
    r <- R.app Nothing f []
    case r of
        R.BString t -> liftIO $ putStrLn t
        _ -> error $ "non text returned from x._torepr()" <> R.debugShowValue v <> " " <> R.debugShowValue r
    pure R.BNothing
bPrint _ = error "bad args to print"

bToString :: [Value] -> R.Runtime Value
bToString [v@(R.BString {})] = pure v
bToString [v] = do
    f <- R.getMember Nothing v "_torepr"
    R.app Nothing f []
bToString _ = error "bad args to tostring"

bNot :: [Value] -> R.Runtime Value
bNot [R.Boolean t] = pure $ R.Boolean $ not t
bNot _ = error "bad args to not"
