
{-

built in ffi module used to bootstrap the interpreter/handle

Not sure about this at all.

What you definitely need, is some of the items below, like the types
and some of the user accessible functions, to be in a bootstrap ffi
and bound to burdock names.

especially, the data decl support functions and the make module,
these could just be interpreter syntax directly, and cut out the middle
man and all the indirection. It would make the runtime and interpreter
syntax a lot more bulky, but all that exists, the current method just
misleadingly hides it. plus it's not complex, it's just extra lines
of relatively simple code.

the only benefit is that the runtime module is less cluttered, but
maybe it will still be perfectly manageable with this folded into it.

revisit this after doing all the ffi helper code

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Burdock.FFIModules.Bootstrap
    (burdockBootstrapModule
    ) where

import Prelude hiding (error, show, putStrLn)
import Burdock.Utils (error, show)

import Data.Text (Text)
import qualified Data.Text as T

import qualified Burdock.Runtime as R
import Burdock.Runtime (Value)
import Burdock.Scientific (Scientific, showScientific)
import Burdock.ModuleMetadata
    (ModuleMetadata(..)
    ,BindingMeta(..)
    )

------------------------------------------------------------------------------

burdockBootstrapModule :: R.Runtime (ModuleMetadata, Value)
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

    let bs' = [-- core language types
              (BEType 0, ("_type-ffitag", burdockFFITag))
             ,(BEType 0, ("_type-number", burdockNumberTag))
             ,(BEType 0, ("_type-haskell-list", haskellListTag))
             ,(BEType 0, ("_type-variant-tag", variantTag))
             
             ,(BEType 0, ("_type-tuple", tupleTypeFFITag))
             ,(BEVariant (-1), ("_variant-tuple", tupleVariantFFITag))
             ,(BEType 0, ("_type-record", recordTypeFFITag))
             ,(BEVariant (-1), ("_variant-record", recordVariantFFITag))
             ]
             ++ map (BEIdentifier,)
             [("show-tuple", R.Fun showTuple)
             ,("show-record", R.Fun showRecord)
             
             ,("true", R.Boolean True)
             ,("false", R.Boolean False)
             
             ,("nothing", R.BNothing)
             ,("raise", R.Fun bRaise)
             
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

             ]
        -- todo: what's a nice way to make all the originids unique for ffi code
        -- also, this is rubbish hack
        (ms, bs) = unzip $ flip map bs' $ \(t,(n,v)) ->
            ((n, Nothing, t, ("<bootstrap>",Nothing)), (n,v))
    pure (ModuleMetadata ms, (R.Module bs))

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

makeModule :: [Value] -> R.Runtime Value
-- todo: check the tag
makeModule [R.Variant _ fs] = pure $ R.Module fs
makeModule _ = error "bad args to makeModule"

bNot :: [Value] -> R.Runtime Value
bNot [R.Boolean t] = pure $ R.Boolean $ not t
bNot _ = error "bad args to not"
