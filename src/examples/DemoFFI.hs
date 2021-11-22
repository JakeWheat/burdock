
{-

sketch start of how FFI for haskell could work

the main areas to work on is a nice ffi api, instead of exposing
internals of the interpreter, things to stop exposing:

Interpreter? not sure
Value(..) <- don't want to expose the value representation
  just do vanilla adt approach?
addFFI:
  this is the main thing to work on
  create helper functions so the user uses these and
    a haskell function with a native signature
  to add an ffi function
  later, the system will also want to know the types of the input
    and output


it might be nice to try to find some shorthand to avoid adding an ffi
function with an api call, then binding it in burdock code?

add some helpers to make a wrapper for the command line for scripts or
repl, with some additional haskell ffi, really easy to write and
minimal boilerplate, something like: 

main = burdockExe additionalFFITypes additionalFFIFunctions

-}

{-# LANGUAGE QuasiQuotes #-}

import qualified  Burdock.Interpreter as B
import Burdock.Scientific (extractInt)

import qualified Text.RawString.QQ as R

import Data.Dynamic (--Dynamic
                     toDyn
                    ,fromDynamic
                    )

import Control.Monad (void)


main :: IO ()
main = do
    h <- B.newHandle

    B.addFFIImpls h [("demo-make-haskell-int", demoMakeHaskellInt)
             ,("demo-extract-int", demoExtractInt)
             ,("demo-add-to-int", demoAddToInt)]

    -- could also run from a file easily
    
    void $ B.runScript h Nothing [] [R.r|

import _internals as _internals
include from _internals:
  get-ffi-value
end

demo-make-haskell-int = get-ffi-value("demo-make-haskell-int")
demo-extract-int = get-ffi-value("demo-extract-int")
demo-add-to-int = get-ffi-value("demo-add-to-int")


v0 = demo-make-haskell-int(3)
print(v0)
v1 = demo-add-to-int(v0, 4)
print(v1)
v2 = demo-extract-int(v1)

print(v2)
# not implemented yet:
#print(is-number(v2))

                    |]

demoMakeHaskellInt :: [B.Value] -> B.Interpreter B.Value
demoMakeHaskellInt [B.NumV x] = do
    let y :: Int
        Just y = extractInt x
    pure $ B.FFIValue $ toDyn y
demoMakeHaskellInt _ = error "bad args to demo-make-haskell-int"


demoExtractInt :: [B.Value] -> B.Interpreter B.Value
demoExtractInt [B.FFIValue x] = do
    let y :: Int
        Just y = fromDynamic x
    pure $ B.NumV $ fromIntegral y
demoExtractInt _ = error "bad args to demo-extract-int"

demoAddToInt :: [B.Value] -> B.Interpreter B.Value
demoAddToInt [B.FFIValue x, B.NumV a] = do
    let y :: Int
        Just y = fromDynamic x
        Just a' = extractInt a
    pure $ B.FFIValue $ toDyn $ y + a'
demoAddToInt _ = error "bad args to demo-add-to-int"
