
{-
Currently loads all tests from tests/rewrite-tests
using the RenamerTestParser
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Burdock.RenamerTests
    (renamerTests
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

import System.IO.Unsafe (unsafePerformIO)

import Burdock.TestLib (TestTree(..))
    
import Burdock.RenamerTestParser
    (RenamerTest(..)
    ,parseRenamerTestFile
    )

import Burdock.Renamer
    (renameScript
    ,renameModule
    --,StaticError(..)
    )

import Burdock.ModuleMetadata (tempEmptyModuleMetadata)

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L
import Burdock.Parse (parseScript)

import Burdock.Syntax (resetSourcePositions)

import qualified Test.Tasty.HUnit as Tst
import Control.Monad (when)
import Burdock.Pretty (prettyScript)
import Data.List ((\\))

------------------------------------------------------------------------------

renamerTests :: TestTree
renamerTests = unsafePerformIO $ do
    let fn = "tests/rewrite-tests"
    f <- L.readFile fn
    let renamerTestData = either error id $ parseRenamerTestFile (T.pack fn) f

    pure $ TestGroup "renamerTests" $ map makeRenamerTest renamerTestData

------------------------------------------------------------------------------

makeRenamerTest :: RenamerTest -> TestTree
makeRenamerTest rt = TestCase (rName rt) $ do
    -- parse each of the sources, rename them, and get the metadata
    let f acc [] = pure acc
        f acc ((fn,src):srcs) = do
            let ast = either (error . show) id $ parseScript fn src
            (mmeta, _) <- renameModule tempEmptyModuleMetadata acc ast
            f ((fn,mmeta):acc) srcs
    let ctx = either (error . show) id $ f [] (rSources rt)
    -- parse the script, rename it
    let res = case rScript rt of
            Left m -> let ast = either (error . show) id $ parseScript (rName rt) m
                      in either Left (Right . snd) $ renameModule tempEmptyModuleMetadata ctx ast
            Right s -> let ast = either (error . show) id $ parseScript (rName rt) s
                      in either Left (Right .  snd) $ renameScript tempEmptyModuleMetadata ctx ast
    -- check if it matches the expected, or the expected errors
    case (res, rResult rt) of
        (Left errs, Left expErrs) ->
            let (unmatches1, unmatches2) = symDiff (resetSourcePositions expErrs, resetSourcePositions errs)
            in case (unmatches1,unmatches2) of
                   ([],[]) -> Tst.assertBool "" True
                   _ -> Tst.assertFailure $ "unmatched errors: " ++ strShowLines unmatches1
                        ++ "\nextra errors: " ++ strShowLines unmatches2
        (Right rast, Right eSrc) -> do
            let eAst = either (error . show) id $ parseScript "result" eSrc
            when (resetSourcePositions rast /= resetSourcePositions eAst) $ do
                L.putStrLn $ prettyScript rast
                L.putStrLn "!="
                L.putStrLn $ prettyScript eAst
            Tst.assertEqual "" (resetSourcePositions rast) (resetSourcePositions eAst)
        (a,b) -> error $ "results not equal:" <> show a <> "\n!=\n" <> show b
  where
    symDiff (a,b) = (a \\ b, b \\ a)
    strShowLines es = T.unpack $ T.unlines $ map show es
