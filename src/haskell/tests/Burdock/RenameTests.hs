{-
Currently loads all tests from src/rewrite-tests
using the RenameTestParser
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Burdock.RenameTests
    (renameTests
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

import System.IO.Unsafe (unsafePerformIO)

import Burdock.TestLib (TestTree(..))
    
import Burdock.RenameTestParser
    (RenameTest(..)
    ,parseRenameTestFile
    )

import Burdock.RenameAst
    (rename
    )

import Burdock.ModuleMetadata
    (ModuleID(..)
    --,ModuleMetadata(..)
    )

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as L
import Burdock.Parse (parseScript)

import Burdock.Syntax (resetSourcePositions
                      ,ImportSource(..))

import qualified Test.Tasty.HUnit as Tst
import Control.Monad (when)
import Burdock.Pretty (prettyScript)
import Data.List ((\\))

------------------------------------------------------------------------------

renameTests :: TestTree
renameTests = unsafePerformIO $ do
    let fn = "src/rewrite-tests"
    f <- L.readFile fn
    let renameTestData = either error id $ parseRenameTestFile (T.pack fn) f

    pure $ TestGroup "renameTests" $ map makeRenameTest renameTestData

------------------------------------------------------------------------------

makeRenameTest :: RenameTest -> TestTree
makeRenameTest rt = TestCase (rName rt) $ do
    -- parse each of the sources, rename them, and get the metadata
    let parseIt nm src =
            -- stop any additional use context stuff
            let src' = "use context _bootstrap-interpreter\n" <> src
            in either (error . show) id $ parseScript nm src'
        f acc [] = pure acc
        f acc ((fn,src):srcs) = do
            let ast = parseIt fn src
                hackISCtx = flip map acc $ \(x@(ModuleID nm as),_) -> (ImportSpecial nm as, x)
            (mmeta, _) <- rename hackISCtx acc ast
            f ((ModuleID "file" [fn],mmeta):acc) srcs
    let ctx = (either (error . show) id $ f [] (rSources rt))
        isCtx = flip map ctx $ \(x@(ModuleID nm as),_) -> (ImportSpecial nm as,x)
        
    -- parse the script, rename it
    let res = either Left (Right . snd) $ rename isCtx ctx $ parseIt (rName rt) (rScript rt)
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
            Tst.assertEqual "" (resetSourcePositions eAst) (resetSourcePositions rast)
        (a,b) -> error $ "results not equal:" <> show a <> "\n!=\n" <> show b
  where
    symDiff (a,b) = (a \\ b, b \\ a)
    strShowLines es = T.unpack $ T.unlines $ map show es
