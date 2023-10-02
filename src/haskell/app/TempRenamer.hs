
{-

quick hack to take files on the command line, and rename them
and print the nice error messages if there are any
to check both manually

todo: put this functionality inside the interpreter
  then more chance of it working exactly the same as
  when it attempts to run the code

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module TempRenamer
    (tempRenamer
    ,tempDesugar
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error)

import Burdock.Parse (parseScript)
import qualified Burdock.Syntax as S

import Burdock.Renamer
    (renameScript
    ,renameModule
    ,prettyStaticErrors)

import Burdock.Renamer
    (ModuleMetadata)
import Burdock.ModuleMetadata
    (tempEmptyModuleMetadata
    )
    
import Burdock.Pretty (prettyScript)

import qualified Data.Text.Lazy.IO as L
import qualified Data.Text as T

import Data.Text (Text)

import Burdock.Runtime
    (runRuntime
    ,getTempEnvStage
    )
import Burdock.Interpreter
    (createHandle)
import Burdock.Desugar
    (desugarScript
    ,desugarModule
    ,getSourceDependencies
    )

import Burdock.InterpreterPretty
    (prettyStmts)

tempRenamer :: Bool -> Text -> IO ()
tempRenamer isModule fn = do
    -- how will this work with included modules for now?
    src <- L.readFile (T.unpack fn)
    let ast = case parseScript fn src of
            Left e -> error e
            Right ast' -> ast'
    let imps = hackGetImports ast
    -- todo: need to memoize and share and stuff
    ctx <- concat <$> mapM processImport imps
    -- todo: use function to extract the imported files, and recurse
    -- so can show renamed files that import stuff
    mm <- hackGetPreexistingEnv
    case if isModule
         then renameModule fn mm ctx ast
         else renameScript fn mm ctx ast of
        Left  e -> error $ prettyStaticErrors e
        Right (_,res) -> L.putStrLn $ prettyScript res

processImport :: Text -> IO [(Text, ModuleMetadata)]
processImport mfn = do
        src <- L.readFile (T.unpack mfn)
        let ast = case parseScript mfn src of
                Left e -> error e
                Right ast' -> ast'
        let imps = hackGetImports ast
        -- todo: need to memoize and share and stuff
        ctx <- concat <$> mapM processImport imps
        let x = renameModule mfn tempEmptyModuleMetadata ctx ast
        case x of
            Left e -> error $ prettyStaticErrors e
            Right (m,_) -> pure ((mfn,m) : ctx)

tempDesugar :: Bool -> Text -> IO ()
tempDesugar isModule fn = do

    src <- L.readFile (T.unpack fn)
    let ast = case parseScript fn src of
            Left e -> error e
            Right ast' -> ast'
    let imps = hackGetImports ast
    ctx <- concat <$> mapM processImport imps
    mm <- hackGetPreexistingEnv
    let stmts = snd $ (if isModule
                       then desugarModule
                       else desugarScript) mm fn ctx ast
    L.putStrLn $ prettyStmts stmts
        
hackGetImports :: S.Script -> [Text]
hackGetImports ast = map (head . snd) $ getSourceDependencies ast

hackGetPreexistingEnv :: IO ModuleMetadata
hackGetPreexistingEnv = do
    h <- createHandle
    runRuntime h getTempEnvStage
