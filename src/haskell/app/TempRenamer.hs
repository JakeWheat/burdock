
{-

quick hack to take files on the command line, and rename them
and print the nice error messages if there are any
to check both manually

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module TempRenamer
    (tempRenamer
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error)

import Burdock.Parse (parseScript)
import qualified Burdock.Syntax as S

import Data.List (nub)
import Data.Maybe (mapMaybe)

import Burdock.Renamer
    (renameScript
    ,renameModule
    ,prettyStaticErrors)

import Burdock.Renamer
    (ModuleMetadata)
import Burdock.ModuleMetadata (tempEmptyModuleMetadata)

    
import Burdock.Pretty (prettyScript)

import qualified Data.Text.Lazy.IO as L
import qualified Data.Text as T

import Data.Text (Text)

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
    --let ctx = []
    case if isModule
         then renameModule tempEmptyModuleMetadata ctx ast
         else renameScript tempEmptyModuleMetadata ctx ast of
        Left  e -> error $ prettyStaticErrors e
        Right (_,res) -> L.putStrLn $ prettyScript res
  where
    processImport :: Text -> IO [(Text, ModuleMetadata)]
    processImport mfn = do
        src <- L.readFile (T.unpack mfn)
        let ast = case parseScript mfn src of
                Left e -> error e
                Right ast' -> ast'
        let imps = hackGetImports ast
        -- todo: need to memoize and share and stuff
        ctx <- concat <$> mapM processImport imps
        let x = renameModule tempEmptyModuleMetadata ctx ast
        case x of
            Left e -> error $ prettyStaticErrors e
            Right (m,_) -> pure ((mfn,m) : ctx)
        
hackGetImports :: S.Script -> [Text]
hackGetImports (S.Script stmts) = nub $ flip mapMaybe stmts $ \case
    S.Import _ (S.ImportSpecial "file" [fn]) _ -> Just fn
    -- todo: cover the rest of the statements, update to built ins when these exist
    -- unless the main interpreter can do this at that point
    _ -> Nothing
