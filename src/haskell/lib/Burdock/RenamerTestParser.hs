
{-

It's line based

load a file
produce a list of parsed tests

a test is:

Test
{tname :: Text
,tfiles :: [(Text, L.Text)]
,tresult :: Either [StaticError] L.Text
}

split by lines, pair each line with line number
remove lines starting with # or whitespace then #
   or which are only whitespace

repeat:
look for Test: or eof
  read the test name until the end of the line
  then look for 0 or more file
    file: then read name until end of line
    then read lines as the body until you see:
    test:, file: script: result:, errors: at the start of a line
  then look for script:, make sure there's only whitespace until end of line
    then read body (same as with file)
  then look for either result:
      then read body (same as with file)
    or read errors
      for now, one error per line
      it's burdock syntax so parse it like that
      then there's a case which matches specific errors and converts them to haskell
      if it doesn't match, there's an error
you need to keep the line numbers to give error messages during parsing

TODO:
use a state monad based simple parser or use megaparsec or something
generalise so you pass the fields, and it parses any combo (if the need comes up)
  then you have a second step for a specific test

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Burdock.RenamerTestParser
    (parseRenamerTestFile
    ,RenamerTest(..)
    ) where

import Prelude hiding (error, putStrLn, show)
import Burdock.Utils (error, show)

import qualified Burdock.Parse as B (parseScript)

import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Data.Char (isSpace)

import qualified Burdock.Syntax as S
import Control.Arrow (first)

import Burdock.Renamer (StaticError(..))
                      
data RenamerTest
    = RenamerTest
      {rName :: Text
      ,rSources :: [(Text,L.Text)]
      ,rScript :: Either L.Text L.Text -- left is module, right is script
      ,rResult :: Either [StaticError] L.Text
      }
      deriving Show

-- remove blank lines and comment lines
-- the test format itself only allows comments at the start of the line
-- with #, or whitespace then #, it doesn't allow trailing # comments
-- trailing comments will work inside burdock body text, but not anywhere else
-- in the test source
prepSource :: L.Text -> [(Int,L.Text)]
prepSource src = 
    let l1 = L.lines src
        l2 = zip [1..] l1
    in flip filter l2 $ \(_,l) ->
            let l' = L.dropWhile isSpace l
            in l' /= "" && L.head l' /= '#'

parseRenamerTestFile :: Text -> L.Text -> Either Text [RenamerTest]
parseRenamerTestFile sfn src = parseAnother $ prepSource src
  where
    parseAnother [] = Right []
    parseAnother (l:ls) | Just t <- readField "Test:" l = do
        (fs,ls') <- parseFiles ls
        (sc,ls'') <- parseScriptOrModule ls'
        (res,ls''') <- parseResult ls''
        (RenamerTest t fs sc res:) <$> parseAnother ls'''
    parseAnother ((n,_):_) = error $ formatError n $ "expected Test:"

    parseFiles :: [(Int,L.Text)] -> Either Text ([(Text, L.Text)], [(Int,L.Text)])
    parseFiles (l:ls) | Just fn <- readField "File:" l = do
        (bdy,ls') <- readBody ls
        (morefiles,ls'') <- parseFiles ls'
        Right (((fn,bdy):morefiles), ls'')
    parseFiles ls = pure ([], ls)

    readBody xs@(x:_)
        | isRecognisedField x
        = Right ("", xs)
    readBody ((_,l):xs) = do
        (bdy',rst) <- readBody xs
        pure (L.unlines [l,bdy'], rst)
    readBody [] = pure ("",[])
    parseScriptOrModule :: [(Int,L.Text)] -> Either Text (Either L.Text L.Text, [(Int,L.Text)])
    parseScriptOrModule (l:ls) | readBlankField "Script:" l
        = first Right <$> readBody ls
    parseScriptOrModule (l:ls) | readBlankField "Module:" l
        = first Left <$> readBody ls
    parseScriptOrModule ((n,_):_) = error $ formatError n $ "expected Script: or Module:"
    parseScriptOrModule [] = error $ "expected script, got end of file"

    parseResult :: [(Int,L.Text)] -> Either Text (Either [StaticError] L.Text, [(Int,L.Text)])
    parseResult (l:ls)
        | readBlankField "Result:" l
        = do
              (r,ls') <- readBody ls
              pure (Right r, ls')
    parseResult (l:ls)
        | readBlankField "Errors:" l
        = let (x,rst) = readErrors ls
          in pure (Left x, rst)
    parseResult ((n,_):_) = error $ formatError n $ "expected Result: or Errors:"
    parseResult [] = error $ "expected result, got end of file"
    readErrors :: [(Int,L.Text)] -> ([StaticError], [(Int,L.Text)])
    readErrors (x@(n,_):_)
        | isRecognisedField x
        = error $ formatError n $ ", expected some errors"
    readErrors []
        = error $ "expected errors, got end of file"
    readErrors (l:ls) =
        first (parseError l:) $ readErrors' ls
    readErrors' :: [(Int,L.Text)] -> ([StaticError], [(Int,L.Text)])
    readErrors' ls@(l:_)
        | isRecognisedField l
        = ([], ls)
    readErrors' (l:ls) =
        first (parseError l:) $ readErrors' ls
    readErrors' [] = ([],[])
    parseError :: (Int,L.Text) -> StaticError
    parseError (n,l) =
        -- todo: allow passing in a position for parsing errors
        let ast = either (\x -> error $ formatError n x) id $ B.parseScript sfn l
        in case ast of
            S.Script [S.StmtExpr _ (S.App _ (S.Iden _ "unrecognised-identifier") [S.Construct _ ["list"] fs])]
              | Just fs' <- mapM getNm fs
              -> UnrecognisedIdentifier Nothing fs'
            S.Script [S.StmtExpr _ (S.App _ (S.Iden _ "identifier-redefined") [S.Text _ nm])]
              -> IdentifierRedefined Nothing Nothing nm
            _ -> error $ formatError n $ "couldn't parse error: " <> show ast
    getNm (S.Text _ nm) = Just nm
    getNm _ = Nothing
    isRecognisedField (_,l) =
        any (\fld -> L.isPrefixOf fld l)
        ["Test:", "File:", "Script:", "Result:", "Errors", "Module:"]
    readField nm (n,l) =
        if L.isPrefixOf nm l
        then let x = L.dropWhile isSpace $ L.drop (L.length nm) l
             in if x == ""
                then error $ formatError n "empty field"
                else Just $ L.toStrict x
        else Nothing
    readBlankField nm (n,l) =
        if L.isPrefixOf nm l
        then let x = L.dropWhile isSpace $ L.drop (L.length nm) l
             in if x == ""
                then True
                else error $ formatError n "field should be empty"
        else False
    formatError :: Int -> Text -> Text
    formatError ln msg = sfn <> ":" <> show ln <> ": " <> msg
             

{-
Test: import unrec
File: imported
a = 3                                                
Script:
import file("imported") as i
i.b
Errors:
unrecognised-identifier([list: "i"])

-}
