{-

support for the relational features demo

-}
{-# LANGUAGE MultiWayIf #-}
module Burdock.Relational
    (toList
    ,fromList
    ,tableDee
    ,tableDum
    ,relationsEqual
    ,union
    ,RelationalError(..)
    ,showRelation
    ,Relation

    ,parseTable
    ) where

import Data.List (sortOn
                 ,sort
                 ,intercalate
                 )
import Data.Typeable (Typeable)

import Data.Maybe (catMaybes)

import Text.Megaparsec (Parsec
                       ,(<|>)
                       ,parse
                       ,eof
                       ,option
                       ,(<?>)
                       ,many
                       ,takeWhileP
                       ,takeWhile1P
                       ,errorBundlePretty
                       ,satisfy
                       )

import Text.Megaparsec.Char (char
                            ,letterChar
                            )
import Data.Void (Void)
import Control.Monad (void, when)

import Data.Char (isAlphaNum,isDigit,isSpace)
import Text.Read (readMaybe)

import Burdock.Scientific

---------------------------------------

type Record a = [(String,a)]
-- todo: add the heading
data Relation a = Relation [Record a]
    deriving (Show, Typeable)

data RelationalError = RelationalError String
    deriving Show

---------------------------------------

-- basic stuff

-- todo: do something nicer
showRelation :: Show a => Relation a -> String
showRelation (Relation r) = intercalate "\n" $ map show r

toList :: Relation a -> Either RelationalError [Record a]
toList (Relation a) = pure a

fromList :: [Record a] -> Either RelationalError (Relation a)
fromList = pure . Relation

-- check all tuples consistent with heading/each other
-- check the invariants: sorted records, sorted rows, no duplicate rows
_checkRelationWellFormed :: Relation a -> Maybe String
_checkRelationWellFormed = undefined

tableDee :: Relation a
tableDee = Relation [[]]

tableDum :: Relation a
tableDum = Relation []

sortedRecord :: Record a -> Record a
sortedRecord = sortOn fst

---------------------------------------

-- main functions on relations

relationsEqual :: Ord a => Relation a -> Relation a -> Either RelationalError Bool
relationsEqual (Relation a) (Relation b) =
    -- todo: sort on fromList/other creation of relations
    -- todo: check headings for consistency
    let a' = map sortedRecord a
        b' = map sortedRecord b
    in pure $ sort a' == sort b'

-- todo: consistency check
-- todo: remove duplicates
-- todo: maintain sorted order
union :: Relation a -> Relation a -> Either RelationalError (Relation a)
union (Relation a) (Relation b) = pure $ Relation $ a ++ b



---------------------------------------

{-

quick test parser for creating larger relation values for testing and
examples
it's a simple version of a csv parser

```
a,b,c
1,2,3
3,4,5
```
it will guess the types:
number -> number
true|false -> bool
other text -> text

-}

type MakeBool a = Bool -> a
type MakeString a = String -> a
type MakeNumber a = Scientific -> a

parseTable :: (Eq a, Show a) => (MakeBool a, MakeString a, MakeNumber a) -> String -> Either String (Relation a)
parseTable fs src =
    either (Left . errorBundlePretty) Right $
    parse (whiteSpace *> parseT fs <* eof) "" src

type Parser = Parsec Void String

parseT :: (Eq a, Show a) => (MakeBool a, MakeString a, MakeNumber a) -> Parser (Relation a)
parseT fs = do
    h <- parseHeader
    Relation <$> catMaybes <$> many (parseLine fs h)

parseHeader :: Parser [String]
parseHeader = commaSep1 iden <* lexeme_ (char_ '\n')

parseLine :: (Eq a, Show a) => (MakeBool a, MakeString a, MakeNumber a) -> [String]
          -> Parser (Maybe (Record a))
parseLine (mb, ms, mn) h = do
    fs <- commaSep1 field <* (lexeme_ (char_ '\n') <|> eof)
    if fs == [ms ""]
       then pure Nothing
       else do
        when (length h /= length fs)
            $ fail $ "wrong number of fields on line\n" ++ show h ++ "\n" ++ show fs
        pure $ Just (zip h fs)
  where
    field = do
        f' <- takeWhile1P Nothing (`notElem` "\n,")
        let f = trim f'
        pure $ if | not (null f) && all isDigit f -> mn . myr $ f
                  | f == "true" -> mb True
                  | f == "false" -> mb False
                  | otherwise -> ms f
    trim :: String -> String
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    myr s = maybe (error $ "no read num? " ++ s) id $ readMaybe s
    
iden :: Parser String
iden =
    lexeme ((:)
    <$> (letterChar <|> char '_' <|> char '-')
    <*> takeWhileP Nothing (\a -> (isAlphaNum a || a `elem` "?-+_")))
    <?> "identifier"


lexeme :: Parser a -> Parser a
lexeme f = f <* whiteSpace

lexeme_ :: Parser a -> Parser ()
lexeme_ = void . lexeme

whiteSpace :: Parser ()
whiteSpace = void (satisfy (`elem` "\t ")) <|> pure ()

xSep1 :: Char -> Parser f -> Parser [f]
xSep1 x f = (:) <$> f <*> option [] (lexeme_ (char_ x) *> xSep1 x f)

commaSep1 :: Parser f -> Parser [f]
commaSep1 = xSep1 ','

char_ :: Char -> Parser ()
char_ x = () <$ char x
