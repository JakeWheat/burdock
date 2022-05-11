{-

support for the relational features demo

-}
{-# LANGUAGE MultiWayIf #-}
module Burdock.Relational
    (toList
    ,fromList
    ,tableDee
    ,tableDum

    ,relEqual
    ,relUnion
    ,relWhere
    ,relUpdate
    ,relProject
    ,relRename
    ,relJoin
    ,relNotMatching
    ,relGroup
    ,relUngroup
    ,relSummarize
    
    ,RelationalError(..)
    ,showRel
    ,Relation
    ,Record

    ,parseTable
    ) where

import Data.List (sortOn
                 ,sort
                 ,intercalate
                 ,(\\)
                 ,intersect
                 ,partition
                 )
import Data.Typeable (Typeable)

import Data.Maybe (catMaybes, isNothing)

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
import Control.Monad (void
                     ,when
                     ,filterM
                     ,foldM)

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
showRel :: Show a => Relation a -> String
showRel (Relation r) = intercalate "\n" $ map show r

toList :: Relation a -> Either RelationalError [Record a]
toList (Relation a) = pure a

fromList :: [Record a] -> Either RelationalError (Relation a)
fromList = pure . Relation

-- check all tuples consistent with heading/each other
-- check the invariants: sorted records, sorted rows, no duplicate rows
_checkRelWellFormed :: Relation a -> Maybe String
_checkRelWellFormed = undefined

tableDee :: Relation a
tableDee = Relation [[]]

tableDum :: Relation a
tableDum = Relation []

sortedRecord :: Record a -> Record a
sortedRecord = sortOn fst

---------------------------------------

-- main functions on relations

relEqual :: Ord a => Relation a -> Relation a -> Either RelationalError Bool
relEqual (Relation a) (Relation b) =
    -- todo: sort on fromList/other creation of relations
    -- todo: check headings for consistency
    let a' = map sortedRecord a
        b' = map sortedRecord b
    in pure $ sort a' == sort b'

-- todo: consistency check
-- todo: remove duplicates
-- todo: maintain sorted order
relUnion :: Relation a -> Relation a -> Either RelationalError (Relation a)
relUnion (Relation a) (Relation b) = pure $ Relation $ a ++ b


relWhere :: Applicative m =>
                  Relation a
               -> (Record a -> m Bool)
               -> m (Either RelationalError (Relation a))
relWhere (Relation rs) pr =
    (pure . Relation) <$> filterM pr rs


relUpdate :: Monad m =>
                  Relation a
               -> (Record a -> m (Record a))
               -> (Record a -> m Bool)
               -> m (Either RelationalError (Relation a))
relUpdate (Relation rs) upd pr =
    (pure . Relation) <$> mapM f rs
  where
    --f :: Record a -> m (Record a)
    f r = do
        t <- pr r
        if t
           then upd r
           else pure r


relProject :: [String] -> Relation a -> Either RelationalError (Relation a)
relProject cs (Relation rs)  =
    pure $ Relation $ map (filter ((`elem` cs) . fst)) rs

relRename :: [(String,String)] -> Relation a -> Either RelationalError (Relation a)
relRename cs (Relation rs)  =
    pure $ Relation $ map (renameRecord cs) rs
  where
    renameRecord [] r = r
    renameRecord ((f,t):cs') r =
        let r' = flip map r $ \(n,v) ->
                (if n == f
                 then t
                 else n, v)
        in renameRecord cs' r'

relJoin :: Eq a => Relation a -> Relation a -> Either RelationalError (Relation a)
relJoin (Relation rs) (Relation ts) = do
    pure $ Relation $ catMaybes $
        [ joinRecs r t
        | r <- rs
        , t <- ts]

joinRecs :: Eq a => Record a -> Record a -> Maybe (Record a)
joinRecs as bs =
        let ks = intersect (map fst as) (map fst bs)
            avs = (map fst as) \\ ks
            bvs = (map fst bs) \\ ks
            proj js r = filter ((`elem` js) . fst) r
        in if proj ks as == proj ks bs
           then Just (proj ks as ++ proj avs as ++ proj bvs bs)
           else Nothing

relNotMatching :: Eq a => Relation a -> Relation a -> Either RelationalError (Relation a)
relNotMatching (Relation rs) (Relation ts) = do
    pure $ Relation $ flip filter rs $ \r ->
        and $ map isNothing $ map (joinRecs r) ts

relGroup :: (Ord a, Show a, Eq a) =>
            (Relation a -> a)
         -> [String]
         -> String
         -> Relation a
         -> Either RelationalError (Relation a)
relGroup makeRelVal knms gnm (Relation as) =
    pure $ Relation $ newKey $ sortByKeys as
  where
    sortByKeys = sortOn splitKeysOut
    splitKeysOut = partition ((`elem` knms) . fst)
    newKey [] = []
    newKey (r:rs) =
        let (kr,vr) = splitKeysOut r
        in ctuKey kr [vr] rs
    makeRec k vals =
        k ++ [(gnm, makeRelVal $ either (error . show) id $ fromList $ reverse vals)]
    ctuKey curKey curVals [] = [makeRec curKey curVals]
    ctuKey curKey curVals (r:rs) =
         let (kr,vr) = splitKeysOut r
         in if kr == curKey
            then ctuKey curKey (vr:curVals) rs
            else makeRec curKey curVals : ctuKey kr [vr] rs

relUngroup :: (a -> [Record a])
           -> String
           -> Relation a
           -> Either RelationalError (Relation a)
relUngroup unmakeRel k (Relation as) =
    pure $ Relation $ concatMap ungroup as
  where
    ungroup r =
        let v = maybe (error $ "ungroup key not found: " ++ k) id
                $ lookup k r
            v' = unmakeRel v
            kpart = filter ((/=k) . fst) r
        in map (kpart ++) v'

relSummarize :: Monad m =>
                (a, a -> a -> m a)
             -> String
             -> Relation a
             -> m (Either RelationalError a)
relSummarize (z, fop) c (Relation as) =
    let cs = maybe (error $ "col not found: " ++ c) id
             $ mapM (lookup c) as
    in Right <$> foldM fop z cs


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
