{-

support for the relational features demo

-}

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
    ) where

import Data.List (sortOn
                 ,sort
                 ,intercalate
                 )
import Data.Typeable (Typeable)


type Record a = [(String,a)]
-- todo: add the heading
data Relation a = Relation [Record a]
    deriving (Show, Typeable)

data RelationalError = RelationalError String
    deriving Show

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

{-
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
{-parseTable :: String -> [[TableValue]]
parseTable s =
    let (l,s') = parseLine s
    in l : parseTable s'
  where
    parseLine = undefined-}
