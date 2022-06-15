
{-# LANGUAGE QuasiQuotes #-}

module Burdock.LiterateTests
    (testData) where

import qualified Text.RawString.QQ as R

import Burdock.TestTypes

testData :: TestTree
testData = TestGroup "literate-parse-tests"
    [LiterateParseTest [R.r|

some stuff before

.. code:: burdock
               
  data BinTree:
    | leaf
    | node(value, left, right)
  end

some stuff after

|] [R.r|
  data BinTree:
    | leaf
    | node(value, left, right)
  end
|]
    ,LiterateParseTest [R.r|

some stuff before

.. code:: burdock
  
  data BinTree:
    | leaf
    | node(value, left, right)
  end

some stuff after

.. code:: burdock
  
  data BinTree:
    | leaf
    | node(value, left, right)
  end

more text
    
|]  [R.r|
  data BinTree:
    | leaf
    | node(value, left, right)
  end

  data BinTree:
    | leaf
    | node(value, left, right)
  end

|]

        

    ]
