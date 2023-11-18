

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.ParseTests
    (testData
    ) where

import Burdock.Syntax

import qualified Test.Tasty.HUnit as Tst
import qualified Burdock.TestLib as Tst

import qualified Text.RawString.QQ as R
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Data (Data)
import Burdock.Parse
    (parseExpr
    ,parseStmt
    ,parseScript
    )
import Burdock.Pretty
    (prettyExpr
    ,prettyStmt
    ,prettyScript
    )

makeTests :: TestTree -> Tst.TestTree
makeTests (TestGroup nm ts) = Tst.TestGroup nm $ map makeTests ts

makeTests (ExprParseTest src ex) = makeParseTest parseExpr prettyExpr src ex
makeTests (StmtParseTest src ex) = makeParseTest parseStmt prettyStmt src ex
makeTests (ScriptParseTest src ex) = makeParseTest parseScript prettyScript src ex


makeParseTest :: (Eq a, Show a, Data a) =>
                 (T.Text -> L.Text -> Either T.Text a)
              -> (a -> L.Text)
              -> L.Text
              -> a
              -> Tst.TestTree
makeParseTest parse pretty src expected = Tst.TestCase (L.toStrict src) $ do
    --let rmsp = transformBi (\(_ :: SourcePosition) -> Nothing)
    let got = either (error . T.unpack) resetSourcePositions $ parse "" src
    Tst.assertEqual "parse" expected got
    let printed = pretty got
    let roundtrip = either (error . T.unpack) resetSourcePositions $ parse "" printed
    Tst.assertEqual "parse pretty roundtrip" expected roundtrip


data TestTree
    = TestGroup Text [TestTree]
    | ExprParseTest L.Text Expr
    | StmtParseTest L.Text Stmt
    | ScriptParseTest L.Text Script

testData :: Tst.TestTree
testData = makeTests testData'

testData' :: TestTree
testData' = TestGroup "parsetests"
    [exprParseTests
    ,statementParseTests
    ,scriptParseTests
    ]


-- todo: how to make this more readable?

exprParseTests :: TestTree
exprParseTests = TestGroup "exprParseTests" $ map (uncurry ExprParseTest)
    [("1", Num np 1)
    ,("\"test\"", Text np "test")
    ,([R.r|```multiline string
         "stuff"
      ```|], Text np "multiline string\n\
\         \"stuff\"\n\
\      ")
    ,("test", Iden np "test")
    ,("_@internal", Iden np "_@internal")
    ,("(2)", Parens np (Num np 2))
    ,("a(7)", App Nothing (Iden np "a") [Num np 7])
    ,("a + b", BinOp np (Iden np "a") "+" (Iden np "b"))
    ,("a + b + c", BinOp np (BinOp np (Iden np "a") "+" (Iden np "b")) "+" (Iden np "c"))

    ,("lam(): 2 end", lam [] (sts $ Num np 2))

    ,("lam() block: 2 end", lam [] (sts $ Num np 2))

    ,("lam(x): x + 1 end", lam ["x"] (sts $ BinOp np (Iden np "x") "+" (Num np 1)))
    ,("lam(x, y): x - y end"
     ,lam ["x","y"] (sts $ BinOp np (Iden np "x") "-" (Iden np "y")))

    ,("let x=3: x + 1 end"
     , Let np [(nm "x", Num np 3)]
         (sts $ BinOp np (Iden np "x") "+" (Num np 1)))
    ,("let x=3 block: x + 1 end"
     , Let np [(nm "x", Num np 3)]
         (sts $ BinOp np (Iden np "x") "+" (Num np 1)))
    ,("let x=3,y=4: x + y end"
     , Let np [(nm "x", Num np 3)
           ,(nm "y", Num np 4)]
         (sts $ BinOp np (Iden np "x") "+" (Iden np "y")))
    ,("let x=3: x + 4 end"
     ,Let np [(nm "x", Num np 3)]
         (sts $ BinOp np (Iden np "x") "+" (Num np 4)))

    ,("let _ as b = 3: b end"
     ,Let np [(AsBinding np (WildcardBinding np) NoShadow "b", Num np 3)] $ sts $ Iden np "b")

    ,("let _ as shadow b = 3: b end"
     ,Let np [(AsBinding np (WildcardBinding np) Shadow "b", Num np 3)] $ sts $ Iden np "b")

    
    ,("let {a;b} = {1;2}: a end"
     ,Let np [(TupleBinding np [NameBinding np "a", NameBinding np "b"]
           ,(TupleSel np [Num np 1, Num np 2]))] $ sts $ Iden np "a")

    
    ,("letrec a = 5: a end"
     ,LetRec np [(nm "a",Num np 5)] (sts $ Iden np "a"))
    ,("letrec a = 5 block: a end"
     ,LetRec np [(nm "a",Num np 5)] (sts $ Iden np "a"))

    
    ,("block: end", Block np [])
    ,("block: \n\
      \  a\n\
      \end", Block np [StmtExpr np $ Iden np "a"])
    ,("block: \n\
      \  a\n\
      \  1 + 2\n\
      \end", Block np [StmtExpr np $ Iden np "a", StmtExpr np $ BinOp np (Num np 1) "+" (Num np 2)])

    ,("if n == 1: 1 else: 2 end"
     ,If np [(BinOp np (Iden np "n") "==" (Num np 1), sts $ Num np 1)] (Just (sts $ Num np 2)))

    ,("if n == 1 block: 1 else: 2 end"
     ,If np [(BinOp np (Iden np "n") "==" (Num np 1), sts $ Num np 1)] (Just (sts $ Num np 2)))

    ,("if n == 1:\n\
      \  0\n\
      \else if n == 2:\n\
      \  1\n\
      \else:\n\
      \  2\n\
      \end"
     ,If np [(BinOp np (Iden np "n") "==" (Num np 1), sts $ Num np 0)
         ,(BinOp np (Iden np "n") "==" (Num np 2), sts $ Num np 1)] (Just (sts $ Num np 2)))

    ,("ask: | otherwise: a end", Ask np [] (Just (sts $ Iden np "a")))
    ,("ask block: | otherwise: a end", Ask np [] (Just (sts $ Iden np "a")))
    ,("ask:\n\
      \   | a == b then: c\n\
      \   | c == d then: e\n\
      \   | otherwise: f\n\
      \end"
      ,Ask np [(BinOp np (Iden np "a") "==" (Iden np "b"), sts $ Iden np "c")
           ,(BinOp np (Iden np "c") "==" (Iden np "d"), sts $ Iden np "e")
           ] (Just (sts $ Iden np "f")))
    ,("ask:\n\
      \   | a == b then: c\n\
      \   | c == d then: e\n\
      \end"
      ,Ask np [(BinOp np (Iden np "a") "==" (Iden np "b"), sts $ Iden np "c")
           ,(BinOp np (Iden np "c") "==" (Iden np "d"), sts $ Iden np "e")
           ] Nothing)


    ,("let shadow a = 4 : a end"
     ,Let np [(ShadowBinding np "a", Num np 4)] (sts $ Iden np "a"))
    ,("a.b", DotExpr np (Iden np "a") "b")
    ,("f(1,2).c", DotExpr np (App Nothing (Iden np "f") [Num np 1, Num np 2]) "c")
     ,("cases a :: List:\n\
      \  | empty => \"empty\"\n\
      \  | link(f, r) => \"link\"\n\
      \end"
     ,Cases np (Iden np "a") (Just (TName np ["List"]))
        [(NameBinding np "empty", Nothing, sts $ Text np "empty")
        ,(VariantBinding np ["link"] [nm "f", nm "r"], Nothing, sts $ Text np "link")]
        Nothing)

    ,("cases a:\n\
      \  | empty => \"empty\"\n\
      \  | else => \"else\"\n\
      \end"
     ,Cases np (Iden np "a") Nothing
        [(NameBinding np "empty", Nothing, sts $ Text np "empty")]
        (Just $ sts $ Text np "else"))

    ,("cases a block:\n\
      \  | empty => \"empty\"\n\
      \  | else => \"else\"\n\
      \end"
     ,Cases np (Iden np "a") Nothing
        [(NameBinding np "empty", Nothing, sts $ Text np "empty")]
        (Just $ sts $ Text np "else"))

    
    ,("cases a :: z.List:\n\
      \  | z.empty => \"empty\"\n\
      \  | z.link(f, r) => x\n\
      \  | else => \"else\"\n\
      \end"
     ,Cases np (Iden np "a") (Just $ TName np ["z", "List"])
        [(VariantBinding np ["z","empty"] [], Nothing, sts $ Text np "empty")
        ,(VariantBinding np ["z","link"] [nm "f", nm "r"], Nothing, sts $ Iden np "x")]
        (Just $ sts $ Text np "else"))

     ,("cases a:\n\
      \  | empty when c => \"empty\"\n\
      \  | link(f, r) when d => \"link\"\n\
      \end"
     ,Cases np (Iden np "a") Nothing
        [(NameBinding np "empty", Just (Iden np "c"), sts $ Text np "empty")
        ,(VariantBinding np ["link"] [nm "f", nm "r"], Just (Iden np "d"), sts $ Text np "link")]
        Nothing)

     ,("cases a:\n\
      \  | 1 => true\n\
      \  | \"test\" => false\n\
      \end"
     ,Cases np (Iden np "a") Nothing
        [(NumberLitBinding np 1, Nothing, sts $ Iden np "true")
        ,(StringLitBinding np "test", Nothing, sts $ Iden np "false")]
        Nothing)

    
    -- tuple selector
    ,("{\"a\";\"b\";true}", TupleSel np [Text np "a", Text np "b", Iden np "true"])
    ,("{1}", TupleSel np [Num np 1])
    -- tuple field get
    ,("myobj4.{0}", TupleGet np (Iden np "myobj4") 0)
    ,("f().{1}", TupleGet np (App Nothing (Iden np "f") []) 1)
    -- record selector
    ,("{a: \"one\", b : 2, c : x }", RecordSel np [("a", Text np "one")
                                               ,("b", Num np 2)
                                               ,("c", Iden np "x")])
    ,("{}", RecordSel np [])

    ,("{ method m(self, x): self.y + x end, y: 22 }"
     ,RecordSel np [("m", MethodExpr np $ Method
                     (FunHeader [] [NameBinding np "self", NameBinding np "x"] Nothing)
                     [StmtExpr np $ BinOp np (DotExpr np (Iden np "self") "y") "+" (Iden np "x")])
                ,("y", Num np 22)])

    ,("v.{a:1}", Extend np (Iden np "v") [("a", Num np 1)])
    ,("v.{a:1, b:2}", Extend np (Iden np "v") [("a", Num np 1)
                                        ,("b", Num np 2)])
    
    ,("[list:]", Construct np ["list"] [])
    ,("[list: 1,2,3]", Construct np ["list"] [Num np 1, Num np 2, Num np 3])
    ,("[my.list:]", Construct np ["my", "list"] [])

    ,("type-let a = b: c end"
     ,TypeLet np [TypeDecl "a" [] (TName np ["b"])] (sts $ Iden np "c"))
    ,("type-let a<x> = b: c end"
     ,TypeLet np [TypeDecl "a" ["x"] (TName np ["b"])] (sts $ Iden np "c"))
    
    ,("assert-type-compat(x :: Number)"
     ,AssertTypeCompat np (Iden np "x") (TName np ["Number"]))
    ,("assert-type-compat(x :: X.Something)"
     ,AssertTypeCompat np (Iden np "x") (TName np ["X","Something"]))
    ,("assert-type-compat(x :: {Number; Boolean})"
     ,AssertTypeCompat np (Iden np "x") (TTuple np [TName np ["Number"], TName np ["Boolean"]]))
    ,("assert-type-compat(x :: {Number})"
     ,AssertTypeCompat np (Iden np "x") (TTuple np [TName np ["Number"]]))

    ,("assert-type-compat(x :: {Number})"
     ,AssertTypeCompat np (Iden np "x") (TTuple np [TName np ["Number"]]))
    ,("assert-type-compat(x :: List<Number>)"
     ,AssertTypeCompat np (Iden np "x") (TParam np ["List"] [TName np ["Number"]]))
    ,("assert-type-compat(x :: Stuff<Number, Number>)"
     ,AssertTypeCompat np (Iden np "x") (TParam np ["Stuff"] [TName np ["Number"], TName np ["Number"]]))
    ,("assert-type-compat(x :: {x :: Number, y :: String})"
     ,AssertTypeCompat np (Iden np "x") (TRecord np [("x", TName np ["Number"]),("y", TName np ["String"])]))
    ,("assert-type-compat(x :: {})"
     ,AssertTypeCompat np (Iden np "x") (TRecord np []))

    ,("assert-type-compat(x :: String, (String -> String) -> String)"
     ,AssertTypeCompat np (Iden np "x") (TArrow np [TName np ["String"], TParens np (TArrow np [TName np ["String"]] $ TName np ["String"])] $ TName np ["String"]))
    ,("assert-type-compat(x :: -> String)"
     ,AssertTypeCompat np (Iden np "x") (TArrow np [] $ TName np ["String"]))
    ,("assert-type-compat(x :: String, String -> String)"
     ,AssertTypeCompat np (Iden np "x") (TArrow np [TName np ["String"], TName np ["String"]] $ TName np ["String"]))
    ,("assert-type-compat(x :: String -> String)"
     ,AssertTypeCompat np (Iden np "x") (TArrow np [TName np ["String"]] $ TName np ["String"]))
    ,("assert-type-compat(x :: (s :: String, t :: String) -> String)"
     ,AssertTypeCompat np (Iden np "x") (TNamedArrow np [("s",TName np ["String"]), ("t", TName np ["String"])] $ TName np ["String"]))
    ,("assert-type-compat", Iden np "assert-type-compat")

    ,("let a :: MyType = 4: a end"
     ,Let np [(TypedBinding np (NameBinding np "a") (TName np ["MyType"]), Num np 4)] (sts $ Iden np "a"))

    ,("callf<A,B>(x)"
     ,App Nothing (InstExpr np (Iden np "callf") [TName np ["A"], TName np ["B"]]) [Iden np "x"])
    ,("callg<Predicate<A>>(x)"
     ,App Nothing (InstExpr np (Iden np "callg") [TParam np ["Predicate"] [TName np ["A"]]]) [Iden np "x"])

    ,("ctor<a>"
     ,InstExpr np (Iden np "ctor") [TName np ["a"]])

    ,([R.r|
lam(x :: Number):
  x + 1
end|]
     ,Lam np (FunHeader [] [nmt "x" "Number"] Nothing)
      (sts $ BinOp np (Iden np "x") "+" (Num np 1)))

    ,([R.r|
lam(x) -> Number:
  x + 1
end|]
     ,Lam np (FunHeader [] [nm "x"] (Just $ TName np ["Number"]))
      (sts $ BinOp np (Iden np "x") "+" (Num np 1)))

    ,([R.r|
lam(x :: Number) -> Number:
  x + 1
end|]
     ,Lam np (FunHeader [] [nmt "x" "Number"] (Just $ TName np ["Number"]))
      (sts $ BinOp np (Iden np "x") "+" (Num np 1)))

    ,([R.r|
lam<a>(x :: List<a>) -> Boolean:
  cases x :: List<a>:
    | empty => true
    | link(_,_) => false
  end
end|]
     ,Lam np (FunHeader
            ["a"]
            [TypedBinding np (NameBinding np "x") (TParam np ["List"] [TName np ["a"]])]
            (Just $ TName np ["Boolean"]))
      $ sts $ Cases np (Iden np "x") (Just $ TParam np ["List"] [TName np ["a"]])
        [(NameBinding np "empty", Nothing, sts $ Iden np "true")
        ,(VariantBinding np ["link"] [WildcardBinding np, WildcardBinding np], Nothing, sts $ Iden np "false")]
        Nothing)

     ,("{(y) : x + y}"
      ,CurlyLam np (FunHeader [] [nm "y"] Nothing)
       $ sts $ BinOp np (Iden np "x") "+" (Iden np "y"))

     ,("{(y) block: x + y}"
      ,CurlyLam np (FunHeader [] [nm "y"] Nothing)
       $ sts $ BinOp np (Iden np "x") "+" (Iden np "y"))


     ,("{(x,y) : x + y}"
      ,CurlyLam np (FunHeader [] [nm "x",nm "y"] Nothing)
       $ sts $ BinOp np (Iden np "x") "+" (Iden np "y"))


     ,("{(y :: Number) -> Number: x + y}"
      ,CurlyLam np (FunHeader []
                [TypedBinding np (NameBinding np "y") $ TName np ["Number"]]
                (Just $ TName np ["Number"]))
       $ sts $ BinOp np (Iden np "x") "+" (Iden np "y"))

     ,("-1 is 0 - 1"
      ,BinOp np (UnaryMinus np (Num np 1)) "is" (BinOp np (Num np 0) "-" (Num np 1)))
    ,("ex1!x"
     ,UnboxRef np (Iden np "ex1") "x")
    ,("ex2!x!y"
     ,UnboxRef np (UnboxRef np (Iden np "ex2") "x") "y")
    ,("ex2.y!x"
     ,UnboxRef np (DotExpr np (Iden np "ex2") "y") "x")

    ,("...", Template Nothing)

    ,([R.r|
for fold(sum from 0, number from [list: 1,2]):
  sum + number
end|], For np (Iden np "fold")
           [(NameBinding np "sum", Num np 0)
           ,(NameBinding np "number", Construct np ["list"] [Num np 1, Num np 2])]
           Nothing
           [StmtExpr np $ BinOp np (Iden np "sum") "+" (Iden np "number")])

    ,([R.r|
receive:
  | a => b
end
     |], Receive np [(NameBinding np "a", Nothing, sts $ Iden np "b")] Nothing)

    ,([R.r|
receive block:
  | a => b
end
     |], Receive np [(NameBinding np "a", Nothing, sts $ Iden np "b")] Nothing)

    ,([R.r|
receive:
  | pat1(a) => a
  | pat2(b) => b
end
     |], Receive np [(VariantBinding np ["pat1"] [nm "a"], Nothing, sts $ Iden np "a")
                 ,(VariantBinding np ["pat2"] [nm "b"], Nothing, sts $ Iden np "b")] Nothing)
    ,([R.r|
receive:
  | after infinity => c
end
     |], Receive np [] (Just (Iden np "infinity", sts $ Iden np "c")))
    ,([R.r|
receive:
  | after 1.1 => c
end
     |], Receive np [] (Just (Num np 1.1, sts $ Iden np "c")))
    ,([R.r|
receive:
  | pat1(a) => a
  | after f(10) => c
end
     |], Receive np [(VariantBinding np ["pat1"] [nm "a"], Nothing, sts $ Iden np "a")]
                 (Just (App Nothing (Iden np "f") [Num np 10], sts $ Iden np "c")))

    ,([R.r|
table a,b:
  row: 1, true
  row: 2, false
end|], TableSel np ["a", "b"] [RowSel np [Num np 1, Iden np "true"]
                           ,RowSel np [Num np 2, Iden np "false"]])

    ,("method(self): self end"
     ,MethodExpr np $ Method (FunHeader [] [NameBinding np "self"] Nothing) (sts $ Iden np "self"))

    ]
  where
    nm x = NameBinding np x
    nmt x t = TypedBinding np (NameBinding np x) (TName np [t])
    lam as = Lam np (FunHeader [] (map nm as) Nothing)
    sts e = [StmtExpr np e]

statementParseTests :: TestTree
statementParseTests = TestGroup "statementParseTests" $ map (uncurry StmtParseTest)
    [("a = 5"
     ,LetDecl np (nm "a") (Num np 5))
    ,(" a = 5"
     ,LetDecl np (nm "a") (Num np 5))
    ,(" \na = 5"
     ,LetDecl np (nm "a") (Num np 5))
    ,("#test\na = 5"
     ,LetDecl np (nm "a") (Num np 5))
    ,("#|test|#a = 5"
     ,LetDecl np (nm "a") (Num np 5))
    ,("shadow a = 5"
     ,LetDecl np (ShadowBinding np "a") (Num np 5))

    ,("when x == 3: 4 end"
     ,When np (BinOp np (Iden np "x") "==" (Num np 3)) (sts $ Num np 4))

    ,("when x == 3 block: 4 end"
     ,When np (BinOp np (Iden np "x") "==" (Num np 3)) (sts $ Num np 4))

     
    ,("var a = 5"
     ,VarDecl np (snm "a") (Num np 5))
    ,("a := 6"
     ,SetVar np (Iden np "a") (Num np 6))

    ,("B.a := 6"
     ,SetVar np (DotExpr np (Iden np "B") "a") (Num np 6))

    ,("ex1!{x: 42}"
     ,SetRef np (Iden np "ex1") [("x", Num np 42)])
    ,("ex1!{x: 42, y:43}"
     ,SetRef np (Iden np "ex1") [("x", Num np 42), ("y", Num np 43)])
    ,("ex1!a!{x: 42}"
     ,SetRef np (UnboxRef np (Iden np "ex1") "a") [("x", Num np 42)])

    ,("ex1.a!{x: 42}"
     ,SetRef np (DotExpr np (Iden np "ex1") "a") [("x", Num np 42)])

    ,("data BTree:\n\
      \  | node(value, left, right)\n\
      \  | leaf(value)\n\
      \end", DataDecl np "BTree" [] [VariantDecl np "node" [(Con, snm "value"), (Con, snm "left"), (Con, snm "right")] []
                                 ,VariantDecl np "leaf" [(Con, snm "value")] []] [] Nothing)

    ,("data MyEnum:\n\
      \  | node(left, right)\n\
      \  | leaf\n\
      \end", DataDecl np "MyEnum" []
             [VariantDecl np "node" [(Con, snm "left"), (Con, snm "right")] []
             ,VariantDecl np "leaf" [] []] [] Nothing)

    ,("data MyEnum:\n\
      \  | node(left, right)\n\
      \  | leaf()\n\
      \end", DataDecl np "MyEnum" []
             [VariantDecl np "node" [(Con, snm "left"), (Con, snm "right")] []
             ,VariantDecl np "leaf" [] []] [] Nothing)

    ,("data Point:\n\
      \  | pt(x, y)\n\
      \end", DataDecl np "Point" [] [VariantDecl np "pt" [(Con, snm "x"), (Con, snm "y")] []] [] Nothing)

    ,("data Point: pt(x, y) end"
     ,DataDecl np "Point" [] [VariantDecl np "pt" [(Con, snm "x"), (Con, snm "y")] []] [] Nothing)

    ,("data Point: pt() end"
     ,DataDecl np "Point" [] [VariantDecl np "pt" [] []] [] Nothing)

    ,("data Point: pt end"
     ,DataDecl np "Point" [] [VariantDecl np "pt" [] []] [] Nothing)

    ,("PI :: Number = 3.141592"
     ,LetDecl np (TypedBinding np (NameBinding np "PI") (TName np ["Number"])) (Num np 3.141592))

    ,([R.r|
data MyEnum:
  | node(left, right) with:
    method size(self): 1 end
end |], DataDecl np "MyEnum" []
             [VariantDecl np "node"
              [(Con, snm "left"), (Con, snm "right")]
                  [("size", Method (FunHeader [] [NameBinding np "self"] Nothing)
                            [StmtExpr np $ Num np 1])]
             ] [] Nothing)

    ,([R.r|
data MyEnum:
  | node(left, right)
sharing:
  method values-equal(self, other): 1 end
end
  |], DataDecl np "MyEnum" []
             [VariantDecl np "node" [(Con, snm "left"), (Con, snm "right")] []]
         [("values-equal", Method (FunHeader [] [NameBinding np "self", NameBinding np "other"] Nothing)
                            [StmtExpr np $ Num np 1])] Nothing)

   ,([R.r|
data BinTree:
  | leaf
  | node(value :: Number, left :: BinTree, right :: BinTree)
end
  |]
    ,DataDecl np "BinTree" []
      [VariantDecl np "leaf" [] []
      ,VariantDecl np "node" [(Con, snmt "value" "Number")
                          ,(Con, snmt "left" "BinTree")
                          ,(Con, snmt "right" "BinTree")] []]
      [] Nothing)

   ,([R.r|
data List<A>:
  | empty
  | link(first :: A, rest :: List<A>)
end
  |]
    ,DataDecl np "List" ["A"]
      [VariantDecl np "empty" [] []
      ,VariantDecl np "link" [(Con, snmt "first" "A")
                          ,(Con, SimpleBinding np NoShadow "rest" (Just $ TParam np ["List"] [TName np ["A"]]))] []
      ]
      [] Nothing)

    
    ,("fun f(a): a + 1 end"
     ,FunDecl np (snm "f") (fh ["a"]) Nothing (sts $ BinOp np (Iden np "a") "+" (Num np 1)) Nothing)

    ,("fun f(a) block: a + 1 end"
     ,FunDecl np (snm "f") (fh ["a"]) Nothing (sts $ BinOp np (Iden np "a") "+" (Num np 1)) Nothing)

    
    ,("fun f(a):\n\
      \  a = 1\n\
      \  a + 1\n\
      \end", FunDecl np (snm "f")
                        (fh ["a"]) Nothing
                        ([LetDecl np (nm "a") (Num np 1)
                               ,StmtExpr np $ BinOp np (Iden np "a") "+" (Num np 1)]) Nothing)
    ,("fun double(n):\n\
      \  n + n\n\
      \where:\n\
      \  double(10) is 20\n\
      \  double(15) is 30\n\
      \end"
     ,FunDecl np (snm "double") (fh ["n"]) Nothing (sts $ BinOp np (Iden np "n") "+" (Iden np "n"))
      (Just [StmtExpr np $ BinOp np (App Nothing (Iden np "double") [Num np 10]) "is" (Num np 20)
           ,StmtExpr np $ BinOp np (App Nothing (Iden np "double") [Num np 15]) "is" (Num np 30)]))

    ,([R.r|
fun f(x):
  doc: "adds one to the argument"
  x + 1
end|]
     ,FunDecl np (snm "f") (FunHeader [] [nm "x"] Nothing)
      (Just "adds one to the argument")
      (sts $ BinOp np (Iden np "x") "+" (Num np 1)) Nothing)

    
    ,([R.r|
fun f(x :: Number):
  x + 1
end|]
     ,FunDecl np (snm "f") (FunHeader [] [nmt "x" "Number"] Nothing) Nothing
      (sts $ BinOp np (Iden np "x") "+" (Num np 1)) Nothing)

    ,([R.r|
fun f(x) -> Number:
  x + 1
end|]
     ,FunDecl np (snm "f") (FunHeader [] [nm "x"] (Just $ TName np ["Number"])) Nothing
      (sts $ BinOp np (Iden np "x") "+" (Num np 1)) Nothing)

    ,([R.r|
fun f(x :: Number) -> Number:
  x + 1
end|]
     ,FunDecl np (snm "f") (FunHeader [] [nmt "x" "Number"] (Just $ TName np ["Number"])) Nothing
      (sts $ BinOp np (Iden np "x") "+" (Num np 1)) Nothing)

    ,([R.r|
fun f<a>(x :: List<a>) -> Boolean:
  cases x :: List<a>:
    | empty => true
    | link(_,_) => false
  end
end|]
     ,FunDecl np (snm "f") (FunHeader
            ["a"]
            [TypedBinding np (NameBinding np "x") (TParam np ["List"] [TName np ["a"]])]
            (Just $ TName np ["Boolean"])) Nothing
      (sts $ Cases np (Iden np "x") (Just $ TParam np ["List"] [TName np ["a"]])
        [(NameBinding np "empty", Nothing, sts $ Iden np "true")
        ,(VariantBinding np ["link"] [WildcardBinding np, WildcardBinding np], Nothing, sts $ Iden np "false")]
        Nothing) Nothing)

    
    ,("rec fact = lam(x):\n\
      \    if x == 0: 1\n\
      \    else: x * fact(x - 1)\n\
      \    end\n\
      \  end"

     ,RecDecl np (nm "fact")
            $ Lam np (fh ["x"]) $ sts $
                    If np [(BinOp np (Iden np "x") "==" (Num np 0), sts $ Num np 1)]
                    (Just $ sts (BinOp np (Iden np "x") "*" (App Nothing (Iden np "fact") [BinOp np (Iden np "x") "-" (Num np 1)])))
            )

    ,("type NumPredicate = (Number -> Boolean)"
     ,TypeStmt np $ TypeDecl "NumPredicate" [] (TParens np (TArrow np [TName np ["Number"]] $ TName np ["Boolean"])))

    ,("type Predicate<a> = (a -> Boolean)"
     ,TypeStmt np $ TypeDecl "Predicate" ["a"] (TParens np (TArrow np [TName np ["a"]] $ TName np ["Boolean"])))

    ,("ffitype BType = 'htype'"
     ,FFITypeStmt np "BType" "htype")

    ,("a :: Number"
     ,Contract np "a" $ TName np ["Number"])

    ,("a :: String, String -> String"
     ,Contract np "a" $ TArrow np [TName np ["String"], TName np ["String"]] $ TName np ["String"])

    ,("provide: a end"
     ,Provide np [ProvideName np ["a"]])
    ,("provide: a as b end"
     ,Provide np [ProvideAlias np ["a"] "b"])
    ,("provide: * end"
     ,Provide np [ProvideAll np])
    ,("provide: * hiding (a) end"
     ,Provide np [ProvideHiding np ["a"]])
    ,("provide: * hiding (a,b) end"
     ,Provide np [ProvideHiding np ["a","b"]])
    ,("provide: * hiding () end"
     ,Provide np [ProvideHiding np []])
    ,("provide: a,b end"
     ,Provide np [ProvideName np ["a"], ProvideName np ["b"]])
    ,("provide: a.b end"
     ,Provide np [ProvideName np ["a", "b"]])
    ,("provide: a.c as b end"
     ,Provide np [ProvideAlias np ["a","c"] "b"])
    
    ,("provide: type A end"
     ,Provide np [ProvideType np ["A"]])
    ,("provide: type A as B end"
     ,Provide np [ProvideTypeAlias np ["A"] "B"])
    ,("provide: type * end"
     ,Provide np [ProvideTypeAll np])
    ,("provide: type * hiding (B) end"
     ,Provide np [ProvideTypeHiding np ["B"]])
    
    ,("provide: data A end"
     ,Provide np [ProvideData np ["A"]])
    -- data doesn't do rename
    -- and the hiding version of data has to be named
    -- you can't do data * hiding ()
    ,("provide: data a hiding (is-a) end"
     ,Provide np [ProvideDataHiding np ["a"] ["is-a"]])
    ,("provide: data * end"
     ,Provide np [ProvideDataAll np])

    ,("provide: module A end"
     ,Provide np [ProvideModule np ["A"]])
    ,("provide: module A.B end"
     ,Provide np [ProvideModule np ["A","B"]])
    ,("provide: module A as B end"
     ,Provide np [ProvideModuleAlias np ["A"] "B"])
    ,("provide: module A.B as B end"
     ,Provide np [ProvideModuleAlias np ["A","B"] "B"])

    ,("provide from a: * end"
     ,ProvideFrom np "a" [ProvideAll np])
    
    ,("include file(\"file.tea\")"
     ,Include np (ImportSpecial "file" ["file.tea"]))

    ,("include string-dict"
     ,Include np (ImportName ["string-dict"]))

    ,("include package.module"
     ,Include np (ImportName ["package", "module"]))
    
    ,("import file(\"file.tea\") as X"
     ,Import np (ImportSpecial "file" ["file.tea"]) "X")

    ,("import string-dict as X"
     ,Import np (ImportName ["string-dict"]) "X")

    ,("include from X: * end"
     ,IncludeFrom np "X" [ProvideAll np])

    ,("include from X: a end"
     ,IncludeFrom np "X" [ProvideName np ["a"]])

    ,("include from X: a,b end"
     ,IncludeFrom np "X" [ProvideName np ["a"], ProvideName np ["b"]])

    ,("include from X: a as b end"
     ,IncludeFrom np "X" [ProvideAlias np ["a"] "b"])

    ,("import from string-dict: a as b end"
     ,ImportFrom np (ImportName ["string-dict"]) [ProvideAlias np ["a"] "b"])

    -- todo: add optional alias
    ,("use package \"dir/my-package\""
     ,UsePackage np "dir/my-package")

    ,("use context test"
     ,UseContext np (ImportName ["test"]))

    ]
  where
    nm x = NameBinding np x
    snm x = SimpleBinding np NoShadow x Nothing
    fh as = FunHeader [] (map nm as) Nothing
    nmt x t = TypedBinding np (NameBinding np x) (TName np [t])
    snmt x t = SimpleBinding np NoShadow x (Just $ TName np [t])
    sts e = [StmtExpr np e]


scriptParseTests :: TestTree
scriptParseTests = TestGroup "scriptParseTests" $ map (uncurry ScriptParseTest)
    [([R.r|
check:
  1 is 1
  "test" is "test"
end
     |], Script [Check np Nothing
                 [StmtExpr np $ BinOp np (Num np 1) "is" (Num np 1)
                 ,StmtExpr np $ BinOp np (Text np "test") "is"(Text np "test")]])

    ,([R.r|
check:
  f(x) raises "something"
end
     |], Script [Check np Nothing
                 [StmtExpr np $ BinOp np (App Nothing (Iden np "f") [Iden np "x"])
                             "raises" (Text np "something")]])
    ,([R.r|

a = 5
print(a)

check "test-1":
  b = 6
  1 is 1
end                    

check:
  2 is 3
end

     |], Script
         [LetDecl np (nm "a") (Num np 5)
         ,StmtExpr np $ App Nothing (Iden np "print") [Iden np "a"]
         ,Check np (Just "test-1") [LetDecl np (nm "b") (Num np 6)
                                   ,StmtExpr np $ BinOp np (Num np 1) "is" (Num np 1)]
         ,Check np Nothing [StmtExpr np $ BinOp np (Num np 2) "is" (Num np 3)]
                                
         ])
    ,([R.r|
       # data decl|]
     ,Script [])
    ]
  where
    nm x = NameBinding np x


np :: SourcePosition
np = Nothing
