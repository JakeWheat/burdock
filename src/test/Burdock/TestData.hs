

{-# LANGUAGE QuasiQuotes #-}

module Burdock.TestData
    (testdata
    ,TestTree(..)
    ) where

import Burdock.Syntax

import Burdock.TestTypes
import qualified Burdock.LiterateTests as LiterateTests

import qualified Text.RawString.QQ as R


testdata :: TestTree
testdata = TestGroup "allTests"
    [exprParseTests
    ,statementParseTests
    ,scriptParseTests
    ,interpreterTests
    ,LiterateTests.testData
    ]


-- todo: how to make this more readable?

exprParseTests :: TestTree
exprParseTests = TestGroup "exprParseTests" $ map (uncurry ExprParseTest)
    [("1", Num 1)
    ,("\"test\"", Text "test")
    ,([R.r|```multiline string
         "stuff"
      ```|], Text "multiline string\n\
\         \"stuff\"\n\
\      ")
    ,("test", Iden "test")
    ,("(2)", Parens (Num 2))
    ,("a(7)", App Nothing (Iden "a") [Num 7])
    ,("a + b", BinOp (Iden "a") "+" (Iden "b"))
    ,("a + b + c", BinOp (BinOp (Iden "a") "+" (Iden "b")) "+" (Iden "c"))

    ,("lam(): 2 end", lam [] (sts $ Num 2))

    ,("lam() block: 2 end", lam [] (sts $ Num 2))

    ,("lam(x): x + 1 end", lam ["x"] (sts $ BinOp (Iden "x") "+" (Num 1)))
    ,("lam(x, y): x - y end"
     ,lam ["x","y"] (sts $ BinOp (Iden "x") "-" (Iden "y")))

    ,("let x=3: x + 1 end"
     , Let [(nm "x", Num 3)]
         (sts $ BinOp (Iden "x") "+" (Num 1)))
    ,("let x=3 block: x + 1 end"
     , Let [(nm "x", Num 3)]
         (sts $ BinOp (Iden "x") "+" (Num 1)))
    ,("let x=3,y=4: x + y end"
     , Let [(nm "x", Num 3)
           ,(nm "y", Num 4)]
         (sts $ BinOp (Iden "x") "+" (Iden "y")))
    ,("let x=3: x + 4 end"
     ,Let [(nm "x", Num 3)]
         (sts $ BinOp (Iden "x") "+" (Num 4)))

    ,("let _ as b = 3: b end"
     ,Let [(AsBinding WildcardBinding "b", Num 3)] $ sts $ Iden "b")

    ,("let {a;b} = {1;2}: a end"
     ,Let [(TupleBinding [NameBinding "a", NameBinding "b"]
           ,(TupleSel [Num 1, Num 2]))] $ sts $ Iden "a")

    
    ,("letrec a = 5: a end"
     ,LetRec [(nm "a",Num 5)] (sts $ Iden "a"))
    ,("letrec a = 5 block: a end"
     ,LetRec [(nm "a",Num 5)] (sts $ Iden "a"))

    
    ,("block: end", Block [])
    ,("block: \n\
      \  a\n\
      \end", Block [StmtExpr $ Iden "a"])
    ,("block: \n\
      \  a\n\
      \  1 + 2\n\
      \end", Block [StmtExpr $ Iden "a", StmtExpr $ BinOp (Num 1) "+" (Num 2)])

    ,("if n == 1: 1 else: 2 end"
     ,If [(BinOp (Iden "n") "==" (Num 1), sts $ Num 1)] (Just (sts $ Num 2)))

    ,("if n == 1 block: 1 else: 2 end"
     ,If [(BinOp (Iden "n") "==" (Num 1), sts $ Num 1)] (Just (sts $ Num 2)))

    ,("if n == 1:\n\
      \  0\n\
      \else if n == 2:\n\
      \  1\n\
      \else:\n\
      \  2\n\
      \end"
     ,If [(BinOp (Iden "n") "==" (Num 1), sts $ Num 0)
         ,(BinOp (Iden "n") "==" (Num 2), sts $ Num 1)] (Just (sts $ Num 2)))

    ,("ask: | otherwise: a end", Ask [] (Just (sts $ Iden "a")))
    ,("ask block: | otherwise: a end", Ask [] (Just (sts $ Iden "a")))
    ,("ask:\n\
      \   | a == b then: c\n\
      \   | c == d then: e\n\
      \   | otherwise: f\n\
      \end"
      ,Ask [(BinOp (Iden "a") "==" (Iden "b"), sts $ Iden "c")
           ,(BinOp (Iden "c") "==" (Iden "d"), sts $ Iden "e")
           ] (Just (sts $ Iden "f")))
    ,("ask:\n\
      \   | a == b then: c\n\
      \   | c == d then: e\n\
      \end"
      ,Ask [(BinOp (Iden "a") "==" (Iden "b"), sts $ Iden "c")
           ,(BinOp (Iden "c") "==" (Iden "d"), sts $ Iden "e")
           ] Nothing)


    ,("let shadow a = 4 : a end"
     ,Let [(ShadowBinding "a", Num 4)] (sts $ Iden "a"))
    ,("a.b", DotExpr (Iden "a") "b")
    ,("f(1,2).c", DotExpr (App Nothing (Iden "f") [Num 1, Num 2]) "c")
     ,("cases a :: List:\n\
      \  | empty => \"empty\"\n\
      \  | link(f, r) => \"link\"\n\
      \end"
     ,Cases (Iden "a") (Just (TName ["List"]))
        [(NameBinding "empty", Nothing, sts $ Text "empty")
        ,(VariantBinding ["link"] [nm "f", nm "r"], Nothing, sts $ Text "link")]
        Nothing)

    ,("cases a:\n\
      \  | empty => \"empty\"\n\
      \  | else => \"else\"\n\
      \end"
     ,Cases (Iden "a") Nothing
        [(NameBinding "empty", Nothing, sts $ Text "empty")]
        (Just $ sts $ Text "else"))

    ,("cases a block:\n\
      \  | empty => \"empty\"\n\
      \  | else => \"else\"\n\
      \end"
     ,Cases (Iden "a") Nothing
        [(NameBinding "empty", Nothing, sts $ Text "empty")]
        (Just $ sts $ Text "else"))

    
    ,("cases a :: z.List:\n\
      \  | z.empty => \"empty\"\n\
      \  | z.link(f, r) => x\n\
      \  | else => \"else\"\n\
      \end"
     ,Cases (Iden "a") (Just $ TName ["z", "List"])
        [(VariantBinding ["z","empty"] [], Nothing, sts $ Text "empty")
        ,(VariantBinding ["z","link"] [nm "f", nm "r"], Nothing, sts $ Iden "x")]
        (Just $ sts $ Text "else"))

     ,("cases a:\n\
      \  | empty when c => \"empty\"\n\
      \  | link(f, r) when d => \"link\"\n\
      \end"
     ,Cases (Iden "a") Nothing
        [(NameBinding "empty", Just (Iden "c"), sts $ Text "empty")
        ,(VariantBinding ["link"] [nm "f", nm "r"], Just (Iden "d"), sts $ Text "link")]
        Nothing)

     ,("cases a:\n\
      \  | 1 => true\n\
      \  | \"test\" => false\n\
      \end"
     ,Cases (Iden "a") Nothing
        [(NumberLitBinding 1, Nothing, sts $ Iden "true")
        ,(StringLitBinding "test", Nothing, sts $ Iden "false")]
        Nothing)

    
    -- tuple selector
    ,("{\"a\";\"b\";true}", TupleSel [Text "a", Text "b", Iden "true"])
    ,("{1}", TupleSel [Num 1])
    -- tuple field get
    ,("myobj4.{0}", TupleGet (Iden "myobj4") 0)
    ,("f().{1}", TupleGet (App Nothing (Iden "f") []) 1)
    -- record selector
    ,("{a: \"one\", b : 2, c : x }", RecordSel [("a", Text "one")
                                               ,("b", Num 2)
                                               ,("c", Iden "x")])
    ,("{}", RecordSel [])

    ,("{ method m(self, x): self.y + x end, y: 22 }"
     ,RecordSel [("m", MethodExpr $ Method
                     (FunHeader [] [NameBinding "self", NameBinding "x"] Nothing)
                     [StmtExpr $ BinOp (DotExpr (Iden "self") "y") "+" (Iden "x")])
                ,("y", Num 22)])
    
    ,("[list:]", Construct ["list"] [])
    ,("[list: 1,2,3]", Construct ["list"] [Num 1, Num 2, Num 3])
    ,("[my.list:]", Construct ["my", "list"] [])

    ,("type-let a = b: c end"
     ,TypeLet [TypeDecl "a" [] (TName ["b"])] (sts $ Iden "c"))
    ,("type-let a<x> = b: c end"
     ,TypeLet [TypeDecl "a" ["x"] (TName ["b"])] (sts $ Iden "c"))
    
    ,("assert-type-compat(x :: Number)"
     ,AssertTypeCompat (Iden "x") (TName ["Number"]))
    ,("assert-type-compat(x :: X.Something)"
     ,AssertTypeCompat (Iden "x") (TName ["X","Something"]))
    ,("assert-type-compat(x :: {Number; Boolean})"
     ,AssertTypeCompat (Iden "x") (TTuple [TName ["Number"], TName["Boolean"]]))
    ,("assert-type-compat(x :: {Number})"
     ,AssertTypeCompat (Iden "x") (TTuple [TName ["Number"]]))

    ,("assert-type-compat(x :: {Number})"
     ,AssertTypeCompat (Iden "x") (TTuple [TName ["Number"]]))
    ,("assert-type-compat(x :: List<Number>)"
     ,AssertTypeCompat (Iden "x") (TParam ["List"] [TName ["Number"]]))
    ,("assert-type-compat(x :: Stuff<Number, Number>)"
     ,AssertTypeCompat (Iden "x") (TParam ["Stuff"] [TName ["Number"], TName ["Number"]]))
    ,("assert-type-compat(x :: {x :: Number, y :: String})"
     ,AssertTypeCompat (Iden "x") (TRecord [("x", TName ["Number"]),("y", TName ["String"])]))
    ,("assert-type-compat(x :: {})"
     ,AssertTypeCompat (Iden "x") (TRecord []))

    ,("assert-type-compat(x :: String, (String -> String) -> String)"
     ,AssertTypeCompat (Iden "x") (TArrow [TName ["String"], TParens (TArrow [TName ["String"]] $ TName ["String"])] $ TName ["String"]))
    ,("assert-type-compat(x :: -> String)"
     ,AssertTypeCompat (Iden "x") (TArrow [] $ TName ["String"]))
    ,("assert-type-compat(x :: String, String -> String)"
     ,AssertTypeCompat (Iden "x") (TArrow [TName ["String"], TName ["String"]] $ TName ["String"]))
    ,("assert-type-compat(x :: String -> String)"
     ,AssertTypeCompat (Iden "x") (TArrow [TName ["String"]] $ TName ["String"]))
    ,("assert-type-compat(x :: (s :: String, t :: String) -> String)"
     ,AssertTypeCompat (Iden "x") (TNamedArrow [("s",TName ["String"]), ("t", TName ["String"])] $ TName ["String"]))
    ,("assert-type-compat", Iden "assert-type-compat")

    ,("let a :: MyType = 4: a end"
     ,Let [(TypedBinding (NameBinding "a") (TName ["MyType"]), Num 4)] (sts $ Iden "a"))

    ,("callf<A,B>(x)"
     ,App Nothing (InstExpr (Iden "callf") [TName ["A"], TName ["B"]]) [Iden "x"])
    ,("callg<Predicate<A>>(x)"
     ,App Nothing (InstExpr (Iden "callg") [TParam ["Predicate"] [TName ["A"]]]) [Iden "x"])

    ,("ctor<a>"
     ,InstExpr (Iden "ctor") [TName ["a"]])

    ,([R.r|
lam(x :: Number):
  x + 1
end|]
     ,Lam (FunHeader [] [nmt "x" "Number"] Nothing)
      (sts $ BinOp (Iden "x") "+" (Num 1)))

    ,([R.r|
lam(x) -> Number:
  x + 1
end|]
     ,Lam (FunHeader [] [nm "x"] (Just $ TName ["Number"]))
      (sts $ BinOp (Iden "x") "+" (Num 1)))

    ,([R.r|
lam(x :: Number) -> Number:
  x + 1
end|]
     ,Lam (FunHeader [] [nmt "x" "Number"] (Just $ TName ["Number"]))
      (sts $ BinOp (Iden "x") "+" (Num 1)))

    ,([R.r|
lam<a>(x :: List<a>) -> Boolean:
  cases x :: List<a>:
    | empty => true
    | link(_,_) => false
  end
end|]
     ,Lam (FunHeader
            ["a"]
            [TypedBinding (NameBinding "x") (TParam ["List"] [TName ["a"]])]
            (Just $ TName ["Boolean"]))
      $ sts $ Cases (Iden "x") (Just $ TParam ["List"] [TName ["a"]])
        [(NameBinding "empty", Nothing, sts $ Iden "true")
        ,(VariantBinding ["link"] [WildcardBinding, WildcardBinding], Nothing, sts $ Iden "false")]
        Nothing)

     ,("{(y) : x + y}"
      ,CurlyLam (FunHeader [] [nm "y"] Nothing)
       $ sts $ BinOp (Iden "x") "+" (Iden "y"))

     ,("{(y) block: x + y}"
      ,CurlyLam (FunHeader [] [nm "y"] Nothing)
       $ sts $ BinOp (Iden "x") "+" (Iden "y"))


     ,("{(x,y) : x + y}"
      ,CurlyLam (FunHeader [] [nm "x",nm "y"] Nothing)
       $ sts $ BinOp (Iden "x") "+" (Iden "y"))


     ,("{(y :: Number) -> Number: x + y}"
      ,CurlyLam (FunHeader []
                [TypedBinding (NameBinding "y") $ TName ["Number"]]
                (Just $ TName ["Number"]))
       $ sts $ BinOp (Iden "x") "+" (Iden "y"))

     ,("-1 is 0 - 1"
      ,BinOp (UnaryMinus (Num 1)) "is" (BinOp (Num 0) "-" (Num 1)))
    ,("ex1!x"
     ,UnboxRef (Iden "ex1") "x")
    ,("ex2!x!y"
     ,UnboxRef (UnboxRef (Iden "ex2") "x") "y")
    ,("ex2.y!x"
     ,UnboxRef (DotExpr (Iden "ex2") "y") "x")

    ,("...", Template Nothing)

    ,([R.r|
for fold(sum from 0, number from [list: 1,2]):
  sum + number
end|], For (Iden "fold")
           [(NameBinding "sum", Num 0)
           ,(NameBinding "number", Construct ["list"] [Num 1, Num 2])]
           Nothing
           [StmtExpr $ BinOp (Iden "sum") "+" (Iden "number")])

    ,([R.r|
receive:
  | a => b
end
     |], Receive [(NameBinding "a", Nothing, sts $ Iden "b")] Nothing)

    ,([R.r|
receive block:
  | a => b
end
     |], Receive [(NameBinding "a", Nothing, sts $ Iden "b")] Nothing)

    ,([R.r|
receive:
  | pat1(a) => a
  | pat2(b) => b
end
     |], Receive [(VariantBinding ["pat1"] [nm "a"], Nothing, sts $ Iden "a")
                 ,(VariantBinding  ["pat2"] [nm "b"], Nothing, sts $ Iden "b")] Nothing)
    ,([R.r|
receive:
  | after infinity => c
end
     |], Receive [] (Just (Iden "infinity", sts $ Iden "c")))
    ,([R.r|
receive:
  | after 1.1 => c
end
     |], Receive [] (Just (Num 1.1, sts $ Iden "c")))
    ,([R.r|
receive:
  | pat1(a) => a
  | after f(10) => c
end
     |], Receive [(VariantBinding ["pat1"] [nm "a"], Nothing, sts $ Iden "a")]
                 (Just (App Nothing (Iden "f") [Num 10], sts $ Iden "c")))

    ,([R.r|
table a,b:
  row: 1, true
  row: 2, false
end|], TableSel ["a", "b"] [RowSel [Num 1, Iden "true"]
                           ,RowSel [Num 2, Iden "false"]])

    ,("method(self): self end"
     ,MethodExpr $ Method (FunHeader [] [NameBinding "self"] Nothing) (sts $ Iden "self"))

    ]
  where
    nm x = NameBinding x
    nmt x t = TypedBinding (NameBinding x) (TName [t])
    lam as = Lam (FunHeader [] (map nm as) Nothing)
    sts e = [StmtExpr e]

statementParseTests :: TestTree
statementParseTests = TestGroup "statementParseTests" $ map (uncurry StmtParseTest)
    [("a = 5"
     ,LetDecl (nm "a") (Num 5))
    ,("shadow a = 5"
     ,LetDecl (ShadowBinding "a") (Num 5))

    ,("when x == 3: 4 end"
     ,When (BinOp (Iden "x") "==" (Num 3)) (sts $ Num 4))

    ,("when x == 3 block: 4 end"
     ,When (BinOp (Iden "x") "==" (Num 3)) (sts $ Num 4))

     
    ,("var a = 5"
     ,VarDecl (snm "a") (Num 5))
    ,("a := 6"
     ,SetVar "a" (Num 6))

    ,("ex1!{x: 42}"
     ,SetRef (Iden "ex1") [("x", Num 42)])
    ,("ex1!{x: 42, y:43}"
     ,SetRef (Iden "ex1") [("x", Num 42), ("y", Num 43)])
    ,("ex1!a!{x: 42}"
     ,SetRef (UnboxRef (Iden "ex1") "a") [("x", Num 42)])

    ,("ex1.a!{x: 42}"
     ,SetRef (DotExpr (Iden "ex1") "a") [("x", Num 42)])

    ,("data BTree:\n\
      \  | node(value, left, right)\n\
      \  | leaf(value)\n\
      \end", DataDecl "BTree" [] [VariantDecl "node" [(Con, snm "value"), (Con, snm "left"), (Con, snm "right")] []
                                 ,VariantDecl "leaf" [(Con, snm "value")] []] [] Nothing)

    ,("data MyEnum:\n\
      \  | node(left, right)\n\
      \  | leaf\n\
      \end", DataDecl "MyEnum" []
             [VariantDecl "node" [(Con, snm "left"), (Con, snm "right")] []
             ,VariantDecl "leaf" [] []] [] Nothing)

    ,("data MyEnum:\n\
      \  | node(left, right)\n\
      \  | leaf()\n\
      \end", DataDecl "MyEnum" []
             [VariantDecl "node" [(Con, snm "left"), (Con, snm "right")] []
             ,VariantDecl "leaf" [] []] [] Nothing)

    ,("data Point:\n\
      \  | pt(x, y)\n\
      \end", DataDecl "Point" [] [VariantDecl "pt" [(Con, snm "x"), (Con, snm "y")] []] [] Nothing)

    ,("data Point: pt(x, y) end"
     ,DataDecl "Point" [] [VariantDecl "pt" [(Con, snm "x"), (Con, snm "y")] []] [] Nothing)

    ,("data Point: pt() end"
     ,DataDecl "Point" [] [VariantDecl "pt" [] []] [] Nothing)

    ,("data Point: pt end"
     ,DataDecl "Point" [] [VariantDecl "pt" [] []] [] Nothing)

    ,("PI :: Number = 3.141592"
     ,LetDecl (TypedBinding (NameBinding "PI") (TName ["Number"])) (Num 3.141592))

    ,([R.r|
data MyEnum:
  | node(left, right) with:
    method size(self): 1 end
end |], DataDecl "MyEnum" []
             [VariantDecl "node"
              [(Con, snm "left"), (Con, snm "right")]
                  [("size", Method (FunHeader [] [NameBinding "self"] Nothing)
                            [StmtExpr $ Num 1])]
             ] [] Nothing)

    ,([R.r|
data MyEnum:
  | node(left, right)
sharing:
  method values-equal(self, other): 1 end
end
  |], DataDecl "MyEnum" []
             [VariantDecl "node" [(Con, snm "left"), (Con, snm "right")] []]
         [("values-equal", Method (FunHeader [] [NameBinding "self", NameBinding "other"] Nothing)
                            [StmtExpr $ Num 1])] Nothing)

   ,([R.r|
data BinTree:
  | leaf
  | node(value :: Number, left :: BinTree, right :: BinTree)
end
  |]
    ,DataDecl "BinTree" []
      [VariantDecl "leaf" [] []
      ,VariantDecl "node" [(Con, snmt "value" "Number")
                          ,(Con, snmt "left" "BinTree")
                          ,(Con, snmt "right" "BinTree")] []]
      [] Nothing)

   ,([R.r|
data List<A>:
  | empty
  | link(first :: A, rest :: List<A>)
end
  |]
    ,DataDecl "List" ["A"]
      [VariantDecl "empty" [] []
      ,VariantDecl "link" [(Con, snmt "first" "A")
                          ,(Con, SimpleBinding NoShadow "rest" (Just $ TParam ["List"] [TName ["A"]]))] []
      ]
      [] Nothing)

    
    ,("fun f(a): a + 1 end"
     ,FunDecl (snm "f") (fh ["a"]) Nothing (sts $ BinOp (Iden "a") "+" (Num 1)) Nothing)

    ,("fun f(a) block: a + 1 end"
     ,FunDecl (snm "f") (fh ["a"]) Nothing (sts $ BinOp (Iden "a") "+" (Num 1)) Nothing)

    
    ,("fun f(a):\n\
      \  a = 1\n\
      \  a + 1\n\
      \end", FunDecl (snm "f")
                        (fh ["a"]) Nothing
                        ([LetDecl (nm "a") (Num 1)
                               ,StmtExpr $ BinOp (Iden "a") "+" (Num 1)]) Nothing)
    ,("fun double(n):\n\
      \  n + n\n\
      \where:\n\
      \  double(10) is 20\n\
      \  double(15) is 30\n\
      \end"
     ,FunDecl (snm "double") (fh ["n"]) Nothing (sts $ BinOp (Iden "n") "+" (Iden "n"))
      (Just [StmtExpr $ BinOp (App Nothing (Iden "double") [Num 10]) "is" (Num 20)
           ,StmtExpr $ BinOp (App Nothing (Iden "double") [Num 15]) "is" (Num 30)]))

    ,([R.r|
fun f(x):
  doc: "adds one to the argument"
  x + 1
end|]
     ,FunDecl (snm "f") (FunHeader [] [nm "x"] Nothing)
      (Just "adds one to the argument")
      (sts $ BinOp (Iden "x") "+" (Num 1)) Nothing)

    
    ,([R.r|
fun f(x :: Number):
  x + 1
end|]
     ,FunDecl (snm "f") (FunHeader [] [nmt "x" "Number"] Nothing) Nothing
      (sts $ BinOp (Iden "x") "+" (Num 1)) Nothing)

    ,([R.r|
fun f(x) -> Number:
  x + 1
end|]
     ,FunDecl (snm "f") (FunHeader [] [nm "x"] (Just $ TName ["Number"])) Nothing
      (sts $ BinOp (Iden "x") "+" (Num 1)) Nothing)

    ,([R.r|
fun f(x :: Number) -> Number:
  x + 1
end|]
     ,FunDecl (snm "f") (FunHeader [] [nmt "x" "Number"] (Just $ TName ["Number"])) Nothing
      (sts $ BinOp (Iden "x") "+" (Num 1)) Nothing)

    ,([R.r|
fun f<a>(x :: List<a>) -> Boolean:
  cases x :: List<a>:
    | empty => true
    | link(_,_) => false
  end
end|]
     ,FunDecl (snm "f") (FunHeader
            ["a"]
            [TypedBinding (NameBinding "x") (TParam ["List"] [TName ["a"]])]
            (Just $ TName ["Boolean"])) Nothing
      (sts $ Cases (Iden "x") (Just $ TParam ["List"] [TName ["a"]])
        [(NameBinding "empty", Nothing, sts $ Iden "true")
        ,(VariantBinding ["link"] [WildcardBinding, WildcardBinding], Nothing, sts $ Iden "false")]
        Nothing) Nothing)

    
    ,("rec fact = lam(x):\n\
      \    if x == 0: 1\n\
      \    else: x * fact(x - 1)\n\
      \    end\n\
      \  end"

     ,RecDecl (nm "fact")
            $ Lam (fh ["x"]) $ sts $
                    If [(BinOp (Iden "x") "==" (Num 0), sts $ Num 1)]
                    (Just $ sts (BinOp (Iden "x") "*" (App Nothing (Iden "fact") [BinOp (Iden "x") "-" (Num 1)])))
            )

    ,("type NumPredicate = (Number -> Boolean)"
     ,TypeStmt $ TypeDecl "NumPredicate" [] (TParens (TArrow [TName ["Number"]] $ TName ["Boolean"])))

    ,("type Predicate<a> = (a -> Boolean)"
     ,TypeStmt $ TypeDecl "Predicate" ["a"] (TParens (TArrow [TName ["a"]] $ TName ["Boolean"])))

    ,("ffitype BType = 'htype'"
     ,FFITypeStmt "BType" "htype")

    ,("a :: Number"
     ,Contract "a" $ TName ["Number"])

    ,("a :: String, String -> String"
     ,Contract "a" $ TArrow [TName ["String"], TName ["String"]] $ TName ["String"])


    ,("provide: * end"
     ,Provide [ProvideAll])

    ,("provide: a end"
     ,Provide [ProvideName "a"])

    ,("provide: a,b end"
     ,Provide [ProvideName "a", ProvideName "b"])

    ,("provide: a as b end"
     ,Provide [ProvideAlias "a" "b"])

    ,("provide: type A end"
     ,Provide [ProvideType "A"])

    ,("provide: data A end"
     ,Provide [ProvideData "A"])

    ,("include file(\"file.tea\")"
     ,Include (ImportSpecial "file" ["file.tea"]))

    ,("include string-dict"
     ,Include (ImportName "string-dict"))

    ,("import file(\"file.tea\") as X"
     ,Import (ImportSpecial "file" ["file.tea"]) "X")

    ,("import string-dict as X"
     ,Import (ImportName "string-dict") "X")

    ,("include from X: * end"
     ,IncludeFrom "X" [ProvideAll])

    ,("include from X: a end"
     ,IncludeFrom "X" [ProvideName "a"])

    ,("include from X: a,b end"
     ,IncludeFrom "X" [ProvideName "a", ProvideName "b"])

    ,("include from X: a as b end"
     ,IncludeFrom "X" [ProvideAlias "a" "b"])

    ,("import from string-dict: a as b end"
     ,ImportFrom (ImportName "string-dict") [ProvideAlias "a" "b"])

    -- todo: add optional alias
    ,("use package \"dir/my-package\""
     ,UsePackage "dir/my-package")
    ]
  where
    nm x = NameBinding x
    snm x = SimpleBinding NoShadow x Nothing
    fh as = FunHeader [] (map nm as) Nothing
    nmt x t = TypedBinding (NameBinding x) (TName [t])
    snmt x t = SimpleBinding NoShadow x (Just $ TName [t])
    sts e = [StmtExpr e]


scriptParseTests :: TestTree
scriptParseTests = TestGroup "scriptParseTests" $ map (uncurry ScriptParseTest)
    [([R.r|
check:
  1 is 1
  "test" is "test"
end
     |], Script [Check Nothing
                 [StmtExpr $ BinOp (Num 1) "is" (Num 1)
                 ,StmtExpr $ BinOp (Text "test") "is"(Text "test")]])

    ,([R.r|
check:
  f(x) raises "something"
end
     |], Script [Check Nothing
                 [StmtExpr $ BinOp (App Nothing (Iden "f") [Iden "x"])
                             "raises" (Text "something")]])
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
         [LetDecl (nm "a") (Num 5)
         ,StmtExpr $ App Nothing (Iden "print") [Iden "a"]
         ,Check (Just "test-1") [LetDecl (nm "b") (Num 6)
                                ,StmtExpr $ BinOp (Num 1) "is" (Num 1)]
         ,Check Nothing [StmtExpr $ BinOp (Num 2) "is" (Num 3)]
                                
         ])
    ,([R.r|
       # data decl|]
     ,Script [])
    ]
  where
    nm x = NameBinding x

interpreterTests :: TestTree
interpreterTests =
    TestGroup "interpreterTests"
    [InterpreterTestsDir "burdock-test-src/"
    ,InterpreterTestsDir "built-ins/"
    ,InterpreterTestsDir "docs/"
    ,InterpreterTestsDir "examples/"
    ,InterpreterTestsDir "packages/ffitypes-test"
    ,InterpreterTestsDir "packages/python-ffi"
    ,InterpreterTestsOptionalDir "website2/"
    {-
     packages/ffitypes-test/tests/ffitypes-test.bur"
     ,"packages/python-ffi/tests

        TestGroup "language" $ map InterpreterTestsFile
     ["burdock-test-src/basics.bur"
     ,"burdock-test-src/letrec.bur"
     ,"burdock-test-src/fun.bur"
     ,"burdock-test-src/agdt.bur"
     ,"burdock-test-src/tuples.bur"
     ,"burdock-test-src/records.bur"
     ,"burdock-test-src/list-basics.bur"
     ,"burdock-test-src/modules-import-simple.bur"
     ,"burdock-test-src/modules-import-simple-2.bur"
     ,"burdock-test-src/modules-import-simple-3.bur"
     ,"burdock-test-src/include-from-simple.bur"
     ,"burdock-test-src/include-from-simple-2.bur"
     ,"burdock-test-src/include-from-simple-3.bur"
     ,"burdock-test-src/include-simple.bur"
     ,"burdock-test-src/raise.bur"
     ,"burdock-test-src/dynamic-types.bur"
     ,"burdock-test-src/when.bur"
     ,"burdock-test-src/arithmetic.bur"
     ,"burdock-test-src/boolean.bur"
     ,"burdock-test-src/comparisons.bur"
     ,"burdock-test-src/if_ask.bur"
     ,"burdock-test-src/either.bur"
     ,"burdock-test-src/option.bur"
     ,"burdock-test-src/curried.bur"
     ,"burdock-test-src/construct.bur"
     ,"burdock-test-src/functions.bur"
     ,"burdock-test-src/ref.bur"
     ,"burdock-test-src/template.bur"
     ,"burdock-test-src/curly-lam.bur"
     ,"burdock-test-src/binding.bur"
     ,"burdock-test-src/for.bur"
     ,"burdock-test-src/methods.bur"
    ]
    ,TestGroup "built-in modules" $ map InterpreterTestsFile
     ["burdock-test-src/built-in-functions.bur"
     ,"built-ins/lists.bur"
     ,"built-ins/globals.bur"
     ,"built-ins/either.bur"
     ,"built-ins/option.bur"
     ,"built-ins/relational.bur"
     ,"built-ins/numbers.bur"
     ]
    ,TestGroup "additional tests" $ map InterpreterTestsFile
     ["burdock-test-src/lists.bur"
     ,"burdock-test-src/relational-demo.bur"
     ,"burdock-test-src/concurrency.bur"
     ]
    ,TestGroup "packages" $ map InterpreterTestsFile
     ["packages/ffitypes-test/tests/ffitypes-test.bur"
     ,"packages/python-ffi/tests/python-ffi-test.bur"
     ]
    ,TestGroup "docs" $ map InterpreterTestsFile
     ["docs/index.rst"
     ]
    ,TestGroup "docs" $ map InterpreterTestsOptionalDir
     ["website2/"]-}
    ]

