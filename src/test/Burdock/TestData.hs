

{-# LANGUAGE QuasiQuotes #-}

module Burdock.TestData
    (testdata
    ,TestTree(..)
    ) where

import Burdock.Syntax

import qualified Text.RawString.QQ as R



data TestTree = TestGroup String [TestTree]
              | ExprParseTest String Expr
              | StmtParseTest String Stmt
              | ScriptParseTest String Script
              | InterpreterTestsFile FilePath

testdata :: TestTree
testdata = TestGroup "allTests"
    [exprParseTests
    ,scriptParseTests
    ,interpreterTests
    ]


-- todo: how to make this more readable?

exprParseTests :: TestTree
exprParseTests = TestGroup "exprParseTests" $ map (uncurry ExprParseTest)
    [("1", Num 1)
    ,("\"test\"", Text "test") 
    ,("test", Iden "test")
    ,("(2)", Parens (Num 2))
    ,("a(7)", App Nothing (Iden "a") [] [Num 7])
    ,("a + b", BinOp (Iden "a") "+" (Iden "b"))
    ,("a + b + c", BinOp (BinOp (Iden "a") "+" (Iden "b")) "+" (Iden "c"))

    ,("lam(): 2 end", lam [] (Num 2))
    ,("lam(x): x + 1 end", lam ["x"] (BinOp (Iden "x") "+" (Num 1)))
    ,("lam(x, y): x - y end"
     ,lam ["x","y"] (BinOp (Iden "x") "-" (Iden "y")))

    ,("let x=3,y=4: x + y end"
     , Let [(nm "x", Num 3)
           ,(nm "y", Num 4)]
         (BinOp (Iden "x") "+" (Iden "y")))
    ,("let x=3: x + 4 end"
     ,Let [(nm "x", Num 3)]
         (BinOp (Iden "x") "+" (Num 4)))
    
    ,("letrec a = 5: a end"
     ,LetRec [(nm "a",Num 5)] (Iden "a"))

    ,("block: end", Block [])
    ,("block: \n\
      \  a\n\
      \end", Block [StmtExpr $ Iden "a"])
    ,("block: \n\
      \  a\n\
      \  1 + 2\n\
      \end", Block [StmtExpr $ Iden "a", StmtExpr $ BinOp (Num 1) "+" (Num 2)])

    ,("if n == 1: 1 else: 2 end"
     ,If [(BinOp (Iden "n") "==" (Num 1), Num 1)] (Just (Num 2)))
    ,("if n == 1:\n\
      \  0\n\
      \else if n == 2:\n\
      \  1\n\
      \else:\n\
      \  2\n\
      \end"
     ,If [(BinOp (Iden "n") "==" (Num 1), Num 0)
         ,(BinOp (Iden "n") "==" (Num 2), Num 1)] (Just (Num 2)))
    ,("let shadow a = 4 : a end"
     ,Let [(NameBinding Shadow "a" Nothing, Num 4)] (Iden "a"))
    ,("a.b", DotExpr (Iden "a") "b")
    ,("f(1,2).c", DotExpr (App Nothing (Iden "f") [] [Num 1, Num 2]) "c")
     ,("cases(List) a:\n\
      \  | empty => \"empty\"\n\
      \  | link(f, r) => \"link\"\n\
      \end"
     ,Cases "List" (Iden "a")
        [(CaseBinding ["empty"] [], Text "empty")
        ,(CaseBinding ["link"] [nm "f", nm "r"], Text "link")]
        Nothing)

    ,("cases(List) a:\n\
      \  | empty => \"empty\"\n\
      \  | else => \"else\"\n\
      \end"
     ,Cases "List" (Iden "a")
        [(CaseBinding ["empty"] [], Text "empty")]
        (Just $ Text "else"))

    ,("cases(z.List) a:\n\
      \  | z.empty => \"empty\"\n\
      \  | z.link(f, r) => x\n\
      \  | else => \"else\"\n\
      \end"
     ,Cases "z.List" (Iden "a")
        [(CaseBinding ["z","empty"] [], Text "empty")
        ,(CaseBinding ["z","link"] [nm "f", nm "r"], Iden "x")]
        (Just $ Text "else"))

    -- tuple selector
    ,("{\"a\";\"b\";true}", TupleSel [Text "a", Text "b", Iden "true"])
    ,("{1}", TupleSel [Num 1])
    -- tuple field get
    ,("myobj4.{0}", TupleGet (Iden "myobj4") 0)
    ,("f().{1}", TupleGet (App Nothing (Iden "f") [] []) 1)
    -- record selector
    ,("{a: \"one\", b : 2, c : x }", RecordSel [("a", Text "one")
                                               ,("b", Num 2)
                                               ,("c", Iden "x")])
    ,("{}", RecordSel [])

    ,("[list:]", Construct (Iden "list") [])
    ,("[list: 1,2,3]", Construct (Iden "list") [Num 1, Num 2, Num 3])

    ,("assert-type-compat(x :: Number)"
     ,AssertTypeCompat (Iden "x") (TName ["Number"]))
    ,("assert-type-compat(x :: X.Something)"
     ,AssertTypeCompat (Iden "x") (TName ["X","Something"]))
    ,("assert-type-compat(x :: {Number; Boolean})"
     ,AssertTypeCompat (Iden "x") (TTuple [TName ["Number"], TName["Boolean"]]))
    ,("assert-type-compat(x :: {Number})"
     ,AssertTypeCompat (Iden "x") (TTuple [TName ["Number"]]))
    ,("assert-type-compat(x :: List<Number>)"
     ,AssertTypeCompat (Iden "x") (TParam ["List"] [TName ["Number"]]))
    ,("assert-type-compat(x :: Stuff<Number, Number>)"
     ,AssertTypeCompat (Iden "x") (TParam ["Stuff"] [TName ["Number"], TName ["Number"]]))
    ,("assert-type-compat(x :: {x :: Number, y :: String})"
     ,AssertTypeCompat (Iden "x") (TRecord [("x", TName ["Number"]),("y", TName ["String"])]))
    ,("assert-type-compat(x :: String, (String -> String) -> String)"
     ,AssertTypeCompat (Iden "x") (TArrow [TName ["String"], TParens (TArrow [TName ["String"]] $ TName ["String"])] $ TName ["String"]))

    ,("assert-type-compat(x :: String, String -> String)"
     ,AssertTypeCompat (Iden "x") (TArrow [TName ["String"], TName ["String"]] $ TName ["String"]))
    ,("assert-type-compat(x :: String -> String)"
     ,AssertTypeCompat (Iden "x") (TArrow [TName ["String"]] $ TName ["String"]))
    ,("assert-type-compat(x :: (s :: String, t :: String) -> String)"
     ,AssertTypeCompat (Iden "x") (TNamedArrow [("s",TName ["String"]), ("t", TName ["String"])] $ TName ["String"]))
    ,("assert-type-compat", Iden "assert-type-compat")
    ]
  where
    nm x = NameBinding NoShadow x Nothing
    lam as = Lam (FunHeader [] (map nm as) Nothing)


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
         ,StmtExpr $ App Nothing (Iden "print") [] [Iden "a"]
         ,Check (Just "test-1") [LetDecl (nm "b") (Num 6)
                                ,StmtExpr $ BinOp (Num 1) "is" (Num 1)]
         ,Check Nothing [StmtExpr $ BinOp (Num 2) "is" (Num 3)]
                                
         ])
    ,([R.r|
       var a = 5
       a := 6|]
     ,Script [VarDecl (nm "a") (Num 5)
             ,SetVar "a" (Num 6)])
    ,([R.r|
       # data decl|]
     ,Script [])

    ,("data BTree:\n\
      \  | node(value, left, right)\n\
      \  | leaf(value)\n\
      \end", Script [DataDecl "BTree" [] [VariantDecl "node" [(Con, "value"), (Con, "left"), (Con, "right")]
                                         ,VariantDecl "leaf" [(Con, "value")]] Nothing])

    ,("data MyEnum:\n\
      \  | node(left, right)\n\
      \  | leaf\n\
      \end", Script [DataDecl "MyEnum" [] [VariantDecl "node" [(Con, "left"), (Con, "right")]
                    ,VariantDecl "leaf" []] Nothing])

    ,("data MyEnum:\n\
      \  | node(left, right)\n\
      \  | leaf()\n\
      \end", Script [DataDecl "MyEnum" [] [VariantDecl "node" [(Con, "left"), (Con, "right")]
                    ,VariantDecl "leaf" []] Nothing])

    ,("data Point:\n\
      \  | pt(x, y)\n\
      \end", Script [DataDecl "Point" [] [VariantDecl "pt" [(Con, "x"), (Con, "y")]] Nothing])

    ,("data Point: pt(x, y) end"
     ,Script [DataDecl "Point" [] [VariantDecl "pt" [(Con, "x"), (Con, "y")]] Nothing])

    ,("data Point: pt() end"
     ,Script [DataDecl "Point" [] [VariantDecl "pt" []] Nothing])

    ,("data Point: pt end"
     ,Script [DataDecl "Point" [] [VariantDecl "pt" []] Nothing])

    ,("fun f(a): a + 1 end"
     ,Script[FunDecl (nm "f") (fh ["a"]) (BinOp (Iden "a") "+" (Num 1)) Nothing])

    ,("fun f(a):\n\
      \  a = 1\n\
      \  a + 1\n\
      \end", Script [FunDecl (nm "f")
                        (fh ["a"])
                        (Block [LetDecl (nm "a") (Num 1)
                               ,StmtExpr $ BinOp (Iden "a") "+" (Num 1)]) Nothing])
    ,("fun double(n):\n\
      \  n + n\n\
      \where:\n\
      \  double(10) is 20\n\
      \  double(15) is 30\n\
      \end"
     ,Script [FunDecl (nm "double") (fh ["n"]) (BinOp (Iden "n") "+" (Iden "n"))
      (Just [StmtExpr $ BinOp (App Nothing (Iden "double") [] [Num 10]) "is" (Num 20)
           ,StmtExpr $ BinOp (App Nothing (Iden "double") [] [Num 15]) "is" (Num 30)])])

    ,("rec fact = lam(x):\n\
      \    if x == 0: 1\n\
      \    else: x * fact(x - 1)\n\
      \    end\n\
      \  end"

     ,Script [RecDecl (nm "fact")
            $ Lam (fh ["x"]) $
                    If [(BinOp (Iden "x") "==" (Num 0), Num 1)]
                    (Just (BinOp (Iden "x") "*" (App Nothing (Iden "fact") [] [BinOp (Iden "x") "-" (Num 1)])))
            ])

    ,("provide: * end\n\
      \1"
     ,Script [Provide [ProvideAll]
             ,StmtExpr $ Num 1])

    ,("provide: a end\n\
      \1"
     ,Script [Provide [ProvideName "a"]
             ,StmtExpr $ Num 1])

    ,("provide: a,b end\n\
      \1"
     ,Script [Provide [ProvideName "a", ProvideName "b"]
             ,StmtExpr $ Num 1])

    ,("provide: a as b end\n\
      \1"
     ,Script [Provide [ProvideAlias "a" "b"]
             ,StmtExpr $ Num 1])

    ,("include file(\"file.tea\")\n\
      \1"
     ,Script [Include (ImportSpecial "file" ["file.tea"])
             ,StmtExpr $ Num 1])

    ,("include string-dict\n\
      \1"
     ,Script [Include (ImportName "string-dict")
             ,StmtExpr $ Num 1])

    ,("import file(\"file.tea\") as X\n\
      \1"
     ,Script [Import (ImportSpecial "file" ["file.tea"]) "X"
             ,StmtExpr $ Num 1])

    ,("import string-dict as X\n\
      \1"
     ,Script [Import (ImportName "string-dict") "X"
             ,StmtExpr $ Num 1])


    ,("include from X: * end\n\
      \1"
     ,Script [IncludeFrom "X" [ProvideAll]
             ,StmtExpr $ Num 1])

    ,("include from X: a end\n\
      \1"
     ,Script [IncludeFrom "X" [ProvideName "a"]
             ,StmtExpr $ Num 1])

    ,("include from X: a,b end\n\
      \1"
     ,Script [IncludeFrom "X" [ProvideName "a", ProvideName "b"]
             ,StmtExpr $ Num 1])

    ,("include from X: a as b end\n\
      \1"
     ,Script [IncludeFrom "X" [ProvideAlias "a" "b"]
             ,StmtExpr $ Num 1])
    
    ]
  where
    nm x = NameBinding NoShadow x Nothing
    fh as = FunHeader [] (map nm as) Nothing

interpreterTests :: TestTree
interpreterTests =
    TestGroup "interpreterTests"
    $ map InterpreterTestsFile
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
     ]
