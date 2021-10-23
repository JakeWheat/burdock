

{-# LANGUAGE QuasiQuotes #-}

module Burdock.TestData
    (testdata
    ,TestTree(..)
    ) where

import Burdock.Syntax

import qualified Text.RawString.QQ as R



data TestTree = TestGroup String [TestTree]
              | ExprParseTest String Expr
              | ScriptParseTest String Script
              | InterpreterTests String String

testdata :: TestTree
testdata = TestGroup "allTests"
    [exprParseTests
    ,scriptParseTests
    ,interpreterTests]


-- todo: how to make this more readable?

exprParseTests :: TestTree
exprParseTests = TestGroup "exprParseTests" $ map (uncurry ExprParseTest)
    [("1", Num 1)
    ,("\"test\"", Text "test") 
    ,("test", Iden "test")
    ,("(2)", Parens (Num 2))
    ,("a(7)", App (Iden "a") [Num 7])
    ,("a + b", BinOp (Iden "a") "+" (Iden "b"))
    ,("a + b + c", BinOp (BinOp (Iden "a") "+" (Iden "b")) "+" (Iden "c"))

    ,("lam(): 2 end", Lam [] (Num 2))
    ,("lam(x): x + 1 end", Lam [nm "x"] (BinOp (Iden "x") "+" (Num 1)))
    ,("lam(x, y): x - y end"
     ,Lam [nm "x",nm "y"] (BinOp (Iden "x") "-" (Iden "y")))

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
     ,Let [(PatName Shadow "a", Num 4)] (Iden "a"))
    ,("a.b", DotExpr (Iden "a") "b")
    ,("f(1,2).c", DotExpr (App (Iden "f") [Num 1, Num 2]) "c")
     ,("cases(List) a:\n\
      \  | empty => \"empty\"\n\
      \  | link(f, r) => \"link\"\n\
      \end"
     ,Cases "List" (Iden "a")
        [(IdenP $ nm "empty", Text "empty")
        ,(VariantP Nothing "link" [nm "f", nm "r"], Text "link")]
        Nothing)

    ,("cases(List) a:\n\
      \  | empty => \"empty\"\n\
      \  | else => \"else\"\n\
      \end"
     ,Cases "List" (Iden "a")
        [(IdenP $ nm "empty", Text "empty")]
        (Just $ Text "else"))

    ,("cases(z.List) a:\n\
      \  | z.empty => \"empty\"\n\
      \  | z.link(f, r) => x\n\
      \  | else => \"else\"\n\
      \end"
     ,Cases "z.List" (Iden "a")
        [(VariantP (Just "z") "empty" [], Text "empty")
        ,(VariantP (Just "z") "link" [nm "f", nm "r"], Iden "x")]
        (Just $ Text "else"))

    ]
  where
    nm = PatName NoShadow

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
         ,StmtExpr $ App (Iden "print") [Iden "a"]
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
      \end", Script [DataDecl "BTree" [VariantDecl "node" [(Con, "value"), (Con, "left"), (Con, "right")]
                                      ,VariantDecl "leaf" [(Con, "value")]] Nothing])

    ,("data MyEnum:\n\
      \  | node(left, right)\n\
      \  | leaf\n\
      \end", Script [DataDecl "MyEnum" [VariantDecl "node" [(Con, "left"), (Con, "right")]
                    ,VariantDecl "leaf" []] Nothing])

    ,("data MyEnum:\n\
      \  | node(left, right)\n\
      \  | leaf()\n\
      \end", Script [DataDecl "MyEnum" [VariantDecl "node" [(Con, "left"), (Con, "right")]
                    ,VariantDecl "leaf" []] Nothing])

    ,("data Point:\n\
      \  | pt(x, y)\n\
      \end", Script [DataDecl "Point" [VariantDecl "pt" [(Con, "x"), (Con, "y")]] Nothing])

    ,("data Point: pt(x, y) end"
     ,Script [DataDecl "Point" [VariantDecl "pt" [(Con, "x"), (Con, "y")]] Nothing])

    ,("data Point: pt() end"
     ,Script [DataDecl "Point" [VariantDecl "pt" []] Nothing])

    ,("data Point: pt end"
     ,Script [DataDecl "Point" [VariantDecl "pt" []] Nothing])

    ,("fun f(a): a + 1 end"
     ,Script[FunDecl (PatName NoShadow "f") [nm "a"] (BinOp (Iden "a") "+" (Num 1)) Nothing])

    ,("fun f(a):\n\
      \  a = 1\n\
      \  a + 1\n\
      \end", Script [FunDecl (PatName NoShadow "f") [nm "a"] (Block [LetDecl (nm "a") (Num 1)
                                             ,StmtExpr $ BinOp (Iden "a") "+" (Num 1)]) Nothing])
    ,("fun double(n):\n\
      \  n + n\n\
      \where:\n\
      \  double(10) is 20\n\
      \  double(15) is 30\n\
      \end"
     ,Script [FunDecl (PatName NoShadow "double") [nm "n"] (BinOp (Iden "n") "+" (Iden "n"))
      (Just [StmtExpr $ BinOp (App (Iden "double") [Num 10]) "is" (Num 20)
           ,StmtExpr $ BinOp (App (Iden "double") [Num 15]) "is" (Num 30)])])

    ,("rec fact = lam(x):\n\
      \    if x == 0: 1\n\
      \    else: x * fact(x - 1)\n\
      \    end\n\
      \  end"

     ,Script [RecDecl (nm "fact")
            $ Lam [nm "x"] $
                    If [(BinOp (Iden "x") "==" (Num 0), Num 1)]
                    (Just (BinOp (Iden "x") "*" (App (Iden "fact") [BinOp (Iden "x") "-" (Num 1)])))
            ])

    
    ]
  where
    nm = PatName NoShadow

interpreterTests :: TestTree
interpreterTests = TestGroup "interpreterTests" $ map (uncurry InterpreterTests)
    [("basics",
    [R.r|

check:
  1 is 1
  "test" is "test"
  1 + 2 is 3
  let a = 3: a end is 3
  let a = 1, b = a + 1: a + b end is 3

  let g = lam(a): a + 1 end: g(2) end is 3

  block:
    a = 1
    a + 2
  end is 3

  block:
    a = 1
    block:
      #shadow
      a = 2
      a
    end
    a
  end is 1

  za = 1
  zb = 2
  if za == 1: 1 else: 2 end is 1
  if za == zb: 1 else: 2 end is 2

   
end                    

f = lam(x): x + 1 end

check:
  f(3) is 4
end

check:
  var a = 3
  a is 3
  a := 4
  a is 4
end

check:
  true and false is false
  true or false is true
end

check "short circuiting":
  true or raise("fail") is true
  false and raise("fail") is false
end


     |])
   ,("letrec", [R.r|
check:
  letrec fact = lam(n):
    if n == 1: 1 else: n * fact(n - 1) end
  end: fact(5) end is 120

  letrec addeven = lam(x): if x == 0: 0 else: x + addodd(x - 1) end end,
         addodd = lam(x): if x == 0: 0 else: x + addeven(x - 1) end end:
    addeven(6) end is 21

  #letrec x = y + 1,
  #       y = 2: x end is 3

end

# check "letrec":
#   letrec fac = lam(x): if x == 0: 1 else: x * fac(x - 1) end end:
#     {fac(4);fac(5);fac(1)} end
#     is {24;120;1}
# 
#   letrec
#     addeven = lam(x): if x == 0: 0 else: x + addodd(x - 1) end end,
#     addodd = lam(x):  if x == 0: 0 else: x + addeven(x - 1) end end:
#     {addeven(2);addodd(2);addodd(5)}
#   end is {3;3;15}
# 
# end

fun fact(x):
  if x == 0: 1
  else: x * fact(x - 1)
  end
end

check "fun":

  fact(5) is 120
  fact(4) is 24
end

fun fact2(x):
  if x == 0: 1
  else: x * fact2(x - 1)
  end
where:
  fact2(5) is 120
  fact2(4) is 24
end

check "recdecl":

  block:
    fun addeven(x): if x == 0: 0 else: x + addodd(x - 1) end end
    fun addodd(x):  if x == 0: 0 else: x + addeven(x - 1) end end
    addeven(6) is 21
  end

  block:
    rec addeven = lam(x): if x == 0: 0 else: x + addodd(x - 1) end end
    rec addodd = lam(x): if x == 0: 0 else: x + addeven(x - 1) end end
    addeven(6) is 21
  end

  block:
    fun addeven(x): if x == 0: 0 else: x + addodd(x - 1) end end
    rec addodd = lam(x): if x == 0: 0 else: x + addeven(x - 1) end end
    addeven(6) is 21
  end

  block:
    rec addeven = lam(x) : if x == 0: 0 else: x + addodd(x - 1) end end
    fun addodd(x): if x == 0: 0 else: x + addeven(x - 1) end end
    addeven(6) is 21
  end

end

|])
   ,("fun", [R.r|
fun fact(x):
  if x == 0: 1
  else: x * fact(x - 1)
  end
end

check "fun":

  fact(5) is 120
  fact(4) is 24
end

|])
   
   ,("agdt", [R.r|

data Point:
  | pt(x, y)
end
   
p1 = pt(1,2)

check:
  is-pt(p1) is true
  is-Point(p1) is true
  is-Point(1) is false
  # is-Point({1;3}) is false
  p1.x is 1
  p1.y is 2
end

data Nowt:
  | nowt
end

n1 = nowt
check:
  is-Nowt(n1) is true
  is-nowt(n1) is true
  n1 is nowt
end

data Two:
  | pt1(x, y)
  | pt2(x, y)
end

t1 = pt1(1,2)
t2 = pt2(3,4)

fun pf(a,n):
  cases (Two) a:
    | pt1(x,y) => if n == 0: x else: y end
    | pt2(x,y) => if n == 0: x else: y end
  end
end

check:
  is-pt1(t1) is true
  is-pt2(t1) is false
  is-pt1(t2) is false
  is-pt2(t2) is true
  is-Two(t1) is true
  is-Two(t2) is true
  is-Two(p1) is false
  pf(t1,0) is 1
  pf(t1,1) is 2
  pf(t2,0) is 3
  pf(t2,1) is 4

end

data MyOption:
  | my-none
  | my-some(a)
end

check:
  fun f(a): cases(MyOption) a:
      | my-none => "none"
      | my-some(b) => "some" + tostring(b)
    end
  end
  x = my-none
  f(x) is "none"
  # needs the built in (global) function tostring
  # y = my-some(1)
  # f(y) is "some1"
end
 
data BTree:
  | node(value #|:: Number|#, left #|:: BTree|#, right #|:: BTree|#)
  | leaf(value #|:: Number|#)
where:
  a-btree = node(1, leaf(2), node(3, leaf(4), leaf(5)))

  is-BTree(a-btree) is true
  is-BTree("not-a-tree") is false
  is-BTree(leaf(5)) is true
  is-leaf(leaf(5)) is true
  is-leaf(a-btree) is false
  is-leaf("not-a-tree") is false
  is-node(leaf(5)) is false
  is-node(a-btree) is true
  is-node("not-a-tree") is false

  a-btree.value is 1
  a-btree.left.value is 2
  a-btree.right.value is 3
  a-btree.right.left.value is 4
  a-btree.right.right.value is 5

end



|])

    ]
