

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

    -- tuple selector
    ,("{\"a\";\"b\";true}", TupleSel [Text "a", Text "b", Iden "true"])
    ,("{1}", TupleSel [Num 1])
    -- tuple field get
    ,("myobj4.{0}", TupleGet (Iden "myobj4") 0)
    ,("f().{1}", TupleGet (App (Iden "f") []) 1)
    -- record selector
    ,("{a: \"one\", b : 2, c : x }", RecordSel [("a", Text "one")
                                               ,("b", Num 2)
                                               ,("c", Iden "x")])
    ,("{}", RecordSel [])

    ,("[list:]", Construct (Iden "list") [])
    ,("[list: 1,2,3]", Construct (Iden "list") [Num 1, Num 2, Num 3])
     
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

check "letrec":
  letrec fac = lam(x): if x == 0: 1 else: x * fac(x - 1) end end:
    {fac(4);fac(5);fac(1)} end
    is {24;120;1}

  letrec
    addeven = lam(x): if x == 0: 0 else: x + addodd(x - 1) end end,
    addodd = lam(x):  if x == 0: 0 else: x + addeven(x - 1) end end:
    {addeven(2);addodd(2);addodd(5)}
  end is {3;3;15}

end

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
  is-Point({1;3}) is false
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

    ,("tuples", [R.r|
check:
  t = {"a";"b";true}
  t.{0} is "a"
  t.{1} is "b"
  t.{2} is true

  is-tuple(t) is true

  myobj4 = {1}
  myobj4.{0} is 1

end



|])
    ,("records", [R.r|
check:
  x = 33
  a = {a: "one", b : 2, c : x }
  a.a is "one"
  a.b is 2
  a.c is 33

  is-record(a) is true

  myobj3 = {}
  is-record(myobj3) is true


  my-obj = {s: "Hello", b: true, n: 42}
  fun g(thing #|:: {n :: Number}|#)#| -> Number|#:
    thing.n
  end

  g(my-obj) is 42

  # this succeeds with type checking on in pyret:
  #my-obj2 :: {n :: Number, s :: String, b :: Boolean} = {s: "Hello", b: true, n: 42}
  #fun g2(thing :: {n :: Number}) -> Number:
  #  thing.n
  #end
  #g2(my-obj2) is 42

  # check non ordering of fields in equality

  {a:1, b:2} is { b:2, a:1}
end
|])

    ,("list-basics", [R.r|

#|

data List:
   | empty
   | link(first,rest)
end

|#

check:
  empty is [list:]
  link(1, empty) is [list: 1]
  link(1, link(2, empty)) is [list: 1,2]

end

check:
  l1 = [list: 1]
  mt = empty
  is-empty(mt) is true
  is-empty(l1) is false
  is-link(mt) is false
  is-link(l1) is true

  is-List(l1) is true
  is-List(mt) is true
end

check:
  l1 = [list: 1,2,3]
  l1.first is 1
  l1.rest is [list: 2,3]
end

check:
  list1 = [list: 2,3]
  list2 = link(1, list1)
  list2 is [list: 1,2,3]

  [list: 1,2,3].first is 1
  [list: 1,2,3].rest  is [list: 2,3]
end

check:
  empty is [list:]
  link(1, empty) is [list: 1]
  link(1, link(2, empty)) is [list: 1,2]
  link(empty, link(empty, [list: ])) is [list: [list:],[list:]]
end

#|
check:
  x = [list: 1,2,3]
  # -> link(1, link(2, link (3,empty)))
  # test variant binding
  
  link(a,b) = x
  a is 1
  link(_,link(c,d)) = x
  c is 2
  d is [list: 3]
  w = let link(y,z) = x: {y;z} end
  w is {1;[list:2,3]}
  a0 = [list:]
  # this seems pretty dodgy: pattern match on a no arg variant
  # instead of binding to an identifier called empty
  empty = a0
  # todo: do a raises test with this
  nothing
end
   |#

check:
  x = [list:1]
  cases(list) x:
    | empty => true
    | else => false
  end is false

  #shadow
  x = [list:]
  cases(list) x:
    | empty => true
    | else => false
  end is true

  #shadow
  x = [list:1]
  cases(list) x:
    | link(a,b) => block:
        a is 1
        b is empty
        nothing
      end
    | else => nothing
  end
  
end


fun my-is-empty(l):
  cases(List) l:
    | empty => true
    | else => false
  end
end

fun length(l):
  cases(List) l:
    | empty => 0
    | link(f, r) => 1 + length(r)
  end
end

check:
  l1 = empty
  l2 = [list: 1,2,3]
  my-is-empty(l1) is true
  my-is-empty(l2) is false

  length(l1) is 0
  length(l2) is 3
end
|])
    ,("modules-import-simple", [R.r|

import file("test-src/trivial-one.bur") as X
check:
  X.a is 5
end
|])
     ,("modules-import-simple-2", [R.r|

import file("test-src/trivial-two.bur") as X
check:
  X.a is 5
end

|])
     ,("modules-import-simple-3", [R.r|

import file("test-src/trivial-three.bur") as X
check:
  X.c is 5
end

|])
     ,("include-from-simple", [R.r|

import file("test-src/trivial-one.bur") as X
include from X:
  *
end
check:
  X.a is 5
  a is 5
end
|])
     ,("include-from-simple-2", [R.r|

import file("test-src/trivial-one.bur") as X
include from X:
  a
end
check:
  X.a is 5
  a is 5
end
|])
     ,("include-from-simple-3", [R.r|

import file("test-src/trivial-one.bur") as X
include from X:
  a as c
end
check:
  X.a is 5
  c is 5
end
|])
     ,("include-simple", [R.r|

include file("test-src/trivial-one.bur")
check:
  a is 5
end

|])

    ]
