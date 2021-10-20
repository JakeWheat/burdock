

{-# LANGUAGE QuasiQuotes #-}

module Burdock.TestData where

import Burdock.Syntax

import qualified Text.RawString.QQ as R


-- todo: how to make this more readable?


exprParseTests :: [(String,Expr)]
exprParseTests =
    [("1", Num 1)
    ,("\"test\"", Text "test") 
    ,("test", Iden "test")
    ,("(2)", Parens (Num 2))
    ,("a(7)", App (Iden "a") [Num 7])
    ,("a + b", BinOp (Iden "a") "+" (Iden "b"))
    ,("a + b + c", BinOp (BinOp (Iden "a") "+" (Iden "b")) "+" (Iden "c"))

    ,("lam(): 2 end", Lam [] (Num 2))
    ,("lam(x): x + 1 end", Lam ["x"] (BinOp (Iden "x") "+" (Num 1)))
    ,("lam(x, y): x - y end"
     ,Lam ["x","y"] (BinOp (Iden "x") "-" (Iden "y")))

    ,("let x=3,y=4: x + y end"
     , Let [("x", Num 3)
           ,("y", Num 4)]
         (BinOp (Iden "x") "+" (Iden "y")))
    ,("let x=3: x + 4 end"
     ,Let [("x", Num 3)]
         (BinOp (Iden "x") "+" (Num 4)))
    
    ,("letrec a = 5: a end"
     ,LetRec [("a",Num 5)] (Iden "a"))

    ,("block: end", Block [])
    ,("block: \n\
      \  a\n\
      \end", Block [StmtExpr $ Iden "a"])
    ,("block: \n\
      \  a\n\
      \  1 + 2\n\
      \end", Block [StmtExpr $ Iden "a", StmtExpr $ BinOp (Num 1) "+" (Num 2)])

    ]

{-


-}

scriptParseTests :: [(String, Script)]
scriptParseTests =
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

  #letrec fact = lam(n):
  #  if n == 1: 1 else: n * fact(n - 1) end
  #end: fact(5) end is 120
   

     |], Script
         [LetDecl "a" (Num 5)
         ,StmtExpr $ App (Iden "print") [Iden "a"]
         ,Check (Just "test-1") [LetDecl "b" (Num 6)
                                ,StmtExpr $ BinOp (Num 1) "is" (Num 1)]
         ,Check Nothing [StmtExpr $ BinOp (Num 2) "is" (Num 3)]
                                
         ])

    ]


interpreterTests :: [String]
interpreterTests =
    [
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

  #letrec fact = lam(n):
  #  if n == 1: 1 else: n * fact(n - 1) end
  #end: fact(5) end is 120
   
end                    

     |]
    ,    [R.r|

f = lam(x): x + 1 end

check:
  f(3) is 4
end
|]
    ]
