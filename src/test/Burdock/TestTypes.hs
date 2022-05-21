
module Burdock.TestTypes where

import Burdock.Syntax (Expr,Stmt,Script)

data TestTree = TestGroup String [TestTree]
              | ExprParseTest String Expr
              | StmtParseTest String Stmt
              | ScriptParseTest String Script
              | LiterateParseTest String String
              | InterpreterTestsFile FilePath
              | InterpreterTestsDir FilePath
              | InterpreterTestsOptionalDir FilePath

