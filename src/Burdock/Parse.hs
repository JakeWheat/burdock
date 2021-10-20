
module Burdock.Parse
    (parseExpr
    ,parseScript
    ) where


import Text.Megaparsec (Parsec
                       -- ,many
                       ,(<|>)
                       ,parse
                       ,eof
                       -- ,some
                       ,choice
                       ,option
                       ,(<?>)
                       ,many
                       -- --,manyTill
                       ,takeWhileP
                       ,takeWhile1P
                       ,try
                       ,optional
                       -- --,eitherP
                       ,notFollowedBy
                       ,errorBundlePretty
                       ,satisfy
                       ,anySingle)

import Text.Megaparsec.Char (space
                            ,char
                            ,string
                            ,letterChar
                            )

import Data.Char (isAlphaNum,isDigit)

import Control.Applicative ((<**>)
                           ,Alternative
                           )
import Control.Monad (when
                     ,void
                     ,guard
                     )

import Text.Read (readMaybe)


import Data.Void (Void)

import Burdock.Syntax

------------------------------------------------------------------------------

-- api functions

parseExpr :: FilePath -> String -> Either String Expr
parseExpr fn src = parseHelper expr fn src

parseScript :: FilePath -> String -> Either String Script
parseScript fn src = parseHelper script fn src

---------------------------------------

parseHelper :: Parser a -> FilePath -> String -> Either String a
parseHelper parseFn fn src =
    either (Left . errorBundlePretty) Right $
    parse (whiteSpace *> parseFn <* myEof) fn src

------------------------------------------------------------------------------

-- parser helpers and lexer like things

type Parser = Parsec Void String

chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan
  where
    scan = p <**> rst
    rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

boption :: a -> Parser a -> Parser a
boption v p = (option v p) <?> ""

bchoice :: [Parser a] -> Parser a
bchoice cs = choice $ addEmpty cs
  where
    addEmpty (x:xs@(_:_)) = (x <?> "") : addEmpty xs
    addEmpty [x] = [x]
    addEmpty [] = []


myEof :: Parser ()
myEof = eof <?> ""


lexeme :: Parser a -> Parser a
lexeme f = f <* whiteSpace

lexeme_ :: Parser a -> Parser ()
lexeme_ f = () <$ f <* whiteSpace


parens :: Parser a -> Parser a
parens f = lexeme_ (char_ '(')
           *> f
           <* lexeme_ (char_ ')')

xSep :: Char -> Parser f -> Parser [f]
xSep x f = option [] (xSep1 x f)

xSep1 :: Char -> Parser f -> Parser [f]
xSep1 x f = (:) <$> f <*> option [] (lexeme_ (char_ x) *> xSep1 x f)

commaSep :: Parser f -> Parser [f]
commaSep = xSep ','

commaSep1 :: Parser f -> Parser [f]
commaSep1 = xSep1 ','

whiteSpace :: Parser ()
whiteSpace = space *> choice [blockComment *> whiteSpace
                             ,lineComment *> whiteSpace
                             ,pure ()] <?> ""

char_ :: Char -> Parser ()
char_ x = () <$ char x

symbol :: String -> Parser String
symbol x = lexeme (string x)

symbol_ :: String -> Parser ()
symbol_ x = lexeme_ (string x)

keyword :: String -> Parser String
keyword n = lexeme (try (string n <* notFollowedBy (satisfy (\a -> isAlphaNum a || a `elem` "?-+_"))))

keyword_ :: String -> Parser ()
keyword_ n = void $ keyword n

reservedKeywords :: [String]
reservedKeywords =
    ["end", "lam", "let", "letrec", "if", "else", "ask", "then"
    ,"otherwise", "block", "cases", "when", "var", "check"
    ,"where", "fun", "rec", "data"
    ,"import", "provide", "provide-types"
    ,"from", "and", "or", "shadow", "as"
    ,"ref"
    ]

identifierX :: Parser String
identifierX =
    lexeme ((:)
    <$> (letterChar <|> char '_' <|> char '-')
    <*> takeWhileP Nothing (\a -> (isAlphaNum a || a `elem` "?-+_")))
    <?> "identifier"

identifier :: Parser String
identifier = try $ do
    i <- identifierX
    when (i `elem` reservedKeywords)
        $ fail $ "unexpected keyword: " ++ i
    guard (i `notElem` reservedKeywords)
    pure i

lineComment :: Parser ()
lineComment = () <$ try (string "#" <?> "") <* takeWhileP Nothing (/='\n')

blockComment :: Parser ()
blockComment = startComment *> ctu
  where
    startComment = void (try (string "#|") <?> "")
    endComment = void $ try (string "|#")
    ctu = endComment <|> ((blockComment <|> void anySingle) *> ctu)

num :: Parser String
num = lexeme (
    choice [digits <**> bchoice [eSuffix,dotSuffixOnly,pure id]
           ,myChar '.' <**> afterDot
           ]
   -- this is for definitely avoiding possibly ambiguous source
   -- not sure if it is needed
    <* notFollowedBy (satisfy (`elem` "eE."))) <?> "number"
  where
    -- parse one or more 0-9
    digits = takeWhile1P Nothing isDigit
    -- parse .[digits][e[+-]digits]
    dotSuffixOnly = appendA <$> (myChar '.' <**> bchoice [afterDot, eSuffix, pure id])
    -- parse digits[e[+-]digits], used after the .
    afterDot = appendA <$> (digits <**> bchoice [eSuffix, pure id])
    -- parse e[+-]digits
    eSuffix = appendA <$> concatA [myChar 'e', optionalPlusOrMinus,digits]
    optionalPlusOrMinus = boption "" (myChar '+' <|> myChar '-')
    -- parse a char, return it as a string
    myChar c = [c] <$ char_ c
    -- concat in applicative
    concatA xs = concat <$> sequenceA xs
    -- not sure if this def pays its way
    appendA = flip (++)

------------------------------------------------------------------------------

-- main parsing


---------------------------------------

-- expressions

expr :: Parser Expr
expr = chainl1 term f
  where
      f = do
          op <- binOpSym
          pure $ \a b -> BinOp a op b

term :: Parser Expr
term = (do
    x <- choice
        [lamE
        ,expressionLetRec
        ,expressionLet
        ,ifE
        ,block
        ,Iden <$> identifier
        ,numE
        ,stringE
        ,parensE
        ]
    bchoice [termSuffixes x, pure x]) <?> "expression"

termSuffixes :: Expr -> Parser Expr
termSuffixes x = boption x $ do
    y <- choice [pure x <**> appSuffix]
    termSuffixes y

appSuffix :: Parser (Expr -> Expr)
appSuffix = flip App <$> parens (commaSep expr)

binOpSym :: Parser String
binOpSym = choice ([symbol "+"
                  ,symbol "*"
                  ,try $ symbol "<="
                  ,try $ symbol "=="
                  ,try $ symbol ">="
                  ,try $ symbol "<>"
                  ,symbol "<"
                  ,symbol ">"
                  ,symbol "-"
                  ,symbol "/"
                  ,symbol "^"
                  ,symbol "|>"
                  ] ++ map keyword
                  ["and"
                  ,"or"
                  ,"is"
                  ])


lamE :: Parser Expr
lamE = Lam <$> (keyword_ "lam" *> parens (commaSep patName) <* symbol_ ":")
           <*> (expr <* keyword_ "end")

patName :: Parser String
patName = identifier

expressionLetRec :: Parser Expr
expressionLetRec = keyword_ "letrec" *> letBody LetRec

expressionLet :: Parser Expr
expressionLet = keyword_ "let" *> letBody Let

letBody :: ([(PatName,Expr)] -> Expr -> Expr) -> Parser Expr
letBody ctor = ctor <$> commaSep1 binding
                    <*> (symbol_ ":" *> expr <* keyword_ "end")

binding :: Parser (PatName,Expr)
binding = (,) <$> patName
                       <*> (symbol_ "=" *> expr)

ifE :: Parser Expr
ifE = do
    keyword_ "if"
    ife <- cond
    nextBranch [ife]
  where
    cond = (,) <$> expr <*> (symbol_ ":" *> expr)
    nextBranch bs =
        choice [do
                x <- elsePart
                case x of
                    Right el -> endif bs (Just el)
                    Left b -> nextBranch (b:bs)
               ,endif bs Nothing]
    elsePart :: Parser (Either (Expr,Expr) Expr)
    elsePart = do
        keyword_ "else"
        choice
            [Right <$> (symbol_ ":" *> expr)
            ,Left <$> (keyword_ "if" *> cond)
            ]
    endif bs el = keyword_ "end" *> pure (If (reverse bs) el)

block :: Parser Expr
block = Block <$>
    (keyword_ "block" *> symbol_ ":" *>
    many stmt
    <* keyword_ "end")

numE :: Parser Expr
numE = do
    x <- num
    maybe (fail $ "parsing number failed: " ++ x)
          (pure . Num) (readMaybe x)

stringE :: Parser Expr
stringE = Text <$> stringRaw
            <?> "string literal"

stringRaw :: Parser String
stringRaw = unescape <$>
            choice [char_ '\'' *> takeWhileP Nothing (/='\'') <* lexeme_ (char_ '\'')
                   ,char_ '"' *> takeWhileP Nothing (/='"') <* lexeme_ (char_ '"')]
            <?> "string literal"
  where
    unescape ('\\':'n':xs) = '\n':unescape xs
    unescape ('\\':'\\':xs) = '\\':unescape xs
    unescape (x:xs) = x:unescape xs
    unescape [] = []

parensE :: Parser Expr
parensE = Parens <$> parens expr

---------------------------------------

-- statements

script :: Parser Script
script = Script <$> stmts

stmt :: Parser Stmt
stmt = choice
    [varDecl
    ,checkBlock
    ,startsWithExprOrPattern]

stmts :: Parser [Stmt]
stmts = many stmt

checkBlock :: Parser Stmt
checkBlock = do
    keyword_ "check"
    nm <- optional stringRaw
    symbol_ ":"
    ss <- many stmt
    keyword_ "end"
    pure $ Check nm ss

varDecl :: Parser Stmt
varDecl = uncurry VarDecl <$> (keyword_ "var" *> binding)


startsWithExprOrPattern :: Parser Stmt
startsWithExprOrPattern = do
    ex <- expr
    case ex of
        Iden i -> choice
            [SetVar i <$> ((symbol_ ":=" <?> "") *> expr)
            ,LetDecl i <$> ((symbol_ "=" <?> "") *> expr)
            ,pure $ StmtExpr ex]
        _ -> pure $ StmtExpr ex

