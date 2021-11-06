
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Burdock.Parse
    (parseExpr
    ,parseStmt
    ,parseScript
    ) where


import Text.Megaparsec (Parsec
                       -- ,many
                       ,(<|>)
                       ,parse
                       ,eof
                       ,some
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
                       ,anySingle
                       ,getSourcePos
                       ,SourcePos(..)
                       ,unPos
                       )

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

parseStmt :: FilePath -> String -> Either String Stmt
parseStmt fn src = parseHelper stmt fn src

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

boptional :: Parser a -> Parser (Maybe a)
boptional p = optional p <?> ""

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

nonNegativeInteger :: Parser Int
nonNegativeInteger = lexeme (read <$> takeWhile1P Nothing isDigit)

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
        ,cases
        ,typeSel
        ,Iden <$> identifier
        ,numE
        ,stringE
        ,parensE
        ,construct
        ,tupleOrRecord
        ]
    bchoice [termSuffixes x, pure x]) <?> "expression"

termSuffixes :: Expr -> Parser Expr
termSuffixes x = boption x $ do
    y <- choice [pure x <**> appSuffix
                ,pure x <**> dotSuffix 
                ]
    termSuffixes y

appSuffix :: Parser (Expr -> Expr)
appSuffix = f <$> sourcePos <*> parens (commaSep expr)
  where
    f sp as x = App sp x as

sourcePos :: Parser SourcePosition
sourcePos = do
    x <- getSourcePos
    pure $ Just (sourceName x, unPos $ sourceLine x, unPos $ sourceColumn x)

dotSuffix :: Parser (Expr -> Expr)
dotSuffix = symbol_ "." *>
    bchoice [flip TupleGet <$> (symbol_ "{" *> nonNegativeInteger <* symbol_ "}")
            ,flip DotExpr <$> identifier]


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

patName :: Parser Binding
patName = NameBinding <$> boption NoShadow (Shadow <$ keyword_ "shadow")
                      <*> identifier
                      <*> pure Nothing

expressionLetRec :: Parser Expr
expressionLetRec = keyword_ "letrec" *> letBody LetRec

expressionLet :: Parser Expr
expressionLet = keyword_ "let" *> letBody Let
 
letBody :: ([(Binding,Expr)] -> Expr -> Expr) -> Parser Expr
letBody ctor = ctor <$> commaSep1 binding
                    <*> (symbol_ ":" *> expr <* keyword_ "end")

binding :: Parser (Binding,Expr)
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

cases :: Parser Expr
cases = do
    ty <- keyword_ "cases" *> parens typeName
    t <- (expr <* symbol_ ":")
    nextCase ty t []
  where
    typeName = (do
        i <- identifier
        -- todo: don't allow whitespace?
        choice [do
                j <- char '.' *> identifier
                pure $ i ++ "." ++ j
               ,pure i]) <?> "type name"
    nextCase ty t cs =
        choice [do
                x <- casePart
                case x of
                    Right el -> endCase ty t cs (Just el)
                    Left c -> nextCase ty t (c:cs)
               ,endCase ty t cs Nothing]
    casePart :: Parser (Either (Pat,Expr) Expr)
    casePart = do
        symbol_ "|"
        choice
            [Right <$> (keyword_ "else" *> symbol_ "=>" *> expr)
            ,Left <$> ((,) <$> (casePat <?> "pattern") <*> (symbol_ "=>" *> expr))]
    endCase ty t cs el = keyword_ "end" *> pure (Cases ty t (reverse cs) el)

{-
a case pattern can be:
an (optionally dotted) identifier
a (optionally dotted) shadow identifier
a arged variant which is an (optionally dotted) identifier then parens commasep case pattern
a case pattern with an as suffix

a dotted identifier will parse as a variant with no args, it's a bit hacky
-}

casePat :: Parser Pat
casePat = patTerm
  where
    patTerm = choice
        [do
         keyword_ "shadow"
         i <- identifier
         pure (IdenP $ NameBinding Shadow i Nothing)
        ,do
         i <- identifier
         choice [do
               j <- char '.' *> identifier
               choice [do
                       as <- parens (commaSep patName)
                       pure $ VariantP (Just i) j as
                      ,pure $ VariantP (Just i) j []]
              ,choice [do
                       as <- parens (commaSep patName)
                       pure $ VariantP Nothing i as
                      ,pure $ IdenP (NameBinding NoShadow i Nothing)]]]

typeSel :: Parser Expr
typeSel = do
    keyword_ "type-val"
    choice [TypeSel <$> parens typ
           ,pure $ Iden "type-val"]


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

tupleOrRecord :: Parser Expr
tupleOrRecord = tupleOrRecord2 RecordSel
                               TupleSel
                               expr
                               (\case
                                     Iden i -> Just i
                                     _ -> Nothing)

tupleOrRecord2 :: ([(String, Expr)] -> a)
               -> ([a] -> a)
               -> Parser a
               -> (a -> Maybe String)
               -> Parser a
tupleOrRecord2 mkRecSel mkTupSel pTupEl extractIden = do
    symbol_ "{"
    choice [-- {} is an empty record, not an empty tuple
            symbol_ "}" *> pure (mkRecSel [])
           ,eitherElement]
  where
    eitherElement = do
        x <- pTupEl
        if | Just i <- extractIden x -> choice
                [do
                 symbol_ ":"
                 e <- expr
                 moreRecord [(i,e)]
                ,moreTuple [x]]
           | otherwise -> moreTuple [x]
    moreTuple ts = choice
        [symbol_ "}" *> pure (mkTupSel (reverse ts))
        ,symbol ";" *> choice
             [symbol_ "}" *> pure (mkTupSel (reverse ts))
             ,do
              te <- pTupEl
              moreTuple (te:ts)]]
    moreRecord fs = choice
        [symbol_ "}" *> pure (mkRecSel (reverse fs))
        ,symbol "," *> choice
             [symbol_ "}" *> pure (mkRecSel (reverse fs))
             ,do
              f <- fld
              moreRecord (f:fs)]]
    fld = (,) <$> (identifier <* symbol_ ":") <*> expr

construct :: Parser Expr
construct = Construct <$> (symbol_ "[" *> (Iden <$> identifier) <* symbol_ ":")
            <*> (commaSep expr <* symbol_ "]")


---------------------------------------

-- statements

script :: Parser Script
script = Script <$> stmts

stmt :: Parser Stmt
stmt = choice
    [recDecl
    ,funDecl
    ,varDecl
    ,dataDecl
    ,checkBlock
    ,provide
    ,include
    ,importStmt
    ,startsWithExprOrPattern]

stmts :: Parser [Stmt]
stmts = many stmt

recDecl :: Parser Stmt
recDecl = uncurry RecDecl <$> (keyword_ "rec" *> binding)

funDecl :: Parser Stmt
funDecl = FunDecl
    <$> (keyword "fun" *> patName)
    <*> parens (commaSep patName)
    <*> (symbol_ ":" *> (unwrapSingle <$>
         (Block <$> some stmt)))
    <*> (boptional whereBlock <* keyword_ "end")
    
  where
      unwrapSingle (Block [StmtExpr (a)]) = a
      unwrapSingle x = x

whereBlock :: Parser [Stmt]
whereBlock = keyword_ "where" *> symbol_ ":" *> many stmt

varDecl :: Parser Stmt
varDecl = uncurry VarDecl <$> (keyword_ "var" *> binding)

dataDecl :: Parser Stmt
dataDecl = DataDecl
    <$> (keyword_ "data" *> identifier <* symbol_ ":")
    <*> (((:[]) <$> singleVariant) <|> some variant)
    <*> (boptional whereBlock <* keyword_ "end")
  where
    singleVariant = VariantDecl
                    <$> identifier <*> boption [] (parens (commaSep fld))
    variant = VariantDecl
              <$> (symbol_ "|" *> identifier)
              <*> boption [] (parens (commaSep fld))
    fld = (,) <$> boption Con (Ref <$ keyword_ "ref") <*> identifier

checkBlock :: Parser Stmt
checkBlock = do
    keyword_ "check"
    nm <- optional stringRaw
    symbol_ ":"
    ss <- many stmt
    keyword_ "end"
    pure $ Check nm ss

provide :: Parser Stmt
provide = Provide <$> (keyword_ "provide"
                       *> symbol_ ":"
                       *> commaSep provideItem
                       <* keyword_ "end")

provideItem :: Parser ProvideItem
provideItem = choice
    [ProvideAll <$ symbol_ "*"
    ,do
     a <- identifier
     bchoice [ProvideAlias a <$> (keyword_ "as" *> identifier)
            ,pure $ ProvideName a]
    ]

include :: Parser Stmt
include = do
    keyword_ "include"
    choice [IncludeFrom
            <$> (keyword_ "from" *> identifier <* symbol_ ":")
            <*> (commaSep provideItem <* keyword_ "end")
           ,Include <$> importSource]

importSource :: Parser ImportSource
importSource = do
    a <- identifier
    bchoice [ImportSpecial a <$> parens (commaSep stringRaw)
            ,pure $ ImportName a]

importStmt :: Parser Stmt
importStmt = keyword_ "import" *> (Import <$> importSource
                      <*> (keyword_ "as" *> identifier))


startsWithExprOrPattern :: Parser Stmt
startsWithExprOrPattern = do
    ex <- expr
    case ex of
        Iden i -> choice
            [SetVar i <$> ((symbol_ ":=" <?> "") *> expr)
            ,LetDecl (NameBinding NoShadow i Nothing) <$> ((symbol_ "=" <?> "") *> expr)
            ,pure $ StmtExpr ex]
        _ -> pure $ StmtExpr ex

typ :: Parser Type
typ = startsWithIden <|> parensOrNamedArrow <|> ttupleOrRecord
  where
    startsWithIden = do
        i <- identifier
        ctu i
    ctu i = do
        i1 <- tname i
        choice
              [TParam i1 <$> (symbol_ "<" *> commaSep1 noarrow <* symbol_ ">")
              ,(\is r -> TArrow (TName i1:is) r)
              <$> (many (symbol_ "," *> noarrow))
              <*> (symbol_ "->" *> noarrow)
              ,pure $ TName i1]
    noarrow = parensOrNamedArrow <|> do
        i <- identifier
        noarrowctu i
    tname i = do
        sfs <- many (symbol_ "." *> identifier)
        pure (i:sfs)
    noarrowctu i = do
        i1 <- tname i
        choice
              [TParam i1 <$> (symbol_ "<" *> commaSep1 typ <* symbol_ ">")
              ,pure $ TName i1]
    parensOrNamedArrow = symbol_ "(" *> do
        i <- identifier
        choice [do
                x <- symbol_ "::" *> noarrow
                xs <- option [] $ symbol_ "," *> (commaSep1 ((,) <$> identifier <*> (symbol_ "::" *> noarrow)))
                r <- symbol_ ")" *> symbol_ "->" *> noarrow
                pure $ TNamedArrow ((i,x):xs) r
               ,do
                i1 <- ctu i <* symbol_ ")"
                pure $ TParens i1]
               

    ttupleOrRecord = symbol_ "{" *> f <* symbol_ "}"
      where
        f = do
            i <- identifier
            choice
                [do
                 t <- symbol_ "::" *> noarrow
                 ts <- option [] $ symbol_ "," *> commaSep1 ((,) <$> identifier <*> (symbol_ "::" *> noarrow))
                 pure $ TRecord ((i,t):ts)
                ,do
                 i1 <- noarrowctu i
                 ts <- option [] $ symbol_ ";" *> xSep1 ';' noarrow
                 pure $ TTuple (i1:ts)]
