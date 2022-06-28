
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Burdock.Parse
    (parseExpr
    ,parseStmt
    ,parseScript
    ,parseLiterateScript
    ) where


import Text.Megaparsec (ParsecT
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

import Text.Megaparsec.Char (char
                            ,string
                            ,letterChar
                            ,spaceChar
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
import Data.Maybe (catMaybes)

import Data.Void (Void)

import Burdock.Syntax
import Data.List (isPrefixOf)

import Control.Monad.State.Strict (StateT, evalStateT, put, get)
import Control.Monad.Identity (Identity)

------------------------------------------------------------------------------

-- api functions

parseExpr :: FilePath -> String -> Either String Expr
parseExpr fn src = parseHelper expr fn src

parseStmt :: FilePath -> String -> Either String Stmt
parseStmt fn src = parseHelper stmt fn src

parseScript :: FilePath -> String -> Either String Script
parseScript fn src = parseHelper script fn src

parseLiterateScript :: FilePath -> String -> Either String Script
parseLiterateScript fn src = parseHelper script fn $ extractSource src

---------------------------------------

parseHelper :: Parser a -> FilePath -> String -> Either String a
parseHelper parseFn fn src =
    either (Left . errorBundlePretty) Right $
    parse (evalStateT ((whiteSpace *> (put SeenNewline *> parseFn) <* myEof)) SeenNewline) fn src

data ParserState
    = SeenNewline
    | SeenWhitespace
    | NotSeenWhitespace
    deriving (Eq,Show)

------------------------------------------------------------------------------

-- parser helpers and lexer like things

type Parser = StateT ParserState (ParsecT Void String Identity)

chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan
  where
    scan = p <**> rst
    rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

chainr1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan where
  scan = p <**> rst
  rst = (flip <$> op <*> scan) <|> pure id

boption :: a -> Parser a -> Parser a
boption v p = (option v p) <?> ""

boptional :: Parser a -> Parser (Maybe a)
boptional p = optional p <?> ""

optional_ :: Parser a -> Parser ()
optional_ a = optional a *> pure ()

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

{-
track whitespace so can prevent two expressions on the same line,
and require no whitespace between an expression and app parens:
f(x) OK
f (x) ERROR

todo:

extend to <>, see if there's anything else in this category

require whitespace around binary operators:
1+2 ERROR
1+ 2 ERROR
1 +2 ERROR
1 + 2 OK

make the parse error messages better
-}
whiteSpace :: Parser ()
whiteSpace = do
    x <- whiteSpace'
    put $ if | '\n' `elem` x -> SeenNewline
             | not (null x) -> SeenWhitespace
             | otherwise -> NotSeenWhitespace
  where
    whiteSpace' = do
        sp <- space'
        sp1 <- choice [blockComment *> whiteSpace'
                      ,do
                       lineComment
                       x <- whiteSpace'
                       pure ('\n':x)
                      ,pure ""] <?> ""
        pure $ sp ++ sp1
    space' = many spaceChar

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
    ,"ref", "table", "row"
    ,"receive", "after", "method"
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

{-
numpty precedence at the moment:
testpreds left associativity
rightops, just |> atm, right associativity
all other ops, left associativity

todo: pass over the syntax after parsing to prohibit op combinations
that pyret does -> you can only chain the same operator
not sure that non associative (algebra not fixity) operators should
 even allow this 1 + 2 + 3 seems ok, but 1 - 2 - 3 seems weird if
all other fixity is disallowed

-}

expr :: Parser Expr
expr = chainl1 exprlev1 (f testPred)
  where
    exprlev1 = chainr1 exprlev2 (f rightBinOpSym)
    exprlev2 = chainl1 term (f leftBinOpSym)
    f o = do
        op <- o <?> ""
        pure $ \a b -> BinOp a op b

term :: Parser Expr
term = (do
    x <- choice
        [unaryMinus
        ,lamE
        -- todo: factor to get rid of the try
        -- it's unambiguously a lam if it starts with a { then a (
        ,try curlyLam
        ,expressionLetRec
        ,expressionLet
        ,ifE
        ,ask
        ,block
        ,cases
        ,typeLet
        ,forExpr
        ,tableSel
        ,template
        ,assertTypeCompat
        ,receive
        ,methodExpr
        -- make sure all syntax that starts with something that looks like
        -- an identifier appears above here
        ,Iden <$> identifier
        ,numE
        ,stringE
        ,parensE
        ,construct
        ,tupleOrRecord
        ]
    -- todo: put this suffix stuff in the expr thing above
    bchoice [termSuffixes x, pure x]) <?> "expression"

termSuffixes :: Expr -> Parser Expr
termSuffixes x = boption x $ do
    y <- choice [pure x <**> appSuffix
                ,pure x <**> instSuffix
                ,pure x <**> dotSuffix
                ,pure x <**> unboxSuffix
                ]
    termSuffixes y

appSuffix :: Parser (Expr -> Expr)
appSuffix = do
    st <- get
    guard (st == NotSeenWhitespace)
    sp <- sourcePos
    f sp <$> parens (commaSep expr)
  where
    f sp as x = App sp x as

instSuffix :: Parser (Expr -> Expr)
instSuffix = f <$> tyParamList
  where
    f t e = InstExpr e t

-- todo: remove the try when implement the whitespace rules
tyParamList :: Parser [Ann]
tyParamList = try (symbol_ "<" *> commaSep1 (typ False) <* symbol_ ">")

sourcePos :: Parser SourcePosition
sourcePos = do
    x <- getSourcePos
    pure $ Just (sourceName x, unPos $ sourceLine x, unPos $ sourceColumn x)

dotSuffix :: Parser (Expr -> Expr)
dotSuffix = symbol_ "." *>
    bchoice [symbol_ "{" *>
             choice [flip TupleGet <$> (nonNegativeInteger <* symbol_ "}")
                    ,flip Extend <$> (commaSep1 fld <* symbol_ "}")]
            ,flip DotExpr <$> identifier]
  where
    fld = (,) <$> identifier <*> (symbol_ ":" *> expr)

unboxSuffix :: Parser (Expr -> Expr)
unboxSuffix = flip UnboxRef <$> (try (symbol_ "!" *> identifier))

leftBinOpSym :: Parser String
leftBinOpSym = choice ([symbol "+"
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
                  ] ++ map keyword
                  ["and"
                  ,"or"
                  ])

rightBinOpSym:: Parser String
rightBinOpSym = choice ([symbol "|>"])

testPred :: Parser String
testPred = choice (map keyword ["is"
                               ,"is-not"
                               ,"raises"
                               ,"raises-satisfies"
                               ,"satisfies"
                               ])

unaryMinus :: Parser Expr
unaryMinus = UnaryMinus <$> (symbol "-" *> term)

lamE :: Parser Expr
lamE = Lam <$> (keyword_ "lam" *> funHeader <* optional_ (keyword_ "block") <* symbol_ ":")
           <*> (stmts <* keyword_ "end")

curlyLam :: Parser Expr
curlyLam = CurlyLam
    <$> (symbol_ "{" *> funHeader <* optional_ (keyword_ "block") <* symbol_ ":")
    <*> (stmts <* symbol_ "}")

{-
special case for bindings in commasep list -
in some contexts, the parser will parse a type tuple with implicit parens
but this isn't allowed when it can be ambiguous and the , could mean something
else too,
-}

simpleBinding :: Bool -> Parser SimpleBinding
simpleBinding allowImplicitTypeTuple =
    SimpleBinding <$> boption NoShadow (Shadow <$ keyword_ "shadow")
    <*> identifier
    <*> optional (symbol_ "::" *> typ allowImplicitTypeTuple)

-- doesn't parse number, string, variant bindings
limitedBinding :: Bool -> Parser Binding
limitedBinding allowImplicitTypeTuple =
    bindingSuffixes allowImplicitTypeTuple =<< bterm
  where
    bterm = shadowBinding <|> nameBinding <|> tupleBinding
    nameBinding = do
        x <- identifier
        if x == "_"
           then pure WildcardBinding
           else pure $ NameBinding x

bindingSuffixes :: Bool
                -> Binding
                -> Parser Binding
bindingSuffixes allowImplicitTypeTuple e = option e $ do
        s <- choice [asSuffix
                    ,typedSuffix
                    ]
        bindingSuffixes allowImplicitTypeTuple $ s e
  where
    asSuffix = asb 
               <$> (keyword "as" *> boption NoShadow (Shadow <$ keyword_ "shadow"))
               <*> identifier
      where
        asb s i b = AsBinding b s i
    typedSuffix =
        flip TypedBinding <$> (symbol_ "::" *> typ allowImplicitTypeTuple)

shadowBinding :: Parser Binding
shadowBinding = ShadowBinding <$> (keyword_ "shadow" *> identifier)

tupleBinding :: Parser Binding
tupleBinding = TupleBinding <$> (symbol "{" *> xSep1 ';' (binding True) <* symbol "}")

binding :: Bool -> Parser Binding
binding allowImplicitTypeTuple =
    bindingSuffixes allowImplicitTypeTuple =<< bterm
  where
    bterm = shadowBinding <|> nameOrVariantBinding <|> tupleBinding
            <|> numLitBinding <|> stringLitBinding
    numLitBinding = do
        x <- num
        maybe (fail $ "parsing number failed: " ++ x)
          (pure . NumberLitBinding) (readMaybe x)
    stringLitBinding = StringLitBinding <$> stringRaw
    nameOrVariantBinding = do
        n <- variantName
        as <- option [] variantArgs
        pure $ case (n,as) of
            (["_"],[]) -> WildcardBinding
            ([n'],[]) -> NameBinding n'
            _ -> VariantBinding n as
    variantName = do
        i <- identifier
        sfs <- many (symbol_ "." *> identifier)
        pure (i:sfs)
    variantArgs = parens (commaSep (binding False))

expressionLetRec :: Parser Expr
expressionLetRec = keyword_ "letrec" *> letBody LetRec

expressionLet :: Parser Expr
expressionLet = keyword_ "let" *> letBody Let
 
letBody :: ([(Binding,Expr)] -> [Stmt] -> Expr) -> Parser Expr
letBody ctor = ctor <$> commaSep1 bindExpr
                    <*> (optional_ (keyword_ "block") *> symbol_ ":"
                         *> stmts <* keyword_ "end")

bindExpr :: Parser (Binding,Expr)
bindExpr = (,) <$> limitedBinding True <*> (symbol_ "=" *> expr)

simpleBindExpr :: Parser (SimpleBinding,Expr)
simpleBindExpr = (,) <$> simpleBinding True <*> (symbol_ "=" *> expr)

ifE :: Parser Expr
ifE = do
    keyword_ "if"
    ife <- conds
    nextBranch [ife]
  where
    conds = (,) <$> expr <*> (optional_ (keyword_ "block") *> symbol_ ":" *> stmts)
    cond = (,) <$> expr <*> (symbol_ ":" *> stmts)
    nextBranch bs =
        choice [do
                x <- elsePart
                case x of
                    Right el -> endif bs (Just el)
                    Left b -> nextBranch (b:bs)
               ,endif bs Nothing]
    elsePart = do
        keyword_ "else"
        choice
            [Right <$> (symbol_ ":" *> stmts)
            ,Left <$> (keyword_ "if" *> cond)
            ]
    endif bs el = keyword_ "end" *> pure (If (reverse bs) el)

ask :: Parser Expr
ask = do
    keyword_ "ask"
    optional_ (symbol_ "block")
    symbol_ ":"
    nextBranch []
  where
    nextBranch bs =
        choice [do
                x <- branchPart
                case x of
                    Right ot -> endask bs (Just ot)
                    Left b -> nextBranch (b:bs)
               ,endask bs Nothing]
    branchPart = do
        symbol_ "|"
        choice
            [Right <$> (keyword_ "otherwise" *> symbol_ ":" *> stmts)
            ,Left <$> ((,) <$> (expr <* keyword "then" <* symbol_ ":")
                          <*> stmts)
            ]
    endask bs ot = keyword_ "end" *> pure (Ask (reverse bs) ot)     

block :: Parser Expr
block = Block <$>
    (keyword_ "block" *> symbol_ ":" *>
    stmts
    <* keyword_ "end")

cases :: Parser Expr
cases = do
    t <- keyword_ "cases" *> expr
    ty <- optional (symbol_ "::" *> typ True)
          <* optional_ (keyword_ "block") <* symbol_ ":"
    nextCase t ty []
  where
    nextCase t ty cs =
        choice [do
                x <- casePart
                case x of
                    Right el -> endCase t ty cs (Just el)
                    Left c -> nextCase t ty (c:cs)
               ,endCase t ty cs Nothing]
    casePart = do
        symbol_ "|"
        choice
            [Right <$> (keyword_ "else" *> symbol_ "=>" *> stmts)
            ,Left <$> ((,,) <$> (binding False <?> "case pattern")
                       <*> (optional ((keyword_ "when" *> expr) <?> "when clause"))
                       <*> (symbol_ "=>" *> stmts))]
    endCase t ty cs el = keyword_ "end" *> pure (Cases t ty (reverse cs) el)

typeLet :: Parser Expr
typeLet = TypeLet
    <$> (keyword_ "type-let" *> commaSep1 (typeDecl False))
    <*> (symbol_ ":" *> stmts <* keyword_ "end")

forExpr :: Parser Expr
forExpr = For
    -- there's probably a fairly easy way to make this work with
    -- any expr in this position
    <$> (keyword_ "for" *> (Iden <$> identifier))
    <*> parens (commaSep forArg)
    <*> optional (symbol "->" *> typ False)
    <*> (symbol ":" *> stmts <* keyword_ "end")
  where
    forArg = (,) <$> binding False
                 <*> (keyword_ "from" *> expr)

tableSel :: Parser Expr
tableSel = TableSel
    <$> (keyword_ "table" *> commaSep identifier <* symbol_ ":")
    <*> (many relLineSel <* keyword_ "end")
  where
    relLineSel = RowSel <$> (keyword "row" *> symbol_ ":" *> commaSep expr)

template :: Parser Expr
template = do
    sp <- sourcePos
    Template sp <$ try (symbol_ "...")

assertTypeCompat :: Parser Expr
assertTypeCompat = do
    keyword_ "assert-type-compat"
    choice [uncurry AssertTypeCompat <$> parens ((,) <$> expr <*> (symbol_ "::" *> typ True))
           ,pure $ Iden "assert-type-compat"]

receive :: Parser Expr
receive = do
    keyword_ "receive"
    optional_ (keyword_ "block")
    symbol_ ":"
    nextCase []
  where
    nextCase cs =
        choice [do
                x <- casePart
                case x of
                    Right el -> endCase cs (Just el)
                    Left c -> nextCase (c:cs)
               ,endCase cs Nothing]
    casePart = do
        symbol_ "|"
        choice
            [Right <$> ((,) <$> after <*> (symbol_ "=>" *> stmts))
            ,Left <$> ((,,) <$> (binding False <?> "case pattern")
                       <*> (optional ((keyword_ "when" *> expr) <?> "when clause"))
                       <*> (symbol_ "=>" *> stmts))]
    endCase cs aft = keyword_ "end" *> pure (Receive (reverse cs) aft)
    after = keyword_ "after" *> expr

methodExpr :: Parser Expr
methodExpr = MethodExpr <$> (keyword_ "method" *> method)

method :: Parser Method
method = Method
    <$> (funHeader <* optional_ (keyword_ "block") <* symbol_ ":")
    <*> (stmts <* keyword_ "end")

numE :: Parser Expr
numE = do
    x <- num
    maybe (fail $ "parsing number failed: " ++ x)
          (pure . Num) (readMaybe x)

stringE :: Parser Expr
stringE = Text <$> stringRaw
            <?> "string literal"

stringRaw :: Parser String
stringRaw = (quoted <|> multiline) <?> "string literal"
  where
    quoted = unescape <$>
             choice [char_ '\'' *> takeWhileP Nothing (/='\'') <* lexeme_ (char_ '\'')
                    ,char_ '"' *> takeWhileP Nothing (/='"') <* lexeme_ (char_ '"')]
    multiline = startMultiline *> ctu
    startMultiline = string "```" <?> ""
    endMultiline = symbol_ "```"
    ctu = ([] <$ endMultiline) <|> ((:) <$> anySingle <*> ctu)
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
    methodField = do
        keyword_ "method"
        i <- identifier
        m <- method
        pure (i,MethodExpr m)
    firstMethodField = do
        x <- methodField
        moreRecord [x]
    eitherElement = firstMethodField <|> do
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
              f <- methodField
              moreRecord (f:fs)
             ,do
              f <- fld
              moreRecord (f:fs)]]
    fld = (,) <$> (identifier <* symbol_ ":") <*> expr

construct :: Parser Expr
construct = Construct <$> (symbol_ "[" *> xSep1 '.' identifier <* symbol_ ":")
            <*> (commaSep expr <* symbol_ "]")

---------------------------------------

-- statements

script :: Parser Script
script = Script <$> stmts

stmt :: Parser Stmt
stmt = do
    st <- get
    guard (st == SeenNewline)
    choice
        [recDecl
        ,funDecl
        ,varDecl
        ,dataDecl
        ,checkBlock
        ,typeStmt
        ,ffiTypeStmt
        ,provide
        ,include
        ,importStmt
        ,whenStmt
        ,shadowDecl
        ,usePackage
        ,startsWithExprOrBinding]

stmts :: Parser [Stmt]
stmts = do
    put SeenNewline
    many stmt

recDecl :: Parser Stmt
recDecl = uncurry RecDecl <$> (keyword_ "rec" *> bindExpr)

funDecl :: Parser Stmt
funDecl = FunDecl
    <$> (keyword "fun" *> simpleBinding True)
    <*> funHeader
    <*> (optional_ (keyword_ "block") *> symbol_ ":" *> optional ds)
    <*> stmts
    <*> (boptional whereBlock <* keyword_ "end")
    
  where
    -- unwrapSingle (Block [StmtExpr (a)]) = a
    -- unwrapSingle x = x
    ds = keyword_ "doc" *> symbol_ ":" *> stringRaw

funHeader :: Parser FunHeader
funHeader =
    FunHeader
    <$> option [] tyNameList
    <*> parens (commaSep (binding False))
    <*> optional (symbol_ "->" *> typ True)

whereBlock :: Parser [Stmt]
whereBlock = keyword_ "where" *> symbol_ ":" *> stmts

varDecl :: Parser Stmt
varDecl = uncurry VarDecl <$> (keyword_ "var" *> simpleBindExpr)

dataDecl :: Parser Stmt
dataDecl = (DataDecl
    <$> (keyword_ "data" *> identifier)
    <*> option [] tyNameList
    <*> (symbol_ ":" *> (((:[]) <$> singleVariant)
                         <|> some variant))
    <*> option [] sharing
    <*> boptional whereBlock)
    <* keyword_ "end"
  where
    singleVariant = VariantDecl
                    <$> identifier <*> boption [] (parens (commaSep fld))
                    <*> option [] withMeths
    variant = VariantDecl
              <$> (symbol_ "|" *> identifier)
              <*> boption [] (parens (commaSep fld))
              <*> option [] withMeths
    fld = (,) <$> boption Con (Ref <$ keyword_ "ref") <*> simpleBinding False
    withMeths = keyword_ "with" *> symbol_ ":" *> commaSep withMeth
    withMeth = (,) <$> (keyword_ "method" *> identifier) <*> method
    sharing = keyword_ "sharing" *> symbol_ ":" *> commaSep withMeth

-- todo: remove the try when implement the whitespace rules
tyNameList :: Parser [String]
tyNameList = try (symbol_ "<" *> (commaSep1 identifier <?> "type parameter") <* symbol_ ">")

checkBlock :: Parser Stmt
checkBlock = do
    keyword_ "check"
    nm <- optional stringRaw
    symbol_ ":"
    ss <- stmts
    keyword_ "end"
    pure $ Check nm ss

typeStmt :: Parser Stmt
typeStmt = TypeStmt <$> (keyword_ "type" *> typeDecl True)

typeDecl :: Bool -> Parser TypeDecl
typeDecl allowImplicitTuple = TypeDecl
    <$>  identifier
    <*> option [] tyNameList
    <*> (symbol_ "=" *> typ allowImplicitTuple)

ffiTypeStmt :: Parser Stmt
ffiTypeStmt = FFITypeStmt
    <$> (keyword_ "ffitype" *> identifier)
    <*> (symbol_ "=" *> stringRaw)

provide :: Parser Stmt
provide = Provide <$> (keyword_ "provide"
                       *> symbol_ ":"
                       *> commaSep provideItem
                       <* keyword_ "end")

provideItem :: Parser ProvideItem
provideItem = choice
    [ProvideAll <$ symbol_ "*"
    ,ProvideType <$> (keyword_ "type" *> identifier)
    ,ProvideData <$> (keyword_ "data" *> identifier)
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
    a' <- ctu a
    bchoice [ImportSpecial a' <$> parens (commaSep stringRaw)
            ,pure $ ImportName a']
  where
    ctu a = choice
        [do
         symbol_ "."
         b <- identifier
         ctu (a ++ "." ++ b)
        ,pure a]

importStmt :: Parser Stmt
importStmt = keyword_ "import" *> (importFrom <|> importAs)
  where
    importFrom = ImportFrom
        <$> (keyword_ "from" *> importSource <* symbol_ ":")
        <*> (commaSep provideItem <* keyword_ "end")
    importAs = (Import <$> importSource
                <*> (keyword_ "as" *> identifier))

whenStmt :: Parser Stmt
whenStmt = When
           <$> (keyword_ "when" *> expr <* optional_ (keyword_ "block"))
           <*> (symbol_ ":" *> stmts <* keyword_ "end")

-- todo: what other statements can use shadow
-- fun? rec? var?
shadowDecl :: Parser Stmt
shadowDecl = 
    f <$> (keyword_ "shadow" *> identifier)
    <*> optional ((symbol_ "::" <?> "") *> typ True)
    <*> ((symbol_ "=" <?> "") *> expr)
  where
    f i ty v = let b = ShadowBinding i
                   b1 = case ty of
                      Nothing -> b
                      Just tyx -> TypedBinding b tyx
               in LetDecl b1 v

usePackage :: Parser Stmt
usePackage =
    UsePackage <$> (keyword_ "use" *> keyword_ "package" *> stringRaw)

{-
starts with expr or binding
used at the start of parsing statements
an identifier can become:

setvar, followed by :=
contract, followed by ::
setref, followed by !{

letdecl, followed by the rest of a binding and then =
or a stmtexpr, anything thats an expr that starts with an iden

a { can become
a tuple expr, record expr or tuple binding (plus record binding in the
future)

a "pattern or expression" is fixed to be one or the other:
iden only: := :: (becomes a contract or letdecl)
pattern: followed by =
expression: followed by !{, default if not followed by any of the above
-}


startsWithExprOrBinding :: Parser Stmt
startsWithExprOrBinding =
    startsWithPattern <|> startsWithExpr
  where
    startsWithPattern = do
        {-
a bit convoluted to make the parse errors better
want to use try since bindings and expressions overlap
but don't want e.g. a parse error after the  = part
in a let decl, to reset the try and just give the error
as if it's parsing the pattern or "pattern =" as an expression,
want that exact parse error after the =

{something;shadow a}
a = 2

If you write this now, you get something like

NN | a = 2
   | ^^
unexpected "a "
expecting "::", ":=", "= ", or "as"

would prefer to parse it as a pattern, continue, and give an error
'pattern in expression context' or similar, pointing here:
   | {something, ...
   | ^^

don't want to expose this in the user's syntax though, is there a non
tedious way of doing this?
maybe it's worth having a separate tree for parsing, which allows
parsing stuff which will then get a static check on it before converting
to the user's/interpreter/desugarer syntax

        -}
        let makeContract b t = pure $ pure $ Contract b t
        (ctu :: Parser Stmt) <- try $ do
            p <- limitedBinding True
                -- todo: hack to stop it matching ==
                -- fix this when do the whitespace fix pass
            let myLetDecl = symbol_ "= " *> (pure $ (LetDecl p <$> expr))
                myContract = case p of
                    TypedBinding (NameBinding b) t -> Just $ makeContract b t
                    _ -> Nothing
            choice $ (myLetDecl : maybe [] (:[]) myContract)
        ctu
    startsWithExpr = do
        ex <- expr
        let rf = (,) <$> identifier <*> (symbol_ ":" *> expr)
        choice
            [SetRef ex <$> ((symbol_ "!{" <?> "") *> commaSep1 rf <* symbol "}")
            ,SetVar ex <$> ((symbol_ ":=" <?> "") *> expr)
            ,pure $ StmtExpr ex]

typ :: Bool -> Parser Ann
typ allowImplicitTuple =
    (zeroArgArrow allowImplicitTuple
     <|> startsWithIden allowImplicitTuple
     <|> parensOrNamedArrow
     <|> ttupleOrRecord)
    <?> "type annotation"
  where
    startsWithIden it = do
        i <- identifier
        ctu it i
    ctu it i = do
        i1 <- tname i
        choice $ catMaybes
              [Just $ TParam i1 <$> (symbol_ "<" *> commaSep1 noarrow <* symbol_ ">")
              ,if it
               then Just $ (\is r -> TArrow (TName i1:is) r)
                  <$> (many (symbol_ "," *> noarrow))
                  <*> (symbol_ "->" *> noarrow)
               else Nothing
              ,Just $ pure $ TName i1]
    noarrow = (parensOrNamedArrow <|> do
        i <- identifier
        noarrowctu i) <?> "type annotation"
    zeroArgArrow ait = (do
        symbol_ "->"
        t <- typ ait
        pure $ TArrow [] t) <?> "type annotation"
    tname i = (i:) <$> many (symbol_ "." *> identifier)
    noarrowctu i = do
        i1 <- tname i
        choice
              [TParam i1 <$> (symbol_ "<" *> commaSep1 (typ True) <* symbol_ ">")
              ,pure $ TName i1]
    parensOrNamedArrow = symbol_ "(" *> do
        i <- identifier
        choice [do
                x <- symbol_ "::" *> noarrow
                xs <- option [] $ symbol_ "," *> (commaSep1 ((,) <$> identifier <*> (symbol_ "::" *> noarrow)))
                r <- symbol_ ")" *> symbol_ "->" *> noarrow
                pure $ TNamedArrow ((i,x):xs) r
               ,do
                i1 <- ctu True i <* symbol_ ")"
                pure $ TParens i1]
    ttupleOrRecord = symbol_ "{" *> (f <|> pure (TRecord [])) <* symbol_ "}"
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


extractSource :: String -> String
extractSource src =
    let ls = lines src
    in unlines $ process [] ls
  where
    process acc [] = reverse acc
    process acc (x:xs)
        | ".. code:: burdock" `isPrefixOf` x =
          skipBlankLines acc xs
        | otherwise = process acc xs
    skipBlankLines acc ("":xs) = skipBlankLines acc xs
    skipBlankLines acc xs = processAdd acc xs
    processAdd acc (x:xs)
        | "  " `isPrefixOf` x =
          processAdd (x:acc) xs
        | otherwise = process acc xs
    processAdd acc [] = process acc []
