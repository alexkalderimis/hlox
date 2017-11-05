{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lox.Parser where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Data.Text hiding (empty, group, reverse)

import Lox.Syntax
import Lox.Scanner (Tokens, Token(..))
import Debug.Trace

data ParserState = ParserState
    { tokens :: !Tokens
    , location :: !SourceLocation
    , breakable :: !Bool
    , returnable :: !Bool
    } deriving (Show)

tokenStream :: FilePath -> Tokens -> ParserState
tokenStream fp ts = ParserState
    { tokens = ts
    , location = SourceLocation (pack fp) 0 0
    , breakable = False
    , returnable = False
    }

newtype Parser a = Parser
    { runParser :: (ParserState -> (Either [String] a, ParserState)) }
    deriving Functor

instance Applicative Parser where
    pure x = Parser (\s -> (Right x, s))
    pf <*> pa = Parser $ \s -> let (f, s') = runParser pf s
                                   (a, s'') = runParser pa s'
                                in (f <*> a, s'')
instance Monad Parser where
    return = pure
    fail e = Parser $ \s ->
        let SourceLocation t l c = location s
            msg = "At line " <> show l <> ", col " <> show c <> ": " <> e
         in (Left [msg], s)
    pa >>= f = Parser $ \s ->
        let (ma, s') = runParser pa s
         in either (\es -> (Left es, s')) (\a -> runParser (f a) s') ma

instance Alternative Parser where
    empty   = Parser (Left ["<empty>"],)
    some pa = fmap return pa
    left <|> right = Parser $ \s -> case runParser left s of
        (Left es, _) -> runParser right s
        ok           -> ok

program :: Parser Program
program = many declaration <* eof

eof :: Parser ()
eof = expect EOF <|> (do n <- next
                         fail ("Expected EOF, got " <> show n))

declaration :: Parser Statement
declaration =   (functionDefinition <* semis)
            <|> (varDefinition <* semis)
            <|> (varDeclaration <* semis)
            <|> statement
    where semis = skipWhile (== SEMICOLON)

functionDefinition :: Parser Statement
functionDefinition = do
    expect (KEYWORD "fun")
    start <- loc
    IDENTIFIER var <- next
    args <- arguments
    body <- functionBody EQUAL
    end <- loc

    return (DefineFn (start :..: end) var args body)

functionBody :: Token -> Parser Statement
functionBody tok = returning $
    blockStatement
    <|>
    (expect tok >> (returnStatement <|> (Return <$> loc <*> expression)))

arguments :: Parser [VarName]
arguments = do
    expect LEFT_PAREN
    names <- manySepBy COMMA identifier
    expect RIGHT_PAREN
    return names

identifier :: Parser VarName
identifier = do IDENTIFIER var <- next
                return var

varDefinition :: Parser Statement
varDefinition = do
    expect (KEYWORD "let")
    start <- loc
    IDENTIFIER var <- next
    expect EQUAL
    e <- expression
    end <- loc
    return (Define (start :..: end) var e)

varDeclaration :: Parser Statement
varDeclaration = do
    expect (KEYWORD "let")
    start <- loc
    IDENTIFIER var <- next
    end <- loc
    return (Declare (start :..: end) var)

statement :: Parser Statement
statement = do
    s <- ifStatement
         <|> loopControl
         <|> returnStatement
         <|> printStatement
         <|> whileStatement
         <|> oldStyleForLoop
         <|> expressionStatement
         <|> blockStatement
    skipWhile (== SEMICOLON)
    return s

returnStatement :: Parser Statement
returnStatement = do
    b <- is returnable
    if not b then fail "Cannot return outside a function body"
             else do expect (KEYWORD "return")
                     l <- loc
                     mval <- optional expression
                     return (Return l (fromMaybe (Literal l LoxNil) mval))

loopControl :: Parser Statement
loopControl = do
    b <- is breakable
    if b then break <|> continue
         else empty
    where
        break    = expect (KEYWORD "break")    *> (Break <$> loc)
        continue = expect (KEYWORD "continue") *> (Continue <$> loc)

ifStatement :: Parser Statement
ifStatement = do
    expect (KEYWORD "if")
    start <- loc
    condition <- predicate
    whenTrue <- statement
    whenFalse <- optional (expect (KEYWORD "else") >> statement)
    end <- loc
    return (If (start :..: end) condition whenTrue whenFalse)

whileStatement :: Parser Statement
whileStatement = do
    expect (KEYWORD "while")
    start <- loc
    condition <- predicate
    body <- breaking statement
    end <- loc
    return (While (start :..: end) condition body)

oldStyleForLoop :: Parser Statement
oldStyleForLoop = do
    expect (KEYWORD "for")
    start <- loc
    expect LEFT_PAREN
    minit <- optional (varDefinition <|> expressionStatement)
    expect SEMICOLON
    mcond <- optional expression
    expect SEMICOLON
    mpost <- optional expressionStatement
    expect RIGHT_PAREN
    body <- breaking statement
    end <- loc

    let here = (start :..: end)
        while = While here
                      (fromMaybe (Literal here (LoxBool True)) mcond)
                      (Block here (body : maybeToList mpost))
    return (Block here (maybeToList minit <> [while]))

predicate :: Parser Expr
predicate = do
    expect LEFT_PAREN
    condition <- expression
    expect RIGHT_PAREN
    return condition

printStatement :: Parser Statement
printStatement = do
    expect (KEYWORD "print")
    start <- loc
    e <- expression
    end <- loc
    return (Print (start :..: end) e)

blockStatement :: Parser Statement
blockStatement = do
    expect LEFT_BRACE
    start <- loc
    sts <- many declaration
    expect RIGHT_BRACE
    end <- loc
    return (Block (start :..: end) sts)

expressionStatement :: Parser Statement
expressionStatement = ExprS <$> expression

expression :: Parser Expr
expression = discard

binary :: [(Token, BinaryOp)] -> Parser Expr -> Parser Expr
binary ops subexp = do
    subexp >>= build
    where
        build lhs = do
            mt <- optional . anyOf $ fmap fst ops
            case mt >>= (`lookup` ops)  of
              Nothing -> return lhs
              Just op -> Binary op lhs <$> subexp >>= build

discard :: Parser Expr
discard = binary [(COMMA, Seq)] assignment

assignment :: Parser Expr
assignment = do
    lval <- ifThenElse <|> booleans
    assign' lval <|> return lval
    where
        assign' lval = do
            expect EQUAL
            e <- assignment
            case lval of
              (Var l v) -> return (Assign (l :..: sourceLoc e) v e)
              _         -> fail ("Cannot assign to " <> show lval)

ifThenElse :: Parser Expr
ifThenElse = do
    expect (KEYWORD "if")
    start <- loc
    p <- expression
    expect (KEYWORD "then")
    a <- expression
    expect (KEYWORD "else")
    b <- expression
    end <- loc
    return (IfThenElse (start :..: end) p a b)

booleans :: Parser Expr
booleans = binary [(KEYWORD "and", And), (KEYWORD "or", Or)] equality

equality :: Parser Expr
equality = binary [(EQUAL_EQUAL, Equals), (BANG_EQUAL, NotEquals)] comparison

comparison :: Parser Expr
comparison = binary [(GREATER, GreaterThan)
                    ,(GREATER_EQUAL, GreaterThanEq)
                    ,(LESS, LessThan)
                    ,(LESS_EQUAL, LessThanEq)
                    ]
                    addition

addition :: Parser Expr
addition = binary [(PLUS, Add), (MINUS, Subtract)] multiplication

multiplication :: Parser Expr
multiplication = binary [(PERCENT, Mod), (STAR, Multiply), (SLASH, Divide)] unary

unary :: Parser Expr
unary = p <|> fnExpr <|> call <|> atom
  where p = do
            op <- anyOf [BANG, MINUS]
            start <- loc
            e <- expression
            end <- loc
            case op of
                BANG -> return (Not (start :..: end) e)
                MINUS -> return (Negate (start :..: end) e)

call :: Parser Expr
call = do
    e <- atom
    mp <- optional (expect LEFT_PAREN)
    case mp of
      Nothing -> return e
      Just _  -> do args <- manySepBy COMMA assignment <* expect RIGHT_PAREN
                    end <- loc
                    return $ Call (sourceLoc e :..: end) e args

group :: Parser Expr
group = do
    expect LEFT_PAREN
    start <- loc
    e <- expression
    expect RIGHT_PAREN
    end <- loc
    return (Grouping (start :..: end) e)

fnExpr :: Parser Expr
fnExpr = do
    start <- loc
    args <- arguments
    body <- functionBody EQUAL_GT
    end <- loc
    return (Lambda (start :..: end) args body)

atom :: Parser Expr
atom = ident <|> p <|> group <|> fail "expected an expression"
  where
      ident = do t <- next
                 l <- loc
                 case t of
                   IDENTIFIER v -> return (Var l v)
                   _ -> empty
      p = do
            t <- next
            l <- loc
            a <- case t of
                STRING s -> return $ LoxString  s
                NUMBER n -> return $ LoxNum n
                KEYWORD kw -> case kw of
                                "true" -> return $ LoxBool True
                                "false" -> return $ LoxBool False
                                "nil" -> return $ LoxNil
                                _ -> fail ("Unexpected keyword: " <> show kw)
                _ -> fail ("Unexpected token: " <> show t)
            return (Literal l a)

expect :: Token -> Parser ()
expect t = (() <$ anyOf [t]) <|> fail ("expected " <> show t)

anyOf :: [Token] -> Parser Token
anyOf ts = match (`elem` ts)

-- the core parser all others are built on top of:
-- pop a token from the stream and store its location
next :: Parser Token
next = Parser $ \s -> case tokens s of
    []             -> (Left ["EOF"], s)
    ((l, c, t):ts) -> (Right t, s { tokens = ts
                                  , location = SourceLocation (fileName s) l c
                                  })

fileName :: ParserState -> Text
fileName s = let ((t, _, _), _) = range (location s)
              in t

skipWhile :: (Token -> Bool) -> Parser ()
skipWhile f = (match f >> skipWhile f) <|> return ()

manySepBy :: Token -> Parser a -> Parser [a]
manySepBy t pa = go False
    where
        go False = do
            ma <- optional pa
            case ma of
                Nothing -> return []
                Just a -> (a :) <$> (go True <|> pure [])
        go True = do
            expect t
            (:) <$> pa <*> (go True <|> pure [])

match :: (Token -> Bool) -> Parser Token
match f = p <|> fail ("failed match")
    where p = do t <- next
                 if f t
                    then return t
                    else empty

is :: (ParserState -> a) -> Parser a
is f = Parser $ \s -> (Right (f s), s)

loc :: Parser SourceLocation
loc = is location

-- allow parsers to define zones where return statements are allowed
returning :: Parser a -> Parser a
returning pa = do
    b <- is returnable
    setReturning True
    a <- pa
    setReturning b
    return a
    where
        setReturning b = Parser $ \s -> (Right (), s { returnable = b })

-- allow parsers to define zones where break statements are allowed
breaking :: Parser a -> Parser a
breaking pa = do
    b <- is breakable
    setBreaking True
    a <- pa
    setBreaking b
    return a
    where
        setBreaking b = Parser $ \s -> (Right (), s { breakable = b })

