{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lox.Parser where

import Prelude hiding (span)

import Control.Monad
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Data.Text hiding (span, null, empty, group, reverse)
import qualified Data.Vector as V

import Lox.Syntax hiding (Statement, Expr, Method)
import Lox.Scanner (Tokens, Token(..))
import Debug.Trace

-- here we parse literals, not all possible values
type Statement = Statement' VarName Literal
type Expr = Expr' VarName Literal
type Method = Method' VarName Literal

data ParserState = ParserState
    { tokens :: !Tokens
    , location :: !SourceLocation
    , breakable :: !Bool
    , returnable :: !Bool
    , inMethod :: !Bool
    } deriving (Show)

data ParseError = Fatal !SourceLocation !Text
                | Recoverable !SourceLocation !Text
                | NoParse
                deriving (Show, Eq)

tokenStream :: FilePath -> Tokens -> ParserState
tokenStream fp ts = ParserState
    { tokens = ts
    , location = SourceLocation (pack fp) 0 0
    , breakable = False
    , returnable = False
    , inMethod = False
    }

newtype Parser a = Parser
    { runParser :: (ParserState -> (Either ParseError a, ParserState)) }
    deriving Functor

instance Applicative Parser where
    pure x = Parser (\s -> (Right x, s))
    pf <*> pa = Parser $ \s -> let (f, s') = runParser pf s
                                   (a, s'') = runParser pa s'
                                in (f <*> a, s'')
instance Monad Parser where
    return = pure
    fail e = Parser $ \s -> (Left (Recoverable (location s) (pack e)), s)
    pa >>= f = Parser $ \s ->
        let (ma, s') = runParser pa s
         in either (\es -> (Left es, s')) (\a -> runParser (f a) s') ma

instance Alternative Parser where
    empty   = Parser (Left NoParse,)
    some pa = fmap return pa
    left <|> right = Parser $ \s -> case runParser left s of
        r@(Left Fatal{}, _) -> r
        (Left _, _)         -> runParser right s
        ok                  -> ok

backtrack :: Text -> Parser a
backtrack msg = Parser $ \s -> (Left (Recoverable (location s) msg), s)

fatal :: Text -> Parser a
fatal msg = Parser $ \s -> (Left (Fatal (location s) msg), s)

program :: Parser Parsed
program = many declaration <* eof

eof :: Parser ()
eof = expect EOF <|> (do n <- next
                         fatal ("Expected EOF, got " <> pack (show n)))

declaration :: Parser Statement
declaration =   (importStatement <* semis)
            <|> (classDeclaration <* semis)
            <|> (functionDefinition <* semis)
            <|> (varDefinition <* semis)
            <|> (varDeclaration <* semis)
            <|> statement
    where semis = skipWhile (== SEMICOLON)

importStatement :: Parser Statement
importStatement = do
    keyword "import"
    start <- loc
    m <- ModuleIdentifier <$> manySepBy DOT identifier
    mp <- optional (keyword "as" >> pattern)
    end <- loc
    return (Import (span start end) m mp)

classDeclaration :: Parser Statement
classDeclaration = do
    keyword "class"
    start <- loc
    IDENTIFIER className <- next
    superClass <- optional (keyword "extends" >> identifier)
    expect LEFT_BRACE 
    methods <- many (method <* skipWhile (== SEMICOLON))
    expect RIGHT_BRACE
    end <- loc

    return (ClassDecl (span start end) className className superClass methods)

method :: Parser Method
method = constructor <|> staticMethod <|> instanceMethod
    where
      constructor = do IDENTIFIER "init" <- next
                       Constructor <$> arguments <*> thisAllowed blockStatement
      staticMethod = do keyword "static"
                        StaticMethod <$> identifier
                                     <*> arguments
                                     <*> functionBody EQUAL
      instanceMethod = InstanceMethod <$> identifier
                                      <*> arguments
                                      <*> thisAllowed (functionBody EQUAL)

functionDefinition :: Parser Statement
functionDefinition = do
    keyword "fun"
    start <- loc
    IDENTIFIER var <- next
    args <- arguments
    body <- functionBody EQUAL
    end <- loc

    return (DefineFn (span start end) var args body)

functionBody :: Token -> Parser Statement
functionBody tok = returning $
    blockStatement
    <|>
    (expect tok >> (returnStatement <|> (Return <$> loc <*> expression)))

arguments :: Parser Arguments
arguments = expect LEFT_PAREN *> patternList <* expect RIGHT_PAREN

identifier :: Parser VarName
identifier = do IDENTIFIER var <- next
                return var

varDefinition :: Parser Statement
varDefinition = do
    expect (KEYWORD "let")
    start <- loc
    p <- pattern
    expect EQUAL
    e <- expression
    end <- loc
    return (Define (span start end) p e)

varDeclaration :: Parser Statement
varDeclaration = do
    expect (KEYWORD "let")
    start <- loc
    IDENTIFIER var <- next
    end <- loc
    return (Declare (span start end) var)

statement :: Parser Statement
statement = do
    s <- ifStatement
         <|> loopControl
         <|> returnStatement
         <|> printStatement
         <|> throwStatement
         <|> tryCatchStatement
         <|> whileStatement
         <|> oldStyleForLoop
         <|> newStyleForLoop
         <|> expressionStatement
         <|> blockStatement
    skipWhile (== SEMICOLON)
    return s

returnStatement :: Parser Statement
returnStatement = do
    KEYWORD "return" <- next
    b <- is returnable
    if not b then fatal "Cannot return outside a function body"
             else do l <- loc
                     mval <- optional expression
                     return (Return l (fromMaybe (Literal l LitNil) mval))

loopControl :: Parser Statement
loopControl = do
    b <- is breakable
    ms <- optional (break <|> continue)
    case ms of
      Nothing        -> empty
      Just _ | not b -> fatal "Use of loop control statement outside loop"
      Just s         -> return s
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
    return (If (span start end) condition whenTrue whenFalse)

whileStatement :: Parser Statement
whileStatement = do
    expect (KEYWORD "while")
    start <- loc
    condition <- predicate <|> fatal "expected a predicate in while"
    body <- breaking statement <|> fatal "expected a statement"
    end <- loc
    return (While (span start end) condition body)

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

    let here = (span start end)
    return (ForLoop here minit mcond mpost body)
    {-
        while = While here
                      (fromMaybe (Literal here (LitBool True)) mcond)
                      (Block here (body : maybeToList mpost))
    return (Block here (maybeToList minit <> [while]))
    -}

newStyleForLoop :: Parser Statement
newStyleForLoop = do
    keyword "for"
    start <- loc
    expect LEFT_PAREN
    p <- pattern
    keyword "in"
    iteree <- expression
    expect RIGHT_PAREN
    body <- breaking statement
    end <- loc

    let here = span start end
    return (Iterator here p iteree body)

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
    e <- expression <|> fatal "print statement without expression"
    end <- loc
    return (Print (span start end) e)

throwStatement :: Parser Statement
throwStatement = do
    expect (KEYWORD "throw")
    start <- loc
    e <- expression <|> fatal "throw statement without expression"
    end <- loc
    return (Throw (span start end) e)

tryCatchStatement :: Parser Statement
tryCatchStatement = do
    keyword "try"
    start <- loc
    dodgy <- blockStatement
    handlers <- many catchHandler
    end <- loc
    return (Try (span start end) dodgy handlers)
    where
        catchHandler = do
            keyword "catch"
            expect LEFT_PAREN
            IDENTIFIER var <- next
            expect RIGHT_PAREN
            handler <- blockStatement
            return (var, handler)

blockStatement :: Parser Statement
blockStatement = do
    expect LEFT_BRACE
    start <- loc
    sts <- many declaration
    expect RIGHT_BRACE
    end <- loc
    return (Block (span start end) sts)

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
    assign' <|> incOrDec <|> ifThenElse <|> booleans
    where
        lval = do
            lhs <- call
            case lhs of
              (GetField loc e name) -> return (Set e name)
              (Index loc e idx) -> return (SetIdx e idx)
              (Var loc name) -> return (LVar (Name name))
              _ -> empty
        assign' = do
            here <- loc
            lhs <- lval <|> (LVar <$> pattern)
            op <- (Nothing <$ expect EQUAL)
                  <|> (Just Add <$ expect PLUSEQ)
                  <|> (Just Subtract <$ expect MINUSEQ)
            e <- assignment <|> fatal "missing RHS in assignment"
            return (Assign (here `span` sourceLoc e) op lhs e)
        incOrDec = do
            here <- loc
            lhs <- lval
            op <- (Add <$ expect PLUSPLUS) <|> (Subtract <$ expect MINUSMINUS)
            let rhs = Literal here $ LitInt 1
            return (Assign here (Just op) lhs rhs)

pattern :: Parser (Pattern VarName)
pattern = do
    t <- next
    case t of
        KEYWORD "_" -> return Ignore
        IDENTIFIER v -> return (Name v)
        LEFT_BRACE -> FromObject <$> (manySepBy COMMA kvPat <* expect RIGHT_BRACE)
        LEFT_SQUARE -> do (ps, rest) <- patternList <* expect RIGHT_SQUARE
                          return (FromArray ps rest)
        _ -> empty
    where
        kvPat = do
            IDENTIFIER v <- next
            mp <- optional (expect COLON >> pattern)
            return (v, fromMaybe (Name v) mp)

patternList :: Parser ([Pattern VarName], Maybe (Pattern VarName))
patternList = do
    ps <- manySepBy COMMA pattern
    rest <- optional $ do if null ps then pure () else expect COMMA
                          expect DOT >> expect DOT
                          pattern
    return (ps, rest)

ifThenElse :: Parser Expr
ifThenElse = do
    expect (KEYWORD "if")
    start <- loc
    p <- expression
    expect (KEYWORD "then")
    a <- expression <|> fatal "no then-expression in if-then-else expression"
    expect (KEYWORD "else")
    b <- expression <|> fatal "no else-expression in if-then-else expression"
    end <- loc
    return (IfThenElse (span start end) p a b)

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
multiplication = binary [(PERCENT, Mod), (STAR, Multiply), (SLASH, Divide)]
                        exponents

exponents :: Parser Expr
exponents = binary [(STARSTAR, Exponent)] unary

unary :: Parser Expr
unary = p <|> fnExpr <|> call <|> atom
  where p = do
            op <- anyOf [BANG, MINUS]
            start <- loc
            e <- expression
            end <- loc
            case op of
                BANG -> return (Not (span start end) e)
                MINUS -> return (Negate (span start end) e)

call :: Parser Expr
call = atom >>= finishCall
  where finishCall e = do
          mp <- optional (anyOf [DOT, LEFT_SQUARE, LEFT_PAREN])
          case mp of
            Nothing -> return e
            Just LEFT_SQUARE -> do
                idx <- expression
                expect RIGHT_SQUARE
                end <- loc
                finishCall $ Index (sourceLoc e `span` end) e idx
            Just LEFT_PAREN  -> do
                args <- manySepBy COMMA assignment <* expect RIGHT_PAREN
                end <- loc
                finishCall $ Call (sourceLoc e `span` end) e args
            Just DOT -> do
                meth <- is inMethod
                name <- identifier <|> ("class" <$ keyword "class")
                end <- loc
                finishCall $ GetField (sourceLoc e `span` end) e name
        super meth = do keyword "super"
                        if meth then return "super"
                                else fatal "illegal reference to super outside method"


group :: Parser Expr
group = do
    expect LEFT_PAREN
    start <- loc
    e <- expression
    expect RIGHT_PAREN
    end <- loc
    return (Grouping (span start end) e)

fnExpr :: Parser Expr
fnExpr = do
    start <- loc
    args <- arguments
    body <- functionBody EQUAL_GT
    end <- loc
    return (Lambda (span start end) Nothing args body)

atom :: Parser Expr
atom = ident <|> array <|> arrayRange <|> mapping <|> literal <|> group
  where
      ident = do t <- next
                 l <- loc
                 case t of
                   IDENTIFIER v -> return (Var l v)
                   KEYWORD "super" -> do inM <- is inMethod
                                         if inM then return $ GetField l (Var l "this") "super"
                                                else fatal "illegal use of 'super' outside method"
                   KEYWORD "this" -> do inM <- is inMethod
                                        if inM then return (Var l "this")
                                                else fatal "illegal use of 'this' outside method"
                   _ -> empty
      array = do expect LEFT_SQUARE
                 start <- loc
                 xs <- manySepBy COMMA assignment
                 expect RIGHT_SQUARE
                 end <- loc
                 return $ Array (span start end) xs
      arrayRange =
              do expect LEFT_SQUARE
                 start <- loc
                 a <- assignment
                 expect DOT >> expect DOT
                 b <- assignment
                 expect RIGHT_SQUARE
                 end <- loc
                 return $ ArrayRange (span start end) a b
      mapping = do expect LEFT_BRACE
                   start <- loc
                   ps <- manySepBy COMMA keyval
                   expect RIGHT_BRACE
                   end <- loc
                   return $ Mapping (span start end) ps
      keyval = do k <- do t <- next
                          case t of
                            IDENTIFIER k -> return k
                            STRING k -> return k
                            _ -> empty
                  expect COLON
                  v <- assignment
                  return (k, v)
      literal = do
            t <- next
            l <- loc
            a <- case t of
                STRING s -> return $ LitString  s
                NUMBER n -> return $ LitDbl n
                INT n    -> return $ LitInt n
                KEYWORD kw -> case kw of
                                "true" -> return $ LitBool True
                                "false" -> return $ LitBool False
                                "nil" -> return $ LitNil
                                _ -> backtrack ("Unexpected keyword " <> pack (show kw))
                _ -> backtrack ("Unexpected token " <> pack (show t))
            return (Literal l a)

type Keyword = Text

keyword :: Keyword -> Parser ()
keyword = expect . KEYWORD

expect :: Token -> Parser ()
expect t = (() <$ anyOf [t]) <|> backtrack ("expected " <> pack (show t))

anyOf :: [Token] -> Parser Token
anyOf ts = match (`elem` ts)

-- the core parser all others are built on top of:
-- pop a token from the stream and store its location
next :: Parser Token
next = Parser $ \s -> case tokens s of
    []             -> (Left (Fatal (location s) "EOF"), s)
    ((l, c, t):ts) -> (Right t, s { tokens = ts
                                  , location = SourceLocation (fileName s) l c
                                  })

fileName :: ParserState -> Text
fileName s = let (SourceLocation t _ _) = location s
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
match f = p <|> backtrack "Failed match"
    where p = do t <- next
                 if f t
                    then return t
                    else empty

is :: (ParserState -> a) -> Parser a
is f = Parser $ \s -> (Right (f s), s)

loc :: Parser SourceLocation
loc = is location

thisAllowed :: Parser a -> Parser a
thisAllowed pa = do
    b <- is inMethod
    set True
    a <- pa
    set b
    return a
    where
        set b = Parser $ \s -> (Right (), s { inMethod = b })

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

tracePeek :: String -> Parser ()
tracePeek mgs = Parser $ \s -> traceShow (mgs, listToMaybe $ tokens s) (Right (), s)

span :: SourceLocation -> SourceLocation -> SourceLocation
span a b = simplify (a :..: b)
