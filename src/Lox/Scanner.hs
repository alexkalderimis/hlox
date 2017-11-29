{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Hand-written scanner, because the whole point is to go through the exercise
-- of writing a scanner.
module Lox.Scanner where

import Prelude hiding (null, drop, length, span)

import Data.Char
import Data.Monoid
import Data.Text hiding (reverse)
import Control.Monad.State.Strict
import Control.Arrow ((&&&))
import qualified Data.List as L
import qualified Data.Text.Read as Read
import qualified Data.HashSet as HS

type Scanner = State ScannerState ()

data ScannerState = ScannerState
    { productions :: Tokens
    , failed :: ![ScanError]
    , line :: !Int
    , char :: !Int
    } deriving (Show)

type Code = Text

runScanner :: Scanner -> (Tokens, [ScanError])
runScanner scanner = (reverse.productions &&& failed)
                   $ execState scanner
                   $ ScannerState [] [] 1 0

tokens :: Code -> (Tokens, [ScanError])
tokens src = runScanner (scan src)

tokensFrom :: Int -> Text -> (Tokens, [ScanError])
tokensFrom line src = (reverse.productions &&& failed)
                      $ execState (scan src)
                      $ ScannerState [] [] line 0

type ScanError = (Int, Int, Text)
type Tokens = [(Int, Int, Token)]

data Token = 
    -- Single-character tokens.
    LEFT_PAREN | RIGHT_PAREN
    | LEFT_BRACE | RIGHT_BRACE
    | LEFT_SQUARE | RIGHT_SQUARE
    | COMMA | DOT | SEMICOLON | SLASH
    | PERCENT | COLON

    -- One or two character tokens.
    | BANG | BANG_EQUAL
    | EQUAL | EQUAL_EQUAL
    | GREATER | GREATER_EQUAL
    | LESS | LESS_EQUAL
    | EQUAL_GT
    | PLUS | PLUSPLUS | PLUSEQ
    | MINUS | MINUSMINUS | MINUSEQ
    | STAR | STARSTAR

    -- Literals.
    | IDENTIFIER Text
    | STRING Text
    | INT Int
    | NUMBER Double

    -- Keywords.
    | KEYWORD Text
    
    | EOF
    deriving (Ord, Eq, Show)

reservedWords :: HS.HashSet Text
reservedWords = HS.fromList
    ["and", "or", "if", "then", "else", "false", "true"
    ,"class", "extends", "fun", "super", "this", "static"
    ,"let"
    ,"print", "return", "throw", "try", "catch"
    ,"while", "for", "in", "break", "continue"
    ,"nil"
    , "import", "as", "_"
    ]

scan :: Text -> Scanner
scan = go . uncons

go :: Maybe (Char, Text) -> Scanner
go Nothing = addToken EOF
go (Just (c, rst)) =
    let token t = addToken t >> advance >> scan rst
        skip = advance >> scan rst
    in case c of
            '(' -> token LEFT_PAREN
            ')' -> token RIGHT_PAREN
            '{' -> token LEFT_BRACE
            '}' -> token RIGHT_BRACE
            '[' -> token LEFT_SQUARE
            ']' -> token RIGHT_SQUARE
            ',' -> token COMMA
            '.' -> token DOT
            ':' -> token COLON
            ';' -> token SEMICOLON
            '%' -> token PERCENT
            -- whitespace
            ' ' -> skip
            '\t' -> skip
            '\r' -> skip
            '\n' -> do modify' (\s -> s { line = succ (line s), char = 0 })
                       skip
            -- have to peek ahead
            '*' -> checkNext rst STAR [('*', STARSTAR)]
            '+' -> checkNext rst PLUS  [('+', PLUSPLUS), ('=', PLUSEQ)]
            '-' -> checkNext rst MINUS  [('+', MINUSMINUS), ('=', MINUSEQ)]
            '!' -> checkNext rst BANG  [('=', BANG_EQUAL)]
            '=' -> checkNext rst EQUAL [('=', EQUAL_EQUAL), ('>', EQUAL_GT)]
            '<' -> checkNext rst LESS  [('=', LESS_EQUAL)]
            '>' -> checkNext rst GREATER [('=', GREATER_EQUAL)]
            -- comments
            '/' -> case uncons rst of
                    Just ('/', rst') -> consumeComment rst'
                    mr               -> addToken SLASH >> advance >> go mr
            '"' -> advance >> string rst
            '_' -> token (KEYWORD "_")
            _ | isDigit c -> number c rst
            _ | isAlpha c -> varOrKeyword c rst
            _ -> unexpectedCharacter c rst

varOrKeyword :: Char -> Text -> Scanner
varOrKeyword c src = do
    let (str, rst) = span (\c -> isAlphaNum c || c == '_') src
        ident = singleton c <> str
        tok = if HS.member ident reservedWords
                 then KEYWORD ident
                 else IDENTIFIER ident
    addToken tok
    advanceN (length ident)
    scan rst

unexpectedCharacter :: Char -> Text -> Scanner
unexpectedCharacter c rst = do
    let msg = "Unexpected character: " <> singleton c
    addError msg
    advance
    scan rst

string :: Text -> Scanner
string rst = do
    let (prefix, rst') = breakOn "\"" rst
        setChar s = case breakOnEnd "\n" prefix of
                      (start, end) | not (null start) -> length end
                      _                               -> char s + length prefix + 1
    modify' $ \s -> s { char = setChar s
                      , line = line s + count "\n" prefix
                      }
    if null rst'
       then addError "Expected END_QUOTE"
       else advance >> addToken (STRING prefix) >> scan (drop 1 rst')

number :: Char -> Text -> Scanner
number i rst = do
    let (integral, rst') = span isDigit rst
    case uncons rst' of
        Just ('.', rst'') -> do
            let (fractional, rst''') = span isDigit rst''
                numText = mconcat [ singleton i, integral
                                  , singleton '.' , fractional
                                  ]
            advanceN (length numText)
            case Read.double numText of
                Right (d,_) -> addToken (NUMBER d)
                Left e -> addError (pack e)
            scan rst'''
        mr -> do
            let num = singleton i <> integral
            case Read.decimal num of
                Right (i,_) -> addToken (INT i)
                Left e -> addError (pack e)
            advanceN (length num)
            go mr

consumeComment :: Text -> Scanner
consumeComment src = do
    let (prefix, rst) = breakOn "\n" src
    modify' $ \s -> s { line = succ (line s)
                      , char = 1 + length prefix + char s
                      }
    scan rst

advance :: Scanner
advance = modify' (\s -> s { char = succ (char s) })

advanceN :: Int -> Scanner
advanceN n = modify' (\s -> s { char = char s + n })

checkNext :: Text -> Token -> [(Char,Token)] -> Scanner
checkNext rst t1 ts = case uncons rst of
    Just (c', rst') | Just t <- lookup c' ts -> addToken t >> advance >> advance >> go (uncons rst')
    mr                                     -> addToken t1 >> advance >> go mr

addToken :: Token -> Scanner
addToken t = modify' $ \s -> s { productions = (line s, char s, t) : productions s }

addError :: Text -> Scanner
addError msg = modify' $ \s -> s { failed = (line s, char s, msg) : failed s }
