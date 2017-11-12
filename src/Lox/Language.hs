module Lox.Language (
    module Lox.Scanner,
    module Lox.Parser,
    module Lox.Interpreter,
    module Lox.Syntax,
    module Lox.SeqEnv,
    simpleParse -- for use in ghci
                    ) where

import Lox.Scanner (Code, Token(..), Tokens, tokens, tokensFrom)
import Lox.Syntax (
    Env, LoxException, LoxException'(..),
    Expr, Expr'(..), Statement, Statement'(..),
    Program, Parsed, Value, LoxResult,
    LoxVal(..), SourceLocation(..), fromParsed, typeOf, range, nil)
import Lox.Parser (
    tokenStream, expression, program, ParseError(..), Parser(runParser))
import Lox.SeqEnv (Environment(..), declare, assign, enterScope, readEnv, boundNames)
import Lox.Interpreter (
    LoxT, Interpreter(..), interpreter,
    printLox, eval, run, runProgram, runLoxT, evalLoxT)

simpleParse :: Code -> IO (Tokens, Parsed)
simpleParse code = do
    let (ts, _) = tokens code
        (Right parsed, _) = runParser program (tokenStream "__sp" ts)
    return (ts, parsed)
