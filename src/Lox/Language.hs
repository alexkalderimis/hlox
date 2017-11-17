module Lox.Language (
    module Lox.Scanner,
    module Lox.Parser,
    module Lox.Interpreter,
    module Lox.Interpreter.Types,
    module Lox.Syntax,
    module Lox.Environment,
    module Lox.Optimise,
    simpleParse -- for use in ghci
                    ) where

import Lox.Scanner (Code, Token(..), Tokens, tokens, tokensFrom)
import Lox.Optimise (fromParsed)
import Lox.Syntax (
    Env, LoxException, LoxException'(..),
    Expr, Expr'(..), Statement, Statement'(..),
    Program, Parsed, Value, LoxResult,
    LoxVal(..), SourceLocation(..), typeOf, range, nil)
import Lox.Parser (
    tokenStream, expression, program, ParseError(..), Parser(runParser))
import Lox.Environment (Environment(..), declare, assign, enterScope, readEnv, boundNames)
import Lox.Interpreter (
    eval, run, runProgram, printLox)
import Lox.Interpreter.Types (
    LoxT, Interpreter(..), interpreter, runLoxT, evalLoxT)

simpleParse :: Code -> IO (Tokens, Parsed)
simpleParse code = do
    let (ts, _) = tokens code
        (Right parsed, _) = runParser program (tokenStream "__sp" ts)
    return (ts, parsed)
