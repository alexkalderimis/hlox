{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lox.Syntax where

import GHC.Generics (Generic)

import Data.Monoid
import Data.Hashable (Hashable)
import Data.Text hiding (length, reverse)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Lox.Environment (Environment)

type VarName = Text
type Env = Environment VarName Atom

data SourceLocation = SourceLocation !Text !Int !Int
                    | SourceLocation :..: SourceLocation
                    | Unlocated
    deriving (Eq, Show)

range :: SourceLocation -> ((Text, Int, Int), (Text, Int, Int))
range (SourceLocation t l c) = ((t, l, c),(t, l, c))
range (start :..: end) = (fst (range start), snd (range end))
range Unlocated = (("No File", 0, 0), ("No File", 0, 0))

type Program = [Statement]

class Located a where
    sourceLoc :: a -> SourceLocation

data Statement = While SourceLocation Expr Statement
               | DefineFn SourceLocation VarName [VarName] Statement
               | Define SourceLocation VarName Expr
               | Declare SourceLocation VarName
               | ExprS Expr
               | Print SourceLocation Expr
               | Block SourceLocation [Statement]
               | Break SourceLocation
               | Continue SourceLocation
               | Return SourceLocation Expr
               | If SourceLocation Expr Statement (Maybe Statement)
            deriving (Show, Eq)

instance Located Statement where
    sourceLoc (While loc _ _) = loc
    sourceLoc (DefineFn loc _ _ _) = loc
    sourceLoc (Declare loc _) = loc
    sourceLoc (ExprS e) = sourceLoc e
    sourceLoc (Print loc _) = loc
    sourceLoc (Block loc _) = loc
    sourceLoc (Break loc) = loc
    sourceLoc (Continue loc) = loc
    sourceLoc (Return loc _) = loc
    sourceLoc (If loc _ _ _) = loc

data Expr = Literal SourceLocation Atom
          | Grouping SourceLocation Expr
          | Var SourceLocation VarName
          | Negate SourceLocation Expr
          | Not SourceLocation Expr
          | Binary BinaryOp Expr Expr
          | IfThenElse SourceLocation Expr Expr Expr
          | Assign SourceLocation VarName Expr
          | Call SourceLocation Expr [Expr]
          | Lambda SourceLocation [VarName] Statement
          deriving (Show, Eq)

instance Located Expr where
    sourceLoc (Literal loc _) = loc
    sourceLoc (Grouping loc _) = loc
    sourceLoc (Var loc _) = loc
    sourceLoc (Negate loc _) = loc
    sourceLoc (Not loc _) = loc
    sourceLoc (Binary _ lhs rhs) = sourceLoc lhs :..: sourceLoc rhs
    sourceLoc (IfThenElse loc _ _ _) = loc
    sourceLoc (Assign loc _ _) = loc
    sourceLoc (Call loc _ _) = loc
    sourceLoc (Lambda loc _ _) = loc

-- Native function that supports errors and IO
type NativeFn = [Atom] -> IO (Either String Atom)

data Callable = Function [VarName] Statement Env
              | BuiltIn Int NativeFn

arity :: Callable -> Int
arity (Function args _ _) = length args
arity (BuiltIn i _) = i

instance Show Callable where
    show (Function args body _) = "(Function " <> show args
                                <> " (" <> show body <> "))"
    show (BuiltIn _ _) = "[NativeCode]"

instance Ord Callable where
    a `compare` b = EQ

instance Eq Callable where
    a == b = True

data Atom = LoxNil
          | LoxUndefined
          | LoxBool Bool
          | LoxNum Double
          | LoxString Text
          | LoxFn Callable
          deriving (Ord, Show, Eq)

data BinaryOp = Equals
              | NotEquals
              | LessThan
              | LessThanEq
              | GreaterThan
              | GreaterThanEq
              | Add
              | Subtract
              | Multiply
              | Divide
              | And
              | Or
              | Seq
              | Mod
              deriving (Generic, Show, Eq)

instance Hashable BinaryOp
