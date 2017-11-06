{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lox.Syntax where

import Control.Monad.IO.Class
import Data.Fixed
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.IORef
import Data.Monoid
import Data.Text hiding (length, reverse)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Lox.Environment (Environment)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L

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

data LoxExecption = LoxError SourceLocation String
                  | FieldNotFound SourceLocation VarName
                  | LoxReturn SourceLocation Atom
                  | LoxBreak SourceLocation
                  | LoxContinue SourceLocation
                  deriving (Show)

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
               | ClassDecl SourceLocation VarName (Maybe VarName) [Method]
               | Iterator SourceLocation VarName Expr Statement
            deriving (Show)

data Method = Constructor [VarName] Statement
            | StaticMethod VarName [VarName] Statement
            | InstanceMethod VarName [VarName] Statement
            deriving (Show)

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
    sourceLoc (ClassDecl loc _ _ _) = loc
    sourceLoc (Iterator loc _ _ _) = loc

data LVal = LVar VarName
          | Set Expr VarName
          | SetIdx Expr Expr
          deriving (Show)

data Expr = Literal SourceLocation Atom
          | Grouping SourceLocation Expr
          | Var SourceLocation VarName
          | Negate SourceLocation Expr
          | Not SourceLocation Expr
          | Binary BinaryOp Expr Expr
          | IfThenElse SourceLocation Expr Expr Expr
          | Assign SourceLocation LVal Expr
          | Call SourceLocation Expr [Expr]
          | Lambda SourceLocation [VarName] Statement
          | GetField SourceLocation Expr VarName
          | Index SourceLocation Expr Expr
          | Array SourceLocation [Expr]
          deriving (Show)

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
    sourceLoc (GetField loc _ _) = loc
    sourceLoc (Index loc _ _) = loc
    sourceLoc (Array loc _) = loc

-- Native function that supports errors and IO
type NativeFn = [Atom] -> IO (Either LoxExecption Atom)

data Callable = Function [VarName] Statement Env
              | BuiltIn Int NativeFn
              | BuiltInMethod Int NativeFn

arity :: Callable -> Int
arity (Function args _ _) = length args
arity (BuiltIn i _) = i
arity (BuiltInMethod i _) = i

instance Show Callable where
    show (Function args body _) = "(Function " <> show args
                                <> " (" <> show body <> "))"
    show (BuiltIn _ _) = "[NativeCode]"

data Atom = LoxNil
          | LoxBool Bool
          | LoxNum Nano
          | LoxString Text
          | LoxFn Callable
          | LoxClass Class
          | LoxObj Object 
          | LoxArray AtomArray
          deriving (Show)

newtype AtomArray = AtomArray { unArray :: IORef (Vector Atom) }

instance Show AtomArray where
    show a = "AtomArray"

nil :: Atom -> Bool
nil LoxNil = True
nil _ = False

readArray :: MonadIO m => AtomArray -> m (Vector Atom)
readArray = liftIO . readIORef . unArray

type Methods = HM.HashMap VarName Callable

data Class = Class
    { classId :: Int
    , className :: VarName
    , superClass :: Maybe Class
    , initializer :: Maybe Callable
    , staticMethods :: Methods
    , methods :: Methods
    } deriving (Show)

emptyClass :: Class
emptyClass = Class (-1) "" Nothing Nothing mempty mempty

data Object = Object
    { objectId :: Int
    , objectClass :: Class
    , objectFields :: IORef (HM.HashMap VarName Atom)
    }

instance Eq Object where
    a == b = objectId a == objectId b

instance Show Object where
    show o = mconcat ["<", unpack (className $ objectClass o)
                     ,"@", show (objectId o)
                     ,">"
                     ]

instance Eq Class where
    a == b = classId a == classId b

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

instance Hashable BinaryOp -- requires Generic
