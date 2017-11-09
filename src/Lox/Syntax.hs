{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Lox.Syntax where

import Control.Monad.IO.Class
import Data.Fixed
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.IORef
import Data.Monoid
import Data.Text hiding (unwords, length, reverse)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Lox.Environment (Environment)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T

import qualified Lox.Core.Array as A

type Value = Either LoxException Atom
type LoxResult a = IO (Either LoxException a)
type VarName = Text
type Env = Environment VarName Atom

newtype Singleton = Singleton (IORef ())
    deriving Eq

instance Show Singleton where
    show _ = "<id>"

data SourceLocation = SourceLocation !Text !Int !Int
                    | SourceLocation :..: SourceLocation
                    | Unlocated
                    | NativeCode
    deriving (Eq, Show)

range :: SourceLocation -> ((Text, Int, Int), (Text, Int, Int))
range (SourceLocation t l c) = ((t, l, c),(t, l, c))
range (start :..: end) = (fst (range start), snd (range end))
range Unlocated = let r = ("No File", 0, 0) in (r, r)
range NativeCode = let r = ("Native-Code", 0, 0) in (r, r)

type Program = [Statement]

class Located a where
    sourceLoc :: a -> SourceLocation

data LoxException = LoxError SourceLocation String
                  | FieldNotFound SourceLocation VarName
                  | LoxReturn SourceLocation Atom
                  | LoxBreak SourceLocation
                  | LoxContinue SourceLocation
                  | ArgumentError SourceLocation 
                                  VarName [String] [Atom]
                  deriving (Show)

data Statement = While SourceLocation Expr Statement
               | DefineFn SourceLocation VarName Arguments Statement
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

data Method = Constructor Arguments Statement
            | StaticMethod VarName Arguments Statement
            | InstanceMethod VarName Arguments Statement
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

type Arguments = ([VarName], Maybe VarName)

data Expr = Literal SourceLocation Atom
          | Grouping SourceLocation Expr
          | Var SourceLocation VarName
          | Negate SourceLocation Expr
          | Not SourceLocation Expr
          | Binary BinaryOp Expr Expr
          | IfThenElse SourceLocation Expr Expr Expr
          | Assign SourceLocation LVal Expr
          | Call SourceLocation Expr [Expr]
          | Lambda SourceLocation (Maybe VarName) Arguments Statement
          | GetField SourceLocation Expr VarName
          | Index SourceLocation Expr Expr
          | Array SourceLocation [Expr]
          | Mapping SourceLocation [(VarName, Expr)]
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
    sourceLoc (Lambda loc _ _ _) = loc
    sourceLoc (GetField loc _ _) = loc
    sourceLoc (Index loc _ _) = loc
    sourceLoc (Array loc _) = loc
    sourceLoc (Mapping loc _) = loc

-- Native function that supports errors and IO
type NativeFn = [Atom] -> LoxResult Atom

-- functions carry around references to Object and Array
-- which lets us use literal notation.
type CoreClasses = (Class, Class)

data Callable = BuiltIn VarName (Int -> Bool) NativeFn
              | Function (Maybe VarName) [VarName] (Maybe VarName)
                         Statement CoreClasses Env

-- is this arity acceptable to this function?
arity :: Callable -> Int -> Bool
arity (Function _ _ (Just _) _ _ _) _ = True
arity (Function _ args _ _ _ _)     n = length args == n
arity (BuiltIn _ p _)               n = p n

instance Show Callable where
    show (Function n args mr body _ _) = unwords ["(Function"
                                                 , show n
                                                 , show args
                                                 , show mr
                                                 , show body
                                                 , ")"
                                                 ]
    show (BuiltIn n _ _) = "[NativeCode " <> T.unpack n <> "]"

data Atom = LoxNil
          | LoxBool Bool
          | LoxNum Nano
          | LoxString Text
          | LoxFn Callable
          | LoxClass Class
          | LoxObj Object 
          | LoxArray AtomArray
          | LoxIter Stepper
          deriving (Show)

typeOf :: Atom -> String
typeOf LoxNil = "nil"
typeOf (LoxBool _) = "Boolean"
typeOf (LoxNum _) = "Number"
typeOf (LoxString _) = "String"
typeOf (LoxFn _) = "Function"
typeOf (LoxClass _) = "Class"
typeOf (LoxObj _) = "Object"
typeOf (LoxArray _) = "Array"
typeOf (LoxIter _) = "Iterator"

data Stepper = forall a. Stepper a (a -> LoxResult (Maybe Atom, a))

instance Show Stepper where
    show _ = "Iterator"

newtype AtomArray = AtomArray (A.Array Atom)

instance Show AtomArray where
    show _ = "AtomArray"

arrayFromList :: (Monad m, MonadIO m) => [Atom] -> m AtomArray
arrayFromList xs = AtomArray <$> liftIO (A.fromList LoxNil xs)

nil :: Atom -> Bool
nil LoxNil = True
nil _ = False

readArray :: MonadIO m => AtomArray -> m (Vector Atom)
readArray (AtomArray xs) = liftIO $ A.readArray xs

type Methods = HM.HashMap VarName Callable

-- closed set of protocols with special syntactic sugar:
data Protocol = Settable -- a[i]            --> apply fn [a, i]
              | Gettable -- a[i] = b        --> apply fn [a, i, b]
              | Iterable -- for (i in a) {} --> apply fn [a]
              deriving (Show, Eq, Generic)
instance Hashable Protocol

data Class = Class
    { classId :: Singleton
    , className :: VarName
    , superClass :: Maybe Class
    , initializer :: Maybe Callable
    , staticMethods :: Methods
    , methods :: Methods
    , protocols :: HM.HashMap Protocol Callable
    } deriving (Show)

emptyClass :: Class
emptyClass = Class (unsafePerformIO $ newSingleton)
                   "" Nothing Nothing mempty mempty mempty

data Object = Object
    { objectClass :: Class
    , objectFields :: IORef (HM.HashMap VarName Atom)
    }

instance Eq Object where
    a == b = objectFields a == objectFields b

instance Show Object where
    show o = mconcat ["<Instance of "
                     , unpack (className $ objectClass o)
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

newSingleton :: IO Singleton
newSingleton = Singleton <$> newIORef ()

unsafeSingleton :: () -> Singleton
unsafeSingleton = unsafePerformIO . fmap Singleton . newIORef

-- for use in native modules
argumentError :: [String] -> [Atom] -> LoxResult Atom
argumentError types = return . Left . ArgumentError NativeCode "" types

