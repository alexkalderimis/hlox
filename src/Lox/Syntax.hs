{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Lox.Syntax where

import Control.Monad.IO.Class
import Control.Exception.Base (Exception)
import Data.Fixed
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable, hash)
import Data.Default
import Data.IORef
import Data.Monoid
import Data.Text hiding (unwords, length, reverse)
import Data.Data (Typeable, Data)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Lox.Environment (Environment)
import System.IO.Unsafe (unsafePerformIO)
import Data.Bifunctor.TH
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T

import qualified Lox.Core.Array as A

type VarName = Text
type Value = Either LoxException LoxVal
type LoxResult a = IO (Either LoxException a)
type Env = Environment VarName LoxVal

type Statement = Statement' VarName LoxVal
type Expr = Expr' VarName LoxVal
type Method = Method' VarName LoxVal
type Program = [Statement]

type StackFrame = (VarName, SourceLocation)

class HasStackFrame a where
    stackFrame :: a -> StackFrame

instance HasStackFrame (Text, SourceLocation) where
    stackFrame = id

newtype Singleton = Singleton (IORef ())
    deriving Eq

instance Show Singleton where
    show _ = "<id>"

data SourceLocation = SourceLocation !Text !Int !Int
                    | SourceLocation :..: SourceLocation
                    | Unlocated
                    | NativeCode
    deriving (Eq, Show, Data, Typeable)

range :: SourceLocation -> ((Text, Int, Int), (Text, Int, Int))
range (SourceLocation t l c) = ((t, l, c),(t, l, c))
range (start :..: end) = (fst (range start), snd (range end))
range Unlocated = let r = ("No File", 0, 0) in (r, r)
range NativeCode = let r = ("Native-Code", 0, 0) in (r, r)

class Located a where
    sourceLoc :: a -> SourceLocation

type Arguments = Arguments' VarName
type Arguments' v = ([v], Maybe v)

newtype ModuleIdentifier = ModuleIdentifier [Text]
    deriving (Show, Eq, Data, Typeable, Generic)

instance Hashable ModuleIdentifier

data Method' v a
    = Constructor (Arguments' v) (Statement' v a)
    | StaticMethod VarName (Arguments' v) (Statement' v a)
    | InstanceMethod VarName (Arguments' v) (Statement' v a)
    deriving (Show, Functor, Data, Typeable)

data Statement' v a
    = Block SourceLocation [Statement' v a]
    | Break SourceLocation
    | ClassDecl SourceLocation VarName v (Maybe v) [Method' v a]
    | Continue SourceLocation
    | Declare SourceLocation v
    | Define SourceLocation (Pattern v) (Expr' v a)
    | DefineFn SourceLocation v (Arguments' v) (Statement' v a)
    | ExprS (Expr' v a)
    | If SourceLocation (Expr' v a) (Statement' v a) (Maybe (Statement' v a))
    | Iterator SourceLocation (Pattern v) (Expr' v a) (Statement' v a)
    | ForLoop SourceLocation (Maybe (Statement' v a))
                             (Maybe (Expr' v a))
                             (Maybe (Statement' v a))
                             (Statement' v a)
    | Print SourceLocation (Expr' v a)
    | Return SourceLocation (Expr' v a)
    | Throw SourceLocation (Expr' v a)
    | Try SourceLocation (Statement' v a) [(v, (Statement' v a))]
    | While SourceLocation (Expr' v a) (Statement' v a)
    | Import SourceLocation ModuleIdentifier v
    deriving (Show, Data, Typeable, Functor)

instance Located (Statement' v a) where
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
    sourceLoc (ClassDecl loc _ _ _ _) = loc
    sourceLoc (Iterator loc _ _ _) = loc
    sourceLoc (ForLoop loc _ _ _ _) = loc
    sourceLoc (Throw loc _) = loc
    sourceLoc (Try loc _ _) = loc
    sourceLoc (Import loc _ _) = loc

type LVal = LVal' VarName LoxVal
data LVal' v a
    = LVar (Pattern v)
    | Set (Expr' v a) VarName
    | SetIdx (Expr' v a) (Expr' v a)
    deriving (Show, Functor, Data, Typeable)

data Pattern v = Ignore
               | Name v
               | FromObject [(VarName, Pattern v)]
               | FromArray [Pattern v] (Maybe (Pattern v))
               deriving (Show, Functor, Data, Typeable)

data Expr' v a
    = Literal SourceLocation a
    | Grouping SourceLocation (Expr' v a)
    | Var SourceLocation v
    | Negate SourceLocation (Expr' v a)
    | Not SourceLocation (Expr' v a)
    | Binary BinaryOp (Expr' v a) (Expr' v a)
    | IfThenElse SourceLocation (Expr' v a) (Expr' v a) (Expr' v a) 
    | Assign SourceLocation (Maybe BinaryOp) (LVal' v a) (Expr' v a)
    | Call SourceLocation (Expr' v a) [Expr' v a]
    | Lambda SourceLocation (Maybe VarName) (Arguments' v) (Statement' v a)
    | GetField SourceLocation (Expr' v a) VarName
    | Index SourceLocation (Expr' v a) (Expr' v a)
    | Array SourceLocation [Expr' v a]
    | ArrayRange SourceLocation (Expr' v a) (Expr' v a)
    | Mapping SourceLocation [(VarName, Expr' v a)]
    deriving (Show, Functor, Data, Typeable)

instance Located (Expr' v a) where
    sourceLoc (Literal loc _) = loc
    sourceLoc (Grouping loc _) = loc
    sourceLoc (Var loc _) = loc
    sourceLoc (Negate loc _) = loc
    sourceLoc (Not loc _) = loc
    sourceLoc (Binary _ lhs rhs) = sourceLoc lhs :..: sourceLoc rhs
    sourceLoc (IfThenElse loc _ _ _) = loc
    sourceLoc (Assign loc _ _ _) = loc
    sourceLoc (Call loc _ _) = loc
    sourceLoc (Lambda loc _ _ _) = loc
    sourceLoc (GetField loc _ _) = loc
    sourceLoc (Index loc _ _) = loc
    sourceLoc (Array loc _) = loc
    sourceLoc (Mapping loc _) = loc

-- Native function that supports errors and IO
type NativeFn = [LoxVal] -> LoxResult LoxVal

-- functions carry around references to Object and Array
-- which lets us use literal notation.
type CoreClasses = (Class, Class)

data Callable = BuiltIn VarName (Int -> Bool) NativeFn
              | Function StackFrame [VarName] (Maybe VarName)
                         Statement CoreClasses Env

instance HasStackFrame Callable where
    stackFrame (BuiltIn sf _ _) = (sf, NativeCode)
    stackFrame (Function sf _ _ _ _ _) = sf

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

-- out of the parse stage there is a limited set of values that
-- may be instantiated.
data Literal
    = LitNil
    | LitBool !Bool
    | LitInt !Int
    | LitDbl !Double
    | LitString !Text
    deriving (Show, Eq, Data, Typeable)

type Parsed = [Statement' VarName Literal]

data LoxVal
    -- Atomic values
    = LoxNil
    | LoxBool !Bool
    | LoxInt {-# UNPACK #-} !Int
    | LoxDbl {-# UNPACK #-} !Double
    | LoxString !Text
    -- Composite stuff
    | LoxFn !Callable
    | LoxClass !Class
    | LoxObj !Object 
    | LoxArray !AtomArray
    | LoxIter !Stepper
    deriving (Show)

instance Default LoxVal where
    def = LoxNil

typeOf :: LoxVal -> String
typeOf LoxNil = "nil"
typeOf (LoxBool _) = "Boolean"
typeOf (LoxInt _) = "Number"
typeOf (LoxDbl _) = "Number"
typeOf (LoxString _) = "String"
typeOf (LoxFn _) = "Function"
typeOf (LoxClass _) = "Class"
typeOf (LoxObj c) = T.unpack (className $ objectClass c)
typeOf (LoxArray _) = "Array"
typeOf (LoxIter _) = "Iterator"

type LoxException = LoxException' LoxVal
data LoxException' a = LoxError SourceLocation String
                  | FieldNotFound SourceLocation VarName
                  | LoxReturn SourceLocation a
                  | LoxBreak SourceLocation
                  | LoxContinue SourceLocation
                  | UserError SourceLocation a
                  | ArgumentError SourceLocation 
                                  VarName [String] [a]
                  | StackifiedError [StackFrame] (LoxException' a)
                  deriving (Show, Data, Typeable)

instance Exception LoxException

data Stepper = forall a. Stepper a (a -> LoxResult (Maybe LoxVal, a))

instance Show Stepper where
    show _ = "Iterator"

newtype AtomArray = AtomArray (A.Array LoxVal)

instance Show AtomArray where
    show _ = "AtomArray"

arrayFromList :: (Monad m, MonadIO m) => [LoxVal] -> m AtomArray
arrayFromList xs = AtomArray <$> (liftIO $  A.fromList xs)

nil :: LoxVal -> Bool
nil LoxNil = True
nil _ = False

readArray :: MonadIO m => AtomArray -> m (Vector LoxVal)
readArray (AtomArray xs) = liftIO $ A.readArray xs

type Methods = HM.HashMap VarName Callable

-- closed set of protocols with special syntactic sugar:
data Protocol = Settable -- a[i]            --> apply fn [a, i]
              | Gettable -- a[i] = b        --> apply fn [a, i, b]
              | Iterable -- for (i in a) {} --> apply fn [a]
              deriving (Show, Eq, Generic, Data, Typeable)
instance Hashable Protocol

data Class = Class
    { classId :: Singleton
    , className :: VarName
    , superClass :: Maybe Class
    , initializer :: Maybe Callable
    , staticMethods :: Methods
    , methods :: Methods
    , protocols :: HM.HashMap Protocol Callable
    , classLocation :: SourceLocation
    } deriving (Show)

instance HasStackFrame Class where
    stackFrame c = (className c, sourceLoc c)

emptyClass :: Class
emptyClass = Class (unsafePerformIO $ newSingleton)
                   "" Nothing Nothing mempty mempty mempty
                   Unlocated

instance Located Class where
    sourceLoc = classLocation

data Object = Object
    { objectClass :: Class
    , objectFields :: TVar (HM.HashMap VarName LoxVal)
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
              | Exponent
              deriving (Generic, Show, Eq, Data, Typeable)

instance Hashable BinaryOp -- requires Generic

newSingleton :: IO Singleton
newSingleton = Singleton <$> newIORef ()

unsafeSingleton :: () -> Singleton
unsafeSingleton = unsafePerformIO . fmap Singleton . newIORef

-- for use in native modules
argumentError :: [String] -> [LoxVal] -> LoxResult LoxVal
argumentError types = return . Left . ArgumentError NativeCode "" types

simplify :: SourceLocation -> SourceLocation
simplify Unlocated = Unlocated
simplify loc = let (l@(a, b, c), r@(d, e, f)) = range loc
                in if l == r then SourceLocation a b c
                             else SourceLocation a b c :..: SourceLocation d e f

patternVars :: Pattern v -> [v]
patternVars Ignore = []
patternVars (Name v) = [v]
patternVars (FromArray ps mp) = (ps >>= patternVars) ++ maybe [] patternVars mp
patternVars (FromObject pairs) = fmap snd pairs >>= patternVars

$(deriveBifunctor ''LVal')
$(deriveBifunctor ''Method')
$(deriveBifunctor ''Statement')
$(deriveBifunctor ''Expr')
