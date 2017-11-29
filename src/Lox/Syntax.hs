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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Lox.Syntax where

import Control.Monad.IO.Class
import Control.Exception.Base (Exception)
import Control.Exception (try, catch)
import Data.Bifunctor
import Data.Fixed
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable, hash)
import Data.Default
import Data.Proxy
import Data.Monoid
import Data.String
import Data.Text
import Data.Data (Typeable, Data)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Lox.Environment (readEnv, Environment)
import System.IO.Unsafe (unsafePerformIO)
import Data.Bifunctor.TH
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T

import qualified Lox.Core.Array as A

type VarName = Text

type StackFrame = (Text, Loc)

class Located a where
    sourceLoc :: a -> Loc

class Described a where
    describe :: a -> Text

stackFrame :: (Located a, Described a) => a -> StackFrame
stackFrame a = (describe a, simplify (sourceLoc a))

data Loc = Loc !Text !Int !Int
         | Loc :..: Loc
         | Unlocated
         | NativeCode
    deriving (Eq, Show, Data, Typeable)

range :: Loc -> ((Text, Int, Int), (Text, Int, Int))
range (Loc t l c) = ((t, l, c),(t, l, c))
range (start :..: end) = (fst (range start), snd (range end))
range Unlocated = let r = ("No File", 0, 0) in (r, r)
range NativeCode = let r = ("Native-Code", 0, 0) in (r, r)

type Arguments = Arguments' VarName
type Arguments' v = ([Pattern v], Maybe (Pattern v))

newtype ModuleIdentifier = ModuleIdentifier { unModuleIdentifier :: [Text] }
    deriving (Show, Eq, Data, Typeable, Generic)

instance Hashable ModuleIdentifier

type Method = Method' VarName Atom
data Method' v a
    = Constructor (Arguments' v) (Statement' v a)
    | StaticMethod VarName (Arguments' v) (Statement' v a)
    | InstanceMethod VarName (Arguments' v) (Statement' v a)
    deriving (Show, Functor, Data, Typeable)

type Statement = Statement' VarName Atom
data Statement' v a
    = Block Loc [Statement' v a]
    | Break Loc
    | ClassDecl Loc VarName v (Maybe v) [Method' v a]
    | Continue Loc
    | Declare Loc v
    | Define Loc (Pattern v) (Expr' v a)
    | DefineFn Loc v (Arguments' v) (Statement' v a)
    | ExprS (Expr' v a)
    | If Loc (Expr' v a) (Statement' v a) (Maybe (Statement' v a))
    | Iterator Loc (Pattern v) (Expr' v a) (Statement' v a)
    | ForLoop Loc (Maybe (Statement' v a))
                             (Maybe (Expr' v a))
                             (Maybe (Statement' v a))
                             (Statement' v a)
    | Print Loc (Expr' v a)
    | Return Loc (Expr' v a)
    | Throw Loc (Expr' v a)
    | Try Loc (Statement' v a) [(v, Statement' v a)]
    | While Loc (Expr' v a) (Statement' v a)
    | Import Loc ModuleIdentifier (Maybe (Pattern v))
    deriving (Show, Data, Typeable, Functor)

instance Located (Statement' v a) where
    sourceLoc (While loc _ _) = loc
    sourceLoc (DefineFn loc _ _ _) = loc
    sourceLoc (Define loc _ _) = loc
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

instance Described (Statement' v a) where
    describe Block{} = "Block"
    describe Break{} = "<break>"
    describe (ClassDecl _ n _ _ _) = "Class declaration: " <> n
    describe Continue{} = "<continue>"
    describe Declare{} = "Variable declaration"
    describe Define{} = "Variable definition"
    describe DefineFn{} = "Function declaration"
    describe (ExprS e) = describe e
    describe If{} = "Conditional statement"
    describe Iterator{} = "Iterator loop"
    describe ForLoop{} = "For loop"
    describe Print{} = "Print statement"
    describe Return{} = "<return>"
    describe Throw{} = "Throw"
    describe Try{} = "Try/Catch"
    describe While{} = "While loop"
    describe Import{} = "Import statement"

type LVal = LVal' VarName Atom
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

type Expr = Expr' VarName Atom
data Expr' v a
    = Literal Loc a
    | Grouping Loc (Expr' v a)
    | Var Loc v
    | Negate Loc (Expr' v a)
    | Not Loc (Expr' v a)
    | Binary BinaryOp (Expr' v a) (Expr' v a)
    | IfThenElse Loc (Expr' v a) (Expr' v a) (Expr' v a) 
    | Assign Loc (Maybe BinaryOp) (LVal' v a) (Expr' v a)
    | Call Loc (Expr' v a) [Expr' v a]
    | Fn Loc (Lambda v a)
    | GetField Loc (Expr' v a) VarName
    | Index Loc (Expr' v a) (Expr' v a)
    | Array Loc [Expr' v a]
    | ArrayRange Loc (Expr' v a) (Expr' v a)
    | Mapping Loc [(Atom, Expr' v a)]
    deriving (Show, Functor, Data, Typeable)

data Lambda v a = Lambda (Maybe VarName) (Arguments' v) (Statement' v a)
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
    sourceLoc (Fn loc _) = loc
    sourceLoc (GetField loc _ _) = loc
    sourceLoc (Index loc _ _) = loc
    sourceLoc (Array loc _) = loc
    sourceLoc (Mapping loc _) = loc

instance Described (Expr' v a) where
    describe Literal{} = "Literal"
    describe Grouping{} = "Group"
    describe Var{} = "Variable reference"
    describe Negate{} = "Negation"
    describe Not{} = "Boolean negation"
    describe Binary{} = "Operator application"
    describe IfThenElse{} = "Conditional expression"
    describe Assign{} = "Variable assignment"
    describe Call{} = "Function application"
    describe Fn{} = "Function expression"
    describe GetField{} = "Field access"
    describe Index{} = "Indexec field access"
    describe Array{} = "Array literal"
    describe ArrayRange{} = "Array range"
    describe Mapping{} = "Object literal"

-- out of the parse stage there is a limited set of values that
-- may be instantiated.
data Atom
    = Nil
    | ABool !Bool
    | AInt {-# UNPACK #-} !Int
    | ADbl {-# UNPACK #-} !Double
    | Str !Text
    deriving (Show, Data, Typeable, Generic)

instance Hashable Atom

instance IsString Atom where
    fromString = Str . T.pack

-- the differs from the derived instance in that it allows comparison between Int and Dbl
instance Ord Atom where
    Nil `compare` _ = LT
    _ `compare` Nil = GT
    (ABool a) `compare` (ABool b) = a `compare` b
    (ABool _) `compare` _ = LT
    _ `compare` (ABool _) = GT
    (AInt a) `compare` (AInt b) = a `compare` b
    (asDbl -> Just a) `compare` (asDbl -> Just b) = a `compare` b
    (Str a) `compare` (Str b) = a `compare` b
    (Str _) `compare` _ = GT
    _ `compare` (Str _) = LT

instance Eq Atom where
    a == b = EQ == (a `compare` b)

asDbl :: Atom -> Maybe Double
asDbl (ADbl d) = Just d
asDbl (AInt i) = Just (fromIntegral i)
asDbl _ = Nothing

-- coerce to int, if the conversion is lossless.
asInt :: Atom -> Maybe Int
asInt (AInt i) = Just i
asInt (ADbl d) = let i = round d
                  in if d == fromIntegral i then Just i else Nothing
asInt _ = Nothing

type Parsed = [Statement' VarName Atom]

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

simplify :: Loc -> Loc
simplify Unlocated = Unlocated
simplify loc = let (l@(a, b, c), r@(d, e, f)) = range loc
                in if l == r then Loc a b c
                             else Loc a b c :..: Loc d e f

patternVars :: Pattern v -> [v]
patternVars Ignore = []
patternVars (Name v) = [v]
patternVars (FromArray ps mp) = (ps >>= patternVars) ++ maybe [] patternVars mp
patternVars (FromObject pairs) = fmap snd pairs >>= patternVars

$(deriveBifunctor ''Lambda)
$(deriveBifunctor ''LVal')
$(deriveBifunctor ''Method')
$(deriveBifunctor ''Statement')
$(deriveBifunctor ''Expr')
