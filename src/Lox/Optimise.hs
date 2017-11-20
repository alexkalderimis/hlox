{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Lox.Optimise (fromParsed) where

import Data.Monoid
import Data.Generics

import Lox.Syntax

-- turn a parse tree into something we can interpret
fromParsed :: Parsed -> Program
fromParsed = fmap (fmap LoxLit)
           . simplifyExpr

{- -- lift literals to values
asValue :: Literal -> LoxVal
asValue lit = case lit of
                LitNil      -> LoxNil
                LitBool b   -> LoxBool b
                LitInt i    -> LoxInt i
                LitDbl n    -> LoxDbl n
                LitString t -> LoxString t
-}

simplifyExpr :: Parsed -> Parsed
simplifyExpr = everywhere $ mkT k
    where
        k :: Expr' VarName Atom -> Expr' VarName Atom
        k expr = case expr of
                    Grouping _ e -> e

                    Binary op (Literal here (AInt i)) (Literal there (AInt j))               | Just f <- ifn op -> Literal (here :..: there) (AInt (f i j))
                    Binary op (Literal here (AInt i)) (Literal there (AInt j))               | Just f <- fn op  -> Literal (here :..: there) (AInt (f i j))
                    Binary op (Literal here (dbl -> Just i)) (Literal there (dbl -> Just j)) | Just f <- fn op  -> Literal (here :..: there) (ADbl (f i j))
                    Binary Divide (Literal here (dbl -> Just i)) (Literal there (dbl -> Just j))                -> Literal (here :..: there) (ADbl (i / j))

                    Binary op (Literal here (AInt i)) (Literal there (AInt j)) | Just f <- comp op -> Literal (here :..: there) (ABool (f i j))

                    Binary Add (Literal here (Str a)) (Literal there (Str b)) -> Literal (here :..: there) (Str (a <> b))
                    Binary Add (Array here es)        (Array there bs)        -> Array (here :..: there) (es <> bs)

                    Binary And lhs@(Literal _ a) rhs -> if true a then rhs else lhs
                    Binary Or  lhs@(Literal _ a) rhs -> if false a then rhs else lhs

                    Not loc (Literal here a)                               -> Literal loc (ABool (false a))
                    Not loc (Binary op lhs rhs) | Just op' <- inverse op   -> Binary op' lhs rhs

                    Negate loc (Literal _ a) -> Literal loc (num negate a)

                    IfThenElse _ (Literal _ p) e t -> if true p then e else t

                    Assign loc Nothing (LVar (Name v)) (Binary op (Var _ v') e) | v == v'              -> Assign loc (Just op) (LVar (Name v)) e
                    e -> e

        false Nil = True
        false (ABool False) = True
        true = not . false

        fn Add      = Just (+)
        fn Subtract = Just (-)
        fn Multiply = Just (*)
        fn _ = Nothing

        ifn Exponent = Just (^)
        ifn Mod      = Just mod
        ifn _ = Nothing


        comp GreaterThan   = Just (>)
        comp GreaterThanEq = Just (>=)
        comp Equals        = Just (==)
        comp LessThan      = Just (<)
        comp LessThanEq    = Just (<=)
        comp NotEquals     = Just (/=)
        comp _ = Nothing

        inverse Equals        = Just NotEquals
        inverse NotEquals     = Just Equals
        inverse GreaterThan   = Just LessThanEq
        inverse GreaterThanEq = Just LessThan
        inverse LessThan      = Just GreaterThanEq
        inverse LessThanEq    = Just GreaterThan
        inverse _ = Nothing

num :: (forall n. Num n => n -> n) -> Atom -> Atom
num f (AInt i) = AInt (f i)
num f (ADbl i) = ADbl (f i)
num _ a        = a

dbl :: Atom -> Maybe Double
dbl (ADbl d) = Just d
dbl (AInt i) = Just (fromIntegral i)
dbl _ = Nothing
