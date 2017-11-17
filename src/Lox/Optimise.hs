module Lox.Optimise (fromParsed) where

import Data.Monoid
import Data.Generics

import Lox.Syntax

-- turn a parse tree into something we can interpret
fromParsed :: Parsed -> Program
fromParsed = fmap (fmap asValue)
           . simplifyExpr

-- lift literals to values
asValue :: Literal -> LoxVal
asValue lit = case lit of
                LitNil      -> LoxNil
                LitBool b   -> LoxBool b
                LitInt i    -> LoxInt i
                LitDbl n    -> LoxDbl n
                LitString t -> LoxString t

simplifyExpr :: Parsed -> Parsed
simplifyExpr = everywhere $ mkT k
    where
        k :: Expr' VarName Literal -> Expr' VarName Literal
        k expr = case expr of
                    Grouping _ e -> e
                    Binary op (Literal here (LitInt i)) (Literal there (LitInt j)) | Just f <- fn op   -> Literal (here :..: there) (LitInt (f i j))
                    Binary op (Literal here (LitInt i)) (Literal there (LitInt j)) | Just f <- comp op -> Literal (here :..: there) (LitBool (f i j))
                    Binary Add (Literal here (LitString a)) (Literal there (LitString b))              -> Literal (here :..: there) (LitString (a <> b))
                    Binary And (Literal here e) _ | e `elem` [LitNil, LitBool False]                   -> Literal here e
                    Binary And (Literal here (LitBool a)) (Literal there (LitBool b))                  -> Literal (here :..: there) (LitBool (a && b))
                    Binary Or (Literal here (LitBool a)) (Literal there (LitBool b))                   -> Literal (here :..: there) (LitBool (a || b))
                    Binary Or (Literal here a) e | a `elem` [LitNil, LitBool False]                    -> e
                    Not loc (Literal _ (LitBool b))                                                    -> Literal loc (LitBool (not b))
                    Not loc (Literal _ LitNil)                                                         -> Literal loc (LitBool True)
                    Not loc (Binary op lhs rhs)                               | Just op' <- inverse op -> Binary op' lhs rhs
                    Negate loc (Literal _ (LitInt i))                                                  -> Literal loc (LitInt (-i))
                    Negate loc (Literal _ (LitDbl i))                                                  -> Literal loc (LitDbl (-i))
                    Binary Add (Array here es) (Array there bs)                                        -> Array (here :..: there) (es <> bs)
                    IfThenElse _ (Literal _ (LitBool True)) e _                                        -> e
                    IfThenElse _ (Literal _ (LitBool False)) _ e                                       -> e
                    Assign loc Nothing (LVar v) (Binary op (Var _ v') e) | v == v'                     -> Assign loc (Just op) (LVar v) e
                    e -> e

        fn Add      = Just (+)
        fn Subtract = Just (-)
        fn Multiply = Just (*)
        fn Exponent = Just (^)
        fn Mod      = Just mod
        fn _ = Nothing

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
