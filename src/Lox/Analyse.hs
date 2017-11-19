{-# LANGUAGE OverloadedStrings #-}

-- detect things like unused variables, etc
module Lox.Analyse where

import Data.Monoid
import Control.Applicative
import Control.Monad
import qualified Data.HashSet as HS

import Control.Monad.State.Strict
import Lox.Syntax

type AssignedM = State (HS.HashSet VarName)

isAssignedIn :: VarName -> (Statement' VarName a) -> Bool
isAssignedIn var stm = var `HS.member` assigned
    where assigned = evalState (assignedVars stm) HS.empty

assignedVars :: (Statement' VarName a) -> AssignedM (HS.HashSet VarName)
assignedVars stm = do
    closed <- get
    r <- assignedVars' stm
    return (HS.difference r closed)

-- free vars that are assigned
assignedVars' :: (Statement' VarName a) -> AssignedM (HS.HashSet VarName)
assignedVars' (Block _ stms) = do
    closed <- get
    assigned <- HS.unions <$> mapM assignedVars stms
    put closed -- any declarations performed here are now out of scope.
    return assigned
assignedVars' (ClassDecl _ _ classname _ ms) = do
    modify' (HS.insert classname)
    HS.unions <$> mapM assignedInMethod ms
    where
        assignedInMethod (Constructor (vs, mv) stm) = do
            closed <- get
            let bound = (vs >>= patternVars) ++ (maybe [] patternVars mv)
            modify' (HS.insert "this")
            modify' (<> HS.fromList bound)
            assignedVars stm <* put closed
        assignedInMethod (StaticMethod _ (vs, mv) stm) = do
            closed <- get
            let bound = (vs >>= patternVars) ++ (maybe [] patternVars mv)
            modify' (<> HS.fromList bound)
            assignedVars stm <* put closed
        assignedInMethod (InstanceMethod _ (vs, mv) stm) = do
            closed <- get
            let bound = (vs >>= patternVars) ++ (maybe [] patternVars mv)
            modify' (HS.insert "this")
            modify' (<> HS.fromList bound)
            assignedVars stm <* put closed
assignedVars' (Declare _ v) = mempty <$ modify' (HS.insert v)
assignedVars' (Define _ p e) = do
    let vs = patternVars p
    modify' (<> HS.fromList vs)
    assigned <- assignments e
    return assigned
assignedVars' (DefineFn _ v (vs, mv) body) = do
    modify' (HS.insert v)
    closed <- get
    let bound = (vs >>= patternVars) ++ (maybe [] patternVars mv)
    modify' (<> HS.fromList bound)
    assignedVars body <* put closed
assignedVars' (ExprS e) = assignments e
assignedVars' (If _ e stm mstm) = do
    a <- assignments e
    b <- assignedVars stm
    c <- maybe (return mempty) assignedVars mstm
    return (a <> b <> c)
assignedVars' (Iterator _ p e stm) = do
    closed <- get
    a <- assignments e
    put (foldr HS.insert closed $ patternVars p)
    b <- assignedVars stm
    return (a <> b) <* put closed
assignedVars' (ForLoop _ minit mcond mpost body) = do
    closed <- get
    a <- maybe (return mempty) assignedVars minit
    b <- maybe (return mempty) assignments mcond
    c <- assignedVars body
    d <- maybe (return mempty) assignedVars mpost
    return (a <> b <> c <> d) <* put closed
assignedVars' (Print _ e) = assignments e
assignedVars' (Return _ e) = assignments e
assignedVars' (Throw _ e) = assignments e
assignedVars' (Try _ stm handlers) = do
    closed <- get
    a <- assignedVars stm
    put closed
    b <- HS.unions <$> forM handlers
                       (\(v, stm) -> do modify' (HS.insert v)
                                        assignedVars stm <* put closed)
    return (a <> b)
assignedVars' (While _ e stm) = (<>) <$> assignments e <*> assignedVars stm

assignedVars' _ = return mempty

assignments :: (Expr' VarName a) -> AssignedM (HS.HashSet VarName)
assignments e = do
    closed <- get
    s <- assignments' e
    return (HS.difference s closed)

assignments' :: (Expr' VarName a) -> AssignedM (HS.HashSet VarName)
assignments' (Assign _ _ (LVar p) e) = do
    others <- assignments e
    let vs = patternVars p
    modify' $ \s -> foldr HS.insert s vs
    return (HS.fromList vs <> others)
assignments' (Grouping _ e) = assignments' e
assignments' (Negate _ e) = assignments' e
assignments' (Not _ e) = assignments' e
assignments' (Binary _ a b) = HS.union <$> assignments a <*> assignments b
assignments' (IfThenElse _ a b c) = HS.unions <$> mapM assignments [a, b, c]
assignments' (Call _ a args) = HS.union <$> assignments a
                                        <*> (HS.unions <$> mapM assignments args)
assignments' (Lambda _ _ (vs, mv) stm) = do
    closed <- get
    let bound = (vs >>= patternVars) ++ (maybe [] patternVars mv)
    modify' (<> HS.fromList bound)
    assignedVars stm <* put closed
assignments' (GetField _ e _) = assignments' e
assignments' (Index _ a b) = (<>) <$> assignments a <*> assignments b
assignments' (Array _ es) = HS.unions <$> mapM assignments es
assignments' (ArrayRange _ a b) = HS.union <$> assignments a <*> assignments b
assignments' (Mapping _ pairs) = HS.unions <$> mapM assignments (map snd pairs)
assignments' _ = return mempty
