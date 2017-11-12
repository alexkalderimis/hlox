{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Lox.Transform where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Data (Typeable, Data, gmapM)
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Maybe
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Lox.Syntax

type ReplaceM = State (Int, HM.HashMap VarName Int)

type Replaced = [Statement' Name Literal]

type Name = (VarName, Int)
type St = Statement' Name Literal
type Exp = Expr' Name Literal
type Meth = Method' Name Literal
type LV = LVal' Name Literal
type Args = Arguments' Name

renameVars :: Parsed -> Replaced
renameVars p = evalState (renameVarsM p') (0, HM.empty)
    where p' = fmap (mapVSt (\t -> (t, 0))) p

renameVarsM :: [St] -> ReplaceM [St]
renameVarsM = mapM handleStatement

replaceName :: Name -> ReplaceM Name
replaceName (name,_) = do
    (n, m) <- get
    put (n + 1, HM.insert name n m)
    return (name, n)

declareThis :: ReplaceM ()
declareThis = void $ replaceName ("this", 0)

bindArgs :: Args -> ReplaceM Args
bindArgs (names, mname) = (,) <$> mapM replaceName names <*> mapM replaceName mname

scoped :: ReplaceM a -> ReplaceM a
scoped ra = do
    s <- get
    a <- ra
    put s -- discard changes caused by ra
    return a

getSubstitute :: Name -> ReplaceM Name
getSubstitute (name,_) = do
    msub <- gets (HM.lookup name . snd)
    return . (name,) $ fromMaybe notFound msub
    where
        notFound = error $ "undefined name " ++ show name

handleStatement :: St -> ReplaceM St

handleStatement (DefineFn loc name args stm) = do
    name' <- replaceName name
    (args', stm') <- scoped ((,) <$> bindArgs args <*> handleStatement stm)
    return (DefineFn loc name' args' stm')

handleStatement (Declare loc name) = Declare loc <$> replaceName name

handleStatement (Define loc name e) = do
    e' <- handleExpr e
    name' <- replaceName name
    return (Define loc name' e')

handleStatement (ClassDecl loc name var super ms) = do
    var' <- replaceName var
    super' <- sequence $ fmap getSubstitute super
    ms' <- mapM handleMethod ms
    return (ClassDecl loc name var' super' ms')

handleStatement (Iterator loc name expr stm) = do
    expr' <- handleExpr expr
    name' <- replaceName name
    stm' <- scoped (handleStatement stm)
    return (Iterator loc name' expr' stm')

handleStatement (ForLoop loc before cond after body) = scoped $ do
    before' <- traverse handleStatement before
    cond' <- traverse handleExpr cond
    after' <- traverse handleStatement after
    body' <- handleStatement body
    return (ForLoop loc before' cond' after' body')

handleStatement (Try loc stm handlers) =
    Try loc <$> scoped (handleStatement stm)
            <*> mapM (uncurry f) handlers
    where
        f name stm = scoped $ (,) <$> replaceName name <*> handleStatement stm

handleStatement (Block loc stms) =
    Block loc <$> scoped (mapM handleStatement stms)

handleStatement stm = gmapM f stm
    where 
          f :: forall d. Data d => d -> ReplaceM d
          f = mkM handleStatement `extM` handleExpr `extM` handleMethod

handleMethod :: Meth -> ReplaceM Meth
handleMethod (Constructor args stm) = scoped $ do
    declareThis
    Constructor <$> bindArgs args <*> handleStatement stm
handleMethod (StaticMethod nm args stm) = scoped $ do
    StaticMethod nm <$> bindArgs args <*> handleStatement stm
handleMethod (InstanceMethod nm args stm) = scoped $ do
    declareThis
    InstanceMethod nm <$> bindArgs args <*> handleStatement stm

handleExpr :: Exp -> ReplaceM Exp

handleExpr (Var loc name) = do
    name' <- getSubstitute name
    return (Var loc name')

-- name does not need substitution here as it is for stack traces
handleExpr (Lambda loc mname args stm) = do
    (args', stm') <- scoped ((,) <$> bindArgs args <*> handleStatement stm)
    return (Lambda loc mname args' stm')

handleExpr e = gmapM f e
    where 
          f :: forall d. Data d => d -> ReplaceM d
          f = mkM handleStatement `extM` handleExpr `extM` handleLVar

handleLVar :: LV -> ReplaceM LV
handleLVar (LVar name) = LVar <$> getSubstitute name
handleLVar (Set e name) = do
    e' <- handleExpr e
    return (Set e name)
handleLVar (SetIdx l r) = SetIdx <$> handleExpr l <*> handleExpr r

----- helpers to transform variable types.
-- this is horrible, horrible code

-- there must be a better way to do this....
mapVSt :: forall a b c. (a -> b) -> Statement' a c -> Statement' b c
mapVSt f stm = case stm of
    Block loc stms -> Block loc (fmap (mapVSt f) stms)
    ClassDecl loc nm v mv ms -> ClassDecl loc nm (f v) (fmap f mv)
                                           (fmap (mapVM f) ms)
    Declare loc v -> Declare loc (f v)
    Define loc v e -> Define loc (f v) (mapVE f e)
    DefineFn loc v arg stm -> DefineFn loc (f v) (mapVArg f arg) (mapVSt f stm)
    ExprS e -> ExprS (mapVE f e)
    If loc e a b -> If loc (mapVE f e) (mapVSt f a) (fmap (mapVSt f) b)
    Iterator loc v e s -> Iterator loc (f v) (mapVE f e) (mapVSt f s)
    ForLoop loc me0 me1 me2 stm -> ForLoop loc (fmap (mapVSt f) me0)
                                               (fmap (mapVE f) me1)
                                               (fmap (mapVSt f) me0)
                                               (mapVSt f stm)
    Print loc e -> Print loc (mapVE f e)
    Return loc e -> Return loc (mapVE f e)
    Throw loc e -> Throw loc (mapVE f e)
    Try loc body handlers -> Try loc (mapVSt f body) [(f v, mapVSt f h) | (v, h) <- handlers]
    While loc e s -> While loc (mapVE f e) (mapVSt f s)
    _ -> unsafeCoerce stm -- no interesting subterms

mapVM :: forall a b c. (a -> b) -> Method' a c -> Method' b c
mapVM f m = case m of
    Constructor args stm -> Constructor (mapVArg f args) (mapVSt f stm)
    StaticMethod name args stm -> StaticMethod name (mapVArg f args) (mapVSt f stm)
    InstanceMethod name args stm -> InstanceMethod name (mapVArg f args)
                                                        (mapVSt f stm)

mapVArg :: forall a b. (a -> b) -> Arguments' a -> Arguments' b
mapVArg f (vs, mv) = (fmap f vs, fmap f mv)

mapVE :: forall a b c. (a -> b) -> Expr' a c -> Expr' b c
mapVE f e = case e of
    Grouping loc e -> Grouping loc (mapVE f e)
    Var loc v -> Var loc (f v)
    Negate loc e -> Negate loc (mapVE f e)
    Not loc e -> Not loc (mapVE f e)
    Binary op a b -> Binary op (mapVE f a) (mapVE f b)
    IfThenElse loc p a b -> IfThenElse loc (mapVE f p) (mapVE f a) (mapVE f b)
    Assign loc mop lval e -> let lhs = case lval of
                                    LVar v -> LVar (f v)
                                    Set e fld -> Set (mapVE f e) fld
                                    SetIdx e idx -> SetIdx (mapVE f e) (mapVE f idx)
                              in Assign loc mop lhs (mapVE f e)
    Call loc fn args -> Call loc (mapVE f fn) (fmap (mapVE f) args)
    Lambda loc nm args stm -> Lambda loc nm (mapVArg f args) (mapVSt f stm)
    GetField loc e fld -> GetField loc (mapVE f e) fld
    Index loc e i -> Index loc (mapVE f e) (mapVE f i)
    Array loc es -> Array loc (fmap (mapVE f) es)
    ArrayRange loc a b -> ArrayRange loc (mapVE f a) (mapVE f b)
    Mapping loc flds -> Mapping loc [(fld, mapVE f e) | (fld, e) <- flds]
    _ -> unsafeCoerce e

