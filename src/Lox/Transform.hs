{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Lox.Transform where

import Control.Monad.State.Strict
import Data.Data (Data, gmapM)
import Data.Generics.Aliases
import Data.Maybe
import Data.Bifunctor
import qualified Data.HashMap.Strict as HM

import Lox.Syntax

type ReplaceM = State (Int, HM.HashMap VarName Int)

type Replaced = [Statement' Variable Atom]

type Variable = (VarName, Int)
type St = Statement' Variable Atom
type Exp = Expr' Variable Atom
type Meth = Method' Variable Atom
type LV = LVal' Variable Atom
type Args = Arguments' Variable

renameVars :: Parsed -> Replaced
renameVars p = evalState (renameVarsM p') (0, HM.empty)
    where p' = fmap (first (\t -> (t, 0))) p

renameVarsM :: [St] -> ReplaceM [St]
renameVarsM = mapM handleStatement

replaceName :: Variable -> ReplaceM Variable
replaceName (name,_) = do
    (n, m) <- get
    put (n + 1, HM.insert name n m)
    return (name, n)

declareThis :: ReplaceM ()
declareThis = void $ replaceName ("this", 0)

bindArgs :: Args -> ReplaceM Args
bindArgs (names, mname) = (,) <$> mapM handlePattern names
                              <*> mapM handlePattern mname

scoped :: ReplaceM a -> ReplaceM a
scoped ra = do
    s <- get
    a <- ra
    put s -- discard changes caused by ra
    return a

getSubstitute :: Variable -> ReplaceM Variable
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

handleStatement (Define loc p e) = do
    e' <- handleExpr e
    p' <- handlePattern p
    return (Define loc p' e')

handleStatement (ClassDecl loc name var super ms) = do
    var' <- replaceName var
    super' <- traverse getSubstitute super
    ms' <- mapM handleMethod ms
    return (ClassDecl loc name var' super' ms')

handleStatement (Iterator loc p expr stm) = do
    expr' <- handleExpr expr
    p' <- handlePattern p
    stm' <- scoped (handleStatement stm)
    return (Iterator loc p' expr' stm')

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
        f name block = scoped $ (,) <$> replaceName name <*> handleStatement block

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
handleMethod (StaticMethod nm args stm) = scoped $
    StaticMethod nm <$> bindArgs args <*> handleStatement stm
handleMethod (InstanceMethod nm args stm) = scoped $ do
    declareThis
    InstanceMethod nm <$> bindArgs args <*> handleStatement stm

handleExpr :: Exp -> ReplaceM Exp

handleExpr (Var loc name) = do
    name' <- getSubstitute name
    return (Var loc name')

-- name does not need substitution here as it is for stack traces
handleExpr (Fn loc (Lambda mname args stm)) = do
    (args', stm') <- scoped ((,) <$> bindArgs args <*> handleStatement stm)
    return (Fn loc (Lambda mname args' stm'))

handleExpr e = gmapM f e
    where 
          f :: forall d. Data d => d -> ReplaceM d
          f = mkM handleStatement `extM` handleExpr `extM` handleLVar

handleLVar :: LV -> ReplaceM LV
handleLVar (LVar p) = LVar <$> handlePattern p
handleLVar (Set e name) = do
    e' <- handleExpr e
    return (Set e' name)
handleLVar (SetIdx l r) = SetIdx <$> handleExpr l <*> handleExpr r

handlePattern :: Pattern Variable -> ReplaceM (Pattern Variable)
handlePattern Ignore = return Ignore
handlePattern (Name n) = Name <$> getSubstitute n
handlePattern (FromArray ps mp) = FromArray <$> mapM handlePattern ps <*> mapM handlePattern mp
handlePattern (FromObject ps) = FromObject <$> mapM (\(k,p) -> (k,) <$> handlePattern p) ps
