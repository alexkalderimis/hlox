{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lox.Interpreter where

import Data.IORef
import Control.Arrow (second)
import Control.Applicative
import Data.Char (toLower)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Monoid
import Data.Maybe (isJust)
import Data.Text (Text)
import System.Clock
import Data.Traversable
import qualified Data.List as L
import qualified Data.Text as T

import Lox.Syntax
import Lox.Environment (
    declare, assign, resolve, deref, enterScope, enterScopeWith, inCurrentScope)

type Value = Either LoxExecption Atom
type BinaryFn = (Atom -> Atom -> LoxT Atom)

data Interpreter = Interpreter
    { bindings :: !Env
    , warnings :: !(HS.HashSet Text)
    }

data LoxExecption = LoxError SourceLocation String
                  | FieldNotFound SourceLocation VarName
                  | LoxReturn SourceLocation Atom
                  | LoxBreak SourceLocation
                  | LoxContinue SourceLocation
                  deriving (Show, Eq)

-- we require two effects in the interpreter:
-- * statefulness of the bindings
-- * exception handing
type LoxT a = StateT Interpreter (ExceptT LoxExecption IO) a

class Monad m => MonadLox m where
    printLox :: Atom -> m ()

instance MonadLox IO where
    printLox a = do
        s <- runLoxT (stringify a) (Interpreter mempty mempty)
        either (error . show) putStrLn s

instance (MonadLox m) => MonadLox (StateT s m) where
    printLox a = lift (printLox a)

instance MonadLox m => MonadLox (ExceptT e m) where
    printLox a = lift (printLox a)

runLoxT :: LoxT a -> Interpreter -> IO (Either LoxExecption a)
runLoxT lox s = runExceptT $ evalStateT lox s

evalLoxT :: LoxT a -> Interpreter -> IO (Either LoxExecption (a, Interpreter))
evalLoxT lox s = runExceptT $ runStateT lox s

run :: Env -> Program -> IO Value
run env program = runLoxT (runProgram program) (Interpreter env mempty)

runProgram :: Program -> LoxT Atom
runProgram = foldM runStatement LoxNil
    where runStatement _ s = exec s

exec :: -- (MonadError String m, Monad m, MonadLox m) =>
        Statement -> LoxT Atom
exec (Print _ e) = do v <- eval e
                      printLox v
                      return LoxNil
exec (ExprS e) = eval e
exec (DefineFn loc v ns body) = do
    exec (Declare loc v) -- functions can recurse, so declare before define
    eval (Assign loc (LVar v) (Lambda loc ns body))
exec (Define loc v e) = do
    isBound <- gets (inCurrentScope v . bindings)
    when isBound $ 
        warn loc ("Binding for " <> v <> " already exists in the current scope")
    x <- eval e
    exec (Declare loc v)
    gets bindings >>= liftIO . assign v x
    return x
exec (Declare _ v) = do
    env <- gets bindings >>= liftIO . declare v
    modify' $ \s -> s { bindings = env }
    return LoxNil
exec (ClassDecl loc name methods) = do
    exec (Declare loc name)
    env <- gets bindings
    let ms = HM.fromList [(nm, Function args body env) | (nm, args, body) <- methods]
        cls = Class name ms
    liftIO $ assign name (LoxClass cls) env
    return (LoxClass cls)
exec (Block _ sts) = do
    env <- gets bindings
    modify' $ \s -> s { bindings = enterScope (bindings s) }
    mapM_ exec sts
    modify' $ \s -> s { bindings = env  }
    return LoxNil
exec (If _ condition a mb) = do
    p <- truthy <$> eval condition
    if p
        then exec a
        else maybe (return LoxNil) exec mb
exec (While _ condition body) = loop
    where
        loop = do p <- truthy <$> eval condition
                  if p then do broke <- (False <$ exec body) `catchError` loopH
                               if broke then return LoxNil else loop
                       else return LoxNil
        loopH (LoxBreak _)    = return True
        loopH (LoxContinue _) = return False
        loopH e           = throwError e

exec (Break l)    = throwError (LoxBreak l)
exec (Continue l) = throwError (LoxContinue l)
exec (Return l e) = throwError =<< (LoxReturn l <$> eval e)

eval :: Expr -> LoxT Atom

eval (Lambda _ args body) = LoxFn . Function args body <$> gets bindings

eval (GetField loc e field) = do
    inst <- eval e
    case inst of
      (LoxObj (Object cls fs)) -> do
          hm <- liftIO (readIORef fs)
          case HM.lookup field hm of
            Nothing -> gets bindings >>= getMethod cls inst
            Just v -> return v
      _ -> throwError (LoxError loc $ "Cannot access field of " <> typeOf inst)
    where
        fieldNotFound = throwError $ FieldNotFound loc field
        getMethod (Class _ meths) inst env = do
            case HM.lookup field meths of
              Nothing -> fieldNotFound
              Just fn -> bind [("this", inst)] fn

eval (Literal _ a) = pure a

eval (Grouping _ e) = eval e

eval (Negate loc e) = do
    v <- eval e
    case v of
        LoxNum n -> return $ LoxNum (negate n)
        _        -> throwError . LoxError loc $ "expected number, got " <> show v

eval (Not loc e) = do
    v <- eval e
    case v of
        LoxBool b -> return (LoxBool $ not b)
        _         -> throwError . LoxError loc $ "Expected boolean, got " <> show v

eval (Binary And x y) = do
    a <- eval x
    if truthy a
        then eval y
        else return a

eval (Binary Or x y) = do
    a <- eval x
    if truthy a
        then return a
        else eval y

eval b@(Binary op x y) = do
    case HM.lookup op binaryFns of
        Nothing -> throwError . LoxError (sourceLoc b)
                              $ "Unknown operator: " <> show op
        Just f  -> do a <- eval x
                      b <- eval y
                      f a b `catchError` locateError
    where locateError (LoxError Unlocated e) = throwError (LoxError (sourceLoc b) e)
          locateError e                      = throwError e

eval (IfThenElse _ p x y) = do
    b <- truthy <$> eval p
    if b then eval x else eval y

eval (Var loc v) = do
    env <- gets bindings
    ma <- maybe (throwError undef) (liftIO . deref) (resolve v env)
    case ma of
        Nothing -> throwError uninit
        Just v -> return v

    where undef = LoxError loc $ "Undefined variable: " <> show v
          uninit = LoxError loc $ "Uninitialised variable: " <> show v

eval (Assign loc (LVar v) e) = do
    x <- eval e
    env <- gets bindings
    declared <- liftIO (assign v x env)
    if not declared
        then throwError undeclared
        else return x
    where undeclared = LoxError loc $ "Cannot set undeclared variable: " <> show v
eval (Assign loc (Set lhs fld) e) = do
    v <- eval e
    o <- eval lhs
    case o of
      LoxObj (Object _ fs) -> assignField fs v >> return v
      _                    -> throwError $ LoxError loc ("Cannot assign to " <> typeOf o)
    where
        assignField fs v = liftIO (modifyIORef fs (HM.insert fld v))

eval (Call loc callee args) = do
    e <- eval callee
    case e of
        LoxFn fn -> mapM eval args >>= apply loc fn
        LoxClass cls -> mapM eval args >>= instantiate loc cls
        _        -> throwError . LoxError loc $ "Cannot call " <> typeOf e

instantiate :: SourceLocation -> Class -> [Atom] -> LoxT Atom
instantiate loc cls args = do
    construct <- getConstructor cls (length args)
    flds <- (liftIO . newIORef) =<< construct args
    return (LoxObj $ Object cls flds)
    where
        getConstructor _ 0 = return (\[] -> return HM.empty)
        getConstructor _ _ = throwError (LoxError loc $ "Cannot find constructor")

bind :: [(VarName, Atom)] -> Callable -> LoxT Atom
bind xs (BuiltIn _ _) = throwError $ LoxError Unlocated "Cannot bind native functions, yet"
bind xs (Function ns body env) = LoxFn . Function ns body <$> liftIO (enterScopeWith xs env)

apply :: SourceLocation -> Callable -> [Atom] -> LoxT Atom
apply loc fn args | arity fn /= length args = throwError (LoxError loc msg)
    where
        msg = concat ["Wrong number of arguments. "
                     , show (arity fn)
                     ,"arguments required"
                     ]

apply loc (BuiltIn _ fn) args = liftIO (fn args)
                              >>= either (throwError . LoxError loc) return 

apply _ (Function names body env) args = do
   old <- gets bindings
   let xs = zip names args
   env' <- liftIO (enterScopeWith (zip names args) env)
   modify' $ \s -> s { bindings = env' }
   --- unwrap blocks to avoid double scoping
   let sts = case body of Block _ sts -> sts
                          _ -> [body]
   r <- (LoxNil <$ mapM_ exec sts) `catchError` catchReturn
   modify' $ \s -> s { bindings = old }
   return r
   where
    catchReturn (LoxReturn _ v) = return v
    catchReturn e               = throwError e

warn :: SourceLocation -> Text -> LoxT ()
warn loc msg = modify' $ \s -> s { warnings = HS.insert (locS <> msg) (warnings s) }
    where ((f1, l1, c1), (f2, l2, c2)) = range loc
          locS = mconcat ["(line ", T.pack (show l1), ",", T.pack (show c1)
                         ," .. line ", T.pack (show l2), ",", T.pack (show c2)
                         ,") "
                         ]

builtins :: IO Env
builtins = enterScopeWith vals mempty
    where vals = [("clock", LoxFn (BuiltIn 0 clock))]

clock :: NativeFn
clock _ = fmap (Right . LoxNum . (/ 1e9) . realToFrac . toNanoSecs)
        $ getTime Realtime

truthy :: Atom -> Bool
truthy LoxNil      = False
truthy (LoxBool b) = b
truthy _           = True

typeOf :: Atom -> String
typeOf (LoxString _) = "String"
typeOf (LoxNum _) = "Number"
typeOf (LoxBool _) = "Boolean"
typeOf LoxNil = "nil"
typeOf (LoxFn _) = "Function"
typeOf (LoxClass _) = "Class"

addAtoms :: BinaryFn
addAtoms (LoxString a) (LoxString b) = return $ LoxString (a <> b)
addAtoms (LoxNum a) (LoxNum b) = return $ LoxNum (a + b)
addAtoms a (LoxString b) = do s <- T.pack <$> stringify a
                              return $ LoxString (s <> b)
addAtoms (LoxString a) b = do s <- T.pack <$> stringify b
                              return $ LoxString (a <> s)
addAtoms a b = throw' ("Cannot add: " <> show a <> " and " <> show b)

numericalFn :: String -> (Double -> Double -> Double) -> BinaryFn
numericalFn _ f (LoxNum a) (LoxNum b) = return $ LoxNum (f a b)
numericalFn name _ a b = throw' $ concat [ "Cannot ", name, ": "
                                         , show a, " and ", show b
                                         ]

stringify :: Atom -> LoxT String
stringify LoxNil = return "nil"
stringify (LoxString t) = return $ T.unpack t
stringify (LoxBool b) = return $ fmap toLower (show b)
stringify (LoxNum n) = return $
    let s = show n in if isInteger n then takeWhile (/= '.') s else s
stringify (LoxFn fn) = return "<function>"
stringify (LoxClass (Class nm _)) = return $ "<class " <> T.unpack nm <> ">"
stringify (LoxObj (Object (Class n _) flds)) = do
    fs <- HM.toList <$> liftIO (readIORef flds)
    fs' <- mapM (sequence . second stringify) fs
    return $ concat
           $ ["<", T.unpack n, " "]
             ++ L.intersperse " " [T.unpack k <> "=" <> v | (k, v) <- fs']
             ++ [">"]

isInteger :: Double -> Bool
isInteger d = d == fromInteger (floor d)

binaryFns :: HashMap BinaryOp BinaryFn
binaryFns = HM.fromList
    [(Equals,        (lb .) . (==))
    ,(NotEquals,     (lb .) . (/=))
    ,(LessThan,      (lb .) . (<))
    ,(LessThanEq,    (lb .) . (<=))
    ,(GreaterThan,   (lb .) . (>))
    ,(GreaterThanEq, (lb .) . (>=))
    ,(Add,           addAtoms)
    ,(Subtract,      numericalFn "subtract" (-))
    ,(Multiply,      numericalFn "multiply" (*))
    ,(Divide,        numericalFn "divide" (/))
    ,(Mod,           numericalFn "mod" (\a b -> fromInteger(floor a `mod` floor b)))
    ,(Seq,           (\a b -> a `seq` return b))
    ]
    where lb = return . LoxBool

throw' :: String -> LoxT a
throw' = throwError . LoxError Unlocated
