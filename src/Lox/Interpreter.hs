{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Lox.Interpreter (
    eval, apply, exec, bindThis, runProgram, run, printLox,
    (<=>)
    ) where

import Prelude hiding (init)

import Control.Exception (catch)
import Control.Concurrent.STM
import Control.Applicative
import Control.Arrow ((&&&), second)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad (unless, when)
import Data.Default (def)
import Data.Char (toLower)
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Function (on)
import Data.Traversable
import Text.Printf (printf)
import System.FilePath (joinPath, (<.>))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           Data.Vector ((!?))

import Lox.Syntax
import Lox.Analyse (isAssignedIn)
import Lox.Scanner (tokens)
import Lox.Parser (runParser, tokenStream, program)
import Lox.Optimise (fromParsed)
import qualified Lox.Core.Array as A

import Lox.Environment (
    Ref, declare, assign, resolve, newRef, writeRef, deref, readEnv, diffEnv,
    enterScope, enterScopeWith, inCurrentScope)
import Lox.Interpreter.Types

type BinaryFn = (LoxVal -> LoxVal -> LoxT LoxVal)

class Monad m => MonadLox m where
    printLox :: LoxVal -> m ()

instance MonadLox IO where
    printLox a = do
        i <- interpreter mempty mempty
        runLoxT (stringify a) i >>= either (error . show) T.putStrLn

instance MonadLox LoxT where
    printLox a = do
        s <- stringify a
        liftIO (T.putStrLn s)

run :: Env -> Program -> IO Value
run env program = interpreter [] env >>= runLoxT (runProgram program)

runProgram :: Program -> LoxT LoxVal
runProgram = foldM run def
    where run _ = exec

withStack :: (Described a, Located a) => (a -> LoxT LoxVal) -> a -> LoxT LoxVal
withStack f a = do
    let frame = stackFrame a
    stck <- gets stack
    modify' $ \s -> s { stack = frame : stck }
    r <- f a
    modify' $ \s -> s { stack = stck }
    return r

-- stack frame capturing machinery
exec :: Statement -> LoxT LoxVal
exec = withStack exec'

eval :: Expr -> LoxT LoxVal
eval = withStack eval'

exec' (Import loc mod mp) = do
    mod <- getModule mod
    -- in the context of imports, _ means import all
    p <- case mp of
           Just FromArray{} -> loxError "Cannot use array destructuring in import"
           Just p -> return p
           Nothing -> do flds <- liftIO . atomically $ readTVar (objectFields mod)
                         return (FromObject [(k, Name k) | (Str k) <- HM.keys flds])

    declareAndBind p (LoxObj mod)

exec' (Print _ e) = def <$ (eval e >>= printLox)

exec' (ExprS e) = {-# SCC "exec-expr" #-} eval' e

exec' (Throw _ e) = do v <- UserError <$> eval e
                       throwLox v

exec' (Try _ stm handlers) = exec stm `catchError` handleWith handlers

exec' (DefineFn loc v args body) = {-# SCC "exec-define-fun" #-} do
    declareVar v
    s <- get
    let fn = Lambda (Just v) args body
        val = LoxFn (Closure fn s)
    bindPattern (Name v) val
    return val

exec' (Define _ p e) =
    eval e >>= declareAndBind p

exec' (Declare _ v) = do
    env <- gets bindings >>= liftIO . declare v
    putEnv env
    return def

exec' (ClassDecl _ name _ msuper methods) = do
    declareVar name
    s <- get
    parent <- traverse (findSuperClass (bindings s)) msuper
    classId <- liftIO newSingleton
    base <- knownClass "Object"

    let fname x = Just (name <> x)
        constructors = [ Closure (Lambda (fname ".init") args body) s
                       | (Constructor args body) <- methods
                       ]
        statics      = [(n, Closure (Lambda (fname ("." <> n)) args body) s)
                       | (StaticMethod n args body) <- methods
                       ]
        instances    = [(n, Closure (Lambda (fname ("::" <> n)) args body) s)
                       | (InstanceMethod n args body) <- methods
                       ]

    -- verify constructors, and inherit from parent if not overriden.
    constructor <- case constructors of
                     []  -> return (parent >>= initializer)
                     [c] -> return $ Just c
                     _   -> loxError $ "Multiple constructors declared for " <> name

    let cls = LoxClass Class { classId = classId
                             , className = name
                             , superClass = parent <|> Just base
                             , initializer = constructor
                             , staticMethods = HM.fromList statics
                             , methods = HM.fromList instances
                             , protocols = maybe mempty protocols parent
                             }
    liftIO $ assign name cls (bindings s)
    return cls

    where
        findSuperClass :: Env -> VarName -> LoxT Class
        findSuperClass env name = do
            ma <- join <$> traverse (liftIO . deref) (resolve name env)
            case ma of
              Just (LoxClass c) -> return c
              Nothing -> loxError $ "Could not find super-class " <> name
              Just a -> loxError $ "Cannot inherit from " <> typeOf a

exec' (Block _ sts) = do
    env <- gets bindings
    mapM_ exec sts
    putEnv env
    return def

exec' (If _ condition a mb) = {-# SCC "exec-while" #-} do
    p <- truthy <$> eval condition
    if p
        then exec a
        else maybe (return def) exec mb

exec' (While _ condition body) = loop
    where
        loop = do done  <- (||) <$> (not . truthy <$> eval condition)
                                <*> runLoop (exec body)
                  if done then return def else loop

exec' (Break _)    = throwLoop LoxBreak
exec' (Continue _) = throwLoop LoxContinue
exec' (Return _ e) = eval e >>= returnVal

exec' (ForLoop loc minit mcond mpost body) = 
    case (minit, mcond, mpost) of
      (Just (Define _ (Name var) e)
        , (Just (Binary op (Var _ var') limit))
          , (Just (ExprS (Assign _ (Just Add) (LVar (Name var'')) (Literal _ (AInt step))))))
        | op `elem` [LessThan, LessThanEq]
        , var == var'
        , var == var''
        , var `notAssignedIn` body -> do
            start <- eval e
            env <- gets bindings
            case start of
              LoxInt i -> optimisedLoop AInt i step                var (comp op) limit
              LoxDbl i -> optimisedLoop ADbl i (fromIntegral step) var (comp op) limit
              _ -> asWhile
      _ -> asWhile
    where
        comp :: BinaryOp -> Atom -> Atom -> Bool
        comp LessThan = (<)
        comp LessThanEq = (<=)
        -- the optimised loop prevents most loop bookkeeping - preventing
        -- lookups of the loop var and function application of the post-cond.
        optimisedLoop :: forall a. Num a => (a -> Atom) -> a -> a -> VarName -> (Atom -> Atom -> Bool) -> Expr -> LoxT LoxVal
        optimisedLoop con i step var comp' limit = do
            old <- gets bindings
            env <- liftIO (declare var old)
            ref <- maybe (loxError "Could not find declared var") return
                   $ resolve var env
            putEnv env *> whileLoop con ref comp' limit i step <* putEnv old
            return LoxNil

        whileLoop :: forall a. Num a => (a -> Atom) -> Ref LoxVal -> (Atom -> Atom -> Bool) -> Expr -> a -> a -> LoxT ()
        whileLoop con ref comp' limit curr step = (go :: a -> LoxT ()) curr
            where
              go i = do
                p <- compareLimit (comp' (con i)) limit
                when p $ do broke <- runLoop (setRef con ref i >> exec body)
                            unless broke $ go (i + step)

        compareLimit :: (Atom -> Bool) -> Expr -> LoxT Bool
        compareLimit f limit = do lim <- eval limit
                                  case lim of
                                    (LoxLit x) -> return $ f x
                                    _        -> return False

        setRef :: forall a. (a -> Atom) -> Ref LoxVal -> a -> LoxT ()
        setRef con ref i = liftIO (writeRef ref (LoxLit $ con i))

        asWhile = {-# SCC "exec-for-loop-with-while" #-}
                  do
                  let while = While loc
                              (fromMaybe (Literal loc $ ABool True) mcond)
                              (Block loc (body : maybeToList mpost))
                      stms = maybeToList minit ++ [while]
                  env <- gets bindings
                  mapM_ exec stms
                  putEnv env
                  return LoxNil
    
exec' (Iterator _ p e body) = do
    (old, env) <- modEnv enterScope

    Stepper a next <- eval e >>= iterable

    loop env a next

    putEnv old
    return LoxNil
    where
        loop env a next = do
            mapM_ declareVar (patternVars p)
            (ma, a') <- next a
            case ma of
              Nothing -> return LoxNil
              Just curr -> do
                  bindPattern p curr
                  broke <- runLoop (exec body)
                  if broke then return LoxNil
                           else loop env a' next

handleWith :: [(VarName, Statement)] -> RuntimeError -> LoxT LoxVal
handleWith [] e = throwError e
handleWith (h:hs) e = do
    let (var, stm) = h
    old <- gets bindings
    err <- runtimeToLoxVal e
    declareAndBind (Name var) (LoxObj err)
    let cleanup = putEnv old
    (exec stm >> LoxNil <$ cleanup)
        `catchError` (\e -> cleanup >> handleWith hs e)

iterable :: LoxVal -> LoxT Stepper
iterable a = do
    fn <- lookupProtocol Iterable a
          >>= maybe (loxError $ "Cannot iterate over " <> typeOf a) return
    r <- apply fn [a]
    case r of
      (LoxIter it) -> return it
      wrong -> loxError $ "Iterator must return iterable, got " <> typeOf wrong

eval' :: Expr -> LoxT LoxVal

eval' (Fn _ lambda) = LoxFn . Closure lambda <$> get

eval' (GetField loc e field) = {-# SCC "eval-get-field" #-} do
    inst <- eval e
    case inst of
      (LoxClass cls) | field == "name" -> return (Txt $ className cls)
      (LoxClass cls) | Just sm <- HM.lookup field (staticMethods cls) -> return (LoxFn sm)
      (LoxObj Object{..}) | field == "class" -> return (LoxClass objectClass)
      (LoxObj obj) | field == "super" ->
          case superClass (objectClass obj) of
            Nothing -> loxError "No super-class"
            Just sup -> return (LoxObj obj{ objectClass = sup })
      _ -> do getter <- lookupProtocol Gettable inst >>= maybe (cannotGet inst) return
              apply getter [inst, Txt field]
    where
        cannotGet x = loxError $ "Cannot read fields of " <> typeOf x

eval' (Index loc e ei) = do
    o <- eval e
    i <- eval ei
    getter <- lookupProtocol Gettable o
              >>= maybe (cannotGet o) return
    apply getter [o, i] `catchError` onErr
    where
        cannotGet o = loxError $ "Cannot index " <> typeOf o
        onErr (RuntimeError _ FieldNotFound{}) = return LoxNil
        onErr e = throwError e

eval' (Literal _ a) = pure (LoxLit a)

eval' (Grouping _ e) = eval e

eval' (Negate _ e) = do
    v <- eval e
    case v of
      LoxNil   -> return LoxNil
      LoxInt n -> return $ LoxInt (negate n)
      LoxDbl n -> return $ LoxDbl (negate n)
      _        -> throwLox (TypeError "Number" v)

eval' (Not _ e) = do
    v <- not . truthy <$> eval e
    return (LoxBool v)

eval' (Binary And x y) = do
    a <- eval x
    if truthy a
        then eval y
        else return a

eval' (Binary Or x y) = do
    a <- eval x
    if truthy a
        then return a
        else eval y

eval' b@(Binary op x y) =
    case HM.lookup op binaryFns of
        Nothing -> loxError $ "Unknown operator: " <> T.pack (show op)
        Just f  -> do a' <- eval x
                      b' <- eval y
                      f a' b'

eval' (IfThenElse _ p x y) = do
    b <- truthy <$> eval p
    if b then eval x else eval y

eval' (Var _ v) = do
    env <- gets bindings
    ma <- maybe undef (liftIO . deref) (resolve v env)
    case ma of
        Nothing -> uninit
        Just v -> return v

    where undef = loxError $ "Undefined variable: " <> v
          uninit = loxError $ "Uninitialised variable: " <> v

eval' (Assign loc mop (LVar (Name v)) e) = {-# SCC "eval-assign-var" #-} do
    x <- eval e
    env <- gets bindings
    ref <- maybe undeclared return (resolve v env)
    case mop of
      Nothing -> liftIO (writeRef ref x) >> return x
      -- some things we can at least attempt to do in STM
      Just Add -> do r <- liftIO $ (Just <$> transactional ref x)
                                    `catch` (\(e :: LoxException) -> return Nothing)
                     maybe (nonTransactional Add ref x) return r
      -- other things we have to accept discontinuities
      Just op -> nonTransactional op ref x
    where undeclared = loxError $ "Cannot set undeclared variable: " <> v
          noop = loxError "Unknown operator"
          nonTransactional op ref x = do
            old <- fromMaybe LoxNil <$> liftIO (deref ref)
            fn <- maybe noop return $ HM.lookup op binaryFns
            x' <- fn old x
            liftIO (writeRef ref x')
            return x'
          -- TODO: extend to other operations than just Add
          transactional ref x = liftIO . atomically $ do
            old <- fromMaybe LoxNil <$> readTVar ref
            new <- addSTM old x
            writeTVar ref (Just new)
            return new

eval' (Assign loc Nothing (LVar p) e) = do
    x <- eval e
    bindPattern p x
    return x

eval' (Assign loc (Just op) (LVar _) _) =
    loxError "Cannot perform modifying assignment on a complex pattern"

eval' (Assign loc mop (SetIdx lhs idx) e) = {-# SCC "eval-assign-idx" #-} do
    target <- eval lhs
    k <- eval idx
    v <- eval e
    setter <- lookupProtocol Settable target
              >>= maybe (cannotSet target) return
    case mop of
      Nothing -> apply setter [target, k, v] >> return v
      Just op -> do
          getter <- lookupProtocol Gettable target
                    >>= maybe (cannotGet target) return
          old <- apply getter [target, k]
          fn <- maybe noop return $ HM.lookup op binaryFns
          v' <- fn old v
          apply setter [target, k, v']
          return v'
    where
        cannotSet a = loxError $ "Cannot assign fields on " <> typeOf a
        cannotGet a = loxError $ "Cannot read fields on " <> typeOf a
        noop        = loxError $ "Unknown operator"

eval' (Assign _ mop (Set lhs fld) e) = do
    v <- eval e
    o <- eval lhs
    setter <- lookupProtocol Settable o
              >>= maybe (cannotSet o) return
    v' <- value o v
    apply setter [o, LoxString fld, v']
    where
        cannotSet o = loxError $ "Cannot assign to " <> typeOf o
        cannotGet o = loxError $ "Cannot read fields of " <> typeOf o
        unknownOp   = loxError $ "Unknown operator"
        value o v | isNothing mop = return v
        value o v                 = do
            fn <- maybe unknownOp return (mop >>= flip HM.lookup binaryFns)
            getter <- lookupProtocol Gettable o
                      >>= maybe (cannotGet o) return
            old <- apply getter [o, LoxString fld]
            fn old v

eval' (Call _ callee args) = {-# SCC "eval-fun-call" #-} do
    e <- eval callee
    vals <- mapM eval args

    case e of
      LoxFn fn     -> apply fn vals
      LoxClass cls -> instantiate cls vals
      LoxObj o     -> do initing <- gets initialising
                         if not initing
                          then initOutsideInit
                          else LoxNil <$ init o vals
      _            -> loxError $ "Cannot call " <> typeOf e

    where
        initOutsideInit = loxError "Cannot call initializer outside of init()"

eval' (Array _ exprs) = mapM eval exprs >>= atomArray

eval' (ArrayRange loc expr expr') = do
    start <- eval expr
    end <- eval expr'
    case (start, end) of
      (LoxInt i, LoxInt j) -> let vs = fmap LoxInt [i .. j]
                               in atomArray vs
      _ -> throwLox (ArgumentError "[..]" ["Int", "Int"] [start, end])


eval' (Mapping loc pairs) = do
    cls <- knownClass "Object"
    vals <- mapM (sequence . second eval) pairs
    obj <- new cls vals
    return (LoxObj obj)

init :: Object -> [LoxVal] -> LoxT ()
init obj args = construct obj args
    where
        construct = case initializer (objectClass obj) of
            Nothing -> defaultConstr
            Just fn -> \o args -> do bound <- bindThis (LoxObj o) fn
                                     apply bound args
                                     return ()
                                          
        defaultConstr _ args = unless (null args) wrongArity
        wrongArity = loxError $ "Wrong number of arguments to constructor. Expected 0, got "
                              <> T.pack (show (length args))

lookupProtocol :: Protocol -> LoxVal -> LoxT (Maybe Callable)
lookupProtocol p a = do
    mcls <- classOf a
    case mcls of
      Nothing -> return Nothing
      Just cls -> return (classProtocol cls)
    where
        classProtocol cls = case HM.lookup p (protocols cls) of
                              Nothing -> superClass cls >>= classProtocol
                              Just fn -> return fn

classOf :: LoxVal -> LoxT (Maybe Class)
classOf x = case x of
  (LoxString _) -> Just <$> knownClass "String"
  (LoxArray _) -> Just <$> knownClass "Array"
  (LoxObj o) -> return (Just $ objectClass o)
  _ -> return Nothing

-- get a class we know to exist, from the base-environment
knownClass :: VarName -> LoxT Class
knownClass n = do
    env <- gets baseEnv
    mc <- liftIO $ join <$> traverse deref (resolve n env)
    case mc of
        Just (LoxClass c) -> return c
        Nothing -> loxError $ "Illegal state - " <> n <> " not defined"
        Just x  -> loxError $ "Illegal state - " <> n <> " defined as " <> typeOf x
  
instantiate :: Class -> [LoxVal] -> LoxT LoxVal
instantiate cls args = do
    obj <- new cls mempty
    initialise (init obj args)
    return $! LoxObj obj

bindThis :: LoxVal -> Callable -> LoxT Callable
bindThis this (BuiltIn n ar fn)
  = return $ BuiltIn n (\n -> ar (n + 1)) (\args -> fn (this : args))
bindThis this (Closure lam s)
  = do env <- liftIO (enterScopeWith [("this", this)] (bindings s))
       return $ Closure lam s { bindings = env }

apply :: Callable -> [LoxVal] -> LoxT LoxVal
apply fn args = do
    loc <- gets (maybe Unlocated snd . listToMaybe . stack)
    let frame = (fnName fn, loc)
    modify' $ \s -> s { stack = frame : stack s }
    r <- apply' fn args
    modify' $ \s -> s { stack = drop 1 (stack s) }
    return r

apply' :: Callable -> [LoxVal] -> LoxT LoxVal
apply' fn args | not (arity fn (length args)) = loxError "Wrong number of arguments"

apply' (BuiltIn _ _ fn) args = fn args

-- you can see why function calls are expensive: loads of setting up and
-- tearing down of the environment here.
apply' (Closure (Lambda _ (positional, rst) body) s) args = do
    old <- get -- snapshot the old environment
    -- restore the closed over fn environment, with the stack in the current state
    put s { initialising = initialising old, stack = stack old }

    -- Bind function arguments, and rest parameter if provided
    mapM_ declareVar ((positional ++ maybeToList rst) >>= patternVars)
    mapM_ (uncurry bindPattern) (zip positional args)
    case rst of
      Nothing -> pure ()
      Just p -> atomArray (drop (length positional) args) >>= bindPattern p

    -- actually run the function here
    r <- returning (exec body)

    put old
    return r

addAtoms :: BinaryFn
addAtoms (LoxArray (AtomArray a)) (LoxArray (AtomArray b)) = do
    ret <- liftIO $ A.concat a b
    return (LoxArray (AtomArray ret))
addAtoms (LoxString a) (LoxString b) = return $ LoxString (a <> b)
addAtoms a (LoxString b) = do s <- stringify a
                              return $ LoxString (s <> b)
addAtoms (LoxString a) b = do s <- stringify b
                              return $ LoxString (a <> s)
addAtoms a b = fromSTM (addSTM a b)

fromSTM :: STM a -> LoxT a
fromSTM stm = do
    r <- liftIO $ (Right <$> atomically stm) `catch` (return . Left)
    either throwError return r

{-# INLINE addSTM #-}
addSTM :: LoxVal -> LoxVal -> STM LoxVal
addSTM (LoxInt a) (LoxInt b) = return $ LoxInt (a + b)
addSTM (LoxNum a) (LoxNum b) = return $ LoxDbl (a + b)
addSTM a b = let msg = "Cannot add: " <> typeOf a <> " and " <> typeOf b
              in throwSTM (LoxError msg)

type DblFn = (Double -> Double -> Double)
type IntFn = (Int -> Int -> Int)
numericalFn :: Text -> DblFn -> IntFn -> BinaryFn
numericalFn _ _ f (LoxInt a) (LoxInt b) = return $ LoxInt (f a b)
numericalFn _ f _ (LoxNum a) (LoxNum b) = return $ LoxDbl (f a b)
numericalFn name _ _ a b = loxError $ mconcat [ "Cannot apply operator: "
                                              , typeOf a, " ", name, " ", typeOf b
                                              ]

stringify :: LoxVal -> LoxT Text
stringify LoxNil      = return "nil"
stringify (Txt t)     = return t
stringify (LoxBool b) = return . T.pack $ fmap toLower (show b)
stringify (LoxInt n)  = return . T.pack $ show n
stringify (LoxDbl n)  = return . T.pack $ printf "%f" n
stringify (LoxFn fn)  = return "<function>"
stringify (LoxClass cls) = return $ "<class " <> className cls <> ">"
stringify (LoxObj o) | Just fn <- HM.lookup "toString" (methods $ objectClass o) = do
    fn <- bindThis (LoxObj o) fn
    apply fn [] >>= stringify
stringify (LoxObj Object{..}) = do
    fieldMap <- liftIO $ atomically (readTVar objectFields)
    let fs = L.sortBy (compare `on` fst) . HM.toList $ fieldMap
    fs' <- mapM (sequence . second stringify) fs
    return $ mconcat
           $ "{"
             : L.intersperse "," [k <> ":" <> v | (Str k, v) <- fs']
             ++ ["}"]
stringify (LoxArray arr) = do
    vs <- readArray arr
    es <- V.toList <$> mapM quoteString vs
    return $ mconcat $ [ "[" ] ++ L.intersperse ", " es ++ [ "]" ]

quoteString :: LoxVal -> LoxT Text
quoteString (Txt t) = return ("\"" <> t <> "\"")
quoteString a = stringify a

binaryFns :: HashMap BinaryOp BinaryFn
binaryFns = HM.fromList
    [(Equals,        \a b -> LoxBool       <$> (a === b))
    ,(NotEquals,     \a b -> LoxBool . not <$> (a === b))
    ,(LessThan,      lb (== LT))
    ,(LessThanEq,    lb (/= GT))
    ,(GreaterThan,   lb (== GT))
    ,(GreaterThanEq, lb (/= LT))
    ,(Add,           addAtoms)
    ,(Subtract,      numericalFn "-" (-) (-))
    ,(Multiply,      numericalFn "*" (*) (*))
    ,(Divide,        divide)
    ,(Mod,           numericalFn "%" ((fromIntegral .) . mod `on` floor) mod)
    ,(Seq,           \a b -> a `seq` return b)
    ,(Exponent,      numericalFn "**" (**) (^))
    ]
    where
        lb f a b = LoxBool . maybe False f <$> a <=> b
        divide (LoxNum n) (LoxNum d) = return (LoxDbl (n/d))
        divide a          b          = loxError $ mconcat ["Cannot divide "
                                                          , typeOf a
                                                          , " by "
                                                          , typeOf b
                                                          ]

(===) :: LoxVal -> LoxVal -> LoxT Bool
(LoxObj a)   === (LoxObj b)   = return (a == b)
(LoxLit a)   === (LoxLit b)   = return (a == b)
(LoxClass a) === (LoxClass b) = return (a == b)
a            === b            = fmap (== Just EQ) (a <=> b)

(<=>) :: LoxVal -> LoxVal -> LoxT (Maybe Ordering)
(LoxLit a)                <=> (LoxLit b) = return $ Just (a `compare` b)
(LoxArray (AtomArray a))  <=> (LoxArray (AtomArray b))  = do
    as <- liftIO $ A.readArray a
    bs <- liftIO $ A.readArray b
    ords <- V.zipWithM (<=>) as bs
    return $ fromMaybe (Just (V.length as `compare` V.length bs))
           $ V.find (/= Just EQ) ords
a <=> b = return Nothing

initialise :: LoxT a -> LoxT a
initialise lox = do
    initing <- gets initialising
    modify' $ \s -> s { initialising = True }
    r <- lox
    modify' $ \s -> s { initialising = initing }
    return r

notAssignedIn :: VarName -> Statement -> Bool
notAssignedIn var stm = not (var `isAssignedIn` stm)

getModule :: ModuleIdentifier -> LoxT Object
getModule mod = do
    mods <- gets modules
    x <- HM.lookup mod <$> liftIO (readIORef mods)
    case x of
      Just Loading -> loxError ("Module cycle detected " <> showModId mod)
      Just (Loaded o) -> return o
      Nothing -> do
          liftIO $ modifyIORef' mods (HM.insert mod Loading)
          o <- loadModule mod `catchError` unblock mods
          liftIO $ modifyIORef' mods (HM.insert mod (Loaded o))
          return o
    where
        unblock ref e = do liftIO (modifyIORef' ref (HM.delete mod))
                           throwError e

loadModule :: ModuleIdentifier -> LoxT Object
loadModule m = do
    s <- get
    fn <- moduleToFileName m
    code <- liftIO (T.readFile fn) `catchError` fileNotFound
    let (ts, es) = tokens code
    unless (null es) $ loxError $ mconcat $
        ["Could not load ", showModId m, ": "]
        <> L.intersperse "\n" (fmap (T.pack . show) es)
    parsed <- case fst $ runParser program (tokenStream fn ts) of
                Left e -> loxError ("Could not parse " <> showModId m <> ", " <> T.pack (show e))
                Right r -> return r
    put (moduleInterpreter s)
    env <- runProgram (fromParsed parsed) >> gets bindings
    put s
    vals <- liftIO (envToFields $ diffEnv (bindings s) env)
    return (Object (modcls s) vals)
    where
        fileNotFound _ = loxError ("Could not find module: " <> showModId m)

showModId :: ModuleIdentifier -> Text
showModId = mconcat . L.intersperse "." . unModuleIdentifier

moduleToFileName :: ModuleIdentifier -> LoxT FilePath
moduleToFileName (ModuleIdentifier parts) =
    return $ joinPath (map T.unpack parts) <.> "lox"

declareAndBind :: Pattern VarName -> LoxVal -> LoxT LoxVal
declareAndBind p val = do
    mapM_ declareVar (patternVars p)
    bindPattern p val
    return val

declareVar :: VarName -> LoxT ()
declareVar v = gets bindings >>= (liftIO . declare v) >>= putEnv

bindPattern :: Pattern VarName -> LoxVal -> LoxT ()
bindPattern Ignore _ = return ()
bindPattern (Name v) x = gets bindings >>= liftIO . assign v x >> return ()
-- destructure objects (more precisely, gettable things)
bindPattern (FromObject []) _ = return ()
bindPattern (FromObject ps) x = do
    getter <- lookupProtocol Gettable x
               >>= maybe (loxError $ "Cannot destructure " <> typeOf x) return
    destructureObj getter ps x
    where
        destructureObj _ [] _ = return ()
        destructureObj fn ((k,p):ps) x = do
            x' <- apply fn [x, LoxString k]
            bindPattern p x'
            destructureObj fn ps x
-- destructure arrays
bindPattern (FromArray ps mp) (LoxArray (AtomArray arr)) = do
    forM_ (zip [0 ..] ps) $ \(i, p) -> do
        x <- liftIO (A.get i arr)
        bindPattern p x
    case mp of
      Nothing -> return ()
      Just p -> do xs <- V.toList <$> readArray (AtomArray arr)
                   rst <- atomArray (drop (length ps) xs)
                   bindPattern p rst
bindPattern (FromArray _ _) x = loxError $ "Cannot destructure " <> typeOf x <> " as array"
