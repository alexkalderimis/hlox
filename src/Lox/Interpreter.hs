{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Lox.Interpreter where

import Prelude hiding (init)

import Data.Fixed
import Control.Applicative
import Control.Arrow ((&&&), second)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad (when)
import Data.Char (toLower)
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.Maybe
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text (Text)
import Data.Function (on)
import Data.Traversable
import Text.Printf (printf)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Vector ((!?))

import Lox.Syntax
import Lox.Analyse (isAssignedIn)
import qualified Lox.Core.Array as A

import Lox.SeqEnv (
    declare, assign, resolve, newRef, writeRef, deref, enterScope, enterScopeWith, inCurrentScope)

type BinaryFn = (LoxVal -> LoxVal -> LoxT LoxVal)

data Interpreter = Interpreter
    { bindings :: !Env
    , warnings :: !(HS.HashSet Text)
    , initialising :: !Bool
    , stack :: ![StackFrame]
    , object :: !Class -- the base class all classes must inherit from
    , array :: !Class -- the class of arrays
    }

interpreter :: Env -> IO Interpreter
interpreter env = 
    Interpreter env mempty False [] <$> getClass "Object" <*> getClass "Array"
    where
        getClass c = do let mref = resolve c env
                        case mref of
                          Nothing -> return emptyClass
                          Just ref -> do ma <- deref ref
                                         case ma of
                                           Just (LoxClass cls) -> return cls
                                           _                   -> return emptyClass

-- we require two effects in the interpreter:
-- * statefulness of the bindings
-- * exception handing
type LoxT a = ExceptT LoxException (StateT Interpreter IO) a

class Monad m => MonadLox m where
    printLox :: LoxVal -> m ()

instance MonadLox IO where
    printLox a = do
        i <- interpreter mempty
        s <- runLoxT (stringify a) i
        either (error . show) putStrLn s

instance (MonadLox m) => MonadLox (StateT s m) where
    printLox a = lift (printLox a)

instance MonadLox m => MonadLox (ExceptT e m) where
    printLox a = lift (printLox a)

runLoxT :: LoxT a -> Interpreter -> LoxResult a
runLoxT lox s = do
    (ret, s') <- (`runStateT` s) $ runExceptT lox
    case ret of
      Left e -> Left <$> stackify s' e
      Right e -> return (Right e)

evalLoxT :: LoxT a -> Interpreter -> LoxResult (a, Interpreter)
evalLoxT lox s = do
    (ret, s') <- (`runStateT` s) $ runExceptT lox
    case ret of
      Left e -> stackify s' e >> return (Left e)
      Right v -> return (Right (v, s'))

stackify :: Interpreter -> LoxException -> IO LoxException
stackify s e = do
    case stack s of
      [] -> return e
      fs -> return (StackifiedError fs e)

run :: Env -> Program -> IO Value
run env program = interpreter env >>= runLoxT (runProgram program)

runProgram :: Program -> LoxT LoxVal
runProgram = foldM runStatement LoxNil
    where runStatement _ s = exec s

exec :: Statement -> LoxT LoxVal

exec (Print _ e) = do v <- eval e
                      printLox v
                      return LoxNil
exec (ExprS e) = {-# SCC "exec-expr" #-} eval e

exec (Throw loc e) = do v <- eval e
                        throwError (UserError loc v)

exec (Try loc stm handlers) = exec stm `catchError` handleWith handlers

exec (DefineFn loc v args body) = {-# SCC "exec-define-fun" #-} do
    exec (Declare loc v) -- functions can recurse, so declare before define
    eval (Assign loc Nothing (LVar v) (Lambda loc (Just v) args body))

exec (Define loc v e) = {-# SCC "exec-define" #-} do
    isBound <- gets (inCurrentScope v . bindings)
    when isBound $ 
        warn loc ("Binding for " <> v <> " already exists in the current scope")
    x <- eval e
    exec (Declare loc v)
    gets bindings >>= liftIO . assign v x
    return x

exec (Declare _ v) = do
    env <- gets bindings >>= liftIO . declare v
    putEnv env
    return LoxNil

exec (ClassDecl loc name _ msuper methods) = do
    exec (Declare loc name)
    env <- gets bindings
    parent <- sequence $ fmap (findSuperClass env) msuper
    classId <- liftIO newSingleton
    base <- gets object
    core <- gets (object &&& array)

    let constructors = [ Function (name <> ".init", loc) ns r body core env
                       | (Constructor (ns, r) body) <- methods]
        statics      = [(n, Function (name <> "." <> n, loc) ns r b core env)
                       | (StaticMethod n (ns,r) b) <- methods]
        instances    = [(n, Function (name <> "::" <> n, loc) ns r b core env)
                       | (InstanceMethod n (ns,r) b) <- methods]

    -- verify constructors
    constructor <- case constructors of
                     []  -> return (parent >>= initializer)
                     [c] -> return $ Just c
                     _   -> loxError loc $ "Multiple constructors declared for " <> T.unpack name

    let cls = LoxClass $ Class { classId = classId
                               , className = name
                               , superClass = parent <|> Just base
                               , initializer = constructor
                               , staticMethods = (HM.fromList statics)
                               , methods = (HM.fromList instances)
                               , protocols = mempty
                               , classLocation = loc
                               }
    liftIO $ assign name cls env
    return cls

    where
        findSuperClass env name = do
            ma <- maybe undef (liftIO . deref) (resolve name env)
            case ma of
              Just (LoxClass c) -> return c
              Nothing -> undef
              Just a -> loxError loc $ "Cannot inherit from " <> typeOf a
        undef = loxError loc $ "Could not find super-class " <> show name

exec (Block _ sts) = do
    (env, _) <- modEnv enterScope
    mapM_ exec sts
    putEnv env
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

exec (ForLoop loc minit mcond mpost body) = 
    case (minit, mcond, mpost) of
      (Just (Define _ var e)
        , (Just (Binary LessThanEq (Var _ var') limit))
          , (Just (ExprS (Assign _ (Just Add) (LVar var'') (Literal _ (LoxInt 1))))))
        | var == var' && var == var'' && var `notAssignedIn` body -> do
            start <- eval e
            limit <- eval limit
            case (start, limit) of
              (LoxInt i, LoxInt j) -> optimisedLoop i j var
              _ -> asWhile
      _ -> asWhile
    where
        loop = Literal loc (LoxBool True)
        -- the optimised loop prevents most loop bookkeeping, and the 
        -- 2 var lookups and one function application per loop that would be needed
        optimisedLoop i j var = do
            old <- gets bindings
            env <- liftIO (declare var old)
            ref <- maybe (loxError loc "Could not find declared var") return
                   $ resolve var env
            let it curr = do liftIO (writeRef ref (LoxInt curr))
                             exec body `catchError` catchContinue
            r <- (putEnv env >> (LoxNil <$ mapM_ it [i .. j]))
                    `catchError` catchBreak
            r <$ putEnv old
        catchContinue (LoxContinue l) = return LoxNil
        catchContinue e = throwError e
        catchBreak (LoxBreak l) = return LoxNil
        catchBreak e = throwError e
        asWhile = do
                  let while = While loc
                              (fromMaybe loop mcond)
                              (Block loc (body : maybeToList mpost))
                      stms = maybeToList minit ++ [while]
                  exec (Block loc stms)
    
exec (Iterator loc loopVar e body) = do
    (old, env) <- modEnv enterScope

    obj <- eval e
    Stepper a next <- iterable (sourceLoc e) obj
    env <- liftIO (declare loopVar env)
    putEnv env

    loop env next a

    putEnv old
    return LoxNil
    where
        loop env next a = do
            (ma, a') <- fromLoxResult (sourceLoc e) (next a)
            case ma of
              Nothing -> return LoxNil
              Just curr -> do
                  liftIO (assign loopVar curr env)
                  exec body
                  loop env next a'

fromLoxResult :: SourceLocation -> LoxResult a -> LoxT a
fromLoxResult loc r = do
    frames <- gets stack
    liftIO r >>= either (locateError loc . joinStack frames) return
    where
        joinStack :: [StackFrame] -> LoxException -> LoxException
        joinStack [] e = e
        joinStack fs e = StackifiedError fs e

handleWith :: [(VarName, Statement)] -> LoxException -> LoxT LoxVal
handleWith [] e = throwError e
handleWith (h:hs) e = do
    let (var, stm) = h
        stms = unpackBlock stm
    err <- fromException e
    old <- gets bindings
    env <- liftIO $ enterScopeWith [(var, err)] old
    putEnv env
    let cleanup = putEnv old
    (mapM_ exec stms >> return LoxNil <* cleanup)
        `catchError` (\e -> cleanup >> handleWith hs e)
    where
        unpackBlock (Block _ ss) = ss
        unpackBlock s = [s]

fromException :: LoxException -> LoxT LoxVal
fromException (UserError _ e) = return e
fromException (LoxError _ msg) = return (LoxString $ T.pack msg)
fromException e = throwError e

iterable :: SourceLocation -> LoxVal -> LoxT Stepper
iterable loc a = do
    fn <- lookupProtocol Iterable a
          >>= maybe (loxError loc $ "Cannot iterate over " <> typeOf a) return
    r <- apply loc fn [a]
    case r of
      (LoxIter it) -> return it
      wrong -> loxError loc $ "Iterator must return iterable, got " <> typeOf wrong

eval :: Expr -> LoxT LoxVal

eval (Lambda loc mname (names, rst) body) = {-# SCC "eval-lambda" #-} fmap LoxFn $
    let frame = (fromMaybe "Anonymous" mname, loc)
    in Function frame names rst body <$> (gets (object &&& array))
                                  <*> gets bindings

eval (GetField loc e field) = {-# SCC "eval-get-field" #-} do
    inst <- eval e
    case inst of
      (LoxClass cls) | field == "name" -> return (LoxString $ className cls)
      (LoxClass cls) | Just sm <- HM.lookup field (staticMethods cls) -> return (LoxFn sm)
      (LoxObj Object{..}) | field == "class" -> return (LoxClass objectClass)
      (LoxObj obj) | field == "super" -> do
          case superClass (objectClass obj) of
            Nothing -> loxError loc "No super-class"
            Just sup -> return (LoxObj obj{ objectClass = sup })
      _ -> do getter <- lookupProtocol Gettable inst
                        >>= maybe (cannotGet inst) return
              apply loc getter [inst, LoxString field]
                `catchError` locateError loc
    where
        cannotGet x = loxError loc $ "Cannot read fields of " <> typeOf x

eval (Index loc e ei) = {-# SCC "eval-index" #-} do
    o <- eval e
    i <- eval ei
    getter <- lookupProtocol Gettable o
              >>= maybe (cannotGet o) return
    apply loc getter [o, i] `catchError` onErr
    where
        cannotGet o = loxError loc $ "Cannot index " <> typeOf o
        onErr (FieldNotFound _ _) = return LoxNil
        onErr e = locateError loc e

eval (Literal _ a) = {-# SCC "eval-literal" #-} pure a

eval (Grouping _ e) = {-# SCC "eval-grouping" #-} eval e

eval (Negate loc e) = do
    v <- eval e
    case v of
      LoxInt n -> return $ LoxInt (negate n)
      LoxDbl n -> return $ LoxDbl (negate n)
      _        -> loxError loc $ "expected number, got " <> show v

eval (Not loc e) = do
    v <- truthy <$> eval e
    return (LoxBool v)

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

eval b@(Binary op x y) = {-# SCC "eval-binop" #-} do
    case operator of
        Nothing -> loxError (sourceLoc b) $ "Unknown operator: " <> show op
        Just f  -> do a' <- eval x
                      b' <- eval y
                      f a' b' `catchError` locateError (sourceLoc b)
    where operator = HM.lookup op binaryFns

eval (IfThenElse _ p x y) = do
    b <- truthy <$> eval p
    if b then eval x else eval y

eval (Var loc v) = {-# SCC "eval-var" #-} do
    env <- gets bindings
    ma <- maybe undef (liftIO . deref) (resolve v env)
    case ma of
        Nothing -> uninit
        Just v -> return v

    where undef = loxError loc $ "Undefined variable: " <> show v
          uninit = loxError loc $ "Uninitialised variable: " <> show v

eval (Assign loc mop (LVar v) e) = {-# SCC "eval-assign-var" #-} do
    x <- eval e
    env <- gets bindings
    ref <- maybe undeclared return (resolve v env)
    case mop of
      Nothing -> liftIO (writeRef ref x) >> return x
      Just op -> do old <- fromMaybe LoxNil <$> liftIO (deref ref)
                    fn <- maybe noop return $ HM.lookup op binaryFns
                    x' <- fn old x
                    liftIO (writeRef ref x')
                    return x'
    where undeclared = loxError loc $ "Cannot set undeclared variable: " <> show v
          noop = loxError loc $ "Unknown operator"

eval (Assign loc mop (SetIdx lhs idx) e) = {-# SCC "eval-assign-idx" #-} do
    target <- eval lhs
    k <- eval idx
    v <- eval e
    setter <- lookupProtocol Settable target
              >>= maybe (cannotSet target) return
    case mop of
      Nothing -> apply loc setter [target, k, v] >> return v
      Just op -> do
          getter <- lookupProtocol Gettable target
                    >>= maybe (cannotGet target) return
          old <- apply loc getter [target, k] `catchError` locateError loc
          fn <- maybe noop return $ HM.lookup op binaryFns
          v' <- fn old v
          apply loc setter [target, k, v']
          return v'
    where
        cannotSet a = loxError loc $ "Cannot assign fields on " <> typeOf a
        cannotGet a = loxError loc $ "Cannot read fields on " <> typeOf a
        noop = loxError loc $ "Unknown operator"

eval (Assign loc mop (Set lhs fld) e) = do
    v <- eval e
    o <- eval lhs
    case o of
      LoxObj Object{objectFields=fs} -> assignField fs v
      _  -> loxError loc ("Cannot assign to " <> typeOf o)
    where
        noop = loxError loc $ "Unknown operator"
        setFld fs v = liftIO (modifyIORef' fs (HM.insert fld v)) >> return v
        assignField fs v = case mop of
            Nothing -> setFld fs v
            Just op -> do old <- (HM.lookup fld <$> liftIO (readIORef fs))
                                  >>= maybe (throwError (FieldNotFound loc fld)) return
                          fn <- maybe noop return $ HM.lookup op binaryFns
                          fn old v >>= setFld fs

eval (Call loc callee args) = {-# SCC "eval-fun-call" #-} do
    e <- eval callee
    vals <- mapM eval args
    let frame = ("called at", loc) :: StackFrame
    
    inStack frame $ case e of
      LoxFn fn     -> inStack fn (apply loc fn vals)
      LoxClass cls -> inStack cls (instantiate loc cls vals)
      LoxObj o     -> do initing <- gets initialising
                         if not initing
                          then initOutsideInit
                          else inStack (objectClass o) $ LoxNil <$ init loc o vals
      _            -> loxError loc $ "Cannot call " <> typeOf e

    where
        initOutsideInit = loxError loc $ "Cannot call initializer outside of init()"
        inStack :: HasStackFrame o => o -> LoxT a -> LoxT a
        inStack o lox = do
            let frame = stackFrame o
            modify' $ \s -> s { stack = frame : stack s }
            r <- lox
            modify' $ \s -> s { stack = tail (stack s) }
            return r

eval (Array loc exprs) = mapM eval exprs >>= atomArray

eval (ArrayRange loc expr expr') = do
    cls <- gets array
    eval (Call loc (GetField loc (Literal loc $ LoxClass cls) "range")
                   [expr, expr'])

eval (Mapping loc pairs) = do
    cls <- gets object
    obj <- liftIO $ new cls
    vals <- mapM (sequence . second eval) pairs
    liftIO $ writeIORef (objectFields obj) (HM.fromList vals)
    return (LoxObj obj)

atomArray :: [LoxVal] -> LoxT LoxVal
atomArray as = LoxArray . AtomArray <$> liftIO (A.fromList LoxNil as)

init :: SourceLocation -> Object -> [LoxVal] -> LoxT ()
init loc obj args = construct obj args
    where
        construct = case initializer (objectClass obj) of
            Nothing -> defaultConstr
            Just fn -> (\o args -> do bound <- bindThis (LoxObj o) fn
                                      apply loc bound args
                                      return ())
                                          
        defaultConstr _ args = when (not $ null args) $ wrongArity 0
        wrongArity n = loxError loc $ unwords ["Wrong number of arguments to constructor."
                                              ,"Expected", show n
                                              ]


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
classOf (LoxObj o) = return (Just $ objectClass o)
classOf (LoxArray _) = Just <$> gets array
classOf _ = return Nothing

instantiate :: SourceLocation -> Class -> [LoxVal] -> LoxT LoxVal
instantiate loc cls args = do
    obj   <- liftIO (new cls)
    initialise (init loc obj args)
    return (LoxObj $ obj)

new :: Class -> IO Object
new cls = Object cls <$> newIORef HM.empty

bindThis :: LoxVal -> Callable -> LoxT Callable
bindThis this (BuiltIn n ar fn)
  = return $ BuiltIn n (\n -> ar (n + 1)) (\args -> fn (this : args))
bindThis this (Function n ns rst body core env)
  = Function n ns rst body core <$> liftIO (enterScopeWith [("this", this)] env)

apply :: SourceLocation -> Callable -> [LoxVal] -> LoxT LoxVal
apply loc fn args | not (arity fn (length args)) = loxError loc msg
    where
        msg = "Wrong number of arguments."

apply loc (BuiltIn n _ fn) args
  = fromLoxResult loc (fn args) `catchError` nameFunction
      where nameFunction (ArgumentError loc "" ts as) = throwError (ArgumentError loc n ts as)
            nameFunction e = throwError e

apply _ (Function sf names rst body core env) args = do
    old <- get -- snapshot the old environment

    -- setup the closed over environment
    let xs = zip names args
    slurp <- sequence . fmap sequence $
             [(v, atomArray (drop (length names) args)) 
             | v <- maybeToList rst
             ]
    env' <- liftIO $ enterScopeWith (zip names args <> slurp) env
    put old { bindings = env', object = fst core, array = snd core }
    --- unwrap blocks to avoid double scoping
    let sts = case body of Block _ sts -> sts
                           _ -> [body]

    -- actually run the function here
    r <- (LoxNil <$ mapM_ exec sts) `catchError` catchReturn

    -- restore the old environment, along with any new warnings
    ws <- gets warnings
    put old { warnings = ws }
    return r
    where
     catchReturn (LoxReturn _ v) = return v
     catchReturn (ArgumentError loc "" ts as) = throwError $ ArgumentError
                                                             loc
                                                             (fst sf)
                                                             ts
                                                             as
     catchReturn e               = throwError e

warn :: SourceLocation -> Text -> LoxT ()
warn loc msg = modify' $ \s -> s { warnings = HS.insert (locS <> msg) (warnings s) }
    where ((f1, l1, c1), (f2, l2, c2)) = range loc
          locS = mconcat ["(", T.pack (show l1), ",", T.pack (show c1), ")"
                         ," - (", T.pack (show l2), ",", T.pack (show c2)
                         ,") "
                         ]

truthy :: LoxVal -> Bool
truthy LoxNil      = False
truthy (LoxBool b) = b
truthy _           = True

addAtoms :: BinaryFn
addAtoms (LoxInt a) (LoxInt b) = return $ LoxInt (a + b)
addAtoms (LoxDbl a) (LoxDbl b) = return $ LoxDbl (a + b)
addAtoms (LoxInt a) (LoxDbl b) = return $ LoxDbl (fromIntegral a + b)
addAtoms (LoxDbl a) (LoxInt b) = return $ LoxDbl (a + fromIntegral b)
addAtoms (LoxArray (AtomArray a)) (LoxArray (AtomArray b)) = do
    ret <- liftIO $ A.concat a b
    return (LoxArray (AtomArray ret))
addAtoms (LoxString a) (LoxString b) = return $ LoxString (a <> b)
addAtoms a (LoxString b) = do s <- T.pack <$> stringify a
                              return $ LoxString (s <> b)
addAtoms (LoxString a) b = do s <- T.pack <$> stringify b
                              return $ LoxString (a <> s)
addAtoms a b = throw' ("Cannot add: " <> show a <> " and " <> show b)

type DblFn = (Double -> Double -> Double)
type IntFn = (Int -> Int -> Int)
numericalFn :: String -> DblFn -> IntFn -> BinaryFn
numericalFn _ _ f (LoxInt a) (LoxInt b) = return $ LoxInt (f a b)
numericalFn _ f _ (LoxDbl a) (LoxDbl b) = return $ LoxDbl (f a b)
numericalFn _ f _ (LoxInt a) (LoxDbl b) = return $ LoxDbl (f (fromIntegral a) b)
numericalFn _ f _ (LoxDbl a) (LoxInt b) = return $ LoxDbl (f a (fromIntegral b))
numericalFn name _ _ a b = throw' $ concat [ "Cannot ", name, ": "
                                         , show a, " and ", show b
                                         ]

stringify :: LoxVal -> LoxT String
stringify LoxNil = return "nil"
stringify (LoxString t) = return $ T.unpack t
stringify (LoxBool b) = return $ fmap toLower (show b)
stringify (LoxInt n) = return $ show n
stringify (LoxDbl n) = return $ printf "%f" n
stringify (LoxFn fn) = return "<function>"
stringify (LoxClass cls) = return $ "<class " <> T.unpack (className cls) <> ">"
stringify (LoxObj Object{..}) = do
    fs <- L.sortBy (compare `on` fst) . HM.toList <$> liftIO (readIORef objectFields)
    fs' <- mapM (sequence . second stringify) fs
    return $ concat
           $ ["{"]
             ++ L.intersperse "," [T.unpack k <> ":" <> v | (k, v) <- fs']
             ++ ["}"]
stringify (LoxArray arr) = do
    vs <- readArray arr
    es <- V.toList <$> mapM quoteString vs
    return $ concat [ "["
                    , L.intercalate ", " es
                    , "]"
                    ]

quoteString :: LoxVal -> LoxT String
quoteString s@LoxString{} = fmap (\s -> '"' : s <> "\"") (stringify s)
quoteString a = stringify a

isInteger :: Double -> Bool
isInteger d = d == fromIntegral (floor d)

binaryFns :: HashMap BinaryOp BinaryFn
binaryFns = HM.fromList
    [(Equals,        (\a b -> LoxBool       <$> (a === b)))
    ,(NotEquals,     (\a b -> LoxBool . not <$> (a === b)))
    ,(LessThan,      lb (== LT))
    ,(LessThanEq,    lb (/= GT))
    ,(GreaterThan,   lb (== GT))
    ,(GreaterThanEq, lb (/= LT))
    ,(Add,           addAtoms)
    ,(Subtract,      numericalFn "subtract" (-) (-))
    ,(Multiply,      numericalFn "multiply" (*) (*))
    ,(Divide,        divide)
    ,(Mod,           numericalFn "mod" ((fromIntegral .) . mod `on` floor) mod)
    ,(Seq,           (\a b -> a `seq` return b))
    ]
    where lb f a b = LoxBool . f <$> a <=> b
          divide (LoxDbl n) (LoxDbl d) = return (LoxDbl (n/d))
          divide (LoxInt n) (LoxInt d) = let f = (/) `on` fromIntegral
                                          in return (LoxDbl (f n d))
          divide (LoxInt n) (LoxDbl d) = return (LoxDbl $ fromIntegral n / d)
          divide (LoxDbl n) (LoxInt d) = return (LoxDbl $ n / fromIntegral d)
          divide a          b          = throw' $ concat ["Cannot divide "
                                                         , typeOf a
                                                         , " by "
                                                         , typeOf b
                                                         ]

(===) :: LoxVal -> LoxVal -> LoxT Bool
(LoxObj a)   === (LoxObj b)   = return (a == b)
(LoxClass a) === (LoxClass b) = return (a == b)
a            === b            = fmap (== EQ) (a <=> b)

(<=>) :: LoxVal -> LoxVal -> LoxT Ordering
LoxNil        <=> LoxNil = return EQ
LoxNil        <=> _      = return LT
_             <=> LoxNil = return GT
(LoxBool a)   <=> (LoxBool b)   = return (a `compare` b)
(LoxDbl a)    <=> (LoxDbl b)    = return (a `compare` b)
(LoxInt a)    <=> (LoxInt b)    = return (a `compare` b)
(LoxDbl a)    <=> (LoxInt b)    = return (a `compare` fromIntegral b)
(LoxInt a)    <=> (LoxDbl b)    = return (fromIntegral a `compare` b)
(LoxString a) <=> (LoxString b) = return (a `compare` b)
(LoxArray a)  <=> (LoxArray b)  = do
    as <- readArray a
    bs <- readArray b
    ords <- V.zipWithM (<=>) as bs
    return $ fromMaybe (V.length as `compare` V.length bs) $ V.find (/= EQ) ords
a <=> b = throw' $ "Cannot compare " <> typeOf a <> " and " <> typeOf b

throw' :: String -> LoxT a
throw' = throwError . LoxError Unlocated

loxError :: SourceLocation -> String -> LoxT a
loxError loc msg = throwError (LoxError (simplify loc) msg)

initialise :: LoxT a -> LoxT a
initialise lox = do
    initing <- gets initialising
    modify' $ \s -> s { initialising = True }
    r <- lox
    modify' $ \s -> s { initialising = initing }
    return r

putEnv :: Env -> LoxT ()
putEnv env = modify' $ \s -> s { bindings = env }

-- returns (old, new)
modEnv :: (Env -> Env) -> LoxT (Env, Env)
modEnv f = do old <- gets bindings
              let new = f old
              (old, new) <$ putEnv new

locateError :: SourceLocation -> LoxException -> LoxT a
locateError loc (LoxError Unlocated e) = loxError loc e
locateError loc (ArgumentError NativeCode n ts as)
  = throwError (ArgumentError (simplify loc) n ts as)
locateError _  e                      = throwError e

notAssignedIn :: VarName -> Statement -> Bool
notAssignedIn var stm = not (var `isAssignedIn` stm)
