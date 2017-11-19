{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Lox.Interpreter where

import Prelude hiding (init)

import Control.Exception (catch)
import Control.Concurrent.STM
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
    declare, assign, resolve, newRef, writeRef, deref, readEnv, diffEnv,
    enterScope, enterScopeWith, inCurrentScope)
import Lox.Interpreter.Types

type BinaryFn = (LoxVal -> LoxVal -> LoxT LoxVal)

class Monad m => MonadLox m where
    printLox :: LoxVal -> m ()

instance MonadLox IO where
    printLox a = do
        i <- interpreter mempty mempty
        runLoxT (stringify a) i >>= either (error . show) putStrLn

instance MonadLox LoxT where
    printLox a = do
        s <- stringify a
        liftIO (putStrLn s)

run :: Env -> Program -> IO Value
run env program = interpreter [] env >>= runLoxT (runProgram program)

runProgram :: Program -> LoxT LoxVal
runProgram = foldM runStatement LoxNil
    where runStatement _ s = exec s

exec :: Statement -> LoxT LoxVal

exec (Import loc mod var) = do
    mod <- getModule loc mod
    exec (Declare loc var)
    gets bindings >>= liftIO . assign var (LoxObj mod)
    return LoxNil

exec (Print _ e) = do v <- eval e
                      printLox v
                      return LoxNil
exec (ExprS e) = {-# SCC "exec-expr" #-} eval e

exec (Throw loc e) = do v <- eval e
                        throwError (UserError loc v)

exec (Try loc stm handlers) = exec stm `catchError` handleWith handlers

exec (DefineFn loc v args body) = {-# SCC "exec-define-fun" #-} do
    exec (Declare loc v) -- functions can recurse, so declare before define
    eval (Assign loc Nothing (LVar (Name v)) (Lambda loc (Just v) args body))

exec (Define loc p e) = {-# SCC "exec-define" #-} do
    x <- eval e
    mapM_ (exec . Declare loc) (patternVars p)
    bindPattern loc p x
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

exec (If _ condition a mb) = {-# SCC "exec-while" #-} do
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
      (Just (Define _ (Name var) e)
        , (Just (Binary op (Var _ var') limit))
          , (Just (ExprS (Assign _ (Just Add) (LVar (Name var'')) (Literal _ (LoxInt 1))))))
        | op `elem` [LessThan, LessThanEq]
        , var == var'
        , var == var''
        , var `notAssignedIn` body -> do
            start <- eval e
            case start of
              LoxInt i -> optimisedLoop i var (comp op) limit
              _ -> asWhile
      _ -> asWhile
    where
        comp LessThan = (<)
        comp LessThanEq = (<=)
        loop = Literal loc (LoxBool True)
        -- the optimised loop prevents most loop bookkeeping - preventing
        -- lookups of the loop var and function application of the post-cond.
        optimisedLoop i var comp' limit = do
            old <- gets bindings
            env <- liftIO (declare var old)
            ref <- maybe (loxError loc "Could not find declared var") return
                   $ resolve var env
            putEnv env *> whileLoop ref comp' limit i <* putEnv old

        {-# INLINE runLoop #-}
        runLoop ma = (False <$ ma) `catchError` catchLoop

        {-# INLINE whileLoop #-}
        whileLoop ref comp' limit curr = do
            p <- compareLimit (comp' curr) limit
            if not p
              then return LoxNil
              else do broke <- runLoop (setRef ref curr >> exec body)
                      if broke then return LoxNil else (whileLoop ref comp' limit (curr + 1))

        {-# INLINE compareLimit #-}
        compareLimit f limit = do 
            limit' <- eval limit
            case limit' of
              LoxNil -> return True
              LoxInt j -> return (f j)
              LoxDbl j -> return (f $ round j)
              _ -> loxError (sourceLoc limit)
                            $ "cannot compare " <> typeOf limit'

        {-# INLINE setRef #-}
        setRef ref i = liftIO (writeRef ref (LoxInt i))

        -- did we break?
        catchLoop (LoxContinue l) = return False
        catchLoop (LoxBreak l) = return True
        catchLoop e            = throwError e

        asWhile = {-# SCC "exec-for-loop-with-while" #-}
                  do
                  let while = While loc
                              (fromMaybe loop mcond)
                              (Block loc (body : maybeToList mpost))
                      stms = maybeToList minit ++ [while]
                  exec (Block loc stms)
    
exec (Iterator loc p e body) = do
    (old, env) <- modEnv enterScope

    obj <- eval e
    Stepper a next <- iterable (sourceLoc e) obj

    loop env next a

    putEnv old
    return LoxNil
    where
        loop env next a = do
            mapM_ (exec . Declare loc) (patternVars p)
            (ma, a') <- fromLoxResult (sourceLoc e) (next a)
            case ma of
              Nothing -> return LoxNil
              Just curr -> do
                  bindPattern loc p curr
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
    v <- not . truthy <$> eval e
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

eval (Assign loc mop (LVar (Name v)) e) = {-# SCC "eval-assign-var" #-} do
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
    where undeclared = loxError loc $ "Cannot set undeclared variable: " <> show v
          noop = loxError loc $ "Unknown operator"
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

eval (Assign loc Nothing (LVar p) e) = do
    x <- eval e
    bindPattern loc p x
    return x

eval (Assign loc (Just op) (LVar _) _) = do
    loxError loc "Cannot perform modifying assignment on a complex pattern"

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
    setter <- lookupProtocol Settable o
              >>= maybe (cannotSet o) return
    v' <- value o v
    apply loc setter [o, LoxString fld, v']
    where
        cannotSet o = loxError loc ("Cannot assign to " <> typeOf o)
        cannotGet o = loxError loc ("Cannot read fields of " <> typeOf o)
        unknownOp = loxError loc $ "Unknown operator"
        value o v | mop == Nothing = return v
        value o v = do
            fn <- maybe unknownOp return (mop >>= flip HM.lookup binaryFns)
            getter <- lookupProtocol Gettable o
                      >>= maybe (cannotGet o) return
            old <- apply loc getter [o, LoxString fld]
                    `catchError` locateError loc
            fn old v

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
    liftIO $ atomically $ writeTVar (objectFields obj) (HM.fromList vals)
    return (LoxObj obj)

atomArray :: [LoxVal] -> LoxT LoxVal
atomArray = fmap (LoxArray . AtomArray) . liftIO . A.fromList

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
new cls = Object cls <$> newTVarIO HM.empty

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
addAtoms (LoxArray (AtomArray a)) (LoxArray (AtomArray b)) = do
    ret <- liftIO $ A.concat a b
    return (LoxArray (AtomArray ret))
addAtoms (LoxString a) (LoxString b) = return $ LoxString (a <> b)
addAtoms a (LoxString b) = do s <- T.pack <$> stringify a
                              return $ LoxString (s <> b)
addAtoms (LoxString a) b = do s <- T.pack <$> stringify b
                              return $ LoxString (a <> s)
addAtoms a b = fromSTM (addSTM a b)

fromSTM :: STM a -> LoxT a
fromSTM stm = do
    r <- liftIO $ (Right <$> atomically stm) `catch` (return . Left)
    either throwError return r

{-# INLINE addSTM #-}
addSTM :: LoxVal -> LoxVal -> STM LoxVal
addSTM (LoxInt a) (LoxInt b) = return $ LoxInt (a + b)
addSTM (LoxDbl a) (LoxDbl b) = return $ LoxDbl (a + b)
addSTM (LoxInt a) (LoxDbl b) = return $ LoxDbl (fromIntegral a + b)
addSTM (LoxDbl a) (LoxInt b) = return $ LoxDbl (a + fromIntegral b)
addSTM a b = let msg = "Cannot add: " <> show a <> " and " <> show b
              in throwSTM (LoxError Unlocated msg :: LoxException)

type DblFn = (Double -> Double -> Double)
type IntFn = (Int -> Int -> Int)
numericalFn :: String -> DblFn -> IntFn -> BinaryFn
numericalFn _ _ f (LoxInt a) (LoxInt b) = return $ LoxInt (f a b)
numericalFn _ f _ (LoxDbl a) (LoxDbl b) = return $ LoxDbl (f a b)
numericalFn _ f _ (LoxInt a) (LoxDbl b) = return $ LoxDbl (f (fromIntegral a) b)
numericalFn _ f _ (LoxDbl a) (LoxInt b) = return $ LoxDbl (f a (fromIntegral b))
numericalFn name _ _ a b = throw' $ concat [ "Cannot apply operator: "
                                           , typeOf a, " ", name, " ", typeOf b
                                           ]

stringify :: LoxVal -> LoxT String
stringify LoxNil = return "nil"
stringify (LoxString t) = return $ T.unpack t
stringify (LoxBool b) = return $ fmap toLower (show b)
stringify (LoxInt n) = return $ show n
stringify (LoxDbl n) = return $ printf "%f" n
stringify (LoxFn fn) = return "<function>"
stringify (LoxClass cls) = return $ "<class " <> T.unpack (className cls) <> ">"
stringify (LoxObj o) | Just fn <- HM.lookup "toString" (methods $ objectClass o) = do
    fn <- bindThis (LoxObj o) fn
    apply Unlocated fn [] >>= stringify
stringify (LoxObj Object{..}) = do
    fieldMap <- liftIO $ atomically (readTVar objectFields)
    let fs = L.sortBy (compare `on` fst) . HM.toList $ fieldMap
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
    ,(Subtract,      numericalFn "-" (-) (-))
    ,(Multiply,      numericalFn "*" (*) (*))
    ,(Divide,        divide)
    ,(Mod,           numericalFn "%" ((fromIntegral .) . mod `on` floor) mod)
    ,(Seq,           (\a b -> a `seq` return b))
    ,(Exponent,      numericalFn "**" (**) (^))
    ]
    where
        lb f a b = LoxBool . f <$> a <=> b
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
(LoxArray (AtomArray a))  <=> (LoxArray (AtomArray b))  = do
    as <- liftIO $ A.readArray a
    bs <- liftIO $ A.readArray b
    ords <- V.zipWithM (<=>) as bs
    return $ fromMaybe (V.length as `compare` V.length bs)
           $ V.find (/= EQ) ords
a <=> b = let msg = "Cannot compare " <> typeOf a <> " and " <> typeOf b
           in throwError (LoxError Unlocated msg :: LoxException)

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

getModule :: SourceLocation -> ModuleIdentifier -> LoxT Object
getModule loc mod = do
    mods <- gets modules
    x <- HM.lookup mod <$> liftIO (readIORef mods)
    case x of
      Just Loading -> loxError loc ("Module cycle detected " <> show mod)
      Just (Loaded o) -> return o
      Nothing -> do
          liftIO $ modifyIORef' mods (HM.insert mod Loading)
          o <- loadModule loc mod `catchError` unblock mods
          liftIO $ modifyIORef' mods (HM.insert mod (Loaded o))
          return o
    where
        unblock ref e = do liftIO (modifyIORef' ref (HM.delete mod))
                           throwError e

loadModule :: SourceLocation -> ModuleIdentifier -> LoxT Object
loadModule loc m = do
    s <- get
    fn <- moduleToFileName m
    code <- liftIO (T.readFile fn) `catchError` fileNotFound fn
    let (ts, es) = tokens code
    when (not $ null es) $ loxError loc $ mconcat
        ["Could not load ", show m, ": "]
        <> L.intercalate "\n" (map show es)
    let (Right parsed, _) = runParser program (tokenStream fn ts)
    put (moduleInterpreter s)
    env <- runProgram (fromParsed parsed) >> gets bindings
    put s
    vals <- liftIO (readEnv (diffEnv (bindings s) env) >>= newTVarIO)
    return (Object (moduleCls s) vals)
    where
        fileNotFound n _ = loxError loc ("Could not find module: " <> show n)

moduleToFileName :: ModuleIdentifier -> LoxT FilePath
moduleToFileName (ModuleIdentifier parts) = do
    return (joinPath (map T.unpack parts) <.> "lox")

bindPattern :: SourceLocation -> Pattern VarName -> LoxVal -> LoxT ()
bindPattern _ Ignore _ = return ()
bindPattern _ (Name v) x = gets bindings >>= liftIO . assign v x >> return ()
-- destructure objects (more precisely, gettable things)
bindPattern _ (FromObject []) _ = return ()
bindPattern loc (FromObject ps) x = do
    getter <- lookupProtocol Gettable x
               >>= maybe (loxError loc $ "Cannot destructure " <> typeOf x) return
    destructureObj getter ps x
    where
        destructureObj _ [] _ = return ()
        destructureObj fn ((k,p):ps) x = do
            x' <- apply loc fn [x, LoxString k] `catchError` locateError loc
            bindPattern loc p x'
            destructureObj fn ps x
-- destructure arrays
bindPattern loc (FromArray ps mp) (LoxArray (AtomArray arr)) = do
    forM_ (zip [0 ..] ps) $ \(i, p) -> do
        x <- liftIO (A.get i arr) `catchError` locateError loc
        bindPattern loc p x
    case mp of
      Nothing -> return ()
      Just p -> do xs <- V.toList <$> readArray (AtomArray arr)
                   rst <- atomArray (drop (length ps) xs)
                   bindPattern loc p rst
bindPattern loc (FromArray _ _) x = loxError loc $ "Cannot destructure " <> typeOf x <> " as array"
