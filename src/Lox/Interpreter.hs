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
import Control.Arrow (second)
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
import Data.Traversable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Vector ((!?))

import Lox.Syntax
import qualified Lox.Core.Array as A

import Lox.Environment (
    declare, assign, resolve, deref, enterScope, enterScopeWith, inCurrentScope)

type BinaryFn = (Atom -> Atom -> LoxT Atom)

data Interpreter = Interpreter
    { bindings :: !Env
    , warnings :: !(HS.HashSet Text)
    , initialising :: !Bool
    , object :: !Class -- the base class all classes must inherit from
    , array :: !Class -- the class of arrays
    }

interpreter :: Env -> IO Interpreter
interpreter env = 
    Interpreter env mempty False <$> getClass "Object" <*> getClass "Array"
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
type LoxT a = StateT Interpreter (ExceptT LoxException IO) a

class Monad m => MonadLox m where
    printLox :: Atom -> m ()

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
runLoxT lox s = runExceptT $ evalStateT lox s

evalLoxT :: LoxT a -> Interpreter -> LoxResult (a, Interpreter)
evalLoxT lox s = runExceptT $ runStateT lox s

run :: Env -> Program -> IO Value
run env program = interpreter env >>= runLoxT (runProgram program)

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
    putEnv env
    return LoxNil

exec (ClassDecl loc name msuper methods) = do
    exec (Declare loc name)
    env <- gets bindings
    parent <- sequence $ fmap (findSuperClass env) msuper
    classId <- liftIO newSingleton
    base <- gets object

    let constructors = [ Function args body env | (Constructor args body) <- methods]
        statics      = [(n, Function as b env)  | (StaticMethod n as b) <- methods]
        instances    = [(n, Function as b env)  | (InstanceMethod n as b) <- methods]

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
fromLoxResult loc r = liftIO r >>= either (locateError loc) return

iterable :: SourceLocation -> Atom -> LoxT Stepper
iterable loc a = do
    fn <- lookupProtocol Iterable a
          >>= maybe (loxError loc $ "Cannot iterate over " <> typeOf a) return
    r <- apply loc fn [a]
    case r of
      (LoxIter it) -> return it
      wrong -> loxError loc $ "Iterator must return iterable, got " <> typeOf wrong

eval :: Expr -> LoxT Atom

eval (Lambda _ args body) = LoxFn . Function args body <$> gets bindings

eval (GetField loc e field) = do
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
    where
        cannotGet x = loxError loc $ "Cannot read fields of " <> typeOf x

eval (Index loc e ei) = do
    o <- eval e
    i <- eval ei
    case (o, i) of
      (LoxArray arr, LoxNum n) -> do
          vs <- readArray arr
          return $ fromMaybe LoxNil (vs !? floor n)
      (LoxObj _, LoxString k) -> eval (GetField loc (Literal (sourceLoc e) $ o) k)
                                 `catchError` \e -> case e of
                                                      (FieldNotFound{}) -> return LoxNil
                                                      e -> throwError e
      _                       -> loxError loc $ "Cannot index " <> typeOf o <> " with " <> typeOf i

eval (Literal _ a) = pure a

eval (Grouping _ e) = eval e

eval (Negate loc e) = do
    v <- eval e
    case v of
        LoxNum n -> return $ LoxNum (negate n)
        _        -> loxError loc $ "expected number, got " <> show v

eval (Not loc e) = do
    v <- eval e
    case v of
        LoxBool b -> return (LoxBool $ not b)
        _         -> loxError loc $ "Expected boolean, got " <> show v

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
        Nothing -> loxError (sourceLoc b) $ "Unknown operator: " <> show op
        Just f  -> do a' <- eval x
                      b' <- eval y
                      f a' b' `catchError` locateError (sourceLoc b)

eval (IfThenElse _ p x y) = do
    b <- truthy <$> eval p
    if b then eval x else eval y

eval (Var loc v) = do
    env <- gets bindings
    ma <- maybe undef (liftIO . deref) (resolve v env)
    case ma of
        Nothing -> uninit
        Just v -> return v

    where undef = loxError loc $ "Undefined variable: " <> show v
          uninit = loxError loc $ "Uninitialised variable: " <> show v

eval (Assign loc (LVar v) e) = do
    x <- eval e
    env <- gets bindings
    declared <- liftIO (assign v x env)
    if not declared
        then undeclared
        else return x
    where undeclared = loxError loc $ "Cannot set undeclared variable: " <> show v

eval (Assign loc (SetIdx lhs idx) e) = do
    target <- eval lhs
    k <- eval idx
    v <- eval e
    setter <- lookupProtocol Settable target
              >>= maybe (cannotSet target) return
    apply loc setter [target, k, v]
    return v
    where
        cannotSet a = loxError loc $ "Cannot assign fields on " <> typeOf a

eval (Assign loc (Set lhs fld) e) = do
    v <- eval e
    o <- eval lhs
    case o of
      LoxObj Object{objectFields=fs} -> assignField fs v >> return v
      _  -> loxError loc ("Cannot assign to " <> typeOf o)
    where
        assignField fs v = liftIO (modifyIORef fs (HM.insert fld v))

eval (Call loc callee args) = do
    e <- eval callee

    case e of
        LoxFn fn -> mapM eval args >>= apply loc fn
        LoxClass cls -> mapM eval args >>= instantiate loc cls
        LoxObj o -> do initing <- gets initialising
                       if initing then LoxNil <$ (mapM eval args >>= init loc o)
                                  else loxError loc $ "Cannot call initializer outside of init()"
        _        -> loxError loc $ "Cannot call " <> typeOf e

eval (Array loc exprs) = do
    as <- mapM eval exprs >>= liftIO . A.fromList LoxNil
    return (LoxArray (AtomArray as))

init :: SourceLocation -> Object -> [Atom] -> LoxT ()
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


lookupProtocol :: Protocol -> Atom -> LoxT (Maybe Callable)
lookupProtocol p a = do
    mcls <- classOf a
    return (mcls >>= HM.lookup p . protocols)

classOf :: Atom -> LoxT (Maybe Class)
classOf (LoxObj o) = return (Just $ objectClass o)
classOf (LoxArray _) = Just <$> gets array
classOf _ = return Nothing

instantiate :: SourceLocation -> Class -> [Atom] -> LoxT Atom
instantiate loc cls args = do
    obj   <- liftIO (new cls)
    initialise (init loc obj args)
    return (LoxObj $ obj)

new :: Class -> IO Object
new cls = Object cls <$> newIORef HM.empty

bindThis :: Atom -> Callable -> LoxT Callable
bindThis this (BuiltIn ar fn)
  = return $ BuiltIn (\n -> ar (n + 1)) (\args -> fn (this : args))
bindThis this (Function ns body env)
  = Function ns body <$> liftIO (enterScopeWith [("this", this)] env)

apply :: SourceLocation -> Callable -> [Atom] -> LoxT Atom
apply loc fn args | not (arity fn (length args)) = loxError loc msg
    where
        msg = "Wrong number of arguments."

apply loc (BuiltIn _ fn) args = fromLoxResult loc (fn args)

apply _ (Function names body env) args = do
   old <- gets bindings
   let xs = zip names args
   liftIO (enterScopeWith (zip names args) env) >>= putEnv
   --- unwrap blocks to avoid double scoping
   let sts = case body of Block _ sts -> sts
                          _ -> [body]
   r <- (LoxNil <$ mapM_ exec sts) `catchError` catchReturn
   putEnv old
   return r
   where
    catchReturn (LoxReturn _ v) = return v
    catchReturn e               = throwError e

warn :: SourceLocation -> Text -> LoxT ()
warn loc msg = modify' $ \s -> s { warnings = HS.insert (locS <> msg) (warnings s) }
    where ((f1, l1, c1), (f2, l2, c2)) = range loc
          locS = mconcat ["(", T.pack (show l1), ",", T.pack (show c1), ")"
                         ," - (", T.pack (show l2), ",", T.pack (show c2)
                         ,") "
                         ]

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
typeOf (LoxArray _) = "Array"

addAtoms :: BinaryFn
addAtoms (LoxString a) (LoxString b) = return $ LoxString (a <> b)
addAtoms (LoxNum a) (LoxNum b) = return $ LoxNum (a + b)
addAtoms a (LoxString b) = do s <- T.pack <$> stringify a
                              return $ LoxString (s <> b)
addAtoms (LoxString a) b = do s <- T.pack <$> stringify b
                              return $ LoxString (a <> s)
addAtoms a b = throw' ("Cannot add: " <> show a <> " and " <> show b)

numericalFn :: String -> (Nano -> Nano -> Nano) -> BinaryFn
numericalFn _ f (LoxNum a) (LoxNum b) = return $ LoxNum (f a b)
numericalFn name _ a b = throw' $ concat [ "Cannot ", name, ": "
                                         , show a, " and ", show b
                                         ]

stringify :: Atom -> LoxT String
stringify LoxNil = return "nil"
stringify (LoxString t) = return $ T.unpack t
stringify (LoxBool b) = return $ fmap toLower (show b)
stringify (LoxNum n) = return $ showFixed True n
stringify (LoxFn fn) = return "<function>"
stringify (LoxClass cls) = return $ "<class " <> T.unpack (className cls) <> ">"
stringify (LoxObj Object{..}) = do
    fs <- HM.toList <$> liftIO (readIORef objectFields)
    fs' <- mapM (sequence . second stringify) fs
    return $ concat
           $ ["<", T.unpack (className objectClass), " "]
             ++ L.intersperse " " [T.unpack k <> "=" <> v | (k, v) <- fs']
             ++ [">"]
stringify (LoxArray arr) = do
    vs <- readArray arr
    es <- V.toList <$> mapM quoteString vs
    return $ concat [ "["
                    , L.intercalate ", " es
                    , "]"
                    ]

quoteString :: Atom -> LoxT String
quoteString s@LoxString{} = fmap (\s -> '"' : s <> "\"") (stringify s)
quoteString a = stringify a

isInteger :: Double -> Bool
isInteger d = d == fromInteger (floor d)

binaryFns :: HashMap BinaryOp BinaryFn
binaryFns = HM.fromList
    [(Equals,        (\a b -> LoxBool       <$> (a === b)))
    ,(NotEquals,     (\a b -> LoxBool . not <$> (a === b)))
    ,(LessThan,      lb (== LT))
    ,(LessThanEq,    lb (/= GT))
    ,(GreaterThan,   lb (== GT))
    ,(GreaterThanEq, lb (/= LT))
    ,(Add,           addAtoms)
    ,(Subtract,      numericalFn "subtract" (-))
    ,(Multiply,      numericalFn "multiply" (*))
    ,(Divide,        numericalFn "divide" (/))
    ,(Mod,           numericalFn "mod" (\a b -> fromInteger(floor a `mod` floor b)))
    ,(Seq,           (\a b -> a `seq` return b))
    ]
    where lb f a b = LoxBool . f <$> a <=> b

(===) :: Atom -> Atom -> LoxT Bool
(LoxObj a)   === (LoxObj b)   = return (a == b)
(LoxClass a) === (LoxClass b) = return (a == b)
a            === b            = fmap (== EQ) (a <=> b)

(<=>) :: Atom -> Atom -> LoxT Ordering
LoxNil        <=> LoxNil = return EQ
LoxNil        <=> _      = return LT
_             <=> LoxNil = return GT
(LoxBool a)   <=> (LoxBool b)   = return (a `compare` b)
(LoxNum a)    <=> (LoxNum b)    = return (a `compare` b)
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

simplify :: SourceLocation -> SourceLocation
simplify Unlocated = Unlocated
simplify loc = let (l@(a, b, c), r@(d, e, f)) = range loc
                in if l == r then SourceLocation a b c
                             else SourceLocation a b c :..: SourceLocation d e f

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
locateError loc (ArgumentError NativeCode ts as)
  = throwError (ArgumentError loc ts as)
locateError _  e                      = throwError e
