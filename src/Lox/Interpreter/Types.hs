{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Lox.Interpreter.Types where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, IOException, ErrorCall(..), Handler(..),
  throwIO, toException, try, catches)
import Control.Exception.Base (Exception)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad (join, unless, void, foldM)
import Data.Bifunctor
import Data.Data (Typeable, Data)
import Data.Default
import Data.Hashable (Hashable)
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T

import Lox.Syntax
import Lox.Environment (selectBindings, readEnv, Environment)
import qualified Lox.Core.Array as A
import Lox.Environment (resolve, deref, enterScopeWith)

-- we require two effects in the interpreter:
-- * statefulness of the bindings
-- * exception handing
-- Rather than using a stack of transformers, we unroll the stack here manually,
-- because this exercise is all about explicitness.
newtype LoxM a = LoxM (Interpreter -> IO (a, Interpreter))
type Env = Environment VarName LoxVal

type Program = [Statement]

newtype Singleton = Singleton (IORef ())
    deriving Eq

instance Show Singleton where
    show _ = "<id>"

instance Functor LoxM where
    fmap f (LoxM lox) = LoxM $ \s -> do (ret, s') <- lox $! s
                                        let ret' = f ret
                                        ret' `seq` return (ret', s')

instance Applicative LoxM where
    pure a = LoxM $ \s -> return (a, s)
    (LoxM a) <*> (LoxM b) = LoxM $ \s -> do
        (f, s') <- a s
        (x, s'') <- b s'
        return (f $! x, s'')

instance Monad LoxM where
    fail msg = loxError (T.pack msg)
    (LoxM lox) >>= f = LoxM $ \s -> do
        (x, s') <- lox $! s
        let (LoxM lox') = f x
        lox' $! s'

instance MonadState Interpreter LoxM where
    state f = LoxM $ \i -> return (f $! i)

-- the errors we can catch are 'RuntimeError's
instance MonadError RuntimeError LoxM where
    throwError e = LoxM $ \_ -> throwIO e
    catchError (LoxM action) handler = LoxM $ \s -> do
        mret <- try (action $! s)
        case mret of
          Right r -> return r
          Left e -> let (LoxM cont) = handler e in cont $! s

instance MonadIO LoxM where
    liftIO io = LoxM $ \s -> do
        ret <- fmap Right io `catches` handlers
        case ret of
          Left e -> throwIO (RuntimeError (stack s) e)
          Right r -> return (r, s)
        where
            handlers = [ Handler $ \ (ex :: IOException) -> handle ex
                       , Handler $ \ (ex :: ErrorCall) -> handle ex
                       , Handler $ \ (ex :: LoxException) -> return (Left ex)
                       ]
            handle :: (Exception e) => e -> IO (Either LoxException a)
            handle e = return . Left . CaughtEx $ toException e

throwLox :: LoxException -> LoxM a
throwLox e = LoxM $ \s -> throwIO $ RuntimeError (stack s) e

loxError :: Text -> LoxM a
loxError = throwLox . LoxError

throwLoop :: LoopControl -> LoxM a
throwLoop e = LoxM $ \_ -> throwIO e

tryLoop :: LoxM a -> LoxM (Either LoopControl a)
tryLoop (LoxM f) = LoxM $ \s -> do
    ret <- try (f s)
    case ret of
      Left e -> return (Left e, s)
      Right (r, s') -> return (Right r, s')

-- run a loop and return whether the loop body broke
runLoop :: LoxM a -> LoxM Bool
runLoop = fmap (either (== LoxBreak) (const False)) . tryLoop

returnVal :: LoxVal -> LoxM a
returnVal val = LoxM $ \s -> throwIO (LoxReturn val s)

returning :: LoxM a -> LoxM LoxVal
returning (LoxM f) = LoxM $ \s -> do
    ret <- try (f s)
    case ret of
      Left (LoxReturn val s') -> return (val, s')
      Right (_, s') -> return (LoxNil, s')

yieldVal :: LoxVal -> LoxM ()
yieldVal val = do
    channel <- gets yieldChannel
                >>= maybe (loxError "Cannot yield outside iterator") return
    -- yield the value to the parent thread
    liftIO . atomically $ putTMVar channel (Yielded val) 
    -- and wait for this thread to be resumed
    liftIO . atomically $ putTMVar channel Waiting

yielding :: LoxM LoxVal -> LoxM Stepper
yielding lox = do
    channel <- liftIO $ newEmptyTMVarIO
    s <- get

    let 
      runIterator = void $ liftIO $ forkIO $ do
        r <- runLox lox s { yieldChannel = Just channel }
        let yv = case r of Left e -> Failed e
                           Right a -> Done a
        liftIO . atomically $ putTMVar channel yv

      next started = do
        unless started runIterator
        val <- liftIO . atomically $ takeTMVar channel
        case val of
          Waiting -> next True
          Failed e -> throwError e
          Yielded r -> return (Just r, True)
          _ -> return (Nothing, True)

    return (Stepper False next)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust ma f = maybe (return ()) f ma

runLox :: LoxM a -> Interpreter -> LoxResult a
runLox (LoxM f) s = try (fst <$> f s)

evalLox :: LoxM a -> Interpreter -> LoxResult (a, Interpreter)
evalLox lox s = runLox ((,) <$> lox <*> get) s

type Modules = IORef (HM.HashMap ModuleIdentifier LoxModule)

-- for now, a module is just an object
-- this is bad, since objects are mutable, but that is for another day.
data LoxModule = Loading -- detect cycles - mutual imports are not supported.
               | Loaded Object

data Interpreter = Interpreter
    { baseEnv :: !Env
    , bindings :: !Env
    , modules :: !Modules
    , warnings :: !(HS.HashSet Text)
    , initialising :: !Bool
    , stack :: ![StackFrame]
    , modcls :: !Class
    , yieldChannel :: !(Maybe (TMVar Yielded))
    , exports :: !(Maybe (HS.HashSet VarName))
    } deriving (Typeable)

exported :: Interpreter -> Env
exported st = case exports st of
                Nothing -> mempty
                Just vs -> selectBindings vs (bindings st)

data Yielded = Waiting
             | Yielded LoxVal
             | Done LoxVal
             | Failed RuntimeError
             deriving (Typeable, Show)

coreClass :: VarName -> Env -> IO (Either LoxException Class)
coreClass name env = do
    let mref = resolve name env
    case mref of
      Nothing -> do cid <- newSingleton
                    return (Right emptyClass { className = name, classId = cid })
      Just ref -> do
        ma <- deref ref
        case ma of
          Nothing             -> return (Left $ LoxError $ "Undefined core class: " <> name)
          Just (LoxClass cls) -> return (Right cls)
          Just x              -> return (Left $ TypeError "Class" x)

data InitError = InitError LoxException
     deriving (Show, Typeable)

instance Exception InitError

interpreter :: [(ModuleIdentifier, Object)] -> Env -> IO Interpreter
interpreter modules env = do
    env' <- foldM ensureCls env coreClasses
    modCls <- getMod
    mods <- newIORef $ HM.fromList [(m, Loaded o { objectClass = modCls }) | (m, o) <- modules]
    return $ Interpreter env' env' mods mempty False [] modCls Nothing Nothing
    where
        -- initial core object hierarchy
        coreClasses = [ ("Object", Nothing)
                      , ("Array", Nothing)
                      , ("Error", Just "Object")
                      , ("Trace", Just "Object")
                      , ("InternalError", Just "Error")
                      , ("FieldNotFound", Just "Error")
                      , ("TypeError", Just "Error")
                      , ("ArgumentError", Just "Error")
                      , ("UserError", Just "Error")
                      , ("AssertionError", Just "UserError")
                      , ("String", Nothing)
                      ]
        ensureCls e (n, ms) = do
            cls <- getCls e n
            sup <- traverse (getCls e) ms 
            let val = LoxClass cls { superClass = sup }
            enterScopeWith [(n, val)] e
        getCls e n = coreClass n e >>= either (throwIO . InitError) return
        getMod = do cls <- getCls env "Object"
                    cid <- newSingleton
                    return $ cls { className = "Module"
                                 , classId = cid
                                 , protocols = HM.delete Settable (protocols cls)
                                 }

type Value = Either RuntimeError LoxVal
type LoxResult a = IO (Either RuntimeError a)

data LoxVal
    -- Atomic values
    = LoxLit !Atom
    -- Composite stuff
    | LoxFn !Callable
    | LoxClass !Class
    | LoxObj !Object 
    | LoxArray !AtomArray
    | LoxIter !Stepper
    | NativeObj !HSObj
    deriving (Show, Typeable)

-- embedded host object
data HSObj = HSObj Class (forall a b. (Typeable a, Typeable b) => (a -> b) -> Maybe b)
    deriving (Typeable)

instance Show HSObj where
    show _ = "<hs-obj>"

-- helper patterns to avoiding verbose literals
pattern LoxNil :: LoxVal
pattern LoxNil = LoxLit Nil
pattern Yes :: LoxVal
pattern Yes = LoxLit (ABool True)
pattern No :: LoxVal
pattern No = LoxLit (ABool False)

pattern LoxInt :: Int -> LoxVal
pattern LoxInt i = LoxLit (AInt i)
pattern Intish :: Int -> LoxVal
pattern Intish n <- LoxLit (asInt -> Just n) 

pattern LoxDbl :: Double -> LoxVal
pattern LoxDbl i = LoxLit (ADbl i)
pattern LoxNum :: Double -> LoxVal
pattern LoxNum d <- LoxLit (asDbl -> Just d) -- for things that can be interpreted as doubles

pattern LoxString :: Text -> LoxVal
pattern LoxString t = LoxLit (Str t) -- shim
pattern Txt :: Text -> LoxVal
pattern Txt t = LoxLit (Str t)

pattern LoxBool :: Bool -> LoxVal
pattern LoxBool b = LoxLit (ABool b)

instance Default LoxVal where
    def = LoxLit Nil

typeOf :: LoxVal -> Text
typeOf (LoxLit Nil) = "nil"
typeOf (LoxLit ABool{}) = "Boolean"
typeOf (LoxLit AInt{}) = "Number"
typeOf (LoxLit ADbl{}) = "Number"
typeOf (LoxLit Str{}) = "String"
typeOf (LoxFn _) = "Function"
typeOf (LoxClass _) = "Class"
typeOf (LoxObj c) = className $ objectClass c
typeOf (LoxArray _) = "Array"
typeOf (LoxIter _) = "Iterator"
typeOf (NativeObj (HSObj cls _)) = className cls

data LoxException = LoxError Text
                  | FieldNotFound Atom
                  | UserError LoxVal
                  | TypeError Text LoxVal
                  | ArgumentError VarName [String] [LoxVal]
                  | CaughtEx SomeException
                  | AssertionError LoxVal
                  deriving (Show, Typeable)

data RuntimeError = RuntimeError
    { stackTrace :: [StackFrame]
    , cause :: LoxException
    } deriving (Show, Typeable)

instance Exception RuntimeError
-- you can also throw LoxExceptions directly when using embedded IO
-- this will get promoted correctly to a runtime error and can be caught
-- with catchError. 
instance Exception LoxException

data LoxReturn = LoxReturn LoxVal Interpreter deriving (Typeable)
instance Exception LoxReturn

instance Show LoxReturn where
    show (LoxReturn v _) = unwords ["(LoxReturn", show v, ")"]

data LoopControl = LoxContinue | LoxBreak
    deriving (Show, Eq, Data, Typeable)
instance Exception LoopControl

data Stepper = forall a. Stepper a (a -> LoxM (Maybe LoxVal, a))

instance Show Stepper where
    show _ = "Iterator"

newtype AtomArray = AtomArray { unAtomArray :: A.Array LoxVal }

instance Show AtomArray where
    show _ = "AtomArray"

type Methods = HM.HashMap VarName Callable

data Callable = BuiltIn VarName (Int -> Bool) NativeFn
              | Closure (Lambda VarName Atom) Interpreter

-- Native function that supports errors and IO
type NativeFn = [LoxVal] -> LoxM LoxVal

class HasArity f where
    getArity :: (Proxy f) -> Int

instance IsLoxVal v => HasArity (IO v) where
    getArity _ = 0

instance HasArity LoxVal where
    getArity _ = 0

instance HasArity (LoxM a) where
    getArity _ = 0

instance (IsLoxVal v, HasArity r) => HasArity (v -> r) where
    getArity _ = 1 +  getArity (Proxy :: Proxy r)

class IsNativeFn f where
    toNativeFn :: f -> NativeFn

class IsLoxError a where
    toLoxError :: a -> LoxException

instance IsLoxError LoxException where
    toLoxError = id

instance IsLoxError Text where
    toLoxError = LoxError

class IsLoxVal v where
    toLoxVal ::  v -> LoxVal
    fromLoxVal :: LoxVal -> Either LoxException v

natively :: (IsLoxVal v, IsLoxVal r) => VarName -> (v -> r) -> Callable
natively n f = BuiltIn n (== 1) (toNativeFn (io . f))
    where
        io :: a -> IO a
        io = return

callable :: forall f. (HasArity f, IsNativeFn f) => VarName -> f -> Callable
callable name f = let n = getArity (Proxy :: Proxy f)
                   in BuiltIn name (== n) (toNativeFn f)

instance IsLoxVal LoxVal where
    toLoxVal = id
    fromLoxVal lv = Right lv

instance IsLoxVal () where
    toLoxVal () = LoxNil
    fromLoxVal LoxNil = Right ()
    fromLoxVal x = Left (TypeError "Nil" x) 

instance IsLoxVal Text where
    toLoxVal t = Txt t
    fromLoxVal (Txt t) = Right t
    fromLoxVal x = Left (TypeError "String" x)

instance IsLoxVal Int where
    toLoxVal i = LoxInt i
    fromLoxVal (LoxInt i) = Right i
    fromLoxVal x = Left (TypeError "Int" x)

instance IsLoxVal Double where
    toLoxVal n = LoxDbl n
    fromLoxVal (LoxDbl d) = Right d
    fromLoxVal (LoxInt i) = Right (fromIntegral i)
    fromLoxVal x = Left (TypeError "Double" x)

instance IsLoxVal Bool where
    toLoxVal = LoxBool
    fromLoxVal (LoxBool b) = Right b
    fromLoxVal x = Left (TypeError "Bool" x)

instance IsLoxVal Atom where
    toLoxVal = LoxLit
    fromLoxVal (LoxLit a) = Right a
    fromLoxVal x = Left (TypeError "Atom" x)

instance IsLoxVal Object where
    toLoxVal = LoxObj
    fromLoxVal (LoxObj o) = Right o
    fromLoxVal x = Left (TypeError "Object" x)

instance IsLoxVal Callable where
    toLoxVal = LoxFn
    fromLoxVal (LoxFn fn) = Right fn
    fromLoxVal x = Left (TypeError "Function" x)

instance IsLoxVal AtomArray where
    toLoxVal = LoxArray
    fromLoxVal (LoxArray xs) = Right xs
    fromLoxVal x = Left (TypeError "Array" x)

instance IsLoxVal Stepper where
    toLoxVal = LoxIter
    fromLoxVal (LoxIter s) = Right s
    fromLoxVal x = Left (TypeError "Iterator" x)

instance IsLoxVal Class where
    toLoxVal = LoxClass
    fromLoxVal (LoxClass c) = Right c
    fromLoxVal x = Left (TypeError "Class" x)

class IsNativeResult r where
    toNativeResult :: r ->  IO (Either LoxException LoxVal)

instance IsLoxVal v => IsNativeResult (IO v) where
    toNativeResult io = do r <- try io
                           return (fmap toLoxVal r)

newtype NatFn = NativeFn { fromNatFn :: [LoxVal] -> LoxM LoxVal }

instance IsNativeFn NatFn where
    toNativeFn = fromNatFn

instance (IsNativeFn g, IsLoxVal v) => IsNativeFn (v -> g) where
    toNativeFn _ [] = throwLox (LoxError "Not enough arguments")
    toNativeFn f (a:as) = case fromLoxVal a of
                            Left e -> throwLox e
                            Right v -> toNativeFn (f v) as

instance (IsLoxVal v) => IsNativeFn (IO v) where
    toNativeFn r [] = toLoxVal <$> liftIO r
    toNativeFn _ as = throwLox . LoxError . mconcat
                        $ "Unexpected arguments: "
                        : L.intersperse ", " (fmap typeOf as)

instance (IsLoxVal v) => IsNativeFn (LoxM v) where
    toNativeFn lox [] = fmap toLoxVal lox
    toNativeFn _ as = throwLox . LoxError . mconcat
                        $ "Unexpected arguments: "
                        : L.intersperse ", " (fmap typeOf as)

instance IsNativeFn LoxVal where
    toNativeFn r = toNativeFn (return r :: IO LoxVal)

fnName :: Callable -> VarName
fnName (BuiltIn n _ _) = n
fnName (Closure (Lambda mn _ _) _) = fromMaybe "<Anon>" mn

qualifyName :: VarName -> Callable -> Callable
qualifyName p (BuiltIn n a f)
  = BuiltIn (p <> n) a f
qualifyName p (Closure (Lambda mn args body) int)
  = Closure (Lambda (fmap (p <>) mn) args body) int

-- is this arity acceptable to this function?
arity :: Callable -> Int -> Bool
arity (BuiltIn _ p _)               n = p n
arity (Closure (Lambda _ args _) _) n
  = case args of
      (_, Just _) -> True
      (pos, _) -> length pos == n

instance Show Callable where
    show (Closure lam _) = unwords ["(Closure", show lam, ")"]
    show (BuiltIn n _ _) = "[NativeCode " <> T.unpack n <> "]"

-- closed set of protocols with special syntactic sugar:
data Protocol = Settable -- a[i]            --> apply fn [a, i]
              | Gettable -- a[i] = b        --> apply fn [a, i, b]
              | Iterable -- for (i in a) {} --> apply fn [a]
              deriving (Show, Eq, Generic, Data, Typeable)
instance Hashable Protocol

data Class = Class
    { classId :: Singleton
    , className :: VarName
    , superClass :: Maybe Class
    , initializer :: Maybe Callable
    , staticMethods :: Methods
    , methods :: Methods
    , protocols :: HM.HashMap Protocol Callable
    } deriving (Show)

emptyClass :: Class
emptyClass = Class (unsafePerformIO $ newSingleton)
                   "" Nothing Nothing mempty mempty mempty

data Object = Object
    { objectClass :: Class
    , objectFields :: TVar (HM.HashMap Atom LoxVal)
    }

instance Eq Object where
    a == b = objectFields a == objectFields b

instance Show Object where
    show o = mconcat ["<Instance of "
                     , T.unpack (className $ objectClass o)
                     ,">"
                     ]

instance Eq Class where
    a == b = classId a == classId b

new :: Class -> [(Atom, LoxVal)] -> LoxM Object
new cls flds = LoxM $ \s -> do
    o <- Object cls <$> newTVarIO (HM.fromList flds)
    return (o, s)

arrayFromList :: (Monad m, MonadIO m) => [LoxVal] -> m AtomArray
arrayFromList xs = AtomArray <$> (liftIO $  A.fromList xs)

atomArray :: [LoxVal] -> LoxM LoxVal
atomArray = fmap LoxArray . arrayFromList

nil :: LoxVal -> Bool
nil (LoxLit Nil) = True
nil _ = False

truthy :: LoxVal -> Bool
truthy LoxNil      = False
truthy (LoxBool b) = b
truthy _           = True

readArray :: MonadIO m => AtomArray -> m (Vector LoxVal)
readArray (AtomArray xs) = liftIO $ A.readArray xs

-- get an interpreter for loading a module.
moduleInterpreter :: Interpreter -> Interpreter
moduleInterpreter parent = parent { warnings = mempty
                                  , bindings = baseEnv parent
                                  , initialising = False
                                  , stack = []
                                  , exports = Just mempty
                                  }

newSingleton :: IO Singleton
newSingleton = Singleton <$> newIORef ()

unsafeSingleton :: () -> Singleton
unsafeSingleton = unsafePerformIO . fmap Singleton . newIORef

-- for use in native modules
argumentError :: [String] -> [LoxVal] -> LoxM LoxVal
argumentError types = throwLox . ArgumentError "" types

envToFields :: Env -> IO (TVar (HM.HashMap Atom LoxVal))
envToFields env = do
    bindings <- HM.toList <$> readEnv env
    newTVarIO (HM.fromList $ fmap (first Str) bindings)

putEnv :: Env -> LoxM ()
putEnv env = modify' $ \s -> s { bindings = env }

-- returns (old, new)
modEnv :: (Env -> Env) -> LoxM (Env, Env)
modEnv f = do old <- gets bindings
              let env = f old
              (old, env) <$ putEnv env

runtimeToLoxVal :: RuntimeError -> LoxM Object
runtimeToLoxVal e = do
    trace <- errorTrace (stackTrace e)
    env <- gets baseEnv
    let cls n = liftIO (coreClass n env) >>= either throwLox return
        trc = ("stackTrace", trace)
    case (cause e) of
      AssertionError m -> do ec <- cls "AssertionError"
                             new ec [trc, ("message", m)]
      LoxError msg -> do ec <- cls "InternalError"
                         new ec [trc, ("message", Txt msg)]
      FieldNotFound k -> do ec <- cls "FieldNotFound"
                            new ec [trc
                                   , ("message", Txt ("missing field: " <> unatom k))
                                   , ("key", LoxLit k)]
      UserError val -> do ec <- cls "Error"
                          new ec [trc, ("message", val)]
      TypeError ty got -> do ec <- cls "TypeError"
                             new ec [trc
                                    , ("message", Txt (ty <> " != " <> typeOf got))
                                    , ("expected", Txt ty)
                                    , ("received", Txt (typeOf got))
                                    ]
      ArgumentError v _ _ -> do ec <- cls "ArgumentError"
                                new ec [trc, ("callee", Txt v)]
      CaughtEx err -> do ec <- cls "InternalError"
                         new ec [trc, ("message", Txt (T.pack $ show err))]

errorTrace :: [StackFrame] -> LoxM LoxVal
errorTrace frames = do
    env <- gets baseEnv
    trcCls <- liftIO (coreClass "Trace" env) >>= either throwLox return
    ts <- mapM (asTrace trcCls) frames
    atomArray ts
    where 
        asTrace cls (was, wo) = LoxObj <$> do
            let ((file, l, c), (_, l', c')) = range wo
            new cls [("file", Txt file)
                    ,("startLine", LoxInt l)
                    ,("startChar", LoxInt c)
                    ,("endLine", LoxInt l')
                    ,("endChar", LoxInt c')
                    ,("context", Txt was)
                    ]

getMethod :: Methods -> LoxVal -> Text -> LoxM LoxVal
getMethod methods this name =
    case HM.lookup name methods of
      Just fn -> LoxFn <$> bindThis this fn
      Nothing -> throwLox (FieldNotFound (Str name))

objectMethod :: Class -> LoxVal -> Atom -> Maybe (LoxM Callable)
objectMethod c inst (Str key) = bindThis inst <$> go c
    where
        go cls = HM.lookup key (methods cls) <|> (superClass cls >>= go) 
objectMethod _ _ _ = Nothing

classOf :: LoxVal -> LoxM (Maybe Class)
classOf x = case x of
  (LoxString _) -> Just <$> knownClass "String"
  (LoxArray _) -> Just <$> knownClass "Array"
  (LoxObj o) -> return (Just $ objectClass o)
  (NativeObj (HSObj cls _)) -> return (Just cls)
  _ -> return Nothing

-- get a class we know to exist, from the base-environment
knownClass :: VarName -> LoxM Class
knownClass n = do
    env <- gets baseEnv
    mc <- liftIO $ join <$> traverse deref (resolve n env)
    case mc of
        Just (LoxClass c) -> return c
        Nothing -> loxError $ "Illegal state - " <> n <> " not defined"
        Just x  -> loxError $ "Illegal state - " <> n <> " defined as " <> typeOf x
  
bindThis :: LoxVal -> Callable -> LoxM Callable
bindThis this (BuiltIn name ar fn)
  = return $ BuiltIn name (\n -> ar (n + 1)) (\args -> fn (this : args))
bindThis this (Closure lam s)
  = do env <- liftIO (enterScopeWith [("this", this)] (bindings s))
       return $ Closure lam s { bindings = env }

exporting :: Interpreter -> Bool
exporting = isJust . exports

export :: VarName -> Interpreter -> Interpreter
export name st = st { exports = HS.insert name <$> exports st }
