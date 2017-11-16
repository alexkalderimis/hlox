{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lox.Interpreter.Types where

import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

import Lox.Syntax
import Lox.Environment (resolve, deref)

-- we require two effects in the interpreter:
-- * statefulness of the bindings
-- * exception handing
-- Rather than using a stack of transformers, we unroll the stack here manually,
-- because this exercise is all about explicitness.
newtype LoxT a = LoxT (Interpreter -> IO (Either LoxException a, Interpreter))

instance Functor LoxT where
    fmap f (LoxT lox) = LoxT $ \s -> do (ret, s') <- lox $! s
                                        return (fmap f ret, s')

instance Applicative LoxT where
    pure a = LoxT $ \s -> return (Right a, s)
    (LoxT a) <*> (LoxT b) = LoxT $ \s -> do
        (ret, s') <- a $! s
        case ret of
          Left e -> return (Left e, s')
          Right f -> do (retb, sb) <- b s'
                        return (fmap f retb, sb)

instance Monad LoxT where
    (LoxT lox) >>= f = LoxT $ \s -> do
        (ret, s') <- lox $! s
        case ret of
          Left e -> return (Left e, s')
          Right a -> let (LoxT lox') = f a
                      in lox' $! s'

instance MonadState Interpreter LoxT where
    state f = LoxT $ \i -> let (a, s) = f $! i
                            in return (Right a, s)

instance MonadError (LoxException' LoxVal) LoxT where
    throwError e = LoxT $ \s -> return (Left e, s)
    catchError (LoxT action) handler = LoxT $ \s -> do
        (ret, s') <- action $! s
        case ret of
          Right a -> return (Right a, s')
          Left e -> let (LoxT cont) = handler $! e
                     in cont $! s'

instance MonadIO LoxT where
    liftIO io = LoxT $ \s -> do
        a <- io
        return (Right a, s)

runLoxT :: LoxT a -> Interpreter -> LoxResult a
runLoxT (LoxT f) s = do
    (ret, s') <- f $! s
    case ret of
      Left e -> Left <$> stackify s' e
      Right a -> return (Right a)

evalLoxT :: LoxT a -> Interpreter -> LoxResult (a, Interpreter)
evalLoxT (LoxT f) s = do
    (ret, s') <- f $! s
    case ret of
      Left e -> Left <$> stackify s' e
      Right a -> return (Right (a, s'))

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
    , object :: !Class -- the base class all classes must inherit from
    , array :: !Class -- the class of arrays
    , moduleCls :: !Class -- the class of modules
    }

interpreter :: [(ModuleIdentifier, Object)] -> Env -> IO Interpreter
interpreter modules env = do
    mod <- getMod
    mods <- newIORef $ HM.fromList [(m, Loaded o { objectClass = mod }) | (m, o) <- modules]
    Interpreter env env mods mempty False [] <$> getClass "Object"
                                             <*> getClass "Array"
                                             <*> pure mod
    where
        getMod = do cls <- getClass "Object"
                    cid <- newSingleton
                    return $ cls { className = "Module"
                                 , classId = cid
                                 , protocols = HM.delete Settable (protocols cls)
                                 }
        getClass c = do let mref = resolve c env
                        case mref of
                          Nothing -> return emptyClass
                          Just ref -> do ma <- deref ref
                                         case ma of
                                           Just (LoxClass cls) -> return cls
                                           _                   -> return emptyClass

-- get an interpreter for loading a module.
moduleInterpreter :: Interpreter -> Interpreter
moduleInterpreter parent = parent { warnings = mempty
                                  , bindings = baseEnv parent
                                  , initialising = False
                                  , stack = []
                                  }

stackify :: Interpreter -> LoxException -> IO LoxException
stackify s e = do
    case stack s of
      [] -> return e
      fs -> return (StackifiedError fs e)

