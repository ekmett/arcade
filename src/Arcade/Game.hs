{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Arcade.Game
  ( Game(..)
  , (<?>)
  -- * Environment
  , Env(..)
  , HasEnv(..)
  -- * World
  , World(..)
  , HasWorld(..)
  -- * Failure
  , GameException(..)
  , stackTrace
  , gameExceptionMessage
  , AsGameException(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception as Exception
import Control.Exception.Lens
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Catch as Catch
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Foldable
import Data.Typeable
import Arcade.Connection
import Arcade.Request


newtype Env = Env { _envRequestChan :: Chan Request }

makeClassy ''Env

newtype World = World
  { _connections :: [Connection]
  } deriving Typeable

makeClassy ''World

data Result a = OK a !World
  deriving (Functor, Foldable, Typeable)

newtype Game a = Game { runGame :: Env -> World -> IO (Result a) }
  deriving (Functor, Typeable)

data GameException = GameException
  { _stackTrace :: [String]
  , _gameExceptionMessage :: Maybe String
  } deriving (Show, Read, Typeable)

makeLenses ''GameException

instance Exception GameException

class AsGameException t where
  _GameException :: Prism' t GameException

instance AsGameException GameException where
  _GameException = id

instance AsGameException SomeException where
  _GameException = prism' toException fromException

instance Applicative Game where
  pure = return
  (<*>) = ap

instance Alternative Game where
  empty = mzero
  (<|>) = mplus

instance Monad Game where
  return a = Game $ \_ s -> return $ OK a s
  Game m >>= f = Game $ \e s -> m e s >>= \r -> case r of
    OK a s' -> runGame (f a) e s'
  fail r = Game $ \_ _ -> throwIO $ GameException [] (Just r)

instance MonadPlus Game where
  mzero = Game $ \_ _ -> throwIO $ GameException [] Nothing
  mplus (Game m) (Game n) = Game $ \e s -> catching_ id (m e s) (n e s)

instance MonadState World Game where
  state f = Game $ \_ s -> case f s of
    (a, s') -> return $ OK a s'

instance MonadIO Game where
  liftIO m = Game $ \_ s -> do
    a <- m
    return $ OK a s

instance MonadReader Env Game where
  ask = Game $ \e s -> return $ OK e s
  local f (Game m) = Game $ \e s -> m (f e) s

instance MonadError SomeException Game where
  throwError = liftIO . throwIO
  catchError (Game m) h = Game $ \ e s -> m e s `Exception.catch` \x -> runGame (h x) e s

instance MonadCatch Game where
  throwM = liftIO . throwIO
  catch (Game m) h = Game $ \ e s -> m e s `Exception.catch` \x -> runGame (h x) e s
  mask k = Game $ \e s -> Exception.mask $ \u -> runGame (k $ q u) e s
    where q u (Game m) = Game $ \ e s -> u (m e s)
  uninterruptibleMask k = Game $ \e s -> Exception.uninterruptibleMask $ \u -> runGame (k $ q u) e s
    where q u (Game m) = Game $ \ e s -> u (m e s)

infix 0 <?>

(<?>) :: Game a -> String -> Game a
Game m <?> l = Game $ \ e s -> m e s `Exception.catch`
  \(GameException ls msg) -> Exception.throwIO $ GameException (l:ls) msg
