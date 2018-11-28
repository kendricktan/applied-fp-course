{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Level08.AppM where

import           Control.Applicative    (liftA2)
import           Control.Lens           (Lens')
import qualified Control.Lens           as L

import           Control.Monad.Except   (ExceptT (..), MonadError (..),
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..), ReaderT (..),
                                         runReaderT)
import           Database.SQLite.Simple (Connection)

import           Data.Text              (Text)

import           Level08.Types          (Conf (..), DBFilePath, FirstAppDB (..),
                                         Port)
import           Level08.Types.Error    (AsError, Error (..))

data Env = Env
  { envLoggingFn :: Text -> AppM ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

defaultEnvLogger :: Text -> AppM ()
defaultEnvLogger d = liftIO $ print d

newtype AppM a = AppM (ReaderT Env (ExceptT Error IO) a)
  deriving (Applicative, Monad, MonadReader Env, MonadError Error, Functor, MonadIO)

type AsAppM e c m =
  ( AsError e
  , HasEnv c
  , MonadIO m
  , MonadReader c m
  , MonadError e m
  )

runAppM :: AppM a -> Env -> IO (Either Error a)
runAppM (AppM a) e = runExceptT $ runReaderT a e

liftEither :: (AsAppM e c m) => Either e a -> m a
liftEither = either throwError return


class HasConf t where
  conf :: Lens' t Conf

  port :: Lens' t Port
  port = conf . go
    where go f (Conf p f') = (\p' -> Conf p' f') <$> f p

  dbFilePath :: Lens' t DBFilePath
  dbFilePath = conf . go
    where go f (Conf p f') = (\f'' -> Conf p f'') <$> f f'

instance HasConf Conf where
  conf = id


class HasFirstAppDB t where
  firstAppDb :: Lens' t FirstAppDB

  dbconn :: Lens' t Connection
  dbconn = firstAppDb . go
    where go f (FirstAppDB c) = (\c' -> FirstAppDB c') <$> f c

instance HasFirstAppDB FirstAppDB where
  firstAppDb = id

class HasEnv t where
  env :: Lens' t Env

  envconfig :: Lens' t Conf
  envconfig = env . go
    where go f (Env l c d) = (\c' -> Env l c' d) <$> f c

  envdb :: Lens' t FirstAppDB
  envdb = env . go
    where go f (Env l c d) = (\d' -> Env l c d') <$> f d

instance HasEnv Env where
  env = id
