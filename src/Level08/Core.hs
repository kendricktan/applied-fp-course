{-# LANGUAGE OverloadedStrings #-}
module Level08.Core
  ( runApp
  , prepareAppReqs
  , app
  ) where

import           Control.Applicative                (liftA2)
import           Control.Monad                      (join)

import           Control.Monad.Error.Lens           (throwing_)
import           Control.Monad.Except               (throwError)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Data.Bifunctor                     (first)
import           Data.Either                        (Either (Left, Right),
                                                     either)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Text.IO                       (hPutStrLn)

import qualified Data.ByteString.Lazy.Char8         as LBS

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           System.IO                          (stderr)

import qualified Level08.Conf                       as Conf
import qualified Level08.DB                         as DB

import qualified Level08.Responses                  as Res
import Level08.Types.Error (AsError (..))
import           Level08.Types                      (Conf (..), ConfigError,
                                                     ContentType (PlainText),
                                                     Error (DBError, EmptyCommentText, EmptyTopic, UnknownRoute),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     confPortToWai,
                                                     getDBFilePath,
                                                     mkCommentText, mkTopic)

import           Level08.AppM                       (AppM, AsAppM, Env (Env, envConfig, envDB, envLoggingFn),
                                                     defaultEnvLogger,
                                                     liftEither, runAppM)

-- We're going to use the `mtl` ExceptT monad transformer to make the loading of our `Conf` a bit more straight-forward.
import           Control.Monad.Except               (ExceptT (..), runExceptT)

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = ConfErr ConfigError
  | DBInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp = do
  appE <- prepareAppReqs
  either print runWithDBConn appE
  where
    runWithDBConn env =
      appWithDB env >> DB.closeDB (envDB env)

    appWithDB env =
      run ( confPortToWai $ envConfig env ) (app env)

-- Reimplement the `prepareAppReqs` function using the imported `ExceptT`
-- constructor to help eliminate the manual plumbing of the error values.
--
-- We'll use the more general version of our error handling monad transformer to
-- demonstrate how easily it can be applied simplify error handling.
prepareAppReqs :: IO (Either StartUpError Env)
prepareAppReqs = runExceptT $ do
  eitherConfig <- liftIO $ Conf.parseOptions "files/appconfig.json"
  case eitherConfig of
    Left err -> throwError $ ConfErr err
    Right conf -> do
      eitherFirstappdb <- liftIO $ DB.initDB (dbFilePath conf)
      case eitherFirstappdb of
          Left err         -> throwError $ DBInitErr err
          Right firstappdb ->  return $ Env defaultEnvLogger conf firstappdb

--  eitherConfg <- Conf.parseOptions "files/Level08.json"
--  case eitherConfg of
--    Left err  -> return $ Left (ConfigError err)
--    Right conf -> do
--      eitherFirstappdb <- DB.initDB (getDBFilePath $ dBFilePath conf)
--      case eitherFirstappdb of
--          Left err         -> return $ Left (DBInitErr err)
--          Right firstappdb ->  return $ Right (conf, firstappdb)


-- Now that our request handling and response creating functions operate
-- within our AppM context, we need to run the AppM to get our IO action out
-- to be run and handed off to the callback function. We've already written
-- the function for this so include the 'runAppM' with the Env.
app :: Env -> Application
app env rq cb = do
  a <- runAppM (do
    rq' <- mkRequest rq
    handleRequest rq') env
  case a of
    Left err  -> cb $ mkErrorResponse err
    Right val -> cb val


handleRequest :: (AsAppM e c m) => RqType -> m Response
handleRequest rqType = case rqType of
  AddRq t c -> Res.resp200 PlainText "Success" <$ DB.addCommentToTopic t c
  ViewRq t  -> Res.resp200Json <$> DB.getComments t
  ListRq    -> Res.resp200Json <$> DB.getTopics

mkRequest :: (AsAppM e c m) => Request -> m RqType
mkRequest rq =
  liftEither =<< case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> liftIO (mkAddRequest t <$> strictRequestBody rq)
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _                      -> pure ( throwing_ _UnknownRoute )

mkAddRequest :: (AsError e) => Text -> LBS.ByteString -> Either e RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest :: (AsError e) => Text -> Either e RqType
mkViewRequest = fmap ViewRq . mkTopic

mkListRequest :: (AsError e) => Either e RqType
mkListRequest = Right ListRq

mkErrorResponse :: Error -> Response
mkErrorResponse UnknownRoute     = Res.resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText = Res.resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic       = Res.resp400 PlainText "Empty Topic"
mkErrorResponse ( DBError _ )    = Res.resp500 PlainText "OH NOES"
  -- Be a sensible developer and don't leak your DB errors over the internet.
