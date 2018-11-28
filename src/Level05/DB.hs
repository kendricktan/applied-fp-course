{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (..), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM (..), liftEither)
import           Level05.DB.Types                   (DBComment)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB :: FirstAppDB -> IO ()
closeDB = Sql.close . dbConn

initDB :: FilePath -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB :: (a -> Either Error b) -> IO a -> AppM b
runDB f a = AppM $ f <$> a

getComments :: FirstAppDB -> Topic -> AppM [Comment]
getComments (FirstAppDB conn) t =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DBComment
  -- cannot be converted to a Comment, or simply ignoring any DBComment that is
  -- not valid.
  in AppM $ do
    r <- Sql.query conn sql (Sql.Only (getTopic t :: Text)) :: IO [DBComment]
    return . sequence $ fromDBComment <$> r

addCommentToTopic :: FirstAppDB -> Topic -> CommentText -> AppM ()
addCommentToTopic (FirstAppDB conn) t ct =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    topicText = getTopic t
    commentText = getCommentText ct
   in AppM $ do
    time <- getCurrentTime
    r <- Sql.runDBAction $ Sql.execute conn sql (topicText, commentText, time)
    return $ first DBError r


getTopics :: FirstAppDB -> AppM [Topic]
getTopics (FirstAppDB conn) =
  let
    sql = "SELECT DISTINCT topic FROM comments"
   in AppM $ do
     t <- Sql.query_ conn sql :: IO [Topic]
     case length t of
       0 -> return $ Left EmptyTopic
       _ -> return $ Right t


deleteTopic :: FirstAppDB -> Topic -> AppM ()
deleteTopic (FirstAppDB conn) t =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    t' = getTopic t
   in AppM $ do
    r <- Sql.runDBAction $ Sql.execute conn sql (Sql.Only t')
    return $ first DBError r

-- Go to 'src/Level05/Core.hs' next.
