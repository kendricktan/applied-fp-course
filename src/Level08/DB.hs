{-# LANGUAGE OverloadedStrings #-}
module Level08.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Lens
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level08.AppM                       (AppM, AsAppM, Env (envDB),
                                                     HasConf (..), HasEnv (..), HasFirstAppDB (..),
                                                     liftEither)

import           Level08.Types                      (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB :: DBFilePath -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn :: (AsAppM e c m) => m Connection
getDBConn = view (envdb . dbconn)


runDB :: (AsAppM e c m) => (a -> Either e b) -> (Connection -> IO a) -> m b
runDB handleResp handleReq = do
  conn <- getDBConn
  req <- liftIO $ handleReq conn
  liftEither $ handleResp req

getComments :: (AsAppM e c m) => Topic -> m [Comment]
getComments t = do
  -- Write the query with an icky string and remember your placeholders!
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- To be doubly and triply sure we've no garbage in our response, we take care
  -- to convert our DB storage type into something we're going to share with the
  -- outside world. Checking again for things like empty Topic or CommentText values.
  runDB (traverse fromDBComment) (\conn -> Sql.query conn q (Sql.Only . getTopic $ t))

addCommentToTopic :: (AsAppM e c m) => Topic -> CommentText -> m ()
addCommentToTopic t c = do
  nowish <- liftIO getCurrentTime
  -- Note the triple, matching the number of values we're trying to insert, plus
  -- one for the table name.
  let q =
        -- Remember that the '?' are order dependent so if you get your input
        -- parameters in the wrong order, the types won't save you here. More on that
        -- sort of goodness later.
        "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  -- We use the execute function this time as we don't care about anything
  -- that is returned. The execute function will still return the number of rows
  -- affected by the query, which in our case should always be 1.
  runDB Right $ (\conn -> Sql.execute conn q (getTopic t, getCommentText c, nowish))
  -- An alternative is to write a returning query to get the Id of the DBComment
  -- we've created. We're being lazy (hah!) for now, so assume awesome and move on.

getTopics :: (AsAppM e c m) => m [Topic]
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
  in
    runDB (traverse ( mkTopic . Sql.fromOnly )) $ (\conn -> Sql.query_ conn q)

deleteTopic :: (AsAppM e c m) => Topic -> m ()
deleteTopic t =
  let q = "DELETE FROM comments WHERE topic = ?"
  in
    runDB Right $ (\conn -> Sql.execute conn q (Sql.Only . getTopic $ t))

-- Go on to 'src/Level08/Core.hs' next.
