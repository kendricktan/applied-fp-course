{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Level08.Types.Error where

import           Control.Lens                       (Prism', prism')
import           Control.Monad.Error.Lens           (throwing_)
import           Data.Text                          (Text)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  -- This is our new error constructor.
  | DBError SQLiteResponse
  deriving (Eq, Show)

nonEmptyText :: (AsError e) => (Text -> a) -> Prism' e () -> Text -> Either e a
nonEmptyText _ e "" = throwing_ e
nonEmptyText c _ tx = Right (c tx)


class AsError s where
  _Error :: Prism' s Error

  _UnknownRoute :: Prism' s ()
  _UnknownRoute = _Error . _UnknownRoute

  _EmptyCommentText :: Prism' s ()
  _EmptyCommentText = _Error . _EmptyCommentText

  _EmptyTopic :: Prism' s ()
  _EmptyTopic = _Error . _EmptyTopic

  _DBError :: Prism' s SQLiteResponse
  _DBError = _Error . _DBError

instance AsError Error where
  _Error = id
