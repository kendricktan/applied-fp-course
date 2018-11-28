{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Level08.Types.Topic (Topic, mkTopic, getTopic) where

import           Level08.Types.Error (AsError (..), Error (EmptyTopic),
                                      nonEmptyText)

import           Data.Aeson          (ToJSON)
import           Data.Text           (Text)

newtype Topic = Topic Text
  deriving (Show, ToJSON)

mkTopic :: (AsError e) => Text -> Either e Topic
mkTopic = nonEmptyText Topic _EmptyTopic

getTopic :: Topic -> Text
getTopic (Topic t) = t
