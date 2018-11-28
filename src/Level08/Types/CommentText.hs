{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Level08.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  ) where

import           Level08.Types.Error (AsError (..), Error (EmptyCommentText),
                                      nonEmptyText)

import           Data.Aeson          (ToJSON)
import           Data.Text           (Text)

newtype CommentText = CommentText Text
  deriving (Show, ToJSON)

mkCommentText :: (AsError e) => Text -> Either e CommentText
mkCommentText = nonEmptyText CommentText _EmptyCommentText

getCommentText :: CommentText -> Text
getCommentText (CommentText t) = t
