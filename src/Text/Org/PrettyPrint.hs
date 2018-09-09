{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Org.PrettyPrint
  ( printDocument
  ) where

import Data.Foldable (fold)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Org.Types
  ( Document(..)
  , Headline(..)
  , HeadlineChildren(..)
  , NodeProperty(..)
  , NodeProperty(..)
  , PropertyDrawer(..)
  )

type OutlineLevel = Int

type Lines = [Text]

printDocument :: Document -> Text
printDocument Document {..} =
  T.unlines $ printHeadlineChildren 0 documentChildren

printHeadline :: OutlineLevel -> Headline -> Lines
printHeadline outlineLevel Headline {..} =
  pure headline <> foldMap printPropertyDrawer headlinePropertyDrawer <>
  printHeadlineChildren outlineLevel headlineChildren
  where
    headline =
      T.unwords $
      catMaybes
        [ pure $ T.replicate outlineLevel "*"
        , headlineKeyword
        , pure headlineTitle
        ]

printNodeProperty :: NodeProperty -> Lines
printNodeProperty NodeProperty {..} =
  pure $ ":" <> propertyKey <> ": " <> fold nodePropertyValue
  where
    propertyKey =
      if nodePropertyAdd
        then nodePropertyName <> "+"
        else nodePropertyName

printPropertyDrawer :: PropertyDrawer -> Lines
printPropertyDrawer PropertyDrawer {..} =
  pure ":PROPERTIES:" <> foldMap printNodeProperty propertyDrawerContents <>
  pure ":END:"

printHeadlineChildren :: OutlineLevel -> HeadlineChildren -> Lines
printHeadlineChildren outlineLevel HeadlineChildren {..} =
  foldMap printSkipChildren headlineChildrenExtraLevel <>
  foldMap printChild headlineChildrenHeadlines
  where
    printSkipChildren = printHeadlineChildren $ outlineLevel + 1
    printChild = printHeadline $ outlineLevel + 1
