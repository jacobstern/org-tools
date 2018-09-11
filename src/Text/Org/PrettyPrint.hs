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
  pure headline <> foldMap printHeadlinePropertyDrawer headlinePropertyDrawer <>
  printHeadlineChildren outlineLevel headlineChildren
  where
    headline =
      T.unwords $
      catMaybes
        [ pure $ T.replicate outlineLevel "*"
        , headlineKeyword
        , pure headlineTitle
        ]
    printHeadlinePropertyDrawer = printPropertyDrawer outlineLevel

propertyDrawerIndentation :: OutlineLevel -> Text
propertyDrawerIndentation outlineLevel = T.replicate (outlineLevel + 1) " "

printNodeProperty :: OutlineLevel -> NodeProperty -> Lines
printNodeProperty outlineLevel NodeProperty {..} =
  pure $
  indentation <> ":" <> propertyKey <> ":" <> valueIndentation <>
  fold nodePropertyValue
  where
    valueIndentation = T.replicate valueIndentationSpacing " "
    -- Align with end of line that begins drawer
    valueIndentationSpacing = max 1 (9 - T.length propertyKey)
    propertyKey =
      if nodePropertyAdd
        then nodePropertyName <> "+"
        else nodePropertyName
    indentation = propertyDrawerIndentation outlineLevel

printPropertyDrawer :: OutlineLevel -> PropertyDrawer -> Lines
printPropertyDrawer outlineLevel PropertyDrawer {..} =
  pure propertiesBegin <>
  foldMap printContentsNodeProperty propertyDrawerContents <>
  pure propertiesEnd
  where
    propertiesBegin = indentation <> ":PROPERTIES:"
    propertiesEnd = indentation <> ":END:"
    indentation = propertyDrawerIndentation outlineLevel
    printContentsNodeProperty = printNodeProperty outlineLevel

printHeadlineChildren :: OutlineLevel -> HeadlineChildren -> Lines
printHeadlineChildren outlineLevel HeadlineChildren {..} =
  foldMap printSkipChildren headlineChildrenExtraLevel <>
  foldMap printChild headlineChildrenHeadlines
  where
    printSkipChildren = printHeadlineChildren $ outlineLevel + 1
    printChild = printHeadline $ outlineLevel + 1
