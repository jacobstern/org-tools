{-# LANGUAGE DeriveGeneric #-}

module Text.Org.Types
  ( Document(..)
  , Headline(..)
  , HeadlineChildren(..)
  , emptyHeadlineChildren
  , NodeProperty(..)
  , PropertyDrawer(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

{-# ANN Document "HLint: ignore Use newtype instead of data" #-}

type PropertyName = Text

type PropertyValue = Text

data Document = Document
  { documentChildren :: HeadlineChildren
  } deriving (Eq, Show, Ord, Generic)

data NodeProperty = NodeProperty
  { nodePropertyName :: PropertyName
  , nodePropertyValue :: Maybe PropertyValue
  , nodePropertyAdd :: Bool
  } deriving (Eq, Show, Ord, Generic)

newtype PropertyDrawer = PropertyDrawer
  { propertyDrawerContents :: [NodeProperty]
  } deriving (Eq, Show, Ord, Generic)

data Headline = Headline
  { headlineKeyword :: Maybe Text
  , headlineTitle :: Text
  , headlinePropertyDrawer :: Maybe PropertyDrawer
  , headlineChildren :: HeadlineChildren
  } deriving (Eq, Show, Ord, Generic)

data HeadlineChildren = HeadlineChildren
  { headlineChildrenExtraLevel :: Maybe HeadlineChildren
  , headlineChildrenHeadlines :: [Headline]
  } deriving (Eq, Show, Ord, Generic)

emptyHeadlineChildren :: HeadlineChildren
emptyHeadlineChildren = HeadlineChildren Nothing []
