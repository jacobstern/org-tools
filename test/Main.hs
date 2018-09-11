{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (for_)
import qualified Data.Text.IO as T
import Test.Hspec
import Text.Org
import Text.Org.PrettyPrint

basicHeadlinesDocument :: Document
basicHeadlinesDocument =
  Document
    { documentChildren =
        HeadlineChildren
          { headlineChildrenExtraLevel = Nothing
          , headlineChildrenHeadlines =
              [ Headline
                  { headlineKeyword = Nothing
                  , headlineTitle = "Hello world"
                  , headlinePropertyDrawer = Nothing
                  , headlineChildren = emptyHeadlineChildren
                  }
              , Headline
                  { headlineKeyword = Nothing
                  , headlineTitle = "Another headline"
                  , headlinePropertyDrawer = Nothing
                  , headlineChildren =
                      HeadlineChildren
                        { headlineChildrenExtraLevel =
                            Just
                              HeadlineChildren
                                { headlineChildrenExtraLevel = Nothing
                                , headlineChildrenHeadlines =
                                    [ Headline
                                        { headlineKeyword = Nothing
                                        , headlineTitle = "Extra outline level"
                                        , headlinePropertyDrawer = Nothing
                                        , headlineChildren =
                                            emptyHeadlineChildren
                                        }
                                    ]
                                }
                        , headlineChildrenHeadlines =
                            [ Headline
                                { headlineKeyword = Nothing
                                , headlineTitle = "Sub headline"
                                , headlinePropertyDrawer = Nothing
                                , headlineChildren = emptyHeadlineChildren
                                }
                            , Headline
                                { headlineKeyword = Nothing
                                , headlineTitle = "Another sub headline"
                                , headlinePropertyDrawer = Nothing
                                , headlineChildren = emptyHeadlineChildren
                                }
                            ]
                        }
                  }
              , Headline
                  { headlineKeyword = Nothing
                  , headlineTitle = "A third headline"
                  , headlinePropertyDrawer = Nothing
                  , headlineChildren =
                      HeadlineChildren
                        { headlineChildrenExtraLevel = Nothing
                        , headlineChildrenHeadlines =
                            [ Headline
                                { headlineKeyword = Nothing
                                , headlineTitle = "Second level"
                                , headlinePropertyDrawer = Nothing
                                , headlineChildren =
                                    HeadlineChildren
                                      { headlineChildrenExtraLevel = Nothing
                                      , headlineChildrenHeadlines =
                                          [ Headline
                                              { headlineKeyword = Nothing
                                              , headlineTitle =
                                                  "Third level headline"
                                              , headlinePropertyDrawer = Nothing
                                              , headlineChildren =
                                                  emptyHeadlineChildren
                                              }
                                          ]
                                      }
                                }
                            , Headline
                                { headlineKeyword = Nothing
                                , headlineTitle =
                                    "Another second level headline"
                                , headlinePropertyDrawer = Nothing
                                , headlineChildren = emptyHeadlineChildren
                                }
                            ]
                        }
                  }
              ]
          }
    }

basicKeywordsDocument :: Document
basicKeywordsDocument =
  Document
    { documentChildren =
        HeadlineChildren
          { headlineChildrenExtraLevel = Nothing
          , headlineChildrenHeadlines =
              [ Headline
                  { headlineKeyword = Just "TODO"
                  , headlineTitle = "Headline with a TODO keyword"
                  , headlinePropertyDrawer = Nothing
                  , headlineChildren = emptyHeadlineChildren
                  }
              , Headline
                  { headlineKeyword = Nothing
                  , headlineTitle = "Another headline"
                  , headlinePropertyDrawer = Nothing
                  , headlineChildren =
                      HeadlineChildren
                        { headlineChildrenExtraLevel = Nothing
                        , headlineChildrenHeadlines =
                            [ Headline
                                { headlineKeyword = Just "TODO"
                                , headlineTitle = "Another TODO keyword"
                                , headlinePropertyDrawer = Nothing
                                , headlineChildren = emptyHeadlineChildren
                                }
                            ]
                        }
                  }
              ]
          }
    }

propertyDrawersDocument :: Document
propertyDrawersDocument =
  Document
    { documentChildren =
        HeadlineChildren
          { headlineChildrenExtraLevel = Nothing
          , headlineChildrenHeadlines =
              [ Headline
                  { headlineKeyword = Nothing
                  , headlineTitle = "CD collection"
                  , headlinePropertyDrawer = Nothing
                  , headlineChildren =
                      HeadlineChildren
                        { headlineChildrenExtraLevel = Nothing
                        , headlineChildrenHeadlines =
                            [ Headline
                                { headlineKeyword = Nothing
                                , headlineTitle = "Classic"
                                , headlinePropertyDrawer =
                                    Just
                                      PropertyDrawer
                                        { propertyDrawerContents =
                                            [ NodeProperty
                                                { nodePropertyAdd = False
                                                , nodePropertyName = "Genres"
                                                , nodePropertyValue =
                                                    Just "Classic"
                                                }
                                            ]
                                        }
                                , headlineChildren =
                                    HeadlineChildren
                                      { headlineChildrenExtraLevel = Nothing
                                      , headlineChildrenHeadlines =
                                          [ Headline
                                              { headlineKeyword = Nothing
                                              , headlineTitle =
                                                  "Goldberg Variations"
                                              , headlinePropertyDrawer =
                                                  Just
                                                    PropertyDrawer
                                                      { propertyDrawerContents =
                                                          [ NodeProperty
                                                              { nodePropertyAdd =
                                                                  False
                                                              , nodePropertyName =
                                                                  "Title"
                                                              , nodePropertyValue =
                                                                  Just
                                                                    "Goldberg Variations"
                                                              }
                                                          , NodeProperty
                                                              { nodePropertyAdd =
                                                                  False
                                                              , nodePropertyName =
                                                                  "Composer"
                                                              , nodePropertyValue =
                                                                  Just
                                                                    "J. S. Bach"
                                                              }
                                                          , NodeProperty
                                                              { nodePropertyAdd =
                                                                  False
                                                              , nodePropertyName =
                                                                  "Artist"
                                                              , nodePropertyValue =
                                                                  Just
                                                                    "Glen Gould"
                                                              }
                                                          , NodeProperty
                                                              { nodePropertyAdd =
                                                                  False
                                                              , nodePropertyName =
                                                                  "Publisher"
                                                              , nodePropertyValue =
                                                                  Just
                                                                    "Deutsche Grammophon"
                                                              }
                                                          , NodeProperty
                                                              { nodePropertyAdd =
                                                                  False
                                                              , nodePropertyName =
                                                                  "NDisks"
                                                              , nodePropertyValue =
                                                                  Just "1"
                                                              }
                                                          , NodeProperty
                                                              { nodePropertyAdd =
                                                                  True
                                                              , nodePropertyName =
                                                                  "Genres"
                                                              , nodePropertyValue =
                                                                  Just "Baroque"
                                                              }
                                                          ]
                                                      }
                                              , headlineChildren =
                                                  emptyHeadlineChildren
                                              }
                                          ]
                                      }
                                }
                            ]
                        }
                  }
              ]
          }
    }

main :: IO ()
main =
  hspec $
  describe "printing" $
  for_
    [ (basicHeadlinesDocument, "test/ref/basic-headlines.org")
    , (basicKeywordsDocument, "test/ref/basic-keywords.org")
    , (propertyDrawersDocument, "test/ref/property-drawers.org")
    ] $ \(document, filePath) ->
    it ("should match the expected output in " ++ filePath) $ do
      fileText <- T.readFile filePath
      printDocument document `shouldBe` fileText
