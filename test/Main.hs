{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (for_)
import qualified Data.Text.IO as T
import Test.Hspec
import Text.Org
import Text.Org.PrettyPrint

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

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

main :: IO ()
main =
  hspec $
  describe "printing" $
  for_ [(basicHeadlinesDocument, "test/ref/basic-headlines.org")] $ \(document, filePath) ->
    it ("should match the expected output in " ++ filePath) $ do
      fileText <- T.readFile filePath
      printDocument document `shouldBe` fileText
