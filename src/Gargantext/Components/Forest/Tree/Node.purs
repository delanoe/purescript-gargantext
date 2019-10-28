module Gargantext.Components.Forest.Tree.Node where

import Prelude
import Gargantext.Types
import Effect.Uncurried (mkEffectFn1)
-- import Data.Set
import Data.Array (filter)
import Reactix.DOM.HTML as H
import Effect.Aff (Aff, launchAff, runAff)


{-
-- | TODO use Set (needs Ord instance for NodeType)
add :: Array NodeType -> NodeAction
add = Add <<< fromFoldable

-- | TODO
filterWithRights (show action if user can only)

-}

data SettingsBox =
  SettingsBox { show    :: Boolean
              , edit    :: Boolean
              , buttons :: Array NodeAction
              }

settingsBox :: NodeType -> SettingsBox
settingsBox NodeUser = SettingsBox { show : true
                                   , edit : false
                                   , buttons : [Documentation NodeUser
                                               , SearchBox
                                               , Add [ FolderPrivate
                                                     , FolderShared
                                                     , FolderPublic
                                                     ]
                                               , Delete
                                               ]
                                   }

settingsBox FolderPrivate = SettingsBox { show: true
                                        , edit : false
                                        , buttons : [Documentation FolderPrivate
                                                    , SearchBox
                                                    , Add [ Corpus
                                                          , Folder
                                                          ]
                                                    , Delete]
                                        }

settingsBox FolderShared = SettingsBox { show: true
                                        , edit : false
                                        , buttons : [Documentation FolderShared
                                                    --, Team
                                                    , Delete]
                                        }

settingsBox FolderPublic = SettingsBox { show: true
                                        , edit : false
                                        , buttons : [Documentation FolderPublic
                                                    , SearchBox
                                                    , Add [ Corpus
                                                          , Folder
                                                          ]
                                                    , Delete
                                                    ]
                                        }

settingsBox Folder = SettingsBox { show : true
                                 , edit : true
                                 , buttons : [ Documentation Folder
                                             , SearchBox
                                             , Add [ Corpus
                                                   , Folder
                                                   ]
                                             , Delete
                                             ]
                                 }

settingsBox Corpus = SettingsBox { show : true
                                 , edit : true
                                 , buttons : [ Documentation Corpus
                                             , SearchBox
                                             , Add [ NodeList 
                                                   , Graph
                                                   , Dashboard
                                                   ]
                                             , Upload
                                             , Download
                                             , Share
                                             , Move
                                             , Clone
                                             , Delete
                                             ]
                                 }

settingsBox Texts = SettingsBox { show : true
                                , edit : false
                                , buttons : [ Documentation Texts
                                            , Upload
                                            , Download
                                            ]
                                }

settingsBox Graph = SettingsBox { show : true
                                , edit : false
                                , buttons : [ Documentation Graph
                                            , Download
                                            ]
                                }

settingsBox NodeList = SettingsBox { show : true
                                   , edit : false
                                   , buttons : [ Documentation NodeList
                                               , Upload
                                               , Download
                                               ]
                                }

settingsBox Dashboard = SettingsBox { show : true
                                   , edit : false
                                   , buttons : [ Documentation Dashboard
                                               ]
                                }


settingsBox _ = SettingsBox { show : false
                            , edit : false
                            , buttons : []
                          }


------------------------------------------------------------------------
data NodeAction = Documentation NodeType
                | SearchBox
                | Download | Upload | Refresh
                | Move     | Clone  | Delete
                | Share    | Team
                | Add (Array NodeType)


instance eqNodeAction :: Eq NodeAction where
  eq (Documentation x) (Documentation y) = true && (x == y)
  eq SearchBox SearchBox = true
  eq Download Download = true
  eq Upload Upload     = true
  eq Refresh Refresh   = true
  eq Move Move         = true
  eq Clone Clone       = true
  eq Delete Delete     = true
  eq Share Share       = true
  eq Team  Team        = true
  eq (Add x) (Add y)   = true && (x == y)
  eq _ _               = false

