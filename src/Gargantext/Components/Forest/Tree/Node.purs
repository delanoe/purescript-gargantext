module Gargantext.Components.Forest.Tree.Node where

import Prelude
import Data.Array (foldl)
import Gargantext.Types
import Effect.Uncurried (mkEffectFn1)
-- import Data.Set
import Data.Array (filter)
import Reactix.DOM.HTML as H
import Effect.Aff (Aff, launchAff, runAff)

------------------------------------------------------------------------
------------------------------------------------------------------------
{-
-- | TODO
filterWithRights (show action if user can only)

-}
------------------------------------------------------------------------
------------------------------------------------------------------------
data NodeAction = Documentation NodeType
                | SearchBox
                | Download | Upload | Refresh
                | Move     | Clone  | Delete
                | Share    | Link NodeType
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
  eq (Link x) (Link y) = true && (x == y)
  eq (Add x) (Add y)   = true && (x == y)
  eq _ _               = false

instance showNodeAction :: Show NodeAction where
  show (Documentation x) = "Documentation of " <> show x
  show SearchBox         = "SearchBox"
  show Download          = "Download"
  show Upload            = "Upload"
  show Refresh           = "Refresh"
  show Move              = "Move"
  show Clone             = "Clone"
  show Delete            = "Delete"
  show Share             = "Share"
  show (Link x)          = "Link to " <> show x
  show (Add xs)          = foldl (\a b -> a <> show b) "Add " xs


glyphiconNodeAction :: NodeAction -> String
glyphiconNodeAction (Documentation _) = "question-sign"
glyphiconNodeAction Delete            = "trash"
glyphiconNodeAction (Add _)           = "plus"
glyphiconNodeAction SearchBox         = "search"
glyphiconNodeAction Upload            = "upload"
glyphiconNodeAction (Link _)          = "transfer"
glyphiconNodeAction Download          = "download"
glyphiconNodeAction _                 = ""


------------------------------------------------------------------------
------------------------------------------------------------------------

data SettingsBox =
  SettingsBox { show    :: Boolean
              , edit    :: Boolean
              , buttons :: Array NodeAction
              }
------------------------------------------------------------------------

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
                                                          , Annuaire
                                                          ]
                                                    , Delete]
                                        }

settingsBox FolderShared = SettingsBox { show: true
                                       , edit : false
                                       , buttons : [Documentation FolderShared
                                                    -- , Add [Folder, Team]
                                                    , Delete
                                                    ]
                                       }


settingsBox FolderPublic = SettingsBox { show: true
                                       , edit : false
                                       , buttons : [Documentation FolderPublic
                                                    {-, SearchBox
                                                    , Add [ Corpus
                                                          , Folder
                                                          ]-}
                                                    , Delete
                                                    ]
                                        }

settingsBox Folder = SettingsBox { show : true
                                 , edit : true
                                 , buttons : [ Documentation Folder
                                             , SearchBox
                                             , Add [ Corpus
                                                   , Folder
                                                   , Annuaire
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
                                             , Link Annuaire
                                             , Upload
                                             , Download
                                             --, Share
                                             --, Move
                                             --, Clone
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



