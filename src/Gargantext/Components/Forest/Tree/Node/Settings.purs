module Gargantext.Components.Forest.Tree.Node.Settings where

import Prelude (class Eq, class Show, show, (&&), (<>), (==))
import Data.Array (foldl)
import Gargantext.Types

------------------------------------------------------------------------
------------------------------------------------------------------------
{-
-- | RIGHT Management
if user has access to node then he can do all his related actions
-}
------------------------------------------------------------------------
------------------------------------------------------------------------
data Status a = IsBeta a | IsProd a

data NodeAction = Documentation NodeType
                | SearchBox
                | Download | Upload | Refresh | Config
                | Move     | Clone  | Delete
                | Share    | Link NodeType
                | Add (Array NodeType)
                | CopyFromCorpus


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
  eq (Link x) (Link y) = (x == y)
  eq (Add  x) (Add  y) = (x == y)
  eq CopyFromCorpus CopyFromCorpus = true
  eq Config Config     = true
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
  show Config            = "Config"
  show (Link x)          = "Link to " <> show x
  show (Add xs)          = foldl (\a b -> a <> show b) "Add " xs
  show CopyFromCorpus    = "Copy from corpus"


glyphiconNodeAction :: NodeAction -> String
glyphiconNodeAction (Documentation _) = "question-circle"
glyphiconNodeAction Delete            = "trash"
glyphiconNodeAction (Add _)           = "plus"
glyphiconNodeAction SearchBox         = "search"
glyphiconNodeAction Upload            = "upload"
glyphiconNodeAction (Link _)          = "arrows-h"
glyphiconNodeAction Download          = "download"
glyphiconNodeAction CopyFromCorpus    = "random"
glyphiconNodeAction Refresh           = "refresh"
glyphiconNodeAction Config            = "wrench"
glyphiconNodeAction Share             = "user-plus"
glyphiconNodeAction _                 = ""


------------------------------------------------------------------------
data SettingsBox =
  SettingsBox { show    :: Boolean
              , edit    :: Boolean
              , doc     :: NodeAction
              , buttons :: Array NodeAction
              }
------------------------------------------------------------------------

settingsBox :: NodeType -> SettingsBox
settingsBox NodeUser =
  SettingsBox { show : true
              , edit : false
              , doc  : Documentation NodeUser
              , buttons : [ Delete ]
              }

settingsBox FolderPrivate =
  SettingsBox { show : true
              , edit : false
              , doc  : Documentation FolderPrivate
              , buttons : [ Add [ Corpus
                                , Folder
                                , Annuaire
                                ]
                          ]
              }

settingsBox Team =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Team
              , buttons : [ Add [ Corpus
                                , Folder
                                , Annuaire
                                ]
                          , Share
                          , Delete]
              }

settingsBox FolderShared =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation FolderShared
              , buttons : [ Add [Team, FolderShared]
                          -- , Delete
                          ]
              }

settingsBox FolderPublic =
  SettingsBox { show : true
              , edit : false
              , doc  : Documentation FolderPublic
              , buttons : [ Add [ Corpus
                                , Folder
                                ]
                ]
              }

settingsBox Folder =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Folder
              , buttons : [ Add [ Corpus
                                , Folder
                                , Annuaire
                                ]
                          , Delete
                          ]
              }

settingsBox Corpus =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Corpus
              , buttons : [ SearchBox
                          , Add [ NodeList
                                , Graph
                                , Dashboard
                                ]
                          , Upload
                          , Download
                            --, Move
                            --, Clone
                          , Link Annuaire
                          , Delete
                          ]
              }

settingsBox Texts =
  SettingsBox { show : true
              , edit : false
              , doc  : Documentation Texts
              , buttons : [ Refresh
                          , Upload
                          , Download
                          -- , Delete
                          ]
              }

settingsBox Graph =
  SettingsBox { show : true
              , edit : false
              , doc  : Documentation Graph
              , buttons : [ Refresh
                          , Config
                          , Download -- TODO as GEXF or JSON
                          , Delete
                          ]
              }

settingsBox NodeList =
  SettingsBox { show : true
              , edit : false
              , doc  : Documentation NodeList
              , buttons : [ Refresh
                          , Config
                          , Download
                          , Upload
                          , CopyFromCorpus
                          , Delete
                          ]
              }

settingsBox Dashboard =
  SettingsBox { show : true
              , edit : false
              , doc  : Documentation Dashboard
              , buttons : []
              }

settingsBox Annuaire =
  SettingsBox { show : true
              , edit : false
              , doc  : Documentation Annuaire
              , buttons : [ Upload
                          , Delete
                          ]
              }

settingsBox _ =
  SettingsBox { show : false
              , edit : false
              , doc  : Documentation NodeUser
              , buttons : []
              }
