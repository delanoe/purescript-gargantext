module Gargantext.Components.Forest.Tree.Node.Settings where

import Gargantext.Prelude (class Eq, class Show, show, (&&), (<>), (==))
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeParams(..))
import Gargantext.Types

------------------------------------------------------------------------
------------------------------------------------------------------------
{-
-- | RIGHT Management
if user has access to node then he can do all his related actions
-}
------------------------------------------------------------------------
data NodeAction = Documentation NodeType
                | SearchBox
                | Download | Upload | Refresh | Config
                | Delete
                | Share
                | Publish { subTreeParams :: SubTreeParams }
                | Add    (Array NodeType)
                | Merge { subTreeParams :: SubTreeParams }
                | Move  { subTreeParams :: SubTreeParams }
                | Link  { subTreeParams :: SubTreeParams }
                | Clone
                | AddingContact
                | CloseNodePopover
                | WriteNodesDocuments  -- https://gitlab.iscpif.fr/gargantext/purescript-gargantext/issues/331

------------------------------------------------------------------------
instance Eq NodeAction where
  eq (Documentation x) (Documentation y)     = true && (x == y)
  eq SearchBox SearchBox                     = true
  eq Download Download                       = true
  eq Upload Upload                           = true
  eq Refresh Refresh                         = true
  eq (Move x) (Move y)                       = x == y
  eq Clone Clone                             = true
  eq Delete Delete                           = true
  eq Share Share                             = true
  eq (Link x) (Link y)                       = x == y
  eq (Add  x) (Add  y)                       = x == y
  eq (Merge x) (Merge y)                     = x == y
  eq Config Config                           = true
  eq (Publish x) (Publish y)                 = x == y
  eq AddingContact AddingContact             = true
  eq CloseNodePopover CloseNodePopover       = true
  eq WriteNodesDocuments WriteNodesDocuments = true
  eq _ _                                     = false

instance Show NodeAction where
  show (Documentation x)   = "Documentation of " <> show x
  show SearchBox           = "SearchBox"
  show Download            = "Download"
  show Upload              = "Upload"
  show Refresh             = "Refresh"
  show (Move _)            = "Move with subtree params" -- <> show t
  show Clone               = "Clone"
  show Delete              = "Delete"
  show Share               = "Share"
  show Config              = "Config"
  show (Link _)            = "Link to " -- <> show x
  show (Add _)             = "Add Child" -- foldl (\a b -> a <> show b) "Add " xs
  show (Merge _)           = "Merge with subtree" -- <> show t
  show (Publish _)         = "Publish" -- <> show x
  show AddingContact       = "AddingContact"
  show CloseNodePopover    = "CloseNodePopover"
  show WriteNodesDocuments = "WriteNodesDocuments"

glyphiconNodeAction :: NodeAction -> String
glyphiconNodeAction (Documentation _)   = "question-circle"
glyphiconNodeAction Delete              = "trash"
glyphiconNodeAction (Add _)             = "plus"
glyphiconNodeAction SearchBox           = "search"
glyphiconNodeAction Upload              = "upload"
glyphiconNodeAction (Link _)            = "arrows-h"
glyphiconNodeAction Download            = "download"
glyphiconNodeAction (Merge _)           = "random"
glyphiconNodeAction Refresh             = "refresh"
glyphiconNodeAction Config              = "wrench"
glyphiconNodeAction Share               = "user-plus"
glyphiconNodeAction AddingContact       = "user-plus"
glyphiconNodeAction (Move _)            = "share-square-o"
glyphiconNodeAction (Publish _)         = fldr FolderPublic true
glyphiconNodeAction CloseNodePopover    = "close"
glyphiconNodeAction WriteNodesDocuments = "bars"
glyphiconNodeAction _                   = ""

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
              , buttons : [ Add [ FolderPrivate
                                , FolderShared
                                , FolderPublic
                                ]
                          , Delete
                          ]
              }

settingsBox FolderPrivate =
  SettingsBox { show : true
              , edit : false
              , doc  : Documentation FolderPrivate
              , buttons : [ Add [ Corpus
                                , Folder
                                , Annuaire
                                , NodeFrameWrite
                                , NodeFrameCalc
                                ]
                          , Delete
                          ]
              }

settingsBox Team =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Team
              , buttons : [ Add [ Corpus
                                , Folder
                                , Annuaire
                                , NodeFrameWrite
                                , NodeFrameCalc
                                , NodeFrameNotebook
                                , Team
                                , FolderShared
                                , NodeFrameVisio
                                ]
                          , Share
                          , Delete
                          ]
              }

settingsBox FolderShared =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation FolderShared
              , buttons : [ Add [Team, FolderShared]
                          , Delete
                          ]
              }

settingsBox FolderPublic =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation FolderPublic
              , buttons : [ Add [ FolderPublic ]
                          , Delete
                          ]
              }

settingsBox Folder =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Folder
              , buttons : [ Add [ Corpus
                                , Folder
                                , Annuaire
                                , NodeFrameWrite
                                , NodeFrameCalc
                                ]
                          , Move moveParameters
                          , Delete
                          ]
              }

settingsBox Corpus =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Corpus
              , buttons : [ Upload
                          , SearchBox
                          , Download
                            --, Clone
                          , Add [ NodeTexts
                                , NodeList
                                , Graph
                                , Dashboard
                                , NodeFrameWrite
                                , NodeFrameCalc
                                , Phylo
                                , NodeFrameNotebook
                                ]
                          , Link (linkParams Annuaire)
                          , Move moveParameters
                          , WriteNodesDocuments
                          , Delete
                          ]
              }

settingsBox NodeTexts =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation NodeTexts
              , buttons : [ Refresh
                          , Upload
                          , Download
                          , Delete
                          ]
              }

settingsBox Graph =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Graph
              , buttons : [ Refresh
                          , Config
                          , Download -- TODO as GEXF or JSON
                          -- , Publish publishParams
                          , Delete
                          ]
              }

settingsBox Phylo =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Phylo
              , buttons : [ Refresh
                          , Delete
                          ]
              }



settingsBox (NodePublic Graph) =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Graph
              , buttons : [ Download -- TODO as GEXF or JSON
                          , Delete
                          ]
              }

settingsBox (NodePublic Dashboard) =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Dashboard
              , buttons : [ Delete
                          ]
              }

settingsBox (NodePublic NodeFile) =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation NodeFile
              , buttons : [ Delete
                          ]
              }



settingsBox (NodePublic FolderPublic) =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation FolderPublic
              , buttons : [ Add [FolderPublic]
                          , Delete
                          ]
              }


settingsBox NodeList =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation NodeList
              , buttons : [ Refresh
                          , Config
                          , Download
                          , Upload
                          , Merge {subTreeParams : SubTreeParams { showtypes: [ FolderPrivate
                                                                , FolderShared
                                                                , Team
                                                                , FolderPublic
                                                                , Folder
                                                                , Corpus
                                                                , NodeList
                                                                ]
                                                    , valitypes: [ NodeList ]
                                                    }
                                   }
                          , Delete
                          ]
              }

settingsBox Dashboard =
  SettingsBox { show : true
              , edit : false
              , doc  : Documentation Dashboard
              , buttons : [ Refresh
                          , Publish publishParams
                          , Delete
                          ]
              }

settingsBox Annuaire =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation Annuaire
              , buttons : [ Upload
                          , AddingContact
                          , Move moveParameters
                          , Link (linkParams Corpus)
                          , Delete
                          ]
              }


settingsBox NodeFrameWrite =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation NodeFrameWrite
              , buttons : [ Add [ NodeFrameWrite
                                , NodeFrameCalc
                                ]
                          , Move moveFrameParameters
                          , Delete
                          ]
              }


settingsBox NodeFrameCalc =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation NodeFrameCalc
              , buttons : [ Upload
                          , Add [ NodeFrameCalc
                                , NodeFrameWrite
                                ]
                          , Move moveFrameParameters
                          , Delete
                          ]
              }

settingsBox NodeFrameNotebook =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation NodeFrameNotebook
              , buttons : [ Add [ NodeFrameCalc
                                , NodeFrameWrite
                                , NodeFrameNotebook
                                ]
                          , Move moveFrameParameters
                          , Delete
                          ]
              }


settingsBox NodeFrameVisio =
  SettingsBox { show : true
              , edit : true
              , doc  : Documentation NodeFrameVisio
              , buttons : [ Add [ NodeFrameVisio
                                , NodeFrameWrite
                                , NodeFrameCalc
                                ]
                          , Delete
                          ]
              }


settingsBox NodeFile =
  SettingsBox { show: true
              , edit: true
              , doc: Documentation NodeFile
              , buttons: [ Publish publishParams
                         , Delete ]
              }


settingsBox _ =
  SettingsBox { show : false
              , edit : false
              , doc  : Documentation NodeUser
              , buttons : []
              }

-- | SubTree Parameters

moveParameters = { subTreeParams : SubTreeParams
                                 { showtypes: [ FolderPrivate
                                              , FolderShared
                                              , Team
                                              , FolderPublic
                                              , Folder
                                              ]
                                 , valitypes: [ FolderPrivate
                                              , Team
                                              -- , FolderPublic
                                              , Folder
                                              ]
                                 }
                  }


moveFrameParameters = { subTreeParams : SubTreeParams
                                 { showtypes: [ FolderPrivate
                                              , FolderShared
                                              , Team
                                              , FolderPublic
                                              , Folder
                                              , Corpus
                                              , NodeFrameWrite
                                              , NodeFrameCalc
                                              ]
                                 , valitypes: [ FolderPrivate
                                              , Team
                                              -- , FolderPublic
                                              , Folder
                                              , Corpus
                                              , NodeFrameWrite
                                              , NodeFrameCalc
                                              ]
                                 }
                  }


linkParams :: NodeType -> {subTreeParams :: SubTreeParams}
linkParams nodeType =  { subTreeParams : SubTreeParams
                              { showtypes: [ FolderPrivate
                                           , FolderShared
                                           , Team
                                           , FolderPublic
                                           , Folder
                                           , nodeType
                                           ]
                              , valitypes: [ nodeType
                                           ]
                              }
                      }



publishParams =  { subTreeParams : SubTreeParams
                              { showtypes: [ FolderPublic
                                           ]
                              , valitypes: [ FolderPublic
                                           ]
                              }
               }
