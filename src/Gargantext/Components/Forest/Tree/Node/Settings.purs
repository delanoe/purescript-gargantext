module Gargantext.Components.Forest.Tree.Node.Settings where

import Data.Default (class Default, def)
import Data.Lens (Lens', lens, (.~))
import Gargantext.Prelude (class Eq, class Show, show, (&&), (<>), (==), ($), (<<<))
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeParams(..))
import Gargantext.Types

------------------------------------------------------------------------
------------------------------------------------------------------------
{-
-- | RIGHT Management
if user has access to node then he can do all his related actions
-}
------------------------------------------------------------------------
data NodeAction = Add    (Array NodeType)
                | AddingContact
                | CloseNodePopover
                | Config 
                | Delete
                | Documentation NodeType
                | Download 
                | Link  { subTreeParams :: SubTreeParams }
                | ManageTeam
                | Merge { subTreeParams :: SubTreeParams }
                | Move  { subTreeParams :: SubTreeParams }
                | Publish { subTreeParams :: SubTreeParams }
                | Refresh 
                | ReloadWithSettings
                | Reconstruct
                | SearchBox
                | Share
                | Upload 
                | WriteNodesDocuments  -- https://gitlab.iscpif.fr/gargantext/purescript-gargantext/issues/331

------------------------------------------------------------------------
instance Eq NodeAction where
  eq (Add  x) (Add  y)                       = x == y
  eq AddingContact AddingContact             = true
  eq CloseNodePopover CloseNodePopover       = true
  eq Config Config                           = true
  eq Delete Delete                           = true
  eq (Documentation x) (Documentation y)     = true && (x == y)
  eq Download Download                       = true
  eq (Link x) (Link y)                       = x == y
  eq ManageTeam ManageTeam                   = true
  eq (Merge x) (Merge y)                     = x == y
  eq (Move x) (Move y)                       = x == y
  eq (Publish x) (Publish y)                 = x == y
  eq Reconstruct Reconstruct                 = true
  eq Refresh Refresh                         = true
  eq ReloadWithSettings ReloadWithSettings   = true
  eq SearchBox SearchBox                     = true
  eq Share Share                             = true
  eq Upload Upload                           = true
  eq WriteNodesDocuments WriteNodesDocuments = true
  eq _ _                                     = false

instance Show NodeAction where
  show (Add _)             = "Add Child" -- foldl (\a b -> a <> show b) "Add " xs
  show AddingContact       = "AddingContact"
  show CloseNodePopover    = "CloseNodePopover"
  show Config              = "Config"
  show Delete              = "Delete"
  show (Documentation x)   = "Documentation of " <> show x
  show Download            = "Download"
  show (Link _)            = "Link to " -- <> show x
  show ManageTeam          = "Team"
  show (Merge _)           = "Merge with subtree" -- <> show t
  show (Move _)            = "Move with subtree params" -- <> show t
  show (Publish _)         = "Publish" -- <> show x
  show Reconstruct         = "Reconstruct"
  show Refresh             = "Refresh"
  show ReloadWithSettings  = "Reload (with settings)"
  show SearchBox           = "SearchBox"
  show Share               = "Share"
  show Upload              = "Upload"
  show WriteNodesDocuments = "WriteNodesDocuments"

glyphiconNodeAction :: NodeAction -> String
glyphiconNodeAction (Add _)             = "plus"
glyphiconNodeAction AddingContact       = "user-plus"
glyphiconNodeAction CloseNodePopover    = "close"
glyphiconNodeAction Config              = "wrench"
glyphiconNodeAction Delete              = "trash"
glyphiconNodeAction (Documentation _)   = "question-circle"
glyphiconNodeAction Download            = "download"
glyphiconNodeAction (Link _)            = "arrows-h"
glyphiconNodeAction ManageTeam          = "users"
glyphiconNodeAction (Merge _)           = "random"
glyphiconNodeAction (Move _)            = "share-square-o"
glyphiconNodeAction (Publish _)         = fldr FolderPublic true
glyphiconNodeAction Reconstruct         = "cogs"
glyphiconNodeAction Refresh             = "refresh"
glyphiconNodeAction ReloadWithSettings  = "reload-with-settings"
glyphiconNodeAction SearchBox           = "search"
glyphiconNodeAction Share               = "user-plus"
glyphiconNodeAction Upload              = "upload"
glyphiconNodeAction WriteNodesDocuments = "bars"
glyphiconNodeAction _                   = ""

------------------------------------------------------------------------
data SettingsBox =
  SettingsBox { show    :: Boolean
              , edit    :: Boolean
              , doc     :: NodeAction
              , buttons :: Array NodeAction
              }
instance Default SettingsBox where
  def = SettingsBox { show: true
                    , edit: true
                    , doc: Documentation Annuaire
                    , buttons: []
                    }

defNt :: NodeType -> SettingsBox
defNt nt = (_doc .~ Documentation nt) def
        
_show :: Lens' SettingsBox Boolean
_show = lens (\(SettingsBox { show }) -> show) (\(SettingsBox sb) val -> SettingsBox (sb { show = val }))

_edit :: Lens' SettingsBox Boolean
_edit = lens (\(SettingsBox { edit }) -> edit) (\(SettingsBox sb) val -> SettingsBox (sb { edit = val }))

_doc :: Lens' SettingsBox NodeAction
_doc = lens (\(SettingsBox { doc }) -> doc) (\(SettingsBox sb) val -> SettingsBox (sb { doc = val }))

_buttons :: Lens' SettingsBox (Array NodeAction)
_buttons = lens (\(SettingsBox { buttons }) -> buttons)
           (\(SettingsBox sb) val -> SettingsBox (sb { buttons = val }))
------------------------------------------------------------------------
  
settingsBox :: NodeType -> SettingsBox
settingsBox p = (settingsBoxLens p) $ defNt p

settingsBoxLens :: NodeType -> (SettingsBox -> SettingsBox)
settingsBoxLens Annuaire =
  _buttons .~ [ Upload
              , AddingContact
              , Move moveParameters
              , Link (linkParams Corpus)
              , Delete ]
settingsBoxLens Calc =
  _buttons .~ [ Upload
              , Add [ Calc
                    , Notes ]
              , Move moveFrameParameters
              , Delete ]
settingsBoxLens Corpus =
  _buttons .~ [ Add [ Graph
                    , Notes
                    , Calc
                    , NodeTexts
                    , NodeList
                    , Graph
                      -- , Dashboard
                    , Phylo
                      -- , NodeFrameNotebook
                    ]
              , Move moveParameters
              , Upload
              , SearchBox
              , WriteNodesDocuments
                -- , Download
              , Link (linkParams Annuaire)
              , Delete
              ]
settingsBoxLens Dashboard =
    (_edit .~ false) <<<
    (_buttons .~ [ ReloadWithSettings
                 , Publish publishParams
                 , Delete
                 ])
settingsBoxLens Folder =
  _buttons .~ [ Add [ Notes
                    , Corpus
                    , Calc
                    , Folder
                    , Annuaire
                      -- , NodeFrameNotebook
                    ]
              , Move moveParameters
              , Delete
              ]
settingsBoxLens FolderPrivate =
  (_edit .~ false) <<<
  (_buttons .~ [ Add [ Notes
                     , Corpus
                     , Calc
                     , Folder
                     , Annuaire
                       -- , NodeFrameNotebook
                     ]
               ])
settingsBoxLens FolderPublic =
  _buttons .~ [ Add [ FolderPublic ]
              ]
settingsBoxLens FolderShared =
  _buttons .~ [ Add [Team, FolderShared]
              ]
settingsBoxLens Graph =
  _buttons .~ [ ReloadWithSettings
              , Config
              , Download -- TODO as GEXF or JSON
                -- , Publish publishParams
              , Delete
              ]
settingsBoxLens NodeFile =
  _buttons .~ [ Publish publishParams
              , Delete ]
settingsBoxLens NodeFrameNotebook =
  _buttons .~ [ Add [ Calc
                    , Notes
                      -- , NodeFrameNotebook
                    ]
              , Move moveFrameParameters
              , Delete
              ]
settingsBoxLens NodeFrameVisio =
  _buttons .~ [ Add [ NodeFrameVisio
                    , Notes
                    , Calc
                    ]
              , Delete
              ]
settingsBoxLens NodeList =
  _buttons .~ [ ReloadWithSettings
              , Config
              , Upload
              , Download
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
settingsBoxLens (NodePublic Dashboard) =
  _buttons .~ [ Delete
              ]
settingsBoxLens (NodePublic FolderPublic) =
  _buttons .~ [ Add [FolderPublic]
              , Delete
              ]
settingsBoxLens (NodePublic Graph) =
  _buttons .~ [ Download -- TODO as GEXF or JSON
              , Delete
              ]
settingsBoxLens (NodePublic NodeFile) =
  _buttons .~ [ Delete
              ]
settingsBoxLens NodeTexts =
  _buttons .~ [ ReloadWithSettings
              , Upload
              , Download
              , Delete
              ]
settingsBoxLens NodeUser =
  (_edit .~ false) <<<
  (_buttons .~ [ Delete
               ])
settingsBoxLens Notes =
  _buttons .~ [ Add [ Notes
                    , Calc
                    , Folder
                    , Corpus
                    ]
              , Move moveFrameParameters
              , Delete
              ]
settingsBoxLens Phylo =
  _buttons .~ [ Reconstruct
              , Delete
              ]
settingsBoxLens Team =
  _buttons .~ [ Add [ Notes
                    , Corpus
                    , Calc
                    , Folder
                    , Team
                    , Annuaire
                      -- , NodeFrameNotebook
                      -- , FolderShared
                    , NodeFrameVisio
                    ]
              , Share
              , ManageTeam
              , Delete
              ]
settingsBoxLens _ =
  (_show .~ false) <<<
  (_edit .~ false)


-- | SubTree Parameters

moveParameters = { subTreeParams : SubTreeParams
                                 { showtypes: [ FolderPrivate
                                              , FolderShared
                                              , Team
                                              , FolderPublic
                                              , Folder
                                              , Notes
                                              , Corpus
                                              ]
                                 , valitypes: [ FolderPrivate
                                              , Team
                                              -- , FolderPublic
                                              , Folder
                                              , Notes
                                              , Corpus
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
                                              , Notes
                                              , Calc
                                              ]
                                 , valitypes: [ FolderPrivate
                                              , Team
                                              -- , FolderPublic
                                              , Folder
                                              , Corpus
                                              , Notes
                                              , Calc
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
