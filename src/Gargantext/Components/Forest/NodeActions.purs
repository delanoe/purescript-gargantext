module Gargantext.Components.Forest.NodeActions where

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
              , add     :: Array NodeType
              , buttons :: Array NodeAction
              }

settingsBox :: NodeType -> SettingsBox
settingsBox NodeUser = SettingsBox { show : true
                                   , edit : false
                                   , add  : [ FolderPrivate
                                            , FolderShared
                                            , FolderPublic
                                            ]
                                   , buttons : [Documentation NodeUser
                                               , Delete
                                               ]
                                   }

settingsBox FolderPrivate = SettingsBox { show: true
                                        , edit : false
                                        , add  : [ Folder
                                                 , Corpus
                                                 ]
                                        , buttons : [Documentation FolderPrivate, Delete]
                                        }

settingsBox FolderShared = SettingsBox { show: true
                                        , edit : false
                                        , add  : [ Folder
                                                 , Corpus
                                                 ]
                                        , buttons : [Documentation FolderShared, Delete]
                                        }

settingsBox FolderPublic = SettingsBox { show: true
                                        , edit : false
                                        , add  : [ Folder
                                                 , Corpus
                                                 ]
                                        , buttons : [Documentation FolderPublic, Delete]
                                        }

settingsBox Folder = SettingsBox { show : true
                                 , edit : true
                                 , add  : [ Folder
                                          , Corpus
                                          ]
                                 , buttons : [Documentation Folder, Delete]
                                 }

settingsBox Corpus = SettingsBox { show : true
                                 , edit : true
                                 , add  : [ NodeList
                                          , Dashboard
                                          , Graph
                                          , Phylo
                                          ]
                                 , buttons : [ Documentation Corpus
                                             , Search
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
                                , add : []
                                , buttons : [ Documentation Texts
                                            , Upload
                                            , Download
                                            ]
                                }

settingsBox Graph = SettingsBox { show : true
                                , edit : false
                                , add : [Graph]
                                , buttons : [ Documentation Graph
                                            , Upload
                                            , Download
                                            ]
                                }

settingsBox NodeList = SettingsBox { show : true
                                   , edit : false
                                   , add : [NodeList]
                                   , buttons : [ Documentation NodeList
                                               , Upload
                                               , Download
                                               ]
                                }

settingsBox Dashboard = SettingsBox { show : true
                                   , edit : false
                                   , add : []
                                   , buttons : [ Documentation Dashboard
                                               ]
                                }




settingsBox _ = SettingsBox { show : false
                            , edit : false
                            , add : []
                            , buttons : []
                          }


------------------------------------------------------------------------
data NodeAction = Documentation NodeType
                | Search
                | Download | Upload | Refresh
                | Move     | Clone  | Delete
                | Share

data ButtonType = Click | Pop


instance eqButtonType :: Eq ButtonType where
  eq Click Click = true
  eq Pop   Pop   = true
  eq _     _     = false


buttonType :: NodeAction -> ButtonType
buttonType Search  = Pop
buttonType _       = Click


data Buttons = Buttons { click :: Array NodeAction
                       , pop   :: Array NodeAction
                       }

{-
buttons nt = Buttons {click, pop}
  where
    click = init
    pop   = rest
    {init, rest} = span buttonType (nodeActions nt)
    -}
---------------------------------------------------------
