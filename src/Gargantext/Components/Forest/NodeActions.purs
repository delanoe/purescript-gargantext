module Gargantext.Components.Forest.NodeActions where

import Prelude
import Gargantext.Types
import Effect.Uncurried (mkEffectFn1)
-- import Data.Set
import Data.Array (filter)
import Reactix.DOM.HTML as H
import Effect.Aff (Aff, launchAff, runAff)

data NodeAction = Rename
                | Documentation NodeType
                | Add (Array NodeType)
                | Search
                | Download | Upload | Refresh
                | Move     | Clone  | Delete
                | Share
                | Disconnect

data ButtonType = Edit | Click | Pop

instance eqButtonType :: Eq ButtonType where
  eq Edit Edit   = true
  eq Click Click = true
  eq Pop   Pop   = true
  eq _     _     = false


buttonType :: NodeAction -> ButtonType
buttonType Rename  = Edit
buttonType (Add _) = Pop
buttonType Search  = Pop
buttonType _       = Click


data Buttons = Buttons { edit :: Array NodeAction
                       , click :: Array NodeAction
                       , pop   :: Array NodeAction
                       }

buttons nt = Buttons {edit, click, pop}
  where
    edit  = filter' Edit
    click = filter' Click
    pop   = filter' Pop
    filter' b = filter (\a -> buttonType a == b)
                       (nodeActions nt)

{-
-- | TODO use Set (needs Ord instance for NodeType)
add :: Array NodeType -> NodeAction
add = Add <<< fromFoldable

-- | TODO
filterWithRights (show action if user can only)

-}

nodeActions :: NodeType -> Array NodeAction
nodeActions NodeUser      = [ Add [ FolderPrivate
                                  , FolderTeam
                                  , FolderPublic
                                  ]
                            , Disconnect
                            , Delete
                            ]

nodeActions FolderPrivate = [ Add [Folder, Corpus]]
nodeActions FolderTeam    = [ Add [Folder, Corpus]]
nodeActions FolderPublic  = [ Add [Folder, Corpus]]

nodeActions Folder        = [ Add [Corpus], Rename, Delete]

nodeActions Corpus        = [ Rename
                            , Search, Upload, Download
                            , Add [NodeList, Dashboard, Graph, Phylo]
                            , Share, Move , Clone
                            , Delete
                            ]

nodeActions Graph = [Add [Graph], Delete]
nodeActions Texts = [Download, Upload, Delete]

nodeActions _ = []



---------------------------------------------------------

