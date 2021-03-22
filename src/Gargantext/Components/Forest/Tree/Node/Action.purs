module Gargantext.Components.Forest.Tree.Node.Action where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut, SubTreeParams(..))
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction(..), glyphiconNodeAction)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType, UploadFileBlob)
import Gargantext.Components.Forest.Tree.Node.Action.Update.Types (UpdateNodeParams)
import Gargantext.Components.Forest.Tree.Node.Action.Contact.Types (AddContactParams)
import Gargantext.Sessions (Session)
import Gargantext.Types  as GT

type Props =
  ( dispatch :: Action -> Aff Unit
  , id       :: Int
  , nodeType :: GT.NodeType
  , session  :: Session
  )


data Action = AddNode     String GT.NodeType
            | DeleteNode  GT.NodeType
            | RenameNode  String
            | UpdateNode  UpdateNodeParams
            | DoSearch    GT.AsyncTaskWithType
            | UploadFile  GT.NodeType FileType (Maybe String) UploadFileBlob
            | UploadArbitraryFile  (Maybe String) UploadFileBlob
            | DownloadNode
            | RefreshTree
            | ClosePopover

            | ShareTeam   String
            | AddContact  AddContactParams
            | SharePublic {params :: Maybe SubTreeOut}
            | MoveNode    {params :: Maybe SubTreeOut}
            | MergeNode   {params :: Maybe SubTreeOut}
            | LinkNode    {nodeType :: Maybe GT.NodeType, params :: Maybe SubTreeOut}

            | NoAction


subTreeOut :: Action -> Maybe SubTreeOut
subTreeOut (MoveNode    {params}) = params
subTreeOut (MergeNode   {params}) = params
subTreeOut (LinkNode    {params}) = params
subTreeOut (SharePublic {params}) = params
subTreeOut _                    = Nothing

setTreeOut ::  Action -> Maybe SubTreeOut -> Action
setTreeOut (MoveNode  {params:_}) p = MoveNode  {params: p}
setTreeOut (MergeNode {params:_}) p = MergeNode {params: p}
setTreeOut (LinkNode  {nodeType, params:_}) p = LinkNode  {nodeType, params: p}
setTreeOut (SharePublic {params:_}) p = SharePublic  {params: p}
setTreeOut a   _             = a


instance showShow :: Show Action where
  show (AddNode     _ _    )      = "AddNode"
  show (DeleteNode  _      )      = "DeleteNode"
  show (RenameNode  _      )      = "RenameNode"
  show (UpdateNode  _      )      = "UpdateNode"
  show (ShareTeam   _      )      = "ShareTeam"
  show (AddContact  _      )      = "AddContact"
  show (SharePublic _      )      = "SharePublic"
  show (DoSearch    _      )      = "SearchQuery"
  show (UploadFile  _ _ _ _)      = "UploadFile"
  show (UploadArbitraryFile  _ _) = "UploadArbitraryFile"
  show  RefreshTree               = "RefreshTree"
  show  ClosePopover              = "ClosePopover"
  show  DownloadNode              = "Download"
  show (MoveNode  _ )             = "MoveNode"
  show (MergeNode _ )             = "MergeNode"
  show (LinkNode  _ )             = "LinkNode"
  show NoAction                   = "NoAction"

-----------------------------------------------------------------------
icon :: Action -> String
icon (AddNode    _ _)             = glyphiconNodeAction (Add [])
icon (DeleteNode _)               = glyphiconNodeAction Delete
icon (RenameNode _)               = glyphiconNodeAction Config
icon (UpdateNode _)               = glyphiconNodeAction Refresh
icon (ShareTeam   _)              = glyphiconNodeAction Share
icon (AddContact  _)              = glyphiconNodeAction Share
icon (SharePublic _ )             = glyphiconNodeAction (Publish { subTreeParams : SubTreeParams {showtypes:[], valitypes:[] }})
icon (DoSearch   _)               = glyphiconNodeAction SearchBox
icon (UploadFile _ _ _ _)         = glyphiconNodeAction Upload
icon (UploadArbitraryFile _ _ ) = glyphiconNodeAction Upload
icon  RefreshTree                 = glyphiconNodeAction Refresh
icon  ClosePopover                = glyphiconNodeAction CloseNodePopover
icon  DownloadNode                = glyphiconNodeAction Download
icon (MoveNode _ )                = glyphiconNodeAction (Move  { subTreeParams : SubTreeParams {showtypes:[], valitypes:[] }})
icon (MergeNode _ )               = glyphiconNodeAction (Merge { subTreeParams : SubTreeParams {showtypes:[], valitypes:[] }})
icon (LinkNode _  )               = glyphiconNodeAction (Link  { subTreeParams : SubTreeParams {showtypes:[], valitypes:[] }})

icon NoAction                     = "hand-o-right"

-- icon _             = "hand-o-right"

text :: Action -> String
text (AddNode     _ _    )      = "Add !"
text (DeleteNode _       )      = "Delete !"
text (RenameNode  _      )      = "Rename !"
text (UpdateNode  _      )      = "Update !"
text (ShareTeam   _      )      = "Share with team !"
text (AddContact  _      )      = "Add contact !"
text (SharePublic _      )      = "Publish !"
text (DoSearch    _      )      = "Launch search !"
text (UploadFile  _ _ _ _)      = "Upload File !"
text (UploadArbitraryFile  _ _) = "Upload arbitrary file !"
text  RefreshTree               = "Refresh Tree !"
text  ClosePopover              = "Close Popover !"
text DownloadNode               = "Download !"
text (MoveNode  _ )             = "Move !"
text (MergeNode _ )             = "Merge !"
text (LinkNode  _ )             = "Link !"
text NoAction                   = "No Action"
-----------------------------------------------------------------------
