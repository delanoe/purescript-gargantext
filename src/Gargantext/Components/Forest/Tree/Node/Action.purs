module Gargantext.Components.Forest.Tree.Node.Action where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction(..), glyphiconNodeAction)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (ID)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut, SubTreeParams(..))
import Gargantext.Sessions (Session)
import Gargantext.Types as GT

type Props =
  ( dispatch :: Action -> Aff Unit
  , id       :: ID
  , nodeType :: GT.NodeType
  , session  :: Session
  )

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
icon (UploadFile _ _ _ _ _)       = glyphiconNodeAction Upload
icon (UploadArbitraryFile _ _ _ ) = glyphiconNodeAction Upload
icon UploadFrameCalc              = glyphiconNodeAction Upload
icon  RefreshTree                 = glyphiconNodeAction Refresh
icon  ClosePopover                = glyphiconNodeAction CloseNodePopover
icon  DownloadNode                = glyphiconNodeAction Download
icon (MoveNode _ )                = glyphiconNodeAction (Move  { subTreeParams : SubTreeParams {showtypes:[], valitypes:[] }})
icon (MergeNode _ )               = glyphiconNodeAction (Merge { subTreeParams : SubTreeParams {showtypes:[], valitypes:[] }})
icon (LinkNode _  )               = glyphiconNodeAction (Link  { subTreeParams : SubTreeParams {showtypes:[], valitypes:[] }})
icon (DocumentsFromWriteNodes _)  = glyphiconNodeAction (WriteNodesDocuments)

icon NoAction                     = "hand-o-right"

-- icon _             = "hand-o-right"

text :: Action -> String
text (AddNode     _ _    )        = "Add !"
text (DeleteNode _       )        = "Delete !"
text (RenameNode  _      )        = "Rename !"
text (UpdateNode  _      )        = "Update !"
text (ShareTeam   _      )        = "Share with team !"
text (AddContact  _      )        = "Add contact !"
text (SharePublic _      )        = "Publish !"
text (DoSearch    _      )        = "Launch search !"
text (UploadFile  _ _ _ _ _)      = "Upload File !"
text (UploadArbitraryFile _  _ _) = "Upload arbitrary file !"
text UploadFrameCalc              = "Upload frame calc"
text  RefreshTree                 = "Refresh Tree !"
text  ClosePopover                = "Close Popover !"
text DownloadNode                 = "Download !"
text (MoveNode  _ )               = "Move !"
text (MergeNode _ )               = "Merge !"
text (LinkNode  _ )               = "Link !"
text (DocumentsFromWriteNodes _ ) = "Documents from Write Nodes !"
text NoAction                     = "No Action"
-----------------------------------------------------------------------

