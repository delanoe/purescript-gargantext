module Gargantext.Components.Forest.Tree.Node.Action.Types where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Gargantext.Components.Forest.Tree.Node.Action.Contact.Types (AddContactParams)
import Gargantext.Components.Forest.Tree.Node.Action.Update.Types (UpdateNodeParams)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileFormat, FileType, UploadFileBlob)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut)
import Gargantext.Components.ListSelection.Types (Selection)
import Gargantext.Types as GT

data Action = AddNode     String GT.NodeType
            | DeleteNode  GT.NodeType
            | RenameNode  String
            | UpdateNode  UpdateNodeParams
            | DoSearch    GT.AsyncTaskWithType
            | UploadFile  GT.NodeType FileType FileFormat (Maybe String) String Selection
            | UploadArbitraryFile FileFormat (Maybe String) UploadFileBlob Selection
            | UploadFrameCalc
            | DownloadNode
            | RefreshTree
            | CloseBox

            | ShareTeam   String
            | AddContact  AddContactParams
            | SharePublic {params :: Maybe SubTreeOut}
            | MoveNode    {params :: Maybe SubTreeOut}
            | MergeNode   {params :: Maybe SubTreeOut}
            | LinkNode    {nodeType :: Maybe GT.NodeType, params :: Maybe SubTreeOut}

            | DocumentsFromWriteNodes { id :: GT.ID }

            | NoAction

derive instance Generic Action _

instance Eq Action where
  eq (AddNode s1 nt1) (AddNode s2 nt2) = (eq s1 s2) && (eq nt1 nt2)
  eq (DeleteNode nt1) (DeleteNode nt2) = eq nt1 nt2
  eq (RenameNode s1) (RenameNode s2) = eq s1 s2
  eq (UpdateNode un1) (UpdateNode un2) = eq un1 un2
  eq (DoSearch at1) (DoSearch at2) = eq at1 at2
  eq (UploadFile nt1 ft1 ff1 s1 _ _) (UploadFile nt2 ft2 ff2 s2 _ _) =
    (eq nt1 nt2) && (eq ft1 ft2) && (eq ff1 ff2) && (eq s1 s2)
  eq (UploadArbitraryFile ff1 s1 _ _) (UploadArbitraryFile ff2 s2 _ _) = (eq ff1 ff2) && (eq s1 s2)
  eq UploadFrameCalc UploadFrameCalc = true
  eq DownloadNode DownloadNode = true
  eq RefreshTree RefreshTree = true
  eq CloseBox CloseBox = true
  eq (ShareTeam s1) (ShareTeam s2) = eq s1 s2
  eq (AddContact ac1) (AddContact ac2) = eq ac1 ac2
  eq (SharePublic p1) (SharePublic p2) = eq p1 p2
  eq (MoveNode p1) (MoveNode p2) = eq p1 p2
  eq (MergeNode p1) (MergeNode p2) = eq p1 p2
  eq (LinkNode l1) (LinkNode l2) = eq l1 l2
  eq (DocumentsFromWriteNodes { id: id1 }) (DocumentsFromWriteNodes { id: id2 }) = eq id1 id2
  eq NoAction NoAction = true
  eq _ _ = false

instance Show Action where
  show (AddNode     _ _    )         = "AddNode"
  show (DeleteNode  _      )         = "DeleteNode"
  show (RenameNode  _      )         = "RenameNode"
  show (UpdateNode  _      )         = "UpdateNode"
  show (ShareTeam   _      )         = "ShareTeam"
  show (AddContact  _      )         = "AddContact"
  show (SharePublic _      )         = "SharePublic"
  show (DoSearch    _      )         = "SearchQuery"
  show (UploadFile _ _ _ _ _ _)      = "UploadFile"
  show (UploadArbitraryFile _ _ _ _) = "UploadArbitraryFile"
  show UploadFrameCalc               = "UploadFrameCalc"
  show  RefreshTree                  = "RefreshTree"
  show  CloseBox                     = "CloseBox"
  show  DownloadNode                 = "Download"
  show (MoveNode  _ )                = "MoveNode"
  show (MergeNode _ )                = "MergeNode"
  show (LinkNode  _ )                = "LinkNode"
  show (DocumentsFromWriteNodes _ )  = "DocumentsFromWriteNodes"
  show NoAction                      = "NoAction"
