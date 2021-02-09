module Gargantext.Components.Forest.Tree.Node where

import Data.Array (reverse)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree.Node.Settings (SettingsBox(..), settingsBox)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType(..), UploadFileBlob(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload (DroppedFile(..), fileTypeView)
import Gargantext.Components.Forest.Tree.Node.Box (nodePopupView)
import Gargantext.Components.Forest.Tree.Node.Box.Types (CommonProps)
import Gargantext.Components.Forest.Tree.Node.Tools.ProgressBar (asyncProgressBar, BarType(..))
import Gargantext.Components.Forest.Tree.Node.Tools.Sync (nodeActionsGraph, nodeActionsNodeList)
import Gargantext.Components.Forest.Tree.Node.Tools (nodeLink)
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.Lang (Lang(EN))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Version as GV
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (Name, ID)
import Gargantext.Types as GT
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR

thisModule :: String
thisModule = "Gargantext.Components.Forest.Tree.Node"

-- Main Node
type NodeMainSpanProps = (
    appReload     :: GUR.ReloadS
  , asyncTasks    :: GAT.Reductor
  , currentRoute  :: Routes.AppRoute
  , folderOpen    :: R.State Boolean
  , frontends     :: Frontends
  , id            :: ID
  , isLeaf        :: IsLeaf
  , name          :: Name
  , nodeType      :: GT.NodeType
  , setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit))
  | CommonProps
  )

type IsLeaf = Boolean

nodeSpan :: R2.Component NodeMainSpanProps
nodeSpan = R.createElement nodeSpanCpt

nodeSpanCpt :: R.Component NodeMainSpanProps
nodeSpanCpt = R.hooksComponentWithModule thisModule "nodeSpan" cpt
  where
    cpt props children = do
      pure $ H.div {} ([ nodeMainSpan props [] ] <> children)

nodeMainSpan :: R2.Component NodeMainSpanProps
nodeMainSpan = R.createElement nodeMainSpanCpt

nodeMainSpanCpt :: R.Component NodeMainSpanProps
nodeMainSpanCpt = R.hooksComponentWithModule thisModule "nodeMainSpan" cpt
  where
    cpt props@{ appReload
              , asyncTasks: (asyncTasks /\ dispatchAsyncTasks)
              , currentRoute
              , dispatch
              , folderOpen
              , frontends
              , handed
              , id
              , isLeaf
              , name
              , nodeType
              , session
              , setPopoverRef
              } _ = do
      -- only 1 popup at a time is allowed to be opened
      droppedFile   <- R.useState' (Nothing :: Maybe DroppedFile)
      isDragOver    <- R.useState' false

      popoverRef    <- R.useRef null

      R.useEffect' $ do
        R.setRef setPopoverRef $ Just $ Popover.setOpen popoverRef

      let ordering =
            case handed of
              GT.LeftHanded  -> reverse
              GT.RightHanded -> identity

      let isSelected = Just currentRoute == Routes.nodeTypeAppRoute nodeType (sessionId session) id

      pure $ H.span (dropProps droppedFile isDragOver)
                $ ordering
                [ folderIcon  nodeType folderOpen
                , chevronIcon isLeaf handed nodeType folderOpen
                , nodeLink { frontends
                           , handed
                           , folderOpen
                           , id
                           , isSelected
                           , name: name' props
                           , nodeType
                           , session
                           } []

                , fileTypeView { dispatch, droppedFile, id, isDragOver, nodeType }
                , H.div {} (map (\t -> asyncProgressBar { asyncTask: t
                                                       , barType: Pie
                                                       , nodeId: id
                                                       , onFinish: onTaskFinish id t
                                                       , session
                                                       }
                                ) $ GAT.getTasks asyncTasks id
                           )
                , if nodeType == GT.NodeUser
                        then GV.versionView {session}
                        else H.div {} []

                , if showBox then
                        Popover.popover { arrow: false
                                        , open: false
                                        , onClose: \_ -> pure unit
                                        , onOpen:  \_ -> pure unit
                                        , ref: popoverRef } [
                        popOverIcon
                        , mNodePopupView props (onPopoverClose popoverRef)
                        ]
                else H.div {} []

                , nodeActions { id
                              , nodeType
                              , session
                              , triggerRefresh: const $ dispatch RefreshTree
                              }


                ]
        where
          onTaskFinish id t _ = do
            dispatchAsyncTasks $ GAT.Finish id t
            GUR.bump appReload

          SettingsBox {show: showBox} = settingsBox nodeType
          onPopoverClose popoverRef _ = Popover.setOpen popoverRef false

          name' {name, nodeType} = if nodeType == GT.NodeUser then show session else name

          mNodePopupView props@{id, nodeType} onPopoverClose =
                                nodePopupView { dispatch
                                              , handed : props.handed
                                              , id
                                              , name: name' props
                                              , nodeType
                                              , onPopoverClose
                                              , session
                                              }

    chevronIcon true handed' nodeType (open /\ setOpen) = H.div {} []
    chevronIcon false handed' nodeType (open /\ setOpen) =
      H.a { className: "chevron-icon"
          , on: { click: \_ -> setOpen $ not }
          }
        [ H.i { className: if open
                           then "fa fa-chevron-down"
                           else if handed' == GT.RightHanded
                                   then "fa fa-chevron-right"
                                   else "fa fa-chevron-left"
                } [] ]

    folderIcon nodeType (open /\ setOpen) =
      H.a { className: "folder-icon"
          , on: { click: \_ -> setOpen $ not }
          } [
              H.i {className: GT.fldr nodeType open} []
            ]

    popOverIcon = H.a { className: "settings fa fa-cog" 
                      , title : "Each node of the Tree can perform some actions.\n"
                             <> "Click here to execute one of them."
                      } []

    dropProps droppedFile isDragOver =
      { className: "leaf " <> (dropClass droppedFile isDragOver)
      , on: { drop: dropHandler droppedFile
            , dragOver: onDragOverHandler isDragOver
            , dragLeave: onDragLeave isDragOver }
      }
      where
        dropClass   (Just _  /\ _)        _          = "file-dropped"
        dropClass    _                   (true /\ _) = "file-dropped"
        dropClass   (Nothing /\ _)        _          = ""
        dropHandler (_ /\ setDroppedFile) e          = do
          -- prevent redirection when file is dropped
          E.preventDefault e
          E.stopPropagation e
          blob <- R2.dataTransferFileBlob e
          void $ launchAff do
            --contents <- readAsText blob
            liftEffect $ setDroppedFile
                       $ const
                       $ Just
                       $ DroppedFile { blob: (UploadFileBlob blob)
                                     , fileType: Just CSV
                                     , lang    : EN
                                     }
    onDragOverHandler (_ /\ setIsDragOver) e = do
      -- prevent redirection when file is dropped
      -- https://stackoverflow.com/a/6756680/941471
      E.preventDefault e
      E.stopPropagation e
      setIsDragOver $ const true
    onDragLeave (_ /\ setIsDragOver) _ = setIsDragOver $ const false

{-
fldr nt open = if open
               then "fa fa-globe" -- <> color nt
               else "fa fa-folder-globe" -- <> color nt
               --else "fa fa-folder-close" <> color nt
                 where
                   color GT.NodeUser     = ""
                   color FolderPublic = ""
                   color FolderShared = " text-warning"
                   color _            = " text-danger"
-}


-- START nodeActions

type NodeActionsProps =
  ( id          :: ID
  , nodeType    :: GT.NodeType
  , session     :: Session
  , triggerRefresh :: Unit -> Aff Unit
  )

nodeActions :: Record NodeActionsProps -> R.Element
nodeActions p = R.createElement nodeActionsCpt p []

nodeActionsCpt :: R.Component NodeActionsProps
nodeActionsCpt = R.hooksComponentWithModule thisModule "nodeActions" cpt
  where
    cpt { id
        , nodeType: GT.Graph
        , session
        , triggerRefresh
        } _ = do

      useLoader id (graphVersions session) $ \gv ->
        nodeActionsGraph { id
                         , graphVersions: gv
                         , session
                         , triggerRefresh
                         }

    cpt { id
        , nodeType: GT.NodeList
        , session
        , triggerRefresh
        } _ = do
      useLoader { nodeId: id, session } loadCorpusWithChild $
        \{ corpusId } ->
          nodeActionsNodeList { listId: id
                              , nodeId: corpusId
                              , nodeType: GT.TabNgramType GT.CTabTerms
                              , session
                              , triggerRefresh
                              }
    cpt _ _ = do
      pure $ H.div {} []

    graphVersions session graphId = GraphAPI.graphVersions { graphId, session }


-- END nodeActions
