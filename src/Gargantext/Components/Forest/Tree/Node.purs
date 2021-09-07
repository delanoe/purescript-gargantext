module Gargantext.Components.Forest.Tree.Node where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload (DroppedFile(..), fileTypeView)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType(..), UploadFileBlob(..))
import Gargantext.Components.Forest.Tree.Node.Box (nodePopupView)
import Gargantext.Components.Forest.Tree.Node.Settings (SettingsBox(..), settingsBox)
import Gargantext.Components.Forest.Tree.Node.Tools (nodeLink)
import Gargantext.Components.Forest.Tree.Node.Tools.ProgressBar (asyncProgressBar, BarType(..))
import Gargantext.Components.Forest.Tree.Node.Tools.Sync (nodeActionsGraph, nodeActionsNodeList)
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.Lang (Lang(EN))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (ID, Name, reverseHanded)
import Gargantext.Types as GT
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Gargantext.Version as GV
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node"

-- Main Node
type NodeMainSpanProps =
  ( boxes         :: Boxes
  , dispatch      :: Action -> Aff Unit
  , folderOpen    :: T.Box Boolean
  , frontends     :: Frontends
  , id            :: ID
  , isLeaf        :: IsLeaf
  , name          :: Name
  , nodeType      :: GT.NodeType
  , reload        :: T2.ReloadS
  , root          :: ID
  , session       :: Session
  , setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit))
  )

type IsLeaf = Boolean

nodeSpan :: R2.Component NodeMainSpanProps
nodeSpan = R.createElement nodeSpanCpt
nodeSpanCpt :: R.Component NodeMainSpanProps
nodeSpanCpt = here.component "nodeSpan" cpt
  where
    cpt props@{ boxes: { handed } } children = do
      handed' <- T.useLive T.unequal handed
      let className = case handed' of
            GT.LeftHanded  -> "lefthanded"
            GT.RightHanded -> "righthanded"

      pure $ H.div { className } ([ nodeMainSpan props [] ] <> children)

nodeMainSpan :: R2.Component NodeMainSpanProps
nodeMainSpan = R.createElement nodeMainSpanCpt
nodeMainSpanCpt :: R.Component NodeMainSpanProps
nodeMainSpanCpt = here.component "nodeMainSpan" cpt
  where
    cpt props@{ boxes: boxes@{ errors
                             , handed
                             , reloadMainPage
                             , reloadRoot
                             , route
                             , tasks }
              , dispatch
              , folderOpen
              , frontends
              , id
              , isLeaf
              , nodeType
              , reload
              , session
              , setPopoverRef
              } _ = do
      handed' <- T.useLive T.unequal handed
      route' <- T.useLive T.unequal route
      -- only 1 popup at a time is allowed to be opened
      droppedFile   <- T.useBox (Nothing :: Maybe DroppedFile)
      droppedFile'  <- T.useLive T.unequal droppedFile
      isDragOver    <- T.useBox false
      isDragOver'   <- T.useLive T.unequal isDragOver
      popoverRef    <- R.useRef null

      currentTasks <- GAT.focus id tasks
      currentTasks' <- T.useLive T.unequal currentTasks

      R.useEffect' $ do
        R.setRef setPopoverRef $ Just $ Popover.setOpen popoverRef
      let isSelected = Just route' == Routes.nodeTypeAppRoute nodeType (sessionId session) id

      -- tasks' <- T.read tasks

      pure $ H.span (dropProps droppedFile droppedFile' isDragOver isDragOver')
        $ reverseHanded handed'
        [ folderIcon  { folderOpen, nodeType } []
        , chevronIcon { folderOpen, handed, isLeaf, nodeType } []
        , nodeLink { boxes
                   , folderOpen
                   , frontends
                   , id
                   , isSelected
                   , name: name' props
                   , nodeType
                   , session } []

                , fileTypeView { dispatch, droppedFile, id, isDragOver, nodeType }
                , H.div {} (map (\t -> asyncProgressBar { asyncTask: t
                                                        , barType: Pie
                                                        , errors
                                                        , nodeId: id
                                                        , onFinish: onTaskFinish id t
                                                        , session } []
                                ) currentTasks'
                           )
                , if nodeType == GT.NodeUser
                        then GV.versionView { session } []
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
                              , refresh: const $ dispatch RefreshTree
                              , session
                              } []
                ]
        where
          onTaskFinish id' t _ = do
            GAT.finish id' t tasks
            if GAT.asyncTaskTTriggersAppReload t then do
              here.log2 "reloading root for task" t
              T2.reload reloadRoot
            else do
              if GAT.asyncTaskTTriggersTreeReload t then do
                here.log2 "reloading tree for task" t
                T2.reload reload
              else do
                here.log2 "task doesn't trigger a tree reload" t
                pure unit
              if GAT.asyncTaskTTriggersMainPageReload t then do
                here.log2 "reloading main page for task" t
                T2.reload reloadMainPage
              else do
                here.log2 "task doesn't trigger a main page reload" t
                pure unit
            -- snd tasks $ GAT.Finish id' t
            -- mT <- T.read tasks
            -- case mT of
            --   Just t' -> snd t' $ GAT.Finish id' t
            --   Nothing -> pure unit
            -- T2.reload reloadRoot

          SettingsBox {show: showBox} = settingsBox nodeType
          onPopoverClose popoverRef _ = Popover.setOpen popoverRef false

          name' {name: n, nodeType: nt} = if nt == GT.NodeUser then show session else n

          mNodePopupView props'@{ boxes: b, id: i, nodeType: nt } opc =
            nodePopupView { boxes: b
                          , dispatch
                          , id: i
                          , name: name' props'
                          , nodeType: nt
                          , onPopoverClose: opc
                          , session }

    popOverIcon =
      H.a { className: "settings fa fa-cog" 
          , title : "Each node of the Tree can perform some actions.\n"
            <> "Click here to execute one of them." } []
    dropProps droppedFile droppedFile' isDragOver isDragOver' =
      { className: "leaf " <> (dropClass droppedFile' isDragOver')
      , on: { dragLeave: onDragLeave isDragOver
            , dragOver: onDragOverHandler isDragOver
            , drop: dropHandler droppedFile }
      }
      where
        dropClass (Just _) _    = "file-dropped"
        dropClass _        true = "file-dropped"
        dropClass Nothing  _    = ""

        dropHandler droppedFile e          = do
          -- prevent redirection when file is dropped
          E.preventDefault e
          E.stopPropagation e
          blob <- R2.dataTransferFileBlob e
          void $ launchAff do
            --contents <- readAsText blob
            liftEffect $ do
              T.write_ (Just
                       $ DroppedFile { blob: (UploadFileBlob blob)
                                     , fileType: Just CSV
                                     , lang    : EN
                                     }) droppedFile
    onDragOverHandler isDragOver e = do
      -- prevent redirection when file is dropped
      -- https://stackoverflow.com/a/6756680/941471
      E.preventDefault e
      E.stopPropagation e
      T.write_ true isDragOver
    onDragLeave isDragOver _ = T.write_ false isDragOver

type FolderIconProps = (
    folderOpen :: T.Box Boolean
  , nodeType   ::  GT.NodeType
  )

folderIcon :: R2.Component FolderIconProps
folderIcon = R.createElement folderIconCpt
folderIconCpt :: R.Component FolderIconProps
folderIconCpt = here.component "folderIcon" cpt
  where
    cpt { folderOpen, nodeType } _ = do
      open <- T.useLive T.unequal folderOpen
      pure $ H.a { className: "folder-icon", on: { click: \_ -> T.modify_ not folderOpen } }
        [ H.i { className: GT.fldr nodeType open } [] ]

type ChevronIconProps = (
    folderOpen :: T.Box Boolean
  , handed     :: T.Box GT.Handed
  , isLeaf     :: Boolean
  , nodeType   :: GT.NodeType
  )

chevronIcon :: R2.Component ChevronIconProps
chevronIcon = R.createElement chevronIconCpt
chevronIconCpt :: R.Component ChevronIconProps
chevronIconCpt = here.component "chevronIcon" cpt
  where
    cpt { folderOpen, handed, isLeaf: true, nodeType } _ = do
      pure $ H.div {} []
    cpt { folderOpen, handed, isLeaf: false, nodeType } _ = do
      handed' <- T.useLive T.unequal handed
      open <- T.useLive T.unequal folderOpen
      pure $ H.a { className: "chevron-icon"
          , on: { click: \_ -> T.modify_ not folderOpen }
          }
        [ H.i { className: if open
                            then "fa fa-chevron-down"
                            else if handed' == GT.RightHanded
                                    then "fa fa-chevron-right"
                                    else "fa fa-chevron-left"
                } [] ]

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

type NodeActionsCommon =
  ( id       :: ID
  , refresh  :: Unit -> Aff Unit
  , session  :: Session
  )

type NodeActionsProps = ( nodeType :: GT.NodeType | NodeActionsCommon )

nodeActions :: R2.Component NodeActionsProps
nodeActions = R.createElement nodeActionsCpt

nodeActionsCpt :: R.Component NodeActionsProps
nodeActionsCpt = here.component "nodeActions" cpt where
  cpt props _ = pure (child props.nodeType) where
    nodeActionsP = SProxy :: SProxy "nodeType"
    childProps = Record.delete nodeActionsP props
    child GT.NodeList = listNodeActions childProps
    child GT.Graph = graphNodeActions childProps
    child _ = H.div {} []

graphNodeActions :: R2.Leaf NodeActionsCommon
graphNodeActions props = R.createElement graphNodeActionsCpt props []
graphNodeActionsCpt :: R.Component NodeActionsCommon
graphNodeActionsCpt = here.component "graphNodeActions" cpt where
  cpt { id, session, refresh } _ =
    useLoader { errorHandler
              , loader: graphVersions session
              , path: id
              , render: \gv -> nodeActionsGraph { graphVersions: gv, session, id, refresh } [] }
  graphVersions session graphId = GraphAPI.graphVersions { graphId, session }
  errorHandler err = here.log2 "[graphNodeActions] RESTError" err

listNodeActions :: R2.Leaf NodeActionsCommon
listNodeActions props = R.createElement listNodeActionsCpt props []
listNodeActionsCpt :: R.Component NodeActionsCommon
listNodeActionsCpt = here.component "listNodeActions" cpt where
  cpt { id, session, refresh } _ =
    useLoader { errorHandler
              , path: { nodeId: id, session }
              , loader: loadCorpusWithChild
              , render: \{ corpusId } -> nodeActionsNodeList
                 { listId: id, nodeId: corpusId, session, refresh: refresh
                 , nodeType: GT.TabNgramType GT.CTabTerms } }
    where
      errorHandler err = here.log2 "[listNodeActions] RESTError" err

