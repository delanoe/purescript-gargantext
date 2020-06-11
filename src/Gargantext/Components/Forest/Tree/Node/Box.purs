module Gargantext.Components.Forest.Tree.Node.Box where

import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.String as S
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree.Node (NodeAction(..), SettingsBox(..), glyphiconNodeAction, settingsBox)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..), FileType(..), UploadFileContents(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (NodePopup(..), addNodeView)
import Gargantext.Components.Forest.Tree.Node.Action.CopyFrom (copyFromCorpusView)
import Gargantext.Components.Forest.Tree.Node.Action.Documentation (actionDoc)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (renameAction)
import Gargantext.Components.Forest.Tree.Node.Action.Delete (actionDelete)
import Gargantext.Components.Forest.Tree.Node.Action.Search.Frame (searchIframes)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchBar (searchBar)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField (Search, defaultSearch)
import Gargantext.Components.Forest.Tree.Node.Action.Share as Share
import Gargantext.Components.Forest.Tree.Node.Action.Upload (actionUpload, DroppedFile(..), fileTypeView)
import Gargantext.Components.Forest.Tree.Node.Action.Download (actionDownload)
import Gargantext.Components.Forest.Tree.Node.Box.Types
import Gargantext.Components.Forest.Tree.Node.ProgressBar (asyncProgressBar, BarType(..))
import Gargantext.Components.Forest.Tree.Node.Tools (textInputBox, fragmentPT)
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.Lang (allLangs, Lang(EN))
import Gargantext.Components.NgramsTable.API as NTAPI
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Unit, bind, const, discard, identity, map, pure, show, unit, void, ($), (+), (<>), (==))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (ID, Name, Reload)
import Gargantext.Types as GT
import Gargantext.Utils (glyphicon, glyphiconActive)
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.Reactix as R2
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import Web.File.FileReader.Aff (readAsText)


type Tasks =
  ( onTaskAdd    :: GT.AsyncTaskWithType -> Effect Unit
  , onTaskFinish :: GT.AsyncTaskWithType -> Effect Unit
  , tasks        :: Array GT.AsyncTaskWithType
  )

tasksStruct :: Int -> R.State GAT.Storage -> R.State Reload -> Record Tasks
tasksStruct id (asyncTasks /\ setAsyncTasks) (_ /\ setReload) = { onTaskAdd, onTaskFinish, tasks }
  where
    tasks = maybe [] identity $ Map.lookup id asyncTasks
    onTaskAdd t = do
      setReload (_ + 1)
      setAsyncTasks $ Map.alter (maybe (Just [t]) $ (\ts -> Just $ A.cons t ts)) id
    onTaskFinish t = do
      setReload (_ + 1)
      setAsyncTasks $ Map.alter (maybe Nothing $ (\ts -> Just $ GAT.removeTaskFromList ts t)) id

-- Main Node
type NodeMainSpanProps =
  ( id                :: ID
  , folderOpen        :: R.State Boolean
  , frontends         :: Frontends
  , mCurrentRoute     :: Maybe Routes.AppRoute
  , name              :: Name
  , nodeType          :: GT.NodeType
  , tasks             :: Record Tasks
  | CommonProps
  )

nodeMainSpan :: Record NodeMainSpanProps
             -> R.Element
nodeMainSpan p@{ dispatch, folderOpen, frontends, session } = R.createElement el p []
  where
    el = R.hooksComponent "G.C.F.T.N.B.NodeMainSpan" cpt
    cpt props@{id, mCurrentRoute, name, nodeType, tasks: { onTaskFinish, tasks }} _ = do
      -- only 1 popup at a time is allowed to be opened
      droppedFile   <- R.useState' (Nothing :: Maybe DroppedFile)
      isDragOver    <- R.useState' false

      popoverRef    <- R.useRef null

      pure $ H.span (dropProps droppedFile isDragOver) $
        [ folderIcon nodeType folderOpen
        , if showBox then
            Popover.popover { arrow: false
                            , open: false
                            , onClose: \_ -> pure unit
                            , onOpen: \_ -> pure unit
                            , ref: popoverRef } [
                popOverIcon
              , mNodePopupView props (onPopoverClose popoverRef)
            ]
          else H.div {} []
        , H.a { href: (url frontends (GT.NodePath (sessionId session) nodeType (Just id)))
              }
              [ nodeText { isSelected: mAppRouteId mCurrentRoute == Just id
                         , name: name' props
                         } ]
        , nodeActions { id
                      , nodeType
                      , refreshTree: const $ dispatch RefreshTree
                      , session }
        , fileTypeView {dispatch, droppedFile, id, isDragOver, nodeType}
        , H.div {} (map (\t -> asyncProgressBar { asyncTask: t
                                                , barType: Pie
                                                , corpusId: id
                                                , onFinish: const $ onTaskFinish t
                                                , session }) tasks)
        ]
          where
            SettingsBox {show: showBox} = settingsBox nodeType
            onPopoverClose popoverRef _ = Popover.setOpen popoverRef false

    name' {name, nodeType} = if nodeType == GT.NodeUser
                                then show session
                                else name

    folderIcon nodeType folderOpen'@(open /\ _) =
      H.a { className: "folder-icon"
          , onClick: R2.effToggler folderOpen'
          }
          [ H.i {className: GT.fldr nodeType open} [] ]

    popOverIcon =
      H.a { className: "settings fa fa-cog" } []

    mNodePopupView props@{id, nodeType} onPopoverClose =
      nodePopupView { id
                    , dispatch
                    , name: name' props
                    , nodeType
                    , onPopoverClose
                    , session
                    }

    dropProps droppedFile isDragOver =
      { className: "leaf " <> (dropClass droppedFile isDragOver)
      , on: { drop: dropHandler droppedFile
            , dragOver: onDragOverHandler isDragOver
            , dragLeave: onDragLeave isDragOver } }
      where
        dropClass (Just _ /\ _)  _           = "file-dropped"
        dropClass _              (true /\ _) = "file-dropped"
        dropClass (Nothing /\ _) _           = ""
        dropHandler (_ /\ setDroppedFile) e = do
          -- prevent redirection when file is dropped
          E.preventDefault e
          E.stopPropagation e
          blob <- R2.dataTransferFileBlob e
          void $ launchAff do
            contents <- readAsText blob
            liftEffect $ setDroppedFile
                       $ const
                       $ Just
                       $ DroppedFile { contents: (UploadFileContents contents)
                                     , fileType: Just CSV
                                     , lang: Just EN
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
               --else "glyphicon glyphicon-folder-close" <> color nt
                 where
                   color GT.NodeUser     = ""
                   color FolderPublic = ""
                   color FolderShared = " text-warning"
                   color _            = " text-danger"
-}


-- START node text
type NodeTextProps =
  ( isSelected :: Boolean
  , name       :: Name
  )

nodeText :: Record NodeTextProps -> R.Element
nodeText p = R.createElement nodeTextCpt p []

nodeTextCpt :: R.Component NodeTextProps
nodeTextCpt = R.hooksComponent "G.C.F.T.N.B.nodeText" cpt
  where
    cpt { isSelected: true, name } _ = do
      pure $ H.u {} [
        H.b {} [
           H.text ("| " <> name <> " |    ")
         ]
        ]
    cpt {isSelected: false, name} _ = do
      pure $ H.text (name <> "    ")
-- END node text

-- START nodeActions

type NodeActionsProps =
  ( id          :: ID
  , nodeType    :: GT.NodeType
  , refreshTree :: Unit -> Aff Unit
  , session     :: Session
  )

nodeActions :: Record NodeActionsProps -> R.Element
nodeActions p = R.createElement nodeActionsCpt p []

nodeActionsCpt :: R.Component NodeActionsProps
nodeActionsCpt = R.hooksComponent "G.C.F.T.N.B.nodeActions" cpt
  where
    cpt { id, nodeType: GT.Graph, refreshTree, session } _ = do
      useLoader id (graphVersions session) $ \gv ->
        nodeActionsGraph { id, graphVersions: gv, session, triggerRefresh: triggerRefresh refreshTree }
    cpt { id, nodeType: GT.NodeList, refreshTree, session } _ = do
      useLoader { nodeId: id, session } loadCorpusWithChild $
        \{ corpusId } ->
          nodeActionsNodeList { listId: id
                              , nodeId: corpusId
                              , nodeType: GT.TabNgramType GT.CTabTerms
                              , session
                              , triggerRefresh: triggerRefresh refreshTree }
    cpt _ _ = do
      pure $ H.div {} []

    graphVersions session graphId = GraphAPI.graphVersions { graphId, session }
    triggerRefresh refreshTree = refreshTree




-- | Sync Node (Graph)
type NodeActionsGraphProps =
  ( id             :: ID
  , graphVersions  :: Record GraphAPI.GraphVersions
  , session        :: Session
  , triggerRefresh :: Unit -> Aff Unit
  )

nodeActionsGraph :: Record NodeActionsGraphProps -> R.Element
nodeActionsGraph p = R.createElement nodeActionsGraphCpt p []

nodeActionsGraphCpt :: R.Component NodeActionsGraphProps
nodeActionsGraphCpt = R.hooksComponent "G.C.F.T.N.B.nodeActionsGraph" cpt
  where
    cpt { id, graphVersions, session, triggerRefresh } _ = do
      pure $ H.div { className: "node-actions" } [
        if graphVersions.gv_graph == Just graphVersions.gv_repo then
          H.div {} []
        else
          graphUpdateButton { id, session, triggerRefresh }
      ]

type GraphUpdateButtonProps =
  ( id :: ID
  , session :: Session
  , triggerRefresh :: Unit -> Aff Unit
  )

graphUpdateButton :: Record GraphUpdateButtonProps -> R.Element
graphUpdateButton p = R.createElement graphUpdateButtonCpt p []

graphUpdateButtonCpt :: R.Component GraphUpdateButtonProps
graphUpdateButtonCpt = R.hooksComponent "G.C.F.T.N.B.graphUpdateButton" cpt
  where
    cpt { id, session, triggerRefresh } _ = do
      enabled <- R.useState' true

      pure $ H.div { className: "update-button "
                   <> if (fst enabled)
                         then "enabled"
                         else "disabled text-muted"
                   } [ H.span { className: "fa fa-refresh"
                     , on: { click: onClick enabled } } []
                     ]
      where
        onClick (false /\ _) _ = pure unit
        onClick (true /\ setEnabled) _ = do
          launchAff_ $ do
            liftEffect $ setEnabled $ const false
            g <- GraphAPI.updateGraphVersions { graphId: id, session }
            liftEffect $ setEnabled $ const true
            triggerRefresh unit
          pure unit

-- | Sync Node (List)
type NodeActionsNodeListProps =
  (
    listId :: GT.ListId
  , nodeId :: ID
  , nodeType :: GT.TabSubType GT.CTabNgramType
  , session :: Session
  , triggerRefresh :: Unit -> Aff Unit
  )

nodeActionsNodeList :: Record NodeActionsNodeListProps -> R.Element
nodeActionsNodeList p = R.createElement nodeActionsNodeListCpt p []

nodeActionsNodeListCpt :: R.Component NodeActionsNodeListProps
nodeActionsNodeListCpt = R.hooksComponent "G.C.F.T.N.B.nodeActionsNodeList" cpt
  where
    cpt props _ = do
      pure $ H.div { className: "node-actions" } [
        nodeListUpdateButton props
      ]

type NodeListUpdateButtonProps =
  ( listId :: GT.ListId
  , nodeId :: ID
  , nodeType :: GT.TabSubType GT.CTabNgramType
  , session :: Session
  , triggerRefresh :: Unit -> Aff Unit
  )

nodeListUpdateButton :: Record NodeListUpdateButtonProps -> R.Element
nodeListUpdateButton p = R.createElement nodeListUpdateButtonCpt p []

nodeListUpdateButtonCpt :: R.Component NodeListUpdateButtonProps
nodeListUpdateButtonCpt = R.hooksComponent "G.C.F.T.N.B.nodeListUpdateButton" cpt
  where
    cpt { listId, nodeId, nodeType, session, triggerRefresh } _ = do
      enabled <- R.useState' true

      pure $ H.div { className: "update-button " 
                     <> if (fst enabled) then "enabled" else "disabled text-muted"
                   } [ H.span { className: "fa fa-refresh"
                     , on: { click: onClick enabled } } []
                     ]
      where
        onClick (false /\ _) _ = pure unit
        onClick (true /\ setEnabled) _ = do
          launchAff_ $ do
            liftEffect $ setEnabled $ const false
            _ <- NTAPI.updateNodeList { listId, nodeId, nodeType, session }
            liftEffect $ setEnabled $ const true
            triggerRefresh unit
          pure unit

-- END nodeActions

mAppRouteId :: Maybe Routes.AppRoute -> Maybe Int
mAppRouteId (Just (Routes.Folder         _ id)) = Just id
mAppRouteId (Just (Routes.FolderPrivate  _ id)) = Just id
mAppRouteId (Just (Routes.FolderPublic   _ id)) = Just id
mAppRouteId (Just (Routes.FolderShared   _ id)) = Just id
mAppRouteId (Just (Routes.Team           _ id)) = Just id
mAppRouteId (Just (Routes.Corpus         _ id)) = Just id
mAppRouteId (Just (Routes.PGraphExplorer _ id)) = Just id
mAppRouteId (Just (Routes.Dashboard      _ id)) = Just id
mAppRouteId (Just (Routes.Texts          _ id)) = Just id
mAppRouteId (Just (Routes.Lists          _ id)) = Just id
mAppRouteId (Just (Routes.Annuaire       _ id)) = Just id
mAppRouteId (Just (Routes.UserPage       _ id)) = Just id
mAppRouteId (Just (Routes.Document       _ id _  )) = Just id
mAppRouteId (Just (Routes.ContactPage    _ id _  )) = Just id
mAppRouteId (Just (Routes.CorpusDocument _ id _ _)) = Just id
mAppRouteId _ = Nothing


-- | START Popup View

iconAStyle :: { color      :: String
              , paddingTop :: String
              , paddingBottom :: String
              }
iconAStyle = { color         : "black"
             , paddingTop    : "6px"
             , paddingBottom : "6px"
             }

nodePopupView :: Record NodePopupProps -> R.Element
nodePopupView p = R.createElement nodePopupCpt p []

nodePopupCpt :: R.Component NodePopupProps
nodePopupCpt = R.hooksComponent "G.C.F.T.N.B.nodePopupView" cpt
  where
    cpt p _ = do
      isOpen    <- R.useState' false
      iframeRef <- R.useRef    null
      nodePopupState@(nodePopup /\ setNodePopup) <- R.useState' { action: Nothing
                                                                , id: p.id
                                                                , name: p.name
                                                                , nodeType: p.nodeType
                                                                }
      search        <- R.useState' $ defaultSearch { node_id = Just p.id }
      pure $ H.div tooltipProps $
        [ H.div { className: "popup-container" }
          [ H.div { className: "panel panel-default" }
            [ H.div {className: ""}
            [ H.div { className : "col-md-10 flex-between"}
                [ H.h3 { className: GT.fldr p.nodeType true} []
                -- TODO fix names
                , H.text $ S.replace (S.Pattern "Node")   (S.Replacement " ") 
                         $ S.replace (S.Pattern "Folder") (S.Replacement " ")
                         $ show p.nodeType
                , H.p {className: "text-primary center"} [H.text p.name]
                ]
              ]
            , panelHeading isOpen         p
            , panelBody    nodePopupState p
            , mPanelAction nodePopupState p search
            ]
          , if nodePopup.action == Just SearchBox then
              H.div {} [ searchIframes p search iframeRef ]
            else
              H.div {} []
          ]
        ]
      where
        tooltipProps = { className : ""
                       , id        : "node-popup-tooltip"
                       , title     : "Node settings"
                       , data: { toggle: "tooltip"
                               , placement: "right"}
                         --, style: { top: y - 65.0, left: x + 10.0 }
                       }

        panelHeading isOpen@(open /\ _) {dispatch, id, name, nodeType} =
          H.div {className: "panel-heading"}
                [ R2.row
                        [ H.div {className: "col-md-8 flex-end"}
                                [ textInputBox { boxAction: renameAction
                                               , boxName: "Rename", dispatch, id, text:name, isOpen } ]

                        , H.div {className: "flex-end"}
                                [ if edit then editIcon isOpen else H.div {} []
                                , H.div {className: "col-md-1"}
                                        [ H.a { "type"   : "button"
                                              , className: glyphicon "window-close"
                                              , on: { click: \e -> p.onPopoverClose $ R2.unsafeEventTarget e }
                                              , title    : "Close"
                                              } []
                                        ]
                                 ]
                        ]
                ]
          where
            SettingsBox {edit, doc, buttons} = settingsBox nodeType

            editIcon :: R.State Boolean -> R.Element
            editIcon (false /\ setIsOpen) =
              H.div {className : "col-md-1"}
              [ H.a { className: glyphicon "pencil"
                    , id       : "rename1"
                    , title    : "Rename"
                    , on: { click: \_ -> setIsOpen $ const true }
                    }
                []
              ]
            editIcon (true /\ _) = H.div {} []

        panelBody :: R.State (Record ActionState)
                  -> Record NodePopupProps
                  -> R.Element
        panelBody nodePopupState {dispatch: d, nodeType} =
          H.div {className: "panel-body flex-space-between"}
                [ H.p { "style": {"margin":"10px"}} []
                , H.div { className: "flex-center"} 
                        [buttonClick {action: doc, state: nodePopupState}]
                , H.div {className: "flex-center"}
                        $ map (\t -> buttonClick {action: t, state: nodePopupState}) buttons
                ]
          where
            SettingsBox {edit, doc, buttons} = settingsBox nodeType

        mPanelAction :: R.State (Record NodePopupS)
                     -> Record NodePopupProps
                     -> R.State Search
                     -> R.Element
        mPanelAction ({action: Nothing} /\ _) _ _ = H.div {} []
        mPanelAction ({action: Just action} /\ _) props search =
            panelAction { action
                        , dispatch : props.dispatch
                        , id       : props.id
                        , name     : props.name
                        , nodePopup: Just NodePopup
                        , nodeType : props.nodeType
                        , search
                        , session  : props.session
                        }

type ActionState =
  ( action   :: Maybe NodeAction
  , id       :: ID
  , name     :: Name
  , nodeType :: GT.NodeType
  )

type ButtonClickProps =
  ( action :: NodeAction
  , state  :: R.State (Record ActionState)
  )

buttonClick :: Record ButtonClickProps -> R.Element
buttonClick p = R.createElement buttonClickCpt p []

buttonClickCpt :: R.Component ButtonClickProps
buttonClickCpt = R.hooksComponent "G.C.F.T.N.B.buttonClick" cpt
  where
    cpt {action: todo, state: (node@{action} /\ setNodePopup)} _ = do
      pure $ H.div {className: "col-md-1"}
                   [ H.a { style: iconAStyle
                         , className: glyphiconActive (glyphiconNodeAction todo)
                                                       (action == (Just todo)   )
                         , id: show todo
                         , title: show todo
                         , onClick : mkEffectFn1
                                   $ \_ -> setNodePopup
                                   $ const (node { action = action' })
                       }
                     []
                   ]
      where
        action' = if action == (Just todo)
                      then Nothing
                      else (Just todo)

-- END Popup View
type NodeProps =
  ( id       :: ID
  , name     :: Name
  , nodeType :: GT.NodeType
  )


type PanelActionProps =
  ( id       :: ID
  , action   :: NodeAction
  , dispatch :: Action -> Aff Unit
  , name     :: Name
  , nodePopup :: Maybe NodePopup
  , nodeType :: GT.NodeType
  , session  :: Session
  , search   :: R.State Search
  )

panelAction :: Record PanelActionProps -> R.Element
panelAction p = R.createElement panelActionCpt p []

panelActionCpt :: R.Component PanelActionProps
panelActionCpt = R.hooksComponent "G.C.F.T.N.B.panelAction" cpt
  where
    cpt {action: Documentation nodeType}                  _ = actionDoc      nodeType
    cpt {action: Download, id, nodeType, session}         _ = actionDownload nodeType id session
    cpt {action: Upload, dispatch, id, nodeType, session} _ = actionUpload   nodeType id session dispatch
    cpt {action: Delete, nodeType, dispatch}              _ = actionDelete   nodeType dispatch

    cpt {action: Add xs, dispatch, id, name, nodePopup: p, nodeType} _ = do
      pure $ addNodeView {dispatch, id, name, nodeType, nodeTypes: xs}

    cpt {action: Refresh , dispatch, id, nodeType, session} _ = do
      pure $ fragmentPT $ "Update " <> show nodeType

    cpt {action: Config , dispatch, id, nodeType, session} _ = do
      pure $ fragmentPT $ "Config " <> show nodeType

    cpt {action: CopyFromCorpus, dispatch, id, nodeType, session} _ = do
      pure $ copyFromCorpusView {dispatch, id, nodeType, session}

    cpt {action: Link _} _ = pure $ fragmentPT $ "Soon, you will be able "
                                 <> "to link the corpus with your Annuaire"
                                 <> " (and reciprocally)."

    cpt {action : Share, dispatch, id, name } _ = do
      isOpen <- R.useState' true
      pure $ H.div {} [ textInputBox { boxAction: Share.shareAction
                                   , boxName: "Share"
                                   , dispatch
                                   , id
                                   , text: "username"
                                   , isOpen
                                   } ]
    cpt props@{action: SearchBox, search, session, dispatch, nodePopup} _ =
      actionSearch search session dispatch nodePopup

    cpt _ _ = do
      pure $ H.div {} []

-- | Action : Search
actionSearch :: R.State Search
            -> Session
            -> (Action -> Aff Unit)
            -> Maybe NodePopup
            -> R.Hooks R.Element
actionSearch search session dispatch nodePopup =
  pure $ R.fragment [ H.p {"style": {"margin" :"10px"}}
                          [ H.text $ "Search and create a private "
                                  <> "corpus with the search query as corpus name." ]
                    , searchBar { langs: allLangs
                                , onSearch: searchOn dispatch nodePopup
                                , search
                                , session
                                }
                    ]
    where
      searchOn :: (Action -> Aff Unit)
               -> Maybe NodePopup
               -> GT.AsyncTaskWithType
               -> Effect Unit
      searchOn dispatch' p task = do
        _ <- launchAff $ dispatch' (DoSearch task)
        -- close popup
        -- TODO
        --snd p $ const Nothing
        pure unit

