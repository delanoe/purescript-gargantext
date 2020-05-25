module Gargantext.Components.Forest.Tree.Node.Box where

import Gargantext.Prelude

import DOM.Simple as DOM
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Tuple (fst, Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Gargantext.Components.Forest.Tree.Node (NodeAction(..), SettingsBox(..), glyphiconNodeAction, settingsBox)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..), DroppedFile(..), FileType(..), ID, Name, UploadFileContents(..))

import Gargantext.Components.Forest.Tree.Node.Action.Add (NodePopup(..), createNodeView)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (renameBox)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadFileView, fileTypeView, uploadTermListView, copyFromCorpusView)
import Gargantext.Components.Forest.Tree.Node.ProgressBar (asyncProgressBar, BarType(..))
import Gargantext.Components.GraphExplorer.API as GEAPI
import Gargantext.Components.Lang (allLangs, Lang(EN))
import Gargantext.Components.Search.SearchBar (searchBar)
import Gargantext.Components.Search.SearchField (Search, defaultSearch, isIsTex)
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (AppRoute)
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, post)
import Gargantext.Types (NodeType(..))
import Gargantext.Types as GT
import Gargantext.Routes as GR
import Gargantext.Utils (glyphicon, glyphiconActive)
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.Reactix as R2
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as NQP
import URI.Query as Query
import Web.File.FileReader.Aff (readAsText)


type Dispatch = Action -> Aff Unit

type CommonProps =
  (
      dispatch :: Dispatch
    , session :: Session
  )


-- Main Node
type NodeMainSpanProps =
  ( id                :: ID
  , asyncTasks        :: Array GT.AsyncTaskWithType
  , folderOpen        :: R.State Boolean
  , frontends         :: Frontends
  , mCurrentRoute     :: Maybe AppRoute
  , name              :: Name
  , nodeType          :: GT.NodeType
  , onAsyncTaskFinish :: GT.AsyncTaskWithType -> Effect Unit
  | CommonProps
  )

nodeMainSpan :: Record NodeMainSpanProps
             -> R.Element
nodeMainSpan p@{ dispatch, folderOpen, frontends, session } = R.createElement el p []
  where
    el = R.hooksComponent "G.C.F.T.N.B.NodeMainSpan" cpt
    cpt props@{id, asyncTasks, mCurrentRoute, name, nodeType, onAsyncTaskFinish} _ = do
      -- only 1 popup at a time is allowed to be opened
      droppedFile   <- R.useState' (Nothing :: Maybe DroppedFile)
      isDragOver    <- R.useState' false

      popoverRef <- R.useRef null

      pure $ H.span (dropProps droppedFile isDragOver) $
        [ folderIcon nodeType folderOpen
        , if showBox then
            Popover.popover { open: false
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
                     , name: name' props } ]
        , nodeActions { id
                      , nodeType
                      , refreshTree: const $ dispatch RefreshTree
                      , session }
        , fileTypeView {dispatch, droppedFile, id, isDragOver, nodeType}
        , H.div {} (map (\t -> asyncProgressBar { asyncTask: t
                                                , barType: Pie
                                                , corpusId: id
                                                , onFinish: const $ onAsyncTaskFinish t
                                                , session }) asyncTasks)
        ]
          where
            SettingsBox {show: showBox} = settingsBox nodeType
            onPopoverClose popoverRef _ = do
              Popover.setOpen popoverRef false

    name' {name, nodeType} = if nodeType == GT.NodeUser then show session else name

    folderIcon nodeType folderOpen'@(open /\ _) =
      H.a { className: "folder-icon"
          , onClick: R2.effToggler folderOpen' }
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
  , name :: Name
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
  (
    id :: ID
  , nodeType :: GT.NodeType
  , refreshTree :: Unit -> Aff Unit
  , session :: Session
  )

nodeActions :: Record NodeActionsProps -> R.Element
nodeActions p = R.createElement nodeActionsCpt p []

nodeActionsCpt :: R.Component NodeActionsProps
nodeActionsCpt = R.hooksComponent "G.C.F.T.N.B.nodeActions" cpt
  where
    cpt { id, nodeType: GT.Graph, refreshTree, session } _ = do
      useLoader id (graphVersions session) $ \gv ->
        nodeActionsGraph { id, graphVersions: gv, session, triggerRefresh: triggerRefresh refreshTree }
    cpt _ _ = do
      pure $ H.div {} []

    graphVersions session graphId = GEAPI.graphVersions { graphId, session }
    triggerRefresh refreshTree = refreshTree

type NodeActionsGraphProps =
  (
    id             :: ID
  , graphVersions  :: Record GEAPI.GraphVersions
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
  (
    id :: ID
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

      pure $ H.div { className: "update-button " <> if (fst enabled) then "enabled" else "disabled text-muted" } [
        H.span { className: "fa fa-refresh"
               , on: { click: onClick enabled } } []
      ]
      where
        onClick (false /\ _) _ = pure unit
        onClick (true /\ setEnabled) _ = do
          launchAff_ $ do
            liftEffect $ setEnabled $ const false
            g <- GEAPI.updateGraphVersions { graphId: id, session }
            liftEffect $ setEnabled $ const true
            triggerRefresh unit
          pure unit

-- END nodeActions

mAppRouteId :: Maybe AppRoute -> Maybe Int
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
mAppRouteId (Just (Routes.Document       _ id _ )) = Just id
mAppRouteId (Just (Routes.ContactPage    _ id _ )) = Just id
mAppRouteId (Just (Routes.CorpusDocument _ id _ _)) = Just id

mAppRouteId _ = Nothing


-- | START Popup View
type NodePopupProps =
  ( id       :: ID
  , name     :: Name
  , nodeType :: GT.NodeType
  , onPopoverClose :: DOM.Element -> Effect Unit
  | CommonProps
  )

type NodePopupS =
  (
      action   :: Maybe NodeAction
    , id       :: ID
    , name     :: Name
    , nodeType :: GT.NodeType
  )

iconAStyle :: { color :: String, paddingTop :: String, paddingBottom :: String}
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
      renameBoxOpen <- R.useState' false
      nodePopupState@(nodePopup /\ setNodePopup) <- R.useState' {action: Nothing, id: p.id, name: p.name, nodeType: p.nodeType}
      search <- R.useState' $ defaultSearch { node_id = Just p.id }
      pure $ H.div tooltipProps $
        [ H.div { className: "popup-container" }
          [ H.div { className: "panel panel-default" }
            [ H.div {className: ""}
              [ H.div { className : "col-md-11"}
                [ H.h3 { className: GT.fldr p.nodeType true} [H.text $ show p.nodeType]
                , H.p {className: "text-primary center"} [H.text p.name]
                ]
              ]
            , panelHeading renameBoxOpen p
            , panelBody    nodePopupState p
            , mPanelAction nodePopupState p search
            ]
          , if nodePopup.action == Just SearchBox then
              H.div {}
                [
                  searchIsTexIframe p search
                ]
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

        panelHeading renameBoxOpen@(open /\ _) {dispatch, id, name, nodeType} =
          H.div {className: "panel-heading"}
                [ R2.row
                        [ H.div {className: "col-md-8"}
                                [ renameBox { dispatch, id, name, nodeType, renameBoxOpen } ]

                        , H.div {className: "flex-end"}
                                [ if edit then editIcon renameBoxOpen else H.div {} []
                                , H.div {className: "col-md-1"}
                                        [ H.a { "type"   : "button"
                                              , className: glyphicon "remove-circle"
                                              , on: { click: \e -> p.onPopoverClose $ R2.unsafeEventTarget e }
                                              , title    : "Close"} []
                                        ]
                                 ]
                        ]
                ]
          where
            SettingsBox {edit, doc, buttons} = settingsBox nodeType

            editIcon :: R.State Boolean -> R.Element
            editIcon (false /\ setRenameBoxOpen) =
              H.div {className : "col-md-1"}
              [ H.a { className: glyphicon "pencil"
                    , id       : "rename1"
                    , title    : "Rename"
                    , on: { click: \_ -> setRenameBoxOpen $ const true }
                    }
                []
              ]
            editIcon (true /\ _) = H.div {} []

        panelBody :: R.State (Record ActionState)
                  -> Record NodePopupProps
                  -> R.Element
        panelBody nodePopupState {dispatch: d, nodeType} =
          H.div {className: "panel-body flex-space-between"}
                [ H.div {className: "flex-center"} [buttonClick {action: doc, state: nodePopupState}]
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
        mPanelAction ({action: Just action} /\ _) p search =
            panelAction { action
                        , dispatch : p.dispatch
                        , id       : p.id
                        , name     : p.name
                        , nodePopup: Just NodePopup
                        , nodeType : p.nodeType
                        , search
                        , session  : p.session
                        }

        searchIsTexIframe {nodeType} search@(search' /\ _) =
          if isIsTex search'.datafield then
            H.div { className: "istex-search panel panel-default" }
            [
              H.h3 { className: GT.fldr nodeType true} []
            , componentIsTex search
            ]
          else
            H.div {} []

        componentIsTex (search /\ setSearch) =
          H.iframe { src: isTexTermUrl search.term , width: "100%", height: "100%"} []
        isTexUrl = "https://istex.gargantext.org"
        isTexLocalUrl = "http://localhost:8083"
        isTexTermUrl term = isTexUrl <> query
          where
            query = Query.print $ NQP.print identity identity qp

            qp = NQP.QueryPairs [
              Tuple (NQP.keyFromString "query") (Just (NQP.valueFromString term))
              ]


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

type Open = Boolean

type PanelActionProps =
  ( id       :: ID
  , action   :: NodeAction
  , dispatch :: Dispatch
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
    cpt {action: Documentation nodeType}          _ = actionDoc      nodeType
    cpt {action: Download, id, nodeType, session} _ = actionDownload nodeType id session
    cpt {action: Upload, dispatch, id, nodeType: GT.NodeList, session} _ = do
      pure $ uploadTermListView {dispatch, id, nodeType: GT.NodeList, session}

    cpt {action: Upload, dispatch, id, nodeType, session} _ = do
      pure $ uploadFileView {dispatch, id, nodeType, session}


    cpt props@{action: SearchBox, search, session} _ = do
      pure $ R.fragment [ H.p {"style": {"margin" :"10px"}}
                              [ H.text $ "Search and create a private corpus with the search query as corpus name." ]
                        , searchBar {langs: allLangs, onSearch: searchOn props, search, session}
                        ]

    cpt {action: Delete, nodeType: GT.NodeUser} _ = do
      pure $ R.fragment [
        H.div {style: {margin: "10px"}} [H.text "Yes, we are RGPD compliant! But you can not delete User Node yet (we are still on development). Thanks for your comprehensin."]
      ]

    cpt {action: Delete, dispatch} _ = do
      pure $ R.fragment [
        H.div {style: {margin: "10px"}} (map (\t -> H.p {} [H.text t]) ["Are your sure you want to delete it ?", "If yes, click again below."])
        , reallyDelete dispatch
        ]

    cpt {action: Add xs, dispatch, id, name, nodePopup: p, nodeType} _ = do
      pure $ createNodeView {dispatch, id, name, nodeType, nodeTypes: xs}

    cpt {action: CopyFromCorpus, dispatch, id, nodeType, session} _ = do
      pure $ copyFromCorpusView {dispatch, id, nodeType, session}

    cpt {action: Link _} _ = pure $ fragmentPT "Soon, you will be able to link the corpus with your Annuaire (and reciprocally)."
{-
    cpt {action: Refresh, nodeType: GT.Graph, id, session} _ = do

      pure $ H.div {className: "panel-footer"}
            [ H.a { type: "button"
                  , className: "btn glyphicon glyphicon-trash"
                  , id: "delete"
                  , title: "Delete"
                  , on: {click: \_ -> post session (GR.GraphAPI id $ GT.asyncTaskTypePath GT.GraphT) {}
                        -- TODO pure $ GT.AsyncTaskWithType { task, typ: GT.GraphT }
                        }
                  }
              [H.text " Yes, delete!"]
            ]
--}

    cpt _ _ = do
      pure $ H.div {} []


    searchOn :: Record PanelActionProps -> GT.AsyncTaskWithType -> Effect Unit
    searchOn {dispatch, nodePopup: p} task = do
      _ <- launchAff $ dispatch (SearchQuery task)
      -- close popup
      -- TODO
      --snd p $ const Nothing
      pure unit


reallyDelete :: Dispatch -> R.Element
reallyDelete d = H.div {className: "panel-footer"}
            [ H.a { type: "button"
                  , className: "btn glyphicon glyphicon-trash"
                  , id: "delete"
                  , title: "Delete"
                  , on: {click: \_ -> launchAff $ d $ DeleteNode}
                  }
              [H.text " Yes, delete!"]
            ]



-- | Action : Upload


-- | Action : Download
actionDownload :: NodeType -> ID -> Session -> R.Hooks R.Element
actionDownload NodeList id session = do
  let href = url session $ Routes.NodeAPI GT.NodeList (Just id) ""
  pure $ R.fragment [
    H.span { className: "row" }
           [ H.a { className: "btn btn-default"
                 , href
                 , target: "_blank" } [ H.text "Download list" ]
           ]
  ]
actionDownload GT.Graph id session = do
  pure $ R.fragment [ H.div { className: "gexf" }
                            [ H.a { className: "btn btn-default"
                                     , href: url session $ Routes.NodeAPI GT.Graph (Just id) "gexf"
                                     , target: "_blank" } [ H.text "Download GEXF" ]
                            ]
                    ]
actionDownload _ _ _ = do
  pure $ fragmentPT $ "Soon, you will be able to dowload your file here "



-- | Action: Show Documentation
actionDoc :: NodeType -> R.Hooks R.Element
actionDoc nodeType =
  pure $ R.fragment [ H.div { style: {margin: "10px"} }
                            $ [ infoTitle nodeType ]
                            <> (map (\info -> H.p {} [H.text info]) $ docOf nodeType)
                    ]
  where
    infoTitle :: NodeType -> R.Element
    infoTitle nt = H.div { style: {margin: "10px"}}
                         [ H.h3 {} [H.text "Documentation about " ]
                         , H.h3 {className: GT.fldr nt true} [ H.text $ show nt ]
                         ]

-- | TODO add documentation of all NodeType
docOf :: NodeType -> Array String
docOf GT.NodeUser = [ "This account is personal"
                    , "See the instances terms of uses."
                    ]
docOf GT.FolderPrivate = ["This folder and its children are private only."]
docOf GT.FolderPublic  = ["Soon, you will be able to build public folders to share your work with the world!"]
docOf GT.FolderShared  = ["Soon, you will be able to build teams folders to share your work"]
docOf nodeType         = ["More information on " <> show nodeType]


fragmentPT text = H.div {style: {margin: "10px"}} [H.text text]



