module Gargantext.Components.Forest.Tree.Node.Box where

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as NQP
import URI.Query as Query
import Web.File.FileReader.Aff (readAsText)

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node (NodeAction(..), SettingsBox(..), glyphiconNodeAction, settingsBox)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..), DroppedFile(..), FileType(..), ID, Name, UploadFileContents(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (NodePopup(..), createNodeView)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (renameBox)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadFileView, fileTypeView, uploadTermListView)
import Gargantext.Components.Forest.Tree.Node.ProgressBar (asyncProgressBar)
import Gargantext.Components.Data.Lang (allLangs, Lang(EN))
import Gargantext.Components.Search.SearchBar (searchBar)
import Gargantext.Components.Search.SearchField (Search, defaultSearch, isIsTex)

import Gargantext.Ends (Frontends, url)
import Gargantext.Routes (AppRoute)
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types as GT
import Gargantext.Utils (glyphicon, glyphiconActive)
import Gargantext.Utils.Reactix as R2


type Dispatch = Action -> Aff Unit


-- Main Node
type NodeMainSpanProps =
  ( id            :: ID
  , asyncTasks    :: Array GT.AsyncTaskWithType
  , mCurrentRoute :: Maybe AppRoute
  , name          :: Name
  , nodeType      :: GT.NodeType
  , onAsyncTaskFinish :: GT.AsyncTaskWithType -> Effect Unit
  )

nodeMainSpan :: Dispatch
             -> Record NodeMainSpanProps
             -> R.State Boolean
             -> Session
             -> Frontends
             -> R.Element
nodeMainSpan d p folderOpen session frontends = R.createElement el p []
  where
    el = R.hooksComponent "G.C.F.T.N.B.NodeMainSpan" cpt
    cpt props@{id, asyncTasks, mCurrentRoute, name, nodeType, onAsyncTaskFinish} _ = do
      -- only 1 popup at a time is allowed to be opened
      popupOpen     <- R.useState' (Nothing :: Maybe NodePopup)
      popupPosition <- R.useState' (Nothing :: Maybe R2.Point)
      droppedFile   <- R.useState' (Nothing :: Maybe DroppedFile)
      isDragOver    <- R.useState' false

      pure $ H.span (dropProps droppedFile isDragOver) $
        [ folderIcon nodeType folderOpen
        , H.a { href: (url frontends (GT.NodePath (sessionId session) nodeType (Just id)))
              , style: {marginLeft: "22px"}
              }
          [ nodeText { isSelected: (mCorpusId mCurrentRoute) == (Just id)
                     , name: name' props} ]
        , popOverIcon showBox popupOpen popupPosition
        , mNodePopupView props showBox popupOpen popupPosition
        , fileTypeView {action: d, droppedFile, id, isDragOver, nodeType}
        , H.div {} (map (\t -> asyncProgressBar { asyncTask: t
                                                , corpusId: id
                                                , onFinish: \_ -> onAsyncTaskFinish t
                                                , session }) asyncTasks)
        ]
          where
            SettingsBox {show: showBox} = settingsBox nodeType

    name' {name, nodeType} = if nodeType == GT.NodeUser then show session else name

    folderIcon nodeType folderOpen'@(open /\ _) =
      H.a {onClick: R2.effToggler folderOpen'}
      [ H.i {className: GT.fldr nodeType open} [] ]

    popOverIcon false _ _ = H.div {} []
    popOverIcon true (popOver /\ setPopOver) (_ /\ setPopupPosition) =
      H.a { className: "glyphicon glyphicon-cog"
          , id: "rename-leaf"
          , on: { click: toggle popOver }
          } []
      where
        toggle Nothing e = do
          setPopupPosition $ const $ Just $ R2.mousePosition e
          setPopOver $ const $ Just NodePopup
        toggle _ _ = do
          setPopupPosition $ const Nothing
          setPopOver $ const Nothing

    mNodePopupView _ false _ _ = H.div {} []
    mNodePopupView _ _ (Nothing /\ _) _ = H.div {} []
    mNodePopupView _ _ _ (Nothing /\ _) = H.div {} []
    mNodePopupView props@{asyncTasks, id, nodeType} true popupOpen (Just position /\ _) =
      nodePopupView { id
                    , dispatch: d
                    , name: name' props
                    , nodePopupState: popupOpen
                    , nodeType
                    , position
                    , session
                    }

    dropProps droppedFile isDragOver =
      { className: dropClass droppedFile isDragOver
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
nodeText p = R.createElement el p []
  where
    el = R.hooksComponent "NodeText" cpt
    cpt {isSelected: true, name} _ = do
      pure $ H.u {} [H.b {} [H.text ("| " <> name <> " |    ")]]
    cpt {isSelected: false, name} _ = do
      pure $ H.text (name <> "    ")
-- END node text

mCorpusId :: Maybe AppRoute -> Maybe Int
mCorpusId (Just (Routes.Corpus _ id)) = Just id
mCorpusId (Just (Routes.CorpusDocument _ id _ _)) = Just id
mCorpusId _ = Nothing


-- | START Popup View
type NodePopupProps =
  ( id       :: ID
  , dispatch :: Dispatch
  , name     :: Name
  , nodePopupState :: R.State (Maybe NodePopup)
  , nodeType :: GT.NodeType
  , position :: R2.Point
  , session  :: Session
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
    cpt p@{nodePopupState: mPop@(Just NodePopup /\ setPopupOpen)} _ = do
      renameBoxOpen <- R.useState' false
      nodePopupState@(nodePopup /\ setNodePopup) <- R.useState' {action: Nothing, id: p.id, name: p.name, nodeType: p.nodeType}
      search <- R.useState' $ defaultSearch { node_id = Just p.id }
      pure $ H.div (tooltipProps p.position) $
        [ H.div {id: "arrow"} []
        , H.div { className: "popup-container" }
          [ H.div { className: "panel panel-default" }
            [ H.div {className: ""}
              [ H.div { className : "col-md-11"}
                [ H.h3 { className: GT.fldr p.nodeType true} [H.text $ show p.nodeType]
                , H.p {className: "text-primary center"} [H.text p.name]
                ]
              ]
            , panelHeading renameBoxOpen p
            , panelBody    nodePopupState p
            , mPanelAction nodePopup.action p search
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
        tooltipProps (R2.Point {x, y}) = {
          className: ""
          , id: "node-popup-tooltip"
          , title: "Node settings"
          , data: { toggle: "tooltip"
                  , placement: "right"},
            style: { top: y - 65.0, left: x + 10.0 }
          }

        panelHeading renameBoxOpen@(open /\ _) {dispatch: d, id, name, nodeType} =
          H.div {className: "panel-heading"}
                [ R2.row
                        [ H.div {className: "col-md-8"}
                                [ renameBox d {id, name, nodeType} renameBoxOpen ]

                        , H.div {className: "flex-end"}
                                [ if edit then editIcon renameBoxOpen else H.div {} []
                                , H.div {className: "col-md-1"}
                                        [ H.a { "type"   : "button"
                                              , className: glyphicon "remove-circle"
                                              , onClick  : mkEffectFn1
                                                         $ \_ -> setPopupOpen $ const Nothing
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
                    , onClick  : mkEffectFn1
                               $ \_ -> setRenameBoxOpen $ const true
                    }
                []
              ]
            editIcon (true /\ _) = H.div {} []

        panelBody :: R.State (Record ActionState) -> Record NodePopupProps -> R.Element
        panelBody nodePopupState {dispatch: d, nodeType} =
          H.div {className: "panel-body flex-space-between"}
                [ H.div {className: "flex-center"} [buttonClick {action: doc, state: nodePopupState}]
                , H.div {className: "flex-center"}
                        $ map (\t -> buttonClick {action: t, state: nodePopupState}) buttons
                ]
          where
            SettingsBox {edit, doc, buttons} = settingsBox nodeType

        mPanelAction :: Maybe NodeAction -> Record NodePopupProps -> R.State Search -> R.Element
        mPanelAction Nothing _ _ = H.div {} []
        mPanelAction (Just a) p search =
            panelAction {
                  action: a
                , dispatch: p.dispatch
                , id: p.id
                , name: p.name
                , nodePopupState: mPop
                , nodeType: p.nodeType
                , search
                , session: p.session
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

    cpt _ _ = pure $ H.div {} []


type ActionState =
  (
    action :: Maybe NodeAction
  , id :: ID
  , name :: Name
  , nodeType :: GT.NodeType
  )


type ButtonClickProps =
  (
    action :: NodeAction
  , state :: R.State (Record ActionState)
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
  , nodePopupState :: R.State (Maybe NodePopup)
  , nodeType :: GT.NodeType
  , session  :: Session
  , search   :: R.State Search
  )

panelAction :: Record PanelActionProps -> R.Element
panelAction p = R.createElement panelActionCpt p []

panelActionCpt :: R.Component PanelActionProps
panelActionCpt = R.hooksComponent "G.C.F.T.N.B.panelAction" cpt
  where
    cpt {action: Documentation GT.NodeUser} _ = do
      pure $ R.fragment [
        H.div {style: {margin: "10px"}} [ infoTitle GT.NodeUser
                                        , H.p {} [ H.text "This account is personal"]
                                        , H.p {} [ H.text "See the instances terms of uses."]
                                        ]
        ]
    cpt {action: Documentation GT.FolderPrivate} _ = do
      pure $ fragmentPT "This folder and its children are private only!"
    cpt {action: Documentation GT.FolderPublic} _ = do
      pure $ fragmentPT "Soon, you will be able to build public folders to share your work with the world!"
    cpt {action: Documentation GT.FolderShared} _ = do
      pure $ fragmentPT "Soon, you will be able to build teams folders to share your work"
    cpt {action: Documentation x, nodeType} _ = do
      pure $ fragmentPT $ "More information on" <> show nodeType

    cpt {action: Link _} _ = do
      pure $ fragmentPT "Soon, you will be able to link the corpus with your Annuaire (and reciprocally)."
    cpt {action: Upload, dispatch: d, id, nodeType: GT.NodeList, session} _ = do
      pure $ uploadTermListView d {id, nodeType: GT.NodeList, session}
    cpt {action: Upload, dispatch: d, id, nodeType, session} _ = do
      pure $ uploadFileView d {id, nodeType, session}
    cpt {action: Download} _ = do
      pure $ fragmentPT "Soon, you will be able to dowload your file here"
    cpt props@{action: SearchBox, search, session} _ = do
      pure $ R.fragment [
          H.p {"style": {"margin" :"10px"}} [ H.text $ "Search and create a private corpus with the search query as corpus name." ]
        , searchBar {langs: allLangs, onSearch: onSearch props, search, session}
      ]
    cpt {action: Delete, nodeType: GT.NodeUser} _ = do
      pure $ R.fragment [
        H.div {style: {margin: "10px"}} [H.text "Yes, we are RGPD compliant! But you can not delete User Node yet (we are still on development). Thanks for your comprehensin."]
      ]
    cpt {action: Delete, dispatch: d} _ = do
      pure $ R.fragment [
        H.div {style: {margin: "10px"}} (map (\t -> H.p {} [H.text t]) ["Are your sure you want to delete it ?", "If yes, click again below."])
        , reallyDelete d
        ]
    cpt {action: Add xs, dispatch: d, id, name, nodePopupState: p, nodeType} _ = do
      pure $ createNodeView d {id, name, nodeType} p xs
    cpt _ _ = do
      pure $ H.div {} []

    fragmentPT text = H.div {style: {margin: "10px"}} [H.text text]

    onSearch :: Record PanelActionProps -> GT.AsyncTaskWithType -> Effect Unit
    onSearch {dispatch: d, nodePopupState: p} task = do
      _ <- launchAff $ d (SearchQuery task)
      -- close popup
      snd p $ const Nothing
      pure unit


infoTitle :: GT.NodeType -> R.Element
infoTitle nt = H.div {style: {margin: "10px"}} [ H.h3 {} [H.text "Documentation about " ]
                        , H.h3 {className: GT.fldr nt true} [ H.text $ show nt ]
                        ]

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

