module Gargantext.Components.Forest.Tree.Node.Box where

import DOM.Simple.Console (log2)
import Data.Array (filter, null)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff, runAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Gargantext.Components.Forest.Tree.Node
import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Components.Forest.Tree.Node.Action.Add
import Gargantext.Components.Forest.Tree.Node.Action.Rename
import Gargantext.Components.Forest.Tree.Node.Action.Upload
import Gargantext.Components.Search.Types
import Gargantext.Components.Search.SearchBar

import Gargantext.Ends (Frontends, url)
import Gargantext.Routes (AppRoute, SessionRoute(..))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, get, put, post, postWwwUrlencoded, delete)
import Gargantext.Types (class ToQuery, toQuery, NodeType(..), NodePath(..), readNodeType, fldr)
import Gargantext.Utils (id, glyphicon, glyphiconActive)
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafePartial)
import Prelude hiding (div)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import Web.File.File (toBlob)
import Web.File.FileList (FileList, item)
import Web.File.FileReader.Aff (readAsText)


-- Main Node
type NodeMainSpanProps =
  ( id            :: ID
  , name          :: Name
  , nodeType      :: NodeType
  , mCurrentRoute :: Maybe AppRoute
  )

nodeMainSpan :: (Action -> Aff Unit)
             -> Record NodeMainSpanProps
             -> R.State Boolean
             -> Session
             -> Frontends
             -> R.Element
nodeMainSpan d p folderOpen session frontends = R.createElement el p []
  where
    el = R.hooksComponent "NodeMainSpan" cpt
    cpt {id, name, nodeType, mCurrentRoute} _ = do
      -- only 1 popup at a time is allowed to be opened
      popupOpen   <- R.useState' (Nothing :: Maybe NodePopup)
      droppedFile <- R.useState' (Nothing :: Maybe DroppedFile)
      isDragOver  <- R.useState' false

      pure $ H.span (dropProps droppedFile isDragOver) $
        [ folderIcon nodeType folderOpen
        , H.a { href: (url frontends (NodePath (sessionId session) nodeType (Just id)))
              , style: {marginLeft: "22px"}
              }
          [ nodeText { isSelected: (mCorpusId mCurrentRoute) == (Just id)
                     , name: name'} ]
        , if showBox then popOverIcon popupOpen else H.div {} []
        , if showBox 
             then nodePopupView  d { id
                                   , name:name'
                                   , nodeType
                                   , action: Nothing
                                   , session
                                   } popupOpen
             else H.div {} []
        , fileTypeView   d {id, nodeType} droppedFile isDragOver
        ]
          where
            name' = if nodeType == NodeUser then show session else name
            SettingsBox {show:showBox} = settingsBox nodeType


    folderIcon nodeType folderOpen'@(open /\ _) =
      H.a {onClick: R2.effToggler folderOpen'}
      [ H.i {className: fldr nodeType open} [] ]

    popOverIcon (popOver /\ setPopOver) =
      H.a { className: "glyphicon glyphicon-cog"
          , id: "rename-leaf"
          , on: { click: \_ -> setPopOver $ toggle }
          } []
      where
        toggle Nothing = Just NodePopup
        toggle _       = Nothing


    dropProps droppedFile isDragOver =
      { className: dropClass droppedFile isDragOver
      , on: { drop: dropHandler droppedFile
            , dragOver: onDragOverHandler isDragOver
            , dragLeave: onDragLeave isDragOver } }
      where
        dropClass (Just _ /\ _)  _           = "file-dropped"
        dropClass _              (true /\ _) = "file-dropped"
        dropClass (Nothing /\ _) _           = ""
        dropHandler (_ /\ setDroppedFile) e = unsafePartial $ do
          let ff = fromJust $ item 0 $ ((e .. "dataTransfer" .. "files") :: FileList)
          liftEffect $ log2 "drop:" ff
          -- prevent redirection when file is dropped
          E.preventDefault e
          E.stopPropagation e
          let blob = toBlob $ ff
          void $ runAff (\_ -> pure unit) do
            contents <- readAsText blob
            liftEffect $ setDroppedFile
                       $ const
                       $ Just
                       $ DroppedFile { contents: (UploadFileContents contents)
                                     , fileType: Just CSV
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
                   color NodeUser     = ""
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
  , name     :: Name
  , nodeType :: NodeType
  , action   :: Maybe NodeAction
  , session  :: Session
  )

iconAStyle = { color         : "black"
             , paddingTop    : "6px"
             , paddingBottom : "6px"}

nodePopupView :: (Action -> Aff Unit)
              -> Record NodePopupProps
              -> R.State (Maybe NodePopup)
              -> R.Element
nodePopupView d p mPop@(Just NodePopup /\ setPopupOpen) = R.createElement el p []
  where
    el = R.hooksComponent "NodePopupView" cpt
    cpt {id, name, nodeType, action, session} _ = do
      renameBoxOpen <- R.useState' false
      nodePopupState@(nodePopup /\ setNodePopup) <- R.useState' {id, name, nodeType, action}
      pure $ H.div tooltipProps $
        [ H.div {id: "arrow"} []
        , H.div { className: "panel panel-default"
                , style: { border    : "1px solid rgba(0,0,0,0.2)"
                         , boxShadow : "0 2px 5px rgba(0,0,0,0.2)"
                         }
                }
          [ H.div {className: ""}
                  [ H.div { className : "col-md-11"}
                          [ H.h3 { className: fldr nodeType true}
                                 [ H.text $ show nodeType]
                          ]
                  ]
          , panelHeading renameBoxOpen
          , panelBody nodePopupState d
          , if isJust nodePopup.action 
              then H.div { className: glyphicon "remove-circle"
                         , onClick : setNodePopup $ const {id,name,nodeType,action :Nothing}
                       } []
              else H.div {} []
          , panelAction d {id,name,nodeType,action:nodePopup.action, session} mPop
          ]
        ]
      where
        tooltipProps = { className: ""
                       , id: "node-popup-tooltip"
                       , title: "Node settings"
                       , data: { toggle: "tooltip"
                               , placement: "right"}
                       }

        SettingsBox {edit, buttons} = settingsBox nodeType

        panelHeading renameBoxOpen@(open /\ _) =
          H.div {className: "panel-heading"}
          [ -- H.h1 {className : "col-md-12"} [H.text "Settings Box"]
           H.div {className: "row" }
            [
             H.div {className: "col-md-8"} [ renameBox d {id, name, nodeType} renameBoxOpen ]
            , if edit then editIcon renameBoxOpen else H.div {} []
            , H.div {className: "col-md-1"}
              [ H.a { "type" : "button"
                    , className: glyphicon "remove-circle"
                    , onClick: mkEffectFn1 $ \_ -> setPopupOpen $ const Nothing
                    , title: "Close"} []
              ]
            ]
          ]
          where
            editIcon (false /\ setRenameBoxOpen) =
              H.div {className: "col-md-1"}
              [ H.a {style: {color: "black"}
                    , className: glyphicon "pencil"
                    , id: "rename1"
                    , title: "Rename"
                    , onClick: mkEffectFn1 $ \_ -> setRenameBoxOpen $ const true
                    }
                []
              ]
            editIcon (true /\ _) = H.div {} []

        panelBody nodePopupState d =
          H.div {className: "panel-body"
                , style: { display:"flex"
                         , justifyContent : "center"
                         , backgroundColor: "white"
                         , border: "none"
                         }
                }
          $ 
          [H.div {className: "col-md-1"} []]
          <>
          (map (buttonClick nodePopupState d) buttons)
nodePopupView _ p _ = R.createElement el p []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt _ _ = pure $ H.div {} []



-- buttonAction :: NodeAction -> R.Element
buttonClick (node@{action} /\ setNodePopup) _ todo = H.div {className: "col-md-2"}
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

buttonClick _ _ _ = H.div {} []






-- END Popup View

type NodeProps =
  ( id       :: ID
  , name     :: Name
  , nodeType :: NodeType
  )

type Open = Boolean

panelAction :: (Action -> Aff Unit)
            -> Record NodePopupProps
            -> R.State (Maybe NodePopup)
            -> R.Element
panelAction d {id,name,nodeType,action, session} p = case action of
    (Just (Documentation NodeUser))      -> R.fragment [H.div {} [ infoTitle NodeUser
                                                                 , H.p {} [ H.text "This account is personal"]
                                                                 , H.p {} [ H.text "See the instances terms of uses."]
                                                                 ]
                                                        ]
    (Just (Documentation FolderPrivate)) -> R.fragment [H.p {} [H.text $ "This folder and its children are private only!"]]
    (Just (Documentation FolderPublic))  -> R.fragment [H.p {} [H.text $ "Soon, you will be able to build public folders to share your work with the world!"]]
    (Just (Documentation FolderShared))  -> R.fragment [H.p {} [H.text $ "Soon, you will be able to build teams folders to share your work"]]
    (Just (Documentation x)) -> R.fragment [ H.p {} [H.text $ "More information on" <> show nodeType]]

    (Just (Link _))                      -> R.fragment [ H.p {} [H.text $ "Soon, you will be able to link the corpus with your Annuaire (and reciprocally)."]]
    (Just Upload)                        -> R.fragment [ H.p {} [H.text $ "Soon, you will be able to upload  your file here"]]
    (Just Download)                      -> R.fragment [ H.p {} [H.text $ "Soon, you will be able to dowload your file here"]]

    (Just SearchBox)         -> R.fragment [ H.p {} [ H.text $ "Search and create a private corpus with the search query as corpus name." ]
                                           , searchBar {session, databases:allDatabases, langs:allLangs, node_id: (Just id)}
                                           ]
    (Just Delete)            -> case nodeType of
        NodeUser -> R.fragment [ H.div {} [H.text "Yes, we are RGPD compliant! But you can not delete User Node yet (we are still on development). Thanks for your comprehensin."]]
        _        -> R.fragment [ H.div {} (map (\t -> H.p {} [H.text t]) ["Are your sure you want to delete it ?", "If yes, click again below."]), reallyDelete d]
    (Just (Add xs))          -> createNodeView d {id, name, nodeType} p xs
    _                        -> H.div {} []


infoTitle :: NodeType -> R.Element
infoTitle nt = H.div {} [ H.h3 {} [H.text "Documentation about " ]
                        , H.h3 {className: fldr nt true} [ H.text $ show nt ]
                        ]


reallyDelete d = H.div {className: "panel-footer"}
            [ H.a { type: "button"
                  , className: "btn glyphicon glyphicon-trash"
                  , id: "delete"
                  , title: "Delete"
                  , onClick: mkEffectFn1 $ \_ -> launchAff $ d $ DeleteNode}
              [H.text " Yes, delete!"]
            ]




