module Gargantext.Components.Forest.Tree.Node.Box where

import DOM.Simple.Console (log2)
import Data.Array (filter, null)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff, runAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Components.Forest.Tree.Node.Action.Rename
import Gargantext.Components.Forest.Tree.Node.Action.Add
import Gargantext.Components.Forest.Tree.Node.Action.Upload
import Gargantext.Components.Forest.Tree.Node
import Gargantext.Ends (Frontends, url)
import Gargantext.Routes (AppRoute, SessionRoute(..))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, get, put, post, postWwwUrlencoded, delete)
import Gargantext.Types (class ToQuery, toQuery, NodeType(..), NodePath(..), readNodeType)
import Gargantext.Utils (id, glyphicon)
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
        [ folderIcon folderOpen
        , H.a { href: (url frontends (NodePath (sessionId session) nodeType (Just id)))
              , style: {marginLeft: "22px"}
              }
          [ nodeText { isSelected: (mCorpusId mCurrentRoute) == (Just id)
                     , name: name'} ]
        , if showBox then popOverIcon popupOpen else H.div {} []
        , if showBox 
             then nodePopupView  d {id, name:name', nodeType} popupOpen
             else H.div {} []
        , addButton popupOpen
        , fileTypeView   d {id, nodeType} droppedFile isDragOver
        ]
          where
            name' = if nodeType == NodeUser then show session else name
            SettingsBox {show:showBox, add} = settingsBox nodeType
            addButton p = if null add
                           then H.div {} []
                           else createNodeView d {id, name, nodeType} p


    folderIcon folderOpen'@(open /\ _) =
      H.a {onClick: R2.effToggler folderOpen'}
      [ H.i {className: fldr open} [] ]

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
            liftEffect $ setDroppedFile $ const $ Just $ DroppedFile {contents: (UploadFileContents contents), fileType: Just CSV}
    onDragOverHandler (_ /\ setIsDragOver) e = do
      -- prevent redirection when file is dropped
      -- https://stackoverflow.com/a/6756680/941471
      E.preventDefault e
      E.stopPropagation e
      setIsDragOver $ const true
    onDragLeave (_ /\ setIsDragOver) _ = setIsDragOver $ const false


fldr :: Boolean -> String
fldr open = if open
               then "glyphicon glyphicon-folder-open"
               else "glyphicon glyphicon-folder-close"


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
  )

iconAStyle = {color:"black", paddingTop: "6px", paddingBottom: "6px"}

nodePopupView :: (Action -> Aff Unit)
              -> Record NodePopupProps
              -> R.State (Maybe NodePopup)
              -> R.Element
nodePopupView d p (Just NodePopup /\ setPopupOpen) = R.createElement el p []
  where
    el = R.hooksComponent "NodePopupView" cpt
    cpt {id, name, nodeType} _ = do
      renameBoxOpen <- R.useState' false
      pure $ H.div tooltipProps $
        [ H.div {id: "arrow"} []
        , H.div { className: "panel panel-default"
                , style: { border: "1px solid rgba(0,0,0,0.2)"
                         , boxShadow : "0 2px 5px rgba(0,0,0,0.2)"
                         }
                }
          [ panelHeading renameBoxOpen
          , panelBody
          ]
        ]
      where
        tooltipProps = { className: ""
                       , id: "node-popup-tooltip"
                       , title: "Node settings"
                       , data: {toggle: "tooltip", placement: "right"}
                       }
        rowClass true  = "col-md-10"
        rowClass false = "col-md-10"

        SettingsBox {edit, add, buttons} = settingsBox nodeType

        panelHeading renameBoxOpen@(open /\ _) =
          H.div {className: "panel-heading"}
          [ -- H.h1 {className : "col-md-12"} [H.text "Settings Box"]
           H.div {className: "row" }
            [ H.div {className: rowClass open} [ renameBox d {id, name} renameBoxOpen ]
            , if edit then editIcon renameBoxOpen else H.div {} []
            , H.div {className: "col-md-1"}
              [ H.a { type : "button"
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

        panelBody =
          H.div {className: "panel-body"
                , style: { display:"flex"
                         , justifyContent : "center"
                         , backgroundColor: "white"
                         , border: "none"}}
          $ 
          (map (buttonClick d) buttons)
          <>
          ( [if null add then H.div {} [] else buttonPop setPopupOpen] )
nodePopupView _ p _ = R.createElement el p []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt _ _ = pure $ H.div {} []


-- buttonAction :: NodeAction -> R.Element
buttonClick _ (Documentation x ) = H.div {className: "col-md-1"}
            [ H.a { style: iconAStyle
                  , className: (glyphicon "question-sign")
                  , id: "doc"
                  , title: "Documentation of " <> show x
                }
                  -- , onClick: mkEffectFn1 $ \_ -> launchAff $ d $ DeleteNode}
              []
            ]

buttonClick d Delete = H.div {className: "col-md-4"}
            [ H.a { style: iconAStyle
                  , className: (glyphicon "trash")
                  , id: "rename2"
                  , title: "Delete"
                  , onClick: mkEffectFn1 $ \_ -> launchAff $ d $ DeleteNode}
              []
            ]

buttonClick _ Upload = H.div {className: "col-md-4"}
            [ H.a { style: iconAStyle
                  , className: (glyphicon "upload")
                  , id: "upload"
                  , title: "Upload [WIP]"}
              []
            ]

buttonClick _ Download = H.div {className: "col-md-4"}
            [ H.a {style: iconAStyle
                  , className: (glyphicon "download")
                  , id: "download"
                  , title: "Download [WIP]"}
              []
            ]

buttonClick _ _ = H.div {} []


buttonPop f =  H.div {className: "col-md-4"}
              [ H.a { style: iconAStyle
                    , className: (glyphicon "plus")
                    , id: "create"
                    , title: "Create"
                    , onClick: mkEffectFn1 $ \_ -> f $ const $ Just CreatePopup
                    }
                []
              ]

-- END Popup View

