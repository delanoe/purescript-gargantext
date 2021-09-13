module Gargantext.Components.Forest.Tree.Node.Box where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Forest.Tree.Node.Action.Add (NodePopup(..), addNodeView)
import Gargantext.Components.Forest.Tree.Node.Action.Contact as Contact
import Gargantext.Components.Forest.Tree.Node.Action.Delete (actionDelete)
import Gargantext.Components.Forest.Tree.Node.Action.Documentation (actionDoc)
import Gargantext.Components.Forest.Tree.Node.Action.Download (actionDownload)
import Gargantext.Components.Forest.Tree.Node.Action.Link (linkNode)
import Gargantext.Components.Forest.Tree.Node.Action.Merge (mergeNode)
import Gargantext.Components.Forest.Tree.Node.Action.Move (moveNode)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (renameAction)
import Gargantext.Components.Forest.Tree.Node.Action.Search (actionSearch)
import Gargantext.Components.Forest.Tree.Node.Action.Share as Share
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action)
import Gargantext.Components.Forest.Tree.Node.Action.Update (update)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (actionUpload)
import Gargantext.Components.Forest.Tree.Node.Box.Types (NodePopupProps, NodePopupS)
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction(..), SettingsBox(..), glyphiconNodeAction, settingsBox)
import Gargantext.Components.Forest.Tree.Node.Status (Status(..), hasStatus)
import Gargantext.Components.Forest.Tree.Node.Tools (fragmentPT, textInputBox)
import Gargantext.Sessions (Session)
import Gargantext.Types (ID, Name, prettyNodeType)
import Gargantext.Types as GT
import Gargantext.Utils.Glyphicon (glyphicon, glyphiconActive)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Box"

type CommonProps =
  ( dispatch :: Action -> Aff Unit
  , session :: Session )

nodePopupView :: R2.Leaf NodePopupProps
nodePopupView p = R.createElement nodePopupCpt p []
nodePopupCpt :: R.Component NodePopupProps
nodePopupCpt = here.component "nodePopupView" cpt where
  cpt p@{ id, name, nodeType }  _ = do
    renameIsOpen <- T.useBox false
    open <- T.useLive T.unequal renameIsOpen
    nodePopup <- T.useBox { action: Nothing, id, name, nodeType }
    action <- T.useFocused (_.action) (\a b -> b { action = a }) nodePopup
    nodePopup' <- T.useLive T.unequal nodePopup

    pure $ H.div tooltipProps
      [ H.div { className: "popup-container" }
        [ H.div { className: "card" }
          [ panelHeading renameIsOpen open p
          , H.div { className: "popup-container-body" }
            [
              panelBody    action p
            ,
              mPanelAction nodePopup' p
            ]
          ]
        ]
      ]
  closePopover p = p.onPopoverClose <<< R.unsafeEventTarget
  tooltipProps = { id: "node-popup-tooltip", title: "Node settings"
                 , data: { toggle: "tooltip", placement: "right" } }
  panelHeading renameIsOpen open p@{ dispatch, id, name, nodeType } =
    H.div { className: "card-header" }
    [ R2.row
      [ H.div { className: "col-4" }
        [ H.span { className: GT.fldr nodeType true} [] -- TODO fix names
        , H.span { className: "h5" } [ H.text $ prettyNodeType nodeType ] ]
      , H.div { className: "col-6" }
        [ if open then
            textInputBox { boxAction: renameAction, boxName: "Rename"
                         , dispatch, id, text: name, isOpen: renameIsOpen } []
          else H.span { className: "text-primary center" } [ H.text p.name ]
        ]
      , H.div { className: "col-1" } [ editIcon renameIsOpen open ]
      , H.div { className: "col-1" }
        [ H.a { type: "button", on: { click: closePopover p }, title: "Close"
              , className: glyphicon "window-close" } [] ]]] where
           SettingsBox { edit, doc, buttons } = settingsBox nodeType
  editIcon _ true = H.div {} []
  editIcon isOpen false =
    H.a { className: glyphicon "pencil", id: "rename1"
        , title    : "Rename", on: { click: \_ -> T.write_ true isOpen } } []
  panelBody :: T.Box (Maybe NodeAction) -> Record NodePopupProps -> R.Element
  panelBody nodePopupState {dispatch: d, nodeType} =
    let (SettingsBox { edit, doc, buttons}) = settingsBox nodeType in
    H.div {className: "card-body flex-space-between"}
    $ [ H.p { className: "spacer" } []
      , H.div { className: "flex-center" }
        [ buttonClick { action: doc, state: nodePopupState, nodeType } ]
      , H.div {className: "flex-center"}
        $ map (\t -> buttonClick { action: t, state: nodePopupState, nodeType }) buttons ]
        -- FIXME trick to increase the size of the box
        <> if A.length buttons < 2
           then [ H.div { className: "col-4" } [] ]
           else []
  mPanelAction :: Record NodePopupS -> Record NodePopupProps -> R.Element
  mPanelAction { action: Just action }
               { boxes, dispatch, id, name, nodeType, session } =
    panelAction { action
                , boxes
                , dispatch
                , id
                , name
                , nodePopup: Just NodePopup
                , nodeType
                , session
                }
  mPanelAction { action: Nothing } _ =
    H.div { className: "card-footer" }
    [ H.div {className:"center fa-hand-pointer-o"}
      [ H.h5 {} [ H.text " Select available actions of this node" ]
      , H.ul { className: "panel-actions" }
        [ H.div { className: "fa-thumbs-o-up ok-to-use" }
          [ H.text " Black: usable" ]
        , H.div { className: "fa-exclamation-triangle almost-useable" }
          [ H.text " Orange: almost useable" ]
        , H.div { className: "fa-rocket development-in-progress" }
          [ H.text " Red: development in progress" ]]]]

type ActionState =
  ( action   :: Maybe NodeAction
  , id       :: ID
  , name     :: Name
  , nodeType :: GT.NodeType
  )

type ButtonClickProps =
  ( action   :: NodeAction
  , state    :: T.Box (Maybe NodeAction)
  , nodeType :: GT.NodeType
  )

buttonClick :: Record ButtonClickProps -> R.Element
buttonClick p = R.createElement buttonClickCpt p []

buttonClickCpt :: R.Component ButtonClickProps
buttonClickCpt = here.component "buttonClick" cpt where
  cpt {action: todo, state, nodeType} _ = do
    action <- T.useLive T.unequal state
    let className = glyphiconActive (glyphiconNodeAction todo) (action == (Just todo))
    let style = iconAStyle nodeType todo
    let click _ = T.write_ (if action == Just todo then Nothing else Just todo) state
    pure $ H.div { className: "col-1" }
      [ H.a { style, className, id: show todo, title: show todo, on: { click } } [] ]
        -- | Open the help indications if selected already
  iconAStyle n a =
    { color: hasColor (hasStatus n a)
    , paddingTop: "6px", paddingBottom: "6px" }

hasColor :: Status -> String
hasColor Stable = "black"
hasColor Test   = "orange"
hasColor Dev    = "red"

type NodeProps =
  ( id       :: ID
  , name     :: Name
  , nodeType :: GT.NodeType
  )


type PanelActionProps =
  ( action    :: NodeAction
  , boxes     :: Boxes
  , id        :: ID
  , dispatch  :: Action -> Aff Unit
  , name      :: Name
  , nodePopup :: Maybe NodePopup
  , nodeType  :: GT.NodeType
  , session   :: Session
  )

panelAction :: R2.Leaf PanelActionProps
panelAction p = R.createElement panelActionCpt p []
panelActionCpt :: R.Component PanelActionProps
panelActionCpt = here.component "panelAction" cpt
  where
    cpt { action: Documentation nodeType }                  _ = pure $ actionDoc { nodeType } []
    cpt { action: Download, id, nodeType, session }         _ = pure $ actionDownload { id, nodeType, session } []
    cpt { action: Upload, dispatch, id, nodeType, session } _ = pure $ actionUpload { dispatch, id, nodeType, session } []
    cpt { action: Delete, dispatch, nodeType }              _ = pure $ actionDelete { dispatch, nodeType } []
    cpt { action: Add xs, dispatch, id, name, nodeType } _ =
      pure $ addNodeView {dispatch, id, name, nodeType, nodeTypes: xs } []
    cpt { action: Refresh , dispatch, nodeType } _ = pure $ update { dispatch, nodeType } []
    cpt { action: Config, nodeType } _ =
      pure $ fragmentPT $ "Config " <> show nodeType
    -- Functions using SubTree
    cpt { action: Merge {subTreeParams}, boxes, dispatch, id, nodeType, session } _ =
      pure $ mergeNode { boxes, dispatch, id, nodeType, session, subTreeParams } []
    cpt { action: Move {subTreeParams}, boxes, dispatch, id, nodeType, session } _ =
      pure $ moveNode { boxes, dispatch, id, nodeType, session, subTreeParams } []
    cpt { action: Link {subTreeParams}, boxes, dispatch, id, nodeType, session } _ =
      pure $ linkNode { boxes, dispatch, id, nodeType, session, subTreeParams } []
    cpt { action : Share, dispatch, id } _ = pure $ Share.shareNode { dispatch, id } []
    cpt { action : AddingContact, dispatch, id } _ = pure $ Contact.actionAddContact { dispatch, id } []
    cpt { action : Publish {subTreeParams}, boxes, dispatch, id, nodeType, session } _ =
      pure $ Share.publishNode { boxes, dispatch, id, nodeType, session, subTreeParams } []
    cpt { action: SearchBox, boxes, dispatch, id, nodePopup, session } _ =
      pure $ actionSearch { boxes, dispatch, id: (Just id), nodePopup, session } []
    cpt _ _ = pure $ H.div {} []
