module Gargantext.Components.Forest.Tree.Node.Box where

import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (NodePopup(..), addNodeView)
import Gargantext.Components.Forest.Tree.Node.Action.Delete (actionDelete)
import Gargantext.Components.Forest.Tree.Node.Action.Documentation (actionDoc)
import Gargantext.Components.Forest.Tree.Node.Action.Download (actionDownload)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (renameAction)
import Gargantext.Components.Forest.Tree.Node.Action.Search (actionSearch)
import Gargantext.Components.Forest.Tree.Node.Action.Share   as Share
import Gargantext.Components.Forest.Tree.Node.Action.Contact as Contact
import Gargantext.Components.Forest.Tree.Node.Action.Update (update)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (actionUpload)
import Gargantext.Components.Forest.Tree.Node.Action.Move (moveNode)
import Gargantext.Components.Forest.Tree.Node.Action.Link (linkNode)
import Gargantext.Components.Forest.Tree.Node.Action.Merge (mergeNode)
import Gargantext.Components.Forest.Tree.Node.Box.Types (NodePopupProps, NodePopupS)
import Gargantext.Components.Forest.Tree.Node.Settings
  (NodeAction(..), SettingsBox(..), glyphiconNodeAction, settingsBox)
import Gargantext.Components.Forest.Tree.Node.Status (Status(..), hasStatus)
import Gargantext.Components.Forest.Tree.Node.Tools (textInputBox, fragmentPT, panel)
import Gargantext.Sessions (Session)
import Gargantext.Types (Name, ID, prettyNodeType)
import Gargantext.Types as GT
import Gargantext.Utils (glyphicon, glyphiconActive)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Box"

type CommonProps = ( dispatch :: Action -> Aff Unit, session :: Session )

nodePopupView :: Record NodePopupProps -> R.Element
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
          , panelBody    action p
          , mPanelAction nodePopup' p ]]]
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
    let (SettingsBox { edit, doc, buttons }) = settingsBox nodeType in
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
               { dispatch, id, name, nodeType, session, handed } =
    panelAction { action, dispatch, id, name, nodeType, session
                , handed, nodePopup: Just NodePopup }
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
  ( id        :: ID
  , action    :: NodeAction
  , dispatch  :: Action -> Aff Unit
  , name      :: Name
  , nodePopup :: Maybe NodePopup
  , nodeType  :: GT.NodeType
  , session   :: Session
  , handed    :: GT.Handed
  )

panelAction :: Record PanelActionProps -> R.Element
panelAction p = R.createElement panelActionCpt p []

panelActionCpt :: R.Component PanelActionProps
panelActionCpt = here.component "panelAction" cpt
  where
    cpt {action: Documentation nodeType}                  _ = actionDoc      nodeType
    cpt {action: Download, id, nodeType, session}         _ = actionDownload nodeType id session
    cpt {action: Upload, dispatch, id, nodeType, session} _ = actionUpload   nodeType id session dispatch
    cpt {action: Delete, nodeType, dispatch}              _ = actionDelete   nodeType dispatch
    cpt {action: Add xs, dispatch, id, name, nodeType} _ =
      pure $ addNodeView {dispatch, id, name, nodeType, nodeTypes: xs}
    cpt {action: Refresh , dispatch, id, nodeType, session} _ = update nodeType dispatch
    cpt {action: Config , dispatch, id, nodeType, session} _ =
      pure $ fragmentPT $ "Config " <> show nodeType
    -- Functions using SubTree
    cpt {action: Merge {subTreeParams}, dispatch, id, nodeType, session, handed} _ =
      pure $ mergeNode {dispatch, id, nodeType, session, subTreeParams, handed} []
    cpt {action: Move {subTreeParams}, dispatch, id, nodeType, session, handed} _ =
      pure $ moveNode { dispatch, id, nodeType, session, subTreeParams, handed } []
    cpt {action: Link {subTreeParams}, dispatch, id, nodeType, session, handed} _ =
      pure $ linkNode {dispatch, id, nodeType, session, subTreeParams, handed} []
    cpt {action : Share, dispatch, id, name } _ = do
      isOpen <- T.useBox true
      pure $ panel
        [ textInputBox
          { boxAction: Share.shareAction, boxName: "Share"
          , dispatch, id, text: "username", isOpen } []
        ] (H.div {} [])
    cpt {action : AddingContact, dispatch, id, name } _ = do
      isOpen <- T.useBox true
      pure $ Contact.textInputBox
        { id, dispatch, isOpen, boxName:"addContact"
        , params : {firstname:"First Name", lastname: "Last Name"}
        , boxAction: \p -> AddContact p }
    cpt {action : Publish {subTreeParams}, dispatch, id, nodeType, session, handed} _ =
      pure $ Share.shareNode {dispatch, id, nodeType, session, subTreeParams, handed}
    cpt props@{action: SearchBox, id, session, dispatch, nodePopup} _ =
      pure $ actionSearch { dispatch, id: (Just id), nodePopup, session } []
    cpt _ _ = pure $ H.div {} []
