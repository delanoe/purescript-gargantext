module Gargantext.Components.Forest.Tree.Node.Box where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Elevation(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (addNodeView)
import Gargantext.Components.Forest.Tree.Node.Action.Contact as Contact
import Gargantext.Components.Forest.Tree.Node.Action.Delete (actionDelete)
import Gargantext.Components.Forest.Tree.Node.Action.Documentation (actionDoc)
import Gargantext.Components.Forest.Tree.Node.Action.Download (actionDownload)
import Gargantext.Components.Forest.Tree.Node.Action.Link (linkNode)
import Gargantext.Components.Forest.Tree.Node.Action.ManageTeam (actionManageTeam)
import Gargantext.Components.Forest.Tree.Node.Action.Merge (mergeNode)
import Gargantext.Components.Forest.Tree.Node.Action.Move (moveNode)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (renameAction)
import Gargantext.Components.Forest.Tree.Node.Action.Search (actionSearch)
import Gargantext.Components.Forest.Tree.Node.Action.Share as Share
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action)
import Gargantext.Components.Forest.Tree.Node.Action.Update (update)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (actionUpload)
import Gargantext.Components.Forest.Tree.Node.Action.WriteNodesDocuments (actionWriteNodesDocuments)
import Gargantext.Components.Forest.Tree.Node.Box.Types (NodePopupProps, NodePopupS)
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction(..), SettingsBox(..), glyphiconNodeAction, settingsBox)
import Gargantext.Components.Forest.Tree.Node.Status (Status(..), hasStatus)
import Gargantext.Components.Forest.Tree.Node.Tools (fragmentPT, textInputBox)
import Gargantext.Sessions (Session)
import Gargantext.Types (ID, Name, prettyNodeType)
import Gargantext.Types as GT
import Gargantext.Utils.Glyphicon (glyphiconActive)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Box"

type CommonProps =
  ( dispatch  :: Action -> Aff Unit
  , session   :: Session
  )

nodePopupView :: R2.Leaf NodePopupProps
nodePopupView = R2.leafComponent nodePopupCpt
nodePopupCpt :: R.Component NodePopupProps
nodePopupCpt = here.component "nodePopupView" cpt where
  cpt p@{ id, name, nodeType }  _ = do
    renameIsOpen <- T.useBox false
    open <- T.useLive T.unequal renameIsOpen
    nodePopup <- T.useBox { action: Nothing, id, name, nodeType }
    action <- T.useFocused (_.action) (\a b -> b { action = a }) nodePopup
    nodePopup' <- T.useLive T.unequal nodePopup

    pure $

      H.div
      { className: "node-popup-tooltip"
      , title: "Node settings"
      }
      [
        H.div
        { className: "popup-container card" }
        [
          panelHeading renameIsOpen open p
        ,
          panelBody    action p
        ,
          mPanelAction nodePopup' p
        ]
      ]

  panelHeading renameIsOpen open p@{ dispatch, id, name, nodeType } =
    H.div
    { className: "popup-container__header card-header" }
    [
      B.wad
      [ "d-flex", "align-items-center" ]
      [
        B.wad
        [ "w-3/12" ]
        [
          H.span
          { className: GT.fldr nodeType true} [] -- TODO fix names
        ,
          B.span' { className: "ml-1 h5" } $ prettyNodeType nodeType
        ]
      ,
        B.wad
        [ "w-7/12", "pl-1" ]
        [
          if open then
            textInputBox
            { boxAction: renameAction
            , boxName: "Rename"
            , dispatch
            , id
            , text: name
            , isOpen: renameIsOpen
            } []
          else
            B.wad'
            [ "text-primary" ]
            p.name
        ]
      ,
        B.wad
        [ "w-2/12", "text-right" ]
        [
          editIcon renameIsOpen open
        ,
          B.wad_ [ "d-inline-block", "w-3" ]
        ,
          B.iconButton
          { callback: const $ p.closeCallback unit
          , title: "Close"
          , name: "times"
          , elevation: Level1
          }
        ]
      ]
    ]

  editIcon _      true  = mempty
  editIcon isOpen false =
    B.iconButton
    { name: "pencil"
    , title: "Rename"
    , callback: const $ T.write_ true isOpen
    , elevation: Level1
    }

  panelBody :: T.Box (Maybe NodeAction) -> Record NodePopupProps -> R.Element
  panelBody nodePopupState { nodeType } =
    let (SettingsBox { doc, buttons }) = settingsBox nodeType in
    H.div {className: "popup-container__body card-body flex-space-between"}
    $ [ B.wad_ [ "m-1" ]
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
                , nodeType
                , session
                }
  mPanelAction { action: Nothing } _ =
    H.div
    { className: "popup-container__footer card-footer" }
    [
      H.h6
      {}
      [
        B.icon
        { name: "hand-pointer-o"
        , className: "mr-1"
        }
      ,
        H.text "Select available actions of this node"
      ]
    ,
      H.ul
      { className: "panel-actions" }
      [
        H.div
        { className: "panel-actions__ok-to-use" }
        [
          B.icon
          { name: "circle" }
        ,
          B.span_ "usable"
        ]
      ,
        H.div
        { className: "panel-actions__almost-useable" }
        [
          B.icon
          { name: "circle" }
        ,
          B.span_ "almost useable"
        ]
      ,
        H.div
        { className: "panel-actions__development-in-progress" }
        [
          B.icon
          { name: "circle" }
        ,
          B.span_ "development in progress"
        ]
      ]
    ]


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
  , nodeType  :: GT.NodeType
  , session   :: Session
  )

panelAction :: R2.Leaf PanelActionProps
panelAction = R2.leafComponent panelActionCpt
panelActionCpt :: R.Component PanelActionProps
panelActionCpt = here.component "panelAction" cpt
  where
    cpt { action: Documentation nodeType}                  _ = pure $ actionDoc { nodeType } []
    cpt { action: Download, id, nodeType, session}         _ = pure $ actionDownload { id, nodeType, session } []
    cpt { action: Upload, dispatch, id, nodeType, session} _ = pure $ actionUpload { dispatch, id, nodeType, session } []
    cpt { action: Delete, nodeType, dispatch}              _ = pure $ actionDelete { dispatch, nodeType } []
    cpt { action: ManageTeam, nodeType, id, session}       _ = pure $ actionManageTeam { id, nodeType, session } []
    cpt { action: Add xs, dispatch, id, name, nodeType} _ =
      pure $ addNodeView {dispatch, id, name, nodeType, nodeTypes: xs} []
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
    cpt { action: SearchBox, boxes, dispatch, id, session } _ =
      pure $ actionSearch { boxes, dispatch, id: Just id, session } []
    cpt { action: WriteNodesDocuments, boxes, dispatch, id, session } _ =
      pure $ actionWriteNodesDocuments { boxes, dispatch, id, session } []
    cpt _ _ = pure $ H.div {} []
