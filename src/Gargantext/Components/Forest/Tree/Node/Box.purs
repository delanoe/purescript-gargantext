module Gargantext.Components.Forest.Tree.Node.Box where

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (NodePopup(..), addNodeView)
import Gargantext.Components.Forest.Tree.Node.Action.Delete (actionDelete)
import Gargantext.Components.Forest.Tree.Node.Action.Documentation (actionDoc)
import Gargantext.Components.Forest.Tree.Node.Action.Download (actionDownload)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (renameAction)
import Gargantext.Components.Forest.Tree.Node.Action.Search (actionSearch)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField (defaultSearch)
import Gargantext.Components.Forest.Tree.Node.Action.Share   as Share
import Gargantext.Components.Forest.Tree.Node.Action.Contact as Contact
import Gargantext.Components.Forest.Tree.Node.Action.Update (update)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (actionUpload)
import Gargantext.Components.Forest.Tree.Node.Action.Move (moveNode)
import Gargantext.Components.Forest.Tree.Node.Action.Link (linkNode)
import Gargantext.Components.Forest.Tree.Node.Action.Merge (mergeNode)
import Gargantext.Components.Forest.Tree.Node.Box.Types (NodePopupProps, NodePopupS)
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction(..), SettingsBox(..), glyphiconNodeAction, settingsBox)
import Gargantext.Components.Forest.Tree.Node.Status (Status(..), hasStatus)
import Gargantext.Components.Forest.Tree.Node.Tools (textInputBox, fragmentPT)
import Gargantext.Sessions (Session)
import Gargantext.Types (Name, ID)
import Gargantext.Types as GT
import Gargantext.Utils (glyphicon, glyphiconActive)
import Gargantext.Utils.Reactix as R2


type CommonProps =
  ( dispatch :: Action -> Aff Unit
  , session :: Session
  )


-- | START Popup View

nodePopupView :: Record NodePopupProps -> R.Element
nodePopupView p = R.createElement nodePopupCpt p []

nodePopupCpt :: R.Component NodePopupProps
nodePopupCpt = R.hooksComponent "G.C.F.T.N.B.nodePopupView" cpt
  where
    cpt p _ = do
      isOpen    <- R.useState' false

      nodePopupState@(nodePopup /\ setNodePopup)
        <- R.useState' { action  : Nothing
                       , id      : p.id
                       , name    : p.name
                       , nodeType: p.nodeType
                       }

      search  <- R.useState'
               $ defaultSearch { node_id = Just p.id }

      R.useEffect' $ do
        log2 "[nodePopup] nodePopupState" $ fst nodePopupState

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
            , mPanelAction nodePopupState p
            ]
          ]
        ]
      where
        tooltipProps = { className : ""
                       , id        : "node-popup-tooltip"
                       , title     : "Node settings"
                       , data: { toggle   : "tooltip"
                               , placement: "right"
                               }
                         --, style: { top: y - 65.0, left: x + 10.0 }
                       }

        panelHeading isOpen@(open /\ _) {dispatch, id, name, nodeType} =
          H.div {className: "panel-heading"}
                [ R2.row
                        [ H.div {className: "col-md-8 flex-end"}
                                [ textInputBox { boxAction: renameAction
                                               , boxName: "Rename"
                                               , dispatch
                                               , id
                                               , text:name
                                               , isOpen
                                               }
                                ]

                        , H.div {className: "flex-end"}
                                [ if edit then editIcon isOpen else H.div {} []
                                , H.div {className: "col-md-1"}
                                        [ H.a { "type"   : "button"
                                              , className: glyphicon "window-close"
                                              , on       : { click: \e -> p.onPopoverClose
                                                                        $ R2.unsafeEventTarget e
                                                           }
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
                $ [ H.p { "style": {"margin":"10px"} } []
                  , H.div { className: "flex-center" }
                          [ buttonClick { action: doc
                                        , state: nodePopupState
                                        , nodeType
                                        }
                          ]
                  , H.div {className: "flex-center"}
                          $ map (\t -> buttonClick { action: t
                                                   , state : nodePopupState
                                                   , nodeType
                                                   }
                                ) buttons
                  ]
                -- FIXME trick to increase the size of the box
                <> if A.length buttons < 2
                        then [H.div {className: "col-md-4"} []]
                        else []
          where
            SettingsBox {edit, doc, buttons} = settingsBox nodeType

        mPanelAction :: R.State (Record NodePopupS)
                     -> Record NodePopupProps
                     -> R.Element
        mPanelAction ({action: Nothing    } /\ _) _     =
          H.div {className:"center fa-hand-pointer-o"}
            [ H.h4 {} [H.text " Select available actions of this node"]
            , H.ul {} [ H.h5 {style:{color:"black"} , className: "fa-thumbs-o-up"         } [H.text " Black: yes you can use it"    ]
                      , H.h5 {style:{color:"orange"}, className: "fa-exclamation-triangle"} [H.text " Orange: almost useable"       ]
                      , H.h5 {style:{color:"red"}   , className: "fa-rocket"              } [H.text " Red: development in progress" ]
                      ]
            ]
        mPanelAction ({action: Just action} /\ _) props =
            panelAction { action
                        , dispatch : props.dispatch
                        , id       : props.id
                        , name     : props.name
                        , nodePopup: Just NodePopup
                        , nodeType : props.nodeType
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
  , nodeType :: GT.NodeType
  )

buttonClick :: Record ButtonClickProps -> R.Element
buttonClick p = R.createElement buttonClickCpt p []

buttonClickCpt :: R.Component ButtonClickProps
buttonClickCpt = R.hooksComponent "G.C.F.T.N.B.buttonClick" cpt
  where
    cpt {action: todo, state: (node@{action} /\ setNodePopup), nodeType} _ = do
      pure $ H.div {className: "col-md-1"}
                   [ H.a { style: (iconAStyle nodeType todo)
                         , className: glyphiconActive (glyphiconNodeAction todo)
                                                      (action == (Just todo)   )
                         , id: show todo
                         , title: show todo
                         , on: { click: \_ -> doToDo }
                       }
                     []
                   ]
      where
        -- | Open the help indications if selected already
        doToDo = setNodePopup $ const $ node { action = todo' }
          where
            todo' = case action == Just todo of
              true  -> Nothing
              false -> Just todo 

        iconAStyle :: GT.NodeType -> NodeAction -> {
                        color      :: String
                      , paddingTop :: String
                      , paddingBottom :: String
                      }
        iconAStyle n a = { color         : hasColor (hasStatus n a)
                          , paddingTop    : "6px"
                          , paddingBottom : "6px"
                          }
          where
            hasColor :: Status -> String
            hasColor Stable = "black"
            hasColor Test   = "orange"
            hasColor Dev    = "red"


-- END Popup View
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

    cpt {action: Add xs, dispatch, id, name, nodeType} _ = do
      pure $ addNodeView {dispatch, id, name, nodeType, nodeTypes: xs}

    cpt {action: Refresh , dispatch, id, nodeType, session} _ = update nodeType dispatch

    cpt {action: Config , dispatch, id, nodeType, session} _ = do
      pure $ fragmentPT $ "Config " <> show nodeType

-----------
    -- Functions using SubTree
    cpt {action: Merge {subTreeParams}, dispatch, id, nodeType, session} _ = do
      pure $ mergeNode {dispatch, id, nodeType, session, subTreeParams}

    cpt {action: Move {subTreeParams}, dispatch, id, nodeType, session} _ = do
      pure $ moveNode {dispatch, id, nodeType, session, subTreeParams}

    cpt {action: Link {subTreeParams}, dispatch, id, nodeType, session} _ = do
      pure $ linkNode {dispatch, id, nodeType, session, subTreeParams}
-----------

    cpt {action : Share, dispatch, id, name } _ = do
      isOpen <- R.useState' true
      pure $ H.div {} [ textInputBox { boxAction: Share.shareAction
                                     , boxName: "Share"
                                     , dispatch
                                     , id
                                     , text: "username"
                                     , isOpen
                                     }
                      ]

    cpt {action : AddingContact, dispatch, id, name } _ = do
      isOpen <- R.useState' true
      pure $ Contact.textInputBox { id
                           , dispatch
                           , isOpen
                           , boxName:"addContact"
                           , params : {firstname:"First Name", lastname: "Last Name"}
                           , boxAction: \p -> AddContact p
                           }



    cpt {action : Publish {subTreeParams}, dispatch, id, nodeType, session } _ = do
      pure $ Share.shareNode {dispatch, id, nodeType, session, subTreeParams}


    cpt props@{action: SearchBox, id, session, dispatch, nodePopup} _ =
      actionSearch session (Just id) dispatch nodePopup

    cpt _ _ = do
      pure $ H.div {} []


