module Gargantext.Components.Forest.Tree.Node.Action.Contact where

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Uncurried (mkEffectFn1)
import Prelude (($))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Forest.Tree.Node.Action (Action)
import Gargantext.Components.Forest.Tree.Node.Action.Contact.Types (AddContactParams(..))
-- import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Prelude (Unit, bind, const, discard, pure, (<<<), (<>))
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Forest.Tree.Node.Action.Contact"

------------------------------------------------------------------------
contactReq :: Session -> ID -> AddContactParams -> Aff ID
contactReq session nodeId =
  post session $ GR.NodeAPI GT.Annuaire (Just nodeId) "contact"

------------------------------------------------------------------------
type TextInputBoxProps =
  ( id        :: ID
  , dispatch  :: Action -> Aff Unit
  , params    :: Record AddContactProps
  , isOpen    :: R.State Boolean
  , boxName   :: String
  , boxAction :: AddContactParams -> Action
  )

type AddContactProps = ( firstname :: String, lastname :: String)

textInputBox :: Record TextInputBoxProps -> R.Element
textInputBox p@{ boxName, boxAction, dispatch, isOpen: (true /\ setIsOpen), params } = R.createElement el p []
  where
    {firstname, lastname} = params
    el = R.hooksComponentWithModule thisModule (boxName <> "Box") cpt
    cpt {id, params:params'} _ = do
      let {firstname, lastname} = params'
      stateFirstname <- R.useState' firstname
      stateLastname  <- R.useState'  lastname
      pure $ H.div {className: "from-group row-no-padding"}
        [ textInput stateFirstname firstname
        , textInput stateLastname  lastname
        , submitBtn stateFirstname stateLastname
        , cancelBtn
        ]
      where
        textInput (_ /\ set) default =
          H.div {className: "col-md-8"}
          [ H.input { className: "form-control"
                    , defaultValue: default
                    , on: { input: set
                                   <<< const
                                   <<< R.unsafeEventValue }
                    , placeholder: (boxName <> " Node")
                    , type: "text"
                    }
          ]
        submitBtn (val1 /\ _) (val2 /\ _) =
          H.a {className: "btn glyphitem glyphicon glyphicon-ok col-md-2 pull-left"
              , type: "button"
              , on: { click: \_ -> do
                    setIsOpen $ const false
                    launchAff $ dispatch ( boxAction (AddContactParams {firstname:val1, lastname:val2} ))
                    }
              , title: "Submit"
              } []
        cancelBtn =
          H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove col-md-2 pull-left"
              , on: { click: \_ -> setIsOpen $ const false }
              , title: "Cancel"
              , type: "button"
              } []
textInputBox p@{ boxName, isOpen: (false /\ _) } = R.createElement el p []
  where
    el = R.hooksComponentWithModule thisModule (boxName <> "Box") cpt
    cpt {} _ = pure $ H.div {} []



