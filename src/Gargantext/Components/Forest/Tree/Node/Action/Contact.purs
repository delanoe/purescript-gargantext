module Gargantext.Components.Forest.Tree.Node.Action.Contact where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff)
import Formula as F
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Contact.Types (AddContactParams(..))
import Gargantext.Config.REST (RESTError)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Contact"

contactReq :: Session -> ID -> AddContactParams -> Aff (Either RESTError ID)
contactReq session nodeId =
  post session $ GR.NodeAPI GT.Annuaire (Just nodeId) "contact"

type ActionAddContact =
  ( dispatch :: Action -> Aff Unit
  , id :: ID )

actionAddContact :: R2.Component ActionAddContact
actionAddContact = R.createElement actionAddContactCpt
actionAddContactCpt :: R.Component ActionAddContact
actionAddContactCpt = here.component "actionAddContact" cpt where
  cpt { dispatch, id } _ = do
    isOpen <- T.useBox true
    pure $ textInputBox
      { boxAction: \p -> AddContact p
      , boxName:"addContact"
      , dispatch
      , id
      , isOpen
      , params: {firstname:"First Name", lastname: "Last Name"} }

type TextInputBoxProps =
  ( boxAction :: AddContactParams -> Action
  , boxName   :: String
  , dispatch  :: Action -> Aff Unit
  , id        :: ID
  , isOpen    :: T.Box Boolean
  , params    :: Record AddContactProps )

type AddContactProps = ( firstname :: String, lastname :: String )

textInputBox :: R2.Leaf TextInputBoxProps
textInputBox props = R.createElement textInputBoxCpt props []
textInputBoxCpt :: R.Component TextInputBoxProps
textInputBoxCpt = here.component "textInputBox" cpt where
  cpt { boxName, boxAction, dispatch, isOpen
      , params: { firstname, lastname } } _ =
    content <$> T.useLive T.unequal isOpen
            <*> T.useBox firstname <*> T.useBox lastname
    where
      content false _ _ = H.div {} []
      content true firstName lastName =
        H.div { className: "from-group row" }
        [ textInput firstName
        , textInput lastName
        , submitBtn firstName lastName
        , cancelBtn
        ] where
          textInput value =
            H.div {className: "col-md-8"}
            [ F.bindInput
              { value, className: "form-control", type: "text"
              , placeholder: (boxName <> " Node") } ]
          submitBtn first last =
            H.a
            { className: "btn glyphitem fa fa-ok col-md-2 pull-left"
            , type: "button", on: { click }, title:"Submit"
            } [] where
              click _ = do
                f <- T.read first
                l  <- T.read last
                T.write_ false isOpen
                launchAff $
                  dispatch (boxAction $ AddContactParams { firstname: f, lastname: l })
          cancelBtn =
            H.a
            { className: "btn text-danger glyphitem fa fa-remove col-md-2 pull-left"
            , on: { click }, title: "Cancel", type: "button"
            } [] where
              click _ = T.write_ false isOpen
