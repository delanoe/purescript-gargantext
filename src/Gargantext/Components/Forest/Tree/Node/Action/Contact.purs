module Gargantext.Components.Forest.Tree.Node.Action.Contact where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff_)
import Formula as F
import Gargantext.Components.Forest.Tree.Node.Action.Contact.Types (AddContactParams(..))
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools as Tools
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Contact"

contactReq :: Session -> ID -> AddContactParams -> AffRESTError ID
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
    pure $
      Tools.panelNoFooter { mError: Nothing }
        [ textInputBox
          { boxAction: AddContact
          , dispatch
          , id
          , params: { firstname: "", lastname: "" } }
        ]

type TextInputBoxProps =
  ( boxAction :: AddContactParams -> Action
  , dispatch  :: Action -> Aff Unit
  , id        :: ID
  , params    :: Record AddContactProps )

type AddContactProps = ( firstname :: String, lastname :: String )

textInputBox :: R2.Leaf TextInputBoxProps
textInputBox = R2.leaf textInputBoxCpt
textInputBoxCpt :: R.Component TextInputBoxProps
textInputBoxCpt = here.component "textInputBox" cpt where
  cpt { boxAction
      , dispatch
      , params: { firstname, lastname } } _ = do
    
    firstName <- T.useBox firstname
    lastName <- T.useBox lastname

    let submitF unit = do
          f <- T.read firstName
          l <- T.read lastName
          launchAff_ $
            dispatch (boxAction $ AddContactParams { firstname: f, lastname: l })

    pure $ H.div { className: "from-group" }
      [ textInput "First name" firstName firstname submitF true
      , textInput "Last name" lastName lastname submitF false
      , R2.row [
          submitBtn submitF
          ]
      ]
    where
      textInput placeholder value defaultValue submitF autoFocus =
        R2.row
          [ R2.col 8
            [ inputWithEnter { onBlur: \s -> T.write_ s value
                             , onEnter: submitF
                             , onValueChanged: \s -> T.write_ s value
                             , autoFocus
                             , className: "form-control"
                             , defaultValue
                             , placeholder
                             , type: "value"
                             , required: true }
            ]
            -- [ F.bindInput
            --   { value
            --   , className: "form-control"
            --   , type: "text"
            --   , placeholder: boxName <> " Node" }
            -- ]
          ]
      submitBtn submitF =
        H.a { className: "btn glyphitem fa fa-send col-md-2 pull-left"
            , type: "button"
            , on: { click: \_ -> submitF unit }
            , title:"Submit"
            } []
