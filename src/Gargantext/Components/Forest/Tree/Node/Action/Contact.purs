module Gargantext.Components.Forest.Tree.Node.Action.Contact where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Uncurried (mkEffectFn1)
import Formula as F
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.Forest.Tree.Node.Action (Action)
import Gargantext.Components.Forest.Tree.Node.Action.Contact.Types (AddContactParams(..))
-- import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Prelude (Unit, bind, const, discard, pure, (<<<), (<>))
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Contact"

contactReq :: Session -> ID -> AddContactParams -> Aff ID
contactReq session nodeId =
  post session $ GR.NodeAPI GT.Annuaire (Just nodeId) "contact"

type TextInputBoxProps =
  ( id        :: ID
  , dispatch  :: Action -> Aff Unit
  , params    :: Record AddContactProps
  , isOpen    :: T.Cursor Boolean
  , boxName   :: String
  , boxAction :: AddContactParams -> Action
  )

type AddContactProps = ( firstname :: String, lastname :: String)

textInputBox :: R2.Leaf TextInputBoxProps
textInputBox props = R.createElement textInputBoxCpt props []

textInputBoxCpt :: R.Component TextInputBoxProps
textInputBoxCpt = here.component "textInputBox" cpt where
  cpt p@{ boxName, boxAction, dispatch, isOpen
        , params: { firstname, lastname } } _ =
    content <$> T.useLive T.unequal isOpen
            <*> T.useCell firstname <*> T.useCell lastname
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
                firstname <- T.read first
                lastname  <- T.read last
                T2.write_ false isOpen
                launchAff $
                  dispatch (boxAction $ AddContactParams { firstname, lastname })
          cancelBtn =
            H.a
            { className: "btn text-danger glyphitem fa fa-remove col-md-2 pull-left"
            , on: { click }, title: "Cancel", type: "button"
            } [] where
              click _ = T2.write_ false isOpen
