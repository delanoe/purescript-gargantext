module Gargantext.Components.Forest.Tree.Node.Action.Contact where

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff (Aff)
import Effect.Uncurried (mkEffectFn1)
import Gargantext.Components.Forest.Tree.Node.Action.Contact.Types (AddContactParams(..))
import Gargantext.Components.Forest.Tree.Node.Action (Action)
import Gargantext.Components.Forest.Tree.Node.Action (Action)
import Gargantext.Components.Forest.Tree.Node.Action as Action
import Gargantext.Components.Forest.Tree.Node.Action as Action
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Components.Forest.Tree.Node.Tools as Tools
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Effect.Aff (Aff, launchAff)
import Gargantext.Utils.Reactix as R2
import Gargantext.Prelude
import Gargantext.Prelude (class Eq, class Read, class Show)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Sessions (Session, put_)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Types as GT
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson, genericEnumDecodeJson, genericEnumEncodeJson)
import Prelude (($))
import Reactix as R
import Reactix as R
import Reactix.DOM.HTML as H

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
    el = R.hooksComponent (boxName <> "Box") cpt
    cpt {id, params} _ = do
      let {firstname, lastname} = params
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
          [ H.input { type: "text"
                    , placeholder: (boxName <> " Node")
                    , defaultValue: default
                    , className: "form-control"
                    , onInput: mkEffectFn1 $ set
                                         <<< const
                                         <<< R2.unsafeEventValue
                    }
          ]
        submitBtn (val1 /\ _) (val2 /\ _) =
          H.a {className: "btn glyphitem glyphicon glyphicon-ok col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> do
                    setIsOpen $ const false
                    launchAff $ dispatch ( boxAction (AddContactParams {firstname:val1, lastname:val2} ))
              , title: "Submit"
              } []
        cancelBtn =
          H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> setIsOpen $ const false
              , title: "Cancel"
              } []
textInputBox p@{ boxName, isOpen: (false /\ _) } = R.createElement el p []
  where
    el = R.hooksComponent (boxName <> "Box") cpt
    cpt {} _ = pure $ H.div {} []



