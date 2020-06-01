module Gargantext.Components.Forest.Tree.Node.Action.Rename where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Uncurried (mkEffectFn1)
import Prelude (Unit, bind, const, discard, pure, ($), (<<<))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Types as GT
import Gargantext.Types (NodeType, ID, Name)
import Gargantext.Routes as GR
import Gargantext.Utils.Reactix as R2
import Gargantext.Sessions (Session, get, put, post, delete)


renameNode :: Session -> ID -> RenameValue -> Aff (Array ID)
renameNode session renameNodeId =
  put session $ GR.NodeAPI GT.Node (Just renameNodeId) "rename"

newtype RenameValue = RenameValue
  { name :: Name }

instance encodeJsonRenameValue :: EncodeJson RenameValue where
  encodeJson (RenameValue {name})
     = "r_name" := name
    ~> jsonEmptyObject


-- | START Rename Box
type RenameBoxProps =
  ( id            :: ID
  , dispatch      :: Action -> Aff Unit
  , name          :: Name
  , nodeType      :: NodeType
  , renameBoxOpen :: R.State Boolean
  )

renameBox :: Record RenameBoxProps -> R.Element
renameBox p@{ dispatch, renameBoxOpen: (true /\ setRenameBoxOpen) } = R.createElement el p []
  where
    el = R.hooksComponent "RenameBox" cpt
    cpt {id, name, nodeType} _ = do
      renameNodeName <- R.useState' name
      pure $ H.div {className: "from-group row-no-padding"}
        [ renameInput renameNodeName
        , renameBtn renameNodeName
        , cancelBtn
        ]
      where
        renameInput (_ /\ setRenameNodeName) =
          H.div {className: "col-md-8"}
          [ H.input { type: "text"
                    , placeholder: "Rename Node"
                    , defaultValue: name
                    , className: "form-control"
                    , onInput: mkEffectFn1 $ setRenameNodeName <<< const <<< R2.unsafeEventValue
                    }
          ]
        renameBtn (newName /\ _) =
          H.a {className: "btn glyphitem glyphicon glyphicon-ok col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> do
                    setRenameBoxOpen $ const false
                    launchAff $ dispatch $ Submit newName
              , title: "Rename"
              } []
        cancelBtn =
          H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> setRenameBoxOpen $ const false
              , title: "Cancel"
              } []
renameBox p@{ renameBoxOpen: (false /\ _) } = R.createElement el p []
  where
    el = R.hooksComponent "RenameBox" cpt
    cpt {name} _ = pure $ H.div {} []

-- END Rename Box

