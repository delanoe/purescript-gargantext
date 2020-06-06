module Gargantext.Components.Forest.Tree.Node.Action.Rename where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Uncurried (mkEffectFn1)
import Prelude (Unit, bind, const, discard, pure, ($), (<<<), (<>))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Types as GT
import Gargantext.Types (ID)
import Gargantext.Routes as GR
import Gargantext.Utils.Reactix as R2
import Gargantext.Sessions (Session, get, put, post, delete)


------------------------------------------------------------------------
rename :: Session -> ID -> RenameValue -> Aff (Array ID)
rename session renameNodeId =
  put session $ GR.NodeAPI GT.Node (Just renameNodeId) "rename"

renameAction :: String -> Action
renameAction newName = RenameNode newName

------------------------------------------------------------------------
newtype RenameValue = RenameValue
  { text :: String }

instance encodeJsonRenameValue :: EncodeJson RenameValue where
  encodeJson (RenameValue {text})
     = "r_name" := text
    ~> jsonEmptyObject

------------------------------------------------------------------------
-- | START Rename Box
type TextInputBoxProps =
  ( id       :: ID
  , dispatch :: Action -> Aff Unit
  , text     :: String
  , isOpen   :: R.State Boolean
  , boxName  :: String
  , boxAction :: String -> Action
  )

textInputBox :: Record TextInputBoxProps -> R.Element
textInputBox p@{ boxName, boxAction, dispatch, isOpen: (true /\ setIsOpen) } = R.createElement el p []
  where
    el = R.hooksComponent (boxName <> "Box") cpt
    cpt {id, text} _ = do
      renameNodeName <- R.useState' text
      pure $ H.div {className: "from-group row-no-padding"}
        [ textInput renameNodeName
        , submitBtn   renameNodeName
        , cancelBtn
        ]
      where
        textInput (_ /\ setRenameNodeName) =
          H.div {className: "col-md-8"}
          [ H.input { type: "text"
                    , placeholder: (boxName <> " Node")
                    , defaultValue: text
                    , className: "form-control"
                    , onInput: mkEffectFn1 $ setRenameNodeName
                                         <<< const
                                         <<< R2.unsafeEventValue
                    }
          ]
        submitBtn (newName /\ _) =
          H.a {className: "btn glyphitem glyphicon glyphicon-ok col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> do
                    setIsOpen $ const false
                    launchAff $ dispatch ( boxAction newName )
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
    cpt {text} _ = pure $ H.div {} []

-- | END Rename Box

