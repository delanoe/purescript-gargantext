module Gargantext.Components.Forest.Tree.Node.Action.Rename where

import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Types (NodeType)
import Prelude (Unit, bind, const, discard, pure, ($))
import Reactix as R
import Reactix.DOM.HTML as H


-- | START Rename Box
type RenameBoxProps =
  ( id :: ID
  , name :: Name
  , nodeType :: NodeType)

renameBox :: (Action -> Aff Unit) -> Record RenameBoxProps -> R.State Boolean -> R.Element
renameBox d p (true /\ setRenameBoxOpen) = R.createElement el p []
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
                    , onInput: mkEffectFn1 $ \e -> setRenameNodeName $ const $ e .. "target" .. "value"
                    }
          ]
        renameBtn (newName /\ _) =
          H.a {className: "btn glyphitem glyphicon glyphicon-ok col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> do
                    setRenameBoxOpen $ const false
                    launchAff $ d $ Submit newName
              , title: "Rename"
              } []
        cancelBtn =
          H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove col-md-2 pull-left"
              , type: "button"
              , onClick: mkEffectFn1 $ \_ -> setRenameBoxOpen $ const false
              , title: "Cancel"
              } []
renameBox _ p (false /\ _) = R.createElement el p []
  where
    el = R.hooksComponent "RenameBox" cpt
    cpt {name} _ = pure $ H.div {} [ H.text name ]

-- END Rename Box



