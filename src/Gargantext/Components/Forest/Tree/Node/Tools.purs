module Gargantext.Components.Forest.Tree.Node.Tools where

import Data.String as S
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Uncurried (mkEffectFn1)
import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Types (ID)
import Gargantext.Utils.Reactix as R2
import Gargantext.Prelude (Unit, bind, const, discard, pure, show, ($), (<<<), (<>))
import Reactix as R
import Reactix.DOM.HTML as H

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


submitButton :: Action -> (Action -> Aff Unit) -> R.Element
submitButton action dispatch =
  H.div {className: "panel-footer"}
            [ H.div {} []
            , H.div { className: "center"}
                    [ H.button { className : "btn btn-primary fa fa-" <> icon action
                               , type: "button"
                               , style : { width: "50%" }
                               , id: S.toLower $ show action
                               , title: show action
                               , on: {click: \_ -> launchAff $ dispatch action}
                               }
                               [ H.text $ " " <> text action]
                     ]
            ]

-- | Sugar Text style
fragmentPT :: String -> R.Element
fragmentPT text = H.div {style: {margin: "10px"}} [H.text text]


