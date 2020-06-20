module Gargantext.Components.Forest.Tree.Node.Tools 
  where

import Data.Maybe (fromMaybe)
import Data.String as S
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Uncurried (mkEffectFn1)
import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Prelude (Unit, bind, const, discard, pure, show, ($), (<<<), (<>), read, map, class Read, class Show)
import Gargantext.Types (ID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

------------------------------------------------------------------------

type Body    = Array R.Element
type Footer  = R.Element

panel :: Body -> Footer -> R.Element
panel bodies submit =
  H.div {} [ panelBody bodies, footer submit ]
    where
      panelBody bs =
          H.div {className: "panel-body"}
          [ H.div { className: "row"
                  , style: {"margin":"10px"}
                  }
                  [ H.div { className: "col-md-10" }
                          -- TODO add type for text or form here
                          [ H.form {className: "form-horizontal"} bs
                          ]
                  ]
            ]
      footer sb = 
        H.div {className: "panel-footer"}
            [ H.div {} []
            , H.div { className: "center"} [ sb ]
            ]


------------------------------------------------------------------------
-- | START Text input
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
        , submitBtn renameNodeName
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


-- | Sugar Text style
fragmentPT :: String -> R.Element
fragmentPT text = H.div {style: {margin: "10px"}} [H.text text]


-- | Form Edit input
type DefaultText = String

formEdit :: forall previous next
         .  DefaultText
         -> ((previous -> String)
         -> Effect next)
         -> R.Element
formEdit defaultValue setter = 
  H.div {className: "form-group"}
     [ H.input { type        : "text"
               , placeholder : defaultValue
               , defaultValue: "Write" <> defaultValue
               , className   : "form-control"
               , onInput     : mkEffectFn1
                             $ setter
                             <<< const
                             <<< R2.unsafeEventValue
              }
     ]

-- | Form Choice input
-- if the list of options is not big enough, a button is used instead
formChoiceSafe :: forall a b c
               .  Read  a
               => Show  a
               => Array a
               -> a
               -> ((b -> a) -> Effect c)
               -> R.Element
formChoiceSafe [] _ _ = H.div {} []

formChoiceSafe [n] _defaultNodeType setNodeType =
  formButton n setNodeType

formChoiceSafe nodeTypes defaultNodeType setNodeType =
  formChoice nodeTypes defaultNodeType setNodeType

-- | List Form
formChoice :: forall a b c d
           .  Read b
           => Show d
           => Array d
           -> b
           -> ((c -> b) -> Effect a)
           -> R.Element
formChoice nodeTypes defaultNodeType setNodeType = 
  H.div { className: "form-group"}
        [ R2.select { className: "form-control"
                    , onChange : mkEffectFn1
                               $ setNodeType
                               <<< const
                               <<< fromMaybe defaultNodeType
                               <<< read
                               <<< R2.unsafeEventValue
                    }
          (map (\opt -> H.option {} [ H.text $ show opt ]) nodeTypes)
         ]

-- | Button Form
-- FIXME: currently needs a click from the user (by default, we could avoid such click)
formButton :: forall a b c
           .    a
           -> ((b -> a) -> Effect c)
           -> R.Element
formButton nodeType setNodeType =
 H.button { className : "btn btn-primary center"
          , type : "button"
          , title: "Form Button"
          , style : { width: "50%" }
          , onClick : mkEffectFn1
                    $ \_ -> setNodeType ( const nodeType )
          } [H.text $ "Go !"]

------------------------------------------------------------------------
------------------------------------------------------------------------

submitButton :: Action -> (Action -> Aff Unit) -> R.Element
submitButton action dispatch =
  H.button { className : "btn btn-primary fa fa-" <> icon action
           , type: "button"
           , style : { width: "50%" }
           , id: S.toLower $ show action
           , title: show action
           , on: {click: \_ -> launchAff $ dispatch action}
           }
           [ H.text $ " " <> text action]

type Href  = String

submitButtonHref :: Action -> Href -> R.Element
submitButtonHref action href =
  H.a { className : "btn btn-primary fa fa-" <> icon action
      , style : { width: "50%" }
      , href
      , target: "_blank"
      }
      [ H.text $ " " <> text action]





