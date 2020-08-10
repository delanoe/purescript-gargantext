module Gargantext.Components.Forest.Tree.Node.Tools 
  where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Prelude (Unit, bind, const, discard, pure, show, ($), (<<<), (<>), read, map, class Read, class Show, not, class Ord)
import Gargantext.Types (ID, Name)
import Gargantext.Utils (toggleSet)
import Gargantext.Utils.Reactix as R2

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
                  [ H.div { className: "col-md-12" } bs
                          -- TODO add type for text or form here
                          -- [ H.form {className: "form-horizontal"} bs ]
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
        textInput (newName /\ setNewName) =
          H.div {className: "col-md-8"}
          [
            inputWithEnter {
                 onEnter: submit newName
               , onValueChanged: setNewName <<< const
               , autoFocus: false
               , className: "form-control"
               , defaultValue: text
               , placeholder: (boxName <> " Node")
               , type: "text"
               }
          -- [ H.input { type: "text"
          --           , placeholder: (boxName <> " Node")
          --           , defaultValue: text
          --           , className: "form-control"
          --           , on: { input: setRenameNodeName
          --                      <<< const
          --                      <<< R2.unsafeEventValue }
          --           }
          ]
        submitBtn (newName /\ _) =
          H.a {className: "btn glyphitem glyphicon glyphicon-ok col-md-2 pull-left"
              , type: "button"
              , on: { click: submit newName }
              , title: "Submit"
              } []
        cancelBtn =
          H.a {className: "btn text-danger glyphitem glyphicon glyphicon-remove col-md-2 pull-left"
              , type: "button"
              , on: { click: \_ -> setIsOpen $ const false }
              , title: "Cancel"
              } []
        submit newName _ = do
          setIsOpen $ const false
          launchAff_ $ dispatch ( boxAction newName )
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
               , defaultValue: defaultValue
               , className   : "form-control"
               , on: { input: setter
                                <<< const
                                <<< R2.unsafeEventValue }
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
                    , on: { change: \_ -> setNodeType
                                      <<< const
                                      <<< fromMaybe defaultNodeType
                                      <<< read
                                      <<< R2.unsafeEventValue }
                    }
          (map (\opt -> H.option {} [ H.text $ show opt ]) nodeTypes)
         ]

-- | Button Form
-- FIXME: currently needs a click from the user (by default, we could avoid such click)
formButton :: forall a b c
           . Show a
           =>   a
           -> ((b -> a) -> Effect c)
           -> R.Element
formButton nodeType setNodeType =
  H.div {} [ H.text $ "Confirm the selection of: " <> show nodeType
           , bouton
           ]
    where
      bouton = H.button { className : "cold-md-5 btn btn-primary center"
                        , type : "button"
                        , title: "Form Button"
                        , style : { width: "100%" }
                        , on: { click: \_ -> setNodeType ( const nodeType ) }
                        } [H.text $ "Confirmation"]

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

------------------------------------------------------------------------
-- | CheckBox tools
-- checkboxes: Array of poolean values (basic: without pending option)
-- checkbox  : One boolean value only

checkbox :: R.State Boolean -> R.Element
checkbox ( val /\ set ) =
  H.input { id: "checkbox-id"
          , type: "checkbox"
          , value: val
          , className : "checkbox"
          , on: { click: \_ -> set $ const $ not val}
          }

data CheckBoxes = Multiple | Uniq

checkboxes :: forall a
           .  Ord   a
           => Show  a
           => Array a
           -> R.State (Set a)
           -> R.Element
checkboxes xs (val /\ set) =
  H.fieldset {} $ map (\a -> H.div {} [ H.input { type: "checkbox"
                                           , checked: Set.member a val
                                           , on: { click: \_ -> set
                                                             $ const
                                                             $ toggleSet a val
                                                 }
                                           }
                                 , H.div {} [H.text $ show a]
                                 ]
                  ) xs


-- START node text
type NodeTextProps =
  ( isSelected :: Boolean
  , name       :: Name
  )

nodeText :: Record NodeTextProps -> R.Element
nodeText p = R.createElement nodeTextCpt p []

nodeTextCpt :: R.Component NodeTextProps
nodeTextCpt = R.hooksComponent "G.C.F.T.N.B.nodeText" cpt
  where
    cpt { isSelected: true, name } _ = do
      pure $ H.u {} [
        H.b {} [
           H.text ("| " <> name <> " |    ")
         ]
        ]
    cpt {isSelected: false, name} _ = do
      pure $ H.text (name <> "    ")
-- END node text

------------------------------------------------------------------------

