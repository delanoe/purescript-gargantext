module Gargantext.Components.Forest.Tree.Node.Tools where

import Gargantext.Prelude

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Elevation(..), Variant(..))
import Gargantext.Components.Forest.Tree.Node.Action (icon, text)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action)
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Types as GT
import Gargantext.Utils (toggleSet)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Tools"

fragmentPT :: String -> R.Element
fragmentPT text = H.div { style: { margin: "10px" }} [ H.text text ]

type Body    = Array R.Element

type Footer  = R.Element

panel :: Body -> Footer -> R.Element
panel bodies submit =
  R.fragment
  [ H.div { className: "card-body" }
    [ H.div { className: "row" }
        -- TODO add type for text or form here [ H.form {className: "form-horizontal"} bodies ]
      [ H.div { className: "col-12" } bodies ]]
  , H.div {className: "card-footer"}
    [ H.div { className: "row" }
      [ H.div { className: "mx-auto"} [ submit ] ]]]

type TextInputBoxProps =
  ( id       :: GT.ID
  , dispatch :: Action -> Aff Unit
  , text     :: String
  , isOpen    :: T.Box Boolean
  , boxName  :: String
  , boxAction :: String -> Action
  )

textInputBox :: R2.Component TextInputBoxProps
textInputBox = R.createElement textInputBoxCpt
textInputBoxCpt :: R.Component TextInputBoxProps
textInputBoxCpt = here.component "textInputBox" cpt where
  cpt { boxAction, boxName, dispatch, isOpen, text } _ =
    content <$> T.useLive T.unequal isOpen <*> R.useRef text
    where
      content false _ = (R.fragment [])
      content true renameNodeNameRef =
        H.div
        { className: "d-flex align-items-center" }
        [
          textInput renameNodeNameRef
        ,
          B.wad_ [ "d-inline-block", "w-3" ]
        ,
          submitBtn renameNodeNameRef
        ,
          B.wad_ [ "d-inline-block", "w-3" ]
        ,
          cancelBtn
        ]

      textInput renameNodeNameRef =
        H.div
        {}
        [
          inputWithEnter
          { autoFocus: true
          , className: "form-control"
          , defaultValue: text
          , onBlur: R.setRef renameNodeNameRef
          , onEnter: submit renameNodeNameRef
          , onValueChanged: R.setRef renameNodeNameRef
          , placeholder: (boxName <> " Node")
          , type: "text"
          }
        ]

      submitBtn renameNodeNameRef =
        B.iconButton
        { callback: submit renameNodeNameRef
        , title: "Submit"
        , name: "floppy-o"
        , elevation: Level1
        }

      cancelBtn =
        B.iconButton
        { callback: const $ T.write_ false isOpen
        , variant: Danger
        , title: "Cancel"
        , name: "times"
        }

      submit ref _ = do
        launchAff_ $ dispatch (boxAction $ R.readRef ref)
        T.write_ false isOpen

type DefaultText = String

formEdit :: forall prev next
          . DefaultText -> ((prev -> String) -> Effect next) -> R.Element
formEdit defaultValue setter =
  H.div { className: "form-group" }
  [ H.input { defaultValue, type: "text", on: { input }
            , placeholder: defaultValue, className: "form-control" }
  ] where input = setter <<< const <<< R.unsafeEventValue

type FormChoiceSafeProps item m =
  ( items    :: Array item
  , default  :: item
  , callback :: item -> Effect m
  , print    :: item -> String )

-- | Form Choice input
-- if the list of options is not big enough, a button is used instead
formChoiceSafe :: forall item m
  .  Read item
  => Show item
  => R2.Component (FormChoiceSafeProps item m)
formChoiceSafe = R.createElement formChoiceSafeCpt
formChoiceSafeCpt :: forall item m. Read item => Show item => R.Component (FormChoiceSafeProps item m)
formChoiceSafeCpt = here.component "formChoiceSafe" cpt where
  cpt { items, default, callback, print } _ = do
    pure $ case items of
      []  -> H.div {} []
      [n] -> formButton { item: n, callback, print } []
      _   -> formChoice { items, default, callback, print } []

type FormChoiceProps item m =
  ( items    :: Array item
  , default  :: item
  , callback :: item -> Effect m
  , print    :: item -> String )

-- | List Form
formChoice :: forall item m
  .  Read item
  => Show item
  => R2.Component (FormChoiceProps item m)
formChoice = R.createElement formChoiceCpt
formChoiceCpt :: forall item m. Read item => Show item => R.Component (FormChoiceProps item m)
formChoiceCpt = here.component "formChoice" cpt where
  cpt { items, callback, default, print } _ = do
    pure $ H.div { className: "form-group"}
      [
        R2.select
        { className: "form-control with-icon-font"
        , defaultValue: show default
        , on: { change }
        } $ map option items
      ]

    where
      change e = callback $ fromMaybe default $ read $ R.unsafeEventValue e

      option opt = H.option { value: show opt } [ H.text $ print opt ]

type FormButtonProps item m =
  ( item     :: item
  , callback :: item -> Effect m
  , print    :: item -> String )

-- | Button Form
-- FIXME: currently needs a click from the user (by default, we could avoid such click)
formButton :: forall item m. R2.Component (FormButtonProps item m)
formButton = R.createElement formButtonCpt
formButtonCpt :: forall item m. R.Component (FormButtonProps item m)
formButtonCpt = here.component "formButton" cpt where
  cpt { item, callback, print} _ = do
    pure $ H.div {}
      [ H.text $ "Confirm the selection of: " <> print item
      , cta ]

    where
      cta =
        H.button { className : "cold-md-5 btn btn-primary center"
                 , type : "button"
                 , title: "Form Button"
                 , style : { width: "100%" }
                 , on: { click: \_ -> callback item }
                 }
        [ H.text "Confirmation" ]

------------------------------------------------------------------------
------------------------------------------------------------------------

submitButton :: Action -> (Action -> Aff Unit) -> R.Element
submitButton action dispatch =
  H.button { className : "btn btn-primary fa fa-" <> icon action
           , type: "button"
           , id: S.toLower $ show action
           , title: show action
           , on: {click: \_ -> launchAff $ dispatch action}
           }
           [ H.text $ " " <> text action]

type Href  = String

submitButtonHref :: Action -> Href -> R.Element
submitButtonHref action href =
  H.a { className, href, target: "_blank" } [ H.text $ " " <> text action ]
  where
    className = "btn btn-primary fa fa-" <> icon action

------------------------------------------------------------------------
-- | CheckBox tools
-- checkboxes: Array of boolean values (basic: without pending option)
-- checkbox  : One boolean value only

type CheckboxProps =
  ( value :: T.Box Boolean )

checkbox :: R2.Leaf CheckboxProps
checkbox = R2.leafComponent checkboxCpt
checkboxCpt :: R.Component CheckboxProps
checkboxCpt = here.component "checkbox" cpt
  where
    cpt { value } _ = do
      value' <- T.useLive T.unequal value

      pure $ H.input { className: "form-check-input"
                     , on: { click }
                     , type: "checkbox"
                     , value: value' }
      where
        click _ = T.modify_ not value

data CheckBoxes = Multiple | Uniq

type CheckboxesListGroup a =
  ( groups :: Array a
  , options :: T.Box (Set a) )

checkboxesListGroup :: forall a. Ord a => Show a => R2.Component (CheckboxesListGroup a)
checkboxesListGroup = R.createElement checkboxesListGroupCpt
checkboxesListGroupCpt :: forall a. Ord a => Show a => R.Component (CheckboxesListGroup a)
checkboxesListGroupCpt = here.component "checkboxesListGroup" cpt
  where
    cpt { options } _ = do
      options' <- T.useLive T.unequal options

      let one a =
            H.li { className: "list-group-item" }
              [ H.div { className: "form-check" }
                [ H.input { defaultChecked: Set.member a options'
                          , on: { click: \_ -> T.write_ (toggleSet a options') options
                                , type: "checkbox" }
                          , className: "form-check-input" }
                , H.label { className: "form-check-label" } [ H.text (show a) ] ]
              ]

      pure $ R.fragment $ map one $ Set.toUnfoldable options'

prettyNodeType :: GT.NodeType -> String
prettyNodeType
  =   S.replace (S.Pattern "Node")   (S.Replacement " ")
  <<< S.replace (S.Pattern "Folder") (S.Replacement " ")
  <<< show
