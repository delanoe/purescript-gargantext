module Gargantext.Components.Forest.Tree.Node.Tools where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.Tuple.Nested ((/\))
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
import Gargantext.Utils.Show as GUS
import Reactix as R
import Reactix.DOM.HTML as H
import Record.Extra as RX
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Tools"

fragmentPT :: String -> R.Element
fragmentPT text = H.div { style: { margin: "10px" }} [ H.text text ]


type PanelProps =
  ( mError :: Maybe String )

-- | Last element of panel's children goes to footer, all others go to body
panel :: R2.Component PanelProps
panel = R.createElement panelCpt
panelCpt :: R.Component PanelProps
panelCpt = here.component "panel" cpt where
  cpt { mError } children = do
    let errorCpt =
          R2.fromMaybe mError $
            \err ->
              R2.row
                [ R2.col 12
                  [ H.div { className: "alert alert-danger" } [ H.text err ]
                  ]
                ]
    
    pure $ R.fragment
      [ H.div { className: "card-body" }
          [ H.div { className: "card-text" }
            [ R2.row
              -- TODO add type for text or form here [ H.form {className: "form-horizontal"} bodies ]
              [ R2.col 12 bodies ]
            , errorCpt
            ]
          ]
      , H.div {className: "card-footer"}
        [ R2.row
          [ H.div { className: "mx-auto"} [ footer ] ]
        ]
      ]
    where
      bodies /\ footer =
        case A.unsnoc children of
          Nothing -> [] /\ (H.div {} [])
          Just { init, last } -> init /\ last

-- | A panel without a footer
panelNoFooter :: R2.Component PanelProps
panelNoFooter = R.createElement panelNoFooterCpt
panelNoFooterCpt :: R.Component PanelProps
panelNoFooterCpt = here.component "panelNoFooter" cpt where
  cpt props children =
    pure $ panel props (children <> [ H.div {} [] ])


type PanelWithSubmitButtonProps =
  ( action   :: Action
  , dispatch :: Action -> Aff Unit
  | PanelProps )

-- | A panel with 'submitButton { action, dispatch }'
panelWithSubmitButton :: R2.Component PanelWithSubmitButtonProps
panelWithSubmitButton = R.createElement panelWithSubmitButtonCpt
panelWithSubmitButtonCpt :: R.Component PanelWithSubmitButtonProps
panelWithSubmitButtonCpt = here.component "panelWithSubmitButton" cpt where
  cpt props@{ action, dispatch } children = do
    let pProps = (RX.pick props :: Record PanelProps)
    pure $ panel pProps (children
                         -- footer
                         <> [ submitButton { action, dispatch } ])

type PanelWithSubmitButtonHrefProps =
  ( action   :: Action
  , href     :: String
  | PanelProps )

-- | A panel with 'submitButtonHref { action, href }'
panelWithSubmitButtonHref :: R2.Component PanelWithSubmitButtonHrefProps
panelWithSubmitButtonHref = R.createElement panelWithSubmitButtonHrefCpt
panelWithSubmitButtonHrefCpt :: R.Component PanelWithSubmitButtonHrefProps
panelWithSubmitButtonHrefCpt = here.component "panelWithSubmitButtonHref" cpt where
  cpt props@{ action, href } children = do
    let pProps = (RX.pick props :: Record PanelProps)
    pure $ panel pProps (children
                         -- footer
                         <> [ submitButtonHref { action, href } ])


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
        { className: "d-flex align-items-center justify-content-space-between" }
        [
          textInput renameNodeNameRef
        ,
          submitBtn renameNodeNameRef
        ,
          cancelBtn
        ]

      textInput renameNodeNameRef =
        H.div
        { className: "w-10/12" }
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
          , required: false
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

type InviteInputBoxProps =
  ( id       :: GT.ID
  , dispatch :: Action -> Aff Unit
  , text     :: String
  , boxName  :: String
  , boxAction :: String -> Action
  , username :: T.Box String
  )

inviteInputBox :: R2.Component InviteInputBoxProps
inviteInputBox = R.createElement inviteInputBoxCpt
inviteInputBoxCpt :: R.Component InviteInputBoxProps
inviteInputBoxCpt = here.component "textInputBox" cpt where
  cpt { boxAction, boxName, dispatch, text, username } _ =
    content <$> R.useRef text
    where
      content renameNodeNameRef =
        H.div
        { className: "d-flex align-items-center" }
        [
          textInput renameNodeNameRef
        ,
          B.wad_ [ "d-inline-block", "w-3" ]
        ,
          submitBtn renameNodeNameRef
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
          , required: false
          }
        ]

      submitBtn renameNodeNameRef =
        B.iconButton
        { callback: submit renameNodeNameRef
        , title: "Submit"
        , name: "plus"
        , elevation: Level1
        }

      submit ref _ = do
        T.write_ ("Invited " <> R.readRef ref <> " to the team") username
        launchAff_ $ dispatch (boxAction $ R.readRef ref)

-- type DefaultText = String

-- formEdit :: forall prev next
--           . DefaultText -> ((prev -> String) -> Effect next) -> R.Element
-- formEdit defaultValue setter =
--   H.div { className: "form-group" }
--   [ H.input { defaultValue, type: "text", on: { input }
--             , placeholder: defaultValue, className: "form-control" }
--   ] where input = setter <<< const <<< R.unsafeEventValue

type FormChoiceSafeProps item m =
  ( items    :: Array item
  , default  :: item
  , callback :: item -> Effect m
  , print    :: item -> String )

-- | Form Choice input
-- if the list of options is not big enough, a button is used instead
formChoiceSafe :: forall item m
  .  Show item
  => R2.Component (FormChoiceSafeProps item m)
formChoiceSafe = R.createElement formChoiceSafeCpt
formChoiceSafeCpt :: forall item m. Show item => R.Component (FormChoiceSafeProps item m)
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
  .  Show item
  => R2.Component (FormChoiceProps item m)
formChoice = R.createElement formChoiceCpt
formChoiceCpt :: forall item m. Show item => R.Component (FormChoiceProps item m)
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
      change e = callback $ fromMaybe default $ reader $ R.unsafeEventValue e

      option opt = H.option { value: show opt } [ H.text $ print opt ]

      reader = GUS.reader items

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

type SubmitButtonProps =
  ( action :: Action
  , dispatch :: Action -> Aff Unit )

submitButton :: R2.Leaf SubmitButtonProps
submitButton = R2.leaf submitButtonCpt
submitButtonCpt :: R.Component SubmitButtonProps
submitButtonCpt = here.component "submitButton" cpt where
 cpt { action
     , dispatch } _ = do
   pure $ H.button { className : "btn btn-primary fa fa-" <> icon action
                   , type: "button"
                   , id: S.toLower $ show action
                   , title: show action
                   , on: {click: \_ -> launchAff $ dispatch action}
                   }
    [ H.span {className: "font-family-theme mx-1"} [ H.text $ " " <> text action] ]


type Href  = String

type SubmitButtonHrefProps =
  ( action :: Action
  , href   :: Href )

submitButtonHref :: R2.Leaf SubmitButtonHrefProps
submitButtonHref = R2.leaf submitButtonHrefCpt
submitButtonHrefCpt :: R.Component SubmitButtonHrefProps
submitButtonHrefCpt = here.component "submitButtonHref" cpt where
  cpt { action, href } _ = do
    pure $
      H.a { className, href, target: "_blank" }
        [ H.span {className: "font-family-theme mx-1"} [ H.text $ " " <> text action ] ]
    where
      className = "btn btn-primary fa fa-" <> icon action

------------------------------------------------------------------------
-- | CheckBox tools
-- checkboxes: Array of boolean values (basic: without pending option)
-- checkbox  : One boolean value only

type CheckboxProps =
  ( value :: T.Box Boolean )

checkbox :: R2.Leaf CheckboxProps
checkbox = R2.leaf checkboxCpt
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
