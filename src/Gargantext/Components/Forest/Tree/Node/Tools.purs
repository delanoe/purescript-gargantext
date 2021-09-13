module Gargantext.Components.Forest.Tree.Node.Tools where

import Data.Foldable (intercalate)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.String.CodeUnits as DSCU
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Forest.Tree.Node.Action (icon, text)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action)
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Ends (Frontends, url)
import Gargantext.Prelude (class Ord, class Read, class Show, Unit, bind, const, discard, map, not, pure, read, show, when, mempty, ($), (<), (<<<), (<>), (<$>), (<*>))
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types as GT
import Gargantext.Utils (toggleSet, (?))
import Gargantext.Utils.Glyphicon (glyphicon)
import Gargantext.Utils.ReactTooltip as ReactTooltip
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
  cpt { boxAction, boxName, dispatch, id, isOpen, text } _ =
    content <$> T.useLive T.unequal isOpen <*> R.useRef text
    where
      content false _ = (R.fragment [])
      content true renameNodeNameRef =
        H.div { className: "from-group row" }
        [ textInput renameNodeNameRef
        , submitBtn renameNodeNameRef
        , cancelBtn
        ]
      textInput renameNodeNameRef =
        H.div { className: "col-8" }
          [ inputWithEnter {
                autoFocus: true
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
        H.a { type: "button"
            , title: "Submit"
            , on: { click: submit renameNodeNameRef }
            , className: "col-2 " <> glyphicon "floppy-o" } []
      cancelBtn =
        H.a { type: "button", title: "Cancel", on: { click }
            , className: "text-danger col-2 " <> glyphicon "times" } []
      submit ref _ = do
        launchAff_ $ dispatch (boxAction $ R.readRef ref)
        T.write_ false isOpen
      click _ = T.write_ false isOpen

type DefaultText = String

formEdit :: forall prev next
          . DefaultText -> ((prev -> String) -> Effect next) -> R.Element
formEdit defaultValue setter =
  H.div { className: "form-group" }
  [ H.input { defaultValue, type: "text", on: { input }
            , placeholder: defaultValue, className: "form-control" }
  ] where input = setter <<< const <<< R.unsafeEventValue

-- | Form Choice input
-- if the list of options is not big enough, a button is used instead
formChoiceSafe :: forall item m
  .  Read item
  => Show item
  => Array item
  -> item
  -> (item -> Effect m)
  -> (item -> String)
  -> R.Element
formChoiceSafe []  _   _   _    = mempty
formChoiceSafe [n] _   cbk prnt = formButton n cbk prnt
formChoiceSafe arr def cbk prnt = formChoice arr def cbk prnt

-- | List Form
formChoice :: forall item m
  .  Read item
  => Show item
  => Array item
  -> item
  -> (item -> Effect m)
  -> (item -> String)
  -> R.Element
formChoice items def cbk prnt =

  H.div { className: "form-group"}
  [
    R2.select
    { className: "form-control with-icon-font"
    , on: { change }
    } $
    map option items
  ]

  where
    change e = cbk $ fromMaybe def $ read $ R.unsafeEventValue e

    option opt =
      H.option { value: show opt }
      [ H.text $ prnt opt ]

-- | Button Form
-- FIXME: currently needs a click from the user (by default, we could avoid such click)
formButton :: forall item m
  .  item
  -> (item -> Effect m)
  -> (item -> String)
  -> R.Element
formButton item cbk prnt =

  H.div {}
  [
    H.text $ "Confirm the selection of: " <> prnt item
  ,
    cta
  ]

  where
    cta =
      H.button
      { className : "cold-md-5 btn btn-primary center"
      , type : "button"
      , title: "Form Button"
      , style : { width: "100%" }
      , on: { click: \_ -> cbk item }
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
  H.a { className, href, target: "_blank" } [ H.text $ " " <> text action ] where
    className = "btn btn-primary fa fa-" <> icon action

------------------------------------------------------------------------
-- | CheckBox tools
-- checkboxes: Array of boolean values (basic: without pending option)
-- checkbox  : One boolean value only

type CheckboxProps =
  ( value :: T.Box Boolean )

checkbox :: R2.Leaf CheckboxProps
checkbox props = R.createElement checkboxCpt props []
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
    cpt { groups, options } _ = do
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

tooltipId :: GT.NodeID -> String
tooltipId id = "node-link-" <> show id

-- START node link

type NodeLinkProps = (
    boxes      :: Boxes
  , folderOpen :: T.Box Boolean
  , frontends  :: Frontends
  , id         :: Int
  , isSelected :: Boolean
  , name       :: GT.Name
  , nodeType   :: GT.NodeType
  , session    :: Session
  )

nodeLink :: R2.Component NodeLinkProps
nodeLink = R.createElement nodeLinkCpt
nodeLinkCpt :: R.Component NodeLinkProps
nodeLinkCpt = here.component "nodeLink" cpt
  where
    cpt { boxes
        , folderOpen
        , frontends
        , id
        , isSelected
        , name
        , nodeType
        , session
        } _ = do

      pure $
        H.div { className: "node-link"
              , on: { click } }
          [ H.a { href, data: { for: tooltipId id, tip: true } }
            [ nodeText { isSelected, name }
            , ReactTooltip.reactTooltip { effect: "float", id: tooltipId id, type: "dark" }
                [ R2.row
                    [ H.h4 {className: GT.fldr nodeType true}
                        [ H.text $ GT.prettyNodeType nodeType ]
                    ]
                , R2.row [ H.span {} [ H.text $ name ]]
                ]
              ]
            ]

      where
        -- NOTE Don't toggle tree if it is not selected
        -- click on closed -> open
        -- click on open   -> ?
        click _ = when (not isSelected) (T.write_ true folderOpen)
        href = url frontends $ GT.NodePath (sessionId session) nodeType (Just id)
-- END node link

type NodeTextProps =
  ( isSelected :: Boolean
  , name       :: GT.Name
  )

nodeText :: R2.Leaf NodeTextProps
nodeText p = R.createElement nodeTextCpt p []
nodeTextCpt :: R.Memo NodeTextProps
nodeTextCpt = R.memo' $ here.component "nodeText" cpt where
  cpt props@{ isSelected } _ = do
    -- Computed
    let

      className = intercalate " "
        [ "node-text"
        , isSelected ? "node-text--selected" $ ""
        ]

      prefix = isSelected ?
        "" $
        "..."

      name = isSelected ?
        "| " <> (textEllipsisBreak 15 props.name) <> " |    " $
        textEllipsisBreak 15 props.name

    -- Render
    pure $

      H.span { className }
      [
        H.span {}
        [ H.text prefix ]
      ,
        H.span {}
        [ H.text name ]
      ]

textEllipsisBreak :: Int -> String -> String
textEllipsisBreak len n =
  if S.length n < len then n
  else case (DSCU.slice 0 len n) of
    Nothing -> "???"
    Just s  -> s <> "..."
