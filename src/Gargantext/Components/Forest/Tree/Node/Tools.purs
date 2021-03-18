module Gargantext.Components.Forest.Tree.Node.Tools where

import Gargantext.Prelude
  ( class Ord, class Read, class Show, Unit
  , bind, const, discard, map, not, pure, read, show, when
  , ($), (<), (<<<), (<>), (<$>), (<*>) )
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Nullable (null)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.String.CodeUnits as DSCU
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.Forest.Tree.Node.Action (Action, icon, text)
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Ends (Frontends, url)
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types as GT
import Gargantext.Utils (glyphicon, toggleSet)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.ReactTooltip as ReactTooltip

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
formChoiceSafe :: forall a b c
               .  Read  a
               => Show  a
               => Array a
               -> a
               -> (a -> Effect c)
               -- -> ((b -> a) -> Effect c)
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
           -> (b -> Effect a)
           -- -> ((c -> b) -> Effect a)
           -> R.Element
formChoice nodeTypes defaultNodeType setNodeType = 
  H.div { className: "form-group"}
        [ R2.select { className: "form-control"
                    , on: { change: \e -> setNodeType $ fromMaybe defaultNodeType $ read $ R.unsafeEventValue e }
                    }
          (map (\opt -> H.option {} [ H.text $ show opt ]) nodeTypes)
         ]

-- | Button Form
-- FIXME: currently needs a click from the user (by default, we could avoid such click)
formButton :: forall a b c
           . Show a
           =>   a
           -> (a -> Effect c)
           -- -> ((b -> a) -> Effect c)
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
                        , on: { click: \_ -> setNodeType nodeType }
                        } [H.text $ "Confirmation"]

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
-- checkboxes: Array of poolean values (basic: without pending option)
-- checkbox  : One boolean value only

checkbox :: R.State Boolean -> R.Element
checkbox ( val /\ set ) =
  H.input
  { className: "form-check-input", type: "checkbox"
  , value: val, on: { click } } where
    click _ = set $ const $ not val

data CheckBoxes = Multiple | Uniq

checkboxes :: forall a. Ord a => Show a => Array a -> R.State (Set a) -> R.Element
checkboxes xs (val /\ set) = H.fieldset {} $ map field xs where
  field a =
    H.div {}
    [ H.input { defaultChecked, type: "checkbox", on: { click } }
    , H.div {} [ H.text $ show a ] ] where
      click _ = set (const $ toggleSet a val)
      defaultChecked = Set.member a val

checkboxesListGroup
  :: forall a. Ord a => Show a => Array a -> R.State (Set a) -> Array R.Element
checkboxesListGroup xs (val /\ set) = map one xs where
  one a =
    H.li { className: "list-group-item" }
    [ H.div { className: "form-check" }
      [ H.input { defaultChecked, type: "checkbox", on: { click }
                , className: "form-check-input" }
      , H.label { className: "form-check-label" } [ H.text (show a) ] ]
    ] where
      click _ = set (const $ toggleSet a val)
      defaultChecked = Set.member a val

prettyNodeType :: GT.NodeType -> String
prettyNodeType
  =   S.replace (S.Pattern "Node")   (S.Replacement " ")
  <<< S.replace (S.Pattern "Folder") (S.Replacement " ")
  <<< show

tooltipId :: GT.NodeID -> String
tooltipId id = "node-link-" <> show id

-- START node link

type NodeLinkProps = (
    frontends  :: Frontends
  , folderOpen :: T.Box Boolean
  , handed     :: GT.Handed
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
    cpt { folderOpen
        , frontends
        , handed
        , id
        , isSelected
        , name
        , nodeType
        , session
        } _ = do
      popoverRef <- R.useRef null

      pure $
        H.div { className: "node-link"
              , on: { click } }
          [ H.a { href, data: { for: tooltipId id, tip: true } }
            [ nodeText { handed, isSelected, name } []
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
  , handed     :: GT.Handed
  , name       :: GT.Name
  )

nodeText :: R2.Component NodeTextProps
nodeText = R.createElement nodeTextCpt

nodeTextCpt :: R.Component NodeTextProps
nodeTextCpt = here.component "nodeText" cpt where
  cpt { isSelected, handed, name } _ =
    pure $ if isSelected then
              H.u { className }
                [ H.b {}
                  [ H.text ("| " <> name15 name <> " |    ") ]
                ]
              else
                GT.flipHanded l r handed where
                  l = H.text "..."
                  r = H.text (name15 name)
  name_ len n =
    if S.length n < len then n
    else case (DSCU.slice 0 len n) of
      Nothing -> "???"
      Just s  -> s <> "..."
  name15 = name_ 15
  className = "node-text"
