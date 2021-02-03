module Gargantext.Components.Forest.Tree.Node.Tools where

import Data.Maybe (fromMaybe, Maybe(..))
import Data.Nullable (null)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.String.CodeUnits as DSCU
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Ends (Frontends, url)
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (ID, Name)
import Gargantext.Types as GT
import Gargantext.Utils (glyphicon, toggleSet)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.ReactTooltip as ReactTooltip

thisModule :: String
thisModule = "Gargantext.Components.Forest.Tree.Node.Tools"

------------------------------------------------------------------------

type Body    = Array R.Element
type Footer  = R.Element

panel :: Body -> Footer -> R.Element
panel bodies submit =
  R.fragment [ panelBody, footer ]
    where
      panelBody =
          H.div { className: "card-body" }
          [ H.div { className: "row" }
                  [ H.div { className: "col-12" } bodies
                          -- TODO add type for text or form here
                          -- [ H.form {className: "form-horizontal"} bodies ]
                  ]
            ]
      footer =
        H.div {className: "card-footer"}
          [ H.div { className: "row" }
              [ H.div { className: "mx-auto"} [ submit ]
              ]
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

textInputBox :: R2.Component TextInputBoxProps
textInputBox = R.createElement textInputBoxCpt

textInputBoxCpt :: R.Component TextInputBoxProps
textInputBoxCpt = R.hooksComponentWithModule thisModule "textInputBox" cpt
  where
    cpt p@{ boxAction, boxName, dispatch, id, isOpen: (true /\ setIsOpen), text } _ = do
      renameNodeNameRef <- R.useRef text

      pure $ H.div { className: "from-group row" }
        [ textInput renameNodeNameRef
        , submitBtn renameNodeNameRef
        , cancelBtn
        ]
      where
        textInput renameNodeNameRef =
          H.div { className: "col-8" }
            [ inputWithEnter {
                 onBlur: R.setRef renameNodeNameRef
               , onEnter: submit renameNodeNameRef
               , onValueChanged: R.setRef renameNodeNameRef
               , autoFocus: true
               , className: "form-control"
               , defaultValue: text
               , placeholder: (boxName <> " Node")
               , type: "text"
               }
            ]
        submitBtn renameNodeNameRef =
          H.a { className: "col-2 " <> glyphicon "floppy-o"
              , type: "button"
              , on: { click: submit renameNodeNameRef }
              , title: "Submit"
              } []
        cancelBtn =
          H.a { className: "text-danger col-2 " <> glyphicon "times"
              , type: "button"
              , on: { click: \_ -> setIsOpen $ const false }
              , title: "Cancel"
              } []
        submit renameNodeNameRef _ = do
          launchAff_ $ dispatch ( boxAction $ R.readRef renameNodeNameRef )
          setIsOpen $ const false
    cpt { isOpen: (false /\ _) } _ = pure $ H.div {} []

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
                                <<< R.unsafeEventValue }
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
                    , on: { change: setNodeType
                                      <<< const
                                      <<< fromMaybe defaultNodeType
                                      <<< read
                                      <<< R.unsafeEventValue }
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
           , id: S.toLower $ show action
           , title: show action
           , on: {click: \_ -> launchAff $ dispatch action}
           }
           [ H.text $ " " <> text action]

type Href  = String

submitButtonHref :: Action -> Href -> R.Element
submitButtonHref action href =
  H.a { className : "btn btn-primary fa fa-" <> icon action
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
  H.input { className : "form-check-input"
          , type: "checkbox"
          , value: val
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
                                           , defaultChecked: Set.member a val
                                           , on: { click: \_ -> set
                                                             $ const
                                                             $ toggleSet a val
                                                 }
                                           }
                                 , H.div {} [H.text $ show a]
                                 ]
                  ) xs

checkboxesListGroup :: forall a
           .  Ord   a
           => Show  a
           => Array a
           -> R.State (Set a)
           -> Array R.Element
checkboxesListGroup xs (val /\ set) =
  map (\a -> H.li { className: "list-group-item" }
             [ H.div { className: "form-check" }
               [ H.input { type: "checkbox"
                         , className: "form-check-input"
                         , defaultChecked: Set.member a val
                         , on: { click: \_ -> set
                                            $ const
                                            $ toggleSet a val
                             }
                       }
               , H.label { className: "form-check-label" } [ H.text $ show a ]
               ]
             ]
      ) xs


prettyNodeType :: GT.NodeType -> String
prettyNodeType nt = S.replace (S.Pattern "Node")   (S.Replacement " ")
                  $ S.replace (S.Pattern "Folder") (S.Replacement " ")
                  $ show nt


-- START node link
type NodeLinkProps = (
    frontends  :: Frontends
  , id         :: Int
  , folderOpen :: R.State Boolean
  , isSelected :: Boolean
  , name       :: Name
  , nodeType   :: GT.NodeType
  , session    :: Session
  , handed     :: GT.Handed
  )

nodeLink :: R2.Component NodeLinkProps
nodeLink = R.createElement nodeLinkCpt

nodeLinkCpt :: R.Component NodeLinkProps
nodeLinkCpt = R.hooksComponentWithModule thisModule "nodeLink" cpt
  where
    cpt { folderOpen: (_ /\ setFolderOpen)
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
              , on: { click: onClick } }
              [ H.a { data: { for: tooltipId
                            , tip: true
                            }
                     , href: url frontends $ GT.NodePath (sessionId session) nodeType (Just id) }
                                           [ nodeText { isSelected
                                                      , name
                                                      , handed
                                                      }
                                           ]
                     , ReactTooltip.reactTooltip { id: tooltipId }
                                                 [ R2.row [ H.h4 {className: GT.fldr nodeType true}
                                                                 [ H.text $ prettyNodeType nodeType ]
                                                          ]
                                                 , R2.row [ H.span {} [ H.text $ name ]]
                                                 ]
              ]

      where
        -- NOTE Don't toggle tree if it is not selected
        -- click on closed -> open
        -- click on open   -> ?
        onClick _ = if isSelected then
                      setFolderOpen (const true)
                    else
                      setFolderOpen (const true)
        tooltipId = "node-link-" <> show id
-- END node link


-- START node text
type NodeTextProps =
  ( isSelected :: Boolean
  , name       :: Name
  , handed     :: GT.Handed
  )

nodeText :: Record NodeTextProps -> R.Element
nodeText p = R.createElement nodeTextCpt p []

nodeTextCpt :: R.Component NodeTextProps
nodeTextCpt = R.hooksComponentWithModule thisModule "nodeText" cpt
  where
    cpt { isSelected: true, name } _ = do
      pure $ H.u {} [
        H.b {} [
           H.text ("| " <> name15 name <> " |    ")
         ]
        ]
    cpt {isSelected: false, name, handed} _ = do
      pure $ if handed == GT.RightHanded
                then H.text "..." <> H.text (name15 name)
                else H.text (name15 name) <> H.text "..."

    name len n = if S.length n < len then
                 n
               else
                 case (DSCU.slice 0 len n) of
                   Nothing -> "???"
                   Just s  -> s <> "..."
    name15 = name 15
-- END node text

------------------------------------------------------------------------

