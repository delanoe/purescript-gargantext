module Gargantext.Components.Tree where

import Prelude hiding (div)

import Affjax (defaultRequest, printResponseFormatError, request)
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import React (ReactElement)
import React.DOM (a, div, i, li, text, ul)
import React.DOM.Props (Props, className, href, onClick)
import Thermite (PerformAction, Render, Spec, cotransform, simpleSpec)

type Name = String
type Open = Boolean
type URL  = String
type ID   = Int

data NTree a = NLeaf a | NNode ID Open Name (Array (NTree a))

type FTree = NTree (Tuple Name URL)

data Action = ToggleFolder ID

type State = FTree

initialState :: State
initialState = NLeaf (Tuple "" "")

performAction :: PerformAction State {} Action
performAction (ToggleFolder i) _ _ = void $
 cotransform (\td -> toggleNode i td)

toggleNode :: forall t10. Int -> NTree t10 -> NTree t10
toggleNode sid (NNode iid open name ary) =
  NNode iid nopen name $ map (toggleNode sid) ary
  where
    nopen = if sid == iid then not open else open
toggleNode sid a = a



------------------------------------------------------------------------
-- Realistic Tree for the UI

exampleTree :: NTree (Tuple String String)
exampleTree =
  NNode 1 true "françois.pineau"
  [ annuaire 2 "Annuaire"
  , corpus   3 "IMT publications"
  ]

annuaire :: Int -> String -> NTree (Tuple String String)
annuaire n name = NNode n false name
    [ NLeaf (Tuple "IMT community"    "#/docView")
    ]

corpus :: Int -> String -> NTree (Tuple String String)
corpus n name = NNode n false name
    [ NLeaf (Tuple "Facets"    "#/corpus")
    , NLeaf (Tuple "Dashboard" "#/dashboard")
    , NLeaf (Tuple "Graph"     "#/graphExplorer")
    ]


------------------------------------------------------------------------
-- TODO
-- alignment to the right
nodeOptionsCorp :: Boolean -> Array ReactElement
nodeOptionsCorp activated = case activated of
                         true  -> [ i [className "fab fa-whmcs" ] []]
                         false -> []

-- TODO
-- alignment to the right
-- on hover make other options available:
nodeOptionsView :: Boolean -> Array ReactElement
nodeOptionsView activated = case activated of
                         true -> [ i [className "fas fa-sync-alt" ] []
                                 , i [className "fas fa-upload"   ] []
                                 , i [className "fas fa-share-alt"] []
                                 ]
                         false -> []


treeview :: Spec State {} Action
treeview = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      [div [className "tree"] [toHtml dispatch state]]


toHtml :: (Action -> Effect Unit) -> FTree -> ReactElement
toHtml d (NLeaf (Tuple name link)) =
  li []
  [ a [ href link]
    ( [ text (name <> "    ")
      ] <> nodeOptionsView false
    )
  ]
toHtml d (NNode id open name ary) =
  ul [ ]
  [ li [] $
    ( [ a [onClick $ (\e-> d $ ToggleFolder id)] [i [fldr open] []]
      ,  text $ " " <> name <> "    "
      ] <> nodeOptionsCorp false <>
      if open then
        map (toHtml d) ary
        else []
    )
  ]

fldr :: Boolean -> Props
fldr open = if open then className "fas fa-folder-open" else className "fas fa-folder"


newtype LNode = LNode {id :: Int, name :: String}

-- derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj <- decodeJson json
    id_ <- obj .? "id"
    name <- obj .? "name"
    pure $ LNode {id : id_, name}

loadDefaultNode :: Aff (Either String (Array LNode))
loadDefaultNode = do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/user"
         , responseFormat = ResponseFormat.json
         , method = Left GET
         , headers = []
         }
  case res.body of
    Left err -> do
      _ <- liftEffect $ log $ printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      --_ <- liftEffect $ log $ show a.status
      --_ <- liftEffect $ log $ show a.headers
      --_ <- liftEffect $ log $ show a.body
      let obj = decodeJson json
      pure obj


fnTransform :: LNode -> FTree
fnTransform (LNode r) = NNode r.id false r.name []
