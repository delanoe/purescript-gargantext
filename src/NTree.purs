module NTree where

import Prelude hiding (div)

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
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

performAction :: PerformAction _ State _ Action
performAction (ToggleFolder i) _ _ = void (cotransform (\td -> toggleNode i td))

toggleNode :: forall t10. Int -> NTree t10 -> NTree t10
toggleNode sid (NNode iid open name ary) =
  NNode iid nopen name $ map (toggleNode sid) ary
  where
    nopen = if sid == iid then not open else open
toggleNode sid a = a



------------------------------------------------------------------------
-- Realistic Tree for the UI

corpus :: Int -> String -> NTree (Tuple String String)
corpus n name = NNode n false name
    [ NLeaf (Tuple "Facets"    "#/docView")
    , NLeaf (Tuple "Dashboard" "#/dashboard")
    , NLeaf (Tuple "Graph"     "#/graphExplorer")
    ]

annuaire :: Int -> String -> NTree (Tuple String String)
annuaire n name = NNode n false name
    [ NLeaf (Tuple "Facets"    "#/docView")
    ]

exampleTree :: NTree (Tuple String String)
exampleTree =
  NNode 1 true "françois.pineau"
  [ annuaire 2 "Annuaire"
  , corpus   3 "IMT publications"
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


treeview :: Spec _ State _ Action
treeview = simpleSpec performAction render
  where
    render :: Render State _ Action
    render dispatch _ state _ =
      [div [className "tree"] [toHtml dispatch state]]


toHtml :: _ -> FTree -> ReactElement
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

loadDefaultNode :: forall eff. Aff (ajax :: AJAX, console :: CONSOLE | eff) (Either String (Array LNode))
loadDefaultNode = do
  res <- liftAff $ attempt $ affjax defaultRequest
         { url = "http://localhost:8008/user"
         , method = Left GET
         }
  case res of
    Left err -> do
      _ <- liftEff $ log $ show err
      pure $ Left $ show err
    Right a -> do
      _ <- liftEff $ log $ show a.status
      _ <- liftEff $ log $ show a.headers
      _ <- liftEff $ log $ show a.response
      let resp = decodeJson a.response
      pure resp


fnTransform :: LNode -> FTree
fnTransform (LNode r) = NNode r.id false r.name []
