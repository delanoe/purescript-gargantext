module NTree where

import Data.Tuple (Tuple(..))
import Prelude hiding (div)
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

myCorpus :: Int -> String -> NTree (Tuple String String)
myCorpus n name = NNode n false name
    [ NLeaf (Tuple "Facets"    "#/corpus")
    , NLeaf (Tuple "Graph"     "#/corpus")
    , NLeaf (Tuple "Dashboard" "#/userPage")
    ]

exampleTree :: NTree (Tuple String String)
exampleTree =
  NNode 1 true "My gargantext"
  [ myCorpus 2 "My publications"
  , myCorpus 3 "My community"
  , NNode 4 false "My researchs" [ myCorpus 5 "Subject A"
                                 , myCorpus 6 "Subject B"
                                 , myCorpus 7 "Subject C"
                                 ]
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
