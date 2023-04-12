module Gargantext.Components.GraphExplorer.Topbar.Search
  ( Props, nodeSearchControl ) where

import Prelude

import DOM.Simple.Console (log2)
import Data.Foldable (foldl, intercalate)
import Data.Sequence as Seq
import Data.Set as Set
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.InputWithAutocomplete (inputWithAutocomplete)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Utils (queryMatchesLabel)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Topbar.Search"

type Props =
  ( graph              :: SigmaxT.SGraph
  , multiSelectEnabled :: T.Box Boolean
  , selectedNodeIds    :: T.Box SigmaxT.NodeIds
  , className          :: String
  )

-- | Whether a node matches a search string
--   Searches given node and matches it's label or any of the children's labels.
nodeMatchesSearch :: String -> Record SigmaxT.Node -> Boolean
nodeMatchesSearch s n@{ children } =
  foldl (\_ childLabel -> queryMatchesLabel s childLabel) initial children
  where
    initial = queryMatchesLabel s n.label

searchNodes :: String -> Seq.Seq (Record SigmaxT.Node) -> Seq.Seq (Record SigmaxT.Node)
searchNodes "" _ = Seq.empty
searchNodes s nodes = Seq.filter (nodeMatchesSearch s) nodes

nodeSearchControl :: R2.Leaf Props
nodeSearchControl = R2.leaf nodeSearchControlCpt
nodeSearchControlCpt :: R.Component Props
nodeSearchControlCpt = here.component "nodeSearchControl" cpt
  where
    cpt props@{ graph, multiSelectEnabled, selectedNodeIds } _ = do
      search <- T.useBox ""
      search' <- T.useLive T.unequal search
      multiSelectEnabled' <- T.useLive T.unequal multiSelectEnabled

      let doSearch s = triggerSearch graph s multiSelectEnabled' selectedNodeIds

      pure $

        H.form
        { className: intercalate " "
            [ "graph-node-search"
            , props.className
            ]
        }
        [
          inputWithAutocomplete
          { autocompleteSearch: autocompleteSearch graph
          , onAutocompleteClick: doSearch
          , onEnterPress: doSearch
          , classes: ""
          , state: search
          , placeholder: "filter and select terms here..."
          }
        ,
          B.button
          { callback: \_ -> doSearch search'
          , type: "submit"
          , className: "graph-node-search__submit"
          }
          [
            B.icon { name: "search"}
          ]
        ]

autocompleteSearch :: SigmaxT.SGraph -> String -> Array String
autocompleteSearch graph s = Seq.toUnfoldable $ (_.label) <$> searchNodes s nodes
  where
    nodes = SigmaxT.graphNodes graph

triggerSearch :: SigmaxT.SGraph
              -> String
              -> Boolean
              -> T.Box SigmaxT.NodeIds
              -> Effect Unit
triggerSearch graph search multiSelectEnabled selectedNodeIds = do
  let graphNodes = SigmaxT.graphNodes graph
  let matching = Set.fromFoldable $ (_.id) <$> searchNodes search graphNodes

  log2 "[triggerSearch] search" search

  T.modify_ (\nodes ->
    Set.union matching $ if multiSelectEnabled then nodes else SigmaxT.emptyNodeIds) selectedNodeIds
