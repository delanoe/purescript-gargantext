module Gargantext.Components.GraphExplorer.Search
  ( Props, nodeSearchControl ) where

import Prelude
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.InputWithAutocomplete (inputWithAutocomplete)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Utils (queryMatchesLabel)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Search"

type Props = (
    graph              :: SigmaxT.SGraph
  , multiSelectEnabled :: T.Box Boolean
  , selectedNodeIds    :: T.Box SigmaxT.NodeIds
  )

-- | Whether a node matches a search string
nodeMatchesSearch :: String -> Record SigmaxT.Node -> Boolean
nodeMatchesSearch s n = queryMatchesLabel s n.label

searchNodes :: String -> Seq.Seq (Record SigmaxT.Node) -> Seq.Seq (Record SigmaxT.Node)
searchNodes "" _ = Seq.empty
searchNodes s nodes = Seq.filter (nodeMatchesSearch s) nodes

nodeSearchControl :: R2.Component Props
nodeSearchControl = R.createElement sizeButtonCpt

sizeButtonCpt :: R.Component Props
sizeButtonCpt = here.component "nodeSearchControl" cpt
  where
    cpt { graph, multiSelectEnabled, selectedNodeIds } _ = do
      search <- T.useBox ""
      search' <- T.useLive T.unequal search
      multiSelectEnabled' <- T.useLive T.unequal multiSelectEnabled

      pure $ R.fragment
        [ inputWithAutocomplete { autocompleteSearch: autocompleteSearch graph
                                , classes: "mx-2"
                                , onAutocompleteClick: \s -> triggerSearch graph s multiSelectEnabled' selectedNodeIds
                                , onEnterPress: \s -> triggerSearch graph s multiSelectEnabled' selectedNodeIds
                                , state: search } []
        , H.div { className: "btn input-group-addon"
                , on: { click: \_ -> triggerSearch graph search' multiSelectEnabled' selectedNodeIds }
                }
          [ H.span { className: "fa fa-search" } [] ]
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
