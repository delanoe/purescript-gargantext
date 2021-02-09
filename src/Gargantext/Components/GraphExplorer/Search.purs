module Gargantext.Components.GraphExplorer.Search
  ( Props
  , nodeSearchControl
  ) where

import Prelude
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.InputWithAutocomplete (inputWithAutocomplete)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Utils (queryMatchesLabel)
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.GraphExplorer.Search"

type Props = (
    graph           :: SigmaxT.SGraph
  , multiSelectEnabled :: R.State Boolean
  , selectedNodeIds :: R.State SigmaxT.NodeIds
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
sizeButtonCpt = R.hooksComponentWithModule thisModule "nodeSearchControl" cpt
  where
    cpt {graph, multiSelectEnabled, selectedNodeIds} _ = do
      search@(search' /\ setSearch) <- R.useState' ""

      pure $
        H.div { className: "form-group" }
          [ H.div { className: "input-group" }
            [ inputWithAutocomplete { autocompleteSearch: autocompleteSearch graph
                                    , onAutocompleteClick: \s -> triggerSearch graph s multiSelectEnabled selectedNodeIds
                                    , onEnterPress: \s -> triggerSearch graph s multiSelectEnabled selectedNodeIds
                                    , state: search } []
            , H.div { className: "btn input-group-addon"
                    , on: { click: \_ -> triggerSearch graph search' multiSelectEnabled selectedNodeIds }
                    }
              [ H.span { className: "fa fa-search" } [] ]
            ]
          ]

autocompleteSearch :: SigmaxT.SGraph -> String -> Array String
autocompleteSearch graph s = Seq.toUnfoldable $ (_.label) <$> searchNodes s nodes
  where
    nodes = SigmaxT.graphNodes graph

triggerSearch :: SigmaxT.SGraph
              -> String
              -> R.State Boolean
              -> R.State SigmaxT.NodeIds
              -> Effect Unit
triggerSearch graph search (multiSelectEnabled /\ _) (_ /\ setNodeIds) = do
  let graphNodes = SigmaxT.graphNodes graph
  let matching = Set.fromFoldable $ (_.id) <$> searchNodes search graphNodes

  log2 "[triggerSearch] search" search

  setNodeIds $ \nodes ->
    Set.union matching $ if multiSelectEnabled then nodes else SigmaxT.emptyNodeIds
