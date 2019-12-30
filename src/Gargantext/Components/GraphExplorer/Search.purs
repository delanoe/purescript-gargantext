module Gargantext.Components.GraphExplorer.Search
  ( Props
  , nodeSearchControl
  ) where

import Prelude
import Data.Sequence as Seq
import Data.Set as Set
import Data.String as S
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.InputWithAutocomplete (inputWithAutocomplete)
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes

type Props = (
    graph           :: SigmaxTypes.SGraph
  , multiSelectEnabled :: R.State Boolean
  , selectedNodeIds :: R.State SigmaxTypes.SelectedNodeIds
  )

-- | Whether a node matches a search string
nodeMatchesSearch :: String -> Record SigmaxTypes.Node -> Boolean
nodeMatchesSearch s n = S.contains (S.Pattern $ normalize s) (normalize n.label)
  where
    normalize = S.toLower

searchNodes :: String -> Seq.Seq (Record SigmaxTypes.Node) -> Seq.Seq (Record SigmaxTypes.Node)
searchNodes "" _ = Seq.empty
searchNodes s nodes = Seq.filter (nodeMatchesSearch s) nodes

nodeSearchControl :: Record Props -> R.Element
nodeSearchControl props = R.createElement sizeButtonCpt props []

sizeButtonCpt :: R.Component Props
sizeButtonCpt = R.hooksComponent "NodeSearchControl" cpt
  where
    cpt {graph, multiSelectEnabled, selectedNodeIds} _ = do
      search@(search' /\ setSearch) <- R.useState' ""

      pure $
        H.div { className: "form-group" }
          [ H.div { className: "input-group" }
            [ inputWithAutocomplete { autocompleteSearch: autocompleteSearch graph
                                    , onAutocompleteClick: \s -> triggerSearch graph s multiSelectEnabled selectedNodeIds
                                    , onEnterPress: \s -> triggerSearch graph s multiSelectEnabled selectedNodeIds
                                    , state: search }
            , H.div { className: "btn input-group-addon"
                    , on: { click: \_ -> triggerSearch graph search' multiSelectEnabled selectedNodeIds }
                    }
              [ H.span { className: "fa fa-search" } [] ]
            ]
          ]

autocompleteSearch :: SigmaxTypes.SGraph -> String -> Array String
autocompleteSearch graph s = Seq.toUnfoldable $ (_.label) <$> searchNodes s nodes
  where
    nodes = SigmaxTypes.graphNodes graph

triggerSearch :: SigmaxTypes.SGraph
              -> String
              -> R.State Boolean
              -> R.State SigmaxTypes.SelectedNodeIds
              -> Effect Unit
triggerSearch graph search (multiSelectEnabled /\ _) (_ /\ setSelectedNodeIds) = do
  let graphNodes = SigmaxTypes.graphNodes graph
  let matching = Set.fromFoldable $ (_.id) <$> searchNodes search graphNodes

  log2 "[triggerSearch] search" search

  setSelectedNodeIds $ \nodes ->
    Set.union matching $ if multiSelectEnabled then nodes else Set.empty
