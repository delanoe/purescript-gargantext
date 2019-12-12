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
    cpt {graph, selectedNodeIds} _ = do
      search@(search' /\ setSearch) <- R.useState' ""

      pure $
        H.div { className: "form-group" }
          [ H.div { className: "input-group" }
            [ inputWithAutocomplete { autocompleteSearch: autocompleteSearch graph
                                    , onAutocompleteClick: \s -> triggerSearch graph s selectedNodeIds
                                    , onEnterPress: \s -> triggerSearch graph s selectedNodeIds
                                    , state: search }
            , H.div { className: "btn input-group-addon"
                    , on: { click: \_ -> triggerSearch graph search' selectedNodeIds }
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
                  -> R.State SigmaxTypes.SelectedNodeIds
                  -> Effect Unit
    triggerSearch graph search (_ /\ setSelectedNodeIds) = do
      let nodes = SigmaxTypes.graphNodes graph
      let matching = (_.id) <$> searchNodes search nodes

      log2 "[triggerSearch] search" search

      setSelectedNodeIds $ const $ Set.fromFoldable matching
