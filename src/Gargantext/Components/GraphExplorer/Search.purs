module Gargantext.Components.GraphExplorer.Search
  ( Props
  , nodeSearchControl
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Sequence as Seq
import Data.Set as Set
import Data.String as S
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import FFI.Simple ((..))
import Reactix as R
import Reactix.DOM.HTML as H

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
searchNodes s = Seq.filter (nodeMatchesSearch s)

nodeSearchControl :: Record Props -> R.Element
nodeSearchControl props = R.createElement sizeButtonCpt props []

sizeButtonCpt :: R.Component Props
sizeButtonCpt = R.hooksComponent "NodeSearchControl" cpt
  where
    cpt {graph, selectedNodeIds} _ = do
      search@(search' /\ setSearch) <- R.useState' Nothing

      pure $
        H.div { className: "form-group" }
          [ H.div { className: "input-group" }
            [ H.input { type: "text"
                      , className: "form-control"
                      , defaultValue: fromMaybe "" search'
                      , on: { input: \e -> setSearch $ const $ Just $ e .. "target" .. "value" }
                      }
            , H.div { className: "btn input-group-addon"
                    , on: { click: \_ -> triggerSearch graph search selectedNodeIds }
                    }
              [ H.span { className: "fa fa-search" } [] ]
            ]
          ]

    triggerSearch :: SigmaxTypes.SGraph
                  -> R.State (Maybe String)
                  -> R.State SigmaxTypes.SelectedNodeIds
                  -> Effect Unit
    triggerSearch graph (search /\ setSearch) (_ /\ setSelectedNodeIds) = do
      case search of
        Nothing -> pure unit
        Just s  -> do
          let nodes = SigmaxTypes.graphNodes graph

          setSelectedNodeIds $ const $ Set.fromFoldable $ ((_.id) <$> searchNodes s nodes)
