module Gargantext.Components.GraphExplorer.Sidebar.Legend
  ( Props, legend
  ) where

import Prelude hiding (map)

import Data.Sequence (Seq)
import Data.Traversable (foldMap)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.GraphExplorer.Types (Legend(..), intColor)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Sidebar.Legend"

type Props = ( items :: Seq Legend )

legend :: R2.Leaf Props
legend = R2.leaf legendCpt

legendCpt :: R.Component Props
legendCpt = here.component "legend" cpt where
  cpt { items } _ = pure $

    H.ul
    { className: "graph-legend" }
    [
      flip foldMap items \(Legend { id_, label }) ->

        H.li
        { className: "graph-legend__item" }
        [
          H.span
          { className: "graph-legend__code"
          , style: { backgroundColor: intColor id_ }
          }
          []
        ,
          H.span
          { className: "graph-legend__caption" }
          [ H.text label ]
        ]
    ]
