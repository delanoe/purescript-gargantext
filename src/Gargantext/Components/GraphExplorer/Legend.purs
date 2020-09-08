module Gargantext.Components.GraphExplorer.Legend
  ( Props, legend, legendCpt
  ) where

import Prelude hiding (map)

import Data.Sequence (Seq)
import Data.Traversable (foldMap)
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.GraphExplorer.Types (Legend(..), intColor)
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.GraphExplorer.Legend"

type Props = ( items :: Seq Legend )

legend :: Record Props -> R.Element
legend props = R.createElement legendCpt props []

legendCpt :: R.Component Props
legendCpt = R2.hooksComponent thisModule "legend" cpt
  where
    cpt {items} _ = pure $ RH.div {} [foldMap entry items]

entry :: Legend -> R.Element
entry (Legend {id_, label}) =
  RH.p {}
  [ RH.span { style: { width : 10
                     , height: 10
                     , backgroundColor: intColor id_
                     , display: "inline-block"
                     } 
            } []
  , RH.text $ " " <> label
  ]
