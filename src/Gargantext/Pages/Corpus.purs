module Gargantext.Pages.Corpus where

import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Config (Ends)
import Gargantext.Utils.Reactix as R2

type Props = ( nodeId :: Int, ends :: Ends )

corpusLayout :: Record Props -> R.Element
corpusLayout props = R.createElement corpusLayoutCpt props []

corpusLayoutCpt :: R.Component Props
corpusLayoutCpt = R.staticComponent "G.P.Corpus.corpusLayout" cpt
  where
    cpt {nodeId} _children = do
      pure $ H.div {} [ H.h1 {} [H.text "Corpus Description"]
                      , H.p  {} [H.text "Soon: corpus synthesis here (when all others charts/features will be stabilized)."]
                      ]
