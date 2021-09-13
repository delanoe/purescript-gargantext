module Gargantext.Components.Nodes.Corpus.Phylo where

import Gargantext.Prelude
  ( pure, ($) )

-- import Gargantext.Utils.Toestand as T2
-- import Toestand as T
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Phylo"

type Props = ( nodeId :: NodeID, session :: Session )

phyloLayout :: R2.Component Props
phyloLayout = R.createElement phyloLayoutCpt
phyloLayoutCpt :: R.Component Props
phyloLayoutCpt = here.component "phyloLayout" cpt where
  cpt { nodeId, session } content = do
    pure $  H.h1 {} [ H.text "Hello Phylo" ]
