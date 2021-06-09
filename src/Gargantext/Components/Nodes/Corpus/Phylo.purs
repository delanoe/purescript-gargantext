module Gargantext.Components.Nodes.Corpus.Phylo where

import Gargantext.Prelude
  ( Unit, bind, const, discard, pure, read, show, unit, ($), (<$>), (<>), (==) )

import Data.Array as A
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Components.Nodes.Corpus (fieldsCodeEditor)
import Gargantext.Components.Nodes.Corpus.Chart.Predefined as P
import Gargantext.Components.Nodes.Dashboard.Types as DT
import Gargantext.Components.Nodes.Types (FTField, FTFieldsWithIndex, defaultField)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Phylo"

type Props = ( nodeId :: NodeID, session :: Session )

phyloLayout :: R2.Component Props
phyloLayout = R.createElement phyloLayoutCpt

phyloLayoutCpt :: R.Component Props
phyloLayoutCpt = here.component "phyloLayout" cpt where
  cpt { nodeId, session } content = do
    pure $  H.h1 {} [ H.text "Hello Phylo" ]
