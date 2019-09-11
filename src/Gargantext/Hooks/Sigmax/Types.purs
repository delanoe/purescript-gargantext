module Gargantext.Hooks.Sigmax.Types where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Reactix as R
import DOM.Simple.Types (Element)

newtype Graph n e = Graph { nodes :: Seq {|n}, edges :: Seq {|e} }

derive instance eqGraph :: Eq Graph

--instance eqGraph :: Eq Graph where
--  eq (Graph {nodes: n1, edges: e1}) (Graph {nodes: n2, edges: e2}) = n1 == n2 && e1 == e2


type Renderer = { "type" :: String, container :: Element }

