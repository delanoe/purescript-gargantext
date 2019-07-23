module Gargantext.Hooks.Sigmax.Types where

import Data.Maybe (Maybe)
import Data.Sequence as Seq
import Data.Sequence (Seq)
import Reactix as R
import DOM.Simple.Types (Element)

newtype Graph n e = Graph { nodes :: Seq {|n}, edges :: Seq {|e} }

type Renderer = { "type" :: String, container :: Element }

