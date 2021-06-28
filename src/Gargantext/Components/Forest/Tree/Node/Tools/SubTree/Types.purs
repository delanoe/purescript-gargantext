module Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic   (genericEq)
import Data.Show.Generic (genericShow)
import Gargantext.Prelude (class Eq, class Show)
import Gargantext.Types  as GT
import Reactix as R

data SubTreeOut = SubTreeOut { in  :: GT.ID
                             , out :: GT.ID
                             }
derive instance Generic SubTreeOut _
instance Eq SubTreeOut where
  eq = genericEq
instance Show SubTreeOut where
  show = genericShow

------------------------------------------------------------------------
data SubTreeParams = SubTreeParams { showtypes :: Array GT.NodeType
                                   , valitypes :: Array GT.NodeType
                                   }

derive instance Generic SubTreeParams _
instance Eq SubTreeParams where
  eq = genericEq
instance Show SubTreeParams where
  show = genericShow
------------------------------------------------------------------------


