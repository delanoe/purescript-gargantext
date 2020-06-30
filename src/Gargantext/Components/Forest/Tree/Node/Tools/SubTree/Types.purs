module Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq   (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Gargantext.Prelude (class Eq, class Show)
import Gargantext.Types  as GT
import Reactix as R

data SubTreeOut = SubTreeOut { in  :: GT.ID
                             , out :: GT.ID
                             }

------------------------------------------------------------------------
data SubTreeParams = SubTreeParams { showtypes :: Array GT.NodeType
                                   , valitypes :: Array GT.NodeType
                                   }

derive instance eqSubTreeParams      :: Eq SubTreeParams
derive instance genericSubTreeParams :: Generic SubTreeParams _
instance showSubTreeParams           :: Show SubTreeParams where
  show = genericShow
------------------------------------------------------------------------


