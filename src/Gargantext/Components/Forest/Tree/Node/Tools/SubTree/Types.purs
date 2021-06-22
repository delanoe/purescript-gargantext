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
derive instance genericSubTreeOut :: Generic SubTreeOut _
instance eqSubTreOut      :: Eq SubTreeOut where
  eq = genericEq
instance showSubTreeOut           :: Show SubTreeOut where
  show = genericShow

------------------------------------------------------------------------
data SubTreeParams = SubTreeParams { showtypes :: Array GT.NodeType
                                   , valitypes :: Array GT.NodeType
                                   }

derive instance genericSubTreeParams :: Generic SubTreeParams _
instance eqSubTreeParams      :: Eq SubTreeParams where
  eq = genericEq
instance showSubTreeParams           :: Show SubTreeParams where
  show = genericShow
------------------------------------------------------------------------


