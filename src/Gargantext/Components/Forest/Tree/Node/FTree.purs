module Gargantext.Components.Forest.Tree.Node.FTree where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Prelude hiding (div)

import Gargantext.Components.Lang (Lang)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, put, post, delete)
import Gargantext.Routes as GR
import Gargantext.Types  as GT

-----------------------------------------------------------------------
type ID   = Int
type Name = String
-----------------------------------------------------------------------
type FTree = NTree LNode
data NTree a = NTree a (Array (NTree a))
type Tree = { tree       :: FTree
            , asyncTasks :: Array GT.AsyncTaskWithType
            }

instance ntreeFunctor :: Functor NTree where
  map f (NTree x ary) = NTree (f x) (map (map f) ary)

newtype LNode = LNode { id :: ID
                      , name :: Name
                      , nodeType :: GT.NodeType
                      }

derive instance newtypeLNode :: Newtype LNode _

instance decodeJsonLNode :: DecodeJson LNode where
  decodeJson json = do
    obj  <- decodeJson json
    id_  <- obj .: "id"
    name <- obj .: "name"
    nodeType <- obj .: "type"
    pure $ LNode { id : id_
                 , name
                 , nodeType
                 }

instance decodeJsonFTree :: DecodeJson (NTree LNode) where
  decodeJson json = do
    obj    <- decodeJson json
    node   <- obj .: "node"
    nodes  <- obj .: "children"
    node'  <- decodeJson node
    nodes' <- decodeJson nodes
    pure $ NTree node' nodes'

