module Gargantext.Components.Forest.Tree.Node.Tools.FTree where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Simple.JSON as JSON

import Gargantext.Prelude

import Gargantext.Types  as GT

-----------------------------------------------------------------------
type ID   = Int
type Name = String
-----------------------------------------------------------------------
type FTree = NTree LNode
data NTree a = NTree a (Array (NTree a))
derive instance Generic (NTree a) _
instance JSON.ReadForeign (NTree LNode) where
  readImpl f = do
    inst :: { node :: LNode, children :: Array FTree } <- JSON.readImpl f
    let (LNode { id }) = inst.node
    pure $ NTree inst.node ((addParent id) <$> inst.children)
instance Eq a => Eq (NTree a) where
  eq (NTree a1 as1) (NTree a2 as2) = (eq a1 a2) && (eq as1 as2)

type Tree = { tree       :: FTree
            , tasks     :: Array GT.AsyncTaskWithType
            }

fTreeID :: FTree -> ID
fTreeID (NTree (LNode { id }) _) = id

instance Functor NTree where
  map f (NTree x ary) = NTree (f x) (map (map f) ary)

newtype LNode =
  LNode
  { id        :: ID
  , name      :: Name
  , nodeType  :: GT.NodeType
  , parent_id :: Maybe ID
  }
derive instance Newtype LNode _
derive instance Generic LNode _
instance Eq LNode where eq = genericEq
instance JSON.ReadForeign LNode where
  readImpl f = do
    inst :: { id :: ID, name :: Name, type :: GT.NodeType, parent_id :: Maybe ID } <- JSON.readImpl f
    pure $ LNode { id: inst.id
                 , name: inst.name
                 , nodeType: inst.type
                 , parent_id: Nothing }

addParent :: ID -> NTree LNode -> NTree LNode
addParent id (NTree (LNode p@{id:id'}) ary)=
  NTree (LNode (p {parent_id=Just id}))
        (map (addParent id') ary)
