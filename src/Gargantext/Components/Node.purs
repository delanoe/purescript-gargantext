module Gargantext.Components.Node 
  where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Simple.JSON as JSON

import Gargantext.Prelude

type NodePolyCommon a =
  ( id :: Int
  , typename :: Int
  , name :: String
  , date :: String
  , hyperdata :: a )

newtype NodePoly a =
  NodePoly { userId   :: Int
           , parentId  :: Int
           | NodePolyCommon a
           }
derive instance Generic (NodePoly a) _
derive instance Newtype (NodePoly a) _
instance Eq a => Eq (NodePoly a) where eq = genericEq
instance JSON.ReadForeign a => JSON.ReadForeign (NodePoly a) where
  readImpl f = do
    inst :: { user_id :: Int, parent_id :: Int | NodePolyCommon a } <- JSON.readImpl f
    pure $ NodePoly { id: inst.id
                    , typename: inst.typename
                    , userId: inst.user_id
                    , parentId: inst.parent_id
                    , name: inst.name
                    , date: inst.date
                    , hyperdata: inst.hyperdata }

newtype HyperdataList = HyperdataList { preferences :: Maybe String }
derive instance Generic HyperdataList _
derive instance Newtype HyperdataList _
derive instance Eq HyperdataList
derive newtype instance JSON.ReadForeign HyperdataList
