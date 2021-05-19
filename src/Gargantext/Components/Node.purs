module Gargantext.Components.Node 
  where

import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?), (.!=))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)

import Gargantext.Prelude

newtype NodePoly a =
  NodePoly { id :: Int
           , typename :: Int
           , userId   :: Int
           , parentId  :: Int
           , name      :: String
           , date      :: String
           , hyperdata :: a
           }
derive instance genericNodePoly :: Generic (NodePoly a) _
instance eqNodePoly :: Eq a => Eq (NodePoly a) where
  eq = genericEq
instance decodeNodePoly :: (DecodeJson a)
  => DecodeJson (NodePoly a) where
  decodeJson json = do
    obj <- decodeJson json
    id        <- obj .: "id"
    typename  <- obj .: "typename"
    userId    <- obj .: "user_id"
    parentId  <- obj .: "parent_id"
    name      <- obj .: "name"
    date      <- obj .: "date"

    hyperdata  <- obj .: "hyperdata"
    hyperdata' <- decodeJson hyperdata

    pure $ NodePoly { id
                    , date
                    , hyperdata: hyperdata'
                    , name
                    , parentId
                    , typename
                    , userId
                    }

newtype HyperdataList = HyperdataList { preferences :: String }

instance decodeHyperdataList :: DecodeJson HyperdataList where
  decodeJson json = do
    obj <- decodeJson json
    pref <- obj .:? "preferences" .!= ""
    pure $ HyperdataList { preferences : pref }
