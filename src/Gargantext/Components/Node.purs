module Gargantext.Components.Node 
  where

import Gargantext.Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.?))

newtype NodePoly a =
  NodePoly { id :: Int
           , typename :: Int
           , userId   :: Int
           , parentId  :: Int
           , name      :: String
           , date      :: String
           , hyperdata :: a
           }


instance decodeNodePoly :: (DecodeJson a)
  => DecodeJson (NodePoly a) where
  decodeJson json = do
    obj <- decodeJson json
    id        <- obj .? "id"
    typename  <- obj .? "typename"
    userId    <- obj .? "userId"
    parentId  <- obj .? "parentId"
    name      <- obj .? "name"
    date      <- obj .? "date"

    hyperdata  <- obj .? "hyperdata"
    hyperdata' <- decodeJson hyperdata

    pure $ NodePoly  { id : id
                 , typename : typename
                 , userId   : userId
                 , parentId : parentId
                 , name     : name
                 , date     : date
                 , hyperdata: hyperdata'
                 }

newtype HyperdataList = HyperdataList { preferences :: String}

instance decodeHyperdataList :: DecodeJson HyperdataList where
  decodeJson json = do
    obj <- decodeJson json
    pref <- obj .? "preferences"
    pure $ HyperdataList { preferences : pref}

