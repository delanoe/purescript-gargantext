module Gargantext.Users.Types.Types where

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe)
import DecodeMaybe ((.?|))
import Prelude (bind, pure, ($))

newtype User =
  User {
    id ::Int,
    typename :: Maybe Int,
    userId ::Int,
    parentId :: Int,
    name :: String,
    date ::Maybe String,
    hyperdata :: Maybe HyperData
       }

instance decodeUser :: DecodeJson User where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    typename <- obj .?| "typename"
    userId <- obj .? "userId"
    parentId <- obj .? "parentId"
    name <- obj .? "name"
    date <- obj .?| "date"
    hyperdata <- obj .?| "hyperdata"
    pure $ User {id, typename, userId, parentId, name, date, hyperdata}

newtype HyperData = HyperData (Map String String)

instance decodeHyperData :: DecodeJson HyperData where
  decodeJson json = do
    obj <- decodeJObject json
    pure <<< HyperData $ fromFoldable obj
