module Gargantext.Pages.Layout.Specs.AddCorpus.States where

import Prelude hiding (div)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))

type State =
  { select_database :: Boolean
  , unselect_database :: Boolean  --  dummy state
  , response :: Array Response
  }

newtype Response = Response
  {
    count :: Int
  , name :: String
  }

instance decodeJsonresponse :: DecodeJson Response where
  decodeJson json = do
    obj   <- decodeJson json
    count <- obj .? "count"
    name  <- obj .? "name"
    pure $ Response {count,name }

initialState :: State
initialState =
  {
    select_database   : true
  , unselect_database : true
  , response : []
  }



