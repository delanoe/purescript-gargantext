module Gargantext.Utils.EtherCalc where

import Affjax (Error, Response, defaultRequest, request)
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.HTTP.Method (Method(GET))
import Effect.Aff (Aff)
import Gargantext.Prelude

type Base = String
type NodeId = String

downloadCSV :: Base -> NodeId -> Aff (Either Error (Response String))
downloadCSV base nodeId = do
  let req = defaultRequest
            { url = base <> "/" <> nodeId <> ".csv"
            , responseFormat = ResponseFormat.string
            , method = Left GET }
  request req
  
