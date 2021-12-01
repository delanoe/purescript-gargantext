module Gargantext.Components.GraphQL.Endpoints where

import Gargantext.Components.GraphQL.Node
import Gargantext.Components.GraphQL.User
import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Unit (unit)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Components.GraphQL (getClient, queryGql)
import Gargantext.Components.GraphQL.Task as GQLT
import Gargantext.Config.REST (AffRESTError, RESTError(..))
import Gargantext.Sessions (Session)
import Gargantext.Types (AsyncTaskWithType(..), AsyncTask(..), AsyncTaskType(..), NodeType)
import Gargantext.Utils.Reactix as R2
import GraphQL.Client.Args (onlyArgs)
import GraphQL.Client.Query (mutation)
import GraphQL.Client.Variables (withVars)
import Simple.JSON as JSON

here :: R2.Here
here = R2.here "Gargantext.Components.GraphQL.Endpoints"

getNodeParent :: Session -> Int -> NodeType -> Aff (Array Node)
getNodeParent session nodeId parentType = do
  { node_parent } <- queryGql session "get node parent" $
                     nodeParentQuery `withVars` { id: nodeId
                                                , parent_type: show parentType }  -- TODO: remove "show"
  liftEffect $ here.log2 "[getNodeParent] node_parent" node_parent
  pure $ node_parent

getUserInfo :: Session -> Int -> AffRESTError UserInfo
getUserInfo session id = do
  { user_infos } <- queryGql session "get user infos" $ userInfoQuery `withVars` { id }
  liftEffect $ here.log2 "[getUserInfo] user infos" user_infos
  pure $ case A.head user_infos of
    Nothing -> Left (CustomError $ "user with id " <> show id <> " not found")
    -- NOTE Contact is at G.C.N.A.U.C.Types
    Just ui -> Right ui

triggerEthercalcCSVDownload :: Session -> Int -> Int -> Aff (Maybe AsyncTaskWithType)
triggerEthercalcCSVDownload session corpusId nodeId = do
  client <- liftEffect $ getClient session
  res <- mutation
    client
    "trigger ethercalc CSV download"
    { ethercalc_csv_download: onlyArgs { corpusId
                                       , nodeId } }
  pure Nothing
--  pure $ case res.ethercalc_csv_download of
--    Nothing -> Nothing
--    Just { task: { id, status }, typ } ->
--      case JSON.readJSON typ of
--        Left _ -> Nothing
--        Right typ_ ->
--          case JSON.readJSON status of
--            Left _ -> Nothing
--            Right status_ ->
--              Just $ AsyncTaskWithType { task: AsyncTask { id, status: status_ }
--                                       , typ: typ_ }

