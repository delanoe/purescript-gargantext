module Gargantext.Components.Forest.Tree.Node.Tools.Sync where

import Effect.Aff (Aff, launchAff_)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Gargantext.Components.NgramsTable.API as NTAPI
import Effect.Class (liftEffect)
import Data.Tuple (fst)
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Types as GT
import Reactix.DOM.HTML as H
import Reactix as R
import Gargantext.Prelude (Unit, bind, const, discard, pure, unit, ($), (<>), (==))
import Gargantext.Types (ID)
import Gargantext.Sessions (Session)


-- | Sync Node (Graph)
type NodeActionsGraphProps =
  ( id             :: ID
  , graphVersions  :: Record GraphAPI.GraphVersions
  , session        :: Session
  , triggerRefresh :: Unit -> Aff Unit
  )

nodeActionsGraph :: Record NodeActionsGraphProps -> R.Element
nodeActionsGraph p = R.createElement nodeActionsGraphCpt p []

nodeActionsGraphCpt :: R.Component NodeActionsGraphProps
nodeActionsGraphCpt = R.hooksComponent "G.C.F.T.N.B.nodeActionsGraph" cpt
  where
    cpt { id, graphVersions, session, triggerRefresh } _ = do
      pure $ H.div { className: "node-actions" } [
        if graphVersions.gv_graph == Just graphVersions.gv_repo then
          H.div {} []
        else
          graphUpdateButton { id, session, triggerRefresh }
      ]

type GraphUpdateButtonProps =
  ( id :: ID
  , session :: Session
  , triggerRefresh :: Unit -> Aff Unit
  )

graphUpdateButton :: Record GraphUpdateButtonProps -> R.Element
graphUpdateButton p = R.createElement graphUpdateButtonCpt p []

graphUpdateButtonCpt :: R.Component GraphUpdateButtonProps
graphUpdateButtonCpt = R.hooksComponent "G.C.F.T.N.B.graphUpdateButton" cpt
  where
    cpt { id, session, triggerRefresh } _ = do
      enabled <- R.useState' true

      pure $ H.div { className: "update-button "
                   <> if (fst enabled)
                         then "enabled"
                         else "disabled text-muted"
                   } [ H.span { className: "fa fa-refresh"
                     , on: { click: onClick enabled } } []
                     ]
      where
        onClick (false /\ _) _ = pure unit
        onClick (true /\ setEnabled) _ = do
          launchAff_ $ do
            liftEffect $ setEnabled $ const false
            g <- GraphAPI.updateGraphVersions { graphId: id, session }
            liftEffect $ setEnabled $ const true
            triggerRefresh unit
          pure unit

-- | Sync Node (List)
type NodeActionsNodeListProps =
  (
    listId :: GT.ListId
  , nodeId :: ID
  , nodeType :: GT.TabSubType GT.CTabNgramType
  , session :: Session
  , triggerRefresh :: Unit -> Aff Unit
  )

nodeActionsNodeList :: Record NodeActionsNodeListProps -> R.Element
nodeActionsNodeList p = R.createElement nodeActionsNodeListCpt p []

nodeActionsNodeListCpt :: R.Component NodeActionsNodeListProps
nodeActionsNodeListCpt = R.hooksComponent "G.C.F.T.N.B.nodeActionsNodeList" cpt
  where
    cpt props _ = do
      pure $ H.div { className: "node-actions" } [
        nodeListUpdateButton props
      ]

type NodeListUpdateButtonProps =
  ( listId :: GT.ListId
  , nodeId :: ID
  , nodeType :: GT.TabSubType GT.CTabNgramType
  , session :: Session
  , triggerRefresh :: Unit -> Aff Unit
  )

nodeListUpdateButton :: Record NodeListUpdateButtonProps -> R.Element
nodeListUpdateButton p = R.createElement nodeListUpdateButtonCpt p []

nodeListUpdateButtonCpt :: R.Component NodeListUpdateButtonProps
nodeListUpdateButtonCpt = R.hooksComponent "G.C.F.T.N.B.nodeListUpdateButton" cpt
  where
    cpt { listId, nodeId, nodeType, session, triggerRefresh } _ = do
      enabled <- R.useState' true

      pure $ H.div { className: "update-button " 
                     <> if (fst enabled) then "enabled" else "disabled text-muted"
                   } [ H.span { className: "fa fa-refresh"
                     , on: { click: onClick enabled } } []
                     ]
      where
        onClick (false /\ _) _ = pure unit
        onClick (true /\ setEnabled) _ = do
          launchAff_ $ do
            liftEffect $ setEnabled $ const false
            _ <- NTAPI.updateNodeList { listId, nodeId, nodeType, session }
            liftEffect $ setEnabled $ const true
            triggerRefresh unit
          pure unit
