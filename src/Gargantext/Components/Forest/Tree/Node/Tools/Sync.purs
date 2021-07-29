module Gargantext.Components.Forest.Tree.Node.Tools.Sync where

import Gargantext.Prelude (Unit, bind, discard, pure, unit, ($), (<>), (==))
import Effect.Aff (Aff, launchAff_)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Reactix.DOM.HTML as H
import Reactix as R
import Toestand as T

import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Types as GT
import Gargantext.Sessions (Session)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Tools.Sync"


-- | Sync Node (Graph)
type NodeActionsGraphProps =
  ( id             :: GT.ID
  , graphVersions  :: Record GraphAPI.GraphVersions
  , session        :: Session
  , refresh :: Unit -> Aff Unit
  )

nodeActionsGraph :: R2.Component NodeActionsGraphProps
nodeActionsGraph = R.createElement nodeActionsGraphCpt
nodeActionsGraphCpt :: R.Component NodeActionsGraphProps
nodeActionsGraphCpt = here.component "nodeActionsGraph" cpt
  where
    cpt { id, graphVersions, session, refresh } _ = do
      pure $ H.div { className: "node-actions" } [
        if graphVersions.gv_graph == Just graphVersions.gv_repo then
          H.div {} []
        else
          graphUpdateButton { id, session, refresh }
      ]

type GraphUpdateButtonProps =
  ( id :: GT.ID
  , session :: Session
  , refresh :: Unit -> Aff Unit
  )

graphUpdateButton :: Record GraphUpdateButtonProps -> R.Element
graphUpdateButton p = R.createElement graphUpdateButtonCpt p []

graphUpdateButtonCpt :: R.Component GraphUpdateButtonProps
graphUpdateButtonCpt = here.component "graphUpdateButton" cpt
  where
    cpt { id, session, refresh } _ = do
      enabled <- T.useBox true
      enabled' <- T.useLive T.unequal enabled

      pure $ H.div { className: "update-button "
                   <> if enabled'
                         then "enabled"
                         else "disabled text-muted"
                   } [ H.span { className: "fa fa-refresh"
                     , on: { click: onClick enabled' enabled } } []
                     ]
      where
        onClick false _ = pure unit
        onClick true enabled = do
          launchAff_ $ do
            liftEffect $ T.write_ false enabled
            g <- GraphAPI.updateGraphVersions { graphId: id, session }
            liftEffect $ T.write_ true enabled
            refresh unit
          pure unit

-- | Sync Node (List)
type NodeActionsNodeListProps =
  (
    listId :: GT.ListId
  , nodeId :: GT.ID
  , nodeType :: GT.TabSubType GT.CTabNgramType
  , session :: Session
  , refresh :: Unit -> Aff Unit
  )

nodeActionsNodeList :: Record NodeActionsNodeListProps -> R.Element
nodeActionsNodeList p = R.createElement nodeActionsNodeListCpt p []

nodeActionsNodeListCpt :: R.Component NodeActionsNodeListProps
nodeActionsNodeListCpt = here.component "nodeActionsNodeList" cpt
  where
    cpt props _ = do
      pure $ H.div { className: "node-actions" } [
        nodeListUpdateButton props
      ]

type NodeListUpdateButtonProps =
  ( listId :: GT.ListId
  , nodeId :: GT.ID
  , nodeType :: GT.TabSubType GT.CTabNgramType
  , session :: Session
  , refresh :: Unit -> Aff Unit
  )

nodeListUpdateButton :: Record NodeListUpdateButtonProps -> R.Element
nodeListUpdateButton p = R.createElement nodeListUpdateButtonCpt p []

nodeListUpdateButtonCpt :: R.Component NodeListUpdateButtonProps
nodeListUpdateButtonCpt = here.component "nodeListUpdateButton" cpt
  where
    cpt { listId, nodeId, nodeType, session, refresh } _ = do
      -- enabled <- T.useBox true

      pure $ H.div {} [] {- { className: "update-button " 
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
            refresh unit
          pure unit
      -}
