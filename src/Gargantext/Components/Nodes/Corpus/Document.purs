module Gargantext.Components.Nodes.Corpus.Document
  ( node
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Document.API (loadData)
import Gargantext.Components.Document.Layout (layout)
import Gargantext.Components.Document.Types (LoadedData, DocPath)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Types (CTabNgramType(..), ListId, NodeID, TabSubType(..), TabType(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R

type Props =
  ( listId    :: ListId
  , mCorpusId :: Maybe NodeID
  , nodeId    :: NodeID
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Document"

node :: R2.Leaf ( key :: String | Props )
node = R2.leaf nodeCpt

nodeCpt :: R.Component ( key :: String | Props )
nodeCpt = here.component "node" cpt where
  cpt { listId
      , mCorpusId
      , nodeId
      } _ = do
    -- | States
    -- |
    session <- useSession

    state' /\ state <- R2.useBox' (Nothing :: Maybe LoadedData)

    -- | Computed
    -- |
    let
      tabType :: TabType
      tabType = TabDocument (TabNgramType CTabTerms)

      path :: DocPath
      path = { listIds: [listId], mCorpusId, nodeId, session, tabType }

    -- | Hooks
    -- |
    useLoaderEffect
      { errorHandler: logRESTError here "[documentLayoutWithKey]"
      , loader: loadData
      , path
      , state
      }

    -- | Render
    -- |
    pure $

      B.cloak
      { isDisplayed: isJust state'
      , idlingPhaseDuration: Just 150
      , cloakSlot:
          B.preloader
          {}

      , defaultSlot:
          R2.fromMaybe state' \loaded ->
            layout
            { loaded
            , path
            }
      }
