module Gargantext.Components.Nodes.Corpus.Document
  ( node
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector)
import Data.Maybe (Maybe(..), isJust, maybe)
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

    -- @XXX: reset "main-page__main-route" wrapper margin
    --       see Gargantext.Components.Router) (@TODO?)
    R.useLayoutEffect1 [] do
      let mEl = querySelector document ".main-page__main-route"
      -- Mount
      mEl >>= maybe R.nothing (flip R2.addClass ["p-0"])
      -- Unmount
      pure $
        mEl >>= maybe R.nothing (flip R2.removeClass ["p-0"])

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
