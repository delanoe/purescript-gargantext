module Gargantext.Components.App (app) where

import Gargantext.Prelude

import Data.Tuple.Nested ((/\))
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Router (router)
import Gargantext.Hooks (useHashRouter)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Router as Router
import Gargantext.Sessions as Sessions
import Gargantext.Types (CacheParams, defaultCacheParams)
import Gargantext.Utils (getter)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.App"


app :: R2.Leaf ()
app = R2.leaf appCpt

appCpt :: R.Component ()
appCpt = here.component "container" cpt where
  cpt _ _ = do
    -- | States
    -- |

    cache' /\ cache <- R2.useBox' (defaultCacheParams :: CacheParams)

    -- | Hooks
    -- |

    -- load Local Storage cache (if exists)
    useFirstEffect' $
      R2.loadLocalStorageState R2.appParamsKey cache

    -- | Render
    -- |
    pure $

      hydrateStore
      { cacheParams: cache'
      }

--------------------------------------------------------------

type HydrateStoreProps =
  ( cacheParams :: CacheParams
  )

hydrateStore :: R2.Leaf HydrateStoreProps
hydrateStore = R2.leaf hydrateStoreCpt

hydrateStoreCpt :: R.Component HydrateStoreProps
hydrateStoreCpt = here.component "hydrateStore" cpt where
  cpt { cacheParams
      } _ = do
    -- | Computed
    -- |
    (state :: Record AppStore.State) <- pure $
      -- (cache options)
      { expandTableEdition: getter _.expandTableEdition cacheParams
      -- (default options)
      } `Record.merge` AppStore.options

    -- | Render
    -- |
    pure $

      AppStore.provide
      state
      [
        mainApp
        {}
      ]

--------------------------------------------------------------

mainApp :: R2.Leaf ()
mainApp = R2.leaf mainAppCpt

mainAppCpt :: R.Component ()
mainAppCpt = here.component "main" cpt where
  cpt _ _ = do
    boxes <- AppStore.use
    -- tasks   <- T.useBox Nothing             -- storage for asynchronous tasks reductor
    R.useEffectOnce' $ do
      void $ Sessions.load boxes.sessions
    -- tasks <- GAT.useTasks boxes.reloadRoot boxes.reloadForest
    R.useEffectOnce' $ do
      tasksStorage <- GAT.getAsyncTasks
      T.write_ tasksStorage boxes.tasks
    -- R.useEffectOnce' $ do
    --   T.write (Just tasksReductor) tasks
    R.useEffectOnce' $ do
      R2.loadLocalStorageState R2.openNodesKey boxes.forestOpen
      T.listen (R2.listenLocalStorageState R2.openNodesKey) boxes.forestOpen
    useHashRouter Router.router boxes.route -- Install router to window
    pure $ router { boxes }          -- Render router component
