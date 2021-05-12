module Gargantext.Components.App (app) where
import Reactix as R
import Toestand as T

import Gargantext.Prelude

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Data (emptyApp)
import Gargantext.Components.Router (router)
import Gargantext.Hooks (useHashRouter)
import Gargantext.Router as Router
import Gargantext.Sessions as Sessions
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.App"

app :: R2.Component ()
app = R.createElement appCpt

appCpt :: R.Component ()
appCpt = here.component "app" cpt where
  cpt _ _ = do
    box    <- T.useBox emptyApp             -- global data
    boxes <- T.useFocusedFields box {}      -- read-write access for children
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
