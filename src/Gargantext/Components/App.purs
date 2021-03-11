module Gargantext.Components.App (app) where

import Data.Maybe (Maybe(..))
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
    tasks   <- T.useBox Nothing             -- storage for asynchronous tasks reductor
    R.useEffectOnce' $ do
      void $ Sessions.load boxes.sessions
    tasksReductor <- GAT.useTasks boxes.reloadRoot boxes.reloadForest
    R.useEffectOnce' $ do
      T.write (Just tasksReductor) tasks
    useHashRouter Router.router boxes.route -- Install router to window
    pure $ router { boxes, tasks }          -- Render router component
