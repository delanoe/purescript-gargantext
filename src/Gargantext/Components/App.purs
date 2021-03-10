module Gargantext.Components.App (app) where

import Data.Maybe (Maybe(..))
import Prelude
import Reactix as R
import Toestand as T

import Gargantext.Components.App.Data (emptyApp)
import Gargantext.Components.Router (router)
import Gargantext.Hooks (useHashRouter)
import Gargantext.Router as Router
import Gargantext.Sessions as Sessions
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.App"

app :: R2.Component ()
app = R.createElement appCpt

appCpt :: R.Component ()
appCpt = here.component "app" cpt where
  cpt _ _ = do
    cell    <- T.useCell emptyApp             -- global data
    cursors <- T.useFieldCursors cell {}      -- read-write access for children
    -- tasks   <- R.useRef Nothing               -- storage for asynchronous tasks
    tasks   <- T2.useCursed Nothing           -- storage for asynchronous tasks
    useHashRouter Router.router cursors.route -- Install router to window
    pure $ router { cursors, tasks }          -- Render router component
