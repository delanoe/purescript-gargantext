module Gargantext.Components.App (app) where

import Prelude
import Gargantext.Components.App.Data (emptyApp)
import Gargantext.Components.Router (router)
import Gargantext.Hooks (useHashRouter)
import Gargantext.Router as Router
import Gargantext.Sessions as Sessions
import Reactix as R
import Toestand as T

thisModule :: String
thisModule = "Gargantext.Components.App"

app :: R.Element
app = R.createElement appCpt {} []

appCpt :: R.Component ()
appCpt = R.hooksComponentWithModule thisModule "app" cpt where
  cpt _ _ = do
    cell    <- T.useCell emptyApp     -- global data
    views   <- T.useFieldViews cell   -- read-only access for children
    cursors <- T.useFieldCursors cell -- read-write access for children
    tasks   <- R.useRef Nothing       -- storage for asynchronous tasks
    useHashRouter Router.router cursors.route -- Install router to window
    pure $ router { views, cursors, tasks }   -- Render router component
