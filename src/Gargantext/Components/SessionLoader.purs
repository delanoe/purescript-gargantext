-- | A component that loads the session specified in the route and provides it to its child.
-- |
-- | If the session cannot be loaded, displays the homepage.
module Gargantext.Components.SessionLoader
where

import Prelude (($), (<$>))
import Data.Maybe (Maybe(..))
import Reactix as R
import Toestand as T
import Gargantext.Sessions as Sessions
import Gargantext.Sessions (Session, Sessions)
import Gargantext.Types (SessionId)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.SessionWrapper"

type Props sessions =
  (
    fallback  :: R.Element
  , provider  :: R.Provider Session
  , sessionId :: SessionId
  , sessions  :: sessions
  )

sessionWrapper :: forall s. T.Read s Sessions => R2.Component (Props s)
sessionWrapper = R.createElement sessionWrapperCpt

sessionWrapperCpt :: forall s. T.Read s Sessions => R.Component (Props s)
sessionWrapperCpt = here.component "sessionWrapper" cpt where
  cpt { fallback, provider, sessionId, sessions } content =
    cp <$> T.useLive T.unequal sessions where
      cp sessions' = c $ Sessions.lookup sessionId sessions' where
        c (Just session) = (R.provide provider session content)
        c Nothing = fallback
