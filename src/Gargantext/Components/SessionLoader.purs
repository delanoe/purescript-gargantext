-- | A component that loads the session specified in the route and provides it to its child.
-- |
-- | If the session cannot be loaded, displays the homepage.
module Gargantext.Components.SessionLoader
where

import Data.Maybe (Maybe(..))
import Reactix as R
import Toestand as T

import Gargantext.Prelude

import Gargantext.Sessions as Sessions
import Gargantext.Sessions (Session, Sessions)
import Gargantext.Types (SessionId)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.SessionWrapper"

type Props =
  (
    fallback  :: R.Element
  , context   :: R.Context Session
  , sessionId :: SessionId
  , sessions  :: T.Cursor Sessions
  )

sessionWrapper :: R2.Component Props
sessionWrapper = R.createElement sessionWrapperCpt

sessionWrapperCpt :: R.Component Props
sessionWrapperCpt = here.component "sessionWrapper" cpt where
  cpt { fallback, context, sessionId, sessions } content = do
    sessions' <- T.useLive T.unequal sessions
    pure $ cp sessions'
    where
      cp sessions' = c $ Sessions.lookup sessionId sessions' where
        c (Just session) = (R.provideContext context session content)
        c Nothing = fallback
