module Gargantext.Hooks.Version
  ( useVersion
  , Version
  ) where

import Gargantext.Prelude

import DOM.Simple.Console (log2)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Config.REST as REST
import Gargantext.Ends (toUrl)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Sessions (Session(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Toestand as T

-- | (ie. Frontend Version)
foreign import version :: Version

type Version = String


type Input   = Maybe R_Input
type R_Input =
  { session :: Session
  }

type Output   = Maybe R_Output
type R_Output =
  { clientVersion   :: String
  , remoteVersion   :: String
  }

-- | Conditional Hooks checking release version match between client and remove
useVersion :: Input -> R.Hooks (Output)
useVersion mInput = do
  -- States
  mOutput /\ mOutputBox <- R2.useBox' (Nothing :: Output)
  -- Methods
  let
    exec :: Session -> Effect Unit
    exec session
        = launchAff_ $ getBackendVersion session
      >>= case _ of
            Left err -> liftEffect $ log2 "[version] error" err
            Right v  -> liftEffect $ flip T.write_ mOutputBox $ Just
              { clientVersion: version
              , remoteVersion: v
              }
  -- Hooks
  useFirstEffect' $ case mInput of
    Nothing          -> R.nothing
    Just { session } -> exec session
  -- Output
  pure mOutput

getBackendVersion :: Session -> REST.AffRESTError Version
getBackendVersion (Session { backend }) = REST.get Nothing (toUrl backend "version")
