module Gargantext.Components.Loader where

import Prelude
import Data.Maybe (Maybe(..), maybe')
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R

import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Loader"

type Props path loaded =
  ( path  :: path
  , load  :: path -> Aff loaded
  , paint :: loaded -> R.Element )

loader :: forall path loaded. path
                            -> (path -> Aff loaded)
                            -> (loaded -> R.Element)
                            -> R.Element
loader path load paint =
  R.createElement loaderCpt {path,load,paint} []

loaderCpt :: forall path loaded. R.Component (Props path loaded)
loaderCpt = R2.hooksComponent thisModule "loader" cpt where
  cpt {path, load, paint} _ = do
    (loaded /\ setLoaded) <- R.useState' Nothing
    R.useEffect3 path load paint $ do
      R2.affEffect "G.H.Loader.useAff" $
        load path >>= (liftEffect <<< setLoaded <<< const <<< Just)
    pure $ maybe' (\_ -> loadingSpinner {}) paint loaded
