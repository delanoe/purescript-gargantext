module Gargantext.Components.Forest.Tree.Node.Action.Utils where

import Data.Array as A
import Data.Map as Map
import Gargantext.Components.GraphQL.Endpoints (getLanguages)
import Gargantext.Components.Lang (Lang)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Prelude
import Gargantext.Sessions (Session)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Utils"


loadLanguages :: { session :: Session } -> AffRESTError (Array Lang)
loadLanguages { session } = do
  eLangsMap <- getLanguages session
  pure $ A.fromFoldable <$> Map.keys <$> eLangsMap
