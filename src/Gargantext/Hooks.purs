module Gargantext.Hooks ( useHashRouter ) where

import Prelude (Unit, void, ($))
import Reactix as R
import Routing.Match (Match)
import Routing.Hash (matches)
import Toestand as T

-- | Sets up the hash router so it writes the route to the given cell.
-- | Note: if it gets sent to an unrecognised url, it will quietly
-- | drop the change.
useHashRouter :: forall r c. T.Write c r => Match r -> c -> R.Hooks Unit
useHashRouter routes cell = R.useEffectOnce $ matches routes h where
  h _old new = void $ T.write new cell

-- useSession cell =
