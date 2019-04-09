module Gargantext.Utils where

import Prelude
import Data.Ordering (Ordering(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Lens (Lens', lens)

setterv :: forall nt record field. Newtype nt record => (record -> field -> record) -> field -> nt -> nt
setterv fn v t = (setter (flip fn v) t)

setter :: forall nt record. Newtype nt record => (record -> record) -> nt -> nt
setter fn = wrap <<< fn <<< unwrap

getter :: forall record field nt. Newtype nt record => (record -> field) -> nt -> field
getter fn = fn <<< unwrap

-- Default sort order is ascending, we may want descending
invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering GT = LT
invertOrdering EQ = EQ

-- A lens that always returns unit
_unit :: forall s. Lens' s Unit
_unit = lens (\_ -> unit) (\s _ -> s)
