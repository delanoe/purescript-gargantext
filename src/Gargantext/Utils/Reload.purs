module Gargantext.Utils.Reload where

import Gargantext.Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R
import Toestand as T

import Gargantext.Utils.Toestand as T2

type Reload  = Int
type ReloadS = R.State Reload
type ReloadSRef = R.Ref

new :: R.Hooks ReloadS
new = R.useState' 0

bump :: ReloadS -> Effect Unit
bump (_ /\ setReload) = setReload (_ + 1)

bumpCursor :: T.Cursor Reload -> Effect Unit
bumpCursor c = T2.modify_ (_ + 1) c

value :: ReloadS -> Reload
value (val /\ _) = val

-- a ReloadS ref that can be initialized later
data ReloadWithInitialize = Initialize | Ready ReloadS
type ReloadWithInitializeRef = R.Ref ReloadWithInitialize

newI :: R.Hooks ReloadWithInitializeRef
newI = R.useRef Initialize

newIInitialized :: ReloadS -> R.Hooks ReloadWithInitializeRef
newIInitialized reload = R.useRef $ Ready reload

initializeI :: ReloadWithInitializeRef -> ReloadS -> Effect Unit
initializeI ref reloadS = case R.readRef ref of
  Initialize -> R.setRef ref $ Ready reloadS
  Ready _    -> pure unit

bumpI :: ReloadWithInitializeRef -> Effect Unit
bumpI ref = case R.readRef ref of
  Initialize    -> pure unit
  Ready reload -> bump reload
