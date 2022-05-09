module Gargantext.Utils.Toestand
  ( class Reloadable, reload, Reload, ReloadS, newReload, InitReload(..), ready, useMemberBox )
  where

import Prelude (class Ord, Unit, bind, pure, unit, (+))
import Data.Set as Set
import Data.Set (Set)
import Effect (Effect)
import Reactix as R
import Toestand as T

-- | Reload is a simple counter that can be used to force an update.
type Reload = Int
type ReloadS = T.Box Reload

class Reloadable t where
  reload :: t -> Effect Unit

-- | An empty Reload is zero as it has not yet been reloaded.
newReload :: Reload
newReload = 0

instance Reloadable (T.Box Int) where
  reload box = T.modify_ (_ + 1) box

instance Reloadable (c Reload) => Reloadable (T.Box (InitReload c)) where
  reload box = do
    val <- T.read box
    case val of
      Init    -> pure unit
      Ready r -> reload r

-- inner is a Box wrapping a Reload
data InitReload (inner :: Type -> Type) = Init | Ready (inner Reload)

-- | Initialises an InitReload box with the Reload box it contains,
-- | if it has not already been initialised.
ready :: forall box c. T.ReadWrite box (InitReload c) => T.ReadWrite (c Reload) Reload
      => box -> (c Reload) -> Effect Unit
ready box with = do
  val <- T.read box
  case val of
    Init    -> T.write_ (Ready with) box
    Ready _ -> pure unit

-- | Creates a cursor which presents a Boolean over whether the member
-- | is in the set. Adjusting the value will toggle whether the value
-- | is in the underlying set.
useMemberBox
  :: forall box v. Ord v => T.ReadWrite box (Set v)
  => v -> box -> R.Hooks (T.Box Boolean)
useMemberBox val box = T.useFocused (Set.member val) (toggleSet val) box

-- utility for useMemberBox
toggleSet :: forall s. Ord s => s -> Boolean -> Set s -> Set s
toggleSet val true  set = Set.insert val set
toggleSet val false set = Set.delete val set

