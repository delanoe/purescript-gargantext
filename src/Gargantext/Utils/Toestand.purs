module Gargantext.Utils.Toestand
  ( class Reloadable, reload
  , Reload, newReload, InitReload(..), ready
  , useCursed, useIdentityCursor, useMemberCursor
  ) where

import Prelude (class Ord, Unit, bind, identity, pure, unit, void, ($), (+), (>>=))
import Data.Set as Set
import Data.Set (Set)
import Effect (Effect)
import Reactix as R
import Toestand as T

-- | Reload is a simple counter that can be used to force an update.
type Reload = Int

class Reloadable t where
  reload :: t -> Effect Unit

-- | An empty Reload is zero as it has not yet been reloaded.
newReload :: Reload
newReload = 0

instance reloadableCellReload :: Reloadable (T.Cell Int) where
  reload cell = void $ T.modify (_ + 1) cell

instance reloadableCursorReload :: Reloadable (T.Cursor Int) where
  reload cell = void $ T.modify (_ + 1) cell

instance reloadableInitReloadCell :: Reloadable (c Reload) => Reloadable (T.Cell (InitReload c)) where
  reload cell = do
    val <- T.read cell
    case val of
      Init    -> pure unit
      Ready r -> reload r

instance reloadableInitReloadCursor :: Reloadable (c Reload) => Reloadable (T.Cursor (InitReload c)) where
  reload cell = do
    val <- T.read cell
    case val of
      Init    -> pure unit
      Ready r -> reload r

-- c is a cell or cursor wrapping a Reload
data InitReload (c :: Type -> Type) = Init | Ready (c Reload)

-- | Initialises an InitReload cell with the Reload cell it contains,
-- | if it has not already been initialised.
ready :: forall cell c. T.ReadWrite cell (InitReload c) => T.ReadWrite (c Reload) Reload
      => cell -> (c Reload) -> Effect Unit
ready cell with = do
  val <- T.read cell
  case val of
    Init    -> void $ T.write (Ready with) cell
    Ready _ -> pure unit

-- | Turns a Cell into a Cursor.
useIdentityCursor :: forall cell c. T.ReadWrite cell c => cell -> R.Hooks (T.Cursor c)
useIdentityCursor = T.useCursor identity (\a _ -> a)

-- | Creates a cursor directly from a value by creating a cell first.
useCursed :: forall t. t -> R.Hooks (T.Cursor t)
useCursed val = T.useCell val >>= useIdentityCursor

-- | Creates a cursor which presents a Boolean over whether the member
-- | is in the set. Adjusting the value will toggle whether the value
-- | is in the underlying set.
useMemberCursor
  :: forall cell v. Ord v => T.ReadWrite cell (Set v)
  => v -> cell -> R.Hooks (T.Cursor Boolean)
useMemberCursor val cell = T.useCursor (Set.member val) (toggleSet val) cell

-- utility for useMemberCursor
toggleSet :: forall s. Ord s => s -> Boolean -> Set s -> Set s
toggleSet val true  set = Set.insert val set
toggleSet val false set = Set.delete val set
