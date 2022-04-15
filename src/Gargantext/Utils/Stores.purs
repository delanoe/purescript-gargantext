module Gargantext.Utils.Stores
  ( createStore
  , provideStore
  , useStore
  ) where

import Gargantext.Prelude

import Prim.RowList (class RowToList)
import Reactix as R
import Toestand as T
import Toestand.Records as TR

-- StoreFactory: R.Hook (Record Store) → create focus boxes, hydrate box values
-- StoreProvider: R.Hooks (Record Store) → provide context with previous boxes
--    ↓
-- StoreHook: R.Hooks (Record Store) → use context

-- | From state values to focused boxes
createStore :: forall boxes l state.
     RowToList state l
  => TR.UseFocusedFields l boxes () (T.Box (Record state))
  => Record state
  -> R.Hooks (Record boxes)
createStore = T.useBox >=> flip T.useFocusedFields {}

-- | Set <Store.Provider> bearing Stores as a Context
-- |
-- | (!) do not use store binds in this specific component (eg. `useStores`)
provideStore :: forall boxes l state.
     RowToList state l
  => TR.UseFocusedFields l boxes () (T.Box (Record state))
  => String
  -> Record state
  -> R.Context (Record boxes)
  -> Array R.Element
  -> R.Element
provideStore name state context
  = R.createElement (R.hooksComponent name cpt) {}
    where
      cpt _ children = do
        store <- createStore state
        pure $ R.provideContext context store $ children

-- | (?) As we use "React Provide API", we just want to rely on its Global
-- |     Reference (and not as Mutable State thanks to "Consumer API")
-- |     Hence the "unsafeCoerce" used on every provided context, avoiding
-- |     unwanted computing at each import
-- |
-- |     It also implies that every call to the proxy reference (made thanks to
-- |     below <Store.Provider> are made AFTER first mount of this very,
-- |     component, otherwise, every call will return the empty `unit`)
useStore :: forall boxes.
     R.Context (Record boxes)
  -> R.Hooks (Record boxes)
useStore = R.useContext
