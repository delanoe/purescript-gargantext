module Gargantext.Hooks.StateRecord.Unboxed
  ( useStateRecord
  , useStateRecord'
  ) where

import Gargantext.Prelude

import Data.Eq (class EqRecord)
import Effect (Effect)
import Gargantext.Hooks.StateRecord.Behaviors (TwoWayBinding, binder, setter)
import Gargantext.Utils.Reactix as R2
import Prim.RowList (class RowToList)
import Reactix as R
import Toestand as T

-- @XXX StateRecord with distinct value types → eg. conflicts when "string" and
--      "boolean" are coexisting as value on the same hook
--        ↳ workaround is to manually bind the input (copying `setStateKey`
--          method)

type Methods r a =
  -- | Every provided props will be available within the `formFields` proxy
  ( state         ::        Record r
  , stateBox      :: T.Box (Record r)
  -- | When binded with a form input, any form fields can be handled by
  -- | providing this function
  , setStateKey   :: String -> a -> Effect Unit
  -- | API method proposing a two way data binding (such as "v-model" of
  -- | VueJS)
  , bindStateKey  :: String -> Record (TwoWayBinding a)
  )

-- | Hooks inspired from this article
-- |
-- | https://blog.logrocket.com/forms-in-react-in-2020/
-- |
-- | ```purescript
-- |
-- |  r <- useStateRecord defaultValues
-- |
-- | ...
-- |
-- | B.formInput
-- |   { value: r.state.prop
-- |   , callback: r.setStateKey "prop"
-- |   }
-- |
-- | ...
-- |
-- | -- `bindStateKey` will add both `value: ...` and `callback: ...`
-- | B.formInput $
-- |   { ... } `merge` r.bindStateKey "prop"
-- |
useStateRecord :: forall a r l.
     RowToList r l
  => EqRecord l r
  => Record r
  -> R.Hooks (Record (Methods r a))
useStateRecord = T.useBox >=> main

-- | Variant where `stateBox :: Box (Record r)` is already instanciated and
-- | provided
useStateRecord' :: forall a r l.
     T.ReadWrite (T.Box (Record r)) (Record r)
  => RowToList r l
  => EqRecord l r
  => T.Box (Record r)
  -> R.Hooks (Record (Methods r a))
useStateRecord' = main


main :: forall a r l.
     T.ReadWrite (T.Box (Record r)) (Record r)
  => RowToList r l
  => EqRecord l r
  => T.Box (Record r)
  -> R.Hooks (Record (Methods r a))
main stateBox = do

  state <- R2.useLive' stateBox

  pure
    { state
    , stateBox
    , setStateKey : setter stateBox -- (_ # stateBox # setter)
    , bindStateKey: binder stateBox state -- (_ # stateBox # binder $ state)
    }
