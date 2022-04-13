module Gargantext.Hooks.Resize
  ( useResizeHandler
  , ResizeType(..)
  ) where

import Gargantext.Prelude

import DOM.Simple (Document, Window, window, document)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Extra (kebabCase)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn5, runEffectFn2, runEffectFn5)
import Reactix as R


foreign import _add :: EffectFn5
  Window
  Document
  String
  String
  String
  Unit

add ::
     Window
  -> Document
  -> String
  -> String
  -> String
  -> Effect Unit
add = runEffectFn5 _add


foreign import _remove :: EffectFn2
  Document
  String
  Unit

remove ::
     Document
  -> String
  -> Effect Unit
remove = runEffectFn2 _remove

-- @XXX: React.ref element clunky design
--       Using string selector query instead...
type Output =
  ( add     :: String -> String -> ResizeType -> Effect Unit
  , remove  :: String -> Effect Unit
  )

useResizeHandler :: R.Hooks (Record Output)
useResizeHandler = pure
  { add: \source target t ->
      add window document source target (show t)
  , remove: remove document
  }

-----------------------------------------------


data ResizeType
  = Vertical
  | Horizontal
  | Both

derive instance Generic ResizeType _
derive instance Eq ResizeType
instance Show ResizeType where
  show = kebabCase <<< genericShow
