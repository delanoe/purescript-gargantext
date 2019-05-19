module Gargantext.Utils.Selection where

import Prelude
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import DOM.Simple.Types (Element, DOMRect)
import DOM.Simple.Element as Element
import Effect (Effect)
import FFI.Simple ((.?), (..), (...))

-- | Represents a text selection
foreign import data Selection :: Type
-- | Represents a single selection range
foreign import data Range :: Type

-- Terminology:
  -- Anchor: point at which the selection was started
  -- Focus: point at which the selection ends

-- | The Node in which the anchor lies
anchorNode :: Selection -> Maybe Element
anchorNode s = s .? "anchorNode"

-- | The Node in which the focus lies
focusNode :: Selection -> Maybe Element
focusNode s = s .? "focusNode"

-- | Whether the anchor and focus are at the same point
isSelectionCollapsed :: Selection -> Boolean
isSelectionCollapsed s = s .. "isCollapsed"

rangeCount :: Selection -> Int
rangeCount s = s .. "rangeCount"

getRange :: Selection -> Int -> Effect Range
getRange s i = pure $ s ... "getRangeAt" $ [i]

-- | Renders a selection or range as a string
selectionToString :: Selection -> String
selectionToString s = s ... "toString" $ []

-- | Renders a range as a string
rangeToString :: Range -> String
rangeToString s = s ... "toString" $ []

-- | Whether the anchor and focus are at the same point
isRangeCollapsed :: Range -> Boolean
isRangeCollapsed r = r .. "isCollapsed"

cloneRange :: Range -> Range
cloneRange r = r ... "cloneRange" $ []

collapseRange :: Range -> Boolean -> Effect Unit
collapseRange r toStart = pure $ r ... "collapse" $ [toStart]

commonAncestorContainer :: Range -> Element
commonAncestorContainer r = r .. "commonAncestorContainer"

insertNode :: Range -> Element -> Effect Unit
insertNode r e = pure $ r ... "insertNode" $ [e]

boundingRect :: Range -> DOMRect
boundingRect r = r ... "getBoundingClientRect" $ []

-- getSelection

-- | Fetches the current text selection, if any
getSelection :: Effect (Maybe Selection)
getSelection = toMaybe <$> _getSelection

foreign import _getSelection :: Effect (Nullable Selection)

-- | Are both the start and end of the selection contained within an Element
doesSelectionLieWithin :: Selection -> Element -> Boolean
doesSelectionLieWithin sel elem = test anchorNode && test focusNode
  where
    test :: (Selection -> Maybe Element) -> Boolean
    test f = maybe false (Element.contains elem) (f sel)
