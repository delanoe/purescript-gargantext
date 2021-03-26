-- | The ContextMenu component renders a generic context menu
module Gargantext.Components.ContextMenu.ContextMenu where
  -- (MenuProps, Action(..), separator) where

import Data.Maybe ( Maybe(..) )
import Data.Nullable ( Nullable, null, toMaybe )
import Data.Tuple.Nested ( (/\) )
import Data.Traversable ( traverse_ )
import DOM.Simple as DOM
import DOM.Simple.Event as DE
import DOM.Simple.EventListener ( Callback, callback )
import DOM.Simple.Element as Element
import DOM.Simple.Window ( window )
import DOM.Simple.Document ( document )
import DOM.Simple.Types ( DOMRect )
import Effect (Effect)
import FFI.Simple ((..))
import Reactix as R
import Reactix.DOM.HTML as HTML
import Toestand as T

import Gargantext.Prelude

import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.ContextMenu.ContextMenu"

type Props t = (
    onClose :: Effect Unit
  , x :: Number
  , y :: Number
  )

contextMenu :: forall t. R2.Component (Props t)
contextMenu = R.createElement contextMenuCpt

contextMenuCpt :: forall t. R.Component (Props t)
contextMenuCpt = here.component "contextMenu" cpt
  where
    cpt menu@{ x, y, onClose } children = do
      host <- R2.getPortalHost
      root <- R.useRef null
      rect <- T.useBox Nothing
      rect' <- T.useLive T.unequal rect

      R.useLayoutEffect1 (R.readRef root) $ do
        traverse_
          (\r -> T.write_ (Just (Element.boundingRect r)) rect)
          (toMaybe $ R.readRef root)
        pure $ pure unit
      R.useLayoutEffect2 root rect (contextMenuEffect onClose root)
      let cs = [
            HTML.div { className: "popover-content" }
            [ HTML.div { className: "card" }
              [ HTML.ul { className: "list-group" }
                children
              ]
            ]
      ]
      pure $ R.createPortal [ elems root menu rect' $ cs ] host
    elems ref menu (Just rect) = HTML.div
        { ref
        , key: "context-menu"
        , className: "context-menu"
        , style: position menu rect
        , data: {toggle: "popover", placement: "right"}
        }
    elems ref _ _ = HTML.div
        { ref
        , key: "context-menu"
        , className: "context-menu"
        , data: {toggle: "popover", placement: "right"}
        }

contextMenuEffect
  :: forall t.
     Effect Unit
  -> R.Ref (Nullable DOM.Element)
  -> Effect (Effect Unit)
contextMenuEffect onClose rootRef =
  case R.readNullableRef rootRef of
    Just root -> do
      let onClick = documentClickHandler onClose root
      let onScroll = documentScrollHandler onClose
      DOM.addEventListener document "click" onClick
      DOM.addEventListener document "scroll" onScroll
      pure $ do
        DOM.removeEventListener document "click" onClick
        DOM.removeEventListener document "scroll" onScroll
    Nothing -> pure R.nothing

documentClickHandler :: Effect Unit -> DOM.Element -> Callback DE.MouseEvent
documentClickHandler onClose menu =
  R2.named "hideMenuOnClickOutside" $ callback $ \e ->
    when (Element.contains menu (DE.target e)) onClose

documentScrollHandler :: Effect Unit -> Callback DE.MouseEvent
documentScrollHandler onClose =
  R2.named "hideMenuOnScroll" $ callback $ \e -> onClose

position :: forall t. Record (Props t) -> DOMRect -> { left :: Number, top :: Number }
position mouse {width: menuWidth, height: menuHeight} = {left, top}
  where left = if isRight then mouse.x else mouse.x - menuWidth
        top = if isAbove then mouse.y else mouse.y - menuHeight
        isRight = screenWidth - mouse.x > menuWidth -- is there enough space to show above
        isAbove = screenHeight - mouse.y > menuHeight -- is there enough space to show to the right?
        screenWidth = window .. "innerWidth"
        screenHeight = window .. "innerHeight"

contextMenuItem :: Array R.Element -> R.Element
contextMenuItem = R.createElement contextMenuItemCpt {}

contextMenuItemCpt :: R.Component ()
contextMenuItemCpt = here.component "contextMenuItem" cpt
  where
    cpt _props children = pure $ HTML.li { className: "context-menu-item" } children

-- -- CSS Classes

-- menuClass :: String
-- menuClass = "context-menu"

-- menuShownClass :: String
-- menuShownClass = "context-menu-shown"

-- menuHiddenClass :: String
-- menuHiddenClass = "context-menu-hidden"

-- itemClass :: String
-- itemClass = "context-menu-item"

-- separatorClass :: String
-- separatorClass = "context-menu-item"

