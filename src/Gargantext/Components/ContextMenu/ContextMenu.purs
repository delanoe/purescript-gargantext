-- | The ContextMenu component renders a generic context menu
module Gargantext.Components.ContextMenu.ContextMenu where
  -- (MenuProps, Action(..), separator) where

import Prelude hiding (div)
import Data.Maybe ( Maybe(..) )
import Data.Nullable ( Nullable, null, toMaybe )
import Data.Tuple ( Tuple(..) )
import Data.Tuple.Nested ( (/\) )
import Data.Traversable ( traverse_ )
import DOM.Simple as DOM
import DOM.Simple.Console
import DOM.Simple.Event as DE
import DOM.Simple.EventListener ( Callback, callback )
import DOM.Simple.Element as Element
import DOM.Simple.Window ( window )
import DOM.Simple.Document ( document )
import DOM.Simple.Document as Document
import DOM.Simple.Types ( DOMRect )
import Effect (Effect)
import Effect.Uncurried ( mkEffectFn1 )
import FFI.Simple ( (...), (..), delay )
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E

import Gargantext.Utils.Reactix as R'

type Props t = ( x :: Number, y :: Number, setMenu :: Maybe t -> Effect Unit)

getPortalHost :: R.Hooks DOM.Element
getPortalHost = R.unsafeHooksEffect $ delay unit $ \_ -> pure $ document ... "getElementById" $ ["menu-portal"]

contextMenu :: forall t. Record (Props t) -> Array R.Element -> R.Element
contextMenu = R.createElement contextMenuCpt

contextMenuCpt :: forall t. R.Component (Props t)
contextMenuCpt = R.hooksComponent "ContextMenu" cpt
  where
    cpt menu children = do
      host <- getPortalHost
      root <- R.useRef null
      rect /\ setRect <- R.useState $ \_ -> pure Nothing
      R.useLayoutEffect1 (R.readRef root) $ \_ -> do
        traverse_
          (\r -> setRect $ Just (Element.boundingRect r))
          (toMaybe $ R.readRef root)
        pure $ \_ -> pure unit
      R.useLayoutEffect2 root rect (contextMenuEffect menu.setMenu root)
      let cs = [ HTML.ul { className: "context-menu-items" } children ]
      pure $ R.createPortal [ elems root menu rect $ cs ] host
    elems ref menu (Just rect) = HTML.nav { ref , className: "context-menu", style: position menu rect}
    elems ref _ _ = HTML.nav { ref, className: "context-menu" }

contextMenuEffect
  :: forall t
  .  (Maybe t -> Effect Unit)
  -> R.Ref (Nullable DOM.Element)
  -> Unit -> Effect (Unit -> Effect Unit)
contextMenuEffect setMenu ref _ =
  case toMaybe $ R.readRef ref of
    Just elem -> do
      let onClick = documentClickHandler setMenu elem
      let onScroll = documentScrollHandler setMenu
      DOM.addEventListener document "click" onClick
      DOM.addEventListener document "scroll" onScroll
      pure $ \_ -> do
        DOM.removeEventListener document "click" onClick
        DOM.removeEventListener document "scroll" onScroll
    Nothing -> pure $ \_ -> pure unit
          
documentClickHandler :: forall t. (Maybe t -> Effect Unit) -> DOM.Element -> Callback DE.MouseEvent
documentClickHandler hide menu =
  R'.named "hideMenuOnClickOutside" $ callback $ \e ->
    if Element.contains menu (DE.target e)
      then pure unit
      else hide Nothing

documentScrollHandler :: forall t. (Maybe t -> Effect Unit) -> Callback DE.MouseEvent
documentScrollHandler hide =
  R'.named "hideMenuOnScroll" $ callback $ \e -> hide Nothing

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
contextMenuItemCpt = R.hooksComponent "ContextMenuItem" cpt
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

