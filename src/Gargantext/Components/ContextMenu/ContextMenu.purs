-- | The ContextMenu component renders a generic context menu
module Gargantext.Components.ContextMenu.ContextMenu where

-- (MenuProps, Action(..), separator) where
import Prelude hiding (div)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.Tuple.Nested ((/\))
import Data.Traversable (traverse_)
import DOM.Simple as DOM
import DOM.Simple.Event as DE
import DOM.Simple.EventListener (Callback, callback)
import DOM.Simple.Element as Element
import DOM.Simple.Window (window)
import DOM.Simple.Document (document)
import DOM.Simple.Types (DOMRect)
import Effect (Effect)
import FFI.Simple ((..))
import Reactix as R
import Reactix.DOM.HTML as HTML
import Gargantext.Utils.Reactix as R2

type Props t
  = ( x :: Number, y :: Number, setMenu :: R2.Setter (Maybe t) )

contextMenu :: forall t. Record (Props t) -> Array R.Element -> R.Element
contextMenu = R.createElement contextMenuCpt

contextMenuCpt :: forall t. R.Component (Props t)
contextMenuCpt = R.hooksComponent "ContextMenu" cpt
  where
  cpt menu children = do
    host <- R2.getPortalHost
    root <- R.useRef null
    rect /\ setRect <- R.useState $ \_ -> Nothing
    R.useLayoutEffect1 (R.readRef root)
      $ do
          traverse_
            (\r -> setRect (\_ -> Just (Element.boundingRect r)))
            (toMaybe $ R.readRef root)
          pure $ pure unit
    R.useLayoutEffect2 root rect (contextMenuEffect menu.setMenu root)
    let
      cs =
        [ HTML.div { className: "popover-content" }
            [ HTML.div { className: "panel panel-default" }
                [ HTML.ul { className: "list-group" }
                    children
                ]
            ]
        ]
    pure $ R.createPortal [ elems root menu rect $ cs ] host

  elems ref menu (Just rect) =
    HTML.div
      { ref
      , className: "context-menu"
      , style: position menu rect
      , data: { toggle: "popover", placement: "right" }
      }

  elems ref _ _ =
    HTML.div
      { ref
      , className: "context-menu"
      , data: { toggle: "popover", placement: "right" }
      }

contextMenuEffect ::
  forall t.
  R2.Setter (Maybe t) ->
  R.Ref (Nullable DOM.Element) ->
  Effect (Effect Unit)
contextMenuEffect setMenu rootRef = case R.readNullableRef rootRef of
  Just root -> do
    let
      onClick = documentClickHandler setMenu root
    let
      onScroll = documentScrollHandler setMenu
    DOM.addEventListener document "click" onClick
    DOM.addEventListener document "scroll" onScroll
    pure
      $ do
          DOM.removeEventListener document "click" onClick
          DOM.removeEventListener document "scroll" onScroll
  Nothing -> pure R.nothing

documentClickHandler :: forall t. R2.Setter (Maybe t) -> DOM.Element -> Callback DE.MouseEvent
documentClickHandler hide menu =
  R2.named "hideMenuOnClickOutside" $ callback
    $ \e ->
        if Element.contains menu (DE.target e) then
          pure unit
        else
          hide (const Nothing)

documentScrollHandler :: forall t. R2.Setter (Maybe t) -> Callback DE.MouseEvent
documentScrollHandler hide = R2.named "hideMenuOnScroll" $ callback $ \e -> hide (const Nothing)

position :: forall t. Record (Props t) -> DOMRect -> { left :: Number, top :: Number }
position mouse { width: menuWidth, height: menuHeight } = { left, top }
  where
  left = if isRight then mouse.x else mouse.x - menuWidth

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
 -- -- CSS Classes -- menuClass :: String -- menuClass = "context-menu" -- menuShownClass :: String -- menuShownClass = "context-menu-shown" -- menuHiddenClass :: String -- menuHiddenClass = "context-menu-hidden" -- itemClass :: String -- itemClass = "context-menu-item" -- separatorClass :: String -- separatorClass = "context-menu-item"