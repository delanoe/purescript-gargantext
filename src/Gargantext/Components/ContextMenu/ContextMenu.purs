-- | The ContextMenu component renders a generic context menu
module Gargantext.Components.ContextMenu.ContextMenu where
  -- (MenuProps, Action(..), separator) where

import Prelude hiding (div)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.Raw as RDOM

import Gargantext.Utils.Reactix as R'

contextMenu :: Array R.Element -> R.Element
contextMenu = R.createElement contextMenuCpt {}

contextMenuCpt :: R.Component ()
contextMenuCpt = R.hooksComponent "ContextMenu" cpt
  where
    cpt _props children = pure $
      R'.nav { className: "context-menu" }
        [ R'.ul { className: "context-menu-items" } children ]

contextMenuItem :: Array R.Element -> R.Element
contextMenuItem = R.createElement contextMenuItemCpt {}

contextMenuItemCpt :: R.Component ()
contextMenuItemCpt = R.hooksComponent "ContextMenuItem" cpt
  where
    cpt _props children = pure $ R'.li { className: "context-menu-item" } children


-- data Action = Show | Hide


-- contextMenu :: MenuProps -> ReactElement
-- contextMenu props = createElement contextMenuClass props []

-- -- TODO: register callbacks
-- componentDidMount :: Effect Unit
-- componentDidMount = pure unit

-- -- TODO: unregister callbacks
-- componentWillUnmount :: Effect Unit
-- componentWillUnmount = pure unit

-- -- 
-- childRender :: forall s p a. Spec s p a -> Spec s p a
-- childRender = over _render (\c -> wrapItem <<< c)

-- -- | Wraps an item in an li tag with the item classname
-- wrapItem :: ReactElement -> ReactElement
-- wrapItem = wrap $ li [ className itemClass ]

-- -- TODO: Aria and accessibility
-- renderMenu :: Render State MenuProps Action
-- renderMenu d m s c = pure $ wrap outer $ ul' inner
--   where outer = div [className (classes s.open m.classes)]
--         inner = map (\i -> renderMenuItem d i ) c

-- visibilityClass :: Boolean -> String
-- visibilityClass true = contextMenuShown
-- visibilityClass false = contextMenuHidden

-- classes :: Boolean -> String -> String
-- classes visible user = joinWith " " [menuClass, visibilityClass visible, user]

-- -- Class

-- contextMenuClass :: ReactClass (WithChildren State')
-- contextMenuClass = component "ContextMenu" createContextMenuClass

-- createContextMenuClass ::
--   forall given snapshot spec.
--   ReactComponentSpec MenuProps State snapshot given spec
--     => ReactClassConstructor MenuProps State given
--     -> ReactClass MenuProps
-- createContextMenuClass this = pure
--   { state: defaultState
--   , render: renderMenu
--   , componentDidMount: componentDidMount
--   , componentWillUnmount: componentWillUnmount
--   }

-- type Label = String
-- type ClassName = String

-- -- Items

-- simpleItem :: Label -> ClassName -> Effect Unit -> ContextConsumer (Effect Unit) -> ReactElement
-- simpleItem label cls cb hide = a [ onClick (hide *> cb), className cls ] [ text label ]

-- separator :: Effect Unit -> ReactElement
-- separator _ = li [ className "menu-item-separator" ] []



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

