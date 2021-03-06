-- | The SimpleItem is a simple context menu item consisting of a link
-- | It handles automatically closing the context menu for you
module Gargantext.Component.ContextMenu.SimpleItem where
  -- (MenuProps, Action(..), separator) where

import Prelude hiding (div)

-- separator :: ReactElement
-- separator = div [ className "context-menu-separator" ] []

-- type State' = { open :: Boolean }
-- newtype State = State State'

-- defaultState :: State
-- defaultState = State { open: false }

-- type MenuProps = { classes :: String }
-- type ItemProps p = { hideMenu :: Effect () | p }

-- data Action = Show | Hide

-- menuClass :: String
-- menuClass = "context-menu"

-- menuShownClass :: String
-- menuShownClass = "context-menu-shown"

-- menuHiddenClass :: String
-- menuHiddenClass = "context-menu-hidden"

-- itemClass :: String
-- itemClass = "context-menu-item"

-- contextMenu :: MenuProps -> Array ReactElement -> ReactElement
-- contextMenu = createElement contextMenuClass

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

-- renderMenuItem :: Render State MenuItem Action MenuItem
-- renderMenuItem _ Separator _ _ = li [ className "menu-item-separator" ]
-- renderMenuItem d (MenuItem i) _ _ = wrap outer inner
--   where outer = li [ className "context-menu-item" ]
--         inner = a [ onClick callback, style i.style ] [text i.label]
--         callback _ = d Hide *> i.callback

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

-- simpleItem :: Label -> ClassName -> Effect Unit -> Effect Unit -> ReactElement
-- simpleItem label cls cb hide = a [ onClick (hide *> cb), className cls ] [ text label ]

-- separator :: ReactElement
-- separator = li [ className "menu-item-separator" ] []
