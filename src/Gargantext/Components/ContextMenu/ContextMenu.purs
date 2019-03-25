-- | The ContextMeny component renders a generic context menu
-- | 
module Gargantext.Component.ContextMenu.ContextMenu
  (MenuProps, Action(..), separator) where


import Prelude hiding (div)
import Effect (Effect)
import React
  ( class ReactComponentSpec
  , ReactClass, ReactElement, ReactClassConstructor, Children
  , component, createElement )
import React.DOM (a, div, li, ul')
import React.DOM as DOM -- for Props
import React.DOM.Props (className, onContextMenu, onMouseOut, onBlur)
import Thermite (Render, PerformAction, simpleSpec, modifyState_,
                 createReactSpec, defaultRender)
import Gargantext.Utils.Reactil (wrap)

separator :: ReactElement
separator = div [ className "context-menu-separator" ] []

newtype State = State { open :: Boolean }

defaultState :: State
defaultState = State { open: false }

type MenuProps = { classes :: String }

data Action = Show | Hide

contextMenu :: MenuProps -> Array ReactElement -> ReactElement
contextMenu = createElement contextMenuClass

-- TODO: register callbacks
componentDidMount :: Effect Unit
componentDidMount = pure unit

-- TODO: unregister callbacks
componentWillUnmount :: Effect Unit
componentWillUnmount = pure unit

-- renderMenuItem :: Render State MenuItem Action MenuItem
-- renderMenuItem _ Separator _ _ = li [ className "menu-item-separator" ]
-- renderMenuItem d (MenuItem i) _ _ = wrap outer inner
--   where outer = li [ className "context-menu-item" ]
--         inner = a [ onClick callback, style i.style ] [text i.label]
--         callback _ = d Hide *> i.callback

-- TODO: Aria and accessibility
renderMenu :: Render State MenuProps Action
renderMenu d m s c = pure $ wrap outer $ ul' inner
  where outer = div [className (classes s.open m.classes)]
        inner = map (\i -> renderMenuItem d i ) c

classes :: Boolean -> String -> String
classes true user = "context-menu context-menu-shown " <> user
classes false user = "context-menu context-menu-hidden " <> user

-- Class

contextMenuClass :: ReactClass { children :: Children, open :: Boolean }
contextMenuClass = component "ContextMenu" createContextMenuClass

createContextMenuClass ::
  forall given snapshot spec.
  ReactComponentSpec MenuProps State snapshot given spec
    => ReactClassConstructor MenuProps State given
    -> ReactClass MenuProps
createContextMenuClass this = pure
  { state: defaultState
  , render: renderMenu
  , componentDidMount: componentDidMount
  , componentWillUnmount: componentWillUnmount
  }

