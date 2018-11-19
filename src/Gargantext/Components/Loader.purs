module Gargantext.Components.Loader where

import Control.Monad.Cont.Trans (lift)
import Data.Maybe (Maybe(..))
import React as React
import React (ReactClass, Children)
import Gargantext.Prelude
import Effect.Aff (Aff)

import Thermite (Render, PerformAction, simpleSpec, modifyState_, createReactSpec)

type InnerProps a b =
  { path     :: a
  , loaded   :: Maybe b
  , children :: Children
  }

type PropsRow a b c =
  ( path      :: a
  , component :: ReactClass (InnerProps a b)
  | c
  )

type Props a b = Record (PropsRow a b (children :: Children))

type Props' a b = Record (PropsRow a b ())

data Action a = ForceReload | SetPath a

type State a b = { currentPath :: a, loaded :: Maybe b }

createLoaderClass :: forall a b
                   . Eq a
                  => String
                  -> (a -> Aff b)
                  -> ReactClass (Record (PropsRow a b (children :: Children)))
createLoaderClass name loader =
  React.component name
    (\this -> do
       s <- spec this
       pure { state: s.state
            , render: s.render
            , componentDidMount: dispatcher this ForceReload
            })
  where
    initialState {path} = {currentPath: path, loaded: Nothing}

    performAction :: PerformAction (State a b) (Props' a b) (Action a)
    performAction ForceReload _ {currentPath} = do
      loaded <- lift $ loader currentPath
      modifyState_ $ _ { loaded = Just loaded }
    performAction (SetPath newPath) _ {currentPath} =
      when (newPath /= currentPath) $ do
        loaded <- lift $ loader newPath
        modifyState_ $ _ { currentPath = newPath, loaded = Just loaded }

    render :: Render (State a b) (Props' a b) (Action a)
    render _d {component} {currentPath, loaded} c =
      [React.createElement component {path: currentPath, loaded} c]

    {spec, dispatcher} = createReactSpec (simpleSpec performAction render) initialState

{-
createLoaderClass :: forall a b
                   . String
                  -> (a -> Aff b)
                  -> ReactClass (Props a b)
createLoaderClass name loader = React.component name mk
  where
    mk this =
      pure
        { state: { loaded: Nothing, fiber: Nothing }
        , componentDidMount: do
            logs "componentDidMount"
            {path} <- React.getProps this
            fiber <- launchAff $ do
              newState <- loader path
              makeAff $ \cb -> do
                void $ React.modifyStateWithCallback
                         this
                         (_ {loaded = Just newState})
                         (cb (Right unit))
                pure nonCanceler
            React.modifyState this (_ { fiber = Just fiber })
        , componentWillUnmount: do
            {fiber} <- React.getState this
            traverse_ (launchAff_ <<< killFiber (error "Loader: killFiber"))
                      fiber
        , render: do
            {path, component} <- React.getProps this
            {loaded} <- React.getState this
            pure $ React.createElement component {path, loaded} []
        }
-}
