module Gargantext.Components.Loader where

import Control.Monad.Cont.Trans (lift)
import Data.Maybe (Maybe(..))
import React as React
import React (ReactClass, Children)
import Gargantext.Prelude
import Effect (Effect)
import Effect.Aff (Aff)

import Thermite (Render, PerformAction, simpleSpec, modifyState_, createReactSpec)

data Action path = ForceReload | SetPath path

type InnerPropsRow path loaded row =
  ( path     :: path
  , loaded   :: loaded
  , dispatch :: Action path -> Effect Unit
  | row
  )

type InnerProps path loaded row = Record (InnerPropsRow path loaded row)

type PropsRow path loaded row =
  ( path      :: path
  , component :: ReactClass (InnerProps path loaded (children :: Children))
  | row
  )

type Props path loaded = Record (PropsRow path loaded (children :: Children))

type Props' path loaded = Record (PropsRow path loaded ())

type State path loaded = { currentPath :: path, loaded :: Maybe loaded }

createLoaderClass' :: forall path loaded props
                    . Eq path
                   => String
                   -> (path -> Aff loaded)
                   -> Render (State path loaded) {path :: path | props} (Action path)
                   -> ReactClass { path :: path, children :: Children | props }
createLoaderClass' name loader render =
  React.component name
    (\this -> do
       s <- spec this
       pure { state: s.state
            , render: s.render
            , componentDidMount: dispatcher this ForceReload
            })
  where
    initialState {path} = {currentPath: path, loaded: Nothing}

    performAction :: PerformAction (State path loaded) {path :: path | props} (Action path)
    performAction ForceReload _ {currentPath} = do
      loaded <- lift $ loader currentPath
      modifyState_ $ _ { loaded = Just loaded }
    performAction (SetPath newPath) _ {currentPath} =
      when (newPath /= currentPath) $ do
        loaded <- lift $ loader newPath
        modifyState_ $ _ { currentPath = newPath, loaded = Just loaded }

    {spec, dispatcher} = createReactSpec (simpleSpec performAction render) initialState

createLoaderClass :: forall path loaded
                   . Eq path
                  => String
                  -> (path -> Aff loaded)
                  -> ReactClass (Record (PropsRow path loaded (children :: Children)))
createLoaderClass name loader =
    createLoaderClass' name loader render
  where
    render :: Render (State path loaded) (Props' path loaded) (Action path)
    render _ _ {loaded: Nothing} _ =
      -- TODO load spinner
      []
    render dispatch {component} {currentPath, loaded: Just loaded} c =
      [React.createElement component {path: currentPath, loaded, dispatch} c]

{-
createLoaderClass :: forall path loaded
                   . String
                  -> (path -> Aff loaded)
                  -> ReactClass (Props path loaded)
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
