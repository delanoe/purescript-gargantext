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

type InnerProps path loaded props =
  { path     :: path
  , loaded   :: Maybe loaded
  , dispatch :: Action path -> Effect Unit
  , props    :: props
  , children :: Children
  }

type PropsRow path loaded props row =
  ( path      :: path
  , component :: ReactClass (InnerProps path loaded props)
  , props     :: props
  | row
  )

type Props path loaded props = Record (PropsRow path loaded props (children :: Children))

type Props' path loaded props = Record (PropsRow path loaded props ())

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

createLoaderClass :: forall path loaded props
                   . Eq path
                  => String
                  -> (path -> Aff loaded)
                  -> ReactClass (Record (PropsRow path loaded props (children :: Children)))
createLoaderClass name loader =
    createLoaderClass' name loader render
  where
    render :: Render (State path loaded) (Props' path loaded props) (Action path)
    render dispatch {component, props} {currentPath, loaded} c =
      [React.createElement component {path: currentPath, loaded, dispatch, props} c]

{-
createLoaderClass :: forall path loaded props
                   . String
                  -> (path -> Aff loaded)
                  -> ReactClass (Props path loaded props)
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
