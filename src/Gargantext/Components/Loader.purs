module Gargantext.Components.Loader where

import Control.Monad.Cont.Trans (lift)
import Data.Maybe (Maybe(..))
import React as React
import React (ReactClass, Children)
import Gargantext.Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Thermite (Render, PerformAction, simpleSpec, modifyState_, createReactSpec)

data Action path
  = ForceReload
  | SetPath path

type InnerPropsRow path loaded row
  = ( path :: path
    , loaded :: loaded
    , dispatch :: Action path -> Effect Unit
    | row
    )

type InnerProps path loaded row
  = Record (InnerPropsRow path loaded row)

type InnerClass path loaded
  = ReactClass (InnerProps path loaded ( children :: Children ))

type PropsRow path loaded row
  = ( path :: path
    , component :: InnerClass path loaded
    | row
    )

type Props path loaded
  = Record (PropsRow path loaded ( children :: Children ))

type Props' path loaded
  = Record (PropsRow path loaded ())

type State path loaded
  = { currentPath :: path, loaded :: Maybe loaded }

createLoaderClass' ::
  forall path loaded props.
  Eq path =>
  Show path =>
  String ->
  (path -> Aff loaded) ->
  Render (State path loaded) { path :: path | props } (Action path) ->
  ReactClass { path :: path, children :: Children | props }
createLoaderClass' name loader render =
  React.component name
    ( \this -> do
        logs $ "createLoaderClass' " <> name
        s <- spec this
        pure
          { state: s.state
          , render: s.render
          , componentDidMount:
            do
              logs $ name <> ".componentDidMount"
              dispatcher this ForceReload
          , componentDidUpdate:
            \{ path: prevPath } { currentPath } _snapshot -> do
              { path } <- React.getProps this
              logs $ name <> ".componentDidUpdate " <> show { currentPath, path, prevPath }
              -- This guard is the similar to the one in performAction (SetPath ...),
              -- however we need it here to avoid potential infinite loops.
              -- https://reactjs.org/docs/react-component.html#componentdidupdate
              -- Moreover we want to make sure that not only the new prop
              -- `path` is different from the one in the state (`currentPath`)
              -- but also that it is different than the previous `path` prop
              -- (`prevPath`). This avoid the path being reset to the
              -- previous value.
              when (prevPath /= path && path /= currentPath) do
                dispatcher this (SetPath path)
          }
    )
  where
  initialState { path } = { currentPath: path, loaded: Nothing }

  performAction :: PerformAction (State path loaded) { path :: path | props } (Action path)
  performAction ForceReload _ { currentPath } = do
    logs $ name <> ".ForceReload {currentPath: " <> show currentPath <> "}"
    loaded <- lift $ loader currentPath
    modifyState_ $ _ { loaded = Just loaded }

  performAction (SetPath newPath) _ { currentPath } = do
    logs $ name <> ".SetPath " <> show { newPath, currentPath }
    when (newPath /= currentPath) do
      loaded <- lift $ loader newPath
      modifyState_ $ _ { currentPath = newPath, loaded = Just loaded }

  { spec, dispatcher } = createReactSpec (simpleSpec performAction render) initialState

type LoaderClass path loaded
  = ReactClass (Record (PropsRow path loaded ( children :: Children )))

createLoaderClass ::
  forall path loaded.
  Eq path =>
  Show path =>
  String ->
  (path -> Aff loaded) ->
  LoaderClass path loaded
createLoaderClass name loader = createLoaderClass' name loader render
  where
  render :: Render (State path loaded) (Props' path loaded) (Action path)
  render _ _ { loaded: Nothing } _ = -- TODO load spinner
    []

  render dispatch { component } { currentPath, loaded: Just loaded } c = [ React.createElement component { path: currentPath, loaded, dispatch } c ]
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