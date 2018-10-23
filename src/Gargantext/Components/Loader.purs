module Gargantext.Components.Loader where

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import React as React
import React (ReactClass)
import Gargantext.Prelude
import Effect.Aff (Aff, launchAff, launchAff_, makeAff, nonCanceler, killFiber)
import Effect.Exception (error)

type InnerProps a b =
  { path     :: a
  , loaded   :: Maybe b
  , children :: React.Children
  }

type Props a b = { path      :: a
                 , component :: ReactClass (InnerProps a b)
                 }

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
