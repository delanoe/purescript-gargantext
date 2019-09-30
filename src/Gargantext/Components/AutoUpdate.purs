module Gargantext.Components.AutoUpdate where

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import React as React
import React (ReactClass, ReactElement, Children)
import React.DOM (div')
import Gargantext.Prelude
import Effect (Effect)
import Effect.Timer (IntervalId, setInterval, clearInterval)

data Action = Update

type PropsRow =
  ( duration :: Int
  , effect   :: Effect Unit
  )

type Props = { | PropsRow }

type State = { intervalId :: Maybe IntervalId }

autoUpdateClass :: ReactClass { children :: Children | PropsRow }
autoUpdateClass =
  React.component "AutoUpdate"
    (\this -> do
       pure { state: {intervalId: Nothing}
            , render: pure $ div' []
            , componentDidMount: do
                {duration,effect} <- React.getProps this
                intervalId        <- setInterval duration effect
                React.setState this {intervalId: Just intervalId}
            , componentWillUnmount: do
                {intervalId} <- React.getState this
                traverse_ clearInterval intervalId
            })

autoUpdateElt :: Props -> ReactElement
autoUpdateElt props = React.createElement autoUpdateClass props []
