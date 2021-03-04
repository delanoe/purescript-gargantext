module Gargantext.Components.AutoUpdate where

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import React as React
import React (ReactClass, ReactElement, Children)
import React.DOM (div')
import Effect (Effect)
import Effect.Timer (IntervalId, TimeoutId, setInterval, clearInterval, setTimeout, clearTimeout)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

here = R2.here "Gargantext.Components.AutoUpdate"


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
                {duration, effect} <- React.getProps this
                intervalId        <- setInterval duration effect
                React.setState this {intervalId: Just intervalId}
            , componentWillUnmount: do
                {intervalId} <- React.getState this
                traverse_ clearInterval intervalId
            })

autoUpdateElt :: Props -> ReactElement
autoUpdateElt props = React.createElement autoUpdateClass props []

autoUpdate :: Record PropsRow -> R.Element
autoUpdate props = R.createElement autoUpdateCpt props []

autoUpdateCpt :: R.Component PropsRow
autoUpdateCpt = here.component "autoUpdate" cpt
  where
    cpt { duration, effect } _ = do
      intervalRef <- R.useRef Nothing

      R.useEffect' $ do
        let mInterval = R.readRef intervalRef
        case mInterval of
          Nothing -> do
            intervalId <- setInterval duration effect
            R.setRef intervalRef $ Just intervalId
          Just intervalId -> do
            clearInterval intervalId
            intervalId <- setInterval duration effect
            R.setRef intervalRef $ Just intervalId

      pure $ H.div {} []
