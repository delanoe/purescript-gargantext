module Gargantext.Components.ErrorsView (component) where

import Gargantext.Prelude

import Data.Array (deleteAt)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Gargantext.Components.App.Store as AppStore
import Gargantext.Types (FrontendError(..))
import Gargantext.Utils.ReactBootstrap as RB
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.ErrorsView"

component :: R2.Leaf ()
component = R2.leaf componentCpt
componentCpt :: R.Component ()
componentCpt = here.component "main" cpt where
  cpt _ _ = do
    -- | States
    -- |
    { errors
    } <- AppStore.use

    errors' <- R2.useLive' errors

    -- | Render
    -- |
    pure $

      H.div
      {}
      ( mapWithIndex (showError errors) errors' )

  showError errors i (FStringError { error }) =
    RB.alert
    { dismissible: true
    , onClose
    , variant: "danger"
    }
    [ H.text error ]

    where
      onClose = do
        here.error2 "click!" error
        T.modify_ (\es -> case deleteAt i es of
                      Nothing  -> es
                      Just es' -> es'
        ) errors

  showError errors i (FRESTError { error }) =
    RB.alert
    { dismissible: true
    , onClose
    , variant: "danger"
    }
    [ H.text $ show error ]

    where
      onClose = do
        here.error2 "click!" error
        T.modify_ (\es -> case deleteAt i es of
                      Nothing  -> es
                      Just es' -> es'
        ) errors

  showError errors i (FOtherError { error }) =
    RB.alert
    { dismissible: true
    , onClose
    , variant: "danger"
    }
    [ H.text $ show error ]

    where
      onClose = do
        here.error2 "click!" error
        T.modify_ (\es -> case deleteAt i es of
                      Nothing  -> es
                      Just es' -> es'
        ) errors
