module Gargantext.Components.ErrorsView where

import Gargantext.Prelude

import Data.Array (deleteAt)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Types (FrontendError(..))
import Gargantext.Utils.ReactBootstrap as RB
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.ErrorsView"

type ErrorsProps =
  ( errors :: T.Box (Array FrontendError) )

errorsView :: R2.Component ErrorsProps
errorsView = R.createElement errorsViewCpt
errorsViewCpt :: R.Component ErrorsProps
errorsViewCpt = here.component "errorsView" cpt
  where
    cpt { errors } _ = do
      errors' <- T.useLive T.unequal errors

      pure $ H.div {}
        ( mapWithIndex (showError errors) errors' )
    showError errors i (FStringError { error }) =
      RB.alert { dismissible: true
               , onClose
               , variant: "danger" } [ H.text error ]
      where
        onClose = do
          here.log2 "click!" error
          T.modify_ (\es -> case deleteAt i es of
                       Nothing  -> es
                       Just es' -> es'
          ) errors
    showError errors i (FRESTError { error }) =
      RB.alert { dismissible: true
               , onClose
               , variant: "danger" } [ H.text $ show error ]
      where
        onClose = do
          here.log2 "click!" error
          T.modify_ (\es -> case deleteAt i es of
                       Nothing  -> es
                       Just es' -> es'
          ) errors
