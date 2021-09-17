module Gargantext.Components.Login.Modal (Props, modal) where

import Prelude (bind, (<*), (<$>))
import Data.Semigroup ((<>))
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T
import Gargantext.Utils.Reactix as R2

type Props v = ( visible :: v )

here :: R2.Here
here = R2.here "Gargantext.Components.Login.Modal"

modal :: forall v. T.ReadWrite v Boolean
       => Record (Props v) -> R.Element -> R.Element
modal props child = R.createElement modalCpt props [ child ]

modalCpt :: forall v. T.ReadWrite v Boolean => R.Component (Props v)
modalCpt = here.component "modal" cpt where
  cpt { visible } children = do
    v <- T.useLive T.unequal visible
    R.createPortal
      [ H.div
        { id: "loginModal", className: modalClass v, key: 0
        , role: "dialog", data: { show: true }, style: { display: "block"} }
        [ H.div { className: "modal-dialog modal-lg", role: "document"}
          [ H.div { className: "modal-content" }
            [ H.div { className: "modal-header" }
              [ H.div { className: "col-md-10 col-md-push-1" }
                [ H.h2 { className: "text-primary center m-a-2" }
                  -- H.i {className: "material-icons md-36"}
                  -- [ H.text "control_point" ]
                  [ H.span {className: "center icon-text"} [ H.text "Exploring the eco-system with the workspace manager" ]]]
              , H.button -- TODO , font-size : "50px"
                { type: "button", className: "close"
                , data: { dismiss: "modal" }}
                [ H.a { on: { click }, className: "btn fa fa-times" } [] ]]
            , H.div { className: "modal-body" } children ]]]]
      <$> R2.getPortalHost
    where
      click _ = here.log "click!" <* T.write false visible
      modalClass s = "modal myModal" <> if s then "" else " fade"
