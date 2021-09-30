module Gargantext.Components.FolderView.Box where

import Gargantext.Prelude

import DOM.Simple as DOM
import Effect (Effect)
import Gargantext.Components.Forest.Tree.Node.Tools (prettyNodeType)
import Gargantext.Types (ID, Name)
import Gargantext.Types as GT
import Gargantext.Utils.Glyphicon (glyphicon)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.FolderView.Box"

type NodePopupProps =
  ( id             :: ID
  , name           :: Name
  , nodeType       :: GT.NodeType
  , onPopoverClose :: DOM.Element -> Effect Unit
  )

nodePopupView :: R2.Leaf NodePopupProps
nodePopupView props = R.createElement nodePopupViewCpt props []
nodePopupViewCpt :: R.Component NodePopupProps
nodePopupViewCpt = here.component "nodePopupView" cpt where
  cpt props  _ = do

    pure $ H.div tooltipProps
      [ H.div { className: "popup-container" }
        [ H.div { className: "card" }
          [ panelHeading  props
          ]]]

  closePopover props = props.onPopoverClose <<< R.unsafeEventTarget

  tooltipProps = { id: "node-popup-tooltip", title: "Node settings"
                 , data: { toggle: "tooltip", placement: "right" } }

  panelHeading props@{ nodeType } =
    H.div { className: "card-header" }
    [ R2.row
      [ H.div { className: "col-4" }
        [ H.span { className: GT.fldr nodeType true} [] -- TODO fix names
        , H.span { className: "h5" } [ H.text $ prettyNodeType nodeType ] ]
      , H.div { className: "col-6" }
        [ H.span { className: "text-primary center" } [ H.text props.name ] ]
      , H.div { className: "col-1" }
        [ H.a { type: "button", on: { click: closePopover props }, title: "Close"
              , className: glyphicon "window-close" } [] ]]] 
