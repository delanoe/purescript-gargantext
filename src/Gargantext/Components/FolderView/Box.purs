module Gargantext.Components.FolderView.Box where

import Gargantext.Prelude

import DOM.Simple as DOM
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Elevation(..))
import Gargantext.Components.Forest.Tree.Node.Tools (prettyNodeType)
import Gargantext.Types (ID, Name)
import Gargantext.Types as GT
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
nodePopupView = R2.leafComponent nodePopupViewCpt
nodePopupViewCpt :: R.Component NodePopupProps
nodePopupViewCpt = here.component "nodePopupView" cpt where
  cpt props  _ = do

    pure $

      H.div
      { className: "node-popup-tooltip"
      , title: "Node settings"
      }
      [
        H.div
        { className: "popup-container card" }
        [
          panelHeading  props
        ]
      ]

  closeBox props = props.onPopoverClose <<< R.unsafeEventTarget

  panelHeading props@{ nodeType } =
    H.div
    { className: "popup-container__header card-header" }
    [
      B.wad
      [ "d-flex", "align-items-center" ]
      [
        H.div
        { className: "w-3/12" }
        [
          H.span { className: GT.fldr nodeType true} [] -- TODO fix names
        ,
          B.span' { className: "ml-1 h5" } $ prettyNodeType nodeType
        ]
      ,
        B.wad
        [ "w-7/12", "pl-1" ]
        [
          B.wad'
          [ "text-primary" ]
          props.name
        ]
      ,
        B.wad
        [ "w-2/12", "text-right" ]
        [
          B.iconButton
          { callback: closeBox props
          , title: "Close"
          , elevation: Level1
          , name: "times"
          }
        ]
      ]
    ]
