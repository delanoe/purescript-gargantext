module Gargantext.Components.TileMenu
  ( tileMenu
  ) where

import Gargantext.Prelude

import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.UUID as UUID
import Effect (Effect)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Hooks.LinkHandler (useLinkHandler)
import Gargantext.Routes (AppRoute, Tile)
import Gargantext.Utils.Popover as Popover
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.TileMenu"

type Props =
  ( boxes       :: Boxes
  , currentTile :: Maybe (Unit -> Effect AppRoute)
  , xTile       :: Maybe (Unit -> Effect AppRoute)
  , yTile       :: Maybe (Unit -> Effect AppRoute)
  )

tileMenu :: R2.Component Props
tileMenu = R.createElement tileMenuCpt
tileMenuCpt :: R.Component Props
tileMenuCpt = here.component "tileMenu" cpt where
  cpt props@{ boxes } children = do
    -- Hooks
    { goToRoute } <- useLinkHandler
    -- States
    popoverRef <- R.useRef null
    -- Helpers
    let
      newTile :: T.Box (Array (Record Tile)) -> AppRoute -> Effect Unit
      newTile list route = do
        id <- UUID.genUUID
        tile <- pure { id, route }
        T.modify_ (\arr -> snoc arr tile) list

      currentTileCbk :: (Unit -> Effect AppRoute) -> Effect Unit
      currentTileCbk thunk = thunk unit >>= goToRoute

      addTileCbk ::
           (Unit -> Effect AppRoute)
        -> T.Box (Array (Record Tile))
        -> Effect Unit
      addTileCbk thunk list = thunk unit >>= newTile list
    -- Render
    pure $

      H.div { className: "tile-menu" }
      [
        Popover.popover
        { arrow   : false
        , open    : false
        , onClose : const $ pure unit
        , onOpen  : const $ pure unit
        , ref     : popoverRef
        }
        [
          R.fragment children
        ,
          H.div { className: "tile-menu__popover" }
          [
            H.ul {}
            [
              -- Current Tile
              case props.currentTile of
                Nothing    -> mempty
                Just thunk ->
                  H.li { className: "tile-menu__item"}
                  [
                    H.button
                    { className: "btn btn-link"
                    , on: { click: const do
                              currentTileCbk thunk
                              Popover.setOpen popoverRef false
                          }
                    }
                    [
                      H.i { className: "fa fa-share" } []
                    ,
                      H.text "open on current tile"
                    ]
                  ]
            ,
              -- Add vertical tile
              case props.yTile of
                Nothing    -> mempty
                Just thunk ->
                  H.li { className: "tile-menu__item" }
                  [
                    H.button
                    { className: "btn btn-link"
                    , on: { click: const do
                              addTileCbk thunk boxes.tileAxisYList
                              Popover.setOpen popoverRef false
                         }
                    }
                    [
                      H.i { className: "fa fa-caret-square-o-right" } []
                    ,
                      H.text "open from a new tile"
                    ]
                  ]
            ,
              -- Add horizontal tile
              case props.xTile of
                Nothing    -> mempty
                Just thunk ->
                  H.li { className: "tile-menu__item" }
                  [
                    H.button
                    { className: "btn btn-link"
                    , on: { click: const do
                              addTileCbk thunk boxes.tileAxisXList
                              Popover.setOpen popoverRef false
                          }
                    }
                    [
                      H.i { className: "fa fa-caret-square-o-down" } []
                    ,
                      H.text "open from a new tile"
                    ]
                  ]
            ]
          ]
        ]
      ]
