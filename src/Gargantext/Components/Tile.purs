module Gargantext.Components.Tile
  ( tileBlock
  , tileContext
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Routes (Tile)
import Reactix as R
import Reactix.DOM.HTML as H

type Props =
  ( boxes         :: Boxes
  , tile          :: Record Tile
  , key           :: String
  , closeCallback :: Unit -> Effect Unit
  )

tileBlock :: Record Props -> Array R.Element -> R.Element
tileBlock = R.createElement tileBlockCpt

tileBlockCpt :: R.Component Props
tileBlockCpt = R.hooksComponent "tileBlock" cpt where
  cpt props@{ closeCallback } children = do

    -- Render
    pure $

      R.provideContext tileContext (Just props)
      [
        H.div
        { className: "tile-block" }
        [
          H.div { className: "tile-block__header"}
          [
            H.i
            { className: "btn fa fa-times"
            , on: { click: closeCallback }
            }
            []
          ]
        ,
          H.div { className: "tile-block__body" }
          children
        ]
      ]

tileContext :: R.Context (Maybe (Record Props))
tileContext = R.createContext Nothing
