module Gargantext.Hooks.LinkHandler
  ( Methods, useLinkHandler
  , goToRoute, goToURL, goToPreviousPage
  ) where

import Gargantext.Prelude

import Data.Array (findIndex, modifyAtIndices, singleton)
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Effect (Effect)
import Gargantext.Components.Tile (tileContext)
import Gargantext.Routes (AppRoute, Tile, appPath)
import Reactix as R
import Record (get, set)
import Toestand as T
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.History (back)
import Web.HTML.Location (assign, setHref)
import Web.HTML.Window (history, location)

type Methods =
  ( goToRoute        :: AppRoute -> Effect Unit
  , goToPreviousPage :: Unit -> Effect Unit
  , goToURL          :: String -> Effect Unit
  )

useLinkHandler :: R.Hooks (Record Methods)
useLinkHandler = do
  -- retrieve tile context where this hook is called
  -- if the callee is within a tile, some of the hook methods will be altered
  mTileContext <- R.useContext tileContext

  pure
    { goToRoute         : case mTileContext of
        Nothing              -> goToRoute
        Just { boxes, tile } -> changeTileRoute
          boxes.tileAxisXList
          boxes.tileAxisYList
          tile
    , goToPreviousPage  : const goToPreviousPage
    , goToURL           : goToURL
    }

-- (?) Also exporting implementation methods, as it can be useful in an
--     outside-of-hook context

goToRoute :: AppRoute -> Effect Unit
goToRoute route = window >>= location >>= (assign $ "/#/" <> appPath route)

goToPreviousPage :: Effect Unit
goToPreviousPage = window >>= history >>= back

goToURL :: String -> Effect Unit
goToURL url = window >>= location >>= setHref url

--------------------------------------

changeTileRoute ::
     T.Box (Array (Record Tile))
  -> T.Box (Array (Record Tile))
  -> Record Tile
  -> AppRoute
  -> Effect Unit
changeTileRoute tileAxisXList tileAxisYList tile newRoute = do
  listX <- T.read tileAxisXList
  listY <- T.read tileAxisYList

  let
    findTile :: UUID -> Record Tile -> Boolean
    findTile id tile' = eq id $ get (Proxy :: Proxy "id") tile'

    hasTile :: Array (Record Tile) -> UUID -> Maybe Int
    hasTile list id = findIndex (findTile id) list

    updateTile :: Int -> AppRoute -> Array (Record Tile) -> Array (Record Tile)
    updateTile index route list = modifyAtIndices
      (singleton index)
      (set (Proxy :: Proxy "route") route $ _)
      list

  -- (!) to optimize when tile structure design is locked
  case hasTile listX tile.id of
    Nothing    -> pure unit
    Just index -> T.write_ (updateTile index newRoute listX) tileAxisXList

  case hasTile listY tile.id of
    Nothing    -> pure unit
    Just index -> T.write_ (updateTile index newRoute listY) tileAxisYList
