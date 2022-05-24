module Gargantext.Components.Frame.Types
  ( Hyperdata(..)
  , Base, FrameId
  ) where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Simple.JSON as JSON

type Base    = String
type FrameId = String

newtype Hyperdata = Hyperdata
  { base      :: String
  , frame_id  :: String
  }
derive instance Generic Hyperdata _
derive instance Newtype Hyperdata _
instance Eq Hyperdata where eq = genericEq
instance Show Hyperdata where show = genericShow
derive newtype instance JSON.ReadForeign Hyperdata
derive newtype instance JSON.WriteForeign Hyperdata

------------------------------------------------
