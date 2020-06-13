module Gargantext.Components.Forest.Tree.Node.Action.Upload.Types where

import Gargantext.Prelude (class Read, class Show, class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)


data FileType = CSV | CSV_HAL | WOS | PresseRIS

derive instance genericFileType :: Generic FileType _

instance eqFileType :: Eq FileType where
  eq = genericEq

instance showFileType :: Show FileType where
  show = genericShow

instance readFileType :: Read FileType where
  read :: String -> Maybe FileType
  read "CSV"       = Just CSV
  read "CSV_HAL"   = Just CSV_HAL
  read "PresseRIS" = Just PresseRIS
  read "WOS"       = Just WOS
  read _           = Nothing


newtype UploadFileContents = UploadFileContents String


