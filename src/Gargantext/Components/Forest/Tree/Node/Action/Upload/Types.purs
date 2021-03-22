module Gargantext.Components.Forest.Tree.Node.Action.Upload.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Web.File.Blob (Blob)

import Gargantext.Prelude (class Read, class Show, class Eq)


data FileType = CSV | CSV_HAL | WOS | PresseRIS | Arbitrary

derive instance genericFileType :: Generic FileType _
instance eqFileType :: Eq FileType where
  eq = genericEq
instance showFileType :: Show FileType where
  show = genericShow
instance readFileType :: Read FileType where
  read :: String -> Maybe FileType
  read "Arbitrary" = Just Arbitrary
  read "CSV"       = Just CSV
  read "CSV_HAL"   = Just CSV_HAL
  read "PresseRIS" = Just PresseRIS
  read "WOS"       = Just WOS
  read _           = Nothing


newtype UploadFileBlob = UploadFileBlob Blob
