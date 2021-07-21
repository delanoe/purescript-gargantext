module Gargantext.Components.Forest.Tree.Node.Action.Upload.Types where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Web.File.Blob (Blob, size)
import Web.File.FileReader.Aff (readAsText)

import Gargantext.Prelude


data FileType = CSV | CSV_HAL | WOS | PresseRIS | Arbitrary | JSON

derive instance Generic FileType _
instance Eq FileType where
  eq = genericEq
instance Show FileType where
  show = genericShow
instance Read FileType where
  read :: String -> Maybe FileType
  read "Arbitrary" = Just Arbitrary
  read "CSV"       = Just CSV
  read "CSV_HAL"   = Just CSV_HAL
  read "PresseRIS" = Just PresseRIS
  read "WOS"       = Just WOS
  read "JSON"      = Just JSON
  read _           = Nothing


newtype UploadFileBlob = UploadFileBlob Blob
derive instance Generic UploadFileBlob _
instance Eq UploadFileBlob where
  eq (UploadFileBlob b1) (UploadFileBlob b2) = eq (size b1) (size b2)

readUFBAsText :: UploadFileBlob -> Aff String
readUFBAsText (UploadFileBlob b) = readAsText b
