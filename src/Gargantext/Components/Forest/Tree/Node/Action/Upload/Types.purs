module Gargantext.Components.Forest.Tree.Node.Action.Upload.Types where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Web.File.Blob (Blob, size)

import Gargantext.Prelude


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
derive instance genericUploadFileBlob :: Generic UploadFileBlob _
instance eqUploadFileBlob :: Eq UploadFileBlob where
  eq (UploadFileBlob b1) (UploadFileBlob b2) = eq (size b1) (size b2)
