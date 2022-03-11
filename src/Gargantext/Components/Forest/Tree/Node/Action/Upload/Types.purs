module Gargantext.Components.Forest.Tree.Node.Action.Upload.Types where

import Gargantext.Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Gargantext.Utils.ArrayBuffer (arrayBufferToBase64)
import Web.File.Blob (Blob, size)
import Web.File.FileReader.Aff (readAsArrayBuffer, readAsText)


data FileType = CSV | CSV_HAL | WOS | PresseRIS | Arbitrary | JSON

derive instance Generic FileType _
instance Eq FileType where eq = genericEq
instance Show FileType where show = genericShow
instance Read FileType where
  read :: String -> Maybe FileType
  read "Arbitrary" = Just Arbitrary
  read "CSV"       = Just CSV
  read "CSV_HAL"   = Just CSV_HAL
  read "PresseRIS" = Just PresseRIS
  read "WOS"       = Just WOS
  read "JSON"      = Just JSON
  read _           = Nothing

data FileFormat = Plain | ZIP

derive instance Generic FileFormat _
instance Eq FileFormat where eq = genericEq
instance Show FileFormat where show = genericShow
instance Read FileFormat where
  read :: String -> Maybe FileFormat
  read "Plain" = Just Plain
  read "ZIP"   = Just ZIP
  read _       = Nothing

newtype UploadFileBlob = UploadFileBlob Blob
derive instance Generic UploadFileBlob _
instance Eq UploadFileBlob where
  eq (UploadFileBlob b1) (UploadFileBlob b2) = eq (size b1) (size b2)

readUFBAsArrayBuffer :: UploadFileBlob -> Aff ArrayBuffer
readUFBAsArrayBuffer (UploadFileBlob b) = readAsArrayBuffer b

readUFBAsBase64 :: UploadFileBlob -> Aff String
readUFBAsBase64 (UploadFileBlob b) = do
  ab <- readAsArrayBuffer b
  pure $ arrayBufferToBase64 ab
  --pure $ Base64.runBase64 $ Base64.encodeBase64 ab
  --at <- readAsText b
  --pure $ SBase64.encode at

readUFBAsText :: UploadFileBlob -> Aff String
readUFBAsText (UploadFileBlob b) = readAsText b
