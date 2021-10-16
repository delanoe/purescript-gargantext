module Gargantext.Utils.ArrayBuffer where


import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Function.Uncurried (Fn1, runFn1)

foreign import arrayBufferToBase64Impl :: Fn1 ArrayBuffer String

arrayBufferToBase64 :: ArrayBuffer -> String
arrayBufferToBase64 = runFn1 arrayBufferToBase64Impl

