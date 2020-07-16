module Gargantext.Utils.Crypto where

import Gargantext.Prelude
import Crypto.Simple as Crypto

hash :: forall a. Crypto.Hashable a => a -> String
hash = Crypto.toString <<< Crypto.hash Crypto.SHA256

