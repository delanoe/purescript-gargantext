module Utils where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)

setterv :: forall nt record field. Newtype nt record => (record -> field -> record) -> field -> nt -> nt
setterv fn v t = (setter (flip fn v) t)

setter :: forall nt record. Newtype nt record => (record -> record) -> nt -> nt
setter fn = wrap <<< fn <<< unwrap

getter :: forall record field nt. Newtype nt record => (record -> field) -> nt -> field
getter fn = fn <<< unwrap
