module Gargantext.Utils.CacheAPI where

import Control.Promise (Promise, toAffE)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Milkis as M
import Type.Row (class Union)

import Gargantext.Prelude

foreign import data Cache :: Type
foreign import data Request :: Type

newtype CacheName = CacheName String

makeRequest :: forall options trash. Union options trash M.Options =>
               M.URL -> { method :: M.Method | options } -> Request
makeRequest url options = _makeRequest url options

openCache :: CacheName -> Aff Cache
openCache (CacheName cacheName) = toAffE $ _openCache cacheName

cached :: Cache -> Request -> Aff M.Response
cached cache req = fromEffectFnAff $ _cached cache req

delete :: Cache -> Request -> Aff Unit
delete cache req = toAffE $ _delete cache req

foreign import _makeRequest :: forall options trash. Union options trash M.Options =>
                               M.URL -> { method :: M.Method | options } -> Request
foreign import _openCache :: String -> Effect (Promise Cache)
foreign import _cached :: Cache -> Request -> EffectFnAff M.Response
foreign import _delete :: Cache -> Request -> Effect (Promise Unit)
