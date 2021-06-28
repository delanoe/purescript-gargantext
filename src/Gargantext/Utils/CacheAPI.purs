module Gargantext.Utils.CacheAPI where

import Control.Monad.Except (runExcept)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Foreign as F
import Foreign.Object as O
import Milkis as M
import Type.Row (class Union)

import Gargantext.Prelude hiding (add)
import Gargantext.Ends (class ToUrl, toUrl)
import Gargantext.Sessions (Session(..))


get :: forall a p. DecodeJson a => ToUrl Session p => Cache -> Session -> p -> Aff a
get cache session p = do
  let req = makeGetRequest session p
  res <- cached cache req

  j <- M.json res

  case decodeJson (F.unsafeFromForeign j) of
    Left err -> throwError $ error $ "decodeJson affResp.body: " <> show err
    Right b -> pure b

foreign import data Cache :: Type
foreign import data Request :: Type

newtype CacheName = CacheName String
type Token = String

makeRequest :: forall options trash. Union options trash M.Options =>
               M.URL -> { method :: M.Method, headers :: M.Headers | options } -> Request
makeRequest url options = _makeRequest url options

makeTokenRequest :: forall options trash. Union options trash M.Options =>
                    M.URL -> Maybe Token -> { method :: M.Method, headers :: M.Headers | options } -> Request
makeTokenRequest url mToken options = case mToken of
  Nothing -> makeRequest url $ options { headers = mkHeaders O.empty }
  Just t  -> makeRequest url $ options { headers = mkHeaders $ O.singleton "Authorization" $ "Bearer " <> t }
  where
    defaultOptions = O.fromFoldable [ Tuple "Accept" "application/json"
                                    , Tuple "Content-Type" "application/json" ]
    mkHeaders t = O.unions [ options.headers, defaultOptions, t ]

makeGetRequest :: forall p. ToUrl Session p => Session -> p -> Request
makeGetRequest session@(Session { token }) p = makeTokenRequest url (Just token) { method, headers: O.empty }
  where
    method = M.getMethod
    url = M.URL $ toUrl session p

openCache :: CacheName -> Aff Cache
openCache (CacheName cacheName) = toAffE $ _openCache cacheName

delete :: CacheName -> Aff Unit
delete (CacheName cacheName) = toAffE $ _delete cacheName

add :: Cache -> Request -> Aff Unit
add cache req = toAffE $ _add cache req

match :: Cache -> Request -> Aff (Maybe M.Response)
match cache req = do
  res <- toAffE $ _match cache req
  -- _match returns a null/undefined value when cache entity not found
  case runExcept $ F.readNullOrUndefined res of
    Left err -> throwError $ error $ show err
    Right v -> pure $ F.unsafeFromForeign <$> v

cached :: Cache -> Request -> Aff M.Response
cached cache req = do
  mRes <- match cache req
  case mRes of
    Just res -> do
      -- liftEffect $ log2 "[cached] cache hit" req
      pure res
    Nothing -> do
      -- liftEffect $ log2 "[cached] cache miss" req
      _ <- add cache req
      mResFresh <- match cache req
      case mResFresh of
        Just res -> pure res
        Nothing -> throwError $ error $ "Cannot add to cache"

cachedJson :: forall a. DecodeJson a => Cache -> Request -> Aff a
cachedJson cache req = do
  res <- cached cache req
  -- liftEffect $ do
  --   log2 "[cachedJson] res" res
  j <- M.json res

  case decodeJson (F.unsafeFromForeign j) of
    Left err -> throwError $ error $ "[cachedJson] decodeJson affResp.body: " <> show err
    Right b -> pure b

deleteReq :: Cache -> Request -> Aff Unit
deleteReq cache req = toAffE $ _deleteReq cache req


-- No cache: raw API calls

fetch :: Request -> Aff M.Response
fetch req = do
  res <- toAffE $ _fetch req
  pure $ F.unsafeFromForeign res

pureJson :: forall a. DecodeJson a => Request -> Aff a
pureJson req = do
  res <- fetch req
  j <- M.json res
  case decodeJson (F.unsafeFromForeign j) of
    Left err -> throwError $ error $ "[pureJson] decodeJson affResp.body: " <> show err
    Right b -> pure b


foreign import _makeRequest :: forall options trash.
                               M.URL -> { method :: M.Method, headers :: M.Headers | options } -> Request
foreign import _openCache :: String -> Effect (Promise Cache)
foreign import _delete :: String -> Effect (Promise Unit)
foreign import _deleteReq :: Cache -> Request -> Effect (Promise Unit)
foreign import _add :: Cache -> Request -> Effect (Promise Unit)
foreign import _match :: Cache -> Request -> Effect (Promise F.Foreign)
foreign import _fetch :: Request -> Effect (Promise F.Foreign)
