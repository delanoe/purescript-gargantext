module Gargantext.REST where

import Data.Argonaut
import Data.HTTP.Method (Method(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude (bind, ($), pure, show)
import Data.MediaType.Common (applicationJSON)

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Either (Either(..))

import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)


--loadData :: forall eff ajax a b. Bind ajax => MonadAff
--                     ( ajax :: AJAX
--                     , consolle :: CONSOLE
--                     | eff
--                     )
--                     ajax
--                    => Respondable a => (a -> Either String b) -> String -> ajax (Either String b)
loadData f url = do
  affResp <- liftAff $ attempt $ affjax defaultRequest
    { method  = Left GET
    , url     = url
    , headers =  [ ContentType applicationJSON
                 , Accept applicationJSON
              --   , RequestHeader "Authorization" $  "Bearer " <> token
                 ]
    }
  case affResp of
    Left err -> do
      pure $ Left $ show err
    Right a -> do
      let res = f a.response
      pure res
