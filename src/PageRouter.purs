module PageRouter where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (localStorage)
import DOM.WebStorage.Storage (getItem)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Routing.Match (Match)
import Routing.Match.Class (lit, num)

data Routes
  = Home
  | Login

instance showRoutes :: Show Routes where
  show Home = "Home"
  show Login = "Login"

int :: Match Int
int = floor <$> num

routing :: Match Routes
routing =
      loginRoute
  <|> home
  where
    loginRoute = Login <$ route "login"
    home = Home <$ lit ""
    route str = lit "" *> lit str

routeHandler :: forall e. (Maybe Routes -> Routes -> Eff ( dom :: DOM, console :: CONSOLE | e) Unit) -> Maybe Routes -> Routes -> Eff (dom :: DOM, console :: CONSOLE | e) Unit
routeHandler dispatchAction old new = do
  liftEff $ log $ "change route : " <> show new
  w <- window
  ls <- localStorage w
  token <- getItem "accessToken" ls
  let tkn = token
  liftEff $ log $ "JWToken : " <> show tkn
  case tkn of
    Nothing -> do
      dispatchAction old new
      liftEff $ log $ "called SignIn Route :"
    Just t -> do
      dispatchAction old new
      liftEff $ log $ "called Route : " <> show new
