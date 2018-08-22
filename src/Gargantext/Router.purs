module Gargantext.Router where

import Prelude

import Control.Alt ((<|>))
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Routing.Match (Match)

data Routes
  = Home
  | Login
  | AddCorpus
  | DocView
  | SearchView
  | UserPage       Int
  | DocAnnotation  Int
  | Tabview
  | CorpusAnalysis
  | PGraphExplorer
  | NGramsTable
  | Dashboard


instance showRoutes :: Show Routes where
  show Login            = "Login"
  show AddCorpus        = "AddCorpus"
  show DocView          = "DocView"
  show SearchView       = "SearchView"
  show (UserPage i)     = "UserPage"
  show (DocAnnotation i)= "DocumentView"
  show Tabview          = "Tabview"
  show CorpusAnalysis   = "corpus"
  show PGraphExplorer   = "graphExplorer"
  show NGramsTable      = "NGramsTable"
  show Dashboard        = "Dashboard"
  show Home             = "Home"

int :: Match Int
int = floor <$> num

routing :: Match Routes
routing =
      Login          <$  route "login"
  <|> Tabview        <$  route "tabview"
  <|> DocAnnotation  <$> (route "documentView" *> int)
  <|> UserPage       <$> (route "user" *> int)
  <|> SearchView     <$ route "search"
  <|> DocView        <$ route "docView"
  <|> AddCorpus      <$ route "addCorpus"
  <|> CorpusAnalysis <$ route "corpus"
  <|> PGraphExplorer <$ route "graphExplorer"
  <|> NGramsTable    <$ route "ngrams"
  <|> Dashboard      <$ route "dashboard"
  <|> Home           <$ lit ""
  where
    route str      = lit "" *> lit str

routeHandler :: (Maybe Routes -> Routes -> Effect Unit) -> Maybe Routes -> Routes -> Effect Unit
routeHandler dispatchAction old new = do
  liftEff $ log $ "change route : " <> show new
  w      <- window
  ls     <- localStorage w
  token  <- getItem "accessToken" ls
  let tkn = token
  liftEff $ log $ "JWToken : " <> show tkn
  case tkn of
    Nothing -> do
      dispatchAction old new
      liftEff $ log $ "called SignIn Route :"
    Just t -> do
      dispatchAction old new
      liftEff $ log $ "called Route : " <> show new
