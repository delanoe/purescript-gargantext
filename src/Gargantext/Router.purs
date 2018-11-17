module Gargantext.Router where

import Gargantext.Prelude

import Control.Alt ((<|>))
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Routing.Match (Match, lit, num)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

data Routes
  = Home
  | Login
  | SearchView
  | Folder             Int
    | Corpus           Int
      | AddCorpus
      | Document  Int
      | PGraphExplorer Int
      | Dashboard
    | Annuaire         Int
      | UserPage       Int

routing :: Match Routes
routing =
      Login            <$   route "login"
  <|> SearchView       <$   route "search"
  <|> AddCorpus        <$   route "addCorpus"
  <|> Folder           <$> (route "folder"   *> int)
  <|> Corpus           <$> (route "corpus"   *> int)
    <|> Document     <$> (route "document" *> int)
    <|> Dashboard      <$   route "dashboard"
    <|> PGraphExplorer <$>   (route "graph" *> int )
  <|> Annuaire         <$> (route "annuaire" *> int)
    <|> UserPage       <$> (route "user"     *> int)
  <|> Home             <$   lit ""
  
 where
    route str      = lit "" *> lit str
  
    int :: Match Int
    int = floor <$> num

instance showRoutes :: Show Routes where
  show Login            = "Login"
  show AddCorpus        = "AddCorpus"
  show SearchView       = "Search"
  show (UserPage i)     = "User"   <> show i
  show (Document i)     = "Document"
  show (Corpus i)       = "Corpus" <> show i
  show (Annuaire i)     = "Annuaire" <> show i
  show (Folder   i)     = "Folder"   <> show i
  show Dashboard        = "Dashboard"
  show (PGraphExplorer i)   = "graphExplorer" <> show i
  show Home             = "Home"


routeHandler :: (Maybe Routes -> Routes -> Effect Unit)
              -> Maybe Routes -> Routes -> Effect Unit
routeHandler dispatchAction old new = do
  logs $ "change route : " <> show new
  
  w      <- window
  ls     <- localStorage w
  token  <- getItem "accessToken" ls
  let tkn = token
  
  logs $ "JWToken : " <> show tkn
  
  case tkn of
    Nothing -> do
      dispatchAction old new
      logs $ "called SignIn Route :"
    Just t -> do
      dispatchAction old new
      logs $ "called Route : " <> show new
