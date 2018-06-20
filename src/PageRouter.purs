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
  | AddCorpus
  | DocView
  | SearchView
  | UserPage
  | DocAnnotation Int
  | Tabview
  | CorpusAnalysis
  | PGraphExplorer
  | NGramsTable

instance showRoutes :: Show Routes where
  show Home       = "Home"
  show Login      = "Login"
  show AddCorpus  = "AddCorpus"
  show DocView    = "DocView"
  show SearchView = "SearchView"
  show UserPage   = "UserPage"
  show (DocAnnotation i)= "DocumentView"
  show Tabview          = "Tabview"
  show CorpusAnalysis   = "corpus"
  show PGraphExplorer   = "graphExplorer"
  show NGramsTable      = "NGramsTable"

int :: Match Int
int = floor <$> num

routing :: Match Routes
routing =
      Login          <$   route "login"
  <|> Tabview        <$   route "tabview"
  <|> DocAnnotation  <$> (route "documentView" *> int)
  <|> UserPage       <$   route "userPage"
  <|> SearchView     <$   route "search"
  <|> DocView        <$   route "docView"
  <|> AddCorpus      <$   route "addCorpus"
  <|> CorpusAnalysis <$   route "corpus"
  <|> PGraphExplorer <$   route "graphExplorer"
  <|> NGramsTable    <$   route "ngrams"
  <|> Home           <$     lit ""
  where
    route str      = lit "" *> lit str

routeHandler :: forall e. ( Maybe Routes -> Routes -> Eff
                            ( dom     :: DOM
                            , console :: CONSOLE
                            | e
                            ) Unit
                          ) -> Maybe Routes -> Routes -> Eff
                            ( dom     :: DOM
                            , console :: CONSOLE
                            | e
                            ) Unit
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
