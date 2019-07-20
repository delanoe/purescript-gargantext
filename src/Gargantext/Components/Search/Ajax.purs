module Gargantext.Components.Search.Ajax where

import Prelude
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Data.Argonaut (class DecodeJson)
import DOM.Simple.Console
import Gargantext.Types (toQuery)
import Gargantext.Components.Search.Types (SearchQuery)
import Gargantext.Config.REST (post)
import Gargantext.Config (urlPlease, End(Back))
import URI.Query as Q

searchUrl :: SearchQuery -> String
searchUrl q = urlPlease Back $ "new" <> Q.print (toQuery q)

search :: SearchQuery -> Aff Int
search q = do
  let url = searchUrl q
  liftEffect $ log2 "url:" url
  post (searchUrl q) q

