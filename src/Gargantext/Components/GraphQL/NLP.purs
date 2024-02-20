module Gargantext.Components.GraphQL.NLP where

import Gargantext.Prelude
import Gargantext.Components.Lang (Lang, ServerType)


type LanguageProperties
  = { url    :: String
    , server :: ServerType
    }

type Language
  = { lt_lang   :: Lang
    , lt_server :: LanguageProperties }

type NLPQuery =
  { languages ::
       { lt_lang :: Unit
       , lt_server :: {
           url :: Unit
         , server :: Unit }
       }
  }

nlpQuery :: NLPQuery
nlpQuery = { languages:
             { lt_lang   : unit
             , lt_server : {
                 url : unit
               , server: unit }
             }
           }
