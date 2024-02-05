module Gargantext.Components.GraphQL.NLP where

import Gargantext.Prelude

import GraphQL.Client.Args (Args, NotNull, (=>>))
import GraphQL.Client.Variable (Var(..))
import Gargantext.Components.Lang (Lang(..), ServerType)
import Gargantext.Utils.GraphQL as GGQL
import Type.Proxy (Proxy(..))

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
