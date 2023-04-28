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
  = { key   :: Lang
    , value :: LanguageProperties }

type NLPQuery =
  { languages ::
       { key :: Unit
       , value :: {
           url :: Unit
         , server :: Unit }
       }
  }

nlpQuery :: NLPQuery
nlpQuery = { languages:
             { key   : unit
             , value : {
                 url : unit
               , server: unit }
             }
           }
