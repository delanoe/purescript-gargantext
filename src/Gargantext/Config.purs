{- | Main Configuration of Gargantext Front-End

The main function to use for internal link in the Front-End
developpement is : toUrl.

* Example usage:
- for Mock config:
    - toUrl Front Corpus 3 == "http://localhost:2015/corpus/3"
    - (this mode supposes you have the mock haskell backend running)

- for Dev config:
    - toUrl Front Corpus 3 == "http://localhost:8008/corpus/3"
    - (this mode supposes you have the dev haskell backend running)

- for Prod config:
    - toUrl Front Corpus 3 == "https://gargantext.org:8080/corpus/3"
    - (this mode supposes you have a prod haskell backend running on the specified url)
-}
module Gargantext.Config where

import Prelude ( class Eq, class Ord, class Show
               , compare, eq, show, (<>), identity)

import Data.Map (Map)
import Data.Map as DM
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))

endConfig :: EndConfig
endConfig = endConfig' V10

endConfig' :: ApiVersion -> EndConfig
endConfig' v = { front : frontCaddy
               , back  : backDev v }

------------------------------------------------------------------------
frontCaddy :: Config
frontCaddy = { proto   : "http://"
             , port       : 2015
             , domain     : "localhost"
             , prePath    : "/#/"
             }

frontHaskell :: Config
frontHaskell = { proto   : "http://"
               , port       : 8008
               , domain     : "localhost"
               , prePath    : "/index.html#/"
               }

frontProd :: Config
frontProd = { proto   : "https://"
            , port       : 8080
            , domain     : "gargantext.org"
            , prePath    : "/index.html#/"
            }

------------------------------------------------------------------------

backDev :: ApiVersion -> Config
backDev v = { proto      : "http://"
            , port       : 8008
            , domain     : "localhost"
            , prePath    : "/api/" <> show v <> "/"
            }

backProd :: ApiVersion -> Config
backProd v = { proto      : "https://"
            , port       : 8080
            , domain     : "gargantext.org"
            , prePath    : "/api/" <> show v <> "/"
            }
------------------------------------------------------------------------


type EndConfig = { front :: Config
                 , back  :: Config
                 }

type Config = { proto      :: String
              , port       :: Int
              , domain     :: String
              , prePath    :: String
              }

------------------------------------------------------------
type UrlBase  = String
type UrlPath  = String
type UrlParam = String
type Url      = String

doUrl :: UrlBase -> UrlPath -> UrlParam -> Url
doUrl b p ps = b <> p <> ps
------------------------------------------------------------
endBaseUrl :: End -> EndConfig -> UrlBase
endBaseUrl Back  c = baseUrl c.back
endBaseUrl Front c = baseUrl c.front

baseUrl :: Config -> UrlBase
baseUrl conf = conf.proto <> conf.domain <> ":" <> show conf.port
------------------------------------------------------------

endPathUrl :: End -> EndConfig -> NodeType -> Id -> UrlPath
endPathUrl Back  c nt i = pathUrl c.back nt i
endPathUrl Front c nt i = pathUrl c.front nt i

pathUrl :: Config -> NodeType -> Id -> UrlPath
pathUrl c nt i = c.prePath <> urlConfig nt <> "/" <> show i
------------------------------------------------------------

toUrl :: End -> NodeType -> Id -> Url
toUrl e nt i = doUrl base path params
  where
    base = endBaseUrl e endConfig
    path = endPathUrl e endConfig nt i
    params = ""
------------------------------------------------------------
data NodeType = UserPage | Folder | Corpus | Document | Annuaire | Individu | Project | Tree | Error
data End = Back | Front
type Id  = Int
------------------------------------------------------------
data ApiVersion = V10 | V11
instance showApiVersion :: Show ApiVersion where
  show V10 = "v1.0"
  show V11 = "v1.1"
------------------------------------------------------------
------------------------------------------------------------
urlConfig :: NodeType -> Url
urlConfig UserPage = "user"
urlConfig Corpus   = show Corpus
urlConfig Project  = show Project
urlConfig Project  = show Project
urlConfig Document = show Document
urlConfig Annuaire = show Annuaire
urlConfig Individu = show Individu
urlConfig Tree     = show Tree
urlConfig _        = "error Url Config with That Node Type"
------------------------------------------------------------
instance showNodeType :: Show NodeType where
  show UserPage = "user"
  show Project  = "project"
  show Folder   = "folder"
  show Corpus   = "corpus"
  show Document = "document"
  show Annuaire = "annuaire"
  show Individu = "individu"
  show Tree     = "tree"
  show Error    = "errNodeType"

readNodeType :: String -> NodeType
readNodeType "NodeUser"   = UserPage
readNodeType "Project"    = Project
readNodeType "Folder"     = Folder
readNodeType "NodeCorpus" = Corpus
readNodeType "Document"   = Document
readNodeType "Annuaire"   = Annuaire
readNodeType "Individu"   = Individu
readNodeType "Tree"       = Tree
readNodeType _            = Error
------------------------------------------------------------
instance ordNodeType :: Ord NodeType where
  compare n1 n2 = compare (show n1) (show n2)

instance eqNodeType :: Eq NodeType where
  eq n1 n2  = eq (show n1) (show n2)
------------------------------------------------------------
