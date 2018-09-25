{- | Main Configuration of Gargantext Front-End

The main function to use for internal link in the Front-End
developpement is : toUrl.

* Example usage (depending on your Config):
toUrl Back  Corpus 1 == "http://localhost:8008/api/v1.0/corpus/1"
toUrl Front Corpus 1 == "http://localhost:2015/#/corpus/1"
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
    base   = endBaseUrl e endConfig
    path   = endPathUrl e endConfig nt i
    params = ""
------------------------------------------------------------
data NodeType = NodeUser
              | Annuaire
              | Corpus
              | Dashboard
              | Document
              | Folder
              | Graph
              | Individu
              | Project
              | Tree
              | Error
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
urlConfig NodeUser  = show NodeUser
urlConfig Annuaire  = show Annuaire
urlConfig Corpus    = show Corpus
urlConfig Dashboard = show Dashboard
urlConfig Document  = show Document
urlConfig Folder    = show Folder
urlConfig Graph     = show Graph
urlConfig Individu  = show Individu
urlConfig Project   = show Project
urlConfig Tree      = show Tree
urlConfig Error     = show Error
------------------------------------------------------------
instance showNodeType :: Show NodeType where
  show NodeUser  = "user"
  show Annuaire  = "annuaire"
  show Corpus    = "corpus"
  show Dashboard = "dashboard"
  show Document  = "document"
  show Folder    = "folder"
  show Graph     = "graph"
  show Individu  = "individu"
  show Project   = "project"
  show Tree      = "tree"
  show Error     = "ErrorNodeType"

-- | TODO : where is the Read Class ?
-- instance readNodeType :: Read NodeType where
readNodeType :: String -> NodeType
readNodeType "NodeUser"   = NodeUser
readNodeType "NodeCorpus" = Corpus
readNodeType "Annuaire"   = Annuaire
readNodeType "Dashboard"  = Dashboard
readNodeType "Document"   = Document
readNodeType "Folder"     = Folder
readNodeType "Graph"      = Graph
readNodeType "Individu"   = Individu
readNodeType "Project"    = Project
readNodeType "Tree"       = Tree
readNodeType _            = Error
------------------------------------------------------------
instance ordNodeType :: Ord NodeType where
  compare n1 n2 = compare (show n1) (show n2)

instance eqNodeType :: Eq NodeType where
  eq n1 n2  = eq (show n1) (show n2)
------------------------------------------------------------
