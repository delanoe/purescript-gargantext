{- | Main Configuration of Gargantext Front-End

The main function to use for internal link in the Front-End
developpement is : toUrl.

* Example usage (depending on your Config):
toUrl Back  Corpus 1 == "http://localhost:8008/api/v1.0/corpus/1"
toUrl Front Corpus 1 == "http://localhost:2015/#/corpus/1"
-}
module Gargantext.Config where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Map (Map)
import Data.Map as DM
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))

endConfig :: EndConfig
endConfig = endConfig' V10

endConfig' :: ApiVersion -> EndConfig
endConfig' v = { front : frontCaddy
               , back  : backDev v }

-- | Default Root on shared database to develop
-- until authentication implementation
-- (Default Root will be given after authentication)
defaultRoot :: Int
defaultRoot = 347474
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
pathUrl c nt@(Children _ _ _) i = pathUrl c Node i <> "/" <> show nt
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
              | Children NodeType Offset Limit
              | Corpus
              | CorpusV3
              | Dashboard
              | Url_Document
              | Error
              | Folder
              | Graph
              | Individu
              | Node
              | Tree
data End = Back | Front
type Id  = Int

type Limit  = Int
type Offset = Int

------------------------------------------------------------
data ApiVersion = V10 | V11
instance showApiVersion :: Show ApiVersion where
  show V10 = "v1.0"
  show V11 = "v1.1"
------------------------------------------------------------
------------------------------------------------------------
urlConfig :: NodeType -> Url
urlConfig Annuaire  = show Annuaire
urlConfig nt@(Children _ _ _) = show nt
urlConfig Corpus    = show Corpus
urlConfig CorpusV3  = show CorpusV3
urlConfig Dashboard = show Dashboard
urlConfig Url_Document  = show Url_Document
urlConfig Error     = show Error
urlConfig Folder    = show Folder
urlConfig Graph     = show Graph
urlConfig Individu  = show Individu
urlConfig Node      = show Node
urlConfig NodeUser  = show NodeUser
urlConfig Tree      = show Tree
------------------------------------------------------------
instance showNodeType :: Show NodeType where
  show Annuaire  = "annuaire"
  show Corpus    = "corpus"
  show CorpusV3  = "corpus"
  show Dashboard = "dashboard"
  show Url_Document  = "document"
  show Error     = "ErrorNodeType"
  show Folder    = "folder"
  show Graph     = "graph"
  show Individu  = "individu"
  show Node      = "node"
  show NodeUser  = "user"
  show Tree      = "tree"
  show (Children t o l) = "children?type=" <> show t <> "&offset=" <> show o <> "&limit=" <> show l

-- | TODO : where is the Read Class ?
-- instance readNodeType :: Read NodeType where
readNodeType :: String -> NodeType
readNodeType "Annuaire"   = Annuaire
readNodeType "Children"   = (Children Node 0 0)
readNodeType "Dashboard"  = Dashboard
readNodeType "Document"   = Url_Document
readNodeType "Folder"     = Folder
readNodeType "Graph"      = Graph
readNodeType "Individu"   = Individu
readNodeType "Node"       = Node
readNodeType "NodeCorpus" = Corpus
readNodeType "NodeCorpusV3" = CorpusV3
readNodeType "NodeUser"   = NodeUser
readNodeType "Tree"       = Tree
readNodeType _            = Error
------------------------------------------------------------
instance ordNodeType :: Ord NodeType where
  compare n1 n2 = compare (show n1) (show n2)

instance eqNodeType :: Eq NodeType where
  eq n1 n2  = eq (show n1) (show n2)
------------------------------------------------------------
instance decodeJsonNodeType :: DecodeJson NodeType where
  decodeJson json = do
    obj <- decodeJson json
    pure $ readNodeType obj
