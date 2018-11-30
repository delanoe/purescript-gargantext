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
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as DM
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import Gargantext.Types

endConfig :: EndConfig
endConfig = endConfig' V10

endConfig' :: ApiVersion -> EndConfig
endConfig' v = { front : frontRelative
               , back  : backLocal v }

------------------------------------------------------------------------
frontRelative :: Config
frontRelative = { baseUrl: ""
                , prePath: "/#/"
                }

frontCaddy :: Config
frontCaddy = { baseUrl: "http://localhost:2015"
             , prePath: "/#/"
             }

frontHaskell :: Config
frontHaskell = { baseUrl: "http://localhost:8008"
               , prePath: "/#/"
               }

frontDev :: Config
frontDev = { baseUrl: "https://dev.gargantext.org"
           , prePath: "/#/"
           }

frontProd :: Config
frontProd = { baseUrl: "https://gargantext.org"
            , prePath: "/#/"
            }

------------------------------------------------------------------------

backLocal :: ApiVersion -> Config
backLocal v = { baseUrl: "http://localhost:8008"
              , prePath: "/api/" <> show v <> "/"
              }

backDev :: ApiVersion -> Config
backDev v = { baseUrl: "https://dev.gargantext.org"
            , prePath: "/api/" <> show v <> "/"
            }

backProd :: ApiVersion -> Config
backProd v = { baseUrl: "https://gargantext.org"
             , prePath: "/api/" <> show v <> "/"
             }
------------------------------------------------------------------------

type EndConfig = { front :: Config
                 , back  :: Config
                 }

type Config = { baseUrl :: String
              , prePath :: String
              }

------------------------------------------------------------
type UrlBase  = String
type UrlPath  = String
type UrlParam = String
type Url      = String

doUrl :: UrlBase -> UrlPath -> UrlParam -> Url
doUrl b p ps = b <> p <> ps

endOf :: forall cfg. End -> { front :: cfg, back :: cfg } -> cfg
endOf Back  = _.back
endOf Front = _.front

endBaseUrl :: End -> EndConfig -> UrlBase
endBaseUrl end c = (endOf end c).baseUrl

endPathUrl :: End -> EndConfig -> Path -> Maybe Id -> UrlPath
endPathUrl end = pathUrl <<< endOf end

pathUrl :: Config -> Path -> Maybe Id -> UrlPath
pathUrl c (Tab t o l s) i =
    pathUrl c (NodeAPI Node) i <>
      "/" <> "table?view=" <> show t <> "&offset=" <> show o
          <> "&limit=" <> show l <> os
  where
    os = maybe "" (\x -> "&order=" <> show x) s
pathUrl c (Ngrams t listid) i =
    pathUrl c (NodeAPI Node) i <> "/" <> "listGet?ngramsType=" <> show t <> listid'
  where
    listid' = maybe "" (\x -> "&list=" <> show x) listid
pathUrl c Auth Nothing = c.prePath <> "auth"
pathUrl c Auth (Just _) = "impossible" -- TODO better types
pathUrl c (NodeAPI nt) i = c.prePath <> nodeTypeUrl nt <> (maybe "" (\i' -> "/" <> show i') i)
------------------------------------------------------------

class ToUrl a where
  toUrl :: End -> a -> Maybe Id -> Url

instance toUrlNodeType :: ToUrl NodeType where
  toUrl e nt i = toUrl e (NodeAPI nt) i

instance toUrlPath :: ToUrl Path where
  toUrl e p i = doUrl base path params
    where
      base   = endBaseUrl e endConfig
      path   = endPathUrl e endConfig p i
      params = ""
------------------------------------------------------------

data NodeType = NodeUser
              | Annuaire
              | Corpus
              | CorpusV3
              | Dashboard
              | Url_Document
              | Error
              | Folder
              | Graph
              | Individu
              | Node
              | Nodes
              | Tree

data Path
  = Auth
  | Tab TabType Offset Limit (Maybe OrderBy)
  | Ngrams TabType (Maybe TermList)
  | NodeAPI NodeType

data End = Back | Front
type Id  = Int

type Limit  = Int
type Offset = Int
data OrderBy = DateAsc  | DateDesc
             | TitleAsc | TitleDesc
             | FavDesc  | FavAsc

derive instance genericOrderBy :: Generic OrderBy _

instance showOrderBy :: Show OrderBy where
  show = genericShow

------------------------------------------------------------
data ApiVersion = V10 | V11
instance showApiVersion :: Show ApiVersion where
  show V10 = "v1.0"
  show V11 = "v1.1"
------------------------------------------------------------

data TabType = TabDocs | TabTerms | TabSources | TabAuthors | TabInstitutes | TabTrash

instance showTabType :: Show TabType where
  show TabDocs       = "Docs"
  show TabTerms      = "Terms"
  show TabSources    = "Sources"
  show TabAuthors    = "Authors"
  show TabInstitutes = "Institutes"
  show TabTrash      = "Trash"

------------------------------------------------------------
nodeTypeUrl :: NodeType -> Url
nodeTypeUrl Annuaire  = "annuaire"
nodeTypeUrl Corpus    = "corpus"
nodeTypeUrl CorpusV3  = "corpus"
nodeTypeUrl Dashboard = "dashboard"
nodeTypeUrl Url_Document  = "document"
nodeTypeUrl Error     = "ErrorNodeType"
nodeTypeUrl Folder    = "folder"
nodeTypeUrl Graph     = "graph"
nodeTypeUrl Individu  = "individu"
nodeTypeUrl Node      = "node"
nodeTypeUrl Nodes      = "nodes"
nodeTypeUrl NodeUser  = "user"
nodeTypeUrl Tree      = "tree"

readNodeType :: String -> NodeType
readNodeType "NodeAnnuaire"   = Annuaire
readNodeType "NodeDashboard"  = Dashboard
readNodeType "Document"   = Url_Document
readNodeType "NodeFolder"     = Folder
readNodeType "NodeGraph"      = Graph
readNodeType "Individu"   = Individu
readNodeType "Node"       = Node
readNodeType "Nodes"       = Nodes
readNodeType "NodeCorpus" = Corpus
readNodeType "NodeCorpusV3" = CorpusV3
readNodeType "NodeUser"   = NodeUser
readNodeType "Tree"       = Tree
readNodeType _            = Error
{-
------------------------------------------------------------
instance ordNodeType :: Ord NodeType where
  compare n1 n2 = compare (show n1) (show n2)

instance eqNodeType :: Eq NodeType where
  eq n1 n2  = eq (show n1) (show n2)
-}
------------------------------------------------------------
instance decodeJsonNodeType :: DecodeJson NodeType where
  decodeJson json = do
    obj <- decodeJson json
    pure $ readNodeType obj
