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

------------------------------------------------------------
-- | Versions will used later after the release
data ApiVersion = V10 | V11

instance showApiVersion :: Show ApiVersion where
  show V10 = "v1.0"
  show V11 = "v1.1"

data End = Back | Front

-- | Main options of the configuration
data Mode = Mock | Dev | Prod

config :: FrontEndConfig
config = mkConfig Dev V10

mkAdress :: Mode -> String
mkAdress Mock = "localhost"
mkAdress Dev  = "localhost"
mkAdress Prod = "gargantext.org"

mkPort :: Mode -> Int
mkPort Mock = 2015
mkPort Dev  = 8008
mkPort Prod = 8080

mkProto :: Mode -> String
mkProto Mock = "http://"
mkProto Dev  = "http://"
mkProto Prod = "https://"

------------------------------------------------------------
urlConfig :: Map NodeType Url
urlConfig = DM.fromFoldable [ Tuple UserPage "user"
                            , easy Corpus
                            , easy Project
                            , easy Document
                            , easy Annuaire
                            , easy Individu
                            , easy Tree
                            ]
    where
      easy :: NodeType -> Tuple NodeType Url
      easy n = Tuple n (show n)
------------------------------------------------------------
type FrontEndConfig = { proto      :: String
                      , port       :: Int
                      , address    :: String
                      , apiVersion :: ApiVersion
                      , urls       :: Map NodeType Url
                      }
mkConfig :: Mode -> ApiVersion -> FrontEndConfig
mkConfig mode v = { proto      : mkProto  mode
                  , address    : mkAdress mode
                  , port       : mkPort   mode
                  , apiVersion : v
                  , urls       : urlConfig
                  }
------------------------------------------------------------
------------------------------------------------------------
-- | Main function to use in the Front-End developpement
-- for more complex urls, use urlConfig and smart constructors
toUrl :: End -> NodeType -> Id -> Url
toUrl end nt i = config.proto <> config.address <> ":" <> show config.port <> end' <> path
  where
    end' = case end of
                Back  -> "/api/" <> show config.apiVersion <> "/"
                Front -> "/"
    path    = subPath <> "/" <> show i
    subPath = maybe "errorSubPath" identity (DM.lookup nt config.urls)

------------------------------------------------------------
type Url = String
type Id  = Int
------------------------------------------------------------
data NodeType = UserPage | Corpus | Document | Annuaire | Individu | Project | Tree | Error
------------------------------------------------------------
instance showNodeType :: Show NodeType where
  show UserPage = "user"
  show Corpus   = "corpus"
  show Document = "document"
  show Annuaire = "annuaire"
  show Individu = "individu"
  show Project  = "project"
  show Tree     = "tree"
  show Error    = "errNodeType"

readNodeType :: String -> NodeType
readNodeType "NodeUser"   = UserPage
readNodeType "NodeCorpus" = Corpus
readNodeType "Document" = Document
readNodeType "Annuaire" = Annuaire
readNodeType "Individu" = Individu
readNodeType "Project"  = Project
readNodeType "Tree"     = Tree
readNodeType _          = Error

instance ordNodeType :: Ord NodeType where
  compare n1 n2 = compare (show n1) (show n2)

instance eqNodeType :: Eq NodeType where
  eq n1 n2  = eq (show n1) (show n2)
------------------------------------------------------------
