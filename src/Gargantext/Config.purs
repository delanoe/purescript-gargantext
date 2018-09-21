{- | Main Configuration of Gargantext Front-End

The main function to use for internal link in the Front-End
developpement is : toUrl.

* Example usage:
- for Mock config:
    - toUrl Corpus 3 == "http://localhost:2015/corpus/3"
    - (this mode supposes you have the mock haskell backend running)

- for Dev config:
    - toUrl Corpus 3 == "http://localhost:8008/corpus/3"
    - (this mode supposes you have the dev haskell backend running)

- for Prod config:
    - toUrl Corpus 3 == "http://gargantext.org:8080/corpus/3"
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
-- | Versions wille used later after the release
-- data ApiVersion = V1

-- | Main options of the configuration
data Mode = Mock | Dev | Prod

config :: FrontEndConfig
config = mkConfig Mock

mkAdress :: Mode -> String
mkAdress Mock = "localhost"
mkAdress Dev  = "localhost"
mkAdress Prod = "gargantext.org"

mkPort :: Mode -> Int
mkPort Mock = 2015
mkPort Dev  = 8008
mkPort Prod = 8080
------------------------------------------------------------
urlConfig :: Map NodeType Url
urlConfig = DM.fromFoldable [ Tuple UserPage "user"
                            , easy Corpus
                            , easy Document
                            , easy Annuaire
                            , easy Individu
                            ]
    where
      easy :: NodeType -> Tuple NodeType Url
      easy n = Tuple n (show n)
------------------------------------------------------------
type FrontEndConfig = { proto   :: String
                      , port    :: Int
                      , address :: String
                      , urls    :: Map NodeType Url
                      }
mkConfig :: Mode -> FrontEndConfig
mkConfig mode = { proto   : "http://"
                , address : mkAdress mode
                , port    : mkPort   mode
                , urls    : urlConfig
                }
------------------------------------------------------------
------------------------------------------------------------
-- | Main function to use in the Front-End developpement
-- for more complex urls, use urlConfig and smart constructors
toUrl :: NodeType -> Id -> Url
toUrl nt i = config.proto <> config.address <> ":" <> show config.port <> "/" <> path
  where
    path    = subPath <> "/" <> show i
    subPath = maybe "error" identity (DM.lookup nt config.urls)

------------------------------------------------------------
type Url = String
type Id  = Int
------------------------------------------------------------
data NodeType = UserPage | Corpus | Document | Annuaire | Individu | Project
------------------------------------------------------------
instance showNodeType :: Show NodeType where
  show UserPage = "userPage"
  show Corpus   = "corpus"
  show Document = "document"
  show Annuaire = "annuaire"
  show Individu = "individu"
  show Project  = "project"

instance ordNodeType :: Ord NodeType where
  compare n1 n2 = compare (show n1) (show n2)

instance eqNodeType :: Eq NodeType where
  eq n1 n2  = eq (show n1) (show n2)
------------------------------------------------------------
------------------------------------------------------------
