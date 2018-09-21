module Gargantext.Config where

import Prelude (class Eq, class Ord, class Show, compare, eq, show, (<>))

import Data.Map (Map)
import Data.Map as DM
-- import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))


type FrontEndConfig = { proto   :: String
                      , port    :: Int
                      , address :: String
                      , urls    :: Map NodeType Url
                      }
config :: FrontEndConfig
config = { proto    : "http://"
                        , port    : 2015
                        , address : "localhost"
                        , urls    : urlConfig
                      }

-- | Basic starting example
-- for more complex urls, use urlConfig and smart constructors

toUrl :: NodeType -> Id -> Url
toUrl nt i = config.proto <> config.address <> ":" <> show config.port <> "/" <> show nt <> "/" <> show i
  --where
    --path = maybe "error" (DM.lookup nt config.urls)

------------------------------------------------------------
type Url = String
type Id  = Int
------------------------------------------------------------
data NodeType = UserPage | Corpus | Document | Annuaire | Individu | Project

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

trivial :: NodeType -> Tuple NodeType Url
trivial n = Tuple n (show n)

urlConfig :: Map NodeType Url
urlConfig = DM.fromFoldable [ Tuple UserPage "user"
                            , trivial Corpus
                            , trivial Document
                            , trivial Annuaire
                            , trivial Individu
                            ]

