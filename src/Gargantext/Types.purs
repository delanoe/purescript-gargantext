module Gargantext.Types where

import Data.Argonaut ( class DecodeJson, decodeJson, class EncodeJson, encodeJson
                     , jsonEmptyObject, (:=), (~>), (.?), (.??) )
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Gargantext.Prelude

data TermSize = MonoTerm | MultiTerm

derive instance eqTermSize :: Eq TermSize

instance showTermSize :: Show TermSize where
  show MonoTerm  = "MonoTerm"
  show MultiTerm = "MultiTerm"

readTermSize :: String -> Maybe TermSize
readTermSize "MonoTerm"  = Just MonoTerm
readTermSize "MultiTerm" = Just MultiTerm
readTermSize _           = Nothing

termSizes :: Array { desc :: String, mval :: Maybe TermSize }
termSizes = [ { desc: "All types",        mval: Nothing        }
            , { desc: "One-word terms",   mval: Just MonoTerm  }
            , { desc: "Multi-word terms", mval: Just MultiTerm }
            ]

data TermList = GraphTerm | StopTerm | CandidateTerm
-- TODO use generic JSON instance

derive instance eqTermList :: Eq TermList
derive instance ordTermList :: Ord TermList

instance encodeJsonTermList :: EncodeJson TermList where
  encodeJson GraphTerm     = encodeJson "GraphTerm"
  encodeJson StopTerm      = encodeJson "StopTerm"
  encodeJson CandidateTerm = encodeJson "CandidateTerm"

instance decodeJsonTermList :: DecodeJson TermList where
  decodeJson json = do
    s <- decodeJson json
    case s of
      "GraphTerm"     -> pure GraphTerm
      "StopTerm"      -> pure StopTerm
      "CandidateTerm" -> pure CandidateTerm
      _               -> Left "Unexpected list name"

type ListTypeId = Int

listTypeId :: TermList -> ListTypeId
listTypeId GraphTerm     = 1
listTypeId StopTerm      = 2
listTypeId CandidateTerm = 3

instance showTermList :: Show TermList where
  show GraphTerm     = "GraphTerm"
  show StopTerm      = "StopTerm"
  show CandidateTerm = "CandidateTerm"

readTermList :: String -> Maybe TermList
readTermList "GraphTerm"     = Just GraphTerm
readTermList "StopTerm"      = Just StopTerm
readTermList "CandidateTerm" = Just CandidateTerm
readTermList _               = Nothing

termLists :: Array { desc :: String, mval :: Maybe TermList }
termLists = [ { desc: "All terms",   mval: Nothing      }
            , { desc: "Graph terms",   mval: Just GraphTerm   }
            , { desc: "Stop terms",  mval: Just StopTerm  }
            , { desc: "Candidate terms", mval: Just CandidateTerm }
            ]

