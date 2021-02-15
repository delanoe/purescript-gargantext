module Gargantext.Components.Score where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>), encodeJson)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import DOM.Simple.Console (log2)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, sessionId, get, delete, put)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR

type Score = Int
type DocID = Int

thisModule :: String
thisModule = "Gargantext.Components.Score"

type Props = (
    docId       ::DocID
  , nodeId      :: GT.NodeID
  , score       :: Maybe Score
  , session     :: Session
  , tableReload :: GUR.ReloadS
  )

type Choice = Maybe Score

scoreEl :: R2.Component Props
scoreEl = R.createElement scoreElCpt

scoreElCpt :: R.Component Props
scoreElCpt = R.hooksComponentWithModule thisModule "scoreEl" cpt
  where
    cpt { docId, nodeId, score, session, tableReload } _ = do
      pure $ R2.select { className: "form-control"
                       , defaultValue: showChoice score
                       , on: { change: onChange session nodeId docId tableReload }
                       } (map option choices)

    onChange session nodeId docId reloadS e = do
      -- TODO change score via api
      let query = ScoreQuery { nodeIds: [ docId ]
                             , score: readChoice $ R.unsafeEventValue e }
      launchAff_ $ do
        _ <- putScore session nodeId query
        liftEffect $ GUR.bump reloadS

    option :: Choice -> R.Element
    option c = H.option { value: showChoice c } [ H.text $ showChoice c ]

    choices = [ Nothing
              , Just 5
              , Just 10
              , Just 15 ]

    showChoice :: Choice -> String
    showChoice Nothing = "-"
    showChoice (Just c) = show c

    readChoice = fromString


newtype ScoreQuery =
  ScoreQuery { nodeIds :: Array DocID
             , score   :: Choice
             }

instance encodeJsonScoreQuery :: EncodeJson ScoreQuery where
  encodeJson (ScoreQuery post) =
      "nts_nodesId" := post.nodeIds
    ~> "nts_score" := encodeJson post.score
    ~> jsonEmptyObject

putScore :: Session -> GT.NodeID -> ScoreQuery -> Aff (Array Int)
putScore session nodeId = put session $ scoreRoute nodeId
  where
    scoreRoute :: GT.NodeID -> SessionRoute
    scoreRoute nodeId = NodeAPI GT.Node (Just nodeId) "score"
