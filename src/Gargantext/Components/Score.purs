module Gargantext.Components.Score where

import Gargantext.Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, put)
import Gargantext.Types as GT
import Gargantext.Utils.Array as GUA
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as GUT
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON

type Score = Int
type DocID = Int

thisModule :: String
thisModule = "Gargantext.Components.Score"

type Props = (
    docId       ::DocID
  , key :: String
  , nodeId      :: GT.NodeID
  , score       :: Maybe Score
  , session     :: Session
  , tableReload :: GUT.ReloadS
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
        liftEffect $ GUT.reload reloadS

    option :: Choice -> R.Element
    option c = H.option { value: showChoice c } [ H.text $ showChoice c ]

    choices = [ Nothing ] <> (Just <$> GUA.range 5 100 5)

    showChoice :: Choice -> String
    showChoice Nothing = "-"
    showChoice (Just c) = show c

    readChoice = fromString


newtype ScoreQuery =
  ScoreQuery { nodeIds :: Array DocID
             , score   :: Choice
             }

instance JSON.WriteForeign ScoreQuery where
  writeImpl (ScoreQuery post) = JSON.writeImpl { nts_nodesId: post.nodeIds
                                               , nts_score: post.score }

putScore :: Session -> GT.NodeID -> ScoreQuery -> AffRESTError (Array Int)
putScore session nodeId = put session $ scoreRoute nodeId
  where
    scoreRoute :: GT.NodeID -> SessionRoute
    scoreRoute nodeId = NodeAPI GT.Node (Just nodeId) "score"
