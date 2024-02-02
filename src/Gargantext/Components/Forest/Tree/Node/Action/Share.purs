module Gargantext.Components.Forest.Tree.Node.Action.Share where

import Gargantext.Prelude

import Data.Array (filter, nub)
import Data.Generic.Rep (class Generic)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), contains, trim)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Elevation(Level1))
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action)
import Gargantext.Components.Forest.Tree.Node.Action.Types as Action
import Gargantext.Components.Forest.Tree.Node.Tools as Tools
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Components.InputWithAutocomplete (inputWithAutocomplete)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, get, post)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.SimpleJSON as GUSJ
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Share"

------------------------------------------------------------------------
shareReq :: Session -> ID -> ShareNodeParams -> AffRESTError ID
shareReq session nodeId =
  post session $ GR.NodeAPI GT.Node (Just nodeId) "share"

getCompletionsReq :: { session :: Session } -> AffRESTError (Array String)
getCompletionsReq { session }  =
  get session GR.Members


------------------------------------------------------------------------
data ShareNodeParams = ShareTeamParams   { username :: String }
               | SharePublicParams { node_id  :: Int    }
derive instance Eq ShareNodeParams
derive instance Generic ShareNodeParams _
instance JSON.ReadForeign ShareNodeParams where readImpl = GUSJ.taggedSumRep
instance JSON.WriteForeign ShareNodeParams where
  writeImpl (ShareTeamParams { username }) = JSON.writeImpl { "type": "ShareTeamParams"
                                                            , username }
  writeImpl (SharePublicParams { node_id }) = JSON.writeImpl { "type": "SharePublicParams"
                                                             , node_id }
instance Show ShareNodeParams where show = genericShow

------------------------------------------------------------------------
type ShareNode =
  ( id :: ID
  , session :: Session )

shareNode :: R2.Component ShareNode
shareNode = R.createElement shareNodeCpt
shareNodeCpt :: R.Component ShareNode
shareNodeCpt = R2.hereComponent here "shareNode" hCpt where
  hCpt hp { id, session } _ = do
    useLoader { errorHandler: Nothing
              , herePrefix: hp
              , loader: getCompletionsReq
              , path: { session }
              , render: \completions -> shareNodeInner { completions, id, session } []
              }

type ShareNodeInner =
  ( completions :: Array String
  | ShareNode
  )

shareNodeInner :: R2.Component ShareNodeInner
shareNodeInner = R.createElement shareNodeInnerCpt
shareNodeInnerCpt :: R.Component ShareNodeInner
shareNodeInnerCpt = here.component "shareNodeInner" cpt
  where
    cpt { completions, id, session } _ = do
      state' /\ state <- R2.useBox' ""
      text' /\ text <- R2.useBox' ""
      mError' /\ mError <- R2.useBox' Nothing

      pure $ Tools.panel { mError: mError' }
        [ inputWithAutocomplete { autoFocus: true
                                , autocompleteSearch
                                , classes: "share-users-completions d-flex align-items-center"
                                , onAutocompleteClick
                                , onEnterPress: onEnterPress text mError
                                , pattern: "^\\S+$"  -- pattern doesn't allow space characters
                                , placeholder: "username or email"
                                , state
                                , title }
          [ B.iconButton { callback: \_ -> onEnterPress text mError state'
                         , elevation: Level1
                         , name: "send"
                         , title: "Submit" } ]

        -- footer
        , H.div {} [ H.text text' ] ]
      where
        autocompleteSearch input = pure $ nub $ filter (contains (Pattern input)) completions
        onAutocompleteClick _ = pure unit
        onEnterPress text mError val = do
          T.write_ Nothing mError
          launchAff_ do
            eRes <- shareReq session id $ ShareTeamParams { username: val }
            liftEffect $ case eRes of
              Left err -> do
                T.write_ (Just $ show err) mError
              Right _ -> do
                T.write_ ("Invited " <> val <> " to the team") text
                T.write_ Nothing mError
        title = "Enter a username or an email address (space characters are not allowed)"
------------------------------------------------------------------------
publishNode :: R2.Component SubTreeParamsIn
publishNode = R.createElement publishNodeCpt
publishNodeCpt :: R.Component SubTreeParamsIn
publishNodeCpt = here.component "publishNode" cpt
  where
    cpt { dispatch, id, nodeType, session, subTreeParams } _ = do
      action <- T.useBox (Action.SharePublic { params: Nothing })
      action' <- T.useLive T.unequal action

      let button = case action' of
              Action.SharePublic { params } ->
                R2.fromMaybe params $
                  \val -> Tools.submitButton { action: Action.SharePublic {params: Just val}
                                             , dispatch }
              _   -> H.div {} []

      pure $ Tools.panel { mError: Nothing }
        [ subTreeView { action
                      , dispatch
                      , id
                      , nodeType
                      , session
                      , subTreeParams
                      } []

        -- footer
        , button ]
