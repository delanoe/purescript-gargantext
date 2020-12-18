module Gargantext.Components.Nodes.Annuaire where

import Prelude (bind, const, identity, pure, show, ($), (<$>), (<>))
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Array as A
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Sequence as Seq
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types as CT
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Table as T
import Gargantext.Components.Table.Types as T
import Gargantext.Ends (url, Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, get)
import Gargantext.Types (NodeType(..), AffTableResult, TableResult)
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.Annuaire"

newtype IndividuView =
  CorpusView
  { id      :: Int
  , name    :: String
  , role    :: String
  , company :: String }

--toRows :: AnnuaireTable -> Array (Maybe Contact)
--toRows (AnnuaireTable a) = a.annuaireTable

-- | Top level layout component. Loads an annuaire by id and renders
-- | the annuaire using the result
type LayoutProps = (
    frontends :: Frontends
  , nodeId :: Int
  , session :: Session
  )

annuaireLayout :: Record LayoutProps -> R.Element
annuaireLayout props = R.createElement annuaireLayoutCpt props []

annuaireLayoutCpt :: R.Component LayoutProps
annuaireLayoutCpt = R.hooksComponentWithModule thisModule "annuaireLayout" cpt
  where
    cpt { frontends, nodeId, session } _ = do
      let sid = sessionId session

      pure $ annuaireLayoutWithKey { frontends, key: show sid <> "-" <> show nodeId, nodeId, session }

type KeyLayoutProps = (
  key :: String
  | LayoutProps
  )

annuaireLayoutWithKey :: Record KeyLayoutProps -> R.Element
annuaireLayoutWithKey props = R.createElement annuaireLayoutWithKeyCpt props []

annuaireLayoutWithKeyCpt :: R.Component KeyLayoutProps
annuaireLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "annuaireLayoutWithKey" cpt
  where
    cpt { frontends, nodeId, session } _ = do
      path <- R.useState' nodeId

      useLoader (fst path) (getAnnuaireInfo session) $
        \info -> annuaire { frontends, info, path, session }

type AnnuaireProps =
  ( session   :: Session
  , path      :: R.State Int
  , info      :: AnnuaireInfo
  , frontends :: Frontends
  )

-- | Renders a basic table and the page loader
annuaire :: Record AnnuaireProps -> R.Element
annuaire props = R.createElement annuaireCpt props []

-- Abuses closure to work around the Loader
annuaireCpt :: R.Component AnnuaireProps
annuaireCpt = R.hooksComponentWithModule thisModule "annuaire" cpt
  where
    cpt {session, path, info: info@(AnnuaireInfo {name, date: date'}), frontends} _ = do
      pagePath <- R.useState' $ initialPagePath (fst path)

      cacheState <- R.useState' NT.CacheOff

      pure $ R.fragment
        [ T.tableHeaderLayout { afterCacheStateChange: \_ -> launchAff_ $ clearCache unit
                              , cacheState
                              , date
                              , desc: name
                              , key: "annuaire-" <> (show $ fst cacheState)
                              , query: ""
                              , title: name
                              , user: "" }
          , H.p {} []
          , H.div {className: "col-md-3"}
            [ H.text "    Filter ", H.input { className: "form-control", style } ]
          , H.br {}
          , pageLayout { info, session, pagePath, frontends} ]
      where
        date = "Last update: " <> date'
        style = {width: "250px", display: "inline-block"}
        initialPagePath nodeId = {nodeId, params: T.initialParams}

type PagePath = { nodeId :: Int
                , params :: T.Params
                }

type PageLayoutProps =
  ( session      :: Session
  , frontends    :: Frontends
  , info         :: AnnuaireInfo
  , pagePath     :: R.State PagePath
  )

pageLayout :: Record PageLayoutProps -> R.Element
pageLayout props = R.createElement pageLayoutCpt props []

pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = R.hooksComponentWithModule thisModule "pageLayout" cpt
  where
    cpt {info, frontends, pagePath, session} _ = do
      useLoader (fst pagePath) (loadPage session) $
        \table -> page {session, table, frontends, pagePath}

type PageProps = 
  ( session :: Session
  , frontends :: Frontends
  , pagePath :: R.State PagePath
  -- , info :: AnnuaireInfo
  , table :: TableResult CT.NodeContact
  )

page :: Record PageProps -> R.Element
page props = R.createElement pageCpt props []

pageCpt :: R.Component PageProps
pageCpt = R.hooksComponentWithModule thisModule "page" cpt
  where
    cpt { session, pagePath, frontends
        , table: ({count: totalRecords, docs})} _ = do
      pure $ T.table { syncResetButton : [ H.div {} [] ]
                     , rows, params, container
                     , colNames, totalRecords
                     , wrapColElts
                     }
      where
        path = fst pagePath
        rows = (\c -> {
                   row: contactCells { annuaireId: (fst pagePath).nodeId
                                     , frontends
                                     , contact: c
                                     , session }
                   , delete: false }) <$> Seq.fromFoldable docs
        container = T.defaultContainer { title: "Annuaire" } -- TODO
        colNames = T.ColumnName <$> [ "", "First Name", "Last Name", "Company", "Lab", "Role"]
        wrapColElts = const identity
        setParams f = snd pagePath $ \pp@{params: ps} ->
          pp {params = f ps}
        params = (fst pagePath).params /\ setParams

type AnnuaireId = Int

type ContactCellsProps =
  ( annuaireId :: AnnuaireId
  , contact    :: CT.NodeContact
  , frontends  :: Frontends
  , session   :: Session
  )

contactCells :: Record ContactCellsProps -> R.Element
contactCells p = R.createElement contactCellsCpt p []

contactCellsCpt :: R.Component ContactCellsProps
contactCellsCpt = R.hooksComponentWithModule thisModule "contactCells" cpt
  where
    cpt { annuaireId
        , contact: (CT.NodeContact { id, hyperdata: (CT.HyperdataContact {who : Nothing}) })
        , frontends
        , session } _ =
      pure $ T.makeRow [ H.text ""
                       , H.span {} [ H.text "Name" ]
                       --, H.a { href, target: "blank" } [ H.text $ fromMaybe "name" contact.title ]
                       , H.text "No ContactWhere"
                       , H.text "No ContactWhereDept"
                       , H.div { className: "nooverflow"}
                               [ H.text "No ContactWhereRole" ]
                       ]
    cpt { annuaireId
        , contact: (CT.NodeContact { id
                               , hyperdata: ( CT.HyperdataContact { who : Just (CT.ContactWho { firstName
                                                                                              , lastName
                                                                                              }
                                                                               )
                                                                  }
                                            )
                               }
                   )
        , frontends
        , session } _ = do

        pure $ T.makeRow [
          H.text ""
          , H.text $ fromMaybe "First Name" firstName
          , H.text $ fromMaybe "First Name" lastName
          , H.text  "CNRS"
          -- , H.a { href } [ H.text $ fromMaybe "name" contact.title ]
            --, H.a { href, target: "blank" } [ H.text $ fromMaybe "name" contact.title ]
          --, H.text $ maybe "No ContactWhere" contactWhereOrg  (A.head $ ou)
         -- , H.text $ maybe "No ContactWhereDept" contactWhereDept (A.head $ ou)
         -- , H.div {className: "nooverflow"} [
         --     H.text $ maybe "No ContactWhereRole" contactWhereRole (A.head $ ou)
            ]
          where
            --nodepath = NodePath (sessionId session) NodeContact (Just id)
            nodepath = Routes.ContactPage (sessionId session) annuaireId id
            href = url frontends nodepath

            contactWhereOrg (CT.ContactWhere { organization: [] }) = "No Organization"
            contactWhereOrg (CT.ContactWhere { organization: orga }) =
              fromMaybe "No orga (list)" (A.head orga)
            contactWhereDept (CT.ContactWhere { labTeamDepts : [] }) = "Empty Dept"
            contactWhereDept (CT.ContactWhere { labTeamDepts : dept }) =
              fromMaybe "No Dept (list)" (A.head dept)
            contactWhereRole (CT.ContactWhere { role: Nothing }) = "Empty Role"
            contactWhereRole (CT.ContactWhere { role: Just role }) = role


data HyperdataAnnuaire = HyperdataAnnuaire
  { title :: Maybe String
  , desc  :: Maybe String }

instance decodeHyperdataAnnuaire :: DecodeJson HyperdataAnnuaire where
  decodeJson json = do
    obj   <- decodeJson json
    title <- obj .:? "title"
    desc  <- obj .:? "desc"
    pure $ HyperdataAnnuaire { title, desc }

------------------------------------------------------------------------------
newtype AnnuaireInfo = AnnuaireInfo { id        :: Int
                                    , typename  :: Int
                                    , userId    :: Int
                                    , parentId  :: Int
                                    , name      :: String
                                    , date      :: String
                                    , hyperdata :: HyperdataAnnuaire
                                    }

instance decodeAnnuaireInfo :: DecodeJson AnnuaireInfo where
  decodeJson json = do
    obj <- decodeJson json
    id        <- obj .: "id"
    typename  <- obj .: "typename"
    userId    <- obj .: "userId"
    parentId  <- obj .: "parentId"
    name      <- obj .: "name"
    date      <- obj .: "date"
    hyperdata <- obj .: "hyperdata"
    pure $ AnnuaireInfo { id : id
                        , typename : typename
                        , userId   : userId
                        , parentId : parentId
                        , name     : name
                        , date     : date
                        , hyperdata: hyperdata
                        }


--newtype AnnuaireTable  = AnnuaireTable  { annuaireTable :: Array (Maybe Contact)}

--instance decodeAnnuaireTable :: DecodeJson AnnuaireTable where
--  decodeJson json = do
--    rows <- decodeJson json
--    pure $ AnnuaireTable { annuaireTable : rows}

------------------------------------------------------------------------

loadPage :: Session -> PagePath -> AffTableResult CT.NodeContact
loadPage session {nodeId, params: { offset, limit, orderBy }} =
    get session children
 -- TODO orderBy
 -- where
 --   convOrderBy (T.ASC  (T.ColumnName "Name")) = NameAsc
 --   convOrderBy (T.DESC (T.ColumnName "Name")) = NameDesc
 --   ...
 --   convOrderBy _ = NameAsc -- TODO

  where
    children = Children NodeContact offset limit Nothing {-(convOrderBy <$> orderBy)-} (Just nodeId)

getAnnuaireInfo :: Session -> Int -> Aff AnnuaireInfo
getAnnuaireInfo session id = get session (NodeAPI Node (Just id) "")

