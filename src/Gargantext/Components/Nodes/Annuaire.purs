module Gargantext.Components.Nodes.Annuaire where

import Prelude (bind, const, identity, pure, ($), (<$>), (<>))
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Array (head)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types as CT
import Gargantext.Components.Table as T
import Gargantext.Ends (url, Frontends)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, get)
import Gargantext.Types (NodeType(..), AffTableResult, TableResult)
import Gargantext.Hooks.Loader (useLoader)

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
type LayoutProps = ( nodeId :: Int, session :: Session, frontends :: Frontends )

annuaireLayout :: Record LayoutProps -> R.Element
annuaireLayout props = R.createElement annuaireLayoutCpt props []

annuaireLayoutCpt :: R.Component LayoutProps
annuaireLayoutCpt = R.hooksComponent "G.P.Annuaire.annuaireLayout" cpt
  where
    cpt {nodeId, session, frontends} _ = do
      path <- R.useState' nodeId
      useLoader (fst path) (getAnnuaireInfo session) $
        \info -> annuaire {session, path, info, frontends}

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
annuaireCpt = R.hooksComponent "G.P.Annuaire.annuaire" cpt
  where
    cpt {session, path, info: info@(AnnuaireInfo {name, date: date'}), frontends} _ = do
      pagePath <- R.useState' $ initialPagePath (fst path)

      pure $ R.fragment
        [ T.tableHeaderLayout headerProps
          , H.p {} []
          , H.div {className: "col-md-3"}
            [ H.text "    Filter ", H.input { className: "form-control", style } ]
          , H.br {}
          , pageLayout { info, session, pagePath, frontends} ]
      where
        headerProps = { title: name, desc: name, query: "", date, user: ""}
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
pageLayoutCpt = R.hooksComponent "G.P.Annuaire.pageLayout" cpt
  where
    cpt {info, frontends, pagePath, session} _ = do
      useLoader (fst pagePath) (loadPage session) $
        \table -> page {session, table, frontends, pagePath}

type PageProps = 
  ( session :: Session
  , frontends :: Frontends
  , pagePath :: R.State PagePath
  -- , info :: AnnuaireInfo
  , table :: TableResult CT.Contact
  )

page :: Record PageProps -> R.Element
page props = R.createElement pageCpt props []

pageCpt :: R.Component PageProps
pageCpt = R.hooksComponent "LoadedAnnuairePage" cpt
  where
    cpt { session, pagePath, frontends
        , table: ({count: totalRecords, docs})} _ = do
      pure $ T.table { colNames, container, params, rows, totalRecords, wrapColElts }
      where
        path = fst pagePath
        rows = (\c -> {row: contactCells session frontends (fst pagePath).nodeId c, delete: false}) <$> docs
        container = T.defaultContainer { title: "Annuaire" } -- TODO
        colNames = T.ColumnName <$> [ "", "Name", "Company", "Service", "Role"]
        wrapColElts = const identity
        setParams f = snd pagePath $ \pp@{params: ps} ->
          pp {params = f ps}
        params = (fst pagePath).params /\ setParams

type AnnuaireId = Int

contactCells :: Session -> Frontends -> AnnuaireId -> CT.Contact -> Array R.Element
contactCells session frontends aId = render
  where
    render (CT.Contact { id, hyperdata : (CT.HyperdataUser {shared: Nothing} )}) =
        [ H.text ""
        , H.span {} [ H.text "name" ]
        --, H.a { href, target: "blank" } [ H.text $ maybe "name" identity contact.title ]
        , H.text "No ContactWhere"
        , H.text "No ContactWhereDept"
        , H.div {className: "nooverflow"}
          [ H.text "No ContactWhereRole" ]
        ]
    render (CT.Contact { id, hyperdata : (CT.HyperdataUser {shared: Just (CT.HyperdataContact contact@{who: who, ou:ou}) } )}) =
      --let nodepath = NodePath (sessionId session) NodeContact (Just id)
      let nodepath = Routes.ContactPage (sessionId session) aId id
          href = url frontends nodepath in
      [ H.text ""
      , H.a { href} [ H.text $ maybe "name" identity contact.title ]
      --, H.a { href, target: "blank" } [ H.text $ maybe "name" identity contact.title ]
      , H.text $ maybe "No ContactWhere" contactWhereOrg  (head $ ou)
      , H.text $ maybe "No ContactWhereDept" contactWhereDept (head $ ou)
      , H.div {className: "nooverflow"}
        [ H.text $ maybe "No ContactWhereRole" contactWhereRole (head $ ou) ] ]

    contactWhereOrg (CT.ContactWhere { organization: [] }) = "No Organization"
    contactWhereOrg (CT.ContactWhere { organization: orga }) =
      maybe "No orga (list)" identity (head orga)
    contactWhereDept (CT.ContactWhere { labTeamDepts : [] }) = "Empty Dept"
    contactWhereDept (CT.ContactWhere { labTeamDepts : dept }) =
      maybe "No Dept (list)" identity (head dept)
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

loadPage :: Session -> PagePath -> AffTableResult CT.Contact
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

