module Gargantext.Components.Nodes.Annuaire where

import Prelude (bind, identity, pure, ($), (<$>), (<>))
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Array (head)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (Contact(..), HyperdataContact(..), ContactWhere(..))
import Gargantext.Components.Table as T
import Gargantext.Ends (url)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, sessionId, get)
import Gargantext.Types (NodePath(..), NodeType(..))
import Gargantext.Hooks.Loader (useLoader)

newtype IndividuView =
  CorpusView
  { id      :: Int
  , name    :: String
  , role    :: String
  , company :: String }

toRows :: AnnuaireTable -> Array (Maybe Contact)
toRows (AnnuaireTable a) = a.annuaireTable

-- | Top level layout component. Loads an annuaire by id and renders
-- | the annuaire using the result
type LayoutProps = ( nodeId :: Int, session :: Session )

annuaireLayout :: Record LayoutProps -> R.Element
annuaireLayout props = R.createElement annuaireLayoutCpt props []

annuaireLayoutCpt :: R.Component LayoutProps
annuaireLayoutCpt = R.hooksComponent "G.P.Annuaire.annuaireLayout" cpt
  where
    cpt {nodeId, session} _ = do
      path <- R.useState' nodeId
      useLoader (fst path) (getAnnuaireInfo session) $
        \info -> annuaire {session, path, info}

type AnnuaireProps =
  ( session :: Session
  , path :: R.State Int
  , info :: AnnuaireInfo )

-- | Renders a basic table and the page loader
annuaire :: Record AnnuaireProps -> R.Element
annuaire props = R.createElement annuaireCpt props []

-- Abuses closure to work around the Loader
annuaireCpt :: R.Component AnnuaireProps
annuaireCpt = R.staticComponent "G.P.Annuaire.annuaire" cpt
  where
    cpt {session, path, info: info@(AnnuaireInfo {name, date: date'})} _ = R.fragment
      [ T.tableHeaderLayout headerProps
      , H.p {} []
      , H.div {className: "col-md-3"}
        [ H.text "    Filter ", H.input { className: "form-control", style } ]
      , H.br {}
      , pageLayout { info, session, annuairePath: path } ]
      where
        headerProps = { title: name, desc: name, query: "", date, user: ""}
        date = "Last update: " <> date'
        style = {width: "250px", display: "inline-block"}

type PagePath = { nodeId :: Int, params :: T.Params }

type PageLayoutProps =
  ( session :: Session
  , annuairePath :: R.State Int
  , info :: AnnuaireInfo )

pageLayout :: Record PageLayoutProps -> R.Element
pageLayout props = R.createElement pageLayoutCpt props []

pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = R.hooksComponent "G.P.Annuaire.pageLayout" cpt
  where
    cpt {annuairePath, info, session} _ = do
      pagePath <- R.useState' (initialPagePath (fst annuairePath))
      useLoader (fst pagePath) (loadPage session) $
        \table -> page {session, table, pagePath, annuairePath}
    initialPagePath nodeId = {nodeId, params: T.initialParams}

type PageProps = 
  ( session :: Session
  , annuairePath :: R.State Int
  , pagePath :: R.State PagePath
  -- , info :: AnnuaireInfo
  , table :: AnnuaireTable )

page :: Record PageProps -> R.Element
page props = R.createElement pageCpt props []

pageCpt :: R.Component PageProps
pageCpt = R.staticComponent "LoadedAnnuairePage" cpt
  where
    cpt { session, annuairePath, pagePath
        , table: (AnnuaireTable {annuaireTable}) } _ = do
      T.table { rows, params, container, colNames, totalRecords }
      where
        totalRecords = 4361 -- TODO
        rows = (\c -> {row: contactCells session c, delete: false}) <$> annuaireTable
        container = T.defaultContainer { title: "Annuaire" } -- TODO
        colNames = T.ColumnName <$> [ "", "Name", "Company", "Service", "Role"]
        setParams f = snd pagePath $ \{nodeId, params: ps} ->
          {params: f ps, nodeId: fst annuairePath}
        params = T.initialParams /\ setParams

contactCells :: Session -> Maybe Contact -> Array R.Element
contactCells session = maybe [] render
  where
    render (Contact { id, hyperdata : (HyperdataContact contact@{who: who, ou:ou} ) }) =
      let nodepath = NodePath (sessionId session) NodeContact (Just id)
          href = url session nodepath in
      [ H.text ""
      , H.a { href, target: "blank" } [ H.text $ maybe "name" identity contact.title ]
      , H.text $ maybe "No ContactWhere" contactWhereOrg  (head $ ou)
      , H.text $ maybe "No ContactWhere" contactWhereDept (head $ ou)
      , H.div {className: "nooverflow"}
        [ H.text $ maybe "No ContactWhere" contactWhereRole (head $ ou) ] ]
    contactWhereOrg (ContactWhere { organization: [] }) = "No Organization"
    contactWhereOrg (ContactWhere { organization: orga }) =
      maybe "No orga (list)" identity (head orga)
    contactWhereDept (ContactWhere { labTeamDepts : [] }) = "Empty Dept"
    contactWhereDept (ContactWhere { labTeamDepts : dept }) =
      maybe "No Dept (list)" identity (head dept)
    contactWhereRole (ContactWhere { role: Nothing }) = "Empty Role"
    contactWhereRole (ContactWhere { role: Just role }) = role


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


newtype AnnuaireTable  = AnnuaireTable  { annuaireTable :: Array (Maybe Contact)}

instance decodeAnnuaireTable :: DecodeJson AnnuaireTable where
  decodeJson json = do
    rows <- decodeJson json
    pure $ AnnuaireTable { annuaireTable : rows}

------------------------------------------------------------------------

loadPage :: Session -> PagePath -> Aff AnnuaireTable
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
