module Gargantext.Pages.Annuaire where

import Gargantext.Prelude

import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:!))
import Data.Array (head)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Gargantext.Components.Table as T
import Gargantext.Config (NodeType(..), Ends, BackendRoute(..), NodePath(..), url)
import Gargantext.Config.REST (get)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Pages.Annuaire.User.Contacts.Types (Contact(..), HyperdataContact(..), ContactWhere(..))
import Reactix as R
import Reactix.DOM.HTML as H

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
type LayoutProps = ( annuaireId :: Int, ends :: Ends )

annuaireLayout :: Record LayoutProps -> R.Element
annuaireLayout props = R.createElement annuaireLayoutCpt props []

annuaireLayoutCpt :: R.Component LayoutProps
annuaireLayoutCpt = R.hooksComponent "G.P.Annuaire.annuaireLayout" cpt
  where
    cpt {annuaireId, ends} _ = do
      path <- R.useState' annuaireId
      useLoader (fst path) (getAnnuaireInfo ends) $
        \info -> annuaire {ends, path, info}
      
type AnnuaireProps =
  ( ends :: Ends
  , path :: R.State Int
  , info :: AnnuaireInfo )

-- | Renders a basic table and the page loader
annuaire :: Record AnnuaireProps -> R.Element
annuaire props = R.createElement annuaireCpt props []

-- Abuses closure to work around the Loader
annuaireCpt :: R.Component AnnuaireProps
annuaireCpt = R.staticComponent "G.P.Annuaire.annuaire" cpt
  where
    cpt {ends, path, info: info@(AnnuaireInfo {name, date: date'})} _ = R.fragment
      [ T.tableHeaderLayout headerProps
      , H.p {} []
      , H.div {className: "col-md-3"}
        [ H.text "    Filter ", H.input { className: "form-control", style } ]
      , H.br {}
      , pageLayout { info, ends, annuairePath: path } ]
      where
        headerProps = { title: name, desc: name, query: "", date, user: ""}
        date = "Last update: " <> date'
        style = {width: "250px", display: "inline-block"}
type PagePath = { nodeId :: Int, params :: T.Params }

type PageLayoutProps =
  ( ends :: Ends
  , annuairePath :: R.State Int
  , info :: AnnuaireInfo )

pageLayout :: Record PageLayoutProps -> R.Element
pageLayout props = R.createElement pageLayoutCpt props []

pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = R.hooksComponent "G.P.Annuaire.pageLayout" cpt
  where
    cpt {annuairePath, info, ends} _ = do
      pagePath <- R.useState' (initialPagePath (fst annuairePath))
      useLoader (fst pagePath) (loadPage ends) $
        \table -> page {ends, table, pagePath, annuairePath}
    initialPagePath nodeId = {nodeId, params: T.initialParams}

type PageProps = 
  ( ends :: Ends
  , annuairePath :: R.State Int
  , pagePath :: R.State PagePath
  -- , info :: AnnuaireInfo
  , table :: AnnuaireTable )

page :: Record PageProps -> R.Element
page props = R.createElement pageCpt props []

pageCpt :: R.Component PageProps
pageCpt = R.staticComponent "LoadedAnnuairePage" cpt
  where
    cpt { ends, annuairePath, pagePath, table: (AnnuaireTable {annuaireTable}) } _ = do
      T.table { rows, setParams, container, colNames, totalRecords }
      where
        totalRecords =4361 -- TODO
        rows = (\c -> {row: contactCells ends c, delete: false}) <$> annuaireTable
        setParams params = snd pagePath $ const {params, nodeId: fst annuairePath}
        container = T.defaultContainer { title: "Annuaire" } -- TODO
        colNames = T.ColumnName <$> [ "", "Name", "Company", "Service", "Role"]

contactCells :: Ends -> Maybe Contact -> Array R.Element
contactCells ends = maybe [] render
  where
    render (Contact { id, hyperdata : (HyperdataContact contact@{who: who, ou:ou} ) }) =
      let nodepath = NodePath NodeContact (Just id)
          href = url ends nodepath in
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
loadPage :: Ends -> PagePath -> Aff AnnuaireTable
loadPage ends {nodeId, params: { offset, limit, orderBy }} =
    get $ url ends children
 -- TODO orderBy
 -- where
 --   convOrderBy (T.ASC  (T.ColumnName "Name")) = NameAsc
 --   convOrderBy (T.DESC (T.ColumnName "Name")) = NameDesc
 --   ...
 --   convOrderBy _ = NameAsc -- TODO

  where
    children = Children NodeContact offset limit Nothing {-(convOrderBy <$> orderBy)-} (Just nodeId)

------ Annuaire loading ------

getAnnuaireInfo :: Ends -> Int -> Aff AnnuaireInfo
getAnnuaireInfo ends id = get $ url ends (NodeAPI Node (Just id))

