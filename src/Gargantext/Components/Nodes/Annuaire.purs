module Gargantext.Components.Nodes.Annuaire
 -- ( annuaire )
 where

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
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types as CT
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Table as TT
import Gargantext.Components.Table.Types as TT
import Gargantext.Ends (url, Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, get)
import Gargantext.Types (NodeType(..), AffTableResult, TableResult)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Annuaire"

newtype IndividuView =
  CorpusView
  { id      :: Int
  , name    :: String
  , role    :: String
  , company :: String
  }

--toRows :: AnnuaireTable -> Array (Maybe Contact)
--toRows (AnnuaireTable a) = a.annuaireTable

-- | Top level layout component. Loads an annuaire by id and renders
-- | the annuaire using the result
type LayoutProps =
  ( frontends :: Frontends
  , nodeId    :: Int
  , session   :: R.Context Session
  )

annuaireLayout :: R2.Leaf LayoutProps
annuaireLayout props = R.createElement annuaireLayoutCpt props []

annuaireLayoutCpt :: R.Component LayoutProps
annuaireLayoutCpt = here.component "annuaireLayout" cpt where
  cpt { frontends, nodeId, session } _ = cp <$> R.useContext session where
    cp s = annuaireLayoutWithKey { frontends, key, nodeId, session: s } where
      key = show (sessionId s) <> "-" <> show nodeId

type KeyLayoutProps =
  ( frontends :: Frontends
  , nodeId    :: Int
  , session   :: Session
  , key       :: String
  )

annuaireLayoutWithKey :: R2.Leaf KeyLayoutProps
annuaireLayoutWithKey props = R.createElement annuaireLayoutWithKeyCpt props []

annuaireLayoutWithKeyCpt :: R.Component KeyLayoutProps
annuaireLayoutWithKeyCpt = here.component "annuaireLayoutWithKey" cpt where
  cpt { frontends, nodeId, session } _ = do
    path <- T.useBox nodeId
    path' <- T.useLive T.unequal path

    useLoader path' (getAnnuaireInfo session) $
      \info -> annuaire { frontends, info, path, session }

type AnnuaireProps =
  ( session   :: Session
  , path      :: T.Box Int
  , info      :: AnnuaireInfo
  , frontends :: Frontends
  )

-- | Renders a basic table and the page loader
annuaire :: R2.Leaf AnnuaireProps
annuaire props = R.createElement annuaireCpt props []

-- Abuses closure to work around the Loader
annuaireCpt :: R.Component AnnuaireProps
annuaireCpt = here.component "annuaire" cpt
  where
    cpt {session, path, info: info@(AnnuaireInfo {name, date: date'}), frontends} _ = do
      path' <- T.useLive T.unequal path

      pagePath <- T.useBox $ initialPagePath path'
      cacheState <- T.useBox NT.CacheOff
      cacheState' <- T.useLive T.unequal cacheState

      R.useEffectOnce' $ do
        T.listen (\_ -> launchAff_ $ clearCache unit) cacheState

      pure $ R.fragment
        [ TT.tableHeaderLayout
            { cacheState
            , date
            , desc: name
            , key: "annuaire-" <> (show cacheState')
            , query: ""
            , title: name
            , user: "" } []
        , H.p {} []
          -- , H.div {className: "col-md-3"} [ H.text "    Filter ", H.input { className: "form-control", style } ]
          , H.br {}
          , pageLayout { info, session, pagePath, frontends} ]
      where
        date = "Last update: " <> date'
        style = {width: "250px", display: "inline-block"}
        initialPagePath nodeId = {nodeId, params: TT.initialParams}

type PagePath = { nodeId :: Int, params :: TT.Params }

type PageLayoutProps =
  ( session      :: Session
  , frontends    :: Frontends
  , info         :: AnnuaireInfo
  , pagePath     :: T.Box PagePath
  )

pageLayout :: Record PageLayoutProps -> R.Element
pageLayout props = R.createElement pageLayoutCpt props []

pageLayoutCpt :: R.Component PageLayoutProps
pageLayoutCpt = here.component "pageLayout" cpt
  where
    cpt { info, frontends, pagePath, session } _ = do
      pagePath' <- T.useLive T.unequal pagePath

      useLoader pagePath' (loadPage session) $
        \table -> page { session, table, frontends, pagePath }

type PageProps = 
  ( session   :: Session
  , frontends :: Frontends
  , pagePath  :: T.Box PagePath
  -- , info :: AnnuaireInfo
  , table     :: TableResult CT.NodeContact
  )

page :: Record PageProps -> R.Element
page props = R.createElement pageCpt props []

pageCpt :: R.Component PageProps
pageCpt = here.component "page" cpt
  where
    cpt { frontends
        , pagePath
        , session
        , table: ({count: totalRecords, docs}) } _ = do
      pagePath' <- T.useLive T.unequal pagePath
      params <- T.useFocused (_.params) (\a b -> b { params = a }) pagePath

      pure $ TT.table { colNames
                      , container
                      , params
                      , rows: rows pagePath'
                      , syncResetButton : [ H.div {} [] ]
                      , totalRecords
                      , wrapColElts
                      }
      where
        rows pagePath' = (row pagePath') <$> Seq.fromFoldable docs
        row pagePath'@{ nodeId } contact = { row: contactCells { annuaireId: nodeId, frontends, contact, session }
                                           , delete: false }
        container = TT.defaultContainer { title: "Annuaire" } -- TODO
        colNames = TT.ColumnName <$> [ "", "First Name", "Last Name", "Company", "Role"]
        wrapColElts = const identity

type AnnuaireId = Int

type ContactCellsProps =
  ( annuaireId :: AnnuaireId
  , contact    :: CT.NodeContact
  , frontends  :: Frontends
  , session    :: Session
  )

contactCells :: Record ContactCellsProps -> R.Element
contactCells p = R.createElement contactCellsCpt p []

contactCellsCpt :: R.Component ContactCellsProps
contactCellsCpt = here.component "contactCells" cpt where
  cpt { annuaireId, frontends, session
      , contact: CT.NodeContact
        { id, hyperdata: CT.HyperdataContact { who : Nothing }}} _ =
    pure $ TT.makeRow
    [ H.text ""
    , H.span {} [ H.text "Name" ]
      --, H.a { href, target: "blank" } [ H.text $ fromMaybe "name" contact.title ]
    , H.text "No ContactWhere"
    , H.text "No ContactWhereDept"
    , H.div { className: "nooverflow" }
      [ H.text "No ContactWhereRole" ]
    ]
  cpt { annuaireId, frontends, session
      , contact: CT.NodeContact
        { id, hyperdata: CT.HyperdataContact
              { who: Just (CT.ContactWho { firstName, lastName })
              , ou:  ou }}} _ = do
    pure $ TT.makeRow [
      H.text ""
      , H.a { target: "_blank", href: contactUrl annuaireId id }
        [ H.text $ fromMaybe "First Name" firstName ]
      , H.text $ fromMaybe "First Name" lastName
        -- , H.a { href } [ H.text $ fromMaybe "name" contact.title ]
        --, H.a { href, target: "blank" } [ H.text $ fromMaybe "name" contact.title ]
      , H.text $ maybe "No ContactWhere"     contactWhereOrg  (A.head $ ou)
      , H.text $ maybe "No ContactWhereDept" contactWhereDept (A.head $ ou)
        -- , H.div {className: "nooverflow"} [
        --     H.text $ maybe "No ContactWhereRole" contactWhereRole (A.head $ ou)
      ]
      where
        --nodepath = NodePath (sessionId session) NodeContact (Just id)
        nodepath = Routes.ContactPage (sessionId session) annuaireId id
        href = url frontends nodepath
        contactUrl aId id' = url frontends $ Routes.ContactPage (sessionId session) aId id'
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
newtype AnnuaireInfo =
  AnnuaireInfo
  { id        :: Int
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
    pure $ AnnuaireInfo
      { id : id
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

