module Gargantext.Components.Nodes.Annuaire
 -- ( annuaire )
 where

import Gargantext.Prelude

import Data.Array as A
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Sequence as Seq
import Data.Symbol (SProxy(..))
import Effect.Aff (launchAff_)
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types as CT
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Table (defaultContainer, initialParams, makeRow, table, tableHeaderLayout) as TT
import Gargantext.Components.Table.Types (ColumnName(..), Params) as TT
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Ends (url, Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId, get)
import Gargantext.Types (NodeType(..), AffETableResult, TableResult)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Simple.JSON as JSON
import Toestand as T

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
  , session   :: Session
  )

annuaireLayout :: R2.Leaf LayoutProps
annuaireLayout = R2.leafComponent annuaireLayoutCpt
annuaireLayoutCpt :: R.Component LayoutProps
annuaireLayoutCpt = here.component "annuaireLayout" cpt where
  cpt { frontends, nodeId, session } _ = do
    pure $ annuaireLayoutWithKey { frontends, key, nodeId, session }
      where
        key = show (sessionId session) <> "-" <> show nodeId

type KeyLayoutProps =
  ( key       :: String
  | LayoutProps
  )

annuaireLayoutWithKey :: R2.Leaf KeyLayoutProps
annuaireLayoutWithKey = R2.leafComponent annuaireLayoutWithKeyCpt

annuaireLayoutWithKeyCpt :: R.Component KeyLayoutProps
annuaireLayoutWithKeyCpt = here.component "annuaireLayoutWithKey" cpt where
  cpt { frontends, nodeId, session } _ = do
    path <- T.useBox nodeId
    path' <- T.useLive T.unequal path

    useLoader { errorHandler
              , loader: getAnnuaireInfo session
              , path: path'
              , render: \info -> annuaire { frontends, info, path, session } }
    where
      errorHandler = logRESTError here "[annuaireLayoutWithKey]"

type AnnuaireProps =
  ( session   :: Session
  , path      :: T.Box Int
  , info      :: AnnuaireInfo
  , frontends :: Frontends
  )

-- | Renders a basic table and the page loader
annuaire :: R2.Leaf AnnuaireProps
annuaire = R2.leafComponent annuaireCpt

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
    cpt { frontends, pagePath, session } _ = do
      pagePath' <- T.useLive T.unequal pagePath

      useLoader { errorHandler
                , loader: loadPage session
                , path: pagePath'
                , render: \table -> page { session, table, frontends, pagePath } }
      where
        errorHandler = logRESTError here "[pageLayout]"

type PageProps = 
  ( frontends :: Frontends
  , pagePath  :: T.Box PagePath
  -- , info :: AnnuaireInfo
  , session   :: Session
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
        row { nodeId } contact = { row: contactCells { annuaireId: nodeId, frontends, contact, session }
                                 , delete: false }
        container = TT.defaultContainer -- TODO
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
  cpt { contact: CT.NodeContact
        { hyperdata: CT.HyperdataContact { who : Nothing } } } _ =
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
        contactUrl aId id' = url frontends $ Routes.ContactPage (sessionId session) aId id'
        contactWhereOrg (CT.ContactWhere { organization: [] }) = "No Organization"
        contactWhereOrg (CT.ContactWhere { organization: orga }) =
          fromMaybe "No orga (list)" (A.head orga)
        contactWhereDept (CT.ContactWhere { labTeamDepts : [] }) = "Empty Dept"
        contactWhereDept (CT.ContactWhere { labTeamDepts : dept }) =
          fromMaybe "No Dept (list)" (A.head dept)

newtype HyperdataAnnuaire = HyperdataAnnuaire
  { title :: Maybe String
  , desc  :: Maybe String }
derive instance Generic HyperdataAnnuaire _
derive instance Newtype HyperdataAnnuaire _
instance Eq HyperdataAnnuaire where eq = genericEq
derive newtype instance JSON.ReadForeign HyperdataAnnuaire

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
derive instance Generic AnnuaireInfo _
derive instance Newtype AnnuaireInfo _
instance Eq AnnuaireInfo where eq = genericEq
instance JSON.ReadForeign AnnuaireInfo where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ AnnuaireInfo $ Record.rename user_idP userIdP $ Record.rename parent_idP parentIdP inst
    where
      user_idP = SProxy :: SProxy "user_id"
      userIdP = SProxy :: SProxy "userId"
      parent_idP = SProxy :: SProxy "parent_id"
      parentIdP = SProxy :: SProxy "parentId"

--newtype AnnuaireTable  = AnnuaireTable  { annuaireTable :: Array (Maybe Contact)}

--instance DecodeJson AnnuaireTable where
--  decodeJson json = do
--    rows <- decodeJson json
--    pure $ AnnuaireTable { annuaireTable : rows}

------------------------------------------------------------------------

loadPage :: Session -> PagePath -> AffETableResult CT.NodeContact
loadPage session {nodeId, params: { offset, limit }} =
    get session children
 -- TODO orderBy
 -- where
 --   convOrderBy (T.ASC  (T.ColumnName "Name")) = NameAsc
 --   convOrderBy (T.DESC (T.ColumnName "Name")) = NameDesc
 --   ...
 --   convOrderBy _ = NameAsc -- TODO

  where
    children = Children NodeContact offset limit Nothing {-(convOrderBy <$> orderBy)-} (Just nodeId)

getAnnuaireInfo :: Session -> Int -> AffRESTError AnnuaireInfo
getAnnuaireInfo session id = get session (NodeAPI Node (Just id) "")

