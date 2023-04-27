module Gargantext.Components.Table where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Components.Corpus.CodeSection (saveCorpus)
import Gargantext.Components.FolderView as FV
import Gargantext.Components.Forest.Tree.Node.Action.Rename (RenameValue(..), rename)
import Gargantext.Components.Nodes.Corpus.Types (CorpusInfo(..), Hyperdata(..), getCorpusInfo, saveCorpusInfo)
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Nodes.Types (FTFieldList)
import Gargantext.Components.Search (SearchType(..))
import Gargantext.Components.Table.Types (ColumnName(..), OrderBy, OrderByDirection(..), Params, Props, TableContainerProps, columnName)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Sessions.Types (Session)
import Gargantext.Types (NodeID, defaultCacheParams)
import Gargantext.Utils (setter, (?))
import Gargantext.Utils.Reactix (effectLink)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Table"

type Page = Int

type State =
  { page       :: Page
  , pageSize   :: PageSizes
  , orderBy    :: OrderBy
  , searchType :: SearchType
  }

paramsState :: Params -> State
paramsState {offset, limit, orderBy, searchType} = {pageSize, page, orderBy, searchType}
  where
    pageSize = int2PageSizes limit
    page = offset / limit + 1

stateParams :: State -> Params
stateParams {pageSize, page, orderBy, searchType} = {offset, limit, orderBy, searchType}
  where
    limit = pageSizes2Int pageSize
    offset = limit * (page - 1)

type TableHeaderLayoutProps = (
    cacheState :: T.Box NT.CacheState
  , date  :: String
  , desc  :: String
  , key   :: String
  , query :: String
  , title :: String
  , user  :: String
  )

type TableHeaderWithRenameLayoutProps = (
    cacheState  :: T.Box NT.CacheState
  , session     :: Session
  , hyperdata   :: Hyperdata
  , nodeId      :: NodeID
  , name        :: String
  , date        :: String
  , key         :: String
)

type TableHeaderWithRenameBoxedLayoutProps = (
    cacheState  :: T.Box NT.CacheState
  , session     :: Session
  , hyperdata   :: Hyperdata
  , nodeId      :: NodeID
  , name        :: String
  , date        :: String
  , corpusInfoS :: T.Box CorpusInfo
)

initialParams :: Params
initialParams = stateParams {page: 1, pageSize: PS10, orderBy: Just (DESC (ColumnName "Date")), searchType: SearchDoc}
-- TODO: Not sure this is the right place for this

tableHeaderWithRenameLayout :: R2.Leaf TableHeaderWithRenameLayoutProps
tableHeaderWithRenameLayout = R2.leaf tableHeaderWithRenameLayoutCpt

tableHeaderWithRenameLayoutCpt :: R.Component TableHeaderWithRenameLayoutProps
tableHeaderWithRenameLayoutCpt = here.component "tableHeaderWithRenameLayoutCpt" cpt
  where
    cpt { hyperdata: Hyperdata h, nodeId, session, cacheState, name, date } _ = do
      let corpusInfo = getCorpusInfo h.fields
      corpusInfoS <- T.useBox corpusInfo

      pure $ tableHeaderWithRenameBoxedLayout {hyperdata: Hyperdata h, nodeId, session, cacheState, name, date, corpusInfoS} []

tableHeaderWithRenameBoxedLayout :: R2.Component TableHeaderWithRenameBoxedLayoutProps
tableHeaderWithRenameBoxedLayout = R.createElement tableHeaderWithRenameBoxedLayoutCpt

tableHeaderWithRenameBoxedLayoutCpt :: R.Component TableHeaderWithRenameBoxedLayoutProps
tableHeaderWithRenameBoxedLayoutCpt = here.component "tableHeaderWithRenameBoxedLayoutCpt" cpt
  where
    cpt p@{ nodeId
          , session
          , cacheState
          , name
          , date
          , corpusInfoS
          } _ = do
      -- | States
      -- |
      cacheState' <- T.useLive T.unequal cacheState
      CorpusInfo {title, desc, query, authors} <- T.read corpusInfoS

      { expandTableEdition
      } <- AppStore.use

      expandTableEdition' <- R2.useLive' expandTableEdition

      -- | Hooks
      -- |

      topBarPortalKey <- pure $ "portal-topbar::" <> show nodeId

      mTopBarHost <- R.unsafeHooksEffect $ R2.getElementById "portal-topbar"

      -- | Effects
      -- |

      -- transfer local Component change to Local Storage cache
      useFirstEffect' $
        flip T.listen expandTableEdition onExpandTableEditionChange

      -- | Behaviors
      -- |
      let
        onExpandClick _ = T.modify_ (not) expandTableEdition

      -- | Render
      -- |
      pure $

        H.div
        { className: "table-header-rename" }
        [
          -- [To Topbar portal]
          -- @NOTE #446: UI flicker artfact when user toggle the CTA
          --             This is due to a re-render + portal input focus --             lost
{-
          R2.createPortal' mTopBarHost
          [
            R2.fragmentWithKey topBarPortalKey
            [
              B.button
              { className: "table-header-rename__cache-toolbar"
              , callback: cacheClick cacheState
              , variant: cacheState' == NT.CacheOn ?
                  ButtonVariant Light $
                  OutlinedButtonVariant Light
              }
              [
                H.text $ cacheText cacheState'
              ]
            ]
          ]
        ,
-}
          H.div
          { className: "table-header-rename__title" }
          [
            B.div'
            { className: "table-header-rename__title__text" }
            name
          ,
            H.hr
            { className: "table-header-rename__title__line" }
          ,
            B.iconButton
            { name: expandTableEdition' ?
                "caret-up" $
                "caret-down"
            , className: "table-header-rename__title__expand"
            , callback: onExpandClick
            }
          ]
        ,
          R2.when expandTableEdition' $

          tableHeaderEditionBlock
          { hyperdata: p.hyperdata
          , nodeId
          , session
          , corpusInfoS
          , defaultData:
            { name
            , title
            , query
            , desc
            , authors
            , date
            }
          }
        ]

    cacheText NT.CacheOn = "Cache On"
    cacheText NT.CacheOff = "Cache Off"

--    cacheClick cacheState _ = T.modify_ cacheStateToggle cacheState

    cacheStateToggle NT.CacheOn = NT.CacheOff
    cacheStateToggle NT.CacheOff = NT.CacheOn

onExpandTableEditionChange :: T.Change Boolean -> Effect Unit
onExpandTableEditionChange { new } = do
  cache <- R2.loadLocalStorageState' R2.appParamsKey defaultCacheParams
  let update = setter (_ { expandTableEdition = new }) cache
  R2.setLocalStorageState R2.appParamsKey update

----------------------------------------------------------

type TableHeaderEditionBlockDefaultData =
  ( name    :: String
  , title   :: String
  , desc    :: String
  , query   :: String
  , authors :: String
  , date    :: String
  )

type TableHeaderEditionBlockProps =
  ( defaultData :: Record TableHeaderEditionBlockDefaultData
  , corpusInfoS :: T.Box CorpusInfo
  , session     :: Session
  , hyperdata   :: Hyperdata
  , nodeId      :: NodeID
  )

tableHeaderEditionBlock :: R2.Leaf TableHeaderEditionBlockProps
tableHeaderEditionBlock = R2.leaf tableHeaderEditionBlockCpt

tableHeaderEditionBlockCpt :: R.Component TableHeaderEditionBlockProps
tableHeaderEditionBlockCpt = here.component "tableHeaderEditionBlock" cpt where
  cpt { defaultData
      , corpusInfoS
      , hyperdata: Hyperdata h
      , nodeId
      , session
      } _ = do
    -- | States
    -- |
    name' /\ name
      <- R2.useBox' defaultData.name

    title' /\ title
      <- R2.useBox' defaultData.title

    desc' /\ desc
      <- R2.useBox' defaultData.desc

    query' /\ query
      <- R2.useBox' defaultData.query

    authors' /\ authors
      <- R2.useBox' defaultData.authors

    date' /\ _
      <- R2.useBox' defaultData.date

    onNamePending' /\ onNamePending
      <- R2.useBox' false

    onTitlePending' /\ onTitlePending
      <- R2.useBox' false

    onDescPending' /\ onDescPending
      <- R2.useBox' false

    onQueryPending' /\ onQueryPending
      <- R2.useBox' false

    onAuthorsPending' /\ onAuthorsPending
      <- R2.useBox' false

    -- | Behaviors
    -- |
    let
      onRenameCorpus newName = do
        saveCorpusName { name: newName
                       , session
                       , nodeId
                       , onPending: onNamePending
                       }

      onRenameTitle newTitle = do
        _ <- T.modify (\(CorpusInfo c) -> CorpusInfo $ c {title = newTitle}) corpusInfoS
        corpusInfo <- T.read corpusInfoS
        let newFields = saveCorpusInfo corpusInfo h.fields
        save { fields: newFields
             , session
             , nodeId
             , onPending: onTitlePending
             }

      onRenameDesc newDesc = do
        _ <- T.modify (\(CorpusInfo c) -> CorpusInfo $ c {desc = newDesc}) corpusInfoS
        corpusInfo <- T.read corpusInfoS
        let newFields = saveCorpusInfo corpusInfo h.fields
        save { fields: newFields
             , session
             , nodeId
             , onPending: onDescPending
             }

      onRenameQuery newQuery = do
        _ <- T.modify (\(CorpusInfo c) -> CorpusInfo $ c {query = newQuery}) corpusInfoS
        corpusInfo <- T.read corpusInfoS
        let newFields = saveCorpusInfo corpusInfo h.fields
        save { fields: newFields
             , session
             , nodeId
             , onPending: onQueryPending
             }

      onRenameAuthors newAuthors = do
        _ <- T.modify (\(CorpusInfo c) -> CorpusInfo $ c {authors = newAuthors}) corpusInfoS
        corpusInfo <- T.read corpusInfoS
        let newFields = saveCorpusInfo corpusInfo h.fields
        save { fields: newFields
             , session
             , nodeId
             , onPending: onAuthorsPending
             }

    -- | Render
    -- |
    pure $

      H.div
      { className: "table-header-rename-edition" }
      [
        B.wad
        [ "d-flex gap-4 " ]
        [
          B.wad
          [ "flex-grow-1" ]
          [
            -- Corpus name
            H.form
            { className: intercalate " "
                [ "form-group"
                , "input-group input-group-sm"
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                B.icon
                { name: "book" }
              ]
            ,
              H.div
              { className: intercalate " "
                  [ "form-group__field"
                  , "input-group input-group-sm"
                  ]
              }
              [
                B.formInput
                { callback: flip T.write_ name
                , value: name'
                }
              ,
                H.div
                { className: "input-group-append" }
                [
                  B.button
                  {
                    variant: ButtonVariant Light
                  , type: "submit"
                  , callback: const $ onRenameCorpus name'
                  , className: "input-group-text"
                  , status: onNamePending' ?
                      Disabled $
                      Enabled
                  }
                  [
                    B.icon
                    { name: "floppy-o"
                    , className: "text-darker"
                    }
                  ]
                ]
              ]
            ]
          ,
            -- Node title
            H.form
            { className: intercalate " "
                [ "form-group"
                , "input-group input-group-sm"
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                B.icon
                { name: "header" }
              ]
            ,
              H.div
              { className: intercalate " "
                  [ "form-group__field"
                  , "input-group input-group-sm"
                  ]
              }
              [
                B.formInput
                { callback: flip T.write_ title
                , value: title'
                }
              ,
                H.div
                { className: "input-group-append" }
                [
                  B.button
                  { variant: ButtonVariant Light
                  , type: "submit"
                  , callback: const $ onRenameTitle title'
                  , className: "input-group-text"
                  , status: onTitlePending' ?
                      Disabled $
                      Enabled
                  }
                  [
                    B.icon
                    { name: "floppy-o"
                    , className: "text-darker"
                    }
                  ]
                ]
              ]
            ]
          ,
            -- Description
            H.form
            { className: intercalate " "
                [ "form-group"
                , "input-group input-group-sm"
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                B.icon
                { name: "info" }
              ]
            ,
              H.div
              { className: intercalate " "
                  [ "form-group__field"
                  , "input-group input-group-sm"
                  ]
              }
              [
                B.formInput
                { callback: flip T.write_ desc
                , value: desc'
                }
              ,
                H.div
                { className: "input-group-append" }
                [
                  B.button
                  { variant: ButtonVariant Light
                  , type: "submit"
                  , callback: const $ onRenameDesc desc'
                  , className: "input-group-text"
                  , status: onDescPending' ?
                      Disabled $
                      Enabled
                  }
                  [
                    B.icon
                    { name: "floppy-o"
                    , className: "text-darker"
                    }
                  ]
                ]
              ]
            ]
          ]
        ,
          B.wad
          [ "flex-grow-1"]
          [
            -- Search query
            H.form
            { className: intercalate " "
                [ "form-group"
                , "input-group input-group-sm"
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                B.icon
                { name: "search-plus" }
              ]
            ,
              H.div
              { className: intercalate " "
                  [ "form-group__field"
                  , "input-group input-group-sm"
                  ]
              }
              [
                B.formInput
                { callback: flip T.write_ query
                , value: query'
                }
              ,
                H.div
                { className: "input-group-append" }
                [
                  B.button
                  { variant: ButtonVariant Light
                  , type: "submit"
                  , callback: const $ onRenameQuery query'
                  , className: "input-group-text"
                  , status: onQueryPending' ?
                      Disabled $
                      Enabled
                  }
                  [
                    B.icon
                    { name: "floppy-o"
                    , className: "text-darker"
                    }
                  ]
                ]
              ]
            ]
          ,
            -- Authors
            H.form
            { className: intercalate " "
                [ "form-group"
                , "input-group input-group-sm"
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                B.icon
                { name: "user" }
              ]
            ,
              H.div
              { className: intercalate " "
                  [ "form-group__field"
                  , "input-group input-group-sm"
                  ]
              }
              [
                B.formInput
                { callback: flip T.write_ authors
                , value: authors'
                }
              ,
                H.div
                { className: "input-group-append" }
                [
                  B.button
                  { variant: ButtonVariant Light
                  , type: "submit"
                  , callback: const $ onRenameAuthors authors'
                  , className: "input-group-text"
                  , status: onAuthorsPending' ?
                      Disabled $
                      Enabled
                  }
                  [
                    B.icon
                    { name: "floppy-o"
                    , className: "text-darker"
                    }
                  ]
                ]
              ]
            ]
          ,
            -- Date
            H.form
            { className: intercalate " "
                [ "form-group"
                , "input-group input-group-sm"
                ]
            }
            [
              H.div
              { className: "form-group__label" }
              [
                B.icon
                { name: "calendar" }
              ]
            ,
              H.div
              { className: intercalate " "
                  [ "form-group__field"
                  , "input-group input-group-sm"
                  ]
              }
              [
                B.formInput
                { callback: const $ pure unit
                , value: date'
                , status: Idled
                }
              ]
            ]
          ]
        ]
      ]

-----------------------------------------------------------

save ::
      { fields    :: FTFieldList
      , session   :: Session
      , nodeId    :: Int
      , onPending :: T.Box Boolean
      }
  ->  Effect Unit
save { fields, session, nodeId, onPending } = do
  T.write_ true onPending
  launchAff_ do
    res <- saveCorpus $ {hyperdata: Hyperdata {fields}, session, nodeId}
    liftEffect $ do
      case res of
        Left err -> here.warn2 "[corpusLayoutView] onClickSave RESTError" err
        _ -> pure unit
    -- add a human minimal cognitive delay telling something has being executed
    delay $ Milliseconds 200.0
    liftEffect $ T.write_ false onPending

saveCorpusName ::
      { name      :: String
      , session   :: Session
      , nodeId    :: Int
      , onPending :: T.Box Boolean
      }
  ->  Effect Unit
saveCorpusName { name, session, nodeId, onPending } = do
  T.write_ true onPending
  launchAff_ do
    res <- rename session nodeId $ RenameValue {text: name}
    liftEffect $ do
      case res of
        Left err -> here.warn2 "[corpusLayoutView] onClickSave RESTError" err
        _ -> pure unit
    -- add a human minimal cognitive delay telling something has being executed
    delay $ Milliseconds 200.0
    liftEffect $ T.write_ false onPending

tableHeaderLayout :: R2.Component TableHeaderLayoutProps
tableHeaderLayout = R.createElement tableHeaderLayoutCpt
tableHeaderLayoutCpt :: R.Component TableHeaderLayoutProps
tableHeaderLayoutCpt = here.component "tableHeaderLayout" cpt
  where
    cpt { cacheState, date, desc, query, title, user } _ = do
      cacheState' <- T.useLive T.unequal cacheState

      pure $ R.fragment
        [ R2.row [FV.backButton {} []]
        ,
          R2.row
          [ H.div {className: "col-md-3"} [ H.h3 {} [H.text title] ]
          , H.div {className: "col-md-9"}
            [ H.hr {style: {height: "2px", backgroundColor: "black"}} ]
          ]
          , R2.row
            [ H.div {className: "col-md-8 content"}
              [ H.p {}
                [ H.span {className: "fa fa-globe"} []
                , H.text $ " " <> desc
                ]
              , H.p {}
                [ H.span {className: "fa fa-search-plus"} []
                , H.text $ " " <> query
                ]
              , H.p { className: "cache-toggle"
                    , on: { click: cacheClick cacheState } }
                [ H.span { className: "fa " <> (cacheToggle cacheState') } []
                , H.text $ cacheText cacheState'
                ]
              ]
            , H.div {className: "col-md-4 content"}
              [ H.p {}
                [ H.span {className: "fa fa-user"} []
                , H.text $ " " <> user
                ]
              , H.p {}
                [ H.span {className: "fa fa-calendar"} []
                , H.text $ " " <> date
                ]
              ]
            ]
          ]

    cacheToggle NT.CacheOn = "fa-toggle-on"
    cacheToggle NT.CacheOff = "fa-toggle-off"

    cacheText NT.CacheOn = "Cache On"
    cacheText NT.CacheOff = "Cache Off"

    cacheClick cacheState _ = do
      T.modify cacheStateToggle cacheState

    cacheStateToggle NT.CacheOn = NT.CacheOff
    cacheStateToggle NT.CacheOff = NT.CacheOn

table :: R2.Leaf Props
table = R2.leaf tableCpt
tableCpt :: R.Component Props
tableCpt = here.component "table" cpt
  where
    cpt { colNames
        , container
        , params
        , rows
        , syncResetButton
        , totalRecords
        , wrapColElts
        } _ = do
      params' <- T.useLive T.unequal params

      let
        state = paramsState params'
        ps = pageSizes2Int state.pageSize
        totalPages = (totalRecords / ps) + min 1 (totalRecords `mod` ps)
        colHeader :: ColumnName -> R.Element
        colHeader c = H.th {scope: "col"} [ H.b {} cs ]
          where
            lnk mc = effectLink $ void $ T.modify (_ { orderBy = mc }) params
            cs :: Array R.Element
            cs =
              wrapColElts c $
              case state.orderBy of
                Just (ASC d)  | c == d -> [lnk (Just (DESC c)) "ASC " , lnk Nothing (columnName c)]
                Just (DESC d) | c == d -> [lnk (Just (ASC  c)) "DESC ", lnk Nothing (columnName c)]
                _ -> [lnk (Just (ASC c)) (columnName c)]
      pure $ container
        { pageSizeControl: sizeDD { params }
        , pageSizeDescription: textDescription state.page state.pageSize totalRecords
        , paginationLinks: pagination { params, totalPages }
        , syncResetButton
        , tableBody: map _.row $ A.fromFoldable rows
        , tableHead: H.tr {} (colHeader <$> colNames)
        }

makeRow :: Array R.Element -> R.Element
makeRow els = H.tr {} $ (\c -> H.td {} [c]) <$> els

makeRow' :: forall r. Record r -> Array R.Element -> R.Element
makeRow' p els = H.tr p $ (\c -> H.td {} [c]) <$> els

type FilterRowsParams =
  (
    params :: Params
  )

filterRows :: forall a. Record FilterRowsParams -> Seq.Seq a -> Seq.Seq a
filterRows { params: { limit, offset } } rs = newRs
  where
    newRs = Seq.take limit $ Seq.drop offset $ rs

defaultContainer :: Record TableContainerProps -> R.Element
defaultContainer props = R.fragment $ props.syncResetButton <> table_data
  where
    table_data = [
                    controls
                  , R2.row [
                      H.table {className: "col-md-12 table"}
                      [ H.thead {className: ""} [ props.tableHead ]
                      , H.tbody {} props.tableBody
                      ]
                    ]
                  , controls
                 ]
      where
        controls = H.div { className: "table-container__navigation d-flex align-items-center mb-2" }
                    [ H.div {className: "col-md-4"} [ props.pageSizeDescription ]
                    , H.div {className: "col-md-4"} [ props.paginationLinks ]
                    , H.div {className: "col-md-4"} 
                      [ B.wad [ "d-flex", "align-items-center", "justify-content-end" ]
                          [ B.label_ "per page"
                          , B.wad_ [ "virtual-space", "w-1" ]
                          , props.pageSizeControl
                          ]
                      ]
                    ]

-- TODO: this needs to be in Gargantext.Pages.Corpus.Graph.Tabs
graphContainer :: Record TableContainerProps -> R.Element
graphContainer props =
  -- TODO title in tabs name (above)
  H.table {className: "table"}
  [ H.thead {className: ""} [ props.tableHead ]
  , H.tbody {} props.tableBody
  ]
   -- TODO better rendering of the paginationLinks
   -- , props.pageSizeControl
   -- , props.pageSizeDescription
   -- , props.paginationLinks

type SizeDDProps =
  (
    params :: T.Box Params
  )

sizeDD :: Record SizeDDProps -> R.Element
sizeDD p = R.createElement sizeDDCpt p []
sizeDDCpt :: R.Component SizeDDProps
sizeDDCpt = here.component "sizeDD" cpt
  where
    cpt { params } _ = do
      params' <- T.useLive T.unequal params
      let { pageSize } = paramsState params'

      pure $ H.span {} [
        R2.select { className, defaultValue: show pageSize, on: {change} } sizes
      ]
      where
        className = "form-control form-control-sm custom-select"
        change e = do
          let ps = string2PageSize $ R.unsafeEventValue e
          _ <- T.modify (\p -> stateParams $ (paramsState p) { pageSize = ps }) params
          changePage 1 params
        sizes = map option pageSizes
        option size = H.option {value} [H.text value]
          where value = show size

textDescription :: Int -> PageSizes -> Int -> R.Element
textDescription currPage pageSize totalRecords =
  H.div {className: ""} [ H.text msg ] -- TODO or col-md-6 ?
  where
    start = (currPage - 1) * pageSizes2Int pageSize + 1
    end' = currPage * pageSizes2Int pageSize
    end  = if end' > totalRecords then totalRecords else end'
    msg = "Showing " <> show start <> " to " <> show end <> " of " <> show totalRecords

changePage :: Page -> T.Box Params -> Effect Unit
changePage page params =
  void $ T.modify (\p -> stateParams $ (paramsState p) { page = page }) params

type PaginationProps =
  ( params     :: T.Box Params
  , totalPages :: Int )

pagination :: R2.Leaf PaginationProps
pagination = R2.leaf paginationCpt
paginationCpt :: R.Component PaginationProps
paginationCpt = here.component "pagination" cpt
  where
    cpt { params, totalPages } _ = do
      params' <- T.useLive T.unequal params
      let { page } = paramsState params'
          prev = if page == 1 then
                  H.text " Prev. "
                else
                  changePageLink (page - 1) "Prev."
          next = if page == totalPages then
                  H.text " Next "
                else
                  changePageLink (page + 1) "Next"
          first = if page == 1 then
                    H.text ""
                  else
                    changePageLink' 1
          last = if page == totalPages then
                  H.text ""
                else
                  changePageLink' totalPages
          ldots = if page >= 5 then
                    H.text " ... "
                    else
                    H.text ""
          rdots = if page + 3 < totalPages then
                    H.text " ... "
                    else
                    H.text ""
          lnums = map changePageLink' $ A.filter (1  < _) [page - 2, page - 1]
          rnums = map changePageLink' $ A.filter (totalPages > _) [page + 1, page + 2]


      pure $ H.span {} $
        [ H.text " ", prev, first, ldots]
        <>
        lnums
        <>
        [H.b {} [H.text $ " " <> show page <> " "]]
        <>
        rnums
        <>
        [ rdots, last, next ]
        where
          changePageLink :: Int -> String -> R.Element
          changePageLink i s =
            H.span {}
              [ H.text " "
              , effectLink (changePage i params) s
              , H.text " "
              ]

          changePageLink' :: Int -> R.Element
          changePageLink' i = changePageLink i (show i)

data PageSizes = PS10 | PS20 | PS50 | PS100 | PS200

derive instance Eq PageSizes

instance Show PageSizes where
  show PS10  = "10"
  show PS20  = "20"
  show PS50  = "50"
  show PS100 = "100"
  show PS200 = "200"

int2PageSizes :: Int -> PageSizes
int2PageSizes i = string2PageSize $ show i

pageSizes2Int :: PageSizes -> Int
pageSizes2Int PS10  = 10
pageSizes2Int PS20  = 20
pageSizes2Int PS50  = 50
pageSizes2Int PS100 = 100
pageSizes2Int PS200 = 200

pageSizes :: Array PageSizes
pageSizes = [PS10, PS20, PS50, PS100, PS200]

string2PageSize :: String -> PageSizes
string2PageSize "10" = PS10
string2PageSize "20" = PS20
string2PageSize "50" = PS50
string2PageSize "100" = PS100
string2PageSize "200" = PS200
string2PageSize _    = PS10
