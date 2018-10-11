module Gargantext.Pages.Corpus where


import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism, (?~))
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import React.DOM (div, h3, hr, i, p, text)
import React.DOM.Props (className, style)
import Thermite ( Render, Spec, PerformAction, focus
                , simpleSpec, modifyState)
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Config      (toUrl, NodeType(..), End(..))
import Gargantext.Config.REST (get)
---------------------------------------------------------
-- Tabs
import Gargantext.Pages.Corpus.Tabs.Documents as D
import Gargantext.Pages.Corpus.Tabs.Sources   as S
import Gargantext.Pages.Corpus.Tabs.Authors   as A
import Gargantext.Pages.Corpus.Tabs.Terms     as T
import Gargantext.Components.Tab as Tab
-------------------------------------------------------------------
type HeaderState = { info :: Maybe (NodePoly CorpusInfo) }
type State = { headerView  :: HeaderState
             , docsView    :: D.State
             , authorsView :: A.State
             , sourcesView :: S.State
             , termsView   :: T.State
             , activeTab   :: Int
             }

initialState :: State
initialState = { headerView  : { info : Nothing }
               , docsView    : D.initialState
               , authorsView : A.initialState
               , sourcesView : S.initialState
               , termsView   : T.initialState
               , activeTab : 0
               }

------------------------------------------------------------------------
_info :: forall a b. Lens' { info :: a | b } a
_info = lens (\s -> s.info) (\s ss -> s{info = ss})

_headerView :: forall a b. Lens' { headerView :: a | b } a
_headerView = lens (\s -> s.headerView) (\s ss -> s{headerView = ss})

_doclens :: Lens' State D.State
_doclens = lens (\s -> s.docsView) (\s ss -> s {docsView = ss})

_authorlens :: Lens' State A.State
_authorlens = lens (\s -> s.authorsView) (\s ss -> s {authorsView = ss})

_sourcelens :: Lens' State S.State
_sourcelens = lens (\s -> s.sourcesView) (\s ss -> s {sourcesView = ss})

_termslens :: Lens' State T.State
_termslens = lens (\s -> s.termsView) (\s ss -> s {termsView = ss})

_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})
------------------------------------------------------------------------
data HeaderAction = Load Int

data Action = HeaderA       HeaderAction
            | DocviewA      D.Action
            | AuthorviewA   A.Action
            | SourceviewA   S.Action
            | TermsviewA    T.Action
            | TabViewA      Tab.Action

_headerAction :: Prism' Action HeaderAction
_headerAction = prism HeaderA \ action ->
  case action of
    HeaderA haction -> Right haction
    _-> Left action

_docAction :: Prism' Action D.Action
_docAction = prism DocviewA \ action ->
  case action of
    DocviewA laction -> Right laction
    _-> Left action

_authorAction :: Prism' Action A.Action
_authorAction = prism AuthorviewA \ action ->
  case action of
    AuthorviewA laction -> Right laction
    _-> Left action

_sourceAction :: Prism' Action S.Action
_sourceAction = prism SourceviewA \ action ->
  case action of
    SourceviewA laction -> Right laction
    _-> Left action

_termsAction :: Prism' Action T.Action
_termsAction = prism TermsviewA \ action ->
  case action of
    TermsviewA laction -> Right laction
    _-> Left action

_tabAction :: Prism' Action Tab.Action
_tabAction = prism TabViewA \ action ->
  case action of
    TabViewA laction -> Right laction
    _-> Left action

_loadAction :: Prism' HeaderAction Int
_loadAction = prism Load \ action ->
  case action of
    Load x -> Right x
    -- _-> Left action

------------------------------------------------------------------------
newtype CorpusInfo = CorpusInfo { title   :: String
                                , desc    :: String
                                , query   :: String
                                , authors :: String
                                , chart   :: (Maybe (Array Number))
                                }

corpusInfoDefault :: NodePoly CorpusInfo
corpusInfoDefault = NodePoly { id : 0
                             , typename : 0
                             , userId : 0
                             , parentId : 0
                             , name : "Default name"
                             , date  : " Default date"
                             , hyperdata : CorpusInfo
                                { title : "Default title"
                                , desc  : " Default desc"
                                , query : " Default Query"
                                , authors : " Author(s): default"
                                , chart   : Nothing
                                }
                             }

instance decodeCorpusInfo :: DecodeJson CorpusInfo where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    desc  <- obj .? "desc"
    query <- obj .? "query"
    authors <- obj .? "authors"
    chart   <- obj .? "chart"
    pure $ CorpusInfo {title, desc, query, authors, chart}

------------------------------------------------------------------------
layout :: Spec State {} Action
layout = focus _headerView _headerAction corpusHeaderSpec <> facets

corpusHeaderSpec :: Spec HeaderState {} HeaderAction
corpusHeaderSpec = simpleSpec performAction render
  where
    render :: Render HeaderState {} HeaderAction
    render dispatch _ state _ =
        [ div [className "row"]
          [ div [className "col-md-3"] [ h3 [] [text "Corpus " <> text title] ]
          , div [className "col-md-9"] [ hr [style {height : "2px",backgroundColor : "black"}] ]
          ]
        , div [className "row"] [ div [className "jumbotron1", style {padding : "12px 0px 20px 12px"}]
              [ div [ className "col-md-8 content"]
                    [ p [] [ i [className "fa fa-globe"] []
                           , text $ " " <> corpus.desc
                           ]
                    , p [] [ i [className "fab fa-searchengin"] []
                           , text $ " " <> corpus.query
                           ]
                    ]
              , div [ className "col-md-4 content"]
                    [ p [] [ i [className "fa fa-calendar"] []
                           , text $ " " <> date'
                           ]
                    , p [] [ i [className "fa fa-user"] []
                           , text $ " " <> corpus.authors
                           ]
                    ]
              ]
          ]
        ]
          where
            NodePoly { name: title
                     , date: date'
                     , hyperdata : CorpusInfo corpus
                   }
              = maybe corpusInfoDefault identity state.info

------------------------------------------------------------------------
performAction :: PerformAction HeaderState {} HeaderAction
performAction (Load nId) _ _ = do
  eitherInfo <- lift $ getNode nId
  _ <- case eitherInfo of
            (Right node') -> void $ modifyState $ _info ?~ node'
            (Left       err)  -> do
               logs err
  logs $ "Node Corpus fetched."

getNode :: Int -> Aff (Either String (NodePoly CorpusInfo))
getNode id = get $ toUrl Back Node id

------------------------------------------------------------------------
-- Tabs
------------------------------------------------------------------------
facets :: Spec State {} Action
facets =
  Tab.tabs _tablens _tabAction $ fromFoldable [ Tuple "Documents" docPageSpec
                                              , Tuple "Authors"   authorPageSpec
                                              , Tuple "Sources"   sourcePageSpec
                                              , Tuple "Terms"     termsPageSpec
                                              ]

docPageSpec :: Spec State {} Action
docPageSpec = focus _doclens _docAction D.layoutDocview

authorPageSpec :: Spec State  {} Action
authorPageSpec = focus _authorlens _authorAction A.authorspec'

sourcePageSpec :: Spec State {} Action
sourcePageSpec = focus _sourcelens _sourceAction S.sourcespec'

termsPageSpec :: Spec State {} Action
termsPageSpec = focus _termslens _termsAction T.termSpec'

