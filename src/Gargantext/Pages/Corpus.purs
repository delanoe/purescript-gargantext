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
import Thermite ( Render, Spec, PerformAction, focus, hide
                , defaultPerformAction, simpleSpec, modifyState)
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config      (toUrl, NodeType(..), End(..))
import Gargantext.Config.REST (get)
---------------------------------------------------------
-- Facets
import Gargantext.Pages.Corpus.Facets.Documents as D
import Gargantext.Pages.Corpus.Facets.Sources   as S
import Gargantext.Pages.Corpus.Facets.Authors   as A
import Gargantext.Pages.Corpus.Facets.Terms     as T
import Gargantext.Components.Tab as Tab
-------------------------------------------------------------------
type State = { info        :: Maybe (NodePoly CorpusInfo)
             , docsView    :: D.State
             , authorsView :: A.State
             , sourcesView :: S.State
             , termsView   :: T.State
             , activeTab   :: Int
             }

initialState :: State
initialState = { info : Nothing
               , docsView    : D.initialState
               , authorsView : A.initialState
               , sourcesView : S.initialState
               , termsView   : T.initialState
               , activeTab : 0
               }

------------------------------------------------------------------------
_info :: Lens' State (Maybe (NodePoly CorpusInfo))
_info = lens (\s -> s.info) (\s ss -> s{info = ss})

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
data Action = Load          Int
            | DocviewA D.Action
            | AuthorviewA   A.Action
            | SourceviewA   S.Action
            | TermsviewA    T.Action
            | TabViewA      Tab.Action

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

------------------------------------------------------------------------
newtype NodePoly a = NodePoly { id :: Int
                              , typename :: Int
                              , userId   :: Int
                              , parentId  :: Int
                              , name      :: String
                              , date      :: String
                              , hyperdata :: a
                              }

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


instance decodeNode :: (DecodeJson a) => DecodeJson (NodePoly a) where
  decodeJson json = do
    obj <- decodeJson json
    id        <- obj .? "id"
    typename  <- obj .? "typename"
    userId    <- obj .? "userId"
    parentId  <- obj .? "parentId"
    name      <- obj .? "name"
    date      <- obj .? "date"
    
    hyperdata  <- obj .? "hyperdata"
    hyperdata' <- decodeJson hyperdata
    
    pure $ NodePoly  { id : id
                 , typename : typename
                 , userId   : userId
                 , parentId : parentId
                 , name     : name
                 , date     : date
                 , hyperdata: hyperdata'
                 }


------------------------------------------------------------------------
layout :: Spec State {} Action
layout = corpusSpec <> facets

corpusSpec :: Spec State {} Action
corpusSpec = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
        [ div [className "row"]
          [ div [className "col-md-3"] [ h3 [] [text "Corpus " <> text title] ]
          , div [className "col-md-9"] [ hr [style {height : "2px",backgroundColor : "black"}] ]
          ]
        , div [className "row"] [ div [className "jumbotron1", style {padding : "12px 0px 20px 12px"}]
              [ div [ className "col-md-8 content"]
                    [ p [] [ i [className "fa fa-globe"] []
                           , text corpus.desc
                           ]
                    , p [] [ i [className "fab fa-searchengin"] []
                           , text corpus.query
                           ]
                    ]
              , div [ className "col-md-4 content"]
                    [ p [] [ i [className "fa fa-calendar"] []
                           , text date'
                           ]
                    , p [] [ i [className "fa fa-user"] []
                           , text corpus.authors
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
performAction :: PerformAction State {} Action
performAction (Load nId) _ _ = do
  eitherInfo <- lift $ getNode nId
  _ <- case eitherInfo of
            (Right node') -> void $ modifyState $ _info ?~ node'
            (Left       err)  -> do
               logs err
  logs $ "Node Corpus fetched."
performAction (DocviewA    _) _ _ = pure unit
performAction (AuthorviewA _) _ _ = pure unit
performAction (SourceviewA _) _ _ = pure unit
performAction (TabViewA    _) _ _ = pure unit
performAction (TermsviewA  _) _ _ = pure unit

getNode :: Int -> Aff (Either String (NodePoly CorpusInfo))
getNode id = get $ toUrl Back Node id

------------------------------------------------------------------------
-- Facets
------------------------------------------------------------------------
facets :: Spec State {} Action
facets =
  Tab.tabs _tablens _tabAction $ fromFoldable [ Tuple "Documents"    docPageSpec
                                              , Tuple "Authors" authorPageSpec
                                              , Tuple "Sources" sourcePageSpec
                                              , Tuple "Terms"  termsPageSpec
                                              ]

docPageSpec :: Spec State {} Action
docPageSpec = focus _doclens _docAction D.layoutDocview

authorPageSpec :: Spec State  {} Action
authorPageSpec = focus _authorlens _authorAction A.authorspec'

sourcePageSpec :: Spec State {} Action
sourcePageSpec = focus _sourcelens _sourceAction S.sourcespec'

termsPageSpec :: Spec State {} Action
termsPageSpec = focus _termslens _termsAction T.termSpec'


