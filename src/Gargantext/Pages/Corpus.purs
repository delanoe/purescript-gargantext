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
                , simpleSpec, modifyState, noState)
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Config      (toUrl, NodeType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Pages.Corpus.Tabs.States as Tabs
import Gargantext.Pages.Corpus.Tabs.Actions as Tabs
import Gargantext.Pages.Corpus.Tabs.Specs as Tabs
-------------------------------------------------------------------
type HeaderState = { info :: Maybe (NodePoly CorpusInfo) }
type State = { headerView  :: HeaderState
             , tabsView    :: Tabs.State
             }

initialState :: State
initialState = { headerView  : { info : Nothing }
               , tabsView    : Tabs.initialState
               }

------------------------------------------------------------------------
_info :: forall a b. Lens' { info :: a | b } a
_info = lens (\s -> s.info) (\s ss -> s{info = ss})

_headerView :: forall a b. Lens' { headerView :: a | b } a
_headerView = lens (\s -> s.headerView) (\s ss -> s{headerView = ss})

_tabsView :: forall a b. Lens' { tabsView :: a | b } a
_tabsView = lens (\s -> s.tabsView) (\s ss -> s{tabsView = ss})
------------------------------------------------------------------------
data HeaderAction = Load Int

data Action
  = HeaderA HeaderAction
  | TabsA   Tabs.Action

_headerAction :: Prism' Action HeaderAction
_headerAction = prism HeaderA \ action ->
  case action of
    HeaderA haction -> Right haction
    _-> Left action

_tabsAction :: Prism' Action Tabs.Action
_tabsAction = prism TabsA \ action ->
  case action of
    TabsA taction -> Right taction
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
layout = focus _headerView _headerAction corpusHeaderSpec
      <> focus _tabsView _tabsAction Tabs.statefulTabs

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
