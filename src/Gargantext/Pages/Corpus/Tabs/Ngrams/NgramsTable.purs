module Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable where


import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Array (filter, toUnfoldable)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Lens (Lens', Prism', lens, over, prism)
import Data.Lens.Iso (re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry)
import Data.Void (Void)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import React (ReactElement, ReactClass)
import React as React
import React.DOM hiding (style, map)
import React.DOM.Props (_id, _type, checked, className, href, name, onChange, onClick, onInput, placeholder, scope, selected, style, value)
import Thermite (PerformAction, Spec, Render, _render, modifyState_, defaultPerformAction, focusState, hideState, simpleSpec, createClass)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Types
import Gargantext.Components.Table as T
import Gargantext.Prelude
import Gargantext.Config
import Gargantext.Config.REST
import Gargantext.Components.Tree (NTree(..))
import Gargantext.Components.Loader as Loader
import Gargantext.Pages.Corpus.Tabs.Types (CorpusInfo(..), PropsRow)

type Props = { mode :: Mode | PropsRow }

type Props' = { path :: Int
              , loaded :: Maybe NgramsTable
              }

type NgramsTerm = String

newtype NgramsElement = NgramsElement
  { ngrams      :: NgramsTerm
  , list        :: TermList
  , occurrences :: Int
  }

instance decodeJsonNgramsElement :: DecodeJson NgramsElement where
  decodeJson json = do
    obj <- decodeJson json
    ngrams <- obj .? "ngrams"
    list <- obj .? "list"
    occurrences <- obj .? "occurrences"
    pure $ NgramsElement {ngrams, list, occurrences}

type NgramsTable = Array (NTree NgramsElement)

data Replace a
  = Keep
  | Replace { old :: a, new :: a }

type NgramsPatch = { rem_children :: Set NgramsTerm
                   , add_children :: Set NgramsTerm
                   , patch_list   :: Replace TermList
                   }

type NgramsTablePatch = Map NgramsTerm NgramsPatch

type State =
  { ngramsTablePatch :: NgramsTablePatch
  , searchQuery      :: String
  , termListFilter   :: Maybe TermList -- Nothing means all
  , termTypeFilter   :: Maybe TermType -- Nothing means all
  }

initialState :: State
initialState = { ngramsTablePatch: Map.empty
               , searchQuery:      ""
               , termListFilter:   Nothing
               , termTypeFilter:   Nothing
               }

data Action
  = SetTermListItem Int TermList
  | SetTermListFilter (Maybe TermList)
  | SetTermTypeFilter (Maybe TermType)
  | SetSearchQuery String

data Mode = Authors | Sources | Terms

type Dispatch = Action -> Effect Unit

tableContainer :: {searchQuery :: String, dispatch :: Dispatch} -> T.TableContainerProps -> Array ReactElement
tableContainer {searchQuery, dispatch} props =
  [ div [className "container-fluid"]
    [ div [className "jumbotron1"]
      [ div [className "row"]
        [ div [className "panel panel-default"]
          [ div [className "panel-heading"]
            [ h2 [className "panel-title", style {textAlign : "center"}]
              [ span [className "glyphicon glyphicon-hand-down"] []
              , text "Extracted Terms"
              ]
            , div [className "row"]
              [ div [className "savediv pull-left col-md-2", style { marginTop :"1.5em"}]
                [ span [className "needsaveicon glyphicon glyphicon-import"] []
                , button [_id "ImportListOrSaveAll", className "btn btn-warning", style {fontSize : "120%"}]
                  [ text "Import a Termlist" ]
                ]
              , div [className "col-md-4", style {marginTop : "37px"}]
                [ input [ className "form-control "
                        , _id "id_password"
                        , name "search", placeholder "Search"
                        , _type "value"
                        , value searchQuery
                        , onInput \e -> dispatch (SetSearchQuery (unsafeEventValue e))
                        ]
                ]
              , div [_id "filter_terms", className "col-md-6", style{ marginTop : "2.1em",paddingLeft :"1em"}]
                [ div [className "row", style {marginTop : "6px"}]
                  [ div [className "col-md-3"]
                    [ select  [ _id "picklistmenu"
                              , className "form-control custom-select"
                              , onChange (\e -> dispatch (SetTermListFilter $ readTermList $ unsafeEventValue e))
                              ] $ map optps1 termLists
                    ]
                  , div [className "col-md-3"]
                    [ select  [ _id "picktermtype"
                              , className "form-control custom-select"
                              , style {marginLeft : "1em"}
                              , onChange (\e -> dispatch (SetTermTypeFilter $ readTermType $ unsafeEventValue e))
                              ] $ map optps1 termTypes
                    ]
                  , div [className "col-md-3"] [ props.pageSizeControl ]
                  ]
                ]
              , div [className "col-md-6", style {marginTop : "24px", marginBottom : "14px"}]
                [ props.pageSizeDescription
                , props.paginationLinks
                ]
              ]
            ]
          , div [ _id "terms_table", className "panel-body" ]
                [ table [ className "table able table-bordered" ]
                  [ thead [ className "tableHeader table-bordered"] [props.tableHead]
                  , tbody [] props.tableBody
                  ]
                ]
          ]
        ]
      ]
    ]
  ]

ngramsTableSpec' :: Spec State Props' Action
ngramsTableSpec' = simpleSpec performAction render
  where
    performAction :: PerformAction State Props' Action
    performAction (SetTermListFilter c) _ _ = modifyState_ $ _ { termListFilter = c }
    performAction (SetTermTypeFilter c) _ _ = modifyState_ $ _ { termTypeFilter = c }
    performAction (SetSearchQuery s) _ _ = modifyState_ $ _ { searchQuery = s }
    performAction (SetTermListItem _i _l) _ _ = pure unit -- TODO

    render :: Render State Props' Action
    render dispatch {path: nodeId, loaded: initTable} {searchQuery {- TODO more state -} } _ =
      [ T.tableElt
          { loadRows
          , container: tableContainer {searchQuery, dispatch}
          , colNames:
              T.ColumnName <$>
              [ "Graph"
              , "Stop"
              , "Terms"
              , "Occurences (nb)"
              ]
          , totalRecords: 10 -- TODO
          }
      ]
      where
        loadRows {offset, limit, orderBy} = do
          pure []
{-
          _ <- logs "loading documents page"
          res <- loadPage {nodeId,offset,limit,orderBy}
          _ <- logs "OK: loading page documents."
          pure $
            (\(DocumentsView r) ->
                { row:
                    [ div [className $ fa r.fav <> "fa-star"] []
                    -- TODO show date: Year-Month-Day only
                    , text r.date
                    , a [ href (toUrl Front Url_Document r._id) ] [ text r.title ]
                    , text r.source
                    , input [ _type "checkbox"]
                    ]
                , delete: false
                }) <$> res
    fa true  = "fas "
    fa false = "far "
-}

getNgramsTable :: Int -> Aff NgramsTable
getNgramsTable = get <<< toUrl Back (Ngrams TabTerms Nothing)

ngramsLoaderClass :: ReactClass (Loader.Props Int NgramsTable)
ngramsLoaderClass = Loader.createLoaderClass "NgramsLoader" getNgramsTable

ngramsLoader :: Loader.Props Int NgramsTable -> ReactElement
ngramsLoader = React.createLeafElement ngramsLoaderClass

ngramsTableSpec :: Spec {} Props Void
ngramsTableSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {path: nodeId} _ _ =
      -- TODO: ignored mode, ignored loaded: corpusInfo
      [ ngramsLoader { path: nodeId
                     , component: createClass "Layout" ngramsTableSpec' initialState
                     } ]

renderNgramsItem :: { ngrams :: String
                    , occurrences :: Int
                    , termList :: TermList
                    , setTermList :: TermList -> Effect Unit
                    } -> Array (Array ReactElement)
renderNgramsItem { ngrams, occurrences, termList, setTermList } =
  [ [ checkbox GraphTerm]
  , [ checkbox StopTerm]
  , [ span [termStyle termList] [text ngrams] ]
  , [ text $ show occurrences ]
  ]
  where
    checkbox termList' =
      input
        [ _type "checkbox"
        , className "checkbox"
        , checked $ termList == termList'
     -- , title "Mark as completed"
        , onChange $ const $ setTermList termList
        ]

-- termStyle :: TermList -> {}
termStyle GraphTerm     = style {color: "green"}
termStyle StopTerm      = style {color: "red", textDecoration : "line-through"}
termStyle CandidateTerm = style {color: "black"}

optps1 :: forall a. Show a => { desc :: String, mval :: Maybe a } -> ReactElement
optps1 { desc, mval } = option [value val] [text desc]
  where
    val = maybe "" show mval

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
