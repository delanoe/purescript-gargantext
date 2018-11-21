module Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable where


import Data.Argonaut (class DecodeJson, decodeJson, (.?), (.??))
import Data.Array (filter, toUnfoldable)
import Data.Either (Either(..))
import Data.FunctorWithIndex
import Data.Newtype (class Newtype, unwrap)
import Data.Lens (Lens', Prism', lens, over, prism, (^..))
import Data.Lens.At (class At, at)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Fold (folded)
import Data.Lens.Getter (to)
import Data.Lens.Iso (re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry)
import Data.Void (Void)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import React (ReactElement, ReactClass, Children)
import React as React
import React.DOM hiding (style, map)
import React.DOM.Props (_id, _type, checked, className, href, name, onChange, onClick, onInput, placeholder, scope, selected, style, value)
import React.DOM.Props as DOM
import Thermite (PerformAction, Spec, StateCoTransformer, Render, _render, modifyState_, defaultPerformAction, focusState, hideState, simpleSpec, createClass)
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

type PageParams = {nodeId :: Int, params :: T.Params}

type Props' = { path :: PageParams
              , loaded :: Maybe NgramsTable
              , dispatch :: Loader.Action PageParams -> Effect Unit
              }

type NgramsTerm = String

newtype NgramsElement = NgramsElement
  { ngrams      :: NgramsTerm
  , list        :: TermList
  , occurrences :: Int
  , root        :: Maybe NgramsTerm
  , children    :: Set NgramsTerm
  }

derive instance newtypeNgramsElement :: Newtype NgramsElement _

instance decodeJsonNgramsElement :: DecodeJson NgramsElement where
  decodeJson json = do
    obj <- decodeJson json
    ngrams <- obj .? "ngrams"
    list <- obj .? "list"
    occurrences <- obj .? "occurrences"
    root <- obj .?? "root"
    children' <- obj .? "children"
    let children = Set.fromFoldable (children' :: Array NgramsTerm)
    pure $ NgramsElement {ngrams, list, occurrences, root, children}

-- type NgramsTable = Array (NTree NgramsElement)
-- type NgramsTable = Array NgramsElement
newtype NgramsTable = NgramsTable (Map NgramsTerm NgramsElement)

derive instance newtypeNgramsTable :: Newtype NgramsTable _

instance indexNgramsTable :: Index NgramsTable String NgramsElement where
  ix k = _Newtype <<< ix k

instance atNgramsTable :: At NgramsTable String NgramsElement where
  at k = _Newtype <<< at k

instance decodeJsonNgramsTable :: DecodeJson NgramsTable where
  decodeJson json = do
    elements <- decodeJson json
    pure $ NgramsTable
         $ Map.fromFoldable
         $ f <$> (elements :: Array NgramsElement)
    where
      f e@(NgramsElement e') = Tuple e'.ngrams e

data Replace a
  = Keep
  | Replace { old :: a, new :: a }

replace :: forall a. Eq a => a -> a -> Replace a
replace old new
  | old == new = Keep
  | otherwise  = Replace { old, new }

instance semigroupReplace :: Semigroup (Replace a) where
  append Keep p = p
  append p Keep = p
  append (Replace { old: _m, new }) (Replace { old, new: _m' }) =
    -- assert _m == _m'
    Replace { old, new }

instance semigroupMonoid :: Monoid (Replace a) where
  mempty = Keep

applyReplace :: forall a. Eq a => Replace a -> a -> a
applyReplace Keep a = a
applyReplace (Replace { old, new }) a
  | a == old  = new
  | otherwise = a

newtype PatchSet a = PatchSet
  { rem :: Set a
  , add :: Set a
  }

instance semigroupPatchSet :: Ord a => Semigroup (PatchSet a) where
  append (PatchSet p) (PatchSet q) = PatchSet
    { rem: q.rem <> p.rem
    , add: Set.difference q.add p.rem <> p.add
    }

instance monoidPatchSet :: Ord a => Monoid (PatchSet a) where
  mempty = PatchSet { rem: Set.empty, add: Set.empty }

applyPatchSet :: forall a. Ord a => PatchSet a -> Set a -> Set a
applyPatchSet (PatchSet p) s = Set.difference s p.rem <> p.add

newtype NgramsPatch = NgramsPatch
  { patch_children :: PatchSet NgramsTerm
  , patch_list     :: Replace TermList
  }

instance semigroupNgramsPatch :: Semigroup NgramsPatch where
  append (NgramsPatch p) (NgramsPatch q) = NgramsPatch
    { patch_children: p.patch_children <> q.patch_children
    , patch_list:     p.patch_list     <> q.patch_list
    }

instance monoidNgramsPatch :: Monoid NgramsPatch where
  mempty = NgramsPatch { patch_children: mempty, patch_list: mempty }

applyNgramsPatch :: NgramsPatch -> NgramsElement -> NgramsElement
applyNgramsPatch (NgramsPatch p) (NgramsElement e) = NgramsElement
  { ngrams:      e.ngrams
  , list:        applyReplace p.patch_list e.list
  , occurrences: e.occurrences -- TODO: is this correct ?
  , root:        e.root -- TODO: is this correct ?
                        -- See ROOT-UPDATE
  , children:    applyPatchSet p.patch_children e.children
  }

newtype PatchMap k p = PatchMap (Map k p)

instance semigroupPatchMap :: (Ord k, Semigroup p) => Semigroup (PatchMap k p) where
  append (PatchMap p) (PatchMap q) = PatchMap (Map.unionWith append p q)

instance monoidPatchMap :: (Ord k, Semigroup p) => Monoid (PatchMap k p) where
  mempty = PatchMap Map.empty

applyPatchMap :: forall k p v. Ord k => (p -> v -> v) -> PatchMap k p -> Map k v -> Map k v
applyPatchMap applyPatchValue (PatchMap p) = mapWithIndex f
  where
    f k v =
      case Map.lookup k p of
        Nothing -> v
        Just pv -> applyPatchValue pv v

type NgramsTablePatch = PatchMap NgramsTerm NgramsPatch

applyNgramsTablePatch :: NgramsTablePatch -> NgramsTable -> NgramsTable
applyNgramsTablePatch p (NgramsTable m) =
  NgramsTable (applyPatchMap applyNgramsPatch p m)
    -- TODO: update the .root fields...
    -- See ROOT-UPDATE

type State =
  { ngramsTablePatch :: NgramsTablePatch
  , searchQuery      :: String
  , termListFilter   :: Maybe TermList -- Nothing means all
  , termTypeFilter   :: Maybe TermType -- Nothing means all
  }

initialState :: forall props. props -> State
initialState _ =
  { ngramsTablePatch: mempty
  , searchQuery:      ""
  , termListFilter:   Nothing
  , termTypeFilter:   Nothing
  }

data Action
  = SetTermListItem NgramsTerm (Replace TermList)
  | AddTermChildren { parent :: NgramsTerm, child :: NgramsTerm }
  | SetTermListFilter (Maybe TermList)
  | SetTermTypeFilter (Maybe TermType)
  | SetSearchQuery String

data Mode = Authors | Sources | Terms | Trash

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

commitPatch :: NgramsTablePatch -> StateCoTransformer State Unit
commitPatch pt = modifyState_ $ \s -> s { ngramsTablePatch = pt <> s.ngramsTablePatch }

ngramsTableSpec' :: Spec State Props' Action
ngramsTableSpec' = simpleSpec performAction render
  where
    performAction :: PerformAction State Props' Action
    performAction (SetTermListFilter c) _ _ = modifyState_ $ _ { termListFilter = c }
    performAction (SetTermTypeFilter c) _ _ = modifyState_ $ _ { termTypeFilter = c }
    performAction (SetSearchQuery s) _ _ = modifyState_ $ _ { searchQuery = s }
    performAction (SetTermListItem n pl) _ _ = commitPatch pt
      where
        pe = NgramsPatch { patch_list: pl, patch_children: mempty }
        pt = PatchMap $ Map.singleton n pe
    performAction (AddTermChildren { parent, child {- , child_root, parent_root -} }) _ _ = commitPatch pt
      where
        pc = PatchSet { rem: mempty, add: Set.singleton child }
        pe = NgramsPatch { patch_list: mempty, patch_children: pc }
     -- pr = NgramsPatch { patch_list: mempty
     --                  , patch_children: mempty
     --                  , patch_root: replace child_root parent_root
     --                  }
        pt = PatchMap $ Map.fromFoldable [Tuple parent pe]
                                      -- ,Tuple child pr]
        -- TODO ROOT-UPDATE
        -- patch the root of the child to be equal to the root of the parent.

    render :: Render State Props' Action
    render dispatch { path: {nodeId}
                    , loaded: initTable
                    , dispatch: loaderDispatch }
                    { ngramsTablePatch, searchQuery } _children =
      [ T.tableElt
          { rows
          , setParams: \params -> loaderDispatch (Loader.SetPath {nodeId, params})
          , container: tableContainer {searchQuery, dispatch}
          , colNames:
              T.ColumnName <$>
              [ "Graph"
              , "Stop"
              , "Terms"
              , "Occurences (nb)"
              ]
          , totalRecords: 47361 -- TODO
          }
      ]
          where
            rows =
              case applyNgramsTablePatch ngramsTablePatch <$> initTable of
                Nothing -> [] -- or an error
                Just t@(NgramsTable table) ->
                  convertRow t <$> Map.toUnfoldable (Map.filter isRoot table)
            isRoot (NgramsElement e) = e.root == Nothing
            convertRow table (Tuple ngrams (NgramsElement { occurrences, list })) =
              { row:
                  let
                    setTermList Keep = do
                      logs "setTermList Keep"
                      pure unit
                    setTermList rep@(Replace {old,new}) = do
                      logs $ Tuple "setTermList" (Tuple old new)
                      dispatch $ SetTermListItem ngrams rep in
                  renderNgramsItem { table, ngrams, occurrences, termList: list, setTermList }
              , delete: false
              }

initialPageParams :: Int -> PageParams
initialPageParams nodeId = {nodeId, params: T.initialParams}

type PageLoaderProps =
  { path :: PageParams
--, corpusInfo :: Maybe (NodePoly CorpusInfo)
  }

getNgramsTable :: Int -> Aff NgramsTable
getNgramsTable = get <<< toUrl Back (Ngrams TabTerms Nothing)

loadPage :: PageParams -> Aff NgramsTable
loadPage {nodeId} = getNgramsTable nodeId -- TODO this ignores params

ngramsLoaderClass :: ReactClass (Loader.Props PageParams NgramsTable)
ngramsLoaderClass = Loader.createLoaderClass "NgramsLoader" loadPage

ngramsLoader :: Loader.Props' PageParams NgramsTable -> ReactElement
ngramsLoader props = React.createElement ngramsLoaderClass props []

ngramsTableSpec :: Spec {} Props Void
ngramsTableSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {path: nodeId} _ _ =
      -- TODO: ignored mode, ignored loaded: corpusInfo
      [ ngramsLoader { path: initialPageParams nodeId
                     , component: createClass "Layout" ngramsTableSpec' initialState
                     } ]

tree :: NgramsTable -> DOM.Props -> NgramsTerm -> ReactElement
tree table props label =
  li [ style {width : "100%"} ]
     [ i (gray <> [className $ "fas fa-caret-" <> if open then "down" else "right"]) []
     , span [props] [text $ " " <> label]
     , forest table props cs
     ]
  where
    leaf = List.null cs
    open = not leaf || false {- TODO -}
    gray = if leaf then [style {color: "#adb5bd"}] else []
    cs   = table ^.. ix label <<< _Newtype <<< to _.children <<< folded

forest :: NgramsTable -> DOM.Props -> List NgramsTerm -> ReactElement
forest table props = ul [] <<< map (tree table props) <<< List.toUnfoldable

renderNgramsItem :: { table :: NgramsTable
                    , ngrams :: String
                    , occurrences :: Int
                    , termList :: TermList
                    , setTermList :: Replace TermList -> Effect Unit
                    } -> Array ReactElement
renderNgramsItem { table, ngrams, occurrences, termList, setTermList } =
  [ checkbox GraphTerm
  , checkbox StopTerm
  , ul [] [span [className "tree"] [tree table (termStyle termList) ngrams]]
  , text $ show occurrences
  ]
  where

    checkbox termList' =
      let chkd = termList == termList'
          termList'' = if chkd then CandidateTerm else termList'
      in
      input
        [ _type "checkbox"
        , className "checkbox"
        , checked chkd
     -- , title "Mark as completed"
        , onChange $ const $ setTermList (replace termList termList'')
        ]

termStyle :: TermList -> DOM.Props
termStyle GraphTerm     = style {color: "green"}
termStyle StopTerm      = style {color: "red", textDecoration : "line-through"}
termStyle CandidateTerm = style {color: "black"}

optps1 :: forall a. Show a => { desc :: String, mval :: Maybe a } -> ReactElement
optps1 { desc, mval } = option [value val] [text desc]
  where
    val = maybe "" show mval

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
