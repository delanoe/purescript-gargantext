module Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable where


import Data.Array (filter, toUnfoldable)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Lens (Lens', Prism', lens, over, prism)
import Data.Lens.Iso (re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.Tuple (Tuple(..), uncurry)
import Data.Void (Void)
import Data.Unit (Unit)
import Effect (Effect)
import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsItem as NI
import Prelude (class Eq, class Ord, class Show, map, show, void, ($), (*), (+), (-), (/), (<), (<>), (==), (>), (>=))
import React (ReactElement)
import React.DOM hiding (style, map)
import React.DOM.Props (_id, _type, className, href, name, onChange, onClick, onInput, placeholder, scope, selected, style, value)
import Thermite (PerformAction, Spec, _render, cotransform, focus, foreach, modifyState, focusState, hide)
import Unsafe.Coerce (unsafeCoerce)

newtype State = State
  { items        :: List {}
  , search       :: String
  , selectString :: String
  , totalPages   :: Int
  , currentPage  :: Int
  , pageSize     :: PageSizes
  , totalRecords :: Int
  }

derive instance newtypeState :: Newtype State _

initialState :: State
initialState = State { items        : toUnfoldable [{}]
                     , search       : ""
                     , selectString : ""
                     , totalPages   : 10
                     , currentPage  : 1
                     , pageSize     : PS10
                     , totalRecords : 100
                     }

data Action
  = ItemAction Int Void
  | ChangeString String
  | SetInput String
  | ChangePageSize PageSizes
  | ChangePage Int

_itemsList :: Lens' State (List {})
_itemsList = lens (\(State s) -> s.items) (\(State s) v -> State s { items = v })

_ItemAction :: Prism' Action (Tuple Int Void)
_ItemAction = prism (uncurry ItemAction) \ta ->
  case ta of
    ItemAction i a -> Right (Tuple i a)
    _ -> Left ta

type Dispatch = Action -> Effect Unit

performAction :: PerformAction State {} Action

performAction (ChangePageSize ps) _ _ = void (cotransform (\state ->  changePageSize ps state ))

performAction (ChangePage p) _ _ = void  do
  modifyState \(State state) -> State $ state {currentPage = p}



performAction (ChangeString c) _ _ = void do
  modifyState \(State state) -> State $ state { selectString = c }


performAction (SetInput s) _ _ = void do
  modifyState \(State state) -> State $ state { search = s }

performAction _ _ _ = void do
  modifyState \(State state) -> State $ state

tableSpec :: Spec State {} Action -> Spec State {} Action
tableSpec = over _render \render dispatch p (State s) c ->
  [div [className "container-fluid"]
     [
       div [className "jumbotron1"]
       [ div [className "row"]
         [ div [className "panel panel-default"]
           [
             div [className "panel-heading"]
             [ h2 [className "panel-title", style {textAlign : "center"}]
               [ span [className "glyphicon glyphicon-hand-down"] []
               , text "Extracted Terms"
               ]
             , div [className "row"]
               [
                 div [className "savediv pull-left col-md-2", style { marginTop :"1.5em"}]
               [ span [className "needsaveicon glyphicon glyphicon-import"] []
               , button [_id "ImportListOrSaveAll", className "btn btn-warning", style {fontSize : "120%"}]
                 [ text "Import a Termlist"
                 ]
               ]
             ,div [className "col-md-4", style {marginTop : "37px"}]
              [ input [className "form-control "
                     , _id "id_password"
                     , name "search", placeholder "Search"
                     , _type "value"
                     ,value s.search
                     ,onInput \e -> dispatch (SetInput (unsafeEventValue e))
                     ]
                 ]


             , div [_id "filter_terms", className "col-md-6", style{ marginTop : "2.1em",paddingLeft :"1em"}]
               [ div [className "row", style {marginTop : "6px"}]
                 [ div [className "col-md-3"]
                   [ select [_id "picklistmenu"
                        ,className "form-control custom-select"
                        ,onChange (\e -> dispatch (ChangeString $ (unsafeCoerce e).target.value))
                        ] $ map optps1 aryPSMap
                   ]
               , div [className "col-md-3"]
                 [ select [_id "picktermtype",className "form-control custom-select",style {marginLeft : "1em"},onChange (\e -> dispatch (ChangeString $ (unsafeCoerce e).target.value)) ] $ map optps1 aryPS1
                 ]
               ,div [className "col-md-3"]
                 [ sizeDD s.pageSize dispatch
                 ]

                 ]

               ]

              , div [className "col-md-6", style {marginTop : "24px", marginBottom : "14px"}]
                [ textDescription s.currentPage s.pageSize s.totalRecords
               , pagination dispatch s.totalPages s.currentPage
                ]


             ]
           ]
          ,  div [_id "terms_table", className "panel-body"] [table [ className "table able table-bordered"]
           [ thead [ className "tableHeader table-bordered"]
             [ tr []
               [ th [ scope "col"] [ text "Map" ]
               , th [ scope "col"] [ text "Stop"]
               , th [ scope "col"] [ text "Terms"]
               , th [ scope "col"] [ text "Occurences (nb)" ]
               ]
             ]
           , tbody [] $  render dispatch p (State s) c
           ]
         ]
       ]
     ]
  ]
 ]
 ]

ngramsTableSpec :: Spec {} {} Void
ngramsTableSpec =
  hide (unwrap initialState) $
  focusState (re _Newtype) $
  container $
  tableSpec $
  focus _itemsList _ItemAction $
  foreach $ \ _ ->
  NI.ngramsItemSpec

container :: forall state props action. Spec state props action -> Spec state props action
container = over _render \render d p s c ->
  [ div [ className "container-fluid" ] $
    (render d p s c)
  ]



aryPSMap :: Array String
aryPSMap = ["All terms", "Map terms", "Stop terms", "Other terms"]

aryPS1 :: Array String
aryPS1 = ["All types","One-word terms", "Multi-word terms"]


optps1 :: String -> ReactElement
optps1 val = option [ value  val ] [text  val]

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value




changePageSize :: PageSizes -> State  -> State
changePageSize ps (State td) =
  State $ td { pageSize      = ps
                 , totalPages    = td.totalRecords / pageSizes2Int ps
                 , currentPage   = 1
              }


data PageSizes = PS10 | PS20 | PS50 | PS100

derive instance eqPageSizes :: Eq PageSizes

instance showPageSize :: Show PageSizes where
  show PS10 = "10"
  show PS20 = "20"
  show PS50 = "50"
  show PS100 = "100"

pageSizes2Int :: PageSizes -> Int
pageSizes2Int PS10 = 10
pageSizes2Int PS20 = 20
pageSizes2Int PS50 = 50
pageSizes2Int PS100 = 100

aryPS :: Array PageSizes
aryPS = [PS10, PS20, PS50, PS100]

string2PageSize :: String -> PageSizes
string2PageSize "10" = PS10
string2PageSize "20" = PS20
string2PageSize "50" = PS50
string2PageSize "100" = PS100
string2PageSize _    = PS10

sizeDD :: PageSizes -> Dispatch -> ReactElement
sizeDD ps d
  = p []
    [ text "Show : "
    , select [onChange (\e -> d (ChangePageSize $ string2PageSize $ (unsafeCoerce e).target.value))] $ map (optps ps) aryPS
    ]

optps :: PageSizes -> PageSizes -> ReactElement
optps cv val = option [ selected (cv == val), value $ show val ] [text $ show val]


textDescription :: Int -> PageSizes -> Int -> ReactElement
textDescription currPage pageSize totalRecords
  =  div [className "row1"]
          [ div [className "col-md-6"]
                [ text $ "Showing " <> show start <> " to " <> show end <> " of " <> show totalRecords ]
          ]
    where
      start = (currPage - 1) * pageSizes2Int pageSize + 1
      end' = currPage * pageSizes2Int pageSize
      end  = if end' > totalRecords then totalRecords else end'


pagination :: Dispatch -> Int -> Int -> ReactElement
pagination d tp cp
  = span [] $
    [ text "Pages: "
    , prev
    , first
    , ldots
    ]
    <>
    lnums
    <>
    [b' [text $ " " <> show cp <> " "]]
    <>
    rnums
    <>
    [ rdots
    , last
    , next
    ]
    where
      prev = if cp == 1 then
               text " Previous "
               else
               span []
               [ text " "
               , a [ href "javascript:void()"
                   , onClick (\e -> d $ ChangePage $ cp - 1)
                   ] [text "Previous"]
               , text " "
               ]
      next = if cp == tp then
               text " Next "
               else
               span []
               [ text " "
               , a [ href "javascript:void()"
                   , onClick (\e -> d $ ChangePage $ cp + 1)
                   ] [text "Next"]
               , text " "
               ]
      first = if cp == 1 then
                text ""
                else
                span []
                [ text " "
                , a [ href "javascript:void()"
                    , onClick (\e -> d $ ChangePage 1)
                    ] [text "1"]
                , text " "
                ]
      last = if cp == tp then
               text ""
             else
               span []
               [ text " "
               , a [ href "javascript:void()"
                   , onClick (\e -> d $ ChangePage tp)
                   ] [text $ show tp]
               , text " "
               ]
      ldots = if cp >= 5 then
                text " ... "
                else
                text ""
      rdots = if cp + 3 < tp then
                text " ... "
                else
                text ""
      lnums = map (\i -> fnmid d i) $ filter (lessthan 1) [cp - 2, cp - 1]
      rnums = map (\i -> fnmid d i) $ filter (greaterthan tp) [cp + 1, cp + 2]

fnmid :: Dispatch -> Int -> ReactElement
fnmid d i
  = span []
    [ text " "
    , a [ href "javascript:void()"
        , onClick (\e -> d $ ChangePage i)
        ] [text $ show i]
    , text " "
    ]


lessthan :: forall t28. Ord t28 => t28 -> t28 -> Boolean
lessthan x y = x < y

greaterthan :: forall t28. Ord t28 => t28 -> t28 -> Boolean
greaterthan x y = x > y
