module NgramsTable where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Array (filter, fold, toUnfoldable)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, over, prism)
import Data.List (List, deleteAt, index, (!!), (:))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
import Network.HTTP.Affjax (AJAX)
import NgramsItem as NI
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, class Show, bind, const, id, map, show, void, ($), (*), (+), (-), (/), (/=), (<), (<$>), (<*>), (<<<), (<>), (==), (>), (>=))
import React (ReactElement)
import React.DOM hiding (style,map)
import React.DOM.Props (_id, _type, className, href, name, onChange, onClick, onInput, placeholder, scope, selected, style, value)
import Thermite (PerformAction, Render, Spec, _render, cotransform, focus, foreach, modifyState, simpleSpec, withState)
import Unsafe.Coerce (unsafeCoerce)
import Utils (getter, setter)

newtype State = State
  { items :: List NI.State
  , editingGroup :: Boolean
  , groupTerm :: Maybe NI.Term
  , search :: String
  , selectString :: String
  , totalPages   :: Int
  , currentPage  :: Int
  , pageSize     :: PageSizes
  , totalRecords :: Int
  }

initialState :: State
initialState = State
  { items : toUnfoldable items
  , editingGroup : false
  , groupTerm : Nothing
  , search : ""
  , selectString : ""
  , totalPages   : 10
  , currentPage  : 1
  , pageSize     : PS10
  , totalRecords : 100
  }


items :: Array NI.State
items =
  [ itemState $ createTerm 1 "term 1" 10 NI.MapTerm
    [ createTerm 31 "term 31" 10 NI.MapTerm []
    , createTerm 32 "term 32" 10 NI.MapTerm []
    , createTerm 33 "term 33" 10 NI.MapTerm []
    ]
  , itemState $ createTerm 2 "term 2" 10 NI.None []
  , itemState $ createTerm 3 "term 3" 10 NI.None []
  , itemState $ createTerm 4 "term 4" 10 NI.None []
  , itemState $ createTerm 5 "term 5" 10 NI.None []
  , itemState $ createTerm 6 "term 6" 10 NI.None []
  , itemState $ createTerm 7 "term 7" 10 NI.None []
  , itemState $ createTerm 8 "term 8" 10 NI.None []
  , itemState $ createTerm 9 "term 9" 10 NI.None []
  , itemState $ createTerm 10 "term 10" 10 NI.None []
  , itemState $ createTerm 11 "term 11" 10 NI.None []
  , itemState $ createTerm 12 "term 12" 10 NI.None []
  , itemState $ createTerm 13 "term 13" 10 NI.None []
  , itemState $ createTerm 14 "term 14" 10 NI.None []
  , itemState $ createTerm 15 "term 15" 10 NI.None []
  , itemState $ createTerm 16 "term 16" 10 NI.None []
  , itemState $ createTerm 17 "term 17" 10 NI.None []
  , itemState $ createTerm 18 "term 18" 10 NI.None []
  , itemState $ createTerm 19 "term 19" 10 NI.None []
  , itemState $ createTerm 20 "term 20" 10 NI.None []
  , itemState $ createTerm 21 "term 21" 10 NI.None []
  , itemState $ createTerm 22 "term 22" 10 NI.None []
  , itemState $ createTerm 23 "term 23" 10 NI.None []
  , itemState $ createTerm 24 "term 24" 10 NI.None []
  , itemState $ createTerm 25 "term 25" 10 NI.None []
  , itemState $ createTerm 26 "term 26" 10 NI.None []
  , itemState $ createTerm 27 "term 27" 10 NI.None []
  , itemState $ createTerm 28 "term 28" 10 NI.None []
  , itemState $ createTerm 29 "term 29" 10 NI.None []
  , itemState $ createTerm 30 "term 30" 10 NI.None []
]


createTerm :: Int -> String -> Int -> NI.TermType -> Array NI.Term -> NI.Term
createTerm id_ term o t c = NI.Term { id : id_, term : term, occurrence : o, _type : t, children : c}

itemState :: NI.Term -> NI.State
itemState t = NI.State {term : t, editingGroup : false}


data Action
  = NoOp
  | ItemAction Int NI.Action
  | RemoveTerm NI.Term
  | DoneWithGroup
  | ChangeString String
  | SetInput String
  | ChangePageSize PageSizes
  | ChangePage Int

_itemsList :: Lens' State (List NI.State)
_itemsList = lens (\(State s) ->  s.items) (\(State s) v -> State s { items = v })

_ItemAction :: Prism' Action (Tuple Int NI.Action)
_ItemAction = prism (uncurry ItemAction) \ta ->
  case ta of
    ItemAction i a -> Right (Tuple i a)
    _ -> Left ta

setGroupInfo :: Boolean -> NI.State -> NI.State
setGroupInfo eg (NI.State s) = NI.State s {editingGroup = eg}

performAction :: forall eff props. PerformAction ( console :: CONSOLE , ajax :: AJAX, dom :: DOM | eff ) State props Action

performAction (ItemAction idx NI.AddToGroup) _ _ = void do
  _ <- liftEff $ log "addToGroup Called..."
  modifyState \(State s) -> State s
                            { groupTerm = addToGroup <$> s.groupTerm <*> (s.items !! idx)
                            , items = fromMaybe s.items (deleteAt idx s.items)
                            }

performAction (ItemAction idx (NI.ModifyGroup)) _ _ = void do
  _ <- liftEff $ log "modifyGroup Called..."
  modifyState \(State s) -> State s {editingGroup = true, groupTerm = (getter _.term) <$> (index s.items idx), items = map (setGroupInfo true) $ fromMaybe s.items (deleteAt idx s.items)}

performAction DoneWithGroup _ _ = void do
  _ <- liftEff $ log "modifyGroup Called..."
  modifyState \(State s) -> State s {editingGroup = false, groupTerm = Nothing, items = map (setGroupInfo false) (mergeGroupToList s.items $ unsafePartial $ fromJust s.groupTerm)}

performAction (RemoveTerm term@(NI.Term t)) _ _ = void do
  modifyState \(State s) -> State s {items = (NI.State {term :term, editingGroup : true} : s.items )
                                    , groupTerm = removeFromGroup <$> s.groupTerm <*> Just t.id
                                    }
performAction (ChangePageSize ps) _ _ = void (cotransform (\state ->  changePageSize ps state ))

performAction (ChangePage p) _ _ = void  do
  modifyState \(State state) -> State $ state {currentPage = p}



performAction (ChangeString c) _ _ = void do
  modifyState \(State state) -> State $ state { selectString = c }


performAction (SetInput s) _ _ = void do
  modifyState \(State state) -> State $ state { search = s }


performAction _ _ _ = void do
  modifyState id

mergeGroupToList :: List NI.State -> NI.Term -> List NI.State
mergeGroupToList items term = NI.State {term : term, editingGroup : false} : items

removeFromGroup :: NI.Term -> Int -> NI.Term
removeFromGroup (NI.Term t) id_ = NI.Term t {children = filter (\(NI.Term ct) -> ct.id /= id_) t.children  }

addToGroup :: NI.Term -> NI.State -> NI.Term
addToGroup (NI.Term gt) (NI.State s) = NI.Term gt {children = gt.children <> (term s.term)}
  where
    term :: NI.Term -> Array NI.Term
    term (NI.Term t) =
      [ NI.Term t {children = [], _type = gt._type}
      ] <> (map (setter (_{_type = gt._type})) t.children)

tableSpec :: forall eff props. Spec eff State props Action -> Spec eff State props Action
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
                      ] []
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
         ,  div [_id "terms_table", className "panel-body"]
            [ table [ className "table able table-bordered"]
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



ngramsSpec :: forall props eff.
  Spec
    ( console :: CONSOLE
    , ajax :: AJAX
    , dom :: DOM
    | eff
    )
    State
    props
    Action
ngramsSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render d p s@(State state) c = render' state.groupTerm d p s c
      where
        render' Nothing _ _ _ _ = []
        render' (Just t) d p (State s) c =
          [ div'
            [ b' [ text $ getter _.term t ]
            , text " occurrences :"
            , text $ show $ NI.termTotal t
            , dispTermChildren t
            , button [onClick $ d <<< (const DoneWithGroup) ] [text "Done"]
            ]
          ]

        dispTermChildren :: NI.Term -> ReactElement
        dispTermChildren term = ul [] $ map childTerm (getter _.children term)
          where
            childTerm :: NI.Term -> ReactElement
            childTerm child = li []
                      [ button [onClick $ d <<< (const (RemoveTerm child))] [text "Remove"]
                      , NI.dispTerm (getter _.term child) (getter _._type child)
                      ]


ngramsTableSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
ngramsTableSpec =  container $ fold
    [ ngramsSpec
    ,tableSpec $ withState \st ->
      focus _itemsList _ItemAction $
      foreach \_ -> NI.ngramsItemSpec
    ]


container :: forall eff state props action. Spec eff state props action -> Spec eff state props action
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

sizeDD :: PageSizes -> _ -> ReactElement
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


pagination :: _ -> Int -> Int -> ReactElement
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

fnmid :: _ -> Int -> ReactElement
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
