module NgramsTable where

import Prelude hiding (div)

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
import React (ReactElement)
import React.DOM (b', button, div, div', li, table, tbody, text, th, thead, tr, ul)
import React.DOM.Props (className, onClick, scope)
import Thermite (PerformAction, Spec, Render, _render, focus, foreach, modifyState, simpleSpec, withState)
import Utils (getter, setter)

newtype State = State
  { items :: List NI.State
  , editingGroup :: Boolean
  , groupTerm :: Maybe NI.Term
  }

initialState :: State
initialState = State
  { items : toUnfoldable items
  , editingGroup : false
  , groupTerm : Nothing
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
tableSpec = over _render \render dispatch p s c ->
  [ table [ className "table able table-bordered"]
    [ thead [ className "tableHeader table-bordered"]
      [ tr []
        [ th [ scope "col"] [ text "Map" ]
        , th [ scope "col"] [ text "Stop"]
        , th [ scope "col"] [ text "Terms"]
        , th [ scope "col"] [ text "Occurences (nb)" ]
        ]
      ]
    , tbody [] $  render dispatch p s c
    ]
  ]

ngramsTableSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
ngramsTableSpec =  container $ fold
    [ ngramsSpec
    , tableSpec $ withState \st ->
      focus _itemsList _ItemAction $
      foreach \_ -> NI.ngramsItemSpec
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



container :: forall eff state props action. Spec eff state props action -> Spec eff state props action
container = over _render \render d p s c ->
  [ div [ className "container" ] $
    (render d p s c)
  ]
