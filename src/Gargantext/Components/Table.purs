module Gargantext.Components.Table where

import Data.Array (filter)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff)
import React (ReactElement, ReactClass, Children, createElement)
import React.DOM (a, b, b', div, option, select, span, table, tbody, td, text, th, thead, tr)
import React.DOM.Props (className, href, onChange, onClick, scope, selected, value)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec, createClass)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Prelude

type Rows = Array { row    :: Array ReactElement
                  , delete :: Boolean
                  }

type LoadRows = { offset :: Int, limit :: Int } -> Aff (Either String Rows)

type Props' =
  ( title        :: String
  , colNames     :: Array String
  , totalRecords :: Int
  , loadRows     :: LoadRows
  )

type Props = Record Props'

type State =
  { rows        :: Maybe Rows
  , currentPage :: Int
  , pageSize    :: PageSizes
--, tree        :: FTree
  }

initialState :: State
initialState =
  { rows         : Nothing
  , currentPage  : 1
  , pageSize     : PS10
--, tree         : exampleTree
  }

data Action
  = ChangePageSize PageSizes
  | ChangePage     Int

type ChangePageAction = Int -> Effect Unit

-- | Action
-- ChangePageSize
changePageSize :: Int -> PageSizes -> State -> State
changePageSize totalRecords ps td =
  td { pageSize      = ps
     , currentPage   = 1
     }

tableSpec :: Spec State Props Action
tableSpec = simpleSpec performAction render
  where
    performAction :: PerformAction State Props Action
    performAction (ChangePageSize ps) {totalRecords} _ =
      void $ modifyState $ changePageSize totalRecords ps
    performAction (ChangePage p) _ _ = 
      void $ modifyState $ _ { currentPage = p }

    render :: Render State Props Action
    render dispatch {title, colNames, totalRecords}
                    {pageSize, currentPage, rows} _ =
      let totalPages = totalRecords / pageSizes2Int pageSize in
      [ div [className "row"]
        [ div [className "col-md-1"] [b [] [text title]]
        , div [className "col-md-2"] [sizeDD pageSize dispatch]
        , div [className "col-md-3"] [textDescription currentPage pageSize totalRecords]
        , div [className "col-md-3"] [pagination (dispatch <<< ChangePage) totalPages currentPage]
              ]
      , table [ className "table"]
        [ thead [className "thead-dark"]
                [tr [] ((\colName -> th [scope "col"] [ b' [text colName]]) <$> colNames)]
        , tbody [] $ map (tr [] <<< map (\c -> td [] [c]) <<< _.row)
                         (maybe [] identity rows)
                      -- TODO display a loading spinner when rows == Nothing
                      -- instead of an empty list of results.
        ]
      ]

tableClass :: ReactClass {children :: Children | Props'}
tableClass = createClass "Table" tableSpec initialState

tableElt :: Props -> ReactElement
tableElt props = createElement tableClass props []

sizeDD :: PageSizes -> (Action -> Effect Unit) -> ReactElement
sizeDD ps d
  = span []
    [ text "Show : "
    , select [onChange (\e -> d (ChangePageSize $ string2PageSize $ (unsafeCoerce e).target.value))] $ map (optps ps) aryPS
    ]

textDescription :: Int -> PageSizes -> Int -> ReactElement
textDescription currPage pageSize totalRecords
  =  div [className "row1"]
          [ div [className ""]
                [ text $ "Showing " <> show start <> " to " <> show end <> " of " <> show totalRecords ]
          ]
    where
      start = (currPage - 1) * pageSizes2Int pageSize + 1
      end' = currPage * pageSizes2Int pageSize
      end  = if end' > totalRecords then totalRecords else end'

pagination :: ChangePageAction -> Int -> Int -> ReactElement
pagination changePage tp cp
  = span [] $
    [ text "Pages: ", prev, first, ldots]
    <>
    lnums
    <>
    [b' [text $ " " <> show cp <> " "]]
    <>
    rnums
    <>
    [ rdots, last, next ]
    where
      prev = if cp == 1 then
               text " Previous "
               else
               span []
               [ text " "
               , a [ href "javascript:void()"
                   , onClick (\e -> changePage $ cp - 1)
                   ] [text "Previous"]
               , text " "
               ]
      next = if cp == tp then
               text " Next "
               else
               span []
               [ text " "
               , a [ href "javascript:void()"
                   , onClick (\e -> changePage $ cp + 1)
                   ] [text "Next"]
               , text " "
               ]
      first = if cp == 1 then
                text ""
                else
                span []
                [ text " "
                , a [ href "javascript:void()"
                    , onClick (\e -> changePage 1)
                    ] [text "1"]
                , text " "
                ]
      last = if cp == tp then
               text ""
             else
               span []
               [ text " "
               , a [ href "javascript:void()"
                   , onClick (\e -> changePage tp)
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
      lnums = map (\i -> fnmid changePage i) $ filter (1  < _) [cp - 2, cp - 1]
      rnums = map (\i -> fnmid changePage i) $ filter (tp > _) [cp + 1, cp + 2]

fnmid :: ChangePageAction -> Int -> ReactElement
fnmid changePage i
  = span []
    [ text " "
    , a [ href "javascript:void()"
        , onClick (\e -> changePage i)
        ] [text $ show i]
    , text " "
    ]

data PageSizes = PS10 | PS20 | PS50 | PS100

derive instance eqPageSizes :: Eq PageSizes

instance showPageSize :: Show PageSizes where
  show PS10  = "10"
  show PS20  = "20"
  show PS50  = "50"
  show PS100 = "100"

pageSizes2Int :: PageSizes -> Int
pageSizes2Int PS10  = 10
pageSizes2Int PS20  = 20
pageSizes2Int PS50  = 50
pageSizes2Int PS100 = 100

aryPS :: Array PageSizes
aryPS = [PS10, PS20, PS50, PS100]

string2PageSize :: String -> PageSizes
string2PageSize "10" = PS10
string2PageSize "20" = PS20
string2PageSize "50" = PS50
string2PageSize "100" = PS100
string2PageSize _    = PS10

optps :: PageSizes -> PageSizes -> ReactElement
optps cv val = option [ selected (cv == val), value $ show val ] [text $ show val]
