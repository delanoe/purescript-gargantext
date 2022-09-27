module Gargantext.Components.NgramsTable.Search where

import Gargantext.Prelude

import DOM.Simple as DOM
import Data.Foldable (intercalate)
import Data.Nullable (Nullable, null)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.Table (changePage)
import Gargantext.Components.Table.Types (Params)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable.Search"


type SearchInputProps =
  ( searchQuery :: T.Box String
  , params      :: T.Box Params
  )

-- "key": to prevent refreshing & losing input
searchInput :: R2.Leaf ( key :: String | SearchInputProps )
searchInput = R2.leafComponent searchInputCpt
searchInputCpt :: R.Component ( key :: String | SearchInputProps )
searchInputCpt = here.component "searchInput" cpt
  where
    cpt { searchQuery, params } _ = do
      inputRef <- R.useRef null

      pure $ R2.row
        [ H.div { className: "col-12" }
          [ H.div { className: "input-group" }
            [ searchFieldInput { inputRef, searchQuery } []
            , searchButton { inputRef, searchQuery, params } []
            ]
          ]
        ]

type SearchButtonProps =
  ( inputRef    :: R.Ref (Nullable DOM.Element)
  , searchQuery :: T.Box String
  , params      :: T.Box Params
  )

searchButton :: R2.Component SearchButtonProps
searchButton = R.createElement searchButtonCpt

searchButtonCpt :: R.Component SearchButtonProps
searchButtonCpt = here.component "searchButton" cpt where
  cpt { inputRef, searchQuery, params } _ = do
    -- | States
    -- |
    searchQuery' <- T.useLive T.unequal searchQuery

    -- | Behaviors
    -- |
    let
      onReset _ = do
        R2.setInputValue inputRef ""
        T.write_ "" searchQuery
        changePage 1 params

      onSubmit _ = do
        T.write_ (R2.getInputValue inputRef) searchQuery
        changePage 1 params

    -- | Render
    -- |
    pure $

      H.div
      { className: intercalate " "
          [ "ngrams-table-search-button"
          , "input-group-append"
          ]
      }
      [
        if searchQuery' /= ""
        then
          R.fragment
            [
              B.button
              { variant: ButtonVariant Light
              , callback: onReset
              , className: "input-group-text"
              }
                [
                  B.icon
                  { name: "times"
                  , className: "text-danger"
                  }
                ]
            ,
              B.button
              { variant: ButtonVariant Light
              , callback: onSubmit
              , className: "input-group-text"
              }
                [ B.icon
                  { name: "search"
                  , className: "text-secondary"
                  }
                ]
            ]
        else
          B.button
          { variant: ButtonVariant Light
          , callback: onSubmit
          , className: "input-group-text"
          }
          [ B.icon
            { name: "search"
            , className: "text-secondary"
            }
          ]
      ]

type SearchFieldInputProps =
  ( inputRef    :: R.Ref (Nullable DOM.Element)
  , searchQuery :: T.Box String
  )

searchFieldInput :: R2.Component SearchFieldInputProps
searchFieldInput = R.createElement searchFieldInputCpt
searchFieldInputCpt :: R.Component SearchFieldInputProps
searchFieldInputCpt = here.component "searchFieldInput" cpt where
  cpt { inputRef, searchQuery } _ = do

    pure $ H.input { className: "form-control"
                   -- , defaultValue: searchQuery'
                   , name: "search"
                   , on: { keyPress: onKeyPress }
                   , placeholder: "Search"
                   , ref: inputRef
                   , type: "value"
                   }
      where
        onKeyPress e = do
          char <- R2.keyCode e
          if char == 13 then
            T.write_ (R2.getInputValue inputRef) searchQuery
          else
            pure unit
