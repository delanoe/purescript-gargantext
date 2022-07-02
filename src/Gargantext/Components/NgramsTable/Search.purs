module Gargantext.Components.NgramsTable.Search where

import Gargantext.Prelude

import DOM.Simple as DOM
import Data.Foldable (intercalate)
import Data.Nullable (Nullable, null)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable.Search"


type SearchInputProps =
  ( searchQuery :: T.Box String
  )

-- "key": to prevent refreshing & losing input
searchInput :: R2.Leaf ( key :: String | SearchInputProps )
searchInput = R2.leafComponent searchInputCpt
searchInputCpt :: R.Component ( key :: String | SearchInputProps )
searchInputCpt = here.component "searchInput" cpt
  where
    cpt { searchQuery } _ = do
      inputRef <- R.useRef null

      pure $ R2.row
        [ H.div { className: "col-12" }
          [ H.div { className: "input-group" }
            [ searchButton { inputRef, searchQuery } []
            , searchFieldInput { inputRef, searchQuery } []
            ]
          ]
        ]

type SearchButtonProps =
  ( inputRef    :: R.Ref (Nullable DOM.Element)
  , searchQuery :: T.Box String
  )

searchButton :: R2.Component SearchButtonProps
searchButton = R.createElement searchButtonCpt
searchButtonCpt :: R.Component SearchButtonProps
searchButtonCpt = here.component "searchButton" cpt where
  cpt { inputRef, searchQuery } _ = do
    searchQuery' <- T.useLive T.unequal searchQuery

    pure $

      H.div
      { className: intercalate " "
          [ "search-button-prepend"
          , "input-group-prepend"
          ]
         }
      [
        if searchQuery' /= ""
        then
          B.button
          { variant: ButtonVariant Light
          , callback: \_ -> R2.setInputValue inputRef ""
            -- T.write "" searchQuery
          , className: "input-group-text"
          }
          [
            B.icon
            { name: "times"
            , className: "text-danger"
            }
          ]
        else
          B.icon
          { name: "search"
          , className: "input-group-text"
          }
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
    -- searchQuery' <- T.useLive T.unequal searchQuery

    pure $ H.input { className: "form-control"
                   -- , defaultValue: searchQuery'
                   , name: "search"
                   , on: { input: \e -> T.write (R.unsafeEventValue e) searchQuery }
                   , placeholder: "Search"
                   , ref: inputRef
                   , type: "value"
                   }
