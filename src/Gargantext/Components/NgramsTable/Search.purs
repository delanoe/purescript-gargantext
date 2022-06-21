module Gargantext.Components.NgramsTable.Search where

import Data.Nullable (Nullable, null)
import DOM.Simple as DOM
import Gargantext.Prelude
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

    pure $ H.div { className: "input-group-prepend" }
      [ if searchQuery' /= ""
        then
          H.button { className: "btn btn-danger"
                   , on: { click: \_ -> R2.setInputValue inputRef "" } }
                            -- T.write "" searchQuery } }
          [ H.span {className: "fa fa-times"} []]
        else H.span { className: "fa fa-search input-group-text" } []
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
