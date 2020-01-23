module Gargantext.Components.Search.SearchBar
  ( Props, searchBar, searchBarCpt
  ) where

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.Search.Types -- (Database, SearchQuery(..), defaultSearchQuery, performSearch, Lang(..))
import Gargantext.Components.Search.SearchField (Search, searchField)
import Gargantext.Sessions (Session)
import Gargantext.Types as GT

type Props = ( langs     :: Array Lang
             , onSearch  :: GT.AsyncTaskWithType -> Effect Unit
             , search    :: R.State Search
             , session   :: Session
             )

searchBar :: Record Props -> R.Element
searchBar props = R.createElement searchBarCpt props []

searchBarCpt :: R.Component Props
searchBarCpt = R.hooksComponent "G.C.Node.SearchBar.searchBar" cpt
  where
    cpt {langs, onSearch, search: search@(s /\ _), session} _ = do
      --onSearchChange session s
      pure $ H.div {"style": {"margin" :"10px"}}
        [ searchField {databases:allDatabases, langs, onSearch, search, session}]
