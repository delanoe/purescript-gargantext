module Gargantext.Components.Search.SearchBar
  ( Props, searchBar, searchBarCpt
  ) where

import Prelude (pure, ($))
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Search.Types -- (Database, SearchQuery(..), defaultSearchQuery, performSearch, Lang(..))
import Gargantext.Components.Search.SearchField (Search, searchField)
import Gargantext.Sessions (Session)

type Props = ( session   :: Session
             , langs     :: Array Lang
             , search    :: R.State Search
             )

searchBar :: Record Props -> R.Element
searchBar props = R.createElement searchBarCpt props []

searchBarCpt :: R.Component Props
searchBarCpt = R.hooksComponent "G.C.Node.SearchBar.searchBar" cpt
  where
    cpt {session, langs, search: search@(s /\ _)} _ = do
      --onSearchChange session s
      pure $ H.div {"style": {"margin" :"10px"}}
        [ searchField {databases:allDatabases, langs, search, session}]
