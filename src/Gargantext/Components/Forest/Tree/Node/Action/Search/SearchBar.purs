module Gargantext.Components.Forest.Tree.Node.Action.Search.SearchBar
  ( Props, searchBar, searchBarCpt
  ) where

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField (Search, searchField)
import Gargantext.Components.Forest.Tree.Node.Action.Search.Types -- (Database, SearchQuery(..), defaultSearchQuery, performSearch, Lang(..))
import Gargantext.Components.Lang (Lang)
import Gargantext.Prelude (Unit, pure, ($))
import Gargantext.Sessions (Session)
import Gargantext.Types as GT
import Reactix as R
import Reactix.DOM.HTML as H

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
        [ searchField { databases:allDatabases
                      , langs
                      , onSearch
                      , search
                      , session
                      }
        ]
