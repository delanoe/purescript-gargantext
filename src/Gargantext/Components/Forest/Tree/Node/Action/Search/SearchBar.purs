module Gargantext.Components.Forest.Tree.Node.Action.Search.SearchBar
  ( Props
  , searchBar
  ) where

import Gargantext.Components.Forest.Tree.Node.Action.Search.Types

import Effect (Effect)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField (searchField)
import Gargantext.Components.Lang (Lang)
import Gargantext.Prelude (Unit, pure, ($))
import Gargantext.Sessions (Session)
import Gargantext.Types (FrontendError)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Search.SearchBar"

type Props = ( errors    :: T.Box (Array FrontendError)
             , langs     :: Array Lang
             , onSearch  :: GT.AsyncTaskWithType -> Effect Unit
             , search    :: T.Box Search
             , session   :: Session
             )

searchBar :: R2.Component Props
searchBar = R.createElement searchBarCpt
searchBarCpt :: R.Component Props
searchBarCpt = here.component "searchBar" cpt
  where
    cpt { errors, langs, onSearch, search, session } _ = do
      --onSearchChange session s
      pure $ H.div { className: "search-bar" }
                   [ searchField { databases:allDatabases
                                 , errors
                                 , langs
                                 , onSearch
                                 , search
                                 , session
                                 } []
                   ]
