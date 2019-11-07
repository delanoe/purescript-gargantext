module Gargantext.Components.Search.SearchBar
  ( Props, searchBar, searchBarCpt
  ) where

import Prelude (Unit, bind, discard, not, pure, show, ($), (<>))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Traversable (traverse_)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Reactix as R
import DOM.Simple.Console (log2)
import Effect.Aff (Aff, launchAff_)
import Reactix.DOM.HTML as H
import Gargantext.Components.Search.Types -- (Database, SearchQuery(..), defaultSearchQuery, performSearch, Lang(..))
import Gargantext.Components.Modals.Modal (modalShow)
import Gargantext.Components.Search.SearchField (Search, searchField)
import Gargantext.Sessions (Session)

type Props = ( session   :: Session
             , databases :: Array Database
             , langs     :: Array Lang
             )

searchBar :: Record Props -> R.Element
searchBar props = R.createElement searchBarCpt props []

searchBarCpt :: R.Component Props
searchBarCpt = R.hooksComponent "G.C.Node.SearchBar.searchBar" cpt
  where
    cpt {session, databases, langs} _ = do
      search <- R.useState' Nothing
      onSearchChange session search
      pure $ H.div { className: "" } [ searchField {databases, langs, search}]


onSearchChange :: Session -> R.State (Maybe Search) -> R.Hooks Unit
onSearchChange session (search /\ setSearch) =
  R.useLayoutEffect1' search $ traverse_ triggerSearch search
  where
    triggerSearch q =
      launchAff_ $ do

        liftEffect $ do
          log2 "Searching db: " $ show q.database
          log2 "Searching term: " q.term

        r <- (performSearch session $ searchQuery q) :: Aff Unit

        liftEffect $ do
          log2 "Return:" r
          modalShow "addCorpus"

    searchQuery {database: Nothing, lang, term} =
      over SearchQuery (_ {query=term}) defaultSearchQuery

    searchQuery {database: Just db, lang, term} =
      over SearchQuery (_ {databases=[db], lang=lang, query=term}) defaultSearchQuery
