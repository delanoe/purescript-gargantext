module Gargantext.Components.Search.SearchBar
  ( Props, defaultProps, searchBar, searchBarCpt
  ) where

import Prelude (Unit, bind, const, discard, not, pure, show, ($), (<>))
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ( (/\) )
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Reactix as R
import DOM.Simple.Console (log2)
import Effect.Aff (Aff, launchAff)
import Reactix.DOM.HTML as H
import Gargantext.Config (Ends)
import Gargantext.Components.Search.Types (Database, SearchQuery(..), allDatabases, defaultSearchQuery, performSearch)
import Gargantext.Components.Modals.Modal (modalShow)
import Gargantext.Components.Search.SearchField (Search, searchField)

type Props = ( ends :: Ends, open :: Boolean, databases :: Array Database )

defaultProps :: Ends -> Record Props
defaultProps ends = { open: false, databases: allDatabases, ends }

searchBar :: Record Props -> R.Element
searchBar p = R.createElement searchBarCpt p []

searchBarCpt :: R.Component Props
searchBarCpt = R.hooksComponent "SearchBar" cpt
  where
    cpt {ends, databases, open} _ = do
      open' <- R.useState $ const open
      search <- R.useState $ const Nothing
      onSearchChange ends search
      pure $ H.div { className: "search-bar-container" }
        [ toggleButton open'
        , searchFieldContainer open' databases search ]

searchFieldContainer :: R.State Boolean -> Array Database -> R.State (Maybe Search) -> R.Element
searchFieldContainer (open /\ _) databases search =
  H.div { className: "search-bar " <> openClass } [ searchField { databases, search } ]
  where
    openClass = if open then "open" else "closed"

onSearchChange :: Ends -> R.State (Maybe Search) -> R.Hooks Unit
onSearchChange ends (search /\ setSearch) =
  R.useLayoutEffect1' search $ traverse_ triggerSearch search
  where
    triggerSearch q =  do
      launchAff $ do
        liftEffect $ log2 "Searching db: " $ show q.database
        liftEffect $ log2 "Searching term: " q.term
        r <- (performSearch ends $ searchQuery q) :: Aff Unit
        liftEffect $ log2 "Return:" r
        liftEffect $ modalShow "addCorpus"
    searchQuery {database: Nothing, term} = over SearchQuery (_ {query=term}) defaultSearchQuery
    searchQuery {database: Just db, term} = over SearchQuery (_ {databases=[db], query=term}) defaultSearchQuery

toggleButton :: R.State Boolean -> R.Element
toggleButton open =
  H.button { onClick: onToggleExpanded open, className: "search-bar-toggle" }
  [ H.i { className: "material-icons md-24", style  } [ H.text "control_point" ] ]
  where style = { marginTop: "-2px", color: "#000" }

onToggleExpanded :: forall e. R.State Boolean -> EffectFn1 e Unit
onToggleExpanded (_open /\ setOpen) = mkEffectFn1 $ \_ -> setOpen not
