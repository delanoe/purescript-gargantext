module Gargantext.Pages.Layout.Specs.SearchBar
  ( Props, defaultProps, searchBar, searchBarComponent
  ) where

import Prelude
import Control.Monad.Cont.Trans (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Traversable (traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested ( (/\) )
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Reactix as R
import DOM.Simple.Console
import Effect.Aff (launchAff)
import Reactix.DOM.HTML as H
import Gargantext.Components.Search.Types
import Gargantext.Components.Search.Ajax as Ajax
import Gargantext.Components.Modals.Modal (modalShow)
import Gargantext.Components.Search.SearchField (Search, searchField)
import Gargantext.Utils (id)

type Props = ( open :: Boolean, databases :: Array Database )

defaultProps :: Record Props
defaultProps = { open: false, databases: allDatabases }

searchBar :: Record Props -> R.Element
searchBar p = R.createElement searchBarComponent p []

searchBarComponent :: R.Component Props
searchBarComponent = R.hooksComponent "SearchBar" cpt
  where
    cpt props _ = do
      open <- R.useState $ const props.open
      search <- R.useState $ const Nothing
      onSearchChange search
      pure $ H.div { className: "search-bar-container" }
        [ toggleButton open
        , searchFieldContainer open props.databases search ]

searchFieldContainer :: R.State Boolean -> Array Database -> R.State (Maybe Search) -> R.Element
searchFieldContainer (open /\ _) databases search =
  H.div { className: "search-bar " <> openClass } [ searchField { databases, search } ]
  where
    openClass = if open then "open" else "closed"

onSearchChange :: R.State (Maybe Search) -> R.Hooks Unit
onSearchChange (search /\ setSearch) =
  R.useLayoutEffect1' search $ traverse_ triggerSearch search
  where
    triggerSearch q =  do
      launchAff $ do
        liftEffect $ log2 "Searching db: " $ show q.database
        liftEffect $ log2 "Searching term: " q.term
        r <- Ajax.search (searchQuery q)
        liftEffect $ log2 "Return:" r
        liftEffect $ modalShow "addCorpus"
    searchQuery {database: Nothing, term} = over SearchQuery (_ {query=term}) defaultSearchQuery
    searchQuery {database: Just db, term} = over SearchQuery (_ {databases=[db], query=term}) defaultSearchQuery

toggleButton :: R.State Boolean -> R.Element
toggleButton open =
  H.button { onClick: onToggleExpanded open, className: "search-bar-toggle" }
    [ H.i { className: "material-icons md-24"
          , style: { marginTop: "-2px", color: "#000" } }
          [ H.text "control_point" ] ]

onToggleExpanded :: forall e. R.State Boolean -> EffectFn1 e Unit
onToggleExpanded (_open /\ setOpen) = mkEffectFn1 $ \_ -> setOpen not
