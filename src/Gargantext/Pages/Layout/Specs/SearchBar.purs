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
import Thermite (Spec, defaultPerformAction, simpleSpec)
import Reactix as R
import DOM.Simple.Console
import FFI.Simple ((..), (.=))
import Effect.Aff (launchAff)
import Gargantext.Utils.Reactix as R'
import Reactix.DOM.HTML as H
import Gargantext.Components.Search.Types
import Gargantext.Components.Search.Ajax as Ajax
import Gargantext.Components.Modals.Modal (modalShow)
import Gargantext.Components.Search.SearchField (Query(..), Search, searchField)

type Props = ( open :: Boolean, databases :: Array Database, query :: Maybe Query )

defaultProps :: Record Props
defaultProps = { open: false, databases: allDatabases, query: Nothing }

searchBar :: Record Props -> R.Element
searchBar p = R.createElement searchBarComponent p []

searchBarComponent :: R.Component Props
searchBarComponent = R.hooksComponent "SearchBar" cpt
  where
    cpt props _ = do
      open <- R.useState $ \_ -> pure $ props.open
      search <- R.useState $ \_ -> pure Nothing
      onSearchChange search
      pure $ H.div { className: "search-bar-container" }
        [ toggleButton open
        , searchFieldContainer open props.databases search ]

searchFieldContainer :: R.State Boolean -> Array Database -> R.State (Maybe Search) -> R.Element
searchFieldContainer (open /\ _) databases search =
  H.div {className: "search-bar " <> openClass} [ searchField { databases, search } ]
  where
    openClass = if open then "open" else "closed"

onSearchChange :: R.State (Maybe Search) -> R.Hooks Unit
onSearchChange (search /\ setSearch) =
  R'.useLayoutEffect1' search $ \_ -> traverse_ triggerSearch search
  where
    triggerSearch q =  do
      launchAff $ do
        liftEffect $ log2 "Searching query: " q.query
        (r :: Unit) <- Ajax.search (searchQuery q)
        liftEffect $ log2 "Return:" r
        liftEffect $ modalShow "addCorpus"
    searchQuery {query} = case query of
      (Term term)      -> over SearchQuery (_ {query=term}) defaultSearchQuery
      (QueryFile file) -> over SearchQuery (_ {query=file.contents}) defaultSearchQuery

toggleButton :: R.State Boolean -> R.Element
toggleButton open =
  H.button { onClick: onToggleExpanded open, className: "search-bar-toggle" }
    [ H.i { className: "material-icons md-24"
          , style: { marginTop: "-2px", color: "#000" } }
          [ H.text "control_point" ] ]

onToggleExpanded :: forall e. R.State Boolean -> EffectFn1 e Unit
onToggleExpanded open = mkEffectFn1 $ \_ -> R'.overState not open
