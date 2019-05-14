module Gargantext.Pages.Layout.Specs.SearchBar
  ( Props, defaultProps, searchBar, searchBarComponent
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested ( (/\) )
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Thermite (Spec, defaultPerformAction, simpleSpec)
import Reactix as R
import DOM.Simple.Console
import Gargantext.Utils.Reactix as R'
import Reactix.DOM.HTML as H
import Gargantext.Components.Modals.Modal (modalShow)
import Gargantext.Components.Search.SearchField (Search, searchField)

type Props =
  ( open :: Boolean
  , categories :: Array String )

defaultCategories :: Array String
defaultCategories = ["PubMed", "HAL"]

defaultProps :: Record Props
defaultProps = { open: true, categories: defaultCategories }

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
        , searchFieldContainer open search props.categories ]

searchFieldContainer :: R.State Boolean -> R.State (Maybe Search) -> Array String -> R.Element
searchFieldContainer (true /\ _) search categories =
  H.div { className: "search-bar open" } [ searchField { categories, search } ]
searchFieldContainer (false /\ _) _ _ = H.div {className: "search-bar closed"} []

onSearchChange :: R.State (Maybe Search) -> R.Hooks Unit
onSearchChange (search /\ setSearch) =
  R'.useLayoutEffect1' search $ \_ -> traverse_ triggerSearch search
  where
    triggerSearch {term, category} = do
      log4 "Searching term: " term " in cat:" category
      modalShow "addCorpus"

toggleButton :: R.State Boolean -> R.Element
toggleButton open =
  H.button { onClick: onToggleExpanded open, className: "search-bar-toggle" }
    [ H.i { className: "material-icons md-36"
          , style: { marginTop: "-5px", color: "#000" } }
          [ H.text "control_point" ] ]

onToggleExpanded :: forall e. R.State Boolean -> EffectFn1 e Unit
onToggleExpanded open = mkEffectFn1 $ \_ -> R'.overState not open
