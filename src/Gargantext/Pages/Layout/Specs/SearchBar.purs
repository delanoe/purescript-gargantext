module Gargantext.Pages.Layout.Specs.SearchBar where

import Prelude
import Data.Tuple (fst)
import Data.Tuple.Nested ( (/\) )
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Thermite (Spec, defaultPerformAction, simpleSpec)
import Reactix as R
import DOM.Simple.Console
import Gargantext.Utils.Reactix as R'
import Reactix.DOM.HTML as H
import Gargantext.Components.Modals.Modal (modalShow)
import Gargantext.Components.Search.SearchField (searchField)

type Props = ( open :: Boolean, categories :: Array String )

defaultProps :: Record Props
defaultProps = { open: true, categories: ["PubMed", "HAL"] }

searchBar :: Record Props -> R.Element
searchBar p = R.createElement searchBarComponent p []

searchBarComponent :: R.Component Props
searchBarComponent = R.hooksComponent "SearchBar" cpt
  where
    cpt props _ = do
      open <- R.useState $ \_ -> pure $ props.open
      term <- R.useState $ \_ -> pure ""
      R.useLayoutEffect1 (fst term) $ \_ -> do
        case (fst term) of
          "" -> pure unit
          term' -> do
            log2 "Searching term: " term'
            modalShow "addCorpus"
        pure $ \_ -> pure unit
      pure $ H.div { className: "search-bar-container" }
        [ toggleButton open, inner open term props ]
    toggleButton :: R.State Boolean -> R.Element
    toggleButton open =
      H.button {onClick: onClickToggleExpanded open, className: "search-bar-toggle"}
        [ H.i { className: "material-icons md-36", style: { marginTop: "-5px" } }
            [ H.text "control_point" ] ]
    inner :: R.State Boolean -> R.State String -> Record Props -> R.Element
    inner (true /\ _) term props = H.div {className: "search-bar open"}
      [ searchField { categories: props.categories, term: term } ]
    inner (false /\ _) _ _ = H.div {className: "search-bar closed"} []

onClickToggleExpanded :: forall e. R.State Boolean -> EffectFn1 e Unit
onClickToggleExpanded open = mkEffectFn1 $ \_ -> R'.overState not open
