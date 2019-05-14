module Gargantext.Components.Search.SearchField
  ( Search, Props, searchField, searchFieldComponent )where

import Prelude hiding (div)
import Data.Map as Map
import Data.Maybe ( Maybe(..), maybe, maybe' )
import Data.Nullable (Nullable, null)
import Data.Traversable ( traverse_ )
import Data.Tuple ( Tuple(..), fst )
import Data.Tuple.Nested ( (/\) )
import DOM.Simple as DOM
import DOM.Simple.Console
import DOM.Simple.Element as Element
import DOM.Simple.Event as DE
import Effect ( Effect )
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.DOM.HTML (text, button, div, input, option)
import Reactix.SyntheticEvent as E

select = R.createElement "select"

type Search = { category :: String, term :: String }

defaultSearch :: Search
defaultSearch = { category: "PubMed", term: "" }

type Props =
  -- list of categories to search, or parsers to use on uploads
  ( categories :: Array String 
  -- State hook for a search, how we get data in and out
  , search :: R.State (Maybe Search)
  )

searchField :: Record Props -> R.Element
searchField p = R.createElement searchFieldComponent p []

placeholder :: String
placeholder = "Query, URL or FILE (works with Firefox or Chromium browsers)"

searchFieldComponent :: R.Memo Props
searchFieldComponent = R.memo (R.hooksComponent "SearchField" cpt) hasChanged
  where
    cpt props _ = do
      let search = maybe defaultSearch identity (fst props.search)
      cat <- R.useState $ \_ -> pure search.category
      term <- R.useState $ \_ -> pure search.term
      pure $
        div { className: "search-field" }
        [ categoryInput cat props.categories
        , searchInput term
        , submitButton cat term props.search
        ]
    hasChanged p p' = (p.categories /= p'.categories) || (fst p.search /= fst p.search)

categoryInput :: R.State String -> Array String -> R.Element
categoryInput (cat /\ setCat) cats =
  select { className: "category", onChange } (item <$> cats)
  where
    onChange = mkEffectFn1 $ \e -> setCat (e .. "target" .. "value")
    item name = option { value: name } [ text name ]

searchInput :: R.State String -> R.Element
searchInput (term /\ setTerm) =
  input { defaultValue: term
        , type: "text"
        , onChange
        , placeholder }
  where onChange = mkEffectFn1 $ \e -> setTerm $ e .. "target" .. "value"


submitButton :: R.State String -> R.State String -> R.State (Maybe Search) -> R.Element
submitButton (cat /\ _) (term /\ _) (_ /\ setSearch) = button { onClick: click } [ text "Search" ]
  where
    click = mkEffectFn1 $ \_ -> do
      case term of
        "" -> setSearch Nothing
        _ -> setSearch $ Just { category: cat, term: term }
