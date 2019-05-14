module Gargantext.Components.Search.SearchField where

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

type Props =
  -- list of categories to search, or parsers to use on uploads
  ( categories :: Array String 
  -- State hook for a search term, how we get data in and out
  , term :: R.State String
  )

searchField :: Record Props -> R.Element
searchField p = R.createElement searchFieldComponent p []

placeholder :: String
placeholder = "Query, URL or FILE (works with Firefox or Chromium browsers)"

searchFieldComponent :: R.Memo Props
searchFieldComponent = R.memo (R.hooksComponent "SearchField" cpt) hasChanged
  where
    cpt props _ = do
      elemRef <- R.useRef $ null
      pure $
        div { className: "search-field" }
        [ select { className: "category" } (cat <$> props.categories)
        , searchInput elemRef props.term
        , submitButton elemRef props.term
        ]
    cat name = option { value: name } [text name]
    hasChanged p p' = (p.categories /= p'.categories) || (fst p.term /= fst p.term)

searchInput :: R.Ref (Nullable DOM.Element) -> R.State String -> R.Element
searchInput ref (term /\ setTerm) =
  input { defaultValue: term
        , type: "text"
        , ref: ref
        , placeholder: placeholder }

submitButton :: R.Ref (Nullable DOM.Element) -> R.State String -> R.Element
submitButton ref (_ /\ setTerm) = button { onClick: click } [ text "Search" ]
  where
    click = mkEffectFn1 $ \_ -> setTerm $ (R.readRef ref) .. "value"
