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
import Gargantext.Components.Search.Types

select = R.createElement "select"

type Search = { database :: Maybe Database, term :: String }

defaultSearch :: Search
defaultSearch = { database: Nothing, term: "" }

type Props =
  -- list of databases to search, or parsers to use on uploads
  ( databases :: Array Database
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
      term <- R.useState $ \_ -> pure search.term
      db   <- R.useState $ \_ -> pure Nothing
      pure $
        div { className: "search-field" }
        [ databaseInput db props.databases
        , searchInput term
        , submitButton db term props.search
        ]
    hasChanged p p' = (fst p.search /= fst p'.search) || (p.databases /= p'.databases)

databaseInput :: R.State (Maybe Database) -> Array Database -> R.Element
databaseInput (db /\ setDB) dbs =
  div {} [ select { className: "database", onChange } (item <$> dbs) ]
  where
    onChange = mkEffectFn1 $ \e -> setDB (readDatabase (e .. "target" .. "value"))
    item db = option { value: (show db) } [ text (show db) ]

searchInput :: R.State String -> R.Element
searchInput (term /\ setTerm) =
  div {} [ input { defaultValue: term
                 , type: "text"
                 , style: { maxWidth: "110px" }
                 , onChange
                 , placeholder } ]
  where onChange = mkEffectFn1 $ \e -> setTerm $ e .. "target" .. "value"


submitButton :: R.State (Maybe Database) -> R.State String -> R.State (Maybe Search) -> R.Element
submitButton (database /\ _) (term /\ _) (_ /\ setSearch) =
  div {} [ button { onClick: click } [ text "Search" ] ]
  where
    click = mkEffectFn1 $ \_ -> do
      case term of
        "" -> setSearch Nothing
        _ -> setSearch $ Just { database, term }

