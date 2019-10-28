module Gargantext.Components.Search.SearchField
  ( Search, Props, searchField, searchFieldComponent )where

import Prelude (bind, const, identity, pure, show, ($), (/=), (<$>), (||))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Gargantext.Utils.Reactix as R2
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Reactix as R
import Reactix.DOM.HTML (text, button, div, input, span, ul, li, a, option)
import Gargantext.Components.Search.Types (Database(..), readDatabase)

select :: forall props.
          R.IsComponent String props (Array R.Element)
          => Record props
          -> Array R.Element
          -> R.Element
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
placeholder = "Query, URL or FILE"
-- TODO add elsewhere "(works with Firefox or Chromium browsers)"

searchFieldComponent :: R.Memo Props
searchFieldComponent = R.memo (R.hooksComponent "SearchField" cpt) hasChanged
  where
    cpt props _ = do
      let search = maybe defaultSearch identity (fst props.search)
      term <- R.useState' search.term
      db   <- R.useState' (Nothing :: Maybe Database)
      pure $
          div { className: "search-field-group" }
              [ searchInput term
              , div {className: "text-primary center"} [text "in"]
              , databaseInput db props.databases
              , div { className: "panel-footer" } [ submitButton db term props.search ]
              ]
    hasChanged p p' = (fst p.search /= fst p'.search)
                   || (p.databases /= p'.databases)


databaseInput :: R.State (Maybe Database) -> Array Database -> R.Element
databaseInput (db /\ setDB) dbs =
  div { className: "form-group" } 
                   [ R2.select { className: "form-control"
                               , onChange: mkEffectFn1
                                         $ \e -> setDB
                                         $ const
                                         $ readDatabase
                                         $ e .. "target" .. "value"
                               } (liItem <$> dbs)
                   ]
    where
      liItem :: Database -> R.Element
      liItem  db = option {className : "text-primary center"} [ text (show db) ]



searchInput :: R.State String -> R.Element
searchInput (term /\ setTerm) =
  input { defaultValue: term
        , className: "form-control"
        , type: "text"
        , onChange
        , placeholder }
  where onChange = mkEffectFn1 $ \e -> setTerm $ const $ e .. "target" .. "value"


submitButton :: R.State (Maybe Database) -> R.State String -> R.State (Maybe Search) -> R.Element
submitButton (database /\ _) (term /\ _) (_ /\ setSearch) =
  button { className: "btn btn-primary text-center"
         , type: "button"
         , onClick: click
         } [ text "Search" ]
  where
    click = mkEffectFn1 $ \_ -> do
      case term of
        "" -> setSearch $ const Nothing
        _  -> setSearch $ const $ Just { database, term }
