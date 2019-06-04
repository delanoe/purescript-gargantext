module Gargantext.Components.Search.SearchField
  ( File, Query(..), Search, Props, searchField, searchFieldComponent )where

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
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..), (.=))
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.DOM.HTML (text, button, div, input, option, form, span, ul, li, a, i)
import Reactix.SyntheticEvent as E
import Gargantext.Components.Search.Types

type File = { name :: String, contents :: String }
data Query = Term String
           | QueryFile File
derive instance eqQuery :: Eq Query

select = R.createElement "select"

type Search = { database :: Maybe Database, query :: Query }

defaultSearch :: Search
defaultSearch = { database: Nothing, query: Term "" }

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
      query <- R.useState $ \_ -> pure search.query
      db   <- R.useState $ \_ -> pure Nothing
      pure $
          div { className: "search-field input-group" }
              [ databaseInput db props.databases
              , searchDrop query
              , span { className: "input-group-btn" }
                     [ submitButton db query props.search ]
              ]
    hasChanged p p' = (fst p.search /= fst p'.search) || (p.databases /= p'.databases)

databaseInput :: R.State (Maybe Database) -> Array Database -> R.Element
databaseInput (db /\ setDB) dbs =
  div { className: "input-group-btn search-panel dropdown" }
      [
        dropdownBtn db
        , ul {className: "dropdown-menu", role: "menu"} (liItem <$> dbs)
      ]
  where
    liItem db = li { onClick }
                   [ a {href: "#"} [text (show db) ] ]
                where
                  onClick = mkEffectFn1 $ \_ -> setDB $ Just db
    dropdownBtnProps = { id: "search-dropdown"
                        , className: "btn btn-default dropdown-toggle"
                        , type: "button"} .= "data-toggle" $ "dropdown"
    dropdownBtn (Just db) = button dropdownBtnProps [ span {} [ text (show db) ] ]
    dropdownBtn (Nothing) = button dropdownBtnProps [ span {} [ text "-" ] ]

searchDrop :: R.State Query -> R.Element
searchDrop ((Term term) /\ setQuery) =
  input { defaultValue: term
        , className: "form-control"
        , type: "text"
        , onChange
        , onDrop
        , placeholder }
  where
    onChange = mkEffectFn1 $ \e -> setQuery $ Term $ e .. "target" .. "value"
    onDrop = mkEffectFn1 $ \(e :: E.SyntheticEvent DE.MouseEvent) -> do
      liftEffect $ log2 "drop:" (f e)
      E.preventDefault e
      E.stopPropagation e  -- to keep Firefox happy, otherwise it redirects
      setQuery $ QueryFile $ file e
      where
        f e = (e .. "dataTransfer" .. "files" .. "0")
        file e = {name: (f e) .. "name", contents: ""}
searchDrop ((QueryFile file) /\ setQuery) =
  span { className: "input-group" } [
    span {className: "label label-default"} [
       text $ file.name
       , i { className: "remove glyphicon glyphicon-remove-sign glyphicon-white"} []
    ]
  ]


submitButton :: R.State (Maybe Database) -> R.State Query -> R.State (Maybe Search) -> R.Element
submitButton (database /\ _) ((Term query) /\ _) (_ /\ setSearch) =
  button { className: "btn btn-default"
         , type: "button"
         , onClick: click } [ text "Search" ]
  where
    click = mkEffectFn1 $ \_ -> do
      setSearch $ case query of
        "" -> Nothing
        _       -> Just { database, Term query }
submitButton (database /\ _) ((QueryFile file) /\ _) (_ /\ setSearch) =
  button { className: "btn btn-default"
         , type: "button"
         , onClick: click } [ text "Upload" ]
  where
    click = mkEffectFn1 $ \_ -> do
      setSearch $ case query of
        "" -> Nothing
        _       -> Just { database, Term query }

