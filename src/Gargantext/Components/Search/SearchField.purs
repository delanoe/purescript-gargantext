module Gargantext.Components.Search.SearchField
  ( Search, Props, searchField, searchFieldComponent )where

import Prelude (bind, const, identity, pure, show, ($), (/=), (<$>), (||), (==), map, (<>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Gargantext.Utils.Reactix as R2
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Reactix as R
import Reactix.DOM.HTML (text, button, div, input, span, ul, li, a, option, text)
import Gargantext.Components.Search.Types (Database(..), readDatabase, Lang(..), readLang, Org(..), readOrg, allOrgs, allIMTorgs)

select :: forall props.
          R.IsComponent String props (Array R.Element)
          => Record props
          -> Array R.Element
          -> R.Element
select = R.createElement "select"



type Search = { database :: Maybe Database
              , term :: String
              , lang :: Maybe Lang
              }

defaultSearch :: Search
defaultSearch = { database: Nothing
                , term: ""
                , lang: Nothing
                }

type Props =
  -- list of databases to search, or parsers to use on uploads
  ( databases :: Array Database
  , langs   :: Array Lang
  -- State hook for a search, how we get data in and out
  , search :: R.State (Maybe Search)
  )

searchField :: Record Props -> R.Element
searchField p = R.createElement searchFieldComponent p []

searchFieldComponent :: R.Memo Props
searchFieldComponent = R.memo (R.hooksComponent "SearchField" cpt) hasChanged
  where
    cpt props _ = do
      let search = maybe defaultSearch identity (fst props.search)
      term <- R.useState' search.term
      db@(db' /\ _) <- R.useState' (Nothing :: Maybe Database)
      lang          <- R.useState' (Nothing :: Maybe Lang)
      org@(o /\ _)           <- R.useState' (Nothing :: Maybe Org)
      fi            <- R.useState' ""
      pure $
          div { className: "search-field-group" }
              [ searchInput term
              , div {className: "text-primary center"} [text "in"]
              , databaseInput db   props.databases

              , if db' /= Just PubMed then langInput     lang props.langs else div {} []
              , if db' == Just HAL    then orgInput org allOrgs else div {} []
              , if o == (Just $ CNRS {orgs:[]})
                   then filterInput fi
                   else if o == (Just $ IMT {orgs:[]})
                   then ul {} $ map (\o -> li {} [input { type: "checkbox" }, text $ " " <> show o]) allIMTorgs
                        else div {} []

              , div { className: "panel-footer" } [ submitButton db term lang props.search ]
              ]
    hasChanged p p' = (fst p.search /= fst p'.search)
                   || (p.databases  /= p'.databases )
                   || (p.langs      /= p'.langs     )


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


langInput :: R.State (Maybe Lang) -> Array Lang -> R.Element
langInput (lang /\ setLang) langs =
  div { className: "form-group" } 
                   [ text "with lang"
                   , R2.select { className: "form-control"
                               , onChange: mkEffectFn1
                                         $ \e -> setLang
                                         $ const
                                         $ readLang
                                         $ e .. "target" .. "value"
                               } (liItem <$> langs)
                   ]
    where
      liItem :: Lang -> R.Element
      liItem  lang = option {className : "text-primary center"} [ text (show lang) ]

orgInput :: R.State (Maybe Org) -> Array Org -> R.Element
orgInput (org /\ setOrg) orgs =
  div { className: "form-group" } 
                   [ text "filter with organization: "
                   , R2.select { className: "form-control"
                               , onChange: mkEffectFn1
                                         $ \e -> setOrg
                                         $ const
                                         $ readOrg
                                         $ e .. "target" .. "value"
                               } (liItem <$> orgs)
                   ]
    where
      liItem :: Org -> R.Element
      liItem  org = option {className : "text-primary center"} [ text (show org) ]

filterInput :: R.State String -> R.Element
filterInput (term /\ setTerm) =
  input { defaultValue: term
        , className: "form-control"
        , type: "text"
        , onChange: mkEffectFn1 $ \e -> setTerm $ const $ e .. "target" .. "value"
        , placeHolder : "Struct_Ids as integer" }


searchInput :: R.State String -> R.Element
searchInput (term /\ setTerm) =
  input { defaultValue: term
        , className: "form-control"
        , type: "text"
        , onChange
        , placeHolder: "Your Query here" }
  where onChange = mkEffectFn1 $ \e -> setTerm $ const $ e .. "target" .. "value"


submitButton :: R.State (Maybe Database)
             -> R.State String
             -> R.State (Maybe Lang)
             -> R.State (Maybe Search)
             -> R.Element
submitButton (database /\ _) (term /\ _) (lang /\ _) (_ /\ setSearch) =
  button { className: "btn btn-primary text-center"
         , type: "button"
         , onClick: click
         } [ text "Search" ]
  where
    click = mkEffectFn1 $ \_ -> do
      case term of
        "" -> setSearch $ const Nothing
        _  -> setSearch $ const $ Just { database, lang, term }
