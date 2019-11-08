module Gargantext.Components.Search.SearchField
  ( Search, Props, searchField, searchFieldComponent )where

import Prelude (bind, const, identity, pure, show, ($), (/=), (<$>), (||), (==), map, (<>), (&&), (*>), (>>=), (>=>))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Gargantext.Utils.Reactix as R2
import FFI.Simple ((..))
import Reactix as R
import Reactix.DOM.HTML (text, button, div, input, span, ul, li, a, option, text, i)
import Gargantext.Components.Search.Types -- (Database(..), readDatabase, Lang(..), readLang, Org(..), readOrg, allOrgs, allIMTorgs, HAL_Filters(..), IMT_org(..))

select :: forall props.
          R.IsComponent String props (Array R.Element)
          => Record props
          -> Array R.Element
          -> R.Element
select = R.createElement "select"

type Search = { database :: Maybe Database
              , term     :: String
              , lang     :: Maybe Lang
              , org      :: Maybe Org
              , filters  :: Maybe HAL_Filters
              , node_id  :: Maybe Int
              }

defaultSearch :: Search
defaultSearch = { database: Just Gargantext
                , term: ""
                , lang: Nothing
                , org : Nothing
                , filters: Nothing
                , node_id: Nothing
                }

type Props =
  -- list of databases to search, or parsers to use on uploads
  ( databases :: Array Database
  , langs     :: Array Lang
  -- State hook for a search, how we get data in and out
  , search    :: R.State (Maybe Search)
  , node_id   :: Maybe Int
  )

searchField :: Record Props -> R.Element
searchField p = R.createElement searchFieldComponent p []

searchFieldComponent :: R.Memo Props
searchFieldComponent = R.memo (R.hooksComponent "SearchField" cpt) hasChanged
  where
    cpt props@{node_id} _ = do
      let search = maybe defaultSearch identity (fst props.search)
      term <- R.useState' search.term
      db@(curDb /\ setDb)                <- R.useState' (Just Gargantext :: Maybe Database)
      lang                               <- R.useState' (Nothing :: Maybe Lang)
      org@(curOrg /\ setOrg)             <- R.useState' (Nothing :: Maybe Org)
      filters@(curFilters /\ setFilters) <- R.useState' (Nothing :: Maybe HAL_Filters)
      fi                                 <- R.useState' ""
      pure $
          div { className: "search-field-group" }
              [ searchInput term
              , langInput lang props.langs
              , databaseInput db filters org props.databases

              , if isHAL curDb
                   then orgInput org allOrgs
                   else div {} []

              , if isHAL curDb
                  then
                    if curOrg == (Just IMT)
                      then
                        R.fragment
                          [ ul {} $ map ( \org' -> li {}
                                        [ input { type: "checkbox"
                                                , checked: isInFilters org' curFilters
                                                , on: {change: \_ -> setFilters
                                                                  $ const
                                                                  $ updateFilter org' curFilters
                                                      }
                                                }
                                        , if org' == All_IMT
                                             then i {} [text  $ " " <> show org']
                                             else text $ " " <> show org'
                                        ]
                                        ) allIMTorgs
                          , filterInput fi
                          ]
                      else
                        if curOrg == (Just CNRS)
                           then
                             R.fragment [ div {} [], filterInput fi]
                           else
                             div {} []
                  else
                    div {} []
              , submitButton node_id db term lang org filters props.search
              ]
    hasChanged p p' = (fst p.search /= fst p'.search)
                   || (p.databases  /= p'.databases )
                   || (p.langs      /= p'.langs     )
--                   || (fst p.filters /= fst p'.filters   )

isHAL :: Maybe Database -> Boolean
isHAL (Just HAL) = true
isHAL _          = false

isInFilters :: IMT_org -> Maybe HAL_Filters -> Boolean
isInFilters org (Just (HAL_IMT { imtOrgs })) = Set.member org imtOrgs
isInFilters _ _ = false

updateFilter :: IMT_org -> Maybe HAL_Filters -> Maybe HAL_Filters
updateFilter org (Just (HAL_IMT {imtOrgs})) =
  Just $ HAL_IMT { imtOrgs: imtOrgs'
                 , structIds: Set.empty
                 }
  where
    imtOrgs' = if Set.member org imtOrgs
                  then
                    if org == All_IMT
                       then Set.empty
                       else Set.delete All_IMT $ Set.delete org imtOrgs
                  else
                    if org == All_IMT
                       then Set.fromFoldable allIMTorgs
                       else Set.insert org imtOrgs

updateFilter org _ = Just $ HAL_IMT { imtOrgs: imtOrgs', structIds: Set.empty}
  where
    imtOrgs' = if org == All_IMT
                  then Set.fromFoldable allIMTorgs
                  else Set.fromFoldable [org]

langInput :: R.State (Maybe Lang) -> Array Lang -> R.Element
langInput (lang /\ setLang) langs =
              div { className: "form-group" }
                   [ div {className: "text-primary center"} [text "with lang"]
                   , R2.select { className: "form-control"
                               , on: { change: \e -> setLang
                                                   $ const
                                                   $ readLang
                                                   $ e .. "target" .. "value"
                                     }
                               } (liItem <$> langs)
                   ]
    where
      liItem :: Lang -> R.Element
      liItem  lang = option {className : "text-primary center"} [ text (show lang) ]


databaseInput :: R.State (Maybe Database)
              -> R.State (Maybe HAL_Filters)
              -> R.State (Maybe Org)
              -> Array Database
              -> R.Element
databaseInput (db /\ setDB) (_ /\ setFilters) (_ /\ setOrg) dbs =
   div { className: "form-group" }
                   [ div {className: "text-primary center"} [text "in database"]
                   , R2.select { className: "form-control"
                               , on: { change: \e -> (setDB
                                         $ const
                                         $ readDatabase
                                         $ e .. "target" .. "value")
                                         *> (setOrg     $ const Nothing)
                                         *> (setFilters $ const Nothing)
                                   }
                               } (liItem <$> dbs)
                   , div {className:"center"} [ text $ maybe "" doc db ]
                   ]
    where
      liItem :: Database -> R.Element
      liItem  db = option {className : "text-primary center"} [ text (show db) ]


orgInput :: R.State (Maybe Org) -> Array Org -> R.Element
orgInput (org /\ setOrg) orgs =
  div { className: "form-group" }
                   [ div {className: "text-primary center"} [text "filter with organization: "]
                   , R2.select { className: "form-control"
                               , on: { change: \e -> setOrg
                                                   $ const
                                                   $ readOrg
                                                   $ e .. "target" .. "value"
                                      }
                               } (liItem <$> orgs)
                   ]
    where
      liItem :: Org -> R.Element
      liItem  org = option {className : "text-primary center"} [ text (show org) ]

filterInput :: R.State String -> R.Element
filterInput (term /\ setTerm) =
  div {className: "form-group"} [ input { defaultValue: term
                              , className: "form-control"
                              , type: "text"
                              , on: { change: \e -> setTerm
                                                  $ const
                                                  $ e .. "target" .. "value"
                                    }
                              , "required pattern": "[[0-9]+[ ]+]*"
                              -- TODO                          ^FIXME not sure about the regex comprehension: that should match "123 2334 44545" only (Integers separated by one space)
                              -- form validation with CSS
                              -- DOC: https://developer.mozilla.org/en-US/docs/Learn/HTML/Forms/Form_validation
                              , placeholder : "Filter with struct_Ids as integer" 
                              }
                      ]


searchInput :: R.State String -> R.Element
searchInput (term /\ setTerm) =
              div { className : "" }
                    [ input { defaultValue: term
                    , className: "form-control"
                    , type: "text"
                    , on: { change : \e -> setTerm $ const $ e .. "target" .. "value" }
                    , placeholder: "Your Query here" }
                    ]


submitButton :: Maybe Int
             -> R.State (Maybe Database)
             -> R.State String
             -> R.State (Maybe Lang)
             -> R.State (Maybe Org)
             -> R.State (Maybe HAL_Filters)
             -> R.State (Maybe Search)
             -> R.Element
submitButton node_id (database /\ _) (term /\ _) (lang /\ _) (org/\_) (filters /\ _) (_ /\ setSearch) = div { className : "panel-footer" }
                     [ button { className: "btn btn-primary"
                              , type: "button"
                              , on: {click: doSearch}
                              } [ text "Launch Search" ]
                       ]
  where
    doSearch = \_ -> do
      case term of
        "" -> setSearch $ const Nothing
        _  -> setSearch $ const $ Just { database, lang, filters, term, org, node_id}
