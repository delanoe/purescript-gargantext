module Gargantext.Components.Search.SearchField
  ( Search, Props, defaultSearch, searchField, searchFieldComponent, isIsTex) where

import Prelude
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.String (length)
import Data.Set as Set
import Data.Tuple (fst, Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Gargantext.Utils.Reactix as R2
import Reactix.DOM.HTML as H
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

type Search = { datafield :: Maybe DataField
              , term      :: String
              , lang      :: Maybe Lang
              , node_id   :: Maybe Int
              }

eqSearch :: Search -> Search -> Boolean
eqSearch s s' =    (s.datafield == s'.datafield)
                && (s.term == s'.term)
                && (s.lang == s'.lang)
                && (s.node_id == s'.node_id)

defaultSearch :: Search
defaultSearch = { datafield: Nothing
                , term: ""
                , lang: Nothing
                , node_id: Nothing
                }

type Props =
  -- list of databases to search, or parsers to use on uploads
  ( databases :: Array Database
  , langs     :: Array Lang
  -- State hook for a search, how we get data in and out
  , search    :: R.State Search
  )

searchField :: Record Props -> R.Element
searchField p = R.createElement searchFieldComponent p []

searchFieldComponent :: R.Memo Props
searchFieldComponent = R.memo (R.hooksComponent "SearchField" cpt) eqProps
  where
    cpt props@{search: search@(s /\ _)} _ = do
      pure $
        div { className: "search-field-group", style: { width: "100%" } }
          [
            div { className: "row" }
              [
                div { className: "col-md-12" }
                [ searchInput search
                , if length s.term < 3
                  then
                    div {}[]
                  else
                    div {} [ dataFieldNav search dataFields
                            , if isExternal s.datafield
                              then databaseInput search props.databases
                              else div {} []

                            , if isHAL s.datafield
                              then orgInput search allOrgs
                              else div {} []

                            , if isIMT s.datafield
                              then componentIMT search
                              else div {} []

                            , if isCNRS s.datafield
                              then componentCNRS search
                              else div {} []
                            ]
                ]
              ]
          , div { className : "panel-footer" }
                [ if needsLang s.datafield then langNav search props.langs else div {} []
                , div {} []
                , div {className: "flex-center"} [submitButton search]
                ]
          ]
    eqProps p p' =    (fst p.search == fst p'.search)
                   && (p.databases  == p'.databases )
                   && (p.langs      == p'.langs     )
                   && (eqSearch (fst p.search) (fst p'.search))
--                   && (fst p.filters == fst p'.filters   )
    componentIMT (search /\ setSearch) =
      R.fragment
      [ ul {} $ map liCpt allIMTorgs
      --, filterInput fi
      ]
      where
        liCpt org =
          li {}
          [ input { type: "checkbox"
                  , checked: isIn org search.datafield
                  , on: {
                    change: \_ -> (setSearch $ _ { datafield = updateFilter org search.datafield })
                    }
                  }
          , if org == All_IMT
            then i {} [text  $ " " <> show org]
            else text $ " " <> show org
          ]
    componentCNRS (search /\ setSearch) =
      R.fragment [
        div {} []
      --, filterInput fi
      ]


isExternal :: Maybe DataField -> Boolean
isExternal (Just (External _)) = true
isExternal _ = false

isHAL :: Maybe DataField -> Boolean
isHAL (Just (External (Just (HAL _)))) = true
isHAL _ = false

isIsTex :: Maybe DataField -> Boolean
isIsTex (Just (External (Just (IsTex)))) = true
isIsTex _ = false

isIMT :: Maybe DataField -> Boolean
isIMT (Just ( External ( Just ( HAL ( Just ( IMT _)))))) = true
isIMT _ = false

isCNRS :: Maybe DataField -> Boolean
isCNRS (Just ( External ( Just ( HAL ( Just ( CNRS _)))))) = true
isCNRS _ = false

needsLang :: Maybe DataField -> Boolean
needsLang (Just Gargantext) = true
needsLang (Just Web)        = true
needsLang (Just ( External ( Just (HAL _)))) = true
needsLang _ = false


isIn :: IMT_org -> Maybe DataField -> Boolean
isIn org (Just (External (Just (HAL (Just (IMT imtOrgs)))))) = Set.member org imtOrgs
isIn _ _ = false

updateFilter :: IMT_org -> Maybe DataField -> Maybe DataField
updateFilter org (Just (External (Just (HAL (Just (IMT imtOrgs)))))) =
 (Just (External (Just (HAL (Just $ IMT imtOrgs')))))
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

updateFilter org _ = (Just (External (Just (HAL (Just (IMT imtOrgs'))))))
  where
    imtOrgs' = if org == All_IMT
                  then Set.fromFoldable allIMTorgs
                  else Set.fromFoldable [org]

------------------------------------------------------------------------
langList :: R.State Search -> Array Lang -> R.Element
langList (lang /\ setLang) langs =
              div { className: "form-group" }
                   [ div {className: "text-primary center"} [text "with lang"]
                   , R2.select { className: "form-control"
                               , on: { change: \e -> setLang $ _ {lang = lang' e}}
                               } (liItem <$> langs)
                   ]
    where
      liItem :: Lang -> R.Element
      liItem  lang = option {className : "text-primary center"} [ text (show lang) ]

      lang' e = readLang $ e .. "target" .. "value"


langNav :: R.State Search -> Array Lang -> R.Element
langNav ({lang} /\ setSearch) langs =
  R.fragment [ div {className: "text-primary center"} [text "with lang"]
             , div { className: "nav nav-tabs"} (liItem <$> langs)
             ]
    where
      liItem :: Lang -> R.Element
      liItem  lang' =
        div { className : "nav-item nav-link" <> if (Just lang') == lang then " active" else ""
            , on: { click: \_ -> setSearch $ _ { lang = Just lang' } }
            } [ text (show lang') ]

------------------------------------------------------------------------

dataFieldNav :: R.State Search -> Array DataField -> R.Element
dataFieldNav ({datafield} /\ setSearch) datafields =
  R.fragment [ div {className: "text-primary center"} [text "with DataField"]
             , div { className: "nav nav-tabs"} (liItem <$> dataFields)
             , div {className:"center"} [ text $ maybe "" doc datafield ]
             ]
    where
      liItem :: DataField -> R.Element
      liItem  df' =
        div { className : "nav-item nav-link" <> if (Just df') == datafield then " active" else ""
            , on: { click: \_ -> setSearch $ _ { datafield = Just df'} }
            } [ text (show df') ]


------------------------------------------------------------------------

databaseNav  :: R.State Search
              -> Array Database
              -> R.Element
databaseNav ({datafield} /\ setSearch) dbs =
  R.fragment [ div {className: "text-primary center"} [text "with DataField"]
             , div { className: "nav nav-tabs"} (liItem <$> dbs)
             , div {className:"center"} [ text $ maybe "" doc db ]
             ]
    where

      db = case datafield of
        (Just (External (Just x))) -> Just x
        _                          -> Nothing

      liItem :: Database -> R.Element
      liItem  df' =
        div { className : "nav-item nav-link" <> if (Just $ External $ Just df') == datafield then " active" else ""
            , on: { click: \_ -> setSearch $ _ { datafield = Just $ External $ Just df' } }
            } [ text (show df') ]



databaseInput :: R.State Search
              -> Array Database
              -> R.Element
databaseInput ({datafield} /\ setSearch) dbs =
   div { className: "form-group" }
   [ div {className: "text-primary center"} [text "in database"]
   , R2.select { className: "form-control"
               , on: { change: onChange }
               } (liItem <$> dbs)
   , div {className:"center"} [ text $ maybe "" doc db ]
   ]
    where
      db = case datafield of
        (Just (External (Just x))) -> Just x
        _                          -> Nothing

      liItem :: Database -> R.Element
      liItem  db = option {className : "text-primary center"} [ text (show db) ]

      onChange e = do
        let value = e .. "target" .. "value"
        setSearch $ _ {datafield =  Just $ External $ readDatabase value }


orgInput :: R.State Search -> Array Org -> R.Element
orgInput ({datafield} /\ setSearch) orgs =
  div { className: "form-group" }
  [ div {className: "text-primary center"} [text "filter with organization: "]
  , R2.select { className: "form-control"
              , on: { change: onChange }
              } (liItem <$> orgs)
  ]
    where
      liItem :: Org -> R.Element
      liItem  org = option {className : "text-primary center"} [ text (show org) ]
      onChange e = do
        let value = e .. "target" .. "value"
        setSearch $ _ { datafield = Just $ External $ Just $ HAL $ readOrg value }

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


searchInput :: R.State Search -> R.Element
searchInput ({term} /\ setSearch) =
  div { className : "" }
  [ input { defaultValue: term
          , className: "form-control"
          , type: "text"
          , on: { change : onChange }
          , placeholder: "Your Query here" }
  ]
  where
    onChange e = do
      let value = e .. "target" .. "value"
      setSearch $ _ {term = value }


submitButton :: R.State Search
             -> R.Element
submitButton (search /\ setSearch) =
  button { className: "btn btn-primary"
         , type: "button"
         , on: {click: doSearch}
         , style: { width: "100%" } 
         } [ text "Launch Search" ]
  where
    doSearch = \_ -> do
      case search.term of
        "" -> setSearch $ const defaultSearch
        _  -> setSearch $ const search
