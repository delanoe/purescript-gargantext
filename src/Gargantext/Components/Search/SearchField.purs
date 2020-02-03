module Gargantext.Components.Search.SearchField
  ( Search, Props, defaultSearch, searchField, searchFieldComponent, isIsTex) where

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over)
import Data.String (length)
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude (Unit, bind, const, discard, map, pure, show, ($), (&&), (<), (<$>), (<<<), (<>), (==))

import Gargantext.Components.Data.Lang (Lang)
import Gargantext.Components.Search.Types (DataField(..), Database(..), IMT_org(..), Org(..), SearchQuery(..), allIMTorgs, allOrgs, dataFields, defaultSearchQuery, doc, performSearch, readDatabase, readOrg) -- (Database(..), readDatabase, Lang(..), readLang, Org(..), readOrg, allOrgs, allIMTorgs, HAL_Filters(..), IMT_org(..))
import Gargantext.Sessions (Session)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

select :: forall props.
          R.IsComponent String props (Array R.Element)
          => Record props
          -> Array R.Element
          -> R.Element
select = R.createElement "select"

type Search = { databases :: Array Database
              , datafield :: Maybe DataField
              , lang      :: Maybe Lang
              , node_id   :: Maybe Int
              , term      :: String
              }

eqSearch :: Search -> Search -> Boolean
eqSearch s s' =    (s.databases == s'.databases)
                && (s.datafield == s'.datafield)
                && (s.term == s'.term)
                && (s.lang == s'.lang)
                && (s.node_id == s'.node_id)

defaultSearch :: Search
defaultSearch = { databases: []
                , datafield: Nothing
                , node_id: Nothing
                , lang: Nothing
                , term: ""
                }

type Props =
  -- list of databases to search, or parsers to use on uploads
  ( databases :: Array Database
  , langs     :: Array Lang
  -- State hook for a search, how we get data in and out
  , onSearch  :: GT.AsyncTaskWithType -> Effect Unit
  , search    :: R.State Search
  , session   :: Session
  )

searchField :: Record Props -> R.Element
searchField p = R.createElement searchFieldComponent p []

--searchFieldComponent :: R.Memo Props
--searchFieldComponent = R.memo (R.hooksComponent "SearchField" cpt) eqProps
searchFieldComponent :: R.Component Props
searchFieldComponent = R.hooksComponent "G.C.S.SearchField" cpt
  where
    cpt props@{onSearch, search: search@(s /\ _)} _ = do
      pure $
        H.div { className: "search-field-group", style: { width: "100%" } }
          [
            H.div { className: "row" }
              [
                H.div { className: "col-md-12" }
                [ searchInput {search}
                , if length s.term < 3
                  then
                    H.div {}[]
                  else
                    H.div {} [ dataFieldNav search dataFields
                            , if isExternal s.datafield
                              then databaseInput search props.databases
                              else H.div {} []

                            , if isHAL s.datafield
                              then orgInput search allOrgs
                              else H.div {} []

                            , if isIMT s.datafield
                              then componentIMT search
                              else H.div {} []

                            , if isCNRS s.datafield
                              then componentCNRS search
                              else H.div {} []
                            ]
                ]
              ]
          , H.div { className : "panel-footer" }
                [ if needsLang s.datafield then langNav search props.langs else H.div {} []
                , H.div {} []
                , H.div {className: "flex-center"} [submitButton {onSearch, search, session: props.session}]
                ]
          ]
    eqProps :: Record Props -> Record Props -> Boolean
    eqProps p p' =    (p.databases  == p'.databases )
                   && (p.langs      == p'.langs     )
                   && (eqSearch (fst p.search) (fst p'.search))
--                   && (fst p.filters == fst p'.filters   )
    componentIMT (search /\ setSearch) =
      R.fragment
      [ H.ul {} $ map liCpt allIMTorgs
      --, filterInput fi
      ]
      where
        liCpt org =
          H.li {}
          [ H.input { type: "checkbox"
                    , defaultChecked: isIn org search.datafield
                    , on: {
                      change: \_ -> (setSearch $ _ { datafield = updateFilter org search.datafield })
                      }
                  }
          , if org == All_IMT
            then H.i {} [H.text  $ " " <> show org]
            else H.text $ " " <> show org
          ]
    componentCNRS (search /\ setSearch) =
      R.fragment [
        H.div {} []
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
langNav :: R.State Search -> Array Lang -> R.Element
langNav ({lang} /\ setSearch) langs =
  R.fragment [ H.div {className: "text-primary center"} [H.text "with lang"]
             , H.div { className: "nav nav-tabs"} (liItem <$> langs)
             ]
    where
      liItem :: Lang -> R.Element
      liItem  lang' =
        H.div { className : "nav-item nav-link" <> if (Just lang') == lang then " active" else ""
            , on: { click: \_ -> setSearch $ _ { lang = Just lang' } }
            } [ H.text (show lang') ]

------------------------------------------------------------------------

dataFieldNav :: R.State Search -> Array DataField -> R.Element
dataFieldNav ({datafield} /\ setSearch) datafields =
  R.fragment [ H.div {className: "text-primary center"} [H.text "with DataField"]
             , H.div { className: "nav nav-tabs"} (liItem <$> dataFields)
             , H.div {className:"center"} [ H.text $ maybe "" doc datafield ]
             ]
    where
      liItem :: DataField -> R.Element
      liItem  df' =
        H.div { className : "nav-item nav-link" <> if (Just df') == datafield then " active" else ""
            , on: { click: \_ -> setSearch $ _ { datafield = Just df'} }
            } [ H.text (show df') ]


------------------------------------------------------------------------

databaseNav  :: R.State Search
              -> Array Database
              -> R.Element
databaseNav ({datafield} /\ setSearch) dbs =
  R.fragment [ H.div {className: "text-primary center"} [H.text "with DataField"]
             , H.div { className: "nav nav-tabs"} (liItem <$> dbs)
             , H.div {className:"center"} [ H.text $ maybe "" doc db ]
             ]
    where

      db = case datafield of
        (Just (External (Just x))) -> Just x
        _                          -> Nothing

      liItem :: Database -> R.Element
      liItem  df' =
        H.div { className : "nav-item nav-link" <> if (Just $ External $ Just df') == datafield then " active" else ""
            , on: { click: \_ -> setSearch $ _ { datafield = Just $ External $ Just df' } }
            } [ H.text (show df') ]



databaseInput :: R.State Search
              -> Array Database
              -> R.Element
databaseInput ({datafield} /\ setSearch) dbs =
   H.div { className: "form-group" }
   [ H.div {className: "text-primary center"} [H.text "in database"]
   , R2.select { className: "form-control"
               , on: { change: onChange }
               } (liItem <$> dbs)
   , H.div {className:"center"} [ H.text $ maybe "" doc db ]
   ]
    where
      db = case datafield of
        (Just (External (Just x))) -> Just x
        _                          -> Nothing

      liItem :: Database -> R.Element
      liItem  db' = H.option {className : "text-primary center"} [ H.text (show db') ]

      onChange e = do
        let value = R2.unsafeEventValue e
        setSearch $ _ {datafield =  Just $ External $ readDatabase value }


orgInput :: R.State Search -> Array Org -> R.Element
orgInput ({datafield} /\ setSearch) orgs =
  H.div { className: "form-group" }
  [ H.div {className: "text-primary center"} [H.text "filter with organization: "]
  , R2.select { className: "form-control"
              , on: { change: onChange }
              } (liItem <$> orgs)
  ]
    where
      liItem :: Org -> R.Element
      liItem  org = H.option {className : "text-primary center"} [ H.text (show org) ]
      onChange e = do
        let value = R2.unsafeEventValue e
        setSearch $ _ { datafield = Just $ External $ Just $ HAL $ readOrg value }

filterInput :: R.State String -> R.Element
filterInput (term /\ setTerm) =
  H.div {className: "form-group"} [ H.input { defaultValue: term
                              , className: "form-control"
                              , type: "text"
                              , on: { change: setTerm <<< const <<< R2.unsafeEventValue }
                              , "required pattern": "[[0-9]+[ ]+]*"
                              -- TODO                          ^FIXME not sure about the regex comprehension: that should match "123 2334 44545" only (Integers separated by one space)
                              -- form validation with CSS
                              -- DOC: https://developer.mozilla.org/en-US/docs/Learn/HTML/Forms/Form_validation
                              , placeholder : "Filter with struct_Ids as integer" 
                              }
                      ]


type SearchInputProps =
  (
    search :: R.State Search
  )

searchInput :: Record SearchInputProps -> R.Element
searchInput p = R.createElement searchInputComponent p []

searchInputComponent :: R.Component SearchInputProps
searchInputComponent = R.hooksComponent "G.C.S.SearchInput" cpt
  where
    cpt {search: (search /\ setSearch)} _ = do
      pure $
        H.div { className : "" }
        [ H.input { defaultValue: search.term
                  , className: "form-control"
                  , type: "text"
                  , on: { change : onChange setSearch }
                  , placeholder: "Your Query here" }
        ]
    onChange setSearch e = do
      let value = R2.unsafeEventValue e
      setSearch $ _ { term = value }

type SubmitButtonProps =
  (
    onSearch :: GT.AsyncTaskWithType -> Effect Unit
  , search :: R.State Search
  , session :: Session
  )

submitButton :: Record SubmitButtonProps -> R.Element
submitButton p = R.createElement submitButtonComponent p []

submitButtonComponent :: R.Component SubmitButtonProps
submitButtonComponent = R.hooksComponent "G.C.S.SubmitButton" cpt
  where
    cpt {onSearch, search: (search /\ _), session} _ =
      pure $
        H.button { className: "btn btn-primary"
                 , type: "button"
                 , on: {click: doSearch onSearch session search}
                 , style: { width: "100%" }
                 } [ H.text "Launch Search" ]

    doSearch os s q = \_ -> do
      log2 "[submitButton] searching" q
      triggerSearch os s q
      --case search.term of
      --  "" -> setSearch $ const defaultSearch
      --  _  -> setSearch $ const q

triggerSearch :: (GT.AsyncTaskWithType -> Effect Unit) -> Session -> Search -> Effect Unit
triggerSearch os s q =
  launchAff_ $ do

    liftEffect $ do
      -- log2 "Searching datafield: " $ show q.database
      log2 "[triggerSearch] Searching term: " q.term
      log2 "[triggerSearch] Searching lang: " q.lang

    case q.node_id of
      Nothing -> liftEffect $ log "[triggerSearch] node_id is Nothing, don't know what to do"
      Just id -> do
        task <- performSearch s id $ searchQuery q
        liftEffect $ do
          log2 "[triggerSearch] task" task
          os task

    --liftEffect $ do
    --  log2 "Return:" r
    --  modalShow "addCorpus"

searchQuery :: Search -> SearchQuery
searchQuery {datafield: Nothing, term} =
  over SearchQuery (_ {query=term}) defaultSearchQuery
searchQuery {databases, datafield, lang, term, node_id} =
  over SearchQuery (_ { databases=databases
                      , datafield=datafield
                      , lang=lang
                      , query=term
                      , node_id=node_id
                      }) defaultSearchQuery
