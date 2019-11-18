module Gargantext.Components.Search.SearchField
  ( Search, Props, searchField, searchFieldComponent )where

import Prelude (bind, const, identity, pure, show, ($), (/=), (<$>), (||), (==), map, (<>), (&&), (*>), (>>=), (>=>), (<))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.String (length)
import Data.Set as Set
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
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

defaultSearch :: Search
defaultSearch = { datafield: Just Gargantext
                , term: ""
                , lang: Just EN
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
      term@(curTerm /\ _) <- R.useState' search.term
      df@(curDf /\ setDf) <- R.useState' (Just Gargantext :: Maybe DataField)
      lang@(curLg /\ _)   <- R.useState' (Nothing :: Maybe Lang)
      fi                  <- R.useState' ""
      pure $
          div { className: "search-field-group" }
              [ searchInput term
              , if length curTerm < 3
                  then
                    div {}[]
                  else
                    div {} [ langNav lang props.langs
                           , if curLg == Nothing
                               then
                                 div {}[]
                               else
                                 div {} [ dataFieldNav df dataFields
                                         , if isExternal curDf
                                             then databaseInput df props.databases
                                             else div {} []

                                         , if isHAL curDf
                                             then orgInput df allOrgs
                                             else div {} []

                                         , if isIMT curDf
                                             then
                                               R.fragment
                                                     [ ul {} $ map ( \org -> li {}
                                                                   [ input { type: "checkbox"
                                                                           , checked: isIn org curDf
                                                                           , on: {change: \_ -> (setDf
                                                                                             $ const
                                                                                             $ updateFilter org curDf)
                                                                                 }
                                                                           }
                                                                   , if org == All_IMT
                                                                        then i {} [text  $ " " <> show org]
                                                                        else text $ " " <> show org
                                                                   ]
                                                                   ) allIMTorgs
                                                     , filterInput fi
                                                     ]
                                              else div {} []

                                         , if isCNRS curDf
                                            then
                                              R.fragment [ div {} [], filterInput fi]
                                            else
                                              div {} []

                                        , if isIsTex curDf
                                            then H.div { className: ""
                                                       , id: "search-popup-tooltip"
                                                       , title: "Node settings"
                                                       , data: { toggle: "tooltip"
                                                               , placement: "right"
                                                               }
                                                       }
                                                       [ H.div {id: "arrow"} []
                                                       , H.div { className: "panel panel-default"
                                                                , style: { border    : "1px solid rgba(0,0,0,0.2)"
                                                                         , boxShadow : "0 2px 5px rgba(0,0,0,0.2)"
                                                                         }
                                                               } [ H.iframe { src: "https://istex.gargantext.org", width: "100%", height: "100%"} []
                                                                 ]
                                                       ]
                                              else H.div {} []

                                          ]
                            ]
              , submitButton node_id df term lang props.search
              ]
    hasChanged p p' = (fst p.search /= fst p'.search)
                   || (p.databases  /= p'.databases )
                   || (p.langs      /= p'.langs     )
--                   || (fst p.filters /= fst p'.filters   )


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
langList :: R.State (Maybe Lang) -> Array Lang -> R.Element
langList (lang /\ setLang) langs =
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

langNav :: R.State (Maybe Lang) -> Array Lang -> R.Element
langNav (lang /\ setLang) langs =
  R.fragment [ div {className: "text-primary center"} [text "with lang"]
             , div { className: "nav nav-tabs"} (liItem <$> langs)
             ]
    where
      liItem :: Lang -> R.Element
      liItem  lang' = div { className : "nav-item nav-link" <> if (Just lang') == lang then " active" else ""
                         , on: { click: \_ -> setLang $ const $ Just lang' }
                         } [ text (show lang') ]

------------------------------------------------------------------------

dataFieldNav :: R.State (Maybe DataField) -> Array DataField -> R.Element
dataFieldNav (df /\ setDf) datafields =
  R.fragment [ div {className: "text-primary center"} [text "with DataField"]
             , div { className: "nav nav-tabs"} (liItem <$> dataFields)
             , div {className:"center"} [ text $ maybe "" doc df ]
             ]
    where
      liItem :: DataField -> R.Element
      liItem  df' = div { className : "nav-item nav-link" <> if (Just df') == df then " active" else ""
                         , on: { click: \_ -> setDf $ const $ Just df'
                               }
                         } [ text (show df') ]


------------------------------------------------------------------------
databaseInput :: R.State (Maybe DataField)
              -> Array Database
              -> R.Element
databaseInput (df /\ setDf) dbs =
   div { className: "form-group" }
                   [ div {className: "text-primary center"} [text "in database"]
                   , R2.select { className: "form-control"
                               , on: { change: \e -> setDf
                                         $ const
                                         $ Just
                                         $ External
                                         $ readDatabase
                                         $ e .. "target" .. "value"
                                   }
                               } (liItem <$> dbs)
                   , div {className:"center"} [ text $ maybe "" doc db ]
                   ]
    where
      db = case df of
        (Just (External (Just x))) -> Just x
        _                          -> Nothing

      liItem :: Database -> R.Element
      liItem  db = option {className : "text-primary center"} [ text (show db) ]


orgInput :: R.State (Maybe DataField) -> Array Org -> R.Element
orgInput (curDf /\ setDf) orgs =
  div { className: "form-group" }
                   [ div {className: "text-primary center"} [text "filter with organization: "]
                   , R2.select { className: "form-control"
                               , on: { change: \e -> setDf
                                                   $ const
                                                   $ Just $ External $ Just $ HAL
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
             -> R.State (Maybe DataField)
             -> R.State String
             -> R.State (Maybe Lang)
             -> R.State (Maybe Search)
             -> R.Element
submitButton node_id (datafield /\ _) (term /\ _) (lang /\ _) (_ /\ setSearch) = div { className : "panel-footer" }
                     [ button { className: "btn btn-primary"
                              , type: "button"
                              , on: {click: doSearch}
                              } [ text "Launch Search" ]
                       ]
  where
    doSearch = \_ -> do
      case term of
        "" -> setSearch $ const Nothing
        _  -> setSearch $ const $ Just {datafield, term, lang, node_id}




