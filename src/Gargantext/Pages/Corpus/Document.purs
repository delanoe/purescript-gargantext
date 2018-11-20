module Gargantext.Pages.Corpus.Document where


import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (.??), (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens, (?~))
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import React (ReactElement)
import React.DOM (a, button, div, h4, h6, input, li, nav, option, p, select, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, href, name, onChange, onInput, placeholder, role, style, value)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Trans.Class (lift)

import Gargantext.Prelude
import Gargantext.Config          (toUrl, NodeType(..), End(..))
import Gargantext.Config.REST     (get)
import Gargantext.Components.Node (NodePoly(..))


type State =
  { document   :: Maybe (NodePoly Document)
  , inputValue :: String
  }

initialState :: {} -> State
initialState {} =
  { document   : Nothing
  , inputValue : ""
  }

data Action
  = Load Int
  | ChangeString String
  | SetInput String


newtype Status = Status { failed    :: Int
                        , succeeded :: Int
                        , remaining :: Int
                        }

newtype DocumentV3 =
  DocumentV3 { abstract           :: Maybe String
             , authors            :: Maybe String
             --, error              :: Maybe String
             , language_iso2      :: Maybe String
             , language_iso3      :: Maybe String
             , language_name      :: Maybe String
             , publication_date   :: Maybe String
             , publication_day    :: Maybe Int
             , publication_hour   :: Maybe Int
             , publication_minute :: Maybe Int
             , publication_month  :: Maybe Int
             , publication_second :: Maybe Int
             , publication_year   :: Maybe Int
             , realdate_full_     :: Maybe String
             , source             :: Maybe String
             , statuses           :: Maybe (Array Status)
             , title              :: Maybe String
             }

defaultNodeDocumentV3 :: NodePoly DocumentV3
defaultNodeDocumentV3 =
  NodePoly { id : 0
           , typename : 0
           , userId   : 0
           , parentId : 0
           , name     : "Default name"
           , date     : "Default date"
           , hyperdata : defaultDocumentV3
         }

defaultDocumentV3 :: DocumentV3
defaultDocumentV3 =
  DocumentV3 { abstract           : Nothing
             , authors            : Nothing
             --, error              : Nothing
             , language_iso2      : Nothing
             , language_iso3      : Nothing
             , language_name      : Nothing
             , publication_date   : Nothing
             , publication_day    : Nothing
             , publication_hour   : Nothing
             , publication_minute : Nothing
             , publication_month  : Nothing
             , publication_second : Nothing
             , publication_year   : Nothing
             , realdate_full_     : Nothing
             , source             : Nothing
             , statuses           : Nothing
             , title              : Nothing
             }

data Document =
  Document { abstract           :: Maybe String
           , authors            :: Maybe String
           , bdd                :: Maybe String
           , doi                :: Maybe String
           , language_iso2      :: Maybe String
           -- , page               :: Maybe Int
           , publication_date   :: Maybe String
           --, publication_second :: Maybe Int
           --, publication_minute :: Maybe Int
           --, publication_hour   :: Maybe Int
           , publication_day    :: Maybe Int
           , publication_month  :: Maybe Int
           , publication_year   :: Maybe Int
           , source             :: Maybe String
           , institutes         :: Maybe String
           , title              :: Maybe String
           , uniqId             :: Maybe String
           --, url                :: Maybe String
           --, text               :: Maybe String
           }

defaultNodeDocument :: NodePoly Document
defaultNodeDocument =
  NodePoly { id : 0
           , typename : 0
           , userId   : 0
           , parentId : 0
           , name     : "Default name"
           , date     : "Default date"
           , hyperdata : defaultDocument
         }

-- TODO: BUG if DOI does not exist, page is not shown
defaultDocument :: Document
defaultDocument =
  Document { abstract           : Nothing
           , authors            : Nothing
           , bdd                : Nothing
           , doi                : Nothing
           , language_iso2      : Nothing
           --, page               : Nothing
           , publication_date   : Nothing
           --, publication_second : Nothing
           --, publication_minute : Nothing
           --, publication_hour   : Nothing
           , publication_day    : Nothing
           , publication_month  : Nothing
           , publication_year   : Nothing
           , source             : Nothing
           , institutes         : Nothing
           , title              : Nothing
           , uniqId             : Nothing
           --, url                : Nothing
           --, text               : Nothing
           }

derive instance genericDocument   :: Generic Document   _
derive instance genericDocumentV3 :: Generic DocumentV3 _
derive instance genericStatus     :: Generic Status     _

instance showDocument :: Show Document where
  show = genericShow

instance showDocumentV3 :: Show DocumentV3 where
  show = genericShow

instance showStatus :: Show Status where
  show = genericShow

instance decodeStatus :: DecodeJson Status
  where
    decodeJson json = do
      obj <- decodeJson json
      failed <- obj .? "failed"
      succeeded <- obj .? "succeeded"
      remaining <- obj .? "remaining"
      pure $ Status {failed, succeeded, remaining}


instance decodeDocumentV3 :: DecodeJson DocumentV3
  where
    decodeJson json = do
      obj <- decodeJson json
      abstract <- obj .?? "abstract"
      authors  <- obj .? "authors"
      --error    <- obj .? "error"
      language_iso2 <- obj .? "language_iso2"
      language_iso3 <- obj .? "language_iso3"
      language_name <- obj .? "language_name"
      publication_date   <- obj .? "publication_date"
      publication_day    <- obj .? "publication_day"
      publication_hour   <- obj .? "publication_hour"
      publication_minute <- obj .? "publication_minute"
      publication_month  <- obj .? "publication_month"
      publication_second <- obj .? "publication_second"
      publication_year   <- obj .? "publication_year"
      realdate_full_     <- obj .? "realdate_full_"
      source   <- obj .? "source"
      statuses <- obj .? "statuses"
      title    <- obj .? "title"
      pure $ DocumentV3 { abstract
                        , authors
                        --, error
                        , language_iso2
                        , language_iso3
                        , language_name
                        , publication_date
                        , publication_day
                        , publication_hour
                        , publication_minute
                        , publication_month
                        , publication_second
                        , publication_year
                        , realdate_full_
                        , source
                        , statuses
                        , title
                        }

instance decodeDocument :: DecodeJson Document
  where
    decodeJson json = do
      obj <- decodeJson json
      abstract <- obj .?? "abstract"
      authors  <- obj .?? "authors"
      bdd      <- obj .?? "bdd"
      doi      <- obj .?? "doi"
      language_iso2 <- obj .?? "language_iso2"
      -- page          <- obj .?? "page"
      publication_date   <- obj .?? "publication_date"
      --publication_second <- obj .?? "publication_second"
      --publication_minute <- obj .?? "publication_minute"
      --publication_hour   <- obj .?? "publication_hour"
      publication_day    <- obj .?? "publication_day"
      publication_month  <- obj .?? "publication_month"
      publication_year   <- obj .?? "publication_year"
      source             <- obj .?? "sources"
      institutes         <- obj .?? "institutes"
      title              <- obj .?? "title"
      uniqId             <- obj .?? "uniqId"
      --url                <- obj .? "url"
      --text               <- obj .? "text"
      pure $ Document { abstract
                      , authors
                      , bdd
                      , doi
                      , language_iso2
                      -- , page
                      , publication_date
                      --, publication_second
                      --, publication_minute
                      --, publication_hour
                      , publication_day
                      , publication_month
                      , publication_year
                      , source
                      , institutes
                      , title
                      , uniqId
                      --, url
                      --, text
                      }

------------------------------------------------------------------------
performAction :: PerformAction State {} Action
performAction (Load nId) _ _ = do
  node <- lift $ getNode nId
  void $ modifyState $ _document ?~ node
  logs $ "Node Document " <> show nId <> " fetched."
performAction (ChangeString ps) _ _ = pure unit
performAction (SetInput ps) _ _ = void <$> modifyState $ _ { inputValue = ps }


getNode :: Int -> Aff (NodePoly Document)
getNode = get <<< toUrl Back Node

_document :: Lens' State (Maybe (NodePoly Document))
_document = lens (\s -> s.document) (\s ss -> s{document = ss})
------------------------------------------------------------------------

docview :: Spec State {} Action
docview = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      [
          div [className "container1"]
          [
            div [className "row"]
            [
              div [className "col-md-4", style {border : "1px solid black", padding : "34px"}]
              [
                div [className "row"]
                [
                  div [ className "col-md-12 input-group mb-3"] 
                      [ select [ className "form-control custom-select"
                               , onChange (\e -> dispatch (ChangeString $ (unsafeCoerce e).target.value))
                               ] $ map optps aryPS 
                      ]
                ]
              , div [className "row", style { marginTop : "35px"}]
                [
                  nav [ ]
                  [ div [ className "nav nav-tabs", _id "nav-tab", role "tablist"]
                    [ a [ className "nav-item nav-link active"
                        , _id "nav-home-tab"
                        , _data {toggle : "tab"},href "#nav-home"
                        , role "tab"
                        , aria {controls : "nav-home"}
                        , aria {selected:true}
                        ] [ text "STOPLIST"]
                    , a [ className "nav-item nav-link"
                        , _id "nav-profile-tab"
                        , _data {toggle : "tab"}
                        , href "#nav-profile"
                        , role "tab"
                        , aria {controls : "nav-profile"}
                        , aria {selected:true}
                        ] [ text "MAINLIST"]
                    , a [ className "nav-item nav-link"
                        , _id "nav-contact-tab"
                        , _data {toggle : "tab"}
                        , href "#nav-contact"
                        , role "tab"
                        , aria {controls : "nav-contact"}
                        , aria {selected:true}
                        ] [ text "MAPLIST"]

                    ]
                  ]
                , div [className "tab-content" , _id "nav-tabContent"]
                  [
                    div [ className "tab-pane fade show active"
                        , role "tabpanel"
                        , aria {labelledby : "nav-home-tab"}
                        , _id "nav-home"
                        ]
                    [
                      h6 [] [text "Add a free term to STOPLIST"]
                    ,  div [className "form-group"]
                       [ input [ className "form-control"
                               , _id "id_password"
                               , name "password"
                               , placeholder "Any text"
                               , _type "value"
                               , value state.inputValue,onInput \e -> dispatch (SetInput (unsafeEventValue e))
                               ]
                       , div [className "clearfix"] []
                       ]
                    , button [ className "btn btn-primary"
                             , _type "button"
                             ] [text "Create and Add"]
                    ]

                  , div [ className "tab-pane fade show"
                        , role "tabpanel"
                        , aria {labelledby : "nav-profile-tab"}
                        , _id "nav-profile"
                        ]
                    [ ]
                  , div [ className "tab-pane fade show"
                        , role "tabpanel"
                        , aria {labelledby : "nav-contact-tab"}
                        , _id "nav-contact"
                        ]
                    [ ]
                  ]
                ]
              ]
            , div [className "col-md-8"]
              [ h4 [] [text' document.title]
              , ul [className "list-group"]
                [ li' [ span [] [text' document.source]
                      , badge "source"
                      ]
                
                -- TODO add href to /author/ if author present in
                , li' [ span [] [text' document.authors]
                      , badge "authors"
                      ]
                
                , li' [ span [] [text' document.publication_date]
                      , badge "date"
                      ]
                ]
              , badge "abstract"
              , p [] [text' document.abstract]
              
              , div [className "jumbotron"]
                [ p [] [text "Empty Full Text"]
                ]
              ]
            ]
          ]
      ]
        where
          li' = li [className "list-group-item justify-content-between"]
          text' x = text $ maybe "Nothing" identity x
          badge s = span [className "badge badge-default badge-pill"] [text s]

          NodePoly {hyperdata : Document document} = 
            maybe defaultNodeDocument identity state.document

aryPS :: Array String
aryPS = ["Map", "Main", "Stop"]

aryPS1 :: Array String
aryPS1 = ["Nothing Selected","STOPLIST", "MAINLIST", "MAPLIST"]


optps :: String -> ReactElement
optps val = option [ value  val ] [text  val]

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
