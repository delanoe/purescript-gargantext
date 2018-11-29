module Gargantext.Pages.Annuaire where

import Data.Argonaut (class DecodeJson, decodeJson, (.?), (.??))
import Data.Lens (Prism', prism)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import React as React
import React (ReactClass, ReactElement, Children)
import React.DOM (a, br', div, input, p, text)
import React.DOM.Props (href)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Thermite ( Render, Spec
                , createClass, simpleSpec, defaultPerformAction
                )

------------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Table as T
import Gargantext.Config      (toUrl, Path(..), NodeType(..), TabType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Pages.Annuaire.User.Contacts.Types (Contact(..), HyperData(..))
------------------------------------------------------------------------------

type Props = Loader.InnerProps Int AnnuaireInfo ()

data Action
  = TabsA   Tab.Action

_tabsAction :: Prism' Action Tab.Action
_tabsAction = prism TabsA \ action ->
  case action of
    TabsA taction -> Right taction
    -- _-> Left action

newtype IndividuView
  = CorpusView
    { id      :: Int
    , name    :: String
    , role    :: String
    , company :: String
    }

------------------------------------------------------------------------------

-- unused
defaultAnnuaireTable :: AnnuaireTable
defaultAnnuaireTable = AnnuaireTable { annuaireTable : [] }

-- unused
defaultHyperdataAnnuaire :: HyperdataAnnuaire
defaultHyperdataAnnuaire = HyperdataAnnuaire { title: Nothing, desc: Nothing }

-- unused
defaultAnnuaireInfo :: AnnuaireInfo
defaultAnnuaireInfo = AnnuaireInfo { id : 0
                                   , typename : 0
                                   , userId   : 0
                                   , parentId : 0
                                   , name     : ""
                                   , date     : ""
                                   , hyperdata : defaultHyperdataAnnuaire
                                   }
------------------------------------------------------------------------------
layout :: Spec {} {annuaireId :: Int} Void
layout = simpleSpec defaultPerformAction render
  where
    render :: Render {} {annuaireId :: Int} Void
    render _ {annuaireId} _ _ =
      [ annuaireLoader
          { path: annuaireId
          , component: createClass "LoadedAnnuaire" loadedAnnuaireSpec (const {})
          } ]

loadedAnnuaireSpec :: Spec {} Props Void
loadedAnnuaireSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {path: nodeId, loaded: annuaireInfo@AnnuaireInfo {name, date}} _ _ =
      T.renderTableHeaderLayout
        { title: name
        , desc: name
        , query: ""
        , date: "Last update: " <> date
        , user: ""
        } <>
      [ p [] []
      , div [] [ text "    Filter ", input []]
      , br'
      , pageLoader
          { path: initialPageParams nodeId
          , annuaireInfo
          }
      ]

type PageParams = {nodeId :: Int, params :: T.Params}

initialPageParams :: Int -> PageParams
initialPageParams nodeId = {nodeId, params: T.initialParams}

type PageLoaderProps =
  { path :: PageParams
  , annuaireInfo :: AnnuaireInfo
  }

renderPage :: forall props path.
              Render (Loader.State {nodeId :: Int | path} AnnuaireTable)
                     {annuaireInfo :: AnnuaireInfo | props}
                     (Loader.Action PageParams)
renderPage _ _ {loaded: Nothing} _ = [] -- TODO loading spinner
renderPage dispatch {annuaireInfo}
                    { currentPath: {nodeId}
                    , loaded: Just (AnnuaireTable {annuaireTable: res})
                    } _ =
  [ T.tableElt
      { rows
      , setParams: \params -> liftEffect $ dispatch (Loader.SetPath {nodeId, params})
      , container: T.defaultContainer { title: "Annuaire" } -- TODO
      , colNames:
          T.ColumnName <$>
              [ ""
              , "Name"
              , "Role"
              , "Service"
              , "Company"
              ]
      , totalRecords: 47361 -- TODO
      }
  ]
  where
    rows = (\c -> {row: renderContactCells c, delete: false}) <$> res

pageLoaderClass :: ReactClass { path :: PageParams, annuaireInfo :: AnnuaireInfo, children :: Children }
pageLoaderClass = Loader.createLoaderClass' "AnnuairePageLoader" loadPage renderPage

pageLoader :: PageLoaderProps -> ReactElement
pageLoader props = React.createElement pageLoaderClass props []

renderContactCells :: Contact -> Array ReactElement
renderContactCells (Contact { id, hyperdata : HyperData contact }) =
  [ a [ href (toUrl Front NodeUser (Just id)) ] [ text $ maybe' contact.nom <> " " <> maybe' contact.prenom ]
  , text $ maybe' contact.fonction
  , text $ maybe' contact.service
  , text $ maybe' contact.groupe
  ]
  where
    maybe' = maybe "" identity

data HyperdataAnnuaire = HyperdataAnnuaire
  { title :: Maybe String
  , desc  :: Maybe String }

instance decodeHyperdataAnnuaire :: DecodeJson HyperdataAnnuaire where
  decodeJson json = do
    obj   <- decodeJson json
    title <- obj .?? "title"
    desc  <- obj .?? "desc"
    pure $ HyperdataAnnuaire { title, desc }

------------------------------------------------------------------------------
newtype AnnuaireInfo = AnnuaireInfo { id        :: Int
                                    , typename  :: Int
                                    , userId    :: Int
                                    , parentId  :: Int
                                    , name      :: String
                                    , date      :: String
                                    , hyperdata :: HyperdataAnnuaire
                                    }

instance decodeAnnuaireInfo :: DecodeJson AnnuaireInfo where
  decodeJson json = do
    obj <- decodeJson json
    id        <- obj .? "id"
    typename  <- obj .? "typename"
    userId    <- obj .? "userId"
    parentId  <- obj .? "parentId"
    name      <- obj .? "name"
    date      <- obj .? "date"
    hyperdata <- obj .? "hyperdata"
    pure $ AnnuaireInfo { id : id
                        , typename : typename
                        , userId   : userId
                        , parentId : parentId
                        , name     : name
                        , date     : date
                        , hyperdata: hyperdata
                        }


newtype AnnuaireTable = AnnuaireTable { annuaireTable :: Array Contact }

instance decodeAnnuaireTable :: DecodeJson AnnuaireTable where
  decodeJson json = do
    rows <- decodeJson json
    pure $ AnnuaireTable { annuaireTable : rows}
------------------------------------------------------------------------
loadPage :: PageParams -> Aff AnnuaireTable
loadPage {nodeId, params} = get $ toUrl Back (Tab TabDocs 0 10 Nothing) (Just nodeId)
 -- TODO Tab TabDocs is not the right API call
 -- TODO params, see loadPage in Documents

getAnnuaireInfo :: Int -> Aff AnnuaireInfo
getAnnuaireInfo id = get $ toUrl Back Node (Just id)
------------------------------------------------------------------------------

annuaireLoaderClass :: ReactClass (Loader.Props Int AnnuaireInfo)
annuaireLoaderClass = Loader.createLoaderClass "AnnuaireLoader" getAnnuaireInfo

annuaireLoader :: Loader.Props' Int AnnuaireInfo -> ReactElement
annuaireLoader props = React.createElement annuaireLoaderClass props []
