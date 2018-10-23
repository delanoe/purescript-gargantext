module Gargantext.Pages.Annuaire where

import Data.Argonaut (class DecodeJson, decodeJson, (.?), (.??))
import Data.Lens (Prism', prism)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import React as React
import React (ReactClass, ReactElement)
import React.DOM (a, br', div, input, p, text)
import React.DOM.Props (href)
import Effect.Aff (Aff)
import Thermite ( Render, Spec
                , createClass, simpleSpec, defaultPerformAction
                )

------------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Loader as Loader
import Gargantext.Components.Loader (createLoaderClass)
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Table as Table
import Gargantext.Config      (toUrl, NodeType(..), TabType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Pages.Annuaire.User.Contacts.Types (Contact(..), HyperData(..))
------------------------------------------------------------------------------

type Props = {path :: Int, loaded :: Maybe AnnuaireInfo }

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
          , component: createClass "LoadedAnnuaire" loadedAnnuaireSpec {}
          } ]

loadedAnnuaireSpec :: Spec {} Props Void
loadedAnnuaireSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {loaded: Nothing} _ _ = []
    render _ {path, loaded: Just (AnnuaireInfo {name, date})} _ _ =
      Table.renderTableHeaderLayout
        { title: name
        , desc: name
        , query: ""
        , date: "Last update: " <> date
        , user: ""
        } <>
      [ p [] []
      , div [] [ text "    Filter ", input []]
      , br'
      , Table.tableElt
          { loadRows
          , title: "title" -- TODO
          , colNames:
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
        annuaireId = path
        loadRows {offset, limit} = do -- TODO use offset and limit
          (AnnuaireTable {annuaireTable: rows}) <- getTable annuaireId
          pure $ (\c -> {row: renderContactCells c, delete: false}) <$> rows

renderContactCells :: Contact -> Array ReactElement
renderContactCells (Contact { id, hyperdata : HyperData contact }) =
  [ a [ href (toUrl Front NodeUser id) ] [ text $ maybe' contact.nom <> " " <> maybe' contact.prenom ]
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
getTable :: Int -> Aff AnnuaireTable
getTable id = get $ toUrl Back (Tab TabDocs 0 10) id

getAnnuaireInfo :: Int -> Aff AnnuaireInfo
getAnnuaireInfo id = get $ toUrl Back Node id
------------------------------------------------------------------------------

annuaireLoaderClass :: ReactClass (Loader.Props Int AnnuaireInfo)
annuaireLoaderClass = createLoaderClass "AnnuaireLoader" getAnnuaireInfo

annuaireLoader :: Loader.Props Int AnnuaireInfo -> ReactElement
annuaireLoader = React.createLeafElement annuaireLoaderClass
