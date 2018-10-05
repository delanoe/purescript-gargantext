module Gargantext.Pages.Corpus.Annuaire where

import Prelude

import Data.Array (concat)
import Data.Traversable (foldl)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism, (?~))
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (liftEffect)
import React (ReactElement)
import React.DOM (div, h1, h3, hr, i, p, text, thead, tbody, input, br', b, b', tr, th, table, td, a)
import React.DOM.Props (_type, className, href, onChange, onClick, scope, selected, value, style)
import Thermite (Render, Spec
                , simpleSpec, defaultPerformAction
                , PerformAction, modifyState)
import Effect.Console (log)
import Effect.Aff (Aff)

import Gargantext.Config      (toUrl, NodeType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Pages.Corpus.User.Users.Types.Types (User(..), HyperData(..))
import Gargantext.Utils.DecodeMaybe ((.?|))
import Data.Argonaut (class DecodeJson, decodeJson, (.?))

------------------------------------------------------------------------------
type State = { info  :: Maybe AnnuaireInfo
             , stable :: Maybe AnnuaireTable
             }

type Offset = Int
type Limit  = Int
type PageSize = Int

data Action = Load Int
            | ChangePageSize PageSize -- TODO
            | ChangePage     Int      -- TODO


type AnnuaireTable' = Table IndividuView

newtype Table a
  = Table
    { rows         :: Array { row :: a }
    , totalPages   :: Int
    , currentPage  :: Int
    , pageSize     :: PageSize
    , totalRecords :: Int
    , title        :: String
    }

newtype IndividuView
  = CorpusView
    { id      :: Int
    , name    :: String
    , role    :: String
    , company :: String
    }

------------------------------------------------------------------------------
initialState :: State
initialState = { info : Nothing, stable : Nothing }

defaultAnnuaireTable :: AnnuaireTable
defaultAnnuaireTable = AnnuaireTable { annuaireTable : [Nothing] }

defaultAnnuaireInfo :: AnnuaireInfo
defaultAnnuaireInfo = AnnuaireInfo { id : 0
                                   , typename : 0
                                   , userId   : 0
                                   , parentId : 0
                                   , name     : ""
                                   , date     : ""
                                   , hyperdata : ""
                                   }
------------------------------------------------------------------------------
toRows :: AnnuaireTable -> Array (Maybe User)
toRows (AnnuaireTable a) = a.annuaireTable

layoutAnnuaire :: Spec State {} Action
layoutAnnuaire = simpleSpec performAction render

render :: Render State {} Action
render dispatch _ state _ = [ div [className "row"]
                     [ div [className "col-md-3"] [ h3 [] [text info.name] ]
                            , div [className "col-md-9"] [ hr [style {height : "2px",backgroundColor : "black"}] ]
                            ]
                          , div [className "row"] [ div [className "jumbotron1", style {padding : "12px 0px 20px 12px"}]
                                [ div [ className "col-md-8 content"]
                                      [ p [] [ i [className "fa fa-globe"] []
                                      , text info.name
                                             ]
                                      ]
                                , div [ className "col-md-4 content"]
                                      [ p [] [ i [className "fa fa-calendar"] []
                                      , text ("Last update: " <> info.date)
                                             ]
                                      ]
                                ]
                            ]
                            
                            ,             p [] []
            , div [] [ text "    Filter ", input []]
            , br'
            , div [className "row"]
              [  div [className "col-md-1"] [b [] [text "title"]]
              --, div [className "col-md-2"] [sizeDD d.pageSize dispatch]
              --, div [className "col-md-3"] [textDescription d.currentPage d.pageSize d.totalRecords]
              --, div [className "col-md-3"] [pagination dispatch d.totalPages d.currentPage]
                     ]
            , table [ className "table"]
              [thead  [ className "thead-dark"]
                         [tr [] [ th [scope "col"] [ b' [text ""]    ]
                                , th [scope "col"] [ b' [text "Name"]   ]
                                , th [scope "col"] [ b' [text "Role"]  ]
                                , th [scope "col"] [ b' [text "Service"]  ]
                                , th [scope "col"] [ b' [text "Company"]  ]
                                ]
                         ]
                         , tbody [] $ map showRow individuals
              ]
                   ]
                        where
                          (AnnuaireInfo  info)   = maybe defaultAnnuaireInfo  identity state.info
                          (AnnuaireTable stable) = maybe defaultAnnuaireTable identity state.stable
                          individuals = maybe (toRows defaultAnnuaireTable) toRows state.stable


showRow :: Maybe User -> ReactElement
showRow Nothing = tr [][]
showRow (Just (User { id : id, hyperdata : (HyperData user) })) =
  tr []
  [ td [] [ a [ href (toUrl Back NodeUser id) ] [ text $ maybe' user.nom <> " " <> maybe' user.prenom ] ]
  , td [] [text $ maybe' user.fonction]
  , td [] [text $ maybe' user.service]
  , td [] [text $ maybe' user.groupe]
  ]
    where
      maybe' = maybe "" identity


------------------------------------------------------------------------------
newtype AnnuaireInfo = AnnuaireInfo { id        :: Int
                                    , typename  :: Int
                                    , userId    :: Int
                                    , parentId  :: Int
                                    , name      :: String
                                    , date      :: String
                                    , hyperdata :: String
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


newtype AnnuaireTable  = AnnuaireTable  { annuaireTable :: Array (Maybe User)}
instance decodeAnnuaireTable :: DecodeJson AnnuaireTable where
  decodeJson json = do
    rows <- decodeJson json
    pure $ AnnuaireTable { annuaireTable : rows}

------------------------------------------------------------------------
performAction :: PerformAction State {} Action
performAction (Load aId) _ _ = do
  eitherTable <- lift $ getTable aId
  liftEffect $ log "Feching Table"
  _ <- case eitherTable of
            (Right table') -> void $ modifyState $ _table ?~ table'
            (Left       err)  -> do
              liftEffect $ log err

  eitherInfo <- lift $ getInfo aId
  _ <- case eitherInfo of
            (Right info') -> void $ modifyState $ _info ?~ info'
            (Left       err)  -> do
              liftEffect $ log err

  liftEffect <<< log $ "Fetching annuaire page..."
performAction _ _ _ = pure unit
------------------------------------------------------------------------
getTable :: Int -> Aff (Either String AnnuaireTable)
getTable id = get $ toUrl Back Children id

getInfo :: Int -> Aff (Either String AnnuaireInfo)
getInfo id = get $ toUrl Back Node id
------------------------------------------------------------------------------
_table :: Lens' State (Maybe AnnuaireTable)
_table = lens (\s -> s.stable) (\s ss -> s{stable = ss})

_info :: Lens' State (Maybe AnnuaireInfo)
_info = lens (\s -> s.info) (\s ss -> s{info = ss})
------------------------------------------------------------------------------
