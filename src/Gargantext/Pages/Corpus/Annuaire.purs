module Gargantext.Pages.Corpus.Annuaire where

import Prelude

import Data.Array (concat)
import Data.Traversable (foldl)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism, (?~))
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (liftEffect)
import React.DOM (div, h1, p, text)
import Thermite (Render, Spec
                , simpleSpec, defaultPerformAction
                , PerformAction, modifyState)
import Effect.Console (log)
import Effect.Aff (Aff)
import Gargantext.Config      (toUrl, NodeType(..), End(..))
import Gargantext.Config.REST (get)

import Gargantext.Utils.DecodeMaybe ((.?|))
import Data.Argonaut (class DecodeJson, decodeJson, (.?))


defaultAnnuaire :: AnnuaireTable
defaultAnnuaire = AnnuaireTable {annuaireTable : [IndividuRow {id: 0, name :"nothing, error"}] }

toRows :: AnnuaireTable -> Array IndividuRow
toRows (AnnuaireTable {annuaireTable : default}) = default


render :: Render State {} Action
render dispatch _ state _ = [ h1 [] [text "Annuaire 2"]
                            , p  [] [text $ foldl (<>) " " 
                                          $ map (\(IndividuRow {id:_,name:n}) -> n) 
                                          $ maybe (toRows defaultAnnuaire) toRows state.annuaire]
                            ]

layoutAnnuaire :: Spec State {} Action
layoutAnnuaire = simpleSpec performAction render

------------------------------------------------------------------------------
newtype IndividuRow = IndividuRow { id :: Int
                            , name :: String
                          }

newtype AnnuaireTable = AnnuaireTable { annuaireTable :: Array IndividuRow}

instance decodeIndividuRow :: DecodeJson IndividuRow where
  decodeJson json = do
    obj <- decodeJson json
    id  <- obj .? "id"
    name <- obj .? "name"
    pure $ IndividuRow { id : id, name : name}

instance decodeAnnuaireTable :: DecodeJson AnnuaireTable where
  decodeJson json = do
    obj <- decodeJson json
    annuaire'  <- obj .? "annuaire"
    pure $ AnnuaireTable { annuaireTable : annuaire'}

------------------------------------------------------------------------------
getAnnuaire :: Int -> Aff (Either String AnnuaireTable)
getAnnuaire id = get $ toUrl Back Children id

performAction :: PerformAction State {} Action
performAction (Load aId) _ _ = do
  eitherAnnuaire <- lift $ getAnnuaire aId
  _ <- case eitherAnnuaire of
            (Right annuaire') -> void $ modifyState $ _annuaire ?~ annuaire'
            (Left       err)  -> do
              liftEffect $ log err
  liftEffect <<< log $ "Fetching annuaire..."
performAction _ _ _ = pure unit
------------------------------------------------------------------------------
------------------------------------------------------------------------------
type State = { annuaire :: Maybe AnnuaireTable}

initialState :: State
initialState = { annuaire : Nothing }

data Action
  = Load Int
--  | ChangePageSize PageSizes
--  | ChangePage Int
------------------------------------------------------------------------------
_annuaire :: Lens' State (Maybe AnnuaireTable)
_annuaire = lens (\s -> s.annuaire) (\s ss -> s{annuaire = ss})
------------------------------------------------------------------------------
------------------------------------------------------------------------------
