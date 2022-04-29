module Gargnatext.Components.GraphQL.Contact
  ( AnnuaireContact
  , annuaireContactQuery
  -- Lenses
  , _ac_firstName
  , _ac_lastName
  ) where

import Gargantext.Prelude

import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import GraphQL.Client.Args (Args, (=>>))
import GraphQL.Client.Variable (Var(..))

type AnnuaireContact
  = { ac_id           :: Int
    , ac_firstName    :: Maybe String
    , ac_lastName     :: Maybe String
    }

type AnnuaireContactQuery
  = { annuaire_contacts   :: Args
      { contact_id        :: Var "id" Int }
      { ac_id             :: Unit
      , ac_firstName      :: Unit
      , ac_lastName       :: Unit
      }
    }
annuaireContactQuery :: AnnuaireContactQuery
annuaireContactQuery
  = { annuaire_contacts:
      { contact_id: Var :: _ "id" Int } =>>
        { ac_id: unit
        , ac_firstName: unit
        , ac_lastName: unit
        }
    }

------------------------------------------------------------------------

_ac_firstName :: Lens' AnnuaireContact String
_ac_firstName = lens getter setter
  where
    getter ({ ac_firstName: val }) = fromMaybe "" val
    setter rec val = rec { ac_firstName = Just val }

_ac_lastName :: Lens' AnnuaireContact String
_ac_lastName = lens getter setter
  where
    getter ({ ac_lastName: val }) = fromMaybe "" val
    setter rec val = rec { ac_lastName = Just val }
