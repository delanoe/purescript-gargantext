module Gargantext.Components.Nodes.Annuaire.User.Contact
  ( module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
  , contactLayout
  ) where

import Gargantext.Prelude

import Data.Lens (view)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Components.GraphQL.Endpoints (getAnnuaireContact)
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (ContactData', HyperdataContact(..))
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Types (NodeID)
import Gargantext.Utils (nbsp)
import Gargantext.Utils.Reactix as R2
import Gargnatext.Components.GraphQL.Contact (AnnuaireContact, _ac_firstName, _ac_lastName)
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Annuaire.User.Contact"

type Props =
  ( annuaireId  :: NodeID
  , nodeId      :: NodeID
  )

contactLayout :: R2.Leaf ( key :: String | Props )
contactLayout = R2.leaf contactLayoutCpt

contactLayoutCpt :: R.Component ( key :: String | Props )
contactLayoutCpt = here.component "layout" cpt where
  -- Helpers
  errorHandler = logRESTError here "[contactLayoutWithKey]"

  -- Component
  cpt { nodeId
      } _ = do

    session <- useSession

    -- _ /\ reload <- R2.useBox' T2.newReload

    useLoader
      { errorHandler
      , loader: getAnnuaireContact session
      , path: nodeId
      , render:
          \r -> contactForm
            { fdata: r }
      }

---------------------------------------------------------

-- @NOTE: #376 temporary pages and form content
--        YAGNI decision triggered by a change of API (REST to GQL)

type FormProps =
  ( fdata :: AnnuaireContact
  )

contactForm :: R2.Leaf FormProps
contactForm = R2.leaf contactFormCpt

contactFormCpt :: R.Component FormProps
contactFormCpt = here.component "form" cpt where
  cpt { fdata } _ = do

    -- Render
    pure $

      B.wad
      [ "d-flex" ]
      [
        -- Left column: contact card
        H.div
        { className: "card" }
        [
          H.div
          { className: "card-header"}
          [
            B.icon
            { name: "address-book-o" }
          ,
            B.wad
            [ "ml-3", "d-inline-block" ]
            [
              H.text $
                view _ac_firstName fdata <>
                nbsp 1                   <>
                view _ac_lastName fdata
            ]
          ]
        ,
          H.div
          { className: "card-body" }
          [
            H.div
            { className: "form-group" }
            [
              H.div
              { className: "form-group__label" }
              [
                B.label_ "Firstname"
              ]
            ,
              H.div
              { className: "form-group__field" }
              [
                B.formInput
                { status: Disabled
                , callback: const R.nothing
                , value: view _ac_firstName fdata
                }
              ]
            ]
          ,
            H.div
            { className: "form-group" }
            [
              H.div
              { className: "form-group__label" }
              [
                B.label_ "Lastname"
              ]
            ,
              H.div
              { className: "form-group__field" }
              [
                B.formInput
                { status: Disabled
                , callback: const R.nothing
                , value: view _ac_lastName fdata
                }
              ]
            ]
          ]
        ]
      ,
        B.wad
        [ "align-self-start", "ml-3" ]
        [
          B.caveat
          {}
          [
            B.icon
            { name: "rocket" }
          ,
            B.wad
            [ "d-inline-flex", "ml-2", "text-italic" ]
            [
              H.text " development in progress"
            ]
          ]
        ]
      ]
