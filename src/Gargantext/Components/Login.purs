-- The Login component is a modal which allows the user to:
-- * See the currently logged in sessions
-- * Select a backend and log into it
module Gargantext.Components.Login
  ( Props
  , activeConnections
  , backendLabel
  , chooser
  , chooserCpt
  , here
  , login
  , loginContainer
  , loginContainerCpt
  , renderBackend
  )
  where

import Gargantext.Prelude

import Data.Array (head)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DST
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..), Elevation(..), ModalSizing(..), Position(..), TooltipPosition(..), Variant(..))
import Gargantext.Components.Login.PasswordForm as PasswordForm
import Gargantext.Components.Login.LoginForm as LoginForm
import Gargantext.Components.Login.Types (FormType(..))
import Gargantext.Components.NgramsTable.Loader as NTL
import Gargantext.Ends (Backend(..))
import Gargantext.Hooks.Loader as GHL
import Gargantext.Sessions (Session(..), Sessions, Action(Logout), unSessions)
import Gargantext.Sessions as Sessions
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Login"

-- TODO
-- enable anonymous login for first map
-- and ask for login (modal) or account creation after 15 mn when user
-- if not logged user can not save his work

type Props =
  ( backend  :: T.Box (Maybe Backend)
  , backends :: Array Backend
  , sessions :: T.Box Sessions
  , visible  :: T.Box Boolean
  )

login :: R2.Leaf Props
login = R2.leaf loginCpt
loginCpt :: R.Component Props
loginCpt = here.component "login" cpt where
  cpt props@{ visible } _ = do
    -- Render
    pure $

      B.baseModal
      { isVisibleBox: visible
      , title: Just "GarganText Ecosystem Explorer"
      , size: ExtraLargeModalSize
      , modalClassName: "login-modal"
      }
      [
        loginContainer
        props
      ]

-- | @XXX React re-rendering issue with `React.Portal`
-- | @link https://github.com/facebook/react/issues/12247
loginContainer :: R2.Leaf Props
loginContainer = R2.leaf loginContainerCpt

loginContainerCpt :: R.Component Props
loginContainerCpt = here.component "container" cpt where
  cpt props@{ sessions, visible } _ = do
    -- | States
    -- |
    mBackend <- R2.useLive' props.backend
    formType <- T.useBox Manager
    formType' <- T.useLive T.unequal formType

    -- | Render
    -- |
    pure $

      B.wad
      []
      [
        case mBackend of
          Nothing      ->
            chooser $
            { formType
            } `Record.merge` props

          Just backend -> case formType' of
            Login ->
              LoginForm.component
              { backend
              , formType
              , sessions
              , visible
              }
            ForgotPassword ->
              PasswordForm.component
              { backend
              , formType
              }
            Manager ->
              chooser $
              { formType
              } `Record.merge` props

      ]

type ChooserProps =
  ( formType :: T.Box FormType
  | Props
  )

chooser :: R2.Leaf ChooserProps
chooser = R2.leaf chooserCpt
chooserCpt :: R.Component ChooserProps
chooserCpt = here.component "chooser" cpt where
  cpt { backend
      , backends
      , sessions
      , formType
      } _ = do
    -- | States
    -- |
    sessions' <- T.useLive T.unequal sessions

    -- | Render
    -- |
    pure $

      B.wad
      [] $
        activeConnections sessions sessions'
      <>
      [
        H.h6
        {}
        [ H.text "Existing places" ]
      ,
        B.tooltipContainer
        { position: TooltipPosition Top
        , variant: Info
        , tooltipSlot:
            B.span_ "Available soon"
        , defaultSlot:
            B.formInput
            { status: Idled
            , placeholder: "Search for your institute"
            , value: (mempty :: String)
            , callback: const R.nothing
            , className: "mb-1"
            }
        }
      ,
        H.table
        { className : "table" }
        [
          H.thead
          { className: "thead-dark" }
          [
            H.tr
            {} $
            [ "", "GarganText places", "Fonction", "Garg protocol url" ] <#>
              (
                \label -> H.th {} [ H.text label ]
              )
          ]
        ,
          H.tbody
          {} $
          backends <#>

            renderBackend <<<
            { backendBox: backend
            , formTypeBox: formType
            , backend: _
            }
        ]
      ]

-- Shown in the chooser
activeConnections :: T.Box Sessions -> Sessions -> Array R.Element
activeConnections _        sessions' | Sessions.null sessions'  = mempty
activeConnections sessions sessions'                            =
  [
    H.table
    { className: intercalate " "
        [ "login-modal__active-places"
        , "table"
        , "mb-4"
        ]
    }
    [
      H.thead
      { className: "thead-dark" }
      [
        H.tr
        {} $
        [ "", "Active connections", "Fonction", "Clear data/Logout"] <#>
        (
          B.th_
        )
      ]
    ,
      H.tbody
      {} $
      unSessions sessions' <#>

        renderSession <<<
        { sessions
        , session: _
        }
    ]
  ]


type RenderSessionProps =
  ( sessions :: T.Box Sessions
  , session  :: Session
  )

renderSession :: R2.Leaf RenderSessionProps
renderSession = R2.leaf renderSessionCpt
renderSessionCpt :: R.Component RenderSessionProps
renderSessionCpt = here.component "renderSession" cpt where
  cpt { sessions
      , session
      } _ = do
    -- | States
    -- |
    onPending' /\ onPending <- R2.useBox' false

    -- | Computed
    -- |
    let

      Session { backend } = session

      Backend { backendType } = backend

    -- | Behaviors
    -- |
    let

      onSignOutClick _ = void $ Sessions.change (Logout session) sessions

      onClearCallback _ = launchAff_ do
        liftEffect $ T.write_ true onPending
        GHL.clearCache unit
        NTL.clearCache unit
        -- add a human minimal cognitive delay telling something has being executed
        delay $ Milliseconds 600.0
        liftEffect
          $   T.write_ false onPending
          *>  here.info "cache cleared"

    -- | Render
    -- |
    pure $

      H.tr
      {}
      [
        H.td
        {}
        [
          B.icon
          { name: "user"
          }
        ]
      ,
        B.td_ $
        show session
      ,
        B.td_ $
        backendType
      ,
        H.td
        { className: "login-modal__active-places__actions" }
        [
          B.tooltipContainer
          { position: TooltipPosition Top
          , delayShow: 600
          , tooltipSlot:
              B.span_ "Clear cache"
          , defaultSlot:
              B.iconButton
              { name: "eraser"
              , callback: onClearCallback
              , elevation: Level2
              , status: onPending' ? Disabled $ Enabled
              }
          }
        ,
          B.tooltipContainer
          { position: TooltipPosition Top
          , delayShow: 600
          , tooltipSlot:
              B.span_ "Log out"
          , defaultSlot:
              B.iconButton
              { name: "sign-out"
              , callback: onSignOutClick
              , elevation: Level2
              }
          }
        ]
      ]

type RenderBackendProps =
  ( backendBox   :: T.Box (Maybe Backend)
  , formTypeBox  :: T.Box FormType
  , backend      :: Backend
  )

renderBackend :: R2.Leaf RenderBackendProps
renderBackend = R2.leaf renderBackendCpt
renderBackendCpt :: R.Component RenderBackendProps
renderBackendCpt = here.component "renderBackend" cpt where
  cpt { backendBox
      , backend: backend@(Backend { name, baseUrl, backendType })
      , formTypeBox
      } _ = do
    -- | Behaviors
    -- |
    let
      click :: Unit -> Effect Unit
      click _ = do
        T.write_ (Just backend) backendBox
        T.write_ (Login) formTypeBox

    -- | Render
    -- |
    pure $

      H.tr {}
      [
        H.td
        {}
        [
          B.icon
          { name: "database"
          }
        ]
      ,
        H.td
        {}
        [
          H.a
          { on: { click }
          , className: "text-primary"
          }
          [ H.text (backendLabel name) ]
        ]
      ,
        H.td
        {}
        [
          B.span_
          backendType
        ]
      ,
        H.td
        {}
        [
          B.wad
          [ "font-family-monospace", "font-size-95" ]
          [
            H.text $ DST.replace (DST.Pattern "http") (DST.Replacement "garg") $ baseUrl
          ]
        ]
      ]



backendLabel :: String -> String
backendLabel =
  DST.toUpper <<< fromMaybe "" <<< head <<< DST.split (DST.Pattern ".")
