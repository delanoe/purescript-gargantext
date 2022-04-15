module Gargantext.Components.Bootstrap.Ripple
  ( ripple
  ) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..), Variant(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props = ( | Options )

type Options =
  ( status      :: ComponentStatus
  , className   :: String
  , variant     :: Variant
  )

options :: Record Options
options =
  { variant     : Light
  , status      : Enabled
  , className   : ""
  }


-- | Component for a Ripple Effect on DOMElement click (without JavaScript)
-- |
-- | ```
-- | B.ripple
-- | {}
-- | [
-- |   myContent {} []
-- | ]
-- | ```
ripple :: forall r. R2.OptComponent Options Props r
ripple = R2.optComponent component options

componentName :: String
componentName = "b-ripple"

component :: R.Component Props
component = R.hooksComponent componentName cpt where
  cpt props@{ variant
            , status
            } children = do
    -- Computed
    let
      className = intercalate " "
        -- provided custom className
        [ props.className
        -- BEM classNames
        , componentName
        , componentName <> "--" <> show status
        , componentName <> "--" <> show variant
        ]
    -- Render
    pure $

      R.fragment $
      [
        H.div
        { className }
        []
      ]
        <> children
