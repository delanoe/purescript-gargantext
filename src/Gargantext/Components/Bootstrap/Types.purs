module Gargantext.Components.Bootstrap.Types
  ( ComponentStatus(..)
  ) where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Extra (kebabCase)

-- | Component status based on UI/UX overall expression
-- |
-- |    * `Enabled`: default UI/UX behavior
-- |    * `Disabled`: main action of the component has been deactivated, and
-- |      a UI feedback is showed to the user (eg. a disabled button is now
-- |      unclikable, fade color, and disabled CTA feature)
-- |    * `Deffered`: main action of the component has been deactivated, but
-- |      contrary to a disabled state, the altered UX/UI bears characteristics
-- |      of a short-lived state (eg. a button turns to `Deffered`, is now
-- |      unclickable but presents a less inoperative style, for example a
-- |      spinner is now attached in place of the CTA text)
-- |    * `Muted`: on surface the component seems functional, but main action
-- |      of the component has been deactivated, yet no UI nor UX feedback is
-- |      particularly showed to the user accordingly
-- |    * `Idled`: balance between a `Disabled` and `Muted` state, as if the
-- |      component has its main feature deactivated, but told with a less
-- |      strong UI/UX (eg. a input in a "read-only" mode: UI can be alter to
-- |      underline the lack of its main writing feature, but without telling
-- |      to the user that the input is per-se inoperative)
data ComponentStatus =
    Enabled
  | Disabled
  | Deferred
  | Idled
  | Muted

derive instance Generic ComponentStatus _
derive instance Eq ComponentStatus
instance Show ComponentStatus where
  show a = kebabCase $ genericShow a
