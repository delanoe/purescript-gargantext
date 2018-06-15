module Users.Info
       (layoutUser
       )
       where

import Prelude hiding (div)
import Users.Types.Lens
import Users.Types.Types

import Brevets as B
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import DocView as DV
import Network.HTTP.Affjax (AJAX)
import Projects as PS
import React.DOM (a, div, h3, h5, h6, i, img, li, nav, small, span, table, tbody, td, text, th, thead, tr, ul)
import React.DOM.Props (_data, _id, aria, className, href, role, scope, src)
import Tab (tabs)
import Thermite (PerformAction, Render, Spec, focus, modifyState, simpleSpec)

