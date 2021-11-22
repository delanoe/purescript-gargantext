module Gargantext.Components.Reload
  ( reloadContext
  , textsReloadContext ) where

import Data.Maybe (Maybe(..))
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Toestand as T

-- | Reload Context
-- |
-- | Use with `R.provideContext` as a (nested) context
-- |
-- | https://medium.com/@NickIannelli/nested-context-the-underrated-aspect-thats-probably-missing-from-your-react-app-16e73f7d1
reloadContext :: R.Context (Maybe (T.Box T2.Reload))
reloadContext = R.createContext Nothing


-----------------------------------------------------------------

-- @XXX: This custom context solves a wrong monolithic front design where
--       "DocsTable" component is used for many different use cases
--       Normally we would have use the classic "Gargantext.Components.Reload",
--       but we limit side-effects by using another context reference
--
--       See its use in "Gargantext.Components.Nodes.Texts"
textsReloadContext :: R.Context (Maybe (T.Box T2.Reload))
textsReloadContext = R.createContext Nothing
