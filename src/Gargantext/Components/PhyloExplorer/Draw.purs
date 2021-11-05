module Gargantext.Components.PhyloExplorer.Draw where

import Gargantext.Prelude

import Data.Function.Uncurried (Fn7, runFn7)
import Effect (Effect)
import Gargantext.Components.PhyloExplorer.Types (AncestorLink, Branch, BranchLink, Group, Link, Period)

foreign import _drawPhylo :: Fn7
  (Array Branch)
  (Array Period)
  (Array Group)
  (Array Link)
  (Array AncestorLink)
  (Array BranchLink)
  (Array Number)
  (Effect Unit)

drawPhylo ::
     Array Branch
  -> Array Period
  -> Array Group
  -> Array Link
  -> Array AncestorLink
  -> Array BranchLink
  -> Array Number
  -> Effect Unit
drawPhylo = runFn7 _drawPhylo
