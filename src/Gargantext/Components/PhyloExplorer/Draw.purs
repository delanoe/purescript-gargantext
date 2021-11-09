module Gargantext.Components.PhyloExplorer.Draw
  ( drawPhylo
  , highlightSource
  , unhide
  , setGlobalDependencies, setGlobalD3Reference
  ) where

import Gargantext.Prelude

import DOM.Simple (Window)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn7, runEffectFn1, runEffectFn7)
import FFI.Simple (applyTo, (..), (.=), (.?))
import Gargantext.Components.PhyloExplorer.Types (AncestorLink, Branch, BranchLink, GlobalTerm(..), Group(..), Link, Period, PhyloDataSet(..))
import Graphics.D3.Base (D3)

foreign import _drawPhylo :: EffectFn7
  (Array Branch)
  (Array Period)
  (Array Group)
  (Array Link)
  (Array AncestorLink)
  (Array BranchLink)
  (Array Number)
  (Unit)

drawPhylo ::
     Array Branch
  -> Array Period
  -> Array Group
  -> Array Link
  -> Array AncestorLink
  -> Array BranchLink
  -> Array Number
  -> Effect Unit
drawPhylo = runEffectFn7 _drawPhylo

-----------------------------------------------------------

foreign import _highlightSource :: Effect Unit

highlightSource :: Effect Unit
highlightSource = _highlightSource

-----------------------------------------------------------

-- @WIP: still necessary? as we certainly would have only one mode?
foreign import _unhide :: EffectFn1 String Unit

unhide :: String -> Effect Unit
unhide = runEffectFn1 _unhide

-----------------------------------------------------------

setGlobalDependencies :: Window -> PhyloDataSet -> Effect Unit
setGlobalDependencies w (PhyloDataSet o)
  = do
    _ <- pure $ (w .= "freq") {}
    _ <- pure $ (w .= "nbBranches") o.nbBranches
    _ <- pure $ (w .= "nbDocs") o.nbDocs
    _ <- pure $ (w .= "nbFoundations") o.nbFoundations
    _ <- pure $ (w .= "nbGroups") o.nbGroups
    _ <- pure $ (w .= "nbPeriods") o.nbPeriods
    _ <- pure $ (w .= "nbTerms") o.nbTerms
    _ <- pure $ (w .= "sources") o.sources
    _ <- pure $ (w .= "terms") []
    _ <- pure $ (w .= "timeScale") o.timeScale
    _ <- pure $ (w .= "weighted") o.weighted

    (freq :: Array Int)         <- pure $ w .. "freq"
    (terms :: Array GlobalTerm) <- pure $ w .. "terms"

    for_ o.groups \(Group g) -> do

      let
        f = g.foundation
        l = g.label

      forWithIndex_ f \idx val ->
        let
          idx' = show idx
          val' = show val
        -- For each entries in group.foundation array,
        -- increment consequently the global window.keys array
        in case (freq .? val') of
          Nothing -> pure $ (freq .= val') 0
          Just v  -> pure $ (freq .= val') (v +1)
        -- For each entries in group.foundation array,
        -- if the global window.terms does not have it in property,
        -- append an item to the global window.terms
        *> case (terms .? val') of
          Just _  -> pure unit
          Nothing -> void <<< pure $ (terms .= val') $ GlobalTerm
            { label: l .. idx'
            , fdt  : val'
            }

    -- @XXX: FFI.Simple `(...)` throws error (JavaScript issue)
    --       need to decompose computation
    void do
      new <- pure $ applyTo (terms .. "flat") terms []
      pure $ (w .= "terms") new


setGlobalD3Reference :: Window -> D3 -> Effect Unit
setGlobalD3Reference window d3 = void $ pure $ (window .= "d3") d3
