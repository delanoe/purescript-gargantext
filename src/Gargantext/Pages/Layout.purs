module Gargantext.Pages.Layout where

import Prelude hiding (div)
import Gargantext.Pages.Layout.Actions (Action(..))

import Gargantext.Router (Routes(..))

dispatchAction :: forall ignored m.
                  Monad m =>
                  (Action -> m Unit) -> ignored -> Routes -> m Unit

dispatchAction dispatcher _ Home = do
  dispatcher $ SetRoute Home
  -- dispatcher $ LandingA TODO

dispatchAction dispatcher _ Login = do
  dispatcher $ SetRoute Login
  -- dispatcher $ LoginA TODO

dispatchAction dispatcher _ (Corpus n) = do
  dispatcher $ SetRoute $ Corpus n

dispatchAction dispatcher _ (UserPage id) = do
  dispatcher $ SetRoute $ UserPage id

dispatchAction dispatcher _ (ContactPage id) = do
  dispatcher $ SetRoute $ ContactPage id

dispatchAction dispatcher _ (Annuaire id) = do
  dispatcher $ SetRoute $ Annuaire id

dispatchAction dispatcher _ (Folder id) = do
  dispatcher $ SetRoute $ Folder id

dispatchAction dispatcher _ (CorpusDocument c i n) = do
  dispatcher $ SetRoute $ CorpusDocument c i n

dispatchAction dispatcher _ (Document i n) = do
  dispatcher $ SetRoute $ Document i n

dispatchAction dispatcher _ (PGraphExplorer nid) = do
  dispatcher $ SetRoute $ PGraphExplorer nid
  -- dispatcher $ GraphExplorerA $ GE.LoadGraph nid
  --dispatcher $ GraphExplorerA $ GE.LoadGraph "imtNew.json"

dispatchAction dispatcher _ (Texts nid) = do
  dispatcher $ SetRoute $ Texts nid

dispatchAction dispatcher _ (Lists nid) = do
  dispatcher $ SetRoute $ Lists nid

dispatchAction dispatcher _ Dashboard = do
  dispatcher $ SetRoute Dashboard
