module Gargantext.Pages.Layout where

import Prelude hiding (div)
-- import Gargantext.Components.Login as LN
import Gargantext.Pages.Layout.Actions (Action(..))
import Gargantext.Pages.Layout.Specs.AddCorpus as AC
-- import Gargantext.Pages.Corpus.Tabs as TV

import Gargantext.Pages.Corpus.Document       as Document
import Gargantext.Pages.Corpus.Graph          as GE
-- import Gargantext.Pages.Corpus.Tabs.Terms.NgramsTable as NG

import Gargantext.Pages.Annuaire.User.Contacts as C
-- import Gargantext.Pages.Home as L
-- import Gargantext.Pages.Layout.Specs.Search as S
import Gargantext.Router (Routes(..))

dispatchAction :: forall ignored m.
                  Monad m =>
                  (Action -> m Unit) -> ignored -> Routes -> m Unit

dispatchAction dispatcher _ Home = do
  dispatcher Initialize
  dispatcher $ SetRoute Home
  -- dispatcher $ LandingA TODO

dispatchAction dispatcher _ Login = do
  dispatcher Initialize
  dispatcher $ SetRoute Login
  -- dispatcher $ LoginA TODO

dispatchAction dispatcher _ AddCorpus = do
  dispatcher $ SetRoute   AddCorpus
  dispatcher $ AddCorpusA AC.LoadDatabaseDetails

dispatchAction dispatcher _ (Corpus n) = do
  dispatcher $ SetRoute $ Corpus n

dispatchAction dispatcher _ SearchView = do
  dispatcher $ SetRoute SearchView
  -- dispatcher $ SearchA TODO

dispatchAction dispatcher _ (UserPage id) = do
  dispatcher $ SetRoute $ UserPage id
  -- dispatcher $ UserPageA TODO
  dispatcher $ UserPageA $ C.FetchContact id

dispatchAction dispatcher _ (Annuaire id) = do
  dispatcher $ SetRoute $ Annuaire id

dispatchAction dispatcher _ (Folder id) = do
  dispatcher $ SetRoute $ Folder id

dispatchAction dispatcher _ (Document n) = do
  dispatcher $ SetRoute $ Document n
  dispatcher $ DocumentViewA $ Document.Load n

dispatchAction dispatcher _ (PGraphExplorer nid) = do
  dispatcher $ SetRoute $ PGraphExplorer nid
  dispatcher $ GraphExplorerA $ GE.LoadGraph nid
  --dispatcher $ GraphExplorerA $ GE.LoadGraph "imtNew.json"

dispatchAction dispatcher _ NGramsTable = do
  dispatcher $ SetRoute NGramsTable
  -- dispatcher $ NgramsA TODO

dispatchAction dispatcher _ Dashboard = do
  dispatcher $ SetRoute Dashboard
