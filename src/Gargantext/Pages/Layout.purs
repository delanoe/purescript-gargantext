module Gargantext.Pages.Layout where

import Prelude hiding (div)
-- import Gargantext.Components.Login as LN
import Gargantext.Pages.Layout.Actions (Action(..))
import Gargantext.Pages.Layout.Specs.AddCorpus as AC
-- import Gargantext.Pages.Corpus.Doc.Facets as TV
-- import Gargantext.Pages.Corpus.Doc.Annotation as D

import Gargantext.Pages.Corpus.Doc.Facets.Documents         as DV
import Gargantext.Pages.Corpus.Doc.Facets.Graph             as GE
-- import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable as NG

import Gargantext.Pages.Corpus.User.Users as U
import Gargantext.Pages.Corpus.Annuaire   as Annuaire
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

dispatchAction dispatcher _ DocView = do
  dispatcher $ SetRoute   DocView
  dispatcher $ DocViewA $ DV.LoadData

dispatchAction dispatcher _ SearchView = do
  dispatcher $ SetRoute SearchView
  -- dispatcher $ SearchA TODO

dispatchAction dispatcher _ (UserPage id) = do
  dispatcher $ SetRoute $ UserPage id
  -- dispatcher $ UserPageA TODO
  dispatcher $ UserPageA $ U.FetchUser id

dispatchAction dispatcher _ (Annuaire id) = do
  dispatcher $ SetRoute       $ Annuaire id
  dispatcher $ AnnuaireAction $ Annuaire.Load id

dispatchAction dispatcher _ (Folder id) = do
  dispatcher $ SetRoute $ Folder id

dispatchAction dispatcher _ (DocAnnotation i) = do
  dispatcher $ SetRoute $ DocAnnotation i
  -- dispatcher $ DocAnnotationViewA TODO

dispatchAction dispatcher _ Tabview = do
  dispatcher $ SetRoute Tabview
  -- dispatcher $ TabViewA TODO

dispatchAction dispatcher _ CorpusAnalysis = do
  dispatcher $ SetRoute CorpusAnalysis
  -- dispatcher $ CorpusAnalysisA TODO

dispatchAction dispatcher _ PGraphExplorer = do
  dispatcher $ SetRoute PGraphExplorer
  dispatcher $ GraphExplorerA $ GE.LoadGraph "imtNew.json"

dispatchAction dispatcher _ NGramsTable = do
  dispatcher $ SetRoute NGramsTable
  -- dispatcher $ NgramsA TODO

dispatchAction dispatcher _ Dashboard = do
  dispatcher $ SetRoute Dashboard
