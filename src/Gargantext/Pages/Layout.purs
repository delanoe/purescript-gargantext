module Gargantext.Pages.Layout where

import Prelude hiding (div)
import Gargantext.Components.Login as LN
import Gargantext.Pages.Layout.Actions (Action(..))
import Gargantext.Pages.Layout.Specs.AddCorpus as AC
import Gargantext.Pages.Corpus.Doc.Annotation as D
import Gargantext.Pages.Corpus.Doc.Document as DV
import Gargantext.Pages.Corpus.Doc.Facets as TV
import Gargantext.Pages.Corpus.Doc.Facets.Graph as GE
import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable as NG
import Gargantext.Pages.Corpus.User.Users as U
import Gargantext.Pages.Home as L
import Gargantext.Pages.Layout.Specs.Search as S
import Gargantext.Router (Routes(..))

dispatchAction :: forall t115 t445 t447.
                  Bind t445 => Applicative t445  =>
                  (Action -> t445 t447) -> t115 -> Routes -> t445 Unit

dispatchAction dispatcher _ Home = do
  _ <- dispatcher Initialize
  _ <- dispatcher $ SetRoute Home
  _ <- dispatcher $ LandingA L.NoOp
  pure unit

dispatchAction dispatcher _ Login = do
  _ <- dispatcher Initialize
  _ <- dispatcher $ SetRoute Login
  _ <- dispatcher $ LoginA LN.NoOp
  pure unit

dispatchAction dispatcher _ AddCorpus = do
  _ <- dispatcher $ SetRoute AddCorpus
  _ <- dispatcher $ AddCorpusA AC.LoadDatabaseDetails
  pure unit

dispatchAction dispatcher _ DocView = do
  _ <- dispatcher $ SetRoute $ DocView
  _ <- dispatcher $ DocViewA $ DV.LoadData
  pure unit

dispatchAction dispatcher _ SearchView = do
  _ <- dispatcher $ SetRoute $ SearchView
  _ <- dispatcher $ SearchA  $ S.NoOp
  pure unit

dispatchAction dispatcher _ (UserPage id) = do
  _ <- dispatcher $ SetRoute  $ UserPage id
  _ <- dispatcher $ UserPageA $ U.NoOp
  _ <- dispatcher $ UserPageA $ U.FetchUser id
  pure unit

dispatchAction dispatcher _ (DocAnnotation i) = do
  _ <- dispatcher $ SetRoute  $ DocAnnotation i
  _ <- dispatcher $ DocAnnotationViewA $ D.NoOp
  pure unit

dispatchAction dispatcher _ Tabview = do
  _ <- dispatcher $ SetRoute  $ Tabview
  _ <- dispatcher $ TabViewA $ TV.NoOp
  pure unit

dispatchAction dispatcher _ CorpusAnalysis = do
  _ <- dispatcher $ SetRoute  $ CorpusAnalysis
  --_ <- dispatcher $ CorpusAnalysisA $ CA.NoOp
  pure unit

dispatchAction dispatcher _ PGraphExplorer = do
  _ <- dispatcher $ SetRoute  $ PGraphExplorer
  _ <- dispatcher $ GraphExplorerA $ GE.LoadGraph "imtNew.json"
  pure unit

dispatchAction dispatcher _ NGramsTable = do
  _ <- dispatcher $ SetRoute  $ NGramsTable
  _ <- dispatcher $ NgramsA $ NG.NoOp
  pure unit

dispatchAction dispatcher _ Dashboard = do
  _ <- dispatcher $ SetRoute $ Dashboard
  pure unit
