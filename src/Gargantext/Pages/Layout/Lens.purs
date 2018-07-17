module Gargantext.Layout.Lens where


import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, prism)

import Gargantext.Components.Login          as LN
import Gargantext.Components.Tree           as Tree
import Gargantext.Pages.Corpus              as AC
import Gargantext.Pages.Corpus.Doc.Annotation as D
import Gargantext.Pages.Corpus.Doc.Body     as CA
import Gargantext.Pages.Corpus.Doc.Document as DV
import Gargantext.Pages.Corpus.Doc.Facets   as TV
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard as Dsh
import Gargantext.Pages.Corpus.Doc.Facets.Graph     as GE
import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable as NG
import Gargantext.Pages.Corpus.User.Users   as U
import Gargantext.Pages.Home                as L
import Gargantext.Pages.Layout.Actions        (Action(..))
import Gargantext.Pages.Layout.States
import Gargantext.Pages.Search              as S

---- Lens and Prism
_landingState :: Lens' AppState L.State
_landingState = lens (\s -> s.landingState) (\s ss -> s{landingState = ss})

_landingAction :: Prism' Action L.Action
_landingAction = prism LandingA \action ->
  case action of
    LandingA caction -> Right caction
    _-> Left action

_loginState :: Lens' AppState LN.State
_loginState = lens (\s -> s.loginState) (\s ss -> s{loginState = ss})

_loginAction :: Prism' Action LN.Action
_loginAction = prism LoginA \action ->
  case action of
    LoginA caction -> Right caction
    _-> Left action

_addCorpusState :: Lens' AppState AC.State
_addCorpusState = lens (\s -> s.addCorpusState) (\s ss -> s{addCorpusState = ss})

_addCorpusAction :: Prism' Action AC.Action
_addCorpusAction = prism AddCorpusA \action ->
  case action of
    AddCorpusA caction -> Right caction
    _-> Left action

_docViewState :: Lens' AppState DV.State
_docViewState = lens (\s -> s.docViewState) (\s ss -> s{docViewState = ss})

_docViewAction :: Prism' Action DV.Action
_docViewAction = prism DocViewA \action ->
  case action of
    DocViewA caction -> Right caction
    _-> Left action

_searchState :: Lens' AppState S.State
_searchState = lens (\s -> s.searchState) (\s ss -> s{searchState = ss})

_searchAction :: Prism' Action S.Action
_searchAction = prism SearchA \action ->
  case action of
    SearchA caction -> Right caction
    _-> Left action

_userPageState :: Lens' AppState U.State
_userPageState = lens (\s -> s.userPage) (\s ss -> s{userPage = ss})

_userPageAction :: Prism' Action U.Action
_userPageAction = prism UserPageA \action ->
  case action of
    UserPageA caction -> Right caction
    _-> Left action

_dashBoardAction :: Prism' Action Dsh.Action
_dashBoardAction = prism DashboardA \action ->
  case action of
    DashboardA caction -> Right caction
    _ -> Left action

_docAnnotationViewState :: Lens' AppState D.State
_docAnnotationViewState = lens (\s -> s.docAnnotationView) (\s ss -> s{docAnnotationView = ss})

_docAnnotationViewAction :: Prism' Action D.Action
_docAnnotationViewAction = prism DocAnnotationViewA \action ->
  case action of
    DocAnnotationViewA caction -> Right caction
    _-> Left action

_treeState :: Lens' AppState Tree.State
_treeState = lens (\s -> s.ntreeView) (\s ss -> s {ntreeView = ss})

_treeAction :: Prism' Action Tree.Action
_treeAction = prism TreeViewA \action ->
  case action of
    TreeViewA caction -> Right caction
    _-> Left action

_tabviewState :: Lens' AppState TV.State
_tabviewState = lens (\s -> s.tabview) (\s ss -> s {tabview = ss})

_tabviewAction :: Prism' Action TV.Action
_tabviewAction = prism TabViewA \action ->
  case action of
    TabViewA caction -> Right caction
    _-> Left action

_corpusState :: Lens' AppState CA.State
_corpusState = lens (\s -> s.corpusAnalysis) (\s ss -> s {corpusAnalysis = ss})

_corpusAction :: Prism' Action CA.Action
_corpusAction = prism CorpusAnalysisA \action ->
  case action of
    CorpusAnalysisA caction -> Right caction
    _-> Left action

_dashBoardSate :: Lens' AppState Dsh.State
_dashBoardSate = lens (\s -> s.dashboard) (\s ss -> s {dashboard = ss})

_graphExplorerState :: Lens' AppState GE.State
_graphExplorerState = lens (\s -> s.graphExplorer) (\s ss -> s{graphExplorer = ss})

_graphExplorerAction :: Prism' Action GE.Action
_graphExplorerAction = prism GraphExplorerA \action ->
  case action of
    GraphExplorerA caction -> Right caction
    _-> Left action

_ngState :: Lens' AppState NG.State
_ngState = lens (\s -> s.ngState) (\s ss -> s{ngState = ss})

_ngAction :: Prism' Action NG.Action
_ngAction = prism NgramsA \action ->
  case action of
    NgramsA caction -> Right caction
    _-> Left action
