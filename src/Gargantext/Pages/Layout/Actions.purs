module Gargantext.Pages.Layout.Actions where

import Prelude hiding (div)

import Control.Monad.Cont.Trans (lift)
import Data.Either (Either(..))
import Data.Lens (Prism', prism)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Gargantext.Components.Login as LN
import Gargantext.Components.Modals.Modal (modalShow)
import Gargantext.Components.Tree as Tree
import Gargantext.Pages.Corpus.Doc.Annotation as D
import Gargantext.Pages.Corpus.Doc.Facets.Documents as DV
import Gargantext.Pages.Corpus.Doc.Facets.Graph as GE
import Gargantext.Pages.Corpus.User.Users as U
import Gargantext.Pages.Layout.Specs.AddCorpus as AC
import Gargantext.Pages.Layout.Specs.Search as S
import Gargantext.Pages.Layout.States (AppState)
import Gargantext.Router (Routes)
import Thermite (PerformAction, modifyState)


data Action
  = Initialize
  | LoginA     LN.Action
  | SetRoute   Routes
  | AddCorpusA AC.Action
  | DocViewA   DV.Action
  | SearchA    S.Action
  | UserPageA  U.Action
  | DocAnnotationViewA  D.Action
  | TreeViewA  Tree.Action
  | GraphExplorerA GE.Action
  | Search     String
  | Go
  | ShowLogin
  | ShowAddcorpus


performAction :: PerformAction AppState {} Action
performAction (SetRoute route)  _ _ = void do
  modifyState $ _ {currentRoute = pure route}
performAction (Search s)  _ _ = void do
  modifyState $ _ {search = s}

performAction (ShowLogin)  _ _ = void do
  liftEffect $ modalShow "loginModal"
  modifyState $ _ {showLogin = true}

performAction (ShowAddcorpus)  _ _ = void do
  liftEffect $ modalShow "addCorpus"
  modifyState $ _ {showCorpus = true}

performAction Go  _ _ = void do
  liftEffect $ modalShow "addCorpus"
  modifyState $ _ {showCorpus = true}
 -- _ <- lift $ setHash "/addCorpus"
  --modifyState id

performAction Initialize  _ state = void do
  _ <- liftEffect $ log "loading Initial nodes"
  case state.initialized of
    false -> do

      lnodes <- lift $ Tree.loadDefaultNode

      case lnodes of
        Left err -> do
          pure unit
        Right d -> do
          _ <- modifyState $ _ { initialized = true, ntreeState = d}
          page <- lift $ DV.loadPage
          case page of
            Left err -> do
              pure unit
            Right docs -> void do
              modifyState $ _ { initialized = true
                              , ntreeState = d
                                -- if length d > 0
                                --             then Tree.exampleTree
                                --            --then fnTransform $ unsafePartial $ fromJust $ head d
                                --            else Tree.initialState

                              , docViewState = docs
                              }
    _ -> do
      pure unit

performAction (LoginA _) _ _ = pure unit
performAction (AddCorpusA _) _ _ = pure unit
performAction (DocViewA _) _ _ = pure unit
performAction (SearchA _) _ _ = pure unit
performAction (UserPageA _) _ _ = pure unit
performAction (DocAnnotationViewA _) _ _ = pure unit
performAction (TreeViewA _) _ _ = pure unit
performAction (GraphExplorerA _) _ _ = pure unit

----------------------------------------------------------

_loginAction :: Prism' Action LN.Action
_loginAction = prism LoginA \action ->
  case action of
    LoginA caction -> Right caction
    _-> Left action

_addCorpusAction :: Prism' Action AC.Action
_addCorpusAction = prism AddCorpusA \action ->
  case action of
    AddCorpusA caction -> Right caction
    _-> Left action

_docViewAction :: Prism' Action DV.Action
_docViewAction = prism DocViewA \action ->
  case action of
    DocViewA caction -> Right caction
    _-> Left action

_searchAction :: Prism' Action S.Action
_searchAction = prism SearchA \action ->
  case action of
    SearchA caction -> Right caction
    _-> Left action

_userPageAction :: Prism' Action U.Action
_userPageAction = prism UserPageA \action ->
  case action of
    UserPageA caction -> Right caction
    _-> Left action

_docAnnotationViewAction :: Prism' Action D.Action
_docAnnotationViewAction = prism DocAnnotationViewA \action ->
  case action of
    DocAnnotationViewA caction -> Right caction
    _-> Left action

_treeAction :: Prism' Action Tree.Action
_treeAction = prism TreeViewA \action ->
  case action of
    TreeViewA caction -> Right caction
    _-> Left action

_graphExplorerAction :: Prism' Action GE.Action
_graphExplorerAction = prism GraphExplorerA \action ->
  case action of
    GraphExplorerA caction -> Right caction
    _-> Left action
