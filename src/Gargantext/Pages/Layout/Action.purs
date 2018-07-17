module Gargantext.Pages.Layout.Action where

import Prelude hiding (div)

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Array (length)
import Data.Either (Either(..))

import Gargantext.Pages.Corpus.Doc.Annotation   as D
import Gargantext.Pages.Corpus.Doc.Body         as CA
import Gargantext.Pages.Corpus.Doc.Document     as DV
import Gargantext.Components.Login              as LN
import Gargantext.Components.Modals.Modal          (modalShow)
import Gargantext.Components.Tree               as Tree
import Gargantext.Pages.Corpus.Doc.Facets.Dashboard as Dsh
import Gargantext.Pages.Corpus                  as AC
import Gargantext.Router                           (Routes(..))
import Gargantext.Pages.Corpus.User.Users       as U
import Gargantext.Pages.Corpus.Doc.Facets.Graph as GE
import Gargantext.Pages.Home                    as L
import Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsTable as NG
import Gargantext.Pages.Search                  as S
import Gargantext.Pages.Corpus.Doc.Facets       as TV

import Gargantext.Pages.Layout.State (AppState)
import Gargantext.Pages.Corpus.Doc.Document as DV

import Network.HTTP.Affjax (AJAX)
import Thermite (PerformAction, modifyState)



data Action
  = Initialize
  | LandingA   L.Action
  | LoginA     LN.Action
  | SetRoute   Routes
  | AddCorpusA AC.Action
  | DocViewA   DV.Action
  | SearchA    S.Action
  | UserPageA  U.Action
  | DocAnnotationViewA  D.Action
  | TreeViewA  Tree.Action
  | TabViewA   TV.Action
  | GraphExplorerA GE.Action
  | DashboardA Dsh.Action
  | Search     String
  | Go
  | CorpusAnalysisA CA.Action
  | ShowLogin
  | ShowAddcorpus
  | NgramsA    NG.Action


performAction :: forall eff props. PerformAction ( dom :: DOM
                                                 , ajax :: AJAX
                                                 , console :: CONSOLE
                                                 | eff
                                                 ) AppState props Action
performAction (SetRoute route)  _ _ = void do
  modifyState $ _ {currentRoute = pure route}
performAction (Search s)  _ _ = void do
  modifyState $ _ {search = s}

performAction (ShowLogin)  _ _ = void do
  liftEff $ modalShow "loginModal"
  modifyState $ _ {showLogin = true}

performAction (ShowAddcorpus)  _ _ = void do
  liftEff $ modalShow "addCorpus"
  modifyState $ _ {showCorpus = true}

performAction Go  _ _ = void do
  liftEff $ modalShow "addCorpus"
  modifyState $ _ {showCorpus = true}
 -- _ <- lift $ setHash "/addCorpus"
  --modifyState id

performAction Initialize  _ state = void do
  _ <- liftEff $ log "loading Initial nodes"
  case state.initialized of
    false -> do

      lnodes <- lift $ Tree.loadDefaultNode

      case lnodes of
        Left err -> do
          modifyState id
        Right d -> do
          page <- lift $ DV.loadPage
          case page of
            Left err -> do
              modifyState id
            Right docs -> do
              modifyState $ _ { initialized = true
                              , ntreeView = if length d > 0
                                            then Tree.exampleTree
                                           --then fnTransform $ unsafePartial $ fromJust $ head d
                                           else Tree.initialState

                              , docViewState = docs
                              }
    _ -> do
      modifyState id

performAction _ _ _ = void do
  modifyState id
