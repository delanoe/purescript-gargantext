module Tabview where

import Authorview as AV
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, over, prism, view)
import DocView as DV
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (a, div, li, text, ul)
import React.DOM.Props (_data, _id, aria, className, href, role)
import Sourceview as SV
import Termsview as TV
import Thermite (Render, Spec, _performAction, _render, defaultPerformAction, defaultRender, focus, focusState, simpleSpec, withState)

data Action
  =  DocviewA DV.Action
  | SourceviewA SV.Action
  | AuthorviewA AV.Action
  | TermsviewA TV.Action
  | ChangeTab
  | NoOp

data TabTitle = TabTitle String Int

newtype TabTitleState = TabTitleState {tabTitles :: Array TabTitle, selectedTab :: Int}

type State =
  { docview :: DV.State
  , authorview :: AV.State
  , sourceview :: SV.State
  , termsview :: TV.State
  , tabTitle :: TabTitleState
  }

initialState :: State
initialState =
  { docview : DV.tdata
  , authorview : AV.initialState
  , sourceview : SV.initialState
  , termsview : TV.initialState
  , tabTitle : TabTitleState
    { selectedTab : 1
    , tabTitles : [TabTitle "Docview" 1, TabTitle "Sourceview" 2, TabTitle "Authorview" 3, TabTitle "Termsview" 4]
    }
  }

_doclens :: Lens' State DV.State
_doclens = lens (\s -> s.docview) (\s ss -> s {docview = ss})


_authorlens :: Lens' State AV.State
_authorlens = lens (\s -> s.authorview) (\s ss -> s {authorview = ss})


_sourcelens :: Lens' State SV.State
_sourcelens = lens (\s -> s.sourceview) (\s ss -> s {sourceview = ss})


_termslens :: Lens' State TV.State
_termslens = lens (\s -> s.termsview) (\s ss -> s {termsview = ss})


_tabtitlelens :: Lens' State TabTitleState
_tabtitlelens = lens (\s -> s.tabTitle) (\s ss -> s {tabTitle = ss})


_docAction :: Prism' Action DV.Action
_docAction = prism DocviewA \ action ->
  case action of
    DocviewA laction -> Right laction
    _-> Left action

_sourceAction :: Prism' Action SV.Action
_sourceAction = prism SourceviewA \ action ->
  case action of
    SourceviewA laction -> Right laction
    _-> Left action

_authorAction :: Prism' Action AV.Action
_authorAction = prism AuthorviewA \ action ->
  case action of
    AuthorviewA laction -> Right laction
    _-> Left action


_termsAction :: Prism' Action TV.Action
_termsAction = prism TermsviewA \ action ->
  case action of
    TermsviewA laction -> Right laction
    _-> Left action

docPageSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
docPageSpec = focus _doclens _docAction DV.layoutDocview

authorPageSpec :: forall eff props. Spec (dom :: DOM, console::CONSOLE, ajax :: AJAX | eff) State  props Action
authorPageSpec = focus _authorlens _authorAction AV.authorSpec

sourcePageSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
sourcePageSpec = focus _sourcelens _sourceAction SV.sourceSpec

termsPageSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
termsPageSpec = focus _termslens _termsAction TV.termsSpec

docPageActionSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
docPageActionSpec = simpleSpec (view _performAction docPageSpec) defaultRender

sourcePagesActionSpec :: forall eff props. Spec (dom :: DOM, console::CONSOLE, ajax :: AJAX | eff) State props Action
sourcePagesActionSpec = simpleSpec (view _performAction sourcePageSpec) defaultRender

authorPageActionSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
authorPageActionSpec = simpleSpec (view _performAction authorPageSpec) defaultRender

termsPageActionSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
termsPageActionSpec = simpleSpec (view _performAction termsPageSpec) defaultRender

tabSpec' :: forall eff props. Spec eff State props Action
tabSpec' = container $ fold
      [ withState $ wrapSpec docPageSpec
      , withState $ wrapSpec authorPageSpec
      , withState $ wrapSpec sourcePageSpec
      , withState $ wrapSpec termsPageSpec
      ]
  where
    container :: Spec eff State props Action -> Spec eff State props Action
    container = over _render \render d p s c ->
      [ div [className "tab-content"] $ render d p s c ]
    containerTabContent st = over _render \render d p s c ->
      [ div [className "tab-pane fade"] $ render d p s c ]
    wrapSpec spec st =
      simpleSpec defaultPerformAction (view _render spec)

tabSpec :: forall eff props. Spec (dom :: DOM, console::CONSOLE, ajax :: AJAX | eff) State props  Action
tabSpec = fold
            [ focusState _tabtitlelens tabTitleSpec
            , tabSpec'
            , docPageActionSpec
            , sourcePagesActionSpec
            , authorPageActionSpec
            , termsPageActionSpec
            ]


tabTitleSpec :: forall eff props. Spec eff TabTitleState  props Action
tabTitleSpec = simpleSpec defaultPerformAction render
  where
    render :: Render TabTitleState  props Action
    render dispatch _ state@(TabTitleState st)_ =
      [ ul [className "nav nav-tabs", _id "myTab", role "tablist"]
        (map (item st.selectedTab) st.tabTitles)
      ]
      where
        item sid (TabTitle title iid) =
          li [className "nav-item"]
          [ a [className (if sid == iid then "nav-link active" else "nav-link"), _id ("tv-page-tab-item-" <> show iid), _data {toggle : "tab"}, href "", role "tab", aria {controls : title, selected : (if sid == iid then "true" else "false")}]
            [ text title]
          ]
