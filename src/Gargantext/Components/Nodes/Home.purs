module Gargantext.Components.Nodes.Home where

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Routing.Hash (setHash)

import Gargantext.Ends (Backend(..))
import Gargantext.Sessions (Sessions(..))
import Gargantext.Sessions as Sessions

import Gargantext.Components.Data.Landing (BlockText(..), BlockTexts(..), Button(..), LandingData(..))
import Gargantext.Components.Lang (LandingLang(..))
import Gargantext.Components.Lang.Landing.EnUS as En
import Gargantext.Components.Lang.Landing.FrFR as Fr
import Gargantext.Components.Nodes.Home.Public (renderPublic)
import Gargantext.License (license)
import Gargantext.Prelude (Unit, map, pure, unit, void, ($), (<>))
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.Home"

type Props = ()

newtype State = State
  { userName :: String
  , password :: String
  }

derive instance newtypeState :: Newtype State _

initialState :: State
initialState = State { userName : "", password : "" }

data Action
  = Documentation
  | Enter
  | Login
  | SignUp

performAction :: Action -> Effect Unit
performAction Documentation = pure unit
performAction Enter = void $ setHash "/search"
performAction Login = void $ setHash "/login"
performAction SignUp = pure unit

langLandingData :: LandingLang -> LandingData
langLandingData LL_FR = Fr.landingData
langLandingData LL_EN = En.landingData

------------------------------------------------------------------------

type HomeProps = ( lang      :: LandingLang
                 , publicBackend :: Backend
                 , backend   :: R.State (Maybe Backend)
                 , sessions  :: R2.Reductor Sessions Sessions.Action
                 , visible  :: R.State Boolean
                 )

homeLayout :: Record HomeProps -> R.Element
homeLayout props = R.createElement homeLayoutCpt props []

homeLayoutCpt :: R.Component HomeProps
homeLayoutCpt = R.hooksComponentWithModule thisModule "homeLayout" cpt
  where
    cpt {lang, backend, publicBackend, sessions, visible} _ = do
      let landingData = langLandingData lang
      pure $ H.span {}
           [ H.div { className: "home-title container1" } [ jumboTitle landingData false ]
           , H.div { className: "home-research-form container1" } [] -- TODO put research form
           , H.div { className: "home-landing-data container1" } [ blocksRandomText' landingData ]
           , H.div { className: "home-public container1" } [ renderPublic { backend
                                                                          , publicBackend
                                                                          , sessions
                                                                          , visible
                                                                          }
                                                           ]
           , license
           ]

------------------------------------------------------------------------

blocksRandomText' :: LandingData -> R.Element
blocksRandomText' (LandingData hd) = blocksRandomText hd.blockTexts

blocksRandomText :: BlockTexts -> R.Element
blocksRandomText (BlockTexts bt) =
  H.div { className: "row" } ( map showBlock bt.blocks )
  where
    showBlock :: BlockText -> R.Element
    showBlock (BlockText b) =
      H.div { className: "col-md-4 content" }
      [ H.h3 {}
        [ H.a { href: b.href, title: b.title}
          [ H.i {className: b.icon} []
          , H.text ("   " <> b.titleText) ] ]
        , H.p {} [ H.text b.text ]
        , H.p {} [ docButton b.docButton ] ]

docButton :: Button -> R.Element
docButton (Button b) =
  H.a { className: "btn btn-outline-primary btn-sm spacing-class"
      , href: b.href
      , target: "blank"
      , title: b.title
      } [ H.span { aria: {hidden : true}
                 , className: "glyphicon glyphicon-hand-right"
                 } []
        , H.text b.text
        ]

-- | TODO
-- <img src='logo.png' onmouseover="this.src='gargantextuel.png';" onmouseout="this.src='logo.png';" />
jumboTitle :: LandingData -> Boolean -> R.Element
jumboTitle (LandingData hd) b =
  H.div {className: jumbo}
  [ H.div { className: "row" }
    [ H.div { className: "col-md-12 content" }
      [ H.div { className: "center" }
        [ H.div { id: "logo-designed" }
          [ H.img { src: "images/logo.png"
                  , title: hd.logoTitle
                  }
          ]
        ]
      ]
    ]
  ]
  where
    jumbo = case b of
      true  -> "jumbotron"
      false -> ""

imageEnter :: forall t. LandingData -> t -> R.Element
imageEnter (LandingData hd) action =
  H.div {className: "row"}
  [ H.div {className: "col-md-offset-5 col-md-6 content"}
    [ H.img { src: "images/Gargantextuel-212x300.jpg"
            , id: "funnyimg"
            , title: hd.imageTitle
            , action
            }
    ]
  ]

