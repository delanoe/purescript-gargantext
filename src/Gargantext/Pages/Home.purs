module Gargantext.Pages.Home where

import Prelude
import Data.Lens (re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Routing.Hash (setHash)
import Gargantext.Components.Lang.Landing.EnUS as En
import Gargantext.Components.Lang.Landing.FrFR as Fr
import Gargantext.Components.Data.Landing (BlockText(..), BlockTexts(..), Button(..), LandingData(..))
import Gargantext.Components.Data.Lang (Lang(..))
import Gargantext.Utils.Reactix as R2

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

-- Layout |

landingData :: Lang -> LandingData
landingData FR = Fr.landingData
landingData EN = En.landingData

------------------------------------------------------------------------

layoutLanding :: Lang -> R.Element
layoutLanding lang = R.createElement layoutLandingCpt props []
  where props = { landingData: landingData lang }

layoutLandingCpt :: R.Component ( landingData :: LandingData )
layoutLandingCpt = R.hooksComponent "LayoutLanding" cpt
  where
    cpt {landingData} _ = do
      pure $ H.span {} [
        H.div { className: "container1" }
              [ jumboTitle landingData false ]
        , H.div { className: "container1" } [] -- TODO put research form
        , H.div { className: "container1" } [ blocksRandomText' landingData ]
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

imageEnter :: LandingData -> _ -> R.Element
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

