module Gargantext.Components.Nodes.Home where

import Data.Array as Array
import DOM.Simple.Console (log)
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Sequence as Seq
import Effect (Effect)
import Gargantext.Components.Data.Landing (BlockText(..), BlockTexts(..), Button(..), LandingData(..))
import Gargantext.Components.Lang (LandingLang(..))
import Gargantext.Components.Lang.Landing.EnUS as En
import Gargantext.Components.Lang.Landing.FrFR as Fr
import Gargantext.Components.Nodes.Home.Public (renderPublic)
import Gargantext.Ends (Backend(..))
import Gargantext.License (license)
import Gargantext.Prelude -- (Unit, map, pure, unit, void, ($), (<>), (*>))
import Gargantext.Sessions (Sessions(..), unSessions)
import Gargantext.Sessions as Sessions
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Routing.Hash (setHash)

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

           , if Array.length (unSessions $ fst sessions) > 0 
                then tutorial
                else joinButton

           , H.div { className: "home-public container1" } [ renderPublic { backend
                                                                          , publicBackend
                                                                          , sessions
                                                                          , visible
                                                                          }
                                                           ]

           , H.div {className:"center"}
                   [ H.h1 {} [ -- H.span {className: "fa fa-star-o"} []
                              H.text ""
                             ]
                   ]
           , H.div { className: "home-landing-data container1" } [ blocksRandomText' landingData ]
           , license
           ]
      where
        tutorial =  H.div {className:""}
                 $  [ H.h1 {} [H.text "Welcome!"]
                    , H.h2 {} [H.text "For easy start, just watch the tutorials"]
                    ]
                 <> [ H.div { className: "alert alert-info part" } summary ]
                 <> tutos

        summary :: Array R.Element
        summary = [ H.h3 {} [H.text "Tutorial summary"]
                  , H.ol {} [ H.li {} [ H.h4 {} [H.text "How to start ?"]
                                      , H.ol {} toRead
                                      ]
                            , H.li {} [ H.h4 {} [H.text ""]]
                            , H.li {} [ H.h4 {} [H.text "How to play ?"]
                                      , H.ol {} toPlay
                                      ]
                            ]
                  ]
            where
              toRead = map (\(Tuto x) -> H.li {} [H.a {href : "#" <> x.id} [H.text x.title]]) start_tutos
              toPlay = map (\(Tuto x) -> H.li {} [H.a {href : "#" <> x.id} [H.text x.title]]) play_tutos

        tutos :: Array R.Element
        tutos = [ H.h3 {} [H.text "Tutorial resources"]
                , H.div {className : "alert alert-success part"}
                        $ [ H.h4 {} [H.text "How to start ?"] ]
                        <> map (\ (Tuto x) -> H.div { className : "alert alert-success part", id : x.id}
                                            [ video x.id
                                            , H.h4 {} [H.text x.title]
                                            , H.p  {} [H.text x.text ]
                                            ]
                         ) start_tutos
                , H.div {className : "alert alert-warning part"}
                        $ [H.h4 {} [H.text "How to play ?"]]
                        <> map (\ (Tuto x) -> H.div { className : "alert alert-warning part", id : x.id}
                                                    [ video x.id
                                                    , H.h4 {} [H.text x.title]
                                                    , H.p  {} [H.text x.text]
                                                    ]
                               ) play_tutos
                ]

        start_tutos :: Array Tuto
        start_tutos = [ Tuto { title : "The tree is your friend"
                            , id    : "video_tutorial.ogv#t=,8"
                            , text  : "The tree enables you to control all your actions."
                          }
                     ,  Tuto { title : "Edit your profile"
                            , id    : "video_tutorial.ogv#t=9,24"
                            , text  : "At the root of the tree, there is your user node. Your profile is what others users could see to reach you and watch your work. If you delete it you remove all your data from the specified instance."
                            }
                     , Tuto { title : "Discover the nodes of the tree"
                            , id    : "video_tutorial.ogv#t=25,42"
                            , text  : "Under your User node"
                            }
                     , Tuto { title : "Read a corpus"
                            , id    : "video_tutorial.ogv#t=43,79"
                            , text  : "The node corpus has 4 children: docs, list, board, graph.."
                            }
                     , Tuto { title : "Manage your ngrams"
                            , id    : "video_tutorial.ogv#t=80,214"
                            , text  : "4 type of ngrams: Terms, Institutes, Sources, Authors. You will learn how to change the status, group the ngrams and create new categories. You need do save with the sycn button. Then the charts are updated after each sync."
                            }
                     , Tuto { title : "Watch with the board"
                            , id    : "video_tutorial.ogv#t=215,237"
                            , text  : "Build your own watchboard! All your list enable you to have chart to follow the evolution of your corpus."
                            }
                     , Tuto { title : "Explore with the graph"
                            , id    : "video_tutorial.ogv#t=238,293"
                            , text  : "With the map terms you have selected, the graph is built. 3 main panels can be hidden or not to give you more space: tree, controls, side panel (legend, Build your own watchboard! All your list enable you to have chart to follow the evolution of your corpus."
                            }

                     , Tuto { title : "Edit ngrams in your documents"
                            , id    : "video_tutorial.ogv#t=294,312"
                            , text  : "All selected ngrams can be updated in the document and are autmatically updated in the lists."
                            }

                     ]

        play_tutos :: Array Tuto
        play_tutos = [ Tuto { title : "Again the tree is your friend"
                            , id    : "video_tutorial_1.ogv#t=,46"
                            , text  : "At the right of each node, you have a wheel which enable functions exections"
                          }
                      , Tuto { title : "Build your analysis"
                            , id    : "video_tutorial_1.ogv#t=47,146"
                            , text  : "To build your analysis you need to create a corpus. Suppose you want to create it in your private folder. Use the wheel to execute any function on the corpus node in the tree. Search in database to add document to your corpus."
                          }

                      , Tuto { title : "Add document with files, download data"
                            , id    : "video_tutorial_1.ogv#t=157,166"
                            , text  : "You can add CSV files."
                          }

                      , Tuto { title : "Move your corpus elsewhere in the tree"
                            , id    : "video_tutorial_1.ogv#t=167,175"
                            , text  : "Each node can be moved with this function."
                          }

                      , Tuto { title : "Rename your corpus"
                            , id    : "video_tutorial_1.ogv#t=145,160"
                            , text  : "Each node can be renamed."
                          }

                      , Tuto { title : "Delete your corpus"
                            , id    : "video_tutorial_1.ogv#t=179,182"
                            , text  : "Each node can be deleted with its children."
                          }

                      ]



        video fileDuration = H.div {className:"center"}
                                   [ H.video { src: "http://dl.gargantext.org/" <> fileDuration
                                     , title: "tutorial video here"
                                     , id: "source_" <> fileDuration
                                     , width: "900"
                                     , "type": "video/ogg"
                                     , controls: "true"
                                     , muted   : "true"
                                     } [H.text "sorry your browser is not compatible"]
                                   ]

        joinButton = H.div { className:"flex-space-around center" 
                       , paddingTop: "100px"
                       , paddingBottom: "100px"
                       }
                       [ H.button { className: "btn btn-primary my-2"
                                  , on : {click}
                                  , title: "Connect to the server"
                                  } [ H.text "Join"
                                    ]
                       ]
        click _ = log "click!" *> (snd backend) (const $ Just publicBackend)
                               *> (snd visible) (const true)

data Tuto = Tuto { title :: String
                 , id    :: String
                 , text  :: String
               }

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

