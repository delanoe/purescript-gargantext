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
           [ H.div { className: "home-title container1" } [ jumboTitle landingData ]
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
        tutorial =  H.div {className: "mx-auto container"}
                 $  [ H.h1 {} [H.text "Welcome!"]
                    , H.h2 {} [H.text "For easy start, just watch the tutorials"]
                    ]
                 <> [ H.div { className: "" } summary ]
                 <> tutos

        summary :: Array R.Element
        summary = [ H.h3 {} [ H.text "Tutorial summary"]
                            , H.ol {} [ H.div {className: "alert alert-info"}
                                    [ H.li {} [ H.h4 {} [H.text "How to start (beginner users) ?"]
                                              , H.ol {} (toSummary start_tutos)
                                              ]
                                    ]
{-
                            , H.div {className: "alert alert-warning"}
                                    [ H.li {} [ H.h4 {} [H.text "How to play (advanced users)?"]
                                              , H.ol {} (toSummary play_tutos)
                                              ]
                                    ]

                            , H.div {className: "alert alert-danger"}
                                    [ H.li {} [ H.h4 {} [H.text "How to master (expert users)?"]
                                              , H.ol {} (toSummary expert_tutos)
                                              ]
                                    ]
                            -}
                            ]
                  ]
            where
              toSummary x = map (\(Tuto x) -> H.li {} [H.a {href : "#" <> x.id} [H.text x.title]]) x

        tutos :: Array R.Element
        tutos = [ H.h3 {} [H.text "Tutorial resources"]
                , H.div {className : ""}
                        $ [ H.h4 {} [H.text "How to start ?"] ]
                        <> map (\ (Tuto x) -> H.div { className : "alert alert-info", id : x.id}
                                            [ video x.id
                                            , H.h4 {} [H.text x.title]
                                            , H.p  {} [H.text x.text ]
                                            ]
                         ) start_tutos
{-
                , H.div {className : ""}
                        $ [H.h4 {} [H.text "How to play ?"]]
                        <> map (\ (Tuto x) -> H.div { className : "alert alert-warning", id : x.id}
                                                    [ video x.id
                                                    , H.h4 {} [H.text x.title]
                                                    , H.p  {} [H.text x.text]
                                                    ]
                               ) play_tutos
                , H.div {className : ""}
                        $ [H.h4 {} [H.text "How to master ?"]]
                        <> map (\ (Tuto x) -> H.div { className : "alert alert-danger", id : x.id}
                                                    [ video x.id
                                                    , H.h4 {} [H.text x.title]
                                                    , H.p  {} [H.text x.text]
                                                    ]
                               ) expert_tutos
                               -} 
               ]

        start_tutos :: Array Tuto
        start_tutos = [ Tuto { title : "The tree is your friend"
                            , id    : "0_tree.ogv"
                            , text  : "The tree enables you to control all your actions. The Tree has typed nodes. Each node has some attributes and some methods which depend on its type. This specific ergonomy helps the memorization of all the complexity of the GarganTexts' features: hence you do not need to remember all the documentation! Just remember these simple axioms, the Tree is built with parent-children relations of nodes which have specific attributes and methods. To get its methods and attributes, just click on the wheel near its name (for this feature, see advanced tutorial: how to play with GarganText)."
                          }
                     {-,  Tuto { title : "Edit your profile"
                            , id    : "0_edit.ogv"
                            , text  : "At the root of the tree, there is your user node, parent of all others nodes. Your profile is what others users will see or search for to reach you or to watch/follow your work. If you delete it you remove all your data from the specified instance, clear and simple."
                            }
                     -}
                     , Tuto { title : "Discover the nodes of the tree"
                            , id    : "0_nodes.ogv"
                            , text  : "Under your user node you have 3 main nodes: private, shared and public nodes. Each node has its specific attributes and methods! Under private node, all your work is private only. Under shared folder you can create teams to invite your partners, students or colleagues. Under public node, you can publish your work with the world: hello word!"
                            }
{-
                     , Tuto { title : "Read a corpus"
                            , id    : "video_tutorial.mp4#t=43,79"
                            , text  : "Each fresh corpus node has 4 children only: docs, list, board, graph. The docs node enable you to manage your documents and rate it. The list node let the user to manage its ngrams. The board node sum up your analysis with the main charts you made with your ngrams. The graph node let you explore your data in a new way. Others new type of nodes are coming such as Phylo node..."
                            }
                     , Tuto { title : "Manage your ngrams"
                            , id    : "video_tutorial.mp4#t=80,214"
                            , text  : "By default, 4 types of ngrams are created: Terms extracted from text fields such as title or abstract, Institutes are extracted from the Institute field of the metadata, Sources, Authors. In that tutorial, you will learn how to change the status of ngrams, group it or create new categories. Remember you need to save your work with the sycn button. Then the charts are updated after each sync. Your work is either synchronous or asynchronous: you can save locally your data, disconnect your device and sync when your Internet connection is back."
                            }
                     , Tuto { title : "Watch with the board"
                            , id    : "video_tutorial.mp4#t=215,237"
                            , text  : "Build your own watchboard! Easy. All your list enable you to have charts to follow the evolution of your corpus."
                            }
                     , Tuto { title : "Explore with the graph"
                            , id    : "video_tutorial.mp4#t=238,293"
                            , text  : "With the map terms you have selected already, the graph is built. 3 main panels can be hidden or shown to give you more visual space: tree, controls, side panel. The side panel shows the legend, the selected data and the community you are watching. You can link your corpus with a community (check nodes methods to do this)."
                            }

                     , Tuto { title : "Edit ngrams in your documents"
                            , id    : "video_tutorial.mp4#t=294,312"
                            , text  : "All selected ngrams can be updated in the document and they are autmatically updated in the lists."
                            }
                            -}
                     ]

        play_tutos :: Array Tuto
        play_tutos = []{- Tuto { title : "Again the tree is your friend"
                            , id    : "video_tutorial_1.mp4#t=,46"
                            , text  : "At the right of each node, its wheel shows its attributes or enables the execution of its methods. Each type of node has different attributes and methods to help user in an ergonomic way."
                          }
                      , Tuto { title : "Build your analysis"
                             , id    : "video_tutorial_1.mp4#t=47,146"
                             , text  : "To build your analysis you need to create a corpus. Suppose you want to create it in your private folder in this tutorial. Use the wheel to execute any function on the corpus node in the tree. You can search the local database instance, the web or through apis connected to public databases. It becomes easy to add many documents to your dynamic corpus."
                            }

                      , Tuto { title : "Add documents with files and download your data"
                            , id    : "video_tutorial_1.mp4#t=157,166"
                            , text  : "You can add CSV files from Gargantext V3 legacy version: in your previous account, export your corpus and download it on your device. Then, upload it to v4 as CSV file."
                          }

                      , Tuto { title : "Move your corpus elsewhere in the tree"
                            , id    : "video_tutorial_1.mp4#t=167,175"
                            , text  : "Each node can be moved with this function. Move it in your team to share it. Remove it to unshare it. Some nodes can not be moved, it depends on the types methods."
                          }

                      , Tuto { title : "Rename your corpus"
                            , id    : "video_tutorial_1.mp4#t=145,160"
                            , text  : "Some nodes can be renamed, most of them. But you can not rename your User Node which is the root of the tree."
                          }

                      , Tuto { title : "Delete your corpus"
                            , id    : "video_tutorial_1.mp4#t=179,182"
                            , text  : "Each node can be deleted with its children."
                          }

                      ]
                      --}

        expert_tutos :: Array Tuto
        expert_tutos = [] {- Tuto { title : "Share with a team and send invitations"
                              , id    : "video_tutorial_2.mp4#t=,46"
                              , text  : "[Link to update]"
                          }

                       , Tuto { title : "Multi instance connections"
                              , id    : "video_tutorial_2.mp4#t=,46"
                              , text  : "[Link to update]"
                          }

                       , Tuto { title : "Freeze a graph"
                              , id    : "video_tutorial_2.mp4#t=,46"
                              , text  : "[Link to update]"
                          }

                       , Tuto { title : "Publish"
                              , id    : "video_tutorial_2.mp4#t=,46"
                              , text  : "[Link to update]"
                          }

                       , Tuto { title : "Link a set of document (corpus) with a set of persons (community)"
                              , id    : "video_tutorial_2.mp4#t=,46"
                              , text  : "[Link to update]"
                          }
                       ,  Tuto { title : "Social lists: cumulative work made easy"
                              , id    : "video_tutorial_2.mp4#t=,46"
                              , text  : "[Link to update]"
                          }
                       ,  Tuto { title : "Data mining with calc"
                              , id    : "video_tutorial_2.mp4#t=,46"
                              , text  : "[Link to update]"
                          }
                      ,  Tuto { title : "Collaborative sync edition notes"
                                , id    : "video_tutorial_2.mp4#t=,46"
                                , text  : "[Link to update]"
                            }
                      ,  Tuto { title : "Coding with our notebooks"
                                , id    : "video_tutorial_2.mp4#t=,46"
                                , text  : "[Link to update]"
                            }

                      ,  Tuto { title : "Our api"
                                , id    : "video_tutorial_2.mp4#t=,46"
                                , text  : "[Link to update]"
                            }
                      ,  Tuto { title : "A tour in the code"
                                , id    : "video_tutorial_2.mp4#t=,46"
                                , text  : "[Link to update]"
                            }
                      ]
                      --}


        video fileDuration = H.div {className:"center"}
                                   [ H.video { src: "http://dl.gargantext.org/" <> fileDuration
                                     , title: "tutorial video here"
                                     , id: "source_" <> fileDuration
                                     , width: "900"
                                     , "type": "video/ogg"
                                     , controls: true
                                     , muted   : true
                                   } [H.text "Sorry your browser is not compatible: use Firefox or Chromium instead."]
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
                 , className: "fa fa-hand-right"
                 } []
        , H.text b.text
        ]

-- | TODO
-- <img src='logo.png' onmouseover="this.src='gargantextuel.png';" onmouseout="this.src='logo.png';" />
jumboTitle :: LandingData -> R.Element
jumboTitle (LandingData hd) =
  H.div {} [
    H.div { className: "row" } [
       H.div { className: "mx-auto" } [
          H.div { id: "logo-designed" } [
             H.img { src: "images/logo.png"
                   , title: hd.logoTitle
                   }
             ]
          ]
       ]
    ]

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

