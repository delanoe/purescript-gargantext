module Gargantext.Components.Nodes.Home
  ( HomeAction(..)
  , HomeProps
  , State(..)
  , Tuto(..)
  , TutorialProps
  , blocksRandomText
  , blocksRandomText'
  , docButton
  , expertTutos
  , here
  , homeLayout
  , homeLayoutCpt
  , imageEnter
  , incompatible
  , initialState
  , joinButtonOrTutorial
  , jumboTitle
  , langLandingData
  , license
  , performHomeAction
  , playTutos
  , startTutos
  , summary
  , tutorial
  , tutorialCpt
  , video
  )
  where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..), Elevation(..), ModalSizing(..), Position(..), TooltipPosition(..), Variant(..))
import Gargantext.Components.Data.Landing (BlockText(..), BlockTexts(..), Button(..), LandingData(..))
import Gargantext.Components.FolderView as FV
import Gargantext.Components.Lang (LandingLang(..))
import Gargantext.Components.Lang.Landing.EnUS as En
import Gargantext.Components.Lang.Landing.FrFR as Fr
import Gargantext.Config as Config
import Gargantext.Sessions (Session(..), Sessions, Action(Logout), unSessions)
import Gargantext.Sessions as Sessions
import Gargantext.Sessions.Types (Session(..), cleanBackendUrl)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Routing.Hash (setHash)
import Toestand as T

import Effect.Console (log)


here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Home"

newtype State = State { userName :: String, password :: String }

derive instance Newtype State _

initialState :: State
initialState = State { userName: "", password: "" }

data HomeAction
  = Documentation
  | Enter
  | Login
  | SignUp

performHomeAction :: HomeAction -> Effect Unit
performHomeAction Documentation = pure unit
performHomeAction Enter = void $ setHash "/search"
performHomeAction Login = void $ setHash "/login"
performHomeAction SignUp = pure unit

langLandingData :: LandingLang -> LandingData
langLandingData LL_FR = Fr.landingData
langLandingData LL_EN = En.landingData

------------------------------------------------------------------------

type HomeProps =
  ( boxes     :: Boxes )

homeLayout :: R2.Leaf HomeProps
homeLayout = R2.leaf homeLayoutCpt
homeLayoutCpt :: R.Component HomeProps
homeLayoutCpt = here.component "homeLayout" cpt
  where
    cpt { boxes: boxes@{ backend
                       , lang
                       , sessions
                       , showLogin }
        } _ = do
      -- | States
      -- |
      backend'  <- T.useLive T.unequal backend
      sessions' <- T.useLive T.unequal sessions
      lang'     <- T.useLive T.unequal lang

      -- | Computed
      -- |
      let landingData = langLandingData lang'

      -- | Behaviors
      -- |
      let
        click mBackend _ =
          case mBackend of
            Nothing -> do
              mLoc <- Config.matchCurrentLocation
              case mLoc of
                Nothing -> pure unit
                Just b -> do
                  T.write_ (Just b) backend
                  T.write_ true showLogin
            Just _ -> T.write_ true showLogin

      -- | Render
      -- |
      pure $

        R.fragment
        [
          H.div { className: "home-title" }
          [
            H.div
            { className: "home-title__logo" }
            [
              jumboTitle landingData
            ]
          ,
            H.div
            { className: "home-title__jumbo" }
            [
              joinButtonOrTutorial boxes sessions' (click backend')
            ]
          ]
          -- @TODO
          -- H.div { className: "home-research-form" } []
        ,
          blocksRandomText' landingData
        ,
          B.wad
          [ "mt-8" ]
          [
            license
          ]
        ]


joinButtonOrTutorial :: forall e. Boxes
                     -> Sessions
                     -> (e -> Effect Unit)
                     -> R.Element
joinButtonOrTutorial boxes sessions click =
  if Sessions.null sessions
  then joinButton click
  else tutorial { boxes, sessions: Sessions.unSessions sessions }

joinButton :: forall e. (e -> Effect Unit) -> R.Element
joinButton click =
  -- TODO Add G.C.L.F.form -- which backend to use?
  -- form { backend, sessions, visible }
  B.wad
  [ "d-flex", "flex-space-around", "justify-content-center", "align-items-center", "h-100" ]
  [
    H.button
    { className: "btn btn-primary py-2 w-1/3 font-size-140"
    , title: "Connect to the server"
    , on: { click }
    , style: { minWidth: "200px"}
    }
    [
      B.icon
      { name: "sign-in"
      , className: "font-size-90"
      }
    ,
      B.wad_
      [ "virtual-space", "w-2", "d-inline-block" ]

    ,
      H.text "Log in"
    ]
  ]

incompatible :: String
incompatible =
  "Sorry your browser is not compatible: use Firefox or Chromium instead."

video :: String -> R.Element
video fileDuration =
  H.div { className:"col-12 d-flex justify-content-center" }
  [ H.video
    { src, title, id, width: "900", type: "video/ogg", controls: true, muted: true }
    [ H.text incompatible ] ] where
      src = "http://dl.gargantext.org/" <> fileDuration
      title = "tutorial video here"
      id = "source_" <> fileDuration

data Tuto = Tuto { title :: String, id :: String, text  :: String }

summary :: R.Element
summary =
  H.div {}
  [ H.h3 {} [ H.text "Summary"]
  , H.ol {}
    [ sum "Getting Started for beginners" startTutos "alert-info"
    -- , sum "How to play (advanced users)?" playTutos "alert-warning"
    -- , sum "How to master (expert users)?" expertTutos "alert-danger"
    ]]
  where
    sum name tutos class' =
      H.div { className: "alert " <> class' }
      [ H.li {}
        [ H.h4 {} [ H.text name ]
        , H.ol {} (map toSummary tutos) ] ]
    toSummary (Tuto x) = H.li {} [ H.a {href: "#" <> x.id} [ H.text x.title ]]

type TutorialProps =
  ( boxes :: Boxes
  , sessions :: Array Session )

tutorial :: R2.Leaf TutorialProps
tutorial = R2.leaf tutorialCpt
tutorialCpt :: R.Component TutorialProps
tutorialCpt = here.component "tutorial" cpt where
  cpt { boxes, sessions } _ = do

    pure $

      H.div
      { className: "home-tutorial" } $
      makeFolders sessions

      -- , H.h1 {} [H.text "Tutorials"]
      -- , summary
      -- , H.h3 {} [H.text "Resources"]
      -- , section "How to start?" "alert-info" startTutos
      -- , section "How to play?" "alert-warning" playTutos
      -- , section "How to master?" "alert-danger" expertTutos
    where
      {-
      section name class' tutos =
        H.div {} $ Array.cons (H.h4 {} [ H.text name ]) (map (makeTuto class') tutos)
      makeTuto class' (Tuto x) =
        H.div { className : "alert " <> class', id: x.id}
        [ video x.id, H.h4 {} [ H.text x.title ], H.p  {} [ H.text x.text ] ]
      -}

      onSignOutClick session = void $ Sessions.change (Logout session) boxes.sessions


      makeFolders :: Array Session -> Array R.Element
      makeFolders s = sessionToFolder <$> s
        where
          sessionToFolder session@(Session {treeId, username, backend}) =
            H.div
            { className: intercalate " "
                [ "home-title__folders"
                , "card border-dark"
                ]
            }
            [
              H.div
              { className: "card-header bg-dark color-white" }
              [
                B.wad
                [ "d-flex", "align-items-center" ]
                [
                  B.wad
                  [ "text-left", "w-10/12" ]
                  [
                    B.icon
                    { name: "user", className: "pr-1" }
                  ,
                    B.span_ $
                    username <> "@" <> cleanBackendUrl backend
                  ]
                ,
                  B.wad
                  [ "text-right", "w-2/12" ]
                  [
                    B.tooltipContainer
                    { position: TooltipPosition Top
                    , delayShow: 600
                    , tooltipSlot:
                        B.span_ "Log out"
                    , defaultSlot:
                        B.iconButton
                        { name: "sign-out"
                        , callback: \_ -> onSignOutClick session
                        , elevation: Level2
                        , className: "text-light"
                        }
                    }
                  ]
                ]
              ]
            ,
              H.div
              { className: "card-body" }
              [
                FV.folderView
                { nodeId: treeId
                , session
                }
              ]
            ]
      

startTutos :: Array Tuto
startTutos =
  [ Tuto { title: "The tree to manage your data"
         , id: "0_tree.ogv"
         , text : "The tree enables you to control all your actions. The Tree has typed nodes. Each node has some attributes and some methods which depend on its type. This specific ergonomy helps the memorization of all the complexity of the GarganTexts' features: hence you do not need to remember all the documentation! Just remember these simple axioms, the Tree is built with parent-children relations of nodes which have specific attributes and methods. To get its methods and attributes, just click on the wheel near its name (for this feature, see advanced tutorial: how to play with GarganText)." }
  -- ,  Tuto { title : "Edit your profile"
  --         , id    : "0_edit.ogv"
  --         , text  : "At the root of the tree, there is your user node, parent of all others nodes. Your profile is what others users will see or search for to reach you or to watch/follow your work. If you delete it you remove all your data from the specified instance, clear and simple." }
  , Tuto { title : "Discover the nodes of the tree"
         , id    : "0_nodes.ogv"
         , text  : "Under your user node you have 3 main nodes: private, shared and public nodes. Each node has its specific attributes and methods! Under private node, all your work is private only. Under shared folder you can create teams to invite your partners, students or colleagues. Under public node, you can publish your work with the world: hello word!" }
  -- , Tuto { title : "Read a corpus"
  --        , id    : "video_tutorial.mp4#t=43,79"
  --        , text  : "Each fresh corpus node has 4 children only: docs, list, board, graph. The docs node enable you to manage your documents and rate it. The list node let the user to manage its ngrams. The board node sum up your analysis with the main charts you made with your ngrams. The graph node let you explore your data in a new way. Others new type of nodes are coming such as Phylo node..." }
  -- , Tuto { title : "Manage your ngrams"
  --        , id    : "video_tutorial.mp4#t=80,214"
  --        , text  : "By default, 4 types of ngrams are created: Terms extracted from text fields such as title or abstract, Institutes are extracted from the Institute field of the metadata, Sources, Authors. In that tutorial, you will learn how to change the status of ngrams, group it or create new categories. Remember you need to save your work with the sycn button. Then the charts are updated after each sync. Your work is either synchronous or asynchronous: you can save locally your data, disconnect your device and sync when your Internet connection is back." }
  -- , Tuto { title : "Watch with the board"
  --        , id    : "video_tutorial.mp4#t=215,237"
  --        , text  : "Build your own watchboard! Easy. All your list enable you to have charts to follow the evolution of your corpus." }
  -- , Tuto { title : "Explore with the graph"
  --        , id    : "video_tutorial.mp4#t=238,293"
  --        , text  : "With the map terms you have selected already, the graph is built. 3 main panels can be hidden or shown to give you more visual space: tree, controls, side panel. The side panel shows the legend, the selected data and the community you are watching. You can link your corpus with a community (check nodes methods to do this)." }
  -- , Tuto { title : "Edit ngrams in your documents"
  --        , id    : "video_tutorial.mp4#t=294,312"
  --        , text  : "All selected ngrams can be updated in the document and they are autmatically updated in the lists." }
  ]

playTutos :: Array Tuto
playTutos = []
  -- [ Tuto { title : "Again the tree is your friend"
  --        , id    : "video_tutorial_1.mp4#t=,46"
  --        , text  : "At the right of each node, its wheel shows its attributes or enables the execution of its methods. Each type of node has different attributes and methods to help user in an ergonomic way." }
  -- , Tuto { title : "Build your analysis"
  --        , id    : "video_tutorial_1.mp4#t=47,146"
  --        , text  : "To build your analysis you need to create a corpus. Suppose you want to create it in your private folder in this tutorial. Use the wheel to execute any function on the corpus node in the tree. You can search the local database instance, the web or through apis connected to public databases. It becomes easy to add many documents to your dynamic corpus." }
  -- , Tuto { title : "Add documents with files and download your data"
  --        , id    : "video_tutorial_1.mp4#t=157,166"
  --        , text  : "You can add CSV files from Gargantext V3 legacy version: in your previous account, export your corpus and download it on your device. Then, upload it to v4 as CSV file." }
  -- , Tuto { title : "Move your corpus elsewhere in the tree"
  --        , id    : "video_tutorial_1.mp4#t=167,175"
  --        , text  : "Each node can be moved with this function. Move it in your team to share it. Remove it to unshare it. Some nodes can not be moved, it depends on the types methods." }
  -- , Tuto { title : "Rename your corpus"
  --        , id    : "video_tutorial_1.mp4#t=145,160"
  --        , text  : "Some nodes can be renamed, most of them. But you can not rename your User Node which is the root of the tree." }
  -- , Tuto { title : "Delete your corpus"
  --        , id    : "video_tutorial_1.mp4#t=179,182"
  --        , text  : "Each node can be deleted with its children." }
  -- ]

expertTutos :: Array Tuto
expertTutos = []
  -- [ Tuto { title : "Share with a team and send invitations"
  --        , id    : "video_tutorial_2.mp4#t=,46"
  --        , text  : "[Link to update]" }
  -- , Tuto { title : "Multi instance connections"
  --        , id    : "video_tutorial_2.mp4#t=,46"
  --        , text  : "[Link to update]" }
  -- , Tuto { title : "Freeze a graph"
  --        , id    : "video_tutorial_2.mp4#t=,46"
  --        , text  : "[Link to update]" }
  -- , Tuto { title : "Publish"
  --        , id    : "video_tutorial_2.mp4#t=,46"
  --        , text  : "[Link to update]" }
  -- , Tuto { title : "Link a set of document (corpus) with a set of persons (community)"
  --        , id    : "video_tutorial_2.mp4#t=,46"
  --        , text  : "[Link to update]" }
  -- ,  Tuto { title : "Social lists: cumulative work made easy"
  --         , id    : "video_tutorial_2.mp4#t=,46"
  --         , text  : "[Link to update]" }
  -- ,  Tuto { title : "Data mining with calc"
  --         , id    : "video_tutorial_2.mp4#t=,46"
  --         , text  : "[Link to update]" }
  -- ,  Tuto { title : "Collaborative sync edition notes"
  --         , id    : "video_tutorial_2.mp4#t=,46"
  --         , text  : "[Link to update]" }
  -- ,  Tuto { title : "Coding with our notebooks"
  --         , id    : "video_tutorial_2.mp4#t=,46"
  --         , text  : "[Link to update]" }
  -- ,  Tuto { title : "Our api"
  --         , id    : "video_tutorial_2.mp4#t=,46"
  --         , text  : "[Link to update]" }
  -- ,  Tuto { title : "A tour in the code"
  --         , id    : "video_tutorial_2.mp4#t=,46"
  --         , text  : "[Link to update]" }
  -- ]

blocksRandomText' :: LandingData -> R.Element
blocksRandomText' (LandingData hd) = blocksRandomText hd.blockTexts

blocksRandomText :: BlockTexts -> R.Element
blocksRandomText (BlockTexts bt) =
  B.wad
  [ "d-flex", "gap-3" ]
  ( map showBlock bt.blocks )
  where
    showBlock :: BlockText -> R.Element
    showBlock (BlockText b) =
      B.wad
      [ "w-1/3" ]
      [
        H.h3
        {}
        [
          H.a
          { href: b.href
          , title: b.title
          }
          [
            H.i { className: b.icon } []
          , H.text ("   " <> b.titleText) ]
        ]
      ,
        B.wad
        [ "my-2" ]
        [ H.text b.text ]
      ,
        H.p {} [ docButton b.docButton ]
      ]

docButton :: Button -> R.Element
docButton (Button b) =
  H.a
  { className: "btn btn-outline-secondary btn-sm spacing-class"
  , href: b.href
  , target: "blank"
  , title: b.title
  }
  [
    H.span
    { aria: { hidden: true }
    , className: "fa fa-hand-right"
    } []
  ,
    H.text b.text
  ]

-- | TODO
-- <img src='logo.png' onmouseover="this.src='gargantextuel.png';" onmouseout="this.src='logo.png';" />
jumboTitle :: LandingData -> R.Element
jumboTitle (LandingData hd) =
  H.img
  { src: "images/logo.png"
  , title: hd.logoTitle
  }

imageEnter :: forall t. LandingData -> t -> R.Element
imageEnter (LandingData hd) action =
  H.div {className: "row"}
  [ H.div {className: "col-md-offset-5 col-md-6 content"}
    [ H.img { src, action, id: "funnyimg", title: hd.imageTitle } ] ] where
      src = "images/Gargantextuel-212x300.jpg"

license :: R.Element
license =
  H.div
  { className: "home-license" }
  [
    H.text "GarganText "
  ,
    B.icon
    { name: "registered" }
  ,
    H.text " is made by the "
  ,
    H.a
    { href: "https://iscpif.fr"
    , target: "_blank"
    }
    [
      H.text "French Research Agency (FRA), Complex Systems Institute of Paris-Île de France"
    ]
  ,
    H.a
    { href: "http://gitlab.iscpif.fr/humanities/gargantext/blob/stable/LICENSE"
    , target: "_blank"
    , title: "Legal instructions of the project."
    }
    [
      H.text ", with aGPLV3 and CECILL variant Affero compliant licences, "
    ]
  ,
    B.icon
    { name: "copyrights" }
  ,
    H.a
    { href: "https://cnrs.fr"
    , target: "_blank"
    }
    [
      H.text "CNRS-ISCPIF/UAR-3611_2017-Present"
    ]
  ,
    H.text "."
  ]
