module Gargantext.Components.PhyloExplorer.Config.ConfigForm
  ( configForm
  , FormData
  ) where

import Gargantext.Prelude

import DOM.Simple.Console (log3)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Foldable (foldl, intercalate)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Components.PhyloExplorer.API (CliqueFilter(..), ReflexiveClique(..), ReflexiveTimeUnit(..))
import Gargantext.Hooks.FormValidation (VForm, useFormValidation)
import Gargantext.Components.Forest.Tree.Node.Tools (formChoiceSafe, panel)
import Gargantext.Hooks.FormValidation.Unboxed as FV
import Gargantext.Hooks.StateRecord (useStateRecord)
import Gargantext.Hooks.StateRecord.Behaviors (setter)
import Gargantext.Utils (nbsp, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record (merge)
import Record as Record
import Record.Extra (pick)

type Props =
  ( callback  :: Record FormData -> Effect Unit
  , status    :: ComponentStatus
  | Options
  )

type Options = ( | FormData )

data ReflexiveClusterAlgoMode
  = Basic_
  | Expert_

derive instance Generic ReflexiveClusterAlgoMode _
derive instance Eq ReflexiveClusterAlgoMode
instance Show ReflexiveClusterAlgoMode where show = genericShow
instance Read ReflexiveClusterAlgoMode where
  read :: String -> Maybe ReflexiveClusterAlgoMode
  read = case _ of
    "Basic_"  -> Just Basic_
    "Expert_" -> Just Expert_
    _         -> Nothing


options :: Record Options
options = Record.merge {} defaultData

configForm :: forall r. R2.OptLeaf Options Props r
configForm = R2.optLeaf component options

component :: R.Component Props
component = R.hooksComponent "configForm" cpt where
  cpt props _ = do
  -- Hooks

    { state
    , bindStateKey
    , stateBox
    } <- useStateRecord (pick props :: Record FormData)

    fv <- useFormValidation

  -- Behaviors
    let

      -- @onSubmit: exec whole form validation and execute callback
      onSubmit = do

        result <- fv.try (\_ -> formValidation state)
        case result of
          Left err -> log3 "configForm validation error" state err
          Right _  -> props.callback state


    let modeChoice =
          H.div
          { className: "phylo-config-form__group" }
          [
            -- H.div
            -- { className: "phylo-config-form__row" }
            -- [
            --   H.div
            --   { className: "phylo-config-form__col" }
            --   [
                -- Clique type
                H.div
                { className: intercalate " "
                    [ "form-group"
                    , "text-center"
                    ]
                }
                [
                  H.div
                  { className: "form-group__label" }
                  [
                    H.label {} [ H.text "Mode choice" ]
                  ]
                ,
                  H.div
                  { className: "form-group__field" }
                  [
                    H.div
                    { className: "btn-group"
                    , role: "group"
                    }
                    [
                      B.button
                      { callback: \_ -> setter stateBox "defaultMode" $ show true
                      -- , variant: OutlinedButtonVariant Secondary
                      , variant: ButtonVariant Light
                      , className: state.defaultMode == show true ?
                          "active" $
                          "border-gray-300"
                      }
                      [
                        H.text "Automatic"
                      ]
                    ,
                      B.button
                      { callback: \_ -> setter stateBox "defaultMode" $ show false
                      -- , variant: OutlinedButtonVariant Secondary
                      , variant: ButtonVariant Light
                      , className: state.defaultMode == show false ?
                          "active" $
                          "border-gray-300"
                      }
                      [
                        H.text "Advanced"
                      ]
                    ]
                  ]
                ]
              -- ]
              -- ]
              ]


  -- Render

    let
      formAdvanced =
        H.form
        { className: "phylo-config-form" }
        [
          H.div
          { className: "phylo-config-form__group" }
          [
            H.div
            { className: "" }
            [
              H.div
              { className: "" }
              [
                -- Time Unit
                B.fieldset
                { className: "phylo-config-form__group mx-0"
                , titleSlot: H.text "Time unit"
                }
                [
                --   H.div
                --   { className: "phylo-config-form__row" }
                --   [
                --     H.div { className: "" } 
                --     [
                --     ]
                --   ]
                -- ,
                  H.div
                  { className: "phylo-config-form__row" }
                  [
                    H.div { className: "w-auto phylo-config-form__col text-nowrap" }
                    [
                      -- Granularity
                      H.div
                      { className: intercalate " "
                          [ "form-group"
                          , "mb-1"
                          ]
                      }
                      [
                        H.div
                        { className: "form-group__label" }
                        [
                          H.label {} [ H.text "Granularity" ]
                        ]
                      ,
                        H.div
                        { className: "form-group__field" }
                        [
                          B.formSelect
                          (bindStateKey "granularity")
                          [
                            H.option
                            { value: show Year_ }
                            [ H.text "Year" ]
                          ,
                            H.option
                            { value: show Month_ }
                            [ H.text "Month" ]
                          ,
                            H.option
                            { value: show Week_ }
                            [ H.text "Week" ]
                          ,
                            H.option
                            { value: show Day_ }
                            [ H.text "Day" ]
                          ]
                        ]
                      ]
                    ]
                  ,
                    H.div
                    { className: "w-10 ml-1" }
                    [
                      -- Period
                      H.div
                      { className: intercalate " "
                          [ "form-group"
                          , "mb-1"
                          , (fv.hasError' "period") ?
                              "form-group--error" $
                              mempty
                          ]
                      }
                      [
                        H.div
                        { className: "form-group__label" }
                        [
                          H.label {} [ H.text "Period" ]
                        ]
                      ,
                        H.div
                        { className: "form-group__field" }
                        [
                          B.formInput $
                          { type: "number"
                          } `merge` bindStateKey "period"
                        ,
                          R2.when (fv.hasError' "period") $
                            H.div
                            { className: "form-group__error" }
                            [
                              H.text "Please enter an `Int` value (eg. 3)"
                            ]
                        ]
                      ]
                    ]
                  ,
                    H.div
                    { className: "w-10 ml-1" }
                    [
                      -- Step
                      H.div
                      { className: intercalate " "
                          [ "form-group"
                          , "mb-1"
                          , (fv.hasError' "step") ?
                              "form-group--error" $
                              mempty
                          ]
                      }
                      [
                        H.div
                        { className: "form-group__label" }
                        [
                          H.label {} [ H.text "Step" ]
                        ]
                      ,
                        H.div
                        { className: "form-group__field" }
                        [
                          B.formInput $
                          { type: "number"
                          } `merge` bindStateKey "step"
                        ,
                          R2.when (fv.hasError' "step") $
                            H.div
                            { className: "form-group__error" }
                            [
                              H.text "Please enter an `Int` value (eg. 3)"
                            ]
                        ]
                      ]
                    ]
                  ,
                    H.div
                    { className: "w-10 ml-1 text-nowrap" }
                    [
                      -- Matching frame
                      H.div
                      { className: intercalate " "
                          [ "form-group"
                          , "mb-1"
                          , (fv.hasError' "matchingFrame") ?
                              "form-group--error" $
                              mempty
                          ]
                      }
                      [
                        H.div
                        { className: "form-group__label" }
                        [
                          H.label {} [ H.text "Matching frame" ]
                        ]
                      ,
                        H.div
                        { className: "form-group__field" }
                        [
                          B.formInput $
                          { type: "number"
                          } `merge` bindStateKey "matchingFrame"
                        ,
                          R2.when (fv.hasError' "matchingFrame") $
                            H.div
                            { className: "form-group__error" }
                            [
                              H.text "Please enter an `Int` value (eg. 3)"
                            ]
                        ]
                      ]
                    ]
                  ]
                ]
              ,
                -- Quality
                B.fieldset
                { className: "phylo-config-form__group mx-0"
                , titleSlot: H.text "Zoom"
                }
                [
                  H.div
                  { className: intercalate " "
                      [ "form-group"
                      , "mt-0"
                      , "mb-1"
                      , "text-center"
                      , (fv.hasError' "quality") ?
                          "form-group--error" $
                          mempty
                      ]
                  }
                  [
                    H.div
                    { className: "form-group__label" }
                    [
                      H.label {} [ H.text "Level of observation" ]
                    ]
                  ,
                    H.div
                    { className: "form-group__field" }
                    [
                      H.div { className: "range-simple" } 
                      [
                        H.div { className: "range-simple__field d-flex" }
                        [
                          H.div { className: "col-9 p-0 mx-1 mt-1" } [
                            B.formInput $
                            { className: "range-simple__input p-0 h-auto border-0 p-0 text-center"
                            , type: "range"
                            , step: "0.01"
                            , min: "0"
                            , max: "1"
                            } `merge` bindStateKey "quality"
                          ]
                        ,
                          H.div { className: "col-3 p-0 mx-1" } [
                            B.formInput $
                            { className: "range-simple__witness text-small"
                            , type: "number"
                            , step: "0.1"
                            , min: "0"
                            , max: "1"
                            , status: Disabled
                            -- , readOnly: Disabled
                            } `merge` bindStateKey "quality"
                          ]
                        ]
                      ]
                    ]
                  
                    -- ,

                    --   B.formInput $
                    --   { type: "number"
                    --   , step: "0.1"
                    --   , min: "0"
                    --   , max: "1"
                    --   } `merge` bindStateKey "quality"
                  ]
                ]
              ,


                -- Clusterisation algo
                B.fieldset
                { className: "phylo-config-form__group mx-0"
                , titleSlot: H.text "Clusterisation algorithm"
                }
                [
                  -- Mode
                  H.div
                  { className: intercalate " "
                      [ "form-group text-center mb-2"
                      ]
                  }
                  [
                    H.div
                    { className: "form-group__label" }
                    [
                      H.label {} [ H.text "Mode" ]
                    ]
                  ,
                    H.div
                    { className: "form-group__field" }
                    [
                      H.div
                      { className: "btn-group"
                      , role: "group"
                      }
                      [
                        B.button
                        { callback: \_ -> setter stateBox "clusterAlgoMode" $ show Basic_
                        -- , variant: OutlinedButtonVariant Secondary
                        , variant: ButtonVariant Light
                        , className: state.clusterAlgoMode == show Basic_ ?
                            "active" $
                            "border-gray-300"
                        }
                        [
                          H.text "Basic"
                        ]
                      ,
                        B.button
                        { callback: \_ -> setter stateBox "clusterAlgoMode" $ show Expert_
                        -- , variant: OutlinedButtonVariant Secondary
                        , variant: ButtonVariant Light
                        , className: state.clusterAlgoMode == show Expert_ ?
                            "active" $
                            "border-gray-300"
                        }
                        [
                          H.text "Expert"
                        ]
                      ]
                    ]
                  ]
                ,
                  H.div
                  { className: "" }
                  [
                    -- Mode Basic / Basic_
                    R2.when (state.clusterAlgoMode == show Basic_) $

                      H.div { className: "clustrisation-algorythm-basic form-group" }
                      [
                        -- Title FIS
                        H.div
                        { className: "form-group__label text-center" }
                        [
                          H.label {} [ H.text "FIS" ]
                        ]
                      ,
                        H.div
                        { className: "phylo-config-form__row justify-content-center" }
                        [
                        
                          -- Support
                          H.div
                          { className: intercalate " "
                              [ "form-group col-4 w-10 text-center mb-0"
                              , (fv.hasError' "support") ?
                                  "form-group--error" $
                                  mempty
                              ]
                          }
                          [
                            H.div
                            { className: "" }
                            [
                              H.label {} [ H.text "Support" ]
                            ]
                          ,
                            H.div
                            { className: "form-group__field" }
                            [
                              B.formInput $ 
                              { type: "number"
                              , min: "1"
                              } `merge` bindStateKey "support"
                            ,
                              R2.when (fv.hasError' "support") $
                                H.div
                                { className: "form-group__error" }
                                [
                                  H.text "Please enter an `Int` value (eg. 3)"
                                ]
                            ]
                          ]
                        ,
                          -- Size
                          H.div
                          { className: intercalate " "
                              [ "form-group col-4 w-10 text-center mb-0"
                              , (fv.hasError' "size") ?
                                  "form-group--error" $
                                  mempty
                              ]
                          }
                          [
                            H.div
                            { className: "" }
                            [
                              H.label {} [ H.text "Size" ]
                            ]
                          ,
                            H.div
                            { className: "form-group__field" }
                            [
                              B.formInput $ 
                              { type: "number" 
                              } `merge` bindStateKey "size"
                            ,
                              R2.when (fv.hasError' "size") $
                                H.div
                                { className: "form-group__error" }
                                [
                                  H.text "Please enter an `Int` value (eg. 3)"
                                ]
                            ]
                          ]
                        ]
                      ]
                  ]
                ,
                  H.div
                  { className: "" }
                  [
                    -- Mode Expert / Expert_
                    R2.when (state.clusterAlgoMode == show Expert_) $

                      H.div { className: "text-center" }
                      [
                        H.div
                        { className: "form-group__field" }
                        [
                          H.div
                          { className: "btn-group"
                          , role: "group"
                          }
                          [
                            B.button
                            { callback: \_ -> setter stateBox "cliqueType" $ show FIS_
                            -- , variant: OutlinedButtonVariant Secondary
                            , variant: ButtonVariant Light
                            , className: state.cliqueType == show FIS_ ?
                                "active" $
                                ""
                            }
                            [
                              H.text "FIS"
                            ]
                          ,
                            B.button
                            { callback: \_ -> setter stateBox "cliqueType" $ show MaxClique_
                            -- , variant: OutlinedButtonVariant Secondary
                            , variant: ButtonVariant Light
                            , className: state.cliqueType == show MaxClique_ ?
                                "active" $
                                ""
                            }
                            [
                              H.text "MaxClique"
                            ]
                          ]
                        ]
                      
                      ,
                        H.div {} 
                        [
                        
                        R2.when (state.cliqueType == show FIS_) $

                          H.div { className: "clustrisation-algorythm-basic form-group" }
                          [
                            -- Title FIS
                            H.div
                            { className: "form-group__label text-center" }
                            [
                              H.label {} [ H.text "FIS" ]
                            ]
                          ,
                            H.div
                            { className: "phylo-config-form__row justify-content-center" }
                            [
                            
                              -- Support
                              H.div
                              { className: intercalate " "
                                  [ "form-group col-4 w-10 text-center mb-0"
                                  , (fv.hasError' "support") ?
                                      "form-group--error" $
                                      mempty
                                  ]
                              }
                              [
                                H.div
                                { className: "" }
                                [
                                  H.label {} [ H.text "Support" ]
                                ]
                              ,
                                H.div
                                { className: "form-group__field" }
                                [
                                  B.formInput $ 
                                  { type: "number"
                                  , min: "1"
                                  } `merge` bindStateKey "support"
                                ,
                                  R2.when (fv.hasError' "support") $
                                    H.div
                                    { className: "form-group__error" }
                                    [
                                      H.text "Please enter an `Int` value (eg. 3)"
                                    ]
                                ]
                              ]
                            ,
                              -- Size
                              H.div
                              { className: intercalate " "
                                  [ "form-group col-4 w-10 text-center mb-0"
                                  , (fv.hasError' "size") ?
                                      "form-group--error" $
                                      mempty
                                  ]
                              }
                              [
                                H.div
                                { className: "" }
                                [
                                  H.label {} [ H.text "Size" ]
                                ]
                              ,
                                H.div
                                { className: "form-group__field" }
                                [
                                  B.formInput $ 
                                  { type: "number" 
                                  } `merge` bindStateKey "size"
                                ,
                                  R2.when (fv.hasError' "size") $
                                    H.div
                                    { className: "form-group__error" }
                                    [
                                      H.text "Please enter an `Int` value (eg. 3)"
                                    ]
                                ]
                              ]
                            ]
                          ]

                        ]
                      ,
                        H.div {}
                        [

                        R2.when (state.cliqueType == show MaxClique_) $

                          H.div { className: "clustrisation-algorythm-expert form-group pt-1" }
                          [
                            -- Title MaxClique
                            H.div
                            { className: "form-group__label text-center" }
                            [
                              H.label {} [ H.text "MaxClique" ]
                            ]
                          ,
                            H.div
                            { className: "phylo-config-form__row justify-content-center" }
                            [
                              -- Size
                              H.div
                              { className: intercalate " "
                                  [ "form-group col-3 w-10 text-center mb-0"
                                  , (fv.hasError' "size") ?
                                      "form-group--error" $
                                      mempty
                                  ]
                              }
                              [
                                H.div
                                { className: "" }
                                [
                                  H.label {} [ H.text "Size" ]
                                ]
                              ,
                                H.div
                                { className: "form-group__field" }
                                [
                                  B.formInput $
                                  { type: "number"
                                  , step: "1"
                                  , min: "0"
                                  , max: "10"
                                  } `merge` bindStateKey "size"
                                ,
                                  R2.when (fv.hasError' "size") $
                                    H.div
                                    { className: "form-group__error" }
                                    [
                                      H.text "Please enter an `Int` value (eg. 3)"
                                    ]
                                ]
                              ]
                            ,
                              -- Treshold
                              H.div
                              { className: intercalate " "
                                  [ "form-group col-4 w-10 text-center mb-0"
                                  , (fv.hasError' "threshold") ?
                                      "form-group--error" $
                                      mempty
                                  ]
                              }
                              [
                                H.div
                                { className: "" }
                                [
                                  H.label {} [ H.text "Treshold" ]
                                ]
                              ,
                                H.div
                                { className: "form-group__field" }
                                [
                                  B.formInput $
                                  { type: "number"
                                  , step: "0.0001"
                                  , min: "0.0001"
                                  , max: "1"
                                  } `merge` bindStateKey "threshold"
                                ,
                                  R2.when (fv.hasError' "threshold") $
                                    H.div
                                    { className: "form-group__error" }
                                    [
                                      H.text "Please enter a `Double` value (eg. 0.5)"
                                    ]
                                ]
                              ]
                            ,
                              -- Clique filter
                              H.div
                              { className: intercalate " "
                                  [ "form-group col-5 w-10 text-center mb-0 text-nowrap"
                                  ]
                              }
                              [
                                H.div
                                { className: "" }
                                [
                                  H.label {} [ H.text "Filter type" ]
                                ]
                              ,
                                H.div
                                { className: "form-group__field" }
                                [
                                  B.formSelect
                                  ( bindStateKey "cliqueFilter" )
                                  [
                                    H.option
                                    { value: show ByThreshold }
                                    [ H.text "By threshold" ]
                                  ,
                                    H.option
                                    { value: show ByNeighbours }
                                    [ H.text "By neighbours" ]
                                  ]
                                ]
                              ]
                            ]
                          ]

                        ]

                      ]

                    
                  ]
                ]

              ,
                
                H.div 
                { className: "" }
                [
                  -- -- Mode Expert (part 2) / TYPE::MaxClique_
                  -- R2.when (state.cliqueType == show MaxClique_) $

                    H.div
                    { className: "phylo-config-form__row" }
                    [
                      -- Proximity
                      H.div
                      { className: intercalate " "
                          [ "form-group col-3 w-10 text-center mb-0"
                          , (fv.hasError' "proximity") ?
                              "form-group--error" $
                              mempty
                          ]
                      }
                      [
                        H.div
                        { className: "form-group__label" }
                        [
                          H.label {} [ H.text "Proximity" ]
                        ]
                      ,
                        H.div
                        { className: "form-group__field" }
                        [
                          B.formInput $
                          { type: "number"
                          , step: "0.1"
                          , min: "0.1"
                          , max: "1"
                          } `merge` bindStateKey "proximity"
                        ,
                          R2.when (fv.hasError' "proximity") $
                            H.div
                            { className: "form-group__error" }
                            [
                              H.text "Please enter a `Double` value (eg. 0.5)"
                            ]
                        ]
                      ]
                    ,
                      -- Synchrony
                      H.div
                      { className: intercalate " "
                          [ "form-group col-4 w-10 text-center mb-0"
                          , (fv.hasError' "synchrony") ?
                              "form-group--error" $
                              mempty
                          ]
                      }
                      [
                        H.div
                        { className: "form-group__label" }
                        [
                          H.label {} [ H.text "Synchrony" ]
                        ]
                      ,
                        H.div
                        { className: "form-group__field" }
                        [
                          B.formInput $
                          { type: "number"
                          , step: "0.1"
                          , min: "0.1"
                          , max: "1"
                          } `merge` bindStateKey "synchrony"
                        ,
                          R2.when (fv.hasError' "synchrony") $
                            H.div
                            { className: "form-group__error" }
                            [
                              H.text "Please enter a `Double` value (eg. 0.5)"
                            ]
                        ]
                      ]
                    ,
                      -- Minimum branch size
                      H.div
                      { className: intercalate " "
                          [ "form-group col-5 w-10 text-center mb-0 text-nowrap"
                          , (fv.hasError' "exportFilter") ?
                              "form-group--error" $
                              mempty
                          ]
                      }
                      [
                        H.div
                        { className: "form-group__label" }
                        [
                          H.label {} [ H.text "Minimum branch size" ]
                        ]
                      ,
                        H.div
                        { className: "form-group__field" }
                        [
                          B.formInput $
                          { type: "number"
                          } `merge` bindStateKey "exportFilter"
                        ,
                          R2.when (fv.hasError' "exportFilter") $
                            H.div
                            { className: "form-group__error" }
                            [
                              H.text "Please enter a `Double` value (eg. 3.0)"
                            ]
                        ]
                      ]
                      
                    ]
                ]
              ]
            ]
          ]
        ]





    -- let
    --   form = 
    --     H.form
    --     { className: "phylo-config-form" }
    --     [ H.div
    --       { className: "phylo-config-form__group" }
    --       [
    --         H.div
    --         { className: "phylo-config-form__row" }
    --         [
    --           H.div
    --           { className: "phylo-config-form__col" }
    --           [
    --             -- Proximity
    --             H.div
    --             { className: intercalate " "
    --                 [ "form-group"
    --                 , (fv.hasError' "proximity") ?
    --                     "form-group--error" $
    --                     mempty
    --                 ]
    --             }
    --             [
    --               H.div
    --               { className: "form-group__label" }
    --               [
    --                 H.label {} [ H.text "Proximity" ]
    --               ]
    --             ,
    --               H.div
    --               { className: "form-group__field" }
    --               [
    --                 B.formInput $
    --                 { type: "number"
    --                 } `merge` bindStateKey "proximity"
    --               ,
    --                 R2.when (fv.hasError' "proximity") $
    --                   H.div
    --                   { className: "form-group__error" }
    --                   [
    --                     H.text "Please enter a `Double` value (eg. 0.5)"
    --                   ]
    --               ]
    --             ]
    --           ]
    --         ,
    --           H.div
    --           { className: "phylo-config-form__col" }
    --           [
    --             -- Synchrony
    --             H.div
    --             { className: intercalate " "
    --                 [ "form-group"
    --                 , (fv.hasError' "synchrony") ?
    --                     "form-group--error" $
    --                     mempty
    --                 ]
    --             }
    --             [
    --               H.div
    --               { className: "form-group__label" }
    --               [
    --                 H.label {} [ H.text "Synchrony" ]
    --               ]
    --             ,
    --               H.div
    --               { className: "form-group__field" }
    --               [
    --                 B.formInput $
    --                 { type: "number"
    --                 } `merge` bindStateKey "synchrony"
    --               ,
    --                 R2.when (fv.hasError' "synchrony") $
    --                   H.div
    --                   { className: "form-group__error" }
    --                   [
    --                     H.text "Please enter a `Double` value (eg. 0.5)"
    --                   ]
    --               ]
    --             ]
    --           ]
    --         ]
    --       ,
    --         H.div
    --         { className: "phylo-config-form__row" }
    --         [
    --           H.div
    --           { className: "phylo-config-form__col" }
    --           [
    --             -- Quality
    --             H.div
    --             { className: intercalate " "
    --                 [ "form-group"
    --                 , (fv.hasError' "quality") ?
    --                     "form-group--error" $
    --                     mempty
    --                 ]
    --             }
    --             [
    --               H.div
    --               { className: "form-group__label" }
    --               [
    --                 H.label {} [ H.text "Quality" ]
    --               ]
    --             ,
    --               H.div
    --               { className: "form-group__field" }
    --               [
    --                 B.formInput $
    --                 { type: "number"
    --                 , step: "0.1"
    --                 , min: "0"
    --                 , max: "1"
    --                 } `merge` bindStateKey "quality"
    --               ,
    --                 R2.when (fv.hasError' "quality") $
    --                   H.div
    --                   { className: "form-group__error" }
    --                   [
    --                     H.text "Please enter a `Double` value (eg. 0.5)"
    --                   ]
    --               ]
    --             ]
    --           ]
    --         ,
    --           H.div
    --           { className: "phylo-config-form__col" }
    --           [
    --             -- Export filter
    --             H.div
    --             { className: intercalate " "
    --                 [ "form-group"
    --                 , (fv.hasError' "exportFilter") ?
    --                     "form-group--error" $
    --                     mempty
    --                 ]
    --             }
    --             [
    --               H.div
    --               { className: "form-group__label" }
    --               [
    --                 H.label {} [ H.text "Minimum branch size" ]
    --               ]
    --             ,
    --               H.div
    --               { className: "form-group__field" }
    --               [
    --                 B.formInput $
    --                 { type: "number"
    --                 } `merge` bindStateKey "exportFilter"
    --               ,
    --                 R2.when (fv.hasError' "exportFilter") $
    --                   H.div
    --                   { className: "form-group__error" }
    --                   [
    --                     H.text "Please enter a `Double` value (eg. 3.0)"
    --                   ]
    --               ]
    --             ]
    --           ]
    --         ]
    --       ]
    --     ,
    --       -- Time Unit
    --       B.fieldset
    --       { className: "phylo-config-form__group"
    --       , titleSlot: H.text "Time unit"
    --       }
    --       [
    --         H.div
    --         { className: "phylo-config-form__row" }
    --         [
    --           H.div
    --           { className: "phylo-config-form__col" }
    --           [
    --             -- Granularity
    --             H.div
    --             { className: intercalate " "
    --                 [ "form-group"
    --                 ]
    --             }
    --             [
    --               H.div
    --               { className: "form-group__label" }
    --               [
    --                 H.label {} [ H.text "Granularity" ]
    --               ]
    --             ,
    --               H.div
    --               { className: "form-group__field" }
    --               [
    --                 B.formSelect
    --                 (bindStateKey "granularity")
    --                 [
    --                   H.option
    --                   { value: show Year_ }
    --                   [ H.text "Year" ]
    --                 ,
    --                   H.option
    --                   { value: show Month_ }
    --                   [ H.text "Month" ]
    --                 ,
    --                   H.option
    --                   { value: show Week_ }
    --                   [ H.text "Week" ]
    --                 ,
    --                   H.option
    --                   { value: show Day_ }
    --                   [ H.text "Day" ]
    --                 ]
    --               ]
    --             ]
    --           ]
    --         ,
    --           H.div
    --           { className: "phylo-config-form__col" }
    --           [
    --             -- Period
    --             H.div
    --             { className: intercalate " "
    --                 [ "form-group"
    --                 , (fv.hasError' "period") ?
    --                     "form-group--error" $
    --                     mempty
    --                 ]
    --             }
    --             [
    --               H.div
    --               { className: "form-group__label" }
    --               [
    --                 H.label {} [ H.text "Period" ]
    --               ]
    --             ,
    --               H.div
    --               { className: "form-group__field" }
    --               [
    --                 B.formInput $
    --                 { type: "number"
    --                 } `merge` bindStateKey "period"
    --               ,
    --                 R2.when (fv.hasError' "period") $
    --                   H.div
    --                   { className: "form-group__error" }
    --                   [
    --                     H.text "Please enter an `Int` value (eg. 3)"
    --                   ]
    --               ]
    --             ]
    --           ,
    --             -- Step
    --             H.div
    --             { className: intercalate " "
    --                 [ "form-group"
    --                 , (fv.hasError' "step") ?
    --                     "form-group--error" $
    --                     mempty
    --                 ]
    --             }
    --             [
    --               H.div
    --               { className: "form-group__label" }
    --               [
    --                 H.label {} [ H.text "Step" ]
    --               ]
    --             ,
    --               H.div
    --               { className: "form-group__field" }
    --               [
    --                 B.formInput $
    --                 { type: "number"
    --                 } `merge` bindStateKey "step"
    --               ,
    --                 R2.when (fv.hasError' "step") $
    --                   H.div
    --                   { className: "form-group__error" }
    --                   [
    --                     H.text "Please enter an `Int` value (eg. 3)"
    --                   ]
    --               ]
    --             ]
    --           ,
    --             -- Matching frame
    --             H.div
    --             { className: intercalate " "
    --                 [ "form-group"
    --                 , (fv.hasError' "matchingFrame") ?
    --                     "form-group--error" $
    --                     mempty
    --                 ]
    --             }
    --             [
    --               H.div
    --               { className: "form-group__label" }
    --               [
    --                 H.label {} [ H.text "Matching frame" ]
    --               ]
    --             ,
    --               H.div
    --               { className: "form-group__field" }
    --               [
    --                 B.formInput $
    --                 { type: "number"
    --                 } `merge` bindStateKey "matchingFrame"
    --               ,
    --                 R2.when (fv.hasError' "matchingFrame") $
    --                   H.div
    --                   { className: "form-group__error" }
    --                   [
    --                     H.text "Please enter an `Int` value (eg. 3)"
    --                   ]
    --               ]
    --             ]
    --           ]
    --         ]
    --       ]
    --     ,
    --       -- Clique
    --       B.fieldset
    --       { className: "phylo-config-form__group"
    --       , titleSlot: H.text "Clique algorithm"
    --       }
    --       [
    --         H.div
    --         { className: "phylo-config-form__row" }
    --         [
    --           H.div
    --           { className: "phylo-config-form__col" }
    --           [
    --             -- Clique type
    --             H.div
    --             { className: intercalate " "
    --                 [ "form-group"
    --                 ]
    --             }
    --             [
    --               H.div
    --               { className: "form-group__label" }
    --               [
    --                 H.label {} [ H.text "Type" ]
    --               ]
    --             ,
    --               H.div
    --               { className: "form-group__field" }
    --               [
    --                 H.div
    --                 { className: "btn-group"
    --                 , role: "group"
    --                 }
    --                 [
    --                   B.button
    --                   { callback: \_ -> setter stateBox "cliqueType" $ show FIS_
    --                   -- , variant: OutlinedButtonVariant Secondary
    --                   , variant: ButtonVariant Light
    --                   , className: state.cliqueType == show FIS_ ?
    --                       "active" $
    --                       ""
    --                   }
    --                   [
    --                     H.text "FIS"
    --                   ]
    --                 ,
    --                   B.button
    --                   { callback: \_ -> setter stateBox "cliqueType" $ show MaxClique_
    --                   -- , variant: OutlinedButtonVariant Secondary
    --                   , variant: ButtonVariant Light
    --                   , className: state.cliqueType == show MaxClique_ ?
    --                       "active" $
    --                       ""
    --                   }
    --                   [
    --                     H.text "MaxClique"
    --                   ]
    --                 ]
    --               ]
    --             ]
    --           ]
    --         ,
    --           -- TYPE::FIS_
    --           R2.when (state.cliqueType == show FIS_) $

    --             H.div
    --             { className: "phylo-config-form__col" }
    --             [
    --               -- Support
    --               H.div
    --               { className: intercalate " "
    --                   [ "form-group"
    --                   , (fv.hasError' "support") ?
    --                       "form-group--error" $
    --                       mempty
    --                   ]
    --               }
    --               [
    --                 H.div
    --                 { className: "form-group__label" }
    --                 [
    --                   H.label {} [ H.text "Support" ]
    --                 ]
    --               ,
    --                 H.div
    --                 { className: "form-group__field" }
    --                 [
    --                   B.formInput $
    --                     bindStateKey "support"
    --                 ,
    --                   R2.when (fv.hasError' "support") $
    --                     H.div
    --                     { className: "form-group__error" }
    --                     [
    --                       H.text "Please enter an `Int` value (eg. 3)"
    --                     ]
    --                 ]
    --               ]
    --             ,
    --               -- Size
    --               H.div
    --               { className: intercalate " "
    --                   [ "form-group"
    --                   , (fv.hasError' "size") ?
    --                       "form-group--error" $
    --                       mempty
    --                   ]
    --               }
    --               [
    --                 H.div
    --                 { className: "form-group__label" }
    --                 [
    --                   H.label {} [ H.text "Size" ]
    --                 ]
    --               ,
    --                 H.div
    --                 { className: "form-group__field" }
    --                 [
    --                   B.formInput $
    --                     bindStateKey "size"
    --                 ,
    --                   R2.when (fv.hasError' "size") $
    --                     H.div
    --                     { className: "form-group__error" }
    --                     [
    --                       H.text "Please enter an `Int` value (eg. 3)"
    --                     ]
    --                 ]
    --               ]
    --             ]
    --         ,
    --           -- TYPE::MaxClique_
    --           R2.when (state.cliqueType == show MaxClique_) $

    --             H.div
    --             { className: "phylo-config-form__col" }
    --             [
    --               -- Size
    --               H.div
    --               { className: intercalate " "
    --                   [ "form-group"
    --                   , (fv.hasError' "size") ?
    --                       "form-group--error" $
    --                       mempty
    --                   ]
    --               }
    --               [
    --                 H.div
    --                 { className: "form-group__label" }
    --                 [
    --                   H.label {} [ H.text "Size" ]
    --                 ]
    --               ,
    --                 H.div
    --                 { className: "form-group__field" }
    --                 [
    --                   B.formInput $
    --                   { type: "number"
    --                   } `merge` bindStateKey "size"
    --                 ,
    --                   R2.when (fv.hasError' "size") $
    --                     H.div
    --                     { className: "form-group__error" }
    --                     [
    --                       H.text "Please enter an `Int` value (eg. 3)"
    --                     ]
    --                 ]
    --               ]
    --             ,
    --               -- Treshold
    --               H.div
    --               { className: intercalate " "
    --                   [ "form-group"
    --                   , (fv.hasError' "threshold") ?
    --                       "form-group--error" $
    --                       mempty
    --                   ]
    --               }
    --               [
    --                 H.div
    --                 { className: "form-group__label" }
    --                 [
    --                   H.label {} [ H.text "Treshold" ]
    --                 ]
    --               ,
    --                 H.div
    --                 { className: "form-group__field" }
    --                 [
    --                   B.formInput $
    --                   { type: "number"
    --                   } `merge` bindStateKey "threshold"
    --                 ,
    --                   R2.when (fv.hasError' "threshold") $
    --                     H.div
    --                     { className: "form-group__error" }
    --                     [
    --                       H.text "Please enter a `Double` value (eg. 0.5)"
    --                     ]
    --                 ]
    --               ]
    --             ,
    --               -- Clique filter
    --               H.div
    --               { className: intercalate " "
    --                   [ "form-group"
    --                   ]
    --               }
    --               [
    --                 H.div
    --                 { className: "form-group__label" }
    --                 [
    --                   H.label {} [ H.text "Filter type" ]
    --                 ]
    --               ,
    --                 H.div
    --                 { className: "form-group__field" }
    --                 [
    --                   B.formSelect
    --                   ( bindStateKey "cliqueFilter" )
    --                   [
    --                     H.option
    --                     { value: show ByThreshold }
    --                     [ H.text "By threshold" ]
    --                   ,
    --                     H.option
    --                     { value: show ByNeighbours }
    --                     [ H.text "By neighbours" ]
    --                   ]
    --                 ]
    --               ]
    --             ]
    --         ]
    --       ]
    --     ]

    let warningMessage =
          H.div { className: "alert alert-info text-small mx-auto mt-1 w-min-content min-w-75" }
          [
            H.p
            { className: "text-bold text-small" }
            [
              B.icon
              { name: "warning"
              , className: "mr-1"
              }
            ,
              H.text "Warning!"
            ]
          ,
            H.p { className: "text-small" }
            [
              H.text "Please note that the phylomemy reconstruction process consumes a lot of machine resources. If your corpus is < 2000 docs and if your list of terms is < 300 terms then the process will end quickly. If your corpus is < 5000 docs and if your list of terms is < 500 terms then the process will end in less than 30 minutes. If your corpus is > 5000 docs and if your list of terms is > 500 terms then the process can last hours."
            ] 
          ]

    let submit = 
          H.div { className: "phylo-config-form__submit" }
          [
            B.button
            { callback: \_ -> onSubmit
            , status: props.status == Deferred ? Deferred $ Enabled
            , variant: ButtonVariant Primary
            , type: "submit"
            }
            [
              B.icon { name: "cogs" }
            ,
              H.text $ nbsp 1
            ,
              H.text "Reconstruct the phylomemy!"
            ]
          ]

    pure $ H.div {} [ modeChoice
                    , R2.when (state.defaultMode == show false) formAdvanced
                    -- , R2.when (state.defaultMode == show false) form
                    , warningMessage
                    , submit
                    ]

type FormData =
  ( defaultMode       :: String
  , proximity         :: String
  , synchrony         :: String
  , quality           :: String
  , exportFilter      :: String
  -- TimeUnit
  , granularity       :: String
  , period            :: String
  , step              :: String
  , matchingFrame     :: String
  -- Clique
  , clusterAlgoMode   :: String
  , cliqueType        :: String
  , support           :: String
  , size              :: String
  , threshold         :: String
  , cliqueFilter      :: String
  )

defaultData :: Record FormData
defaultData =
  { defaultMode     : show true
  , proximity       : "1.0"
  , synchrony       : "1.0"
  , quality         : "1.0"
  , exportFilter    : "3.0"
  , granularity     : show Year_
  , period          : "1"
  , step            : "1"
  , matchingFrame   : "1"
  , clusterAlgoMode : show Basic_
  , cliqueType      : show FIS_
  , support         : "2"
  , size            : "1"
  , threshold       : "1"
  , cliqueFilter    : show ByThreshold
  }

formValidation :: Record FormData -> Effect VForm
formValidation r = foldl append mempty rules
  where
    rules
       = [ FV.number "quality" r.quality
         -- Time unit
         , FV.int "period" r.period
         , FV.int "step" r.step
         , FV.int "matchingFrame" r.matchingFrame
         ]
         -- Clique
      <> if (r.clusterAlgoMode == show Basic_)
         then
            -- Basic_ (Basic mode)
            [ 
            --   FV.int "support" r.support
            -- , FV.int "size" r.size
            ]
          else
            -- Expert_ (Expert mode)
            [ 
            --   FV.int "size" r.size
            -- , FV.number "threshold" r.threshold
            ]
      <> if (r.cliqueType == show FIS_)
         then
            -- FIS_ (Basic mode)
            [ FV.int "support" r.support
            , FV.int "size" r.size
            ]
          else
            -- MaxClique_ (Expert mode)
            [ FV.int "size" r.size
            , FV.number "threshold" r.threshold
            ]
      <>
         [ FV.number "proximity" r.proximity
         , FV.number "synchrony" r.synchrony
         , FV.number "exportFilter" r.exportFilter
         ]
