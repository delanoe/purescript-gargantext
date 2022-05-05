module Gargantext.Components.PhyloExplorer.ConfigForm
  ( configForm
  , FormData
  ) where

import Gargantext.Prelude

import DOM.Simple.Console (log3)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Components.PhyloExplorer.API (CliqueFilter(..), ReflexiveClique(..), ReflexiveTimeUnit(..))
import Gargantext.Hooks.FormValidation (VForm, useFormValidation)
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

  -- Render

    pure $

      H.form
      { className: "phylo-config-form" }
      [
        H.div
        { className: "phylo-config-form__group" }
        [
          H.div
          { className: "phylo-config-form__row" }
          [
            H.div
            { className: "phylo-config-form__col" }
            [
              -- Proximity
              H.div
              { className: intercalate " "
                  [ "form-group"
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
            ]
          ,
            H.div
            { className: "phylo-config-form__col" }
            [
              -- Synchrony
              H.div
              { className: intercalate " "
                  [ "form-group"
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
            ]
          ]
        ,
          H.div
          { className: "phylo-config-form__row" }
          [
            H.div
            { className: "phylo-config-form__col" }
            [
              -- Quality
              H.div
              { className: intercalate " "
                  [ "form-group"
                  , (fv.hasError' "quality") ?
                      "form-group--error" $
                      mempty
                  ]
              }
              [
                H.div
                { className: "form-group__label" }
                [
                  H.label {} [ H.text "Quality" ]
                ]
              ,
                H.div
                { className: "form-group__field" }
                [
                  B.formInput $
                  { type: "number"
                  } `merge` bindStateKey "quality"
                ,
                  R2.when (fv.hasError' "quality") $
                    H.div
                    { className: "form-group__error" }
                    [
                      H.text "Please enter a `Double` value (eg. 0.5)"
                    ]
                ]
              ]
            ]
          ,
            H.div
            { className: "phylo-config-form__col" }
            [
              -- Export filter
              H.div
              { className: intercalate " "
                  [ "form-group"
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
      ,
        -- Time Unit
        B.fieldset
        { className: "phylo-config-form__group"
        , titleSlot: H.text "Time unit"
        }
        [
          H.div
          { className: "phylo-config-form__row" }
          [
            H.div
            { className: "phylo-config-form__col" }
            [
              -- Granularity
              H.div
              { className: intercalate " "
                  [ "form-group"
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
            { className: "phylo-config-form__col" }
            [
              -- Period
              H.div
              { className: intercalate " "
                  [ "form-group"
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
            ,
              -- Step
              H.div
              { className: intercalate " "
                  [ "form-group"
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
            ,
              -- Matching frame
              H.div
              { className: intercalate " "
                  [ "form-group"
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
        -- Clique
        B.fieldset
        { className: "phylo-config-form__group"
        , titleSlot: H.text "Clique algorithm"
        }
        [
          H.div
          { className: "phylo-config-form__row" }
          [
            H.div
            { className: "phylo-config-form__col" }
            [
              -- Clique type
              H.div
              { className: intercalate " "
                  [ "form-group"
                  ]
              }
              [
                H.div
                { className: "form-group__label" }
                [
                  H.label {} [ H.text "Type" ]
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
              ]
            ]
          ,
            -- TYPE::FIS_
            R2.when (state.cliqueType == show FIS_) $

              H.div
              { className: "phylo-config-form__col" }
              [
                -- Support
                H.div
                { className: intercalate " "
                    [ "form-group"
                    , (fv.hasError' "support") ?
                        "form-group--error" $
                        mempty
                    ]
                }
                [
                  H.div
                  { className: "form-group__label" }
                  [
                    H.label {} [ H.text "Support" ]
                  ]
                ,
                  H.div
                  { className: "form-group__field" }
                  [
                    B.formInput $
                      bindStateKey "support"
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
                    [ "form-group"
                    , (fv.hasError' "size") ?
                        "form-group--error" $
                        mempty
                    ]
                }
                [
                  H.div
                  { className: "form-group__label" }
                  [
                    H.label {} [ H.text "Size" ]
                  ]
                ,
                  H.div
                  { className: "form-group__field" }
                  [
                    B.formInput $
                      bindStateKey "size"
                  ,
                    R2.when (fv.hasError' "sjze") $
                      H.div
                      { className: "form-group__error" }
                      [
                        H.text "Please enter an `Int` value (eg. 3)"
                      ]
                  ]
                ]
              ]
          ,
            -- TYPE::MaxClique_
            R2.when (state.cliqueType == show MaxClique_) $

              H.div
              { className: "phylo-config-form__col" }
              [
                -- Size
                H.div
                { className: intercalate " "
                    [ "form-group"
                    , (fv.hasError' "size") ?
                        "form-group--error" $
                        mempty
                    ]
                }
                [
                  H.div
                  { className: "form-group__label" }
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
              ,
                -- Treshold
                H.div
                { className: intercalate " "
                    [ "form-group"
                    , (fv.hasError' "threshold") ?
                        "form-group--error" $
                        mempty
                    ]
                }
                [
                  H.div
                  { className: "form-group__label" }
                  [
                    H.label {} [ H.text "Treshold" ]
                  ]
                ,
                  H.div
                  { className: "form-group__field" }
                  [
                    B.formInput $
                    { type: "number"
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
                    [ "form-group"
                    ]
                }
                [
                  H.div
                  { className: "form-group__label" }
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
      ,
        -- Submit
        H.div { className: "phylo-config-form__submit" }
        [
          B.button
          { callback: \_ -> onSubmit
          , status: props.status == Deferred ? Deferred $ Enabled
          , variant: ButtonVariant Primary
          , type: "submit"
          }
          [
            B.icon { name: "refresh" }
          ,
            H.text $ nbsp 1
          ,
            H.text "Update!"
          ]
        ]
      ]


type FormData =
  ( proximity     :: String
  , synchrony     :: String
  , quality       :: String
  , exportFilter  :: String
  -- TimeUnit
  , granularity   :: String
  , period        :: String
  , step          :: String
  , matchingFrame :: String
  -- Clique
  , cliqueType    :: String
  , support       :: String
  , size          :: String
  , threshold     :: String
  , cliqueFilter  :: String
  )

defaultData :: Record FormData
defaultData =
  { proximity     : "1.0"
  , synchrony     : "1.0"
  , quality       : "1.0"
  , exportFilter  : "3.0"
  , granularity   : show Year_
  , period        : "1"
  , step          : "1"
  , matchingFrame : "1"
  , cliqueType    : show FIS_
  , support       : "1"
  , size          : "1"
  , threshold     : "1"
  , cliqueFilter  : show ByThreshold
  }

formValidation :: Record FormData -> Effect VForm
formValidation r = foldl append mempty rules
  where
    rules
       = [ FV.number "proximity" r.proximity
         , FV.number "synchrony" r.synchrony
         , FV.number "quality" r.quality
         , FV.number "exportFilter" r.exportFilter
         -- Time unit
         , FV.int "period" r.period
         , FV.int "step" r.step
         , FV.int "matchingFrame" r.matchingFrame
         ]
         -- Clique
      <> if (r.cliqueType == show FIS_)
         then
            [ FV.int "support" r.support
            , FV.int "size" r.size
            ]
          else
            [ FV.int "size" r.size
            , FV.number "threshold" r.threshold
            ]
