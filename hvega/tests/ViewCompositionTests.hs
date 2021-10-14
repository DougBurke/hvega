{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ViewCompositionTests.elm as of version 1.12.0
--
module ViewCompositionTests (testSpecs) where

import qualified Data.Text as T
import qualified Prelude as P

import Graphics.Vega.VegaLite

import Prelude hiding (filter, repeat)

import Data.Aeson (Value(Object))
#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.KeyMap (empty)
#else
import Data.HashMap.Strict (empty)
#endif


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("columns1", columns1)
            , ("columns2", columns2)
            , ("columns3", columns3)
            , ("columns4", columns4)
            , ("lorientrighthdr", lorientRightHdr)
            , ("lorientrightcnf", lorientRightCnf)
            , ("torientrighthdr", torientRightHdr)
            , ("torientrightcnf", torientRightCnf)
            , ("orientrighthdr", orientRightHdr)
            , ("orientrightcnf", orientRightCnf)
            , ("lorientbottomhdr", lorientBottomHdr)
            , ("lorientbottomcnf", lorientBottomCnf)
            , ("torientbottomhdr", torientBottomHdr)
            , ("torientbottomcnf", torientBottomCnf)
            , ("orientbottomhdr", orientBottomHdr)
            , ("orientbottomcnf", orientBottomCnf)
            , ("groupyage", groupByAge)
            , ("grid1", grid1)
            , ("grid2", grid2)
            , ("grid3", grid3)
            , ("grid4", grid4)
            , ("grid5", grid5)
            , ("repeatinglayers", repeatinglayers)
            , ("highlightvalue", highlightvalue)
            , ("highlightDateTime", highlightDateTime)
            ]


noStroke :: [ConfigureSpec] -> PropertySpec
noStroke = configure
           . configuration (ViewStyle [ ViewNoStroke ])


genderChart :: [HeaderProperty] -> [HeaderProperty] -> VegaLite
genderChart hdProps cProps =
  let conf = configure . configuration (HeaderStyle cProps)

      pop =
          dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []

      trans =
          transform
              . filter (FExpr "datum.year == 2000")
              . calculateAs "datum.sex == 2 ? 'Female' : 'Male'" "gender"

      enc =
          encoding
              . column
                  [ FName "gender"
                  , FmType Nominal
                  , FHeader hdProps
                  , FSpacing 0
                  ]
              . position X
                  [ PName "age"
                  , PmType Ordinal
                  ]
              . position Y
                  [ PName "people"
                  , PmType Quantitative
                  , PAggregate Sum
                  , PAxis [ AxTitle "Population" ]
                  ]
              . color
                  [ MName "gender"
                  , MmType Nominal
                  , MScale [ SRange (RStrings [ "#CC9933", "#3399CC" ]) ]
                  ]

  in toVegaLite [ conf [], pop, trans [], enc [], mark Bar [], widthStep 17 ]


columns1, columns2, columns3, columns4 :: VegaLite
columns1 = genderChart [] []
columns2 = genderChart [ HTitleFontSize 20, HLabelFontSize 15 ] []
columns3 = genderChart [] [ HTitleFontSize 20, HLabelFontSize 15 ]
columns4 =
    genderChart
        [ HTitleFontSize 20
        , HTitleFontWeight Normal
        , HTitlePadding (-27)
        , HLabelBaseline AlignLineBottom
        , HLabelFontSize 15
        , HLabelFontWeight Bold
        , HLabelLineHeight 14 -- not used here, but set it anyway
        , HLabelPadding 40
        ]
        []


lorientRightHdr, lorientRightCnf :: VegaLite
lorientRightHdr = genderChart [ HLabelOrient SRight ] []
lorientRightCnf = genderChart [] [ HLabelOrient SRight ]

torientRightHdr, torientRightCnf :: VegaLite
torientRightHdr = genderChart [ HTitleOrient SRight ] []
torientRightCnf = genderChart [] [ HTitleOrient SRight ]

orientRightHdr, orientRightCnf :: VegaLite
orientRightHdr = genderChart [ HOrient SRight ] []
orientRightCnf = genderChart [] [ HOrient SRight ]

lorientBottomHdr, lorientBottomCnf :: VegaLite
lorientBottomHdr = genderChart [ HLabelOrient SBottom ] []
lorientBottomCnf = genderChart [] [ HLabelOrient SBottom ]

torientBottomHdr, torientBottomCnf :: VegaLite
torientBottomHdr = genderChart [ HTitleOrient SBottom ] []
torientBottomCnf = genderChart [] [ HTitleOrient SBottom ]

orientBottomHdr, orientBottomCnf :: VegaLite
orientBottomHdr = genderChart [ HOrient SBottom ] []
orientBottomCnf = genderChart [] [ HOrient SBottom ]



groupByAge :: VegaLite
groupByAge =
  let conf = noStroke
             . configuration (Axis [ DomainWidth 1 ] )

      pop = dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []

      trans =
          transform
              . filter (FExpr "datum.year == 2000")
              . calculateAs "datum.sex == 2 ? 'Female' : 'Male'" "gender"

      enc =
          encoding
              . column
                  [ FName "age"
                  , FmType Ordinal
                  , FSpacing 10
                  ]
              . position Y
                  [ PName "people"
                  , PmType Quantitative
                  , PAggregate Sum
                  , PAxis [ AxTitle "Population", AxGrid False ]
                  ]
              . position X
                  [ PName "gender"
                  , PmType Nominal
                  , PAxis [ AxNoTitle ]
                  ]
              . color
                  [ MName "gender"
                  , MmType Nominal
                  , MScale [ SRange (RStrings [ "#675193", "#ca8861" ]) ]
                  ]

  in toVegaLite [ conf []
                , pop
                , trans []
                , widthStep 12
                , mark Bar []
                , enc []
                ]


dataVals :: [DataColumn] -> Data
dataVals =
    let
        rows =
            Numbers $ concatMap (P.replicate (3 * 5)) [ 1, 2, 3, 4 ]

        cols =
            Numbers $ concat $ P.replicate 4 $ concatMap (P.replicate 3) [ 1, 2, 3, 4, 5 ]

        cats =
            Numbers $ concat $ P.replicate (4 * 5) [ 1, 2, 3 ]

        vals =
            Numbers $ [ 30, 15, 12, 25, 30, 25, 10, 28, 11, 18, 24, 16, 10, 10, 10 ]
                ++ [ 8, 8, 29, 11, 24, 12, 26, 32, 9, 8, 18, 28, 8, 20, 24 ]
                ++ [ 21, 15, 20, 4, 13, 12, 27, 21, 14, 5, 1, 2, 11, 2, 5 ]
                ++ [ 14, 20, 24, 20, 2, 9, 15, 14, 13, 22, 30, 30, 10, 8, 12 ]

    in
    dataFromColumns []
        . dataColumn "row" rows
        . dataColumn "col" cols
        . dataColumn "cat" cats
        . dataColumn "val" vals


encByCatVal :: [EncodingSpec] -> PropertySpec
encByCatVal = encoding
              . position X [ PName "cat", PmType Ordinal ]
              . position Y [ PName "val", PmType Quantitative ]
              . color [ MName "cat", MmType Nominal, MLegend [] ]

specByCatVal :: VLSpec
specByCatVal = asSpec [ width 120, height 120, mark Bar [], encByCatVal [] ]

gridTransform :: PropertySpec
gridTransform = transform
                (calculateAs "datum.row * 1000 + datum.col" "index" [])

gridConfig :: [CompositionConfig] -> [ConfigureSpec] -> PropertySpec
gridConfig fopts =
  configure
  . configuration (HeaderStyle [ HLabelFontSize 0.1 ])
  . configuration (ViewStyle [ ViewStroke "black"
                             , ViewStrokeWidth 2
                             , ViewFill "gray"
                             , ViewFillOpacity 0.2
                             , ViewContinuousHeight 120 ])
  . configuration (FacetStyle fopts)
  . configuration (Axis [Disable True])

grid1 :: VegaLite
grid1 =
    let cfg = gridConfig [ CompSpacing 80, CompColumns 5 ]

    in
    toVegaLite
        [ cfg []
        , dataVals []
        , spacingRC 10 30
        , specification specByCatVal
        , facet
            [ RowBy [ FName "row", FmType Ordinal, FNoTitle ]
            , ColumnBy [ FName "col", FmType Ordinal, FHeader [ HNoTitle ] ]
            ]
        ]


grid2 :: VegaLite
grid2 =
    let cfg = gridConfig [ CompSpacing 80, CompColumns 5 ]

    in
    toVegaLite
        [ cfg []
        , dataVals []
        , gridTransform
        , columns 5
        , specification specByCatVal
        , facetFlow [ FName "index", FmType Ordinal, FHeader [ HNoTitle ] ]
        ]


grid3 :: VegaLite
grid3 =
    let cfg = gridConfig [ CompSpacing 80 ]

    in
    toVegaLite
        [ cfg []
        , dataVals []
        , gridTransform
        , columns 0
        , specification specByCatVal
        , facetFlow [ FName "index", FmType Ordinal, FHeader [ HNoTitle ] ]
        ]


carGrid :: Arrangement -> [PropertySpec] -> VegaLite
carGrid rpt opts =
  let carData = dataFromUrl "https://vega.github.io/vega-lite/data/cars.json"

      enc = encoding
            . position X [ PRepeat rpt, PmType Quantitative, PBin [] ]
            . position Y [ PmType Quantitative, PAggregate Count ]
            . color [ MName "Origin", MmType Nominal ]

      spec = asSpec [ carData [], mark Bar [], enc [] ]

  in toVegaLite (specification spec : opts)


carFields :: [T.Text]
carFields = [ "Horsepower", "Miles_per_Gallon", "Acceleration", "Displacement", "Weight_in_lbs" ]


grid4 :: VegaLite
grid4 =
  let opts = [ columns 3
             , repeatFlow carFields
             ]

  in carGrid Flow opts


grid5 :: VegaLite
grid5 =
  let opts = [ repeat
               [ RowFields carFields
               ]
             ]

  in carGrid Row opts


repeatinglayers :: VegaLite
repeatinglayers =
  let dvals = dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []

      plot = [ mark Line []
             , encoding
               . position X [PName "IMDB_Rating", PmType Quantitative, PBin []]
               . position Y [ PRepeat Layer
                            , PmType Quantitative
                            , PAggregate Mean
                            , PTitle "Mean of US and Worldwide Gross"
                            ]
               . color [MRepeatDatum Layer, MmType Nominal]
               $ []
             ]

  in toVegaLite [ dvals
                , repeat [LayerFields ["US_Gross", "Worldwide_Gross"]]
                , specification (asSpec plot)
                ]


-- layer_line_datum_rule.vl.json
highlightvalue :: VegaLite
highlightvalue =
  let dvals = dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []
      dummy = dataFromJson emptyData []
      emptyData = Object empty

      plot1 = [ dvals
              , mark Line []
              , encoding
                . position X [PName "date", PmType Temporal]
                . position Y [PName "price", PmType Quantitative]
                . color [MName "symbol", MmType Nominal]
                $ []
              ]

      plot2 = [ dummy
              , mark Rule [MStrokeDash [2, 2], MSize 2]
              , encoding (position Y [PDatum (Number 300)] [])
              ]

  in toVegaLite [ layer [asSpec plot1, asSpec plot2]
                ]


-- layer_line_datum_rule_datetime.vl.json
highlightDateTime :: VegaLite
highlightDateTime =
  let dvals = dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []
      dummy = dataFromJson emptyData []
      emptyData = Object empty

      plot1 = [ dvals
              , mark Line []
              , encoding
                . position X [PName "date", PmType Temporal]
                . position Y [PName "price", PmType Quantitative]
                . color [MName "symbol", MmType Nominal]
                $ []
              ]

      plot2 = [ dummy
              , mark Rule [MStrokeDash [2, 2], MSize 2]
              , encoding (position X [PDatum (DateTime [DTYear 2006])] [])
              ]

  in toVegaLite [ layer [asSpec plot1, asSpec plot2]
                ]
