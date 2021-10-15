{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Based on the Elm VegaLite GalleryLabel.elm (from development of version
-- 1.13.0)
--
module Gallery.Label (testSpecs) where

import Data.Aeson (Value(Object))
import Data.Aeson.QQ.Simple (aesonQQ)
#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.KeyMap (empty)
#else
import Data.HashMap.Strict (empty)
#endif

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("label1", label1)
            , ("label2", label2)
            , ("label2r", label2r)
            , ("label3", label3)
            , ("label4", label4)
            , ("label5", label5)
            , ("label6", label6)
            , ("label7", label7)
            , ("label8", label8)
            , ("label9", label9)
            , ("voyager", voyager)
            , ("baselines", baselines)
            ]


noStroke :: [ConfigureSpec] -> PropertySpec
noStroke = configure . configuration (ViewStyle [ViewNoStroke])


label1 :: VegaLite
label1 =
    let
        des =
            description "A simple bar chart with embedded data labels"

        dvals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C" ])
                . dataColumn "b" (Numbers [ 28, 55, 43 ])

        enc =
            encoding
                . position X [ PName "b", PmType Quantitative ]
                . position Y [ PName "a", PmType Ordinal ]

        specBar =
            asSpec [ mark Bar [] ]

        specText =
            asSpec [ mark Text [ MStyle [ "label" ] ], encoding (text [ TName "b", TmType Quantitative ] []) ]

        config =
            configure . configuration (MarkNamedStyles [( "label"
                                                        , [ MAlign AlignLeft, MBaseline AlignMiddle, MdX 3 ])
                                                       ])

    in
    toVegaLite [ des, dvals [], enc [], layer [ specBar, specText ], config [] ]


label2Base :: [ScaleConfig] -> VegaLite
label2Base opts =
    let encPosition =
            encoding
                . position X [ PName "Cylinders", PmType Ordinal ]
                . position Y [ PName "Origin", PmType Ordinal ]

        encRect =
            encoding
                . color [ MName "*", MmType Quantitative, MAggregate Count ]

        specRect =
            asSpec [ mark Rect [], encRect [] ]

        encText =
            encoding
                . color [ MString "white" ]
                . text [ TName "*", TmType Quantitative, TAggregate Count ]

        specText =
            asSpec [ mark Text [], encText [] ]

        scOpts = [ SCBandPaddingInner 0, SCBandPaddingOuter 0 ] ++ opts
        config =
            configure
                . configuration (ScaleStyle scOpts)
                . configuration (TextStyle [ MBaseline AlignMiddle ])
    in
    toVegaLite
        [ description "Layering text over 'heatmap'"
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , encPosition []
        , layer [ specRect, specText ]
        , config []
        ]


label2, label2r :: VegaLite
label2 = label2Base [SCXReverse False]
label2r = label2Base [SCXReverse True]


-- TODO: this has been re-worked in elm
label3 :: VegaLite
label3 =
    let
        des =
            description "Carbon dioxide in the atmosphere."

        trans =
            transform
                . calculateAs "year(datum.Date)" "year"
                . calculateAs "month(datum.Date)" "month"
                . calculateAs "floor(datum.year / 10)" "decade"
                . calculateAs "(datum.year % 10) + (datum.month / 12)" "scaled_date"

        encPosition =
            encoding
                . position X
                    [ PName "scaled_date"
                    , PmType Quantitative
                    , PAxis [ AxTitle "Year into decade"
                            , AxTickCount 10
                            , AxValues (Numbers [1, 2, 3, 4, 5, 6, 7, 8, 9])
                            ]
                    ]
                . position Y
                    [ PName "CO2"
                    , PmType Quantitative
                    , PScale [ SZero False ]
                    , PAxis [ AxTitle "CO₂ concentration in ppm" ]
                    ]

        encLine =
            encoding
                . color [ MName "decade", MmType Nominal, MLegend [] ]

        specLine =
            asSpec [ mark Line [ MOrient Vertical ], encLine [] ]

        transTextMin =
            transform
                . aggregate [ opAs (ArgMin Nothing) "scaled_date" "aggregated" ] [ "decade" ]
                . calculateAs "datum.aggregated.scaled_date" "scaled_date"
                . calculateAs "datum.aggregated.CO2" "CO2"

        encTextMin =
            encoding
                . text [ TName "aggregated.year", TmType Nominal ]

        specTextMin =
            asSpec [ transTextMin [], mark Text [ MAlign AlignLeft, MBaseline AlignTop, MdX 3, MdY 1 ], encTextMin [] ]

        transTextMax =
            transform
                . aggregate [ opAs (ArgMax Nothing) "scaled_date" "aggregated" ] [ "decade" ]
                . calculateAs "datum.aggregated.scaled_date" "scaled_date"
                . calculateAs "datum.aggregated.CO2" "CO2"

        encTextMax =
            encoding
                . text [ TName "aggregated.year", TmType Nominal ]

        specTextMax =
            asSpec [ transTextMax [], mark Text [ MAlign AlignLeft, MBaseline AlignBottom, MdX 3, MdY 1 ], encTextMax [] ]

    in
    toVegaLite
        [ des
        , noStroke []
        , width 800
        , height 500
        , dataFromUrl "https://vega.github.io/vega-lite/data/co2-concentration.csv" [ Parse [ ( "Date", FoUtc "%Y-%m-%d" ) ] ]
        , trans []
        , encPosition []
        , layer [ specLine, specTextMin, specTextMax ]
        ]


-- https://vega.github.io/vega-lite/examples/layer_bar_annotations.html
label4 :: VegaLite
label4 =
  let des = description "The PM2.5 value of Beijing observed 15 days, highlighting the days when PM2.5 level is hazardous to human health. Data source https://chartaccent.github.io/chartaccent.html"

      dvals = dataFromColumns []
              . dataColumn "Day" (Numbers (map fromIntegral [1::Int .. 15]))
              . dataColumn "Value" (Numbers [54.8, 112.1, 63.6, 37.6, 79.7, 137.9, 120.1, 103.3, 394.8, 199.5, 72.3, 51.1, 112.0, 174.5, 130.5])

      encBar = encoding
               . position X [PName "Day", PmType Ordinal, PAxis [AxLabelAngle 0]]
               . position Y [PName "Value", PmType Quantitative]

      specBar = asSpec [mark Bar [], encBar []]

      trans = transform
              . filter (FExpr "datum.Value >= 300")
              . calculateAs "300" "baseline"

      encUpperBar = encoding
                    . position X [PName "Day", PmType Ordinal]
                    . position Y [PName "baseline", PmType Quantitative, PTitle "PM2.5 Value"]
                    . position Y2 [PName "Value"]
                    . color [MString "#e45755"]

      specUpperBar = asSpec [trans [], mark Bar [], encUpperBar []]

      layer0 = asSpec [dvals [], layer [specBar, specUpperBar]]

      thresholdData = dataFromJson (Object empty) []

      specRule = asSpec [mark Rule []]

      encRule = encoding
                . position Y [PDatum (Number 300)]

      specText = asSpec [mark Text [ MAlign AlignRight
                                   , MBaseline AlignBottom
                                   , MdX (-2)
                                   , MdY (-2)
                                   , MXWidth
                                   , MText "hazardous"
                                   ]
                        ]

      layer1 = asSpec [thresholdData, encRule [], layer [specRule, specText]]

  in toVegaLite [ des, layer [ layer0, layer1 ] ]


{-
{
  "description": ,
    ]}, {
      "data": {
         "values": [{}]
      },
      "encoding": {
        "y": {"datum": 300}
      },
      "layer": [{
        "mark": "rule"
      }, {
        "mark": {
          "type": "text",
          "align": "right",
          "baseline": "bottom",
          "dx": -2,
          "dy": -2,
          "x": "width",
          "text": "hazardous"
        }
      }]
    }
  ]
}
-}

label5 :: VegaLite
label5 =
    let
        des =
            description "Monthly precipitation with mean value overlay"

        encBar =
            encoding
                . position X [ PName "date", PmType Ordinal, PTimeUnit (TU Month) ]
                . position Y [ PName "precipitation", PmType Quantitative, PAggregate Mean ]

        specBar =
            asSpec [ mark Bar [], encBar [] ]

        encLine =
            encoding
                . position Y [ PName "precipitation", PmType Quantitative, PAggregate Mean ]
                . color [ MString "red" ]
                . size [ MNumber 3 ]

        specLine =
            asSpec [ mark Rule [], encLine [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []
        , layer [ specBar, specLine ]
        ]


label6 :: VegaLite
label6 =
    let
        des =
            description "Histogram with global mean overlay"

        encBars =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PBin [], PAxis [] ]
                . position Y [ PmType Quantitative, PAggregate Count ]

        specBars =
            asSpec [ mark Bar [], encBars [] ]

        encMean =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PAggregate Mean ]
                . color [ MString "red" ]
                . size [ MNumber 5 ]

        specMean =
            asSpec [ mark Rule [], encMean [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , layer [ specBars, specMean ]
        ]


label7 :: VegaLite
label7 =
    let
        des =
            description "The population of the German city of Falkensee over time with annotated time periods highlighted"

        dvals =
            dataFromColumns [ Parse [ ( "year", FoDate "%Y" ) ] ]
                . dataColumn "year" (Strings [ "1875", "1890", "1910", "1925", "1933", "1939", "1946", "1950", "1964", "1971", "1981", "1985", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014" ])
                . dataColumn "population" (Numbers [ 1309, 1558, 4512, 8180, 15915, 24824, 28275, 29189, 29881, 26007, 24029, 23340, 22307, 22087, 22139, 22105, 22242, 22801, 24273, 25640, 27393, 29505, 32124, 33791, 35297, 36179, 36829, 37493, 38376, 39008, 39366, 39821, 40179, 40511, 40465, 40905, 41258, 41777 ])

        highlights =
            dataFromColumns [ Parse [ ( "start", FoDate "%Y" ), ( "end", FoDate "%Y" ) ] ]
                . dataColumn "start" (Strings [ "1933", "1948" ])
                . dataColumn "end" (Strings [ "1945", "1989" ])
                . dataColumn "event" (Strings [ "Nazi Rule", "GDR (East Germany)" ])

        encRects =
            encoding
                . position X [ PName "start", PmType Temporal, PTimeUnit (TU Year), PAxis [] ]
                . position X2 [ PName "end", PTimeUnit (TU Year) ]
                . color [ MName "event", MmType Nominal ]

        specRects =
            asSpec [ highlights [], mark Rect [], encRects [] ]

        encPopulation =
            encoding
                . position X [ PName "year", PmType Temporal, PTimeUnit (TU Year), PAxis [ AxNoTitle ] ]
                . position Y [ PName "population", PmType Quantitative ]
                . color [ MString "#333" ]

        specLine =
            asSpec [ mark Line [], encPopulation [] ]

        specPoints =
            asSpec [ mark Point [], encPopulation [] ]
    in
    toVegaLite [ des, width 500, dvals [], layer [ specRects, specLine, specPoints ] ]


label8 :: VegaLite
label8 =
  let strColumn n vs = dataColumn n (Strings vs)

      medians =
        dataFromColumns []
        . strColumn "name" [ "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:" ]
        . dataColumn "median" (Numbers [ 1.999976, 2, 1.999969, 2.500045, 1.500022, 2.99998, 4.500007 ])
        . strColumn "lo" [ "Easy", "Easy", "Toolbar", "Toolbar", "Toolbar", "Toolbar", "Phone" ]
        . strColumn "hi" [ "Hard", "Hard", "Gesture", "Gesture", "Gesture", "Gesture", "Tablet" ]

      values =
        dataFromColumns []
        . strColumn "value" [ "P1", "2", "2", "3", "4", "2", "5", "5", "1", "1", "P2", "2", "3", "4", "5", "5", "5", "5", "1", "1", "P3", "2", "2", "2", "1", "2", "1", "5", "1", "0", "P4", "3", "3", "2", "2", "4", "1", "5", "1", "0", "P5", "2", "2", "4", "4", "4", "5", "5", "0", "1", "P6", "1", "3", "3", "4", "4", "4", "4", "0", "1", "P7", "2", "3", "4", "5", "3", "2", "4", "0", "0", "P8", "3", "1", "2", "4", "2", "5", "5", "0", "0", "P9", "2", "3", "2", "4", "1", "4", "4", "1", "1", "P10", "2", "2", "1", "1", "1", "1", "5", "1", "1", "P11", "2", "2", "1", "1", "1", "1", "4", "1", "0", "P12", "1", "3", "2", "3", "1", "3", "3", "0", "1", "P13", "2", "2", "1", "1", "1", "1", "5", "0", "0", "P14", "3", "3", "2", "2", "1", "1", "1", "1", "1", "P15", "4", "5", "1", "1", "1", "1", "5", "1", "0", "P16", "1", "3", "2", "2", "1", "4", "5", "0", "1", "P17", "3", "2", "2", "2", "1", "3", "2", "0", "0" ]
        . strColumn "name" [ "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First", "Participant ID", "Identify Errors:", "Fix Errors:", "Easier to Fix:", "Faster to Fix:", "Easier on Phone:", "Easier on Tablet:", "Device Preference:", "Tablet_First", "Toolbar_First" ]
        . strColumn "id" [ "P1", "P1", "P1", "P1", "P1", "P1", "P1", "P1", "P1", "P1", "P2", "P2", "P2", "P2", "P2", "P2", "P2", "P2", "P2", "P2", "P3", "P3", "P3", "P3", "P3", "P3", "P3", "P3", "P3", "P3", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P5", "P5", "P5", "P5", "P5", "P5", "P5", "P5", "P5", "P5", "P6", "P6", "P6", "P6", "P6", "P6", "P6", "P6", "P6", "P6", "P7", "P7", "P7", "P7", "P7", "P7", "P7", "P7", "P7", "P7", "P8", "P8", "P8", "P8", "P8", "P8", "P8", "P8", "P8", "P8", "P9", "P9", "P9", "P9", "P9", "P9", "P9", "P9", "P9", "P9", "P10", "P10", "P10", "P10", "P10", "P10", "P10", "P10", "P10", "P10", "P11", "P11", "P11", "P11", "P11", "P11", "P11", "P11", "P11", "P11", "P12", "P12", "P12", "P12", "P12", "P12", "P12", "P12", "P12", "P12", "P13", "P13", "P13", "P13", "P13", "P13", "P13", "P13", "P13", "P13", "P14", "P14", "P14", "P14", "P14", "P14", "P14", "P14", "P14", "P14", "P15", "P15", "P15", "P15", "P15", "P15", "P15", "P15", "P15", "P15", "P16", "P16", "P16", "P16", "P16", "P16", "P16", "P16", "P16", "P16", "P17", "P17", "P17", "P17", "P17", "P17", "P17", "P17", "P17", "P17" ]

      enc = encoding
            . position Y [ PName "name"
                         , PmType Nominal
                         , PSort []
                         , PAxis
                           [ AxDomain False
                           , AxOffset 50
                           , AxLabelFontWeight Bold
                           , AxTicks False
                           , AxGrid True
                           , AxNoTitle
                           ]
                         ]

      trans = transform
              . filter (FExpr "datum.name != 'Toolbar_First'")
              . filter (FExpr "datum.name != 'Tablet_First'")
              . filter (FExpr "datum.name != 'Participant ID'")

      encCircle =
        encoding
        . position X [ PName "value"
                     , PmType Quantitative
                     , PScale [ SDomain (DNumbers [ 0, 6 ]) ]
                     , PAxis [ AxGrid False, AxValues (Numbers [ 1, 2, 3, 4, 5 ]) ]
                     ]
        . size [ MAggregate Count
               , MmType Quantitative
               , MLegend [ LTitle "Number of Ratings", LOffset 75 ]
               ]

      specCircle =
        asSpec [ dataFromSource "values" []
               , trans []
               , encCircle []
               , mark Circle [ MColor "#6eb4fd" ]
               ]

      encTick1 =
        encoding
        . position X [ PName "median"
                     , PmType Quantitative
                     , PScale [ SDomain (DNumbers [ 0, 6 ]) ]
                     , PAxis [ AxNoTitle ]
                     ]

      specTick1 =
        asSpec [ encTick1 [], mark Tick [ MColor "black" ] ]

      encTextLo =
        encoding
        . text [ TName "lo", TmType Nominal ]

      specTextLo =
        asSpec [ encTextLo [], mark Text [ MX (-5), MAlign AlignRight ] ]

      encTextHi =
        encoding
        . text [ TName "hi", TmType Nominal ]

      specTextHi =
        asSpec [ encTextHi [], mark Text [ MX 255, MAlign AlignLeft ] ]

  in toVegaLite
        [ noStroke []
        , datasets [ ( "medians", medians [] ), ( "values", values [] ) ]
        , dataFromSource "medians" []
        , title "Questionnaire Ratings" []
        , width 250
        , height 175
        , enc []
        , layer [ specCircle, specTick1, specTextLo, specTextHi ]
        ]


label9 :: VegaLite
label9 =
  let des = description "Comparing Likert scale ratings between two conditions."

      cfg = noStroke
            . configuration
                    (MarkNamedStyles
                        [ ( "arrow-label", [ MdY 12, MFontSize 9.5 ] )
                        , ( "arrow-label2", [ MdY 24, MFontSize 9.5 ] )
                        ]
                    )
            . configuration (TitleStyle [ TFontSize 12 ])

      lickertData =
        dataFromColumns []
        . dataColumn "measure" (Strings [ "Open Exploration", "Focused Question Answering", "Open Exploration", "Focused Question Answering" ])
        . dataColumn "mean" (Numbers [ 1.81, -1.69, 2.19, -0.06 ])
        . dataColumn "lo" (Numbers [ 1.26, -2.33, 1.67, -0.47 ])
        . dataColumn "hi" (Numbers [ 2.37, -1.05, 2.71, 0.35 ])
        . dataColumn "study" (Strings [ "PoleStar vs Voyager", "PoleStar vs Voyager", "PoleStar vs Voyager 2", "PoleStar vs Voyager 2" ])

      labelData =
        dataFromColumns []
        . dataColumn "from" (Numbers [ -0.25, 0.25 ])
        . dataColumn "to" (Numbers [ -2.9, 2.9 ])
        . dataColumn "label" (Strings [ "PoleStar", "Voyager / Voyager 2" ])

      encLickert =
        encoding
        . position Y [ PName "study"
                     , PmType Nominal
                     , PAxis [ AxNoTitle
                             , AxLabelPadding 5
                             , AxDomain False
                             , AxTicks False
                             , AxGrid False
                             ]
                     ]

      encLickertWhiskers =
        encoding
        . position X [ PName "lo"
                     , PmType Quantitative
                     , PScale [ SDomain (DNumbers [ -3, 3 ]) ]
                     , PAxis
                       [ AxNoTitle
                       , AxGridDash [ 3, 3 ]
                       , AxDataCondition (Expr "datum.value === 0") (CAxGridColor "#666" "#ccc")
                       ]
                     ]
        . position X2 [ PName "hi" ]

      specLickertWhiskers = asSpec [ encLickertWhiskers [], mark Rule [] ]

      encLickertMeans =
        encoding
        . position X [ PName "mean", PmType Quantitative ]
        . color [ MName "measure"
                , MmType Nominal
                , MScale [ SRange (RStrings [ "black", "white" ]) ]
                , MLegend []
                ]

      specLickertMeans =
        asSpec [ encLickertMeans [], mark Circle [ MStroke "black", MOpacity 1 ] ]

      specLickert =
        asSpec
        [ title "Mean of Subject Ratings (95% CIs)" [ TFrame FrBounds ]
        , encLickert []
        , layer [ specLickertWhiskers, specLickertMeans ]
        ]

      encLabel1 =
        encoding
        . position X [ PName "from", PmType Quantitative, PScale [ SZero False ], PAxis [] ]
        . position X2 [ PName "to" ]

      specLabel1 = asSpec [ encLabel1 [], mark Rule [] ]

      encLabel2 =
        encoding
        . position X [ PName "to", PmType Quantitative, PAxis [] ]
        . shape [ MDataCondition
                  [ ( Expr "datum.to > 0", [ MSymbol SymTriangleRight ] ) ]
                  [ MSymbol SymTriangleLeft ]
                ]

      specLabel2 = asSpec [ encLabel2 [], mark Point [ MFilled True, MSize 60, MFill "black" ] ]

      transLabel3 =
        transform
        . filter (FExpr "datum.label === 'PoleStar'")

      from = position X [ PName "from", PmType Quantitative, PAxis [] ]

      encLabel3 =
        encoding
        . from
        . text [ TName "label", TmType Nominal ]

      specLabel3 =
        asSpec [ transLabel3 []
               , encLabel3 []
               , mark Text [ MAlign AlignRight, MStyle [ "arrow-label" ] ]
               ]

      transLabel4 =
        transform
        . filter (FExpr "datum.label !== 'PoleStar'")

      encLabel4 = encLabel3

      specLabel4 =
        asSpec [ transLabel4 []
               , encLabel4 []
               , mark Text [ MAlign AlignLeft, MStyle [ "arrow-label" ] ]
               ]

      transLabel5 = transLabel3

      encLabel5 =
        encoding
        . from
        . text [ TString "more valuable" ]

      specLabel5 =
        asSpec [ transLabel5 []
               , encLabel5 []
               , mark Text [ MAlign AlignRight, MStyle [ "arrow-label2" ] ]
               ]

      transLabel6 = transLabel4

      encLabel6 = encLabel5

      specLabel6 =
        asSpec [ transLabel6 []
               , encLabel6 []
               , mark Text [ MAlign AlignLeft, MStyle [ "arrow-label2" ] ]
               ]

      specLabels =
        asSpec [ labelData []
               , layer [ specLabel1, specLabel2, specLabel3, specLabel4, specLabel5, specLabel6 ]
               ]

  in toVegaLite [ des
                , cfg []
                , lickertData []
                , spacing 10
                , vConcat [ specLickert, specLabels ]
                ]

-- like label9 but trying to match the current version (Feb 2020) of
-- https://vega.github.io/vega-lite/examples/concat_layer_voyager_result.html
--
voyagerData :: Value
voyagerData = [aesonQQ|
[ { "measure": "Open Exploration",
    "mean": 1.813,
    "lo": 1.255,
    "hi": 2.37,
    "study": "PoleStar vs Voyager"
  },
  {
    "measure": "Focused Question Answering",
    "mean": -1.688,
    "lo": -2.325,
    "hi": -1.05,
    "study": "PoleStar vs Voyager"
  },
  {
    "measure": "Open Exploration",
    "mean": 2.1875,
    "lo": 1.665,
    "hi": 2.71,
    "study": "PoleStar vs Voyager 2"
  },
  {
    "measure": "Focused Question Answering",
    "mean": -0.0625,
    "lo": -0.474,
    "hi": 0.349,
    "study": "PoleStar vs Voyager 2"
  } ]
|]

voyager :: VegaLite
voyager =
  let dataPlot = [ title "Mean of Subject Ratings (95% CIs)" [TFrame FrBounds]
                 , encoding
                   . position Y [ PName "study"
                                , PmType Nominal
                                , PAxis [ AxNoTitle
                                        , AxLabelPadding 5
                                        , AxDomain False
                                        , AxTicks False
                                        , AxGrid False
                                        ]
                                ]
                   $ []
                 , layer [asSpec rulePlot, asSpec circlePlot]
                 ]

      gridColor = AxDataCondition (Expr "datum.value === 0")
                                  (CAxGridColor "#666" "#CCC")

      rulePlot = [ mark Rule []
                 , encoding
                   . position X [ PName "lo"
                                , PmType Quantitative
                                , PScale [SDomain (DNumbers [-3, 3])]
                                , PAxis [ AxTitle ""
                                        , AxGridDash [3, 3]
                                        , gridColor
                                        ]
                                ]
                   . position X2 [PName "hi"]
                   $ []
                 ]

      circlePlot = [ mark Circle [MStroke "black", MOpacity 1]
                   , encoding
                     . position X [PName "mean", PmType Quantitative]
                     . color [ MName "measure"
                             , MmType Nominal
                             , MScale [SRange (RStrings ["black", "white"])]
                             , MLegend []
                             ]
                     $ []
                   ]

      labelPlot = [ dataFromColumns []
                    . dataColumn "from" (Numbers [-0.25, 0.25])
                    . dataColumn "to" (Numbers [-2.9, 2.9])
                    . dataColumn "label" (Strings ["PoleStar", "Voyager / Voyager 2"])
                    $ []
                  , layer (map asSpec [linePlot, arrowsPlot, pTextPlot, vTextPlot])
                  ]

      linePlot = [ mark Rule []
                 , encoding
                   . position X [ PName "from"
                                , PmType Quantitative
                                , PScale [SZero False]
                                , PAxis []
                                ]
                   . position X2 [PName "to"]
                   $ []
                 ]

      shapeOpts = MDataCondition
                      [(Expr "datum.to > 0", [MSymbol SymTriangleRight])]
                      [MSymbol SymTriangleLeft]

      arrowsPlot = [ mark Point [ MFilled True
                                , MFill "black"
                                , MSize 60 ]
                   , encoding
                     . position X [ PName "to"
                                  , PmType Quantitative
                                  , PAxis []
                                  ]
                     . shape [ shapeOpts ]
                     $ []
                   ]

      from = position X [PName "from", PmType Quantitative, PAxis []]

      pTextPlot = [ mark Text [ MAlign AlignRight
                              , MStyle ["arrow-label"]
                              , MTexts ["Polestar", "More Valuable"]
                              ]
                  , transform
                    . filter (FExpr "datum.label === 'PoleStar'")
                    $ []
                  , encoding
                    . from
                    $ []
                  ]

      vTextPlot = [ mark Text [ MAlign AlignLeft
                              , MStyle ["arrow-label"]
                              , MTexts ["Voyager / Voyager 2", "More Valuable"]
                              ]
                  , transform
                    . filter (FExpr "datum.label !== 'PoleStar'")
                    $ []
                  , encoding
                    . from
                    $ []
                  ]

      styles = MarkNamedStyles
                   [ ("arrow-label", [MdY 12, MFontSize 9.5])
                   , ("arrow-label2", [MdY 24, MFontSize 9.5])
                   ]
      conf = configure
             . configuration (ViewStyle [ViewStroke "transparent"])
             . configuration styles
             . configuration (TitleStyle [TFontSize 12])

      v = [ dataFromJson voyagerData []
          , spacing 10
          , vConcat [asSpec dataPlot, asSpec labelPlot]
          , conf []
          ]

  in toVegaLite v


baselines :: VegaLite
baselines =
  let dvals = dataFromColumns []
              . dataColumn "x" (Numbers [ 5, 30 ])
              . dataColumn "y" (Numbers [ 5, 30 ])

      ax t n = position t [ PName n
                          , PmType Quantitative
                          , PScale [ SNice (IsNice False)
                                   , SDomain (DNumbers [-10, 40])
                                   ]
                          , PAxis [ AxNoTitle
                                  ]
                          ]

      enc = encoding
            . ax X "x"
            . ax Y "y"
            . text [ TStrings ["Xxgq", "qTpP"] ]

      plot l a = asSpec [ enc [], title l [], mark Text [ MBaseline a ] ]

      -- plots are ordered left to right, top to bottom
      plots = vlConcat [ plot "top" AlignTop
                       , plot "line-top" AlignLineTop
                       , plot "middle" AlignMiddle
                       , plot "baseline" AlignBaseline
                       , plot "bottom" AlignBottom
                       , plot "line-bottom" AlignLineBottom
                       ]

  in toVegaLite [ dvals []
                , configure
                  . configuration (MarkStyle [ MFontSize 20
                                             , MFontStyle "italic"
                                             , MLineHeight 30 ])
                  . configuration (TitleStyle [ TFont "Comic Sans MS" ])
                  $ []
                , columns 2
                , plots
                ]
