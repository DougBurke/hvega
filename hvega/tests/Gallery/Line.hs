{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryLine.elm (from development of version
-- 1.13.0)
--
-- TODO: update to v2.0 Elm test suite

module Gallery.Line (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup, repeat)
import Data.Function ((&))


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("line1", line1)
            , ("line2", line2)
            , ("line3", line3)
            , ("line4", line4)
            , ("line5", line5)
            , ("line6", line6)
            , ("line7", line7)
            , ("line8", line8)
            , ("line9", line9)
            , ("line10", line10)
            , ("line11", line11)
            , ("line12", line12)
            , ("conditionalaxis", conditionalAxis)
            , ("linecolorhalo", lineColorHalo)
            ]


stockData :: Data
stockData = dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []

goog :: [TransformSpec] -> PropertySpec
goog = transform . filter (FExpr "datum.symbol === 'GOOG'")

baseEnc :: [EncodingSpec] -> PropertySpec
baseEnc = encoding
          . position X [ PName "date", PmType Temporal, PAxis [ AxFormat "%Y" ] ]
          . position Y [ PName "price", PmType Quantitative ]


line1 :: VegaLite
line1 =
    toVegaLite
        [ description "Google's stock price over time."
        , stockData
        , goog []
        , mark Line []
        , baseEnc []
        ]


line2 :: VegaLite
line2 =
    toVegaLite
        [ description "Google's stock price over time with point markers."
        , stockData
        , goog []
        , mark Line [ MColor "green", MPoint (PMMarker [ MColor "purple" ]) ]
        , baseEnc []
        ]


line3 :: VegaLite
line3 =
    toVegaLite
        [ description "Stock prices of 5 tech companies over time."
        , stockData
        , mark Line []
        , baseEnc (color [ MName "symbol", MmType Nominal ] [])
        ]


line4 :: VegaLite
line4 =
    let
        des =
            description "Slope graph showing the change in yield for different barley sites. It shows the error in the year labels for the Morris site."

        enc =
            encoding
                . position X [ PName "year", PmType Ordinal, PScale [ SPadding 0.5 ] ]
                . position Y [ PName "yield", PmType Quantitative, PAggregate Median ]
                . color [ MName "site", MmType Nominal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/barley.json" []
        , mark Line []
        , widthStep 50
        , enc []
        ]


line5 :: VegaLite
line5 =
    toVegaLite
        [ description "Google's stock price over time (quantized as a step-chart)."
        , stockData
        , goog []
        , mark Line [ MInterpolate StepAfter ]
        , baseEnc []
        ]


line6 :: VegaLite
line6 =
    let
        des =
            description "Google's stock price over time (smoothed with monotonic interpolation)."

    in
    toVegaLite
        [ des
        , stockData
        , goog []
        , mark Line [ MInterpolate Monotone ]
        , baseEnc []
        ]


line7 :: VegaLite
line7 =
    let
        des =
            description "A connected scatterplot can be created by customizing line order and adding point marker in the line mark definition."

        enc =
            encoding
                . position X [ PName "miles", PmType Quantitative, PScale [ SZero False ] ]
                . position Y [ PName "gas", PmType Quantitative, PScale [ SZero False ] ]
                . order [ OName "year" ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/driving.json" []
        , enc []
        , mark Line [ MPoint (PMMarker []) ]
        ]


line8 :: VegaLite
line8 =
    let
        des =
            description "Stock prices of five tech companies over time double encoding price with vertical position and line thickness."

        enc =
            baseEnc
              . size [ MName "price", MmType Quantitative ]
              . color [ MName "symbol", MmType Nominal ]
    in
    toVegaLite
        [ des
        , stockData
        , mark Trail []
        , enc []
        ]


line9 :: VegaLite
line9 =
    let
        des =
            description "Line chart with markers and invalid values."

        dvals =
            dataFromColumns []
                . dataColumn "x" (Numbers [ 1, 2, 3, 4, 5, 6, 7 ])
                . dataColumn "y" (Numbers [ 10, 30, 0 / 0, 15, 0 / 0, 40, 20 ])

        enc =
            encoding
                . position X [ PName "x", PmType Quantitative ]
                . position Y [ PName "y", PmType Quantitative ]
    in
    toVegaLite
        [ des
        , dvals []
        , mark Line [ MPoint (PMMarker []) ]
        , enc []
        ]


line10 :: VegaLite
line10 =
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
                    , PAxis [ AxTitle "CO2 concentration in ppm" ]
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

        config =
            configure . configuration (ViewStyle [ ViewNoStroke ])
    in
    toVegaLite
        [ des
        , config []
        , width 800
        , height 500
        , dataFromUrl "https://vega.github.io/vega-lite/data/co2-concentration.csv" [ Parse [ ( "Date", FoUtc "%Y-%m-%d" ) ] ]
        , trans []
        , encPosition []
        , layer [ specLine, specTextMin, specTextMax ]
        ]


line11 :: VegaLite
line11 =
    let
        des =
            description "Line chart showing ranks over time for thw World Cup 2018 Group F teams"

        dvals =
            dataFromColumns []
                . dataColumn "team" (Strings [ "Germany", "Mexico", "South Korea", "Sweden", "Germany", "Mexico", "South Korea", "Sweden", "Germany", "Mexico", "South Korea", "Sweden" ])
                . dataColumn "matchday" (Numbers [ 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3 ])
                . dataColumn "point" (Numbers [ 0, 3, 0, 3, 3, 6, 0, 3, 3, 6, 3, 6 ])
                . dataColumn "diff" (Numbers [ -1, 1, -1, 1, 0, 2, -2, 0, -2, -1, 0, 3 ])

        trans =
            transform
                . window [ ( [ WOp Rank ], "rank" ) ]
                    [ WSort [ WDescending "point", WDescending "diff" ], WGroupBy [ "matchday" ] ]

        enc =
            encoding
                . position X [ PName "matchday", PmType Ordinal ]
                . position Y [ PName "rank", PmType Ordinal ]
                . color [ MName "team", MmType Nominal, MScale teamColours ]

        teamColours =
            categoricalDomainMap
                [ ( "Germany", "black" )
                , ( "Mexico", "#127153" )
                , ( "South Korea", "#c91a3c" )
                , ( "Sweden", "#0c71ab" )
                ]
    in
    toVegaLite [ des, dvals [], trans [], enc [], mark Line [ MOrient Vertical ] ]


line12 :: VegaLite
line12 =
    let
        des =
            description "Plots a function using a generated sequence"

        dvals =
            dataSequenceAs 0 12.7 0.1 "u"

        trans =
            transform
                . calculateAs "sin(datum.u)" "v"
                . calculateAs "cos(datum.u)" "w"

        encSin =
            encoding
                . position X [ PName "u", PmType Quantitative, PTitle "x" ]
                . position Y [ PName "v", PmType Quantitative, PTitle "sin(x)" ]

        specSin =
            asSpec [ encSin [], mark Line [] ]

        encCos =
            encoding
                . position X [ PName "u", PmType Quantitative, PTitle "x" ]
                . position Y [ PName "w", PmType Quantitative, PTitle "cos(x)" ]

        specCos =
            asSpec [ encCos [], mark Line [ MStroke "firebrick" ] ]
    in
    toVegaLite [ des, width 300, height 150, dvals, trans [], layer [ specSin, specCos ] ]


-- From https://vega.github.io/vega-lite/examples/line_conditional_axis.html
-- added in Vega-Lite 4.6.0
--
conditionalAxis :: VegaLite
conditionalAxis =
  let enc = encoding
            . position Y [PName "price", PmType Quantitative]
            . position X [PName "date", PmType Temporal, PAxis axOpts]

      axOpts = [ AxTickCount 8
               , AxLabelAlign AlignLeft
               , AxLabelExpr expr
               , AxLabelOffset 4
               , AxLabelPadding (-24)
               , AxTickSize 30
               , AxDataCondition
                   cond
                   (CAxGridDash [] [2, 2])
               , AxDataCondition
                   cond
                   (CAxTickDash [] [2, 2])
               ]

      expr = "[timeFormat(datum.value, '%b'), timeFormat(datum.value, '%m') == '01' ? timeFormat(datum.value, '%Y') : '']"

      cond = FEqual "value" (Number 1)
             & FilterOpTrans (MTimeUnit (TU Month))

      yearRange = FRange "date" (NumberRange 2006 2007)
                  & FilterOpTrans (MTimeUnit (TU Year))
                  & FCompose

  in toVegaLite [ description "Line chart with conditional axis ticks, labels, and grid."
                , stockData
                , goog
                  . filter yearRange
                  $ []
                , width 400
                , mark Line []
                , enc []
                , configure
                  . configuration (Axis [DomainColor "#ddd", TickColor "#ddd"])
                  $ []
                ]


-- https://vega.github.io/vega-lite/examples/line_color_halo.html
lineColorHalo :: VegaLite
lineColorHalo =
  let desc = "Multi-series Line Chart with Halo. Use pivot and repeat-layer as a workaround to facet groups of lines and their halo strokes. See https://github.com/vega/vega-lite/issues/6192 for more discussion."

      plot = [layer [asSpec plot1, asSpec plot2]]

      encBase = encoding
                . position X [PName "date", PmType Temporal]
                . position Y [PRepeat Layer, PmType Quantitative, PTitle "price"]
                
      plot1 = [ mark Line [MStroke "white", MStrokeWidth 4]
              , encBase []
              ]
      plot2 = [ mark Line []
              , encBase
                . stroke [MRepeatDatum Layer, MmType Nominal]
                $ []
              ]

  in toVegaLite [ description desc
                , stockData
                , transform
                  . pivot "symbol" "price" [PiGroupBy ["date"]]
                  $ []
                , repeat [LayerFields ["AAPL", "AMZN", "GOOG", "IBM", "MSFT"]]
                , specification (asSpec plot)
                ]
