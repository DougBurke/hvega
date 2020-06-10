{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryLayer.elm (from development of version
-- 1.13.0)
--
module Gallery.Layer (testSpecs) where

import qualified Data.Text as T

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup, repeat)

import Data.Aeson (Value, (.=), object, toJSON)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("layer1", layer1)
            , ("layer2", layer2)
            , ("layer3", layer3)
            , ("layer4", layer4)
            , ("layer5", layer5)
            , ("layer6", layer6)
            , ("layer7", layer7)
            , ("layertimeunitrect", layerTimeunitRect)
            , ("layer_bar_fruit", layerBarFruit)
            ]


seattleData :: Data
seattleData = dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []


layer1 :: VegaLite
layer1 =
    let
        des =
            description "A candlestick chart inspired by Protovis (http://mbostock.github.io/protovis/ex/candlestick.html)"

        dvals =
            dataFromColumns []
                . dataColumn "date" (Strings [ "01-Jun-2009", "02-Jun-2009", "03-Jun-2009", "04-Jun-2009", "05-Jun-2009", "08-Jun-2009", "09-Jun-2009", "10-Jun-2009", "11-Jun-2009", "12-Jun-2009", "15-Jun-2009", "16-Jun-2009", "17-Jun-2009", "18-Jun-2009", "19-Jun-2009", "22-Jun-2009", "23-Jun-2009", "24-Jun-2009", "25-Jun-2009", "26-Jun-2009", "29-Jun-2009", "30-Jun-2009" ])
                . dataColumn "open" (Numbers [ 28.7, 30.04, 29.62, 31.02, 29.39, 30.84, 29.77, 26.9, 27.36, 28.08, 29.7, 30.81, 31.19, 31.54, 29.16, 30.4, 31.3, 30.58, 29.45, 27.09, 25.93, 25.36 ])
                . dataColumn "high" (Numbers [ 30.05, 30.13, 31.79, 31.02, 30.81, 31.82, 29.77, 29.74, 28.11, 28.5, 31.09, 32.75, 32.77, 31.54, 29.32, 32.05, 31.54, 30.58, 29.56, 27.22, 27.18, 27.38 ])
                . dataColumn "low" (Numbers [ 28.45, 28.3, 29.62, 29.92, 28.85, 26.41, 27.79, 26.9, 26.81, 27.73, 29.64, 30.07, 30.64, 29.6, 27.56, 30.3, 27.83, 28.79, 26.3, 25.76, 25.29, 25.02 ])
                . dataColumn "close" (Numbers [ 30.04, 29.63, 31.02, 30.18, 29.62, 29.77, 28.27, 28.46, 28.11, 28.15, 30.81, 32.68, 31.54, 30.03, 27.99, 31.17, 30.58, 29.05, 26.36, 25.93, 25.35, 26.35 ])
                . dataColumn "signal" (Strings [ "short", "short", "short", "short", "short", "short", "short", "short", "short", "short", "long", "short", "short", "short", "short", "short", "short", "long", "long", "long", "long", "long" ])
                . dataColumn "ret" (Numbers [ -4.89396411092985, -0.322580645161295, 3.68663594470045, 4.51010886469673, 6.08424336973478, 1.2539184952978, -5.02431118314424, -5.46623794212217, -8.3743842364532, -5.52763819095477, 3.4920634920635, 0.155038759689914, 5.82822085889571, 8.17610062893082, 8.59872611464968, 15.4907975460123, 11.7370892018779, -10.4234527687296, 0, 0, 5.26315789473684, 6.73758865248228 ])

        trans =
            transform . calculateAs "datum.open > datum.close" "isIncrease"

        encLine =
            encoding
                . position X
                    [ PName "date"
                    , PmType Temporal
                    , PTimeUnit (TU YearMonthDate)
                    , PScale [ SDomain (DDateTimes [ [ DTMonth May, DTDate 31, DTYear 2009 ], [ DTMonth Jul, DTDate 1, DTYear 2009 ] ]) ]
                    , PAxis [ AxTitle "Date in 2009", AxFormat "%m/%d" ]
                    ]
                . position Y [ PName "low", PmType Quantitative, PScale [ SZero False ] ]
                . position Y2 [ PName "high" ]
                . color [ MName "isIncrease", MmType Nominal, MLegend [], MScale [ SRange (RStrings [ "#ae1325", "#06982d" ]) ] ]

        specLine =
            asSpec [ mark Rule [], encLine [] ]

        encBar =
            encoding
                . position X [ PName "date", PmType Temporal, PTimeUnit (TU YearMonthDate) ]
                . position Y [ PName "open", PmType Quantitative ]
                . position Y2 [ PName "close" ]
                . size [ MNumber 5 ]
                . color [ MName "isIncrease", MmType Nominal, MLegend [] ]

        specBar =
            asSpec [ mark Bar [], encBar [] ]
    in
    toVegaLite [ des, width 320, dvals [], trans [], layer [ specLine, specBar ] ]


layer2 :: VegaLite
layer2 =
    let
        des =
            description "A ranged dot plot that uses 'layer' to convey changing life expectancy for the five most populous countries (between 1955 and 2000)."

        trans =
            transform
                . filter (FOneOf "country" (Strings [ "China", "India", "United States", "Indonesia", "Brazil" ]))
                . filter (FOneOf "year" (Numbers [ 1955, 2000 ]))

        encCountry =
            encoding
                . position Y
                    [ PName "country"
                    , PmType Nominal
                    , PAxis [ AxTitle "Country", AxOffset 5, AxTicks False, AxMinExtent 70, AxDomain False ]
                    ]

        encLine =
            encoding
                . position X [ PName "life_expect", PmType Quantitative ]
                . detail [ DName "country", DmType Nominal ]
                . color [ MString "#db646f" ]

        specLine =
            asSpec [ mark Line [], encLine [] ]

        encPoints =
            encoding
                . position X [ PName "life_expect", PmType Quantitative, PAxis [ AxTitle "Life Expectancy (years)" ] ]
                . color [ MName "year", MmType Ordinal, MScale (domainRangeMap ( 1955, "#e6959c" ) ( 2000, "#911a24" )), MLegend [ LTitle "Year" ] ]
                . size [ MNumber 100 ]
                . opacity [ MNumber 1 ]

        specPoints =
            asSpec [ mark Point [ MFilled True ], encPoints [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/countries.json" []
        , trans []
        , encCountry []
        , layer [ specLine, specPoints ]
        ]


layer3 :: VegaLite
layer3 =
    let
        des =
            description "Bullet chart"

        conf =
            configure . configuration (TickStyle [ MThickness 2 ])

        row_ :: T.Text -> [Double] -> [Double] -> Double -> Value
        row_ ttl ranges measures marker =
            object
                [ "title" .= ttl
                , "ranges" .= ranges
                , "measures" .= measures
                , "markers" .= [ marker ]
                ]

        dvals =
            dataFromJson
                (toJSON
                    [ row_ "Revenue" [ 150, 225, 300 ] [ 220, 270 ] 250
                    , row_ "Profit" [ 20, 25, 30 ] [ 21, 23 ] 26
                    , row_ "Order size" [ 350, 500, 600 ] [ 100, 320 ] 550
                    , row_ "New customers" [ 1400, 2000, 2500 ] [ 1000, 1650 ] 2100
                    , row_ "Satisfaction" [ 3.5, 4.25, 5 ] [ 3.2, 4.7 ] 4.4
                    ]
                )

        fac =
            facet [ RowBy [ FName "title", FmType Ordinal, FHeader [ HLabelAngle 30, HNoTitle ] ] ]

        res =
            resolve . resolution (RScale [ ( ChX, Independent ) ])

        enc1 =
            encoding
                . position X
                    [ PName "ranges[2]"
                    , PmType Quantitative
                    , PScale [ SNice (IsNice False) ]
                    , PAxis [ AxNoTitle ]
                    ]

        spec1 =
            asSpec [ mark Bar [ MColor "#eee" ], enc1 [] ]

        enc2 =
            encoding . position X [ PName "ranges[1]", PmType Quantitative ]

        spec2 =
            asSpec [ mark Bar [ MColor "#ddd" ], enc2 [] ]

        enc3 =
            encoding . position X [ PName "ranges[0]", PmType Quantitative ]

        spec3 =
            asSpec [ mark Bar [ MColor "#ccc" ], enc3 [] ]

        enc4 =
            encoding . position X [ PName "measures[1]", PmType Quantitative ]

        spec4 =
            asSpec [ mark Bar [ MColor "lightsteelblue", MSize 10 ], enc4 [] ]

        enc5 =
            encoding . position X [ PName "measures[0]", PmType Quantitative ]

        spec5 =
            asSpec [ mark Bar [ MColor "steelblue", MSize 10 ], enc5 [] ]

        enc6 =
            encoding . position X [ PName "markers[0]", PmType Quantitative ]

        spec6 =
            asSpec [ mark Tick [ MColor "black" ], enc6 [] ]
    in
    toVegaLite
        [ des
        , conf []
        , dvals []
        , fac
        , res []
        , specification (asSpec [ layer [ spec1, spec2, spec3, spec4, spec5, spec6 ] ])
        ]


layer4 :: VegaLite
layer4 =
    let
        des =
            description "Layered bar/line chart with dual axes"

        encTime =
            encoding . position X [ PName "date", PmType Ordinal, PTimeUnit (TU Month) ]

        encBar =
            encoding
                . position Y [ PName "precipitation", PmType Quantitative, PAggregate Mean, PAxis [ AxGrid False ] ]

        specBar =
            asSpec [ mark Bar [], encBar [] ]

        encLine =
            encoding
                . position Y [ PName "temp_max", PmType Quantitative, PAggregate Mean, PAxis [ AxGrid False ], PScale [ SZero False ] ]
                . color [ MString "firebrick" ]

        specLine =
            asSpec [ mark Line [], encLine [] ]

        res =
            resolve
                . resolution (RScale [ ( ChY, Independent ) ])
    in
    toVegaLite
        [ des
        , seattleData
        , encTime []
        , layer [ specBar, specLine ]
        , res []
        ]

layer5 :: VegaLite
layer5 =
    let
        des =
            description "Horizon chart with 2 layers. (See https://idl.cs.washington.edu/papers/horizon/ for more details on horizon charts.)"

        dvals =
            dataFromColumns []
                . dataColumn "x" (Numbers (map fromIntegral [1::Int .. 20]))
                . dataColumn "y" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52, 48, 24, 49, 87, 66, 17, 27, 68, 16, 49, 15 ])

        trans =
            transform . calculateAs "datum.y - 50" "ny"

        encX =
            encoding . position X [ PName "x", PmType Quantitative, PScale [ SZero False, SNice (IsNice False) ] ]

        encLower =
            encoding
                . position Y [ PName "y", PmType Quantitative, PScale [ SDomain (DNumbers [ 0, 50 ]) ] ]
                . opacity [ MNumber 0.6 ]

        specLower =
            asSpec [ mark Area [ MClip True ], encLower [] ]

        encUpper =
            encoding
                . position Y [ PName "ny", PmType Quantitative, PScale [ SDomain (DNumbers [ 0, 50 ]) ], PAxis [ AxTitle "y" ] ]
                . opacity [ MNumber 0.3 ]

        specUpper =
            asSpec [ trans [], mark Area [ MClip True ], encUpper [] ]

        config =
            configure
                . configuration (AreaStyle [ MInterpolate Monotone, MOrient Vertical ])
    in
    toVegaLite
        [ des
        , width 300
        , height 50
        , dvals []
        , encX []
        , layer [ specLower, specUpper ]
        , config []
        ]


layer6 :: VegaLite
layer6 =
  let label = description "A layered bar chart with floating bars representing weekly weather data"
      dvals = dataFromUrl "https://vega.github.io/vega-lite/data/weather.json" []

      titleOpts = title "Weekly Weather\nObservations and Predictions" [TFrame FrGroup]

      axis1 = [AxDomain False, AxTicks False, AxLabels False, AxNoTitle, AxTitlePadding 25, AxOrient STop]
      enc = encoding (position X [PName "id", PmType Ordinal, PAxis axis1] [])

      enc1 = encoding
             . position Y [ PName "record.low", PmType Quantitative
                          , PScale [SDomain (DNumbers [10, 70])]
                          , PAxis [AxTitle "Temperature (F)"]
                          ]
             . position Y2 [PName "record.high"]
             . size [MNumber 20]
             . color [MString "#ccc"]
      lyr1 = [mark Bar [MStyle ["box"]], enc1 []]

      enc2 = encoding
             . position Y [PName "normal.low", PmType Quantitative]
             . position Y2 [PName "normal.high"]
             . size [MNumber 20]
             . color [MString "#999"]
      lyr2 = [mark Bar [MStyle ["box"]], enc2 []]

      enc3 = encoding
             . position Y [PName "actual.low", PmType Quantitative]
             . position Y2 [PName "actual.high"]
             . size [MNumber 12]
             . color [MString "#000"]
      lyr3 = [mark Bar [MStyle ["box"]], enc3 []]

      enc4 = encoding
             . position Y [PName "forecast.low.low", PmType Quantitative]
             . position Y2 [PName "forecast.low.high"]
             . size [MNumber 12]
             . color [MString "#000"]
      lyr4 = [mark Bar [MStyle ["box"]], enc4 []]

      enc5 = encoding
             . position Y [PName "forecast.low.high", PmType Quantitative]
             . position Y2 [PName "forecast.high.low"]
             . size [MNumber 3]
             . color [MString "#000"]
      lyr5 = [mark Bar [MStyle ["box"]], enc5 []]

      enc6 = encoding
             . position Y [PName "forecast.high.low", PmType Quantitative]
             . position Y2 [PName "forecast.high.high"]
             . size [MNumber 12]
             . color [MString "#000"]
      lyr6 = [mark Bar [MStyle ["box"]], enc6 []]

      enc7 = encoding (text [TName "day", TmType Nominal] [])
      lyr7 = [mark Text [MAlign AlignCenter, MBaseline AlignBottom, MY (-5)], enc7]

      lyr = layer (map asSpec [lyr1, lyr2, lyr3, lyr4, lyr5, lyr6, lyr7])

  in toVegaLite [label, titleOpts, dvals, width 250, height 200, enc, lyr]



-- From
-- https://vega.github.io/vega-lite/examples/layer_line_rolling_mean_point_raw.html
-- but slightly adjusted, to use ZIndex on one of the axes.
--
layer7 :: VegaLite
layer7 =
  let desc = "Plot showing a 30 day rolling average with raw values in the background."

      wtrans = window
               [([WAggregateOp Mean, WField "temp_max"], "rolling_mean")]
               [WFrame (Just (-15)) (Just 15)]

      allPoints = [ mark Point [MOpacity 0.3]
                  , encoding
                      . position X [ PName "date"
                                   , PmType Temporal
                                   , PTitle "Date"
                                   , PAxis [ AxZIndex 1
                                           , AxGridColor "orange"
                                           , AxGridOpacity 0.8
                                           ]
                                   ]
                      . position Y [PName "temp_max", PmType Quantitative, PTitle "Max Temperature"]
                      $ []
                  ]
      avgPoints = [ mark Line [MColor "red", MSize 3]
                  , encoding
                      . position X [PName "date", PmType Temporal]
                      . position Y [PName "rolling_mean", PmType Quantitative]
                      $ []
                  ]

      layers = map asSpec [allPoints, avgPoints]

  in toVegaLite
     [ description desc
     , height 300
     , width 400
     , seattleData
     , transform (wtrans [])
     , layer layers
     ]


-- examples/specs/layer_timeunit_rect.vl.json
layerTimeunitRect :: VegaLite
layerTimeunitRect =
  let desc = "Drawing rect bin from the beginning of May to end of July"

      xAxis = [ PTimeUnit (TU Month)
              , PName "date"
              , PmType Temporal
              ]

      lyr1 = [ seattleData
             , mark Bar []
             , encoding
               . position X (xAxis ++
                            [ PTitle "month"
                            , PAxis [ AxLabelAlign AlignLeft
                                    , AxLabelExpr "datum.label[0]"
                                    ]
                            ]
                            )
               . position Y [PAggregate Mean, PName "precipitation", PmType Quantitative]
               $ []
             ]

      lyr2 = [ dataFromColumns []
               . dataColumn "date" (Strings ["May 1, 2010"])
               . dataColumn "date_end" (Strings ["July 15, 2010"])
               $ []
             , mark Rect [MOpacity 0.5, MColor "grey"]
             , encoding
               . position X xAxis
               . position X2 [PTimeUnit (TU Month), PName "date_end"]
               $ []
             ]
  
  in toVegaLite [ description desc
                , layer [asSpec lyr1, asSpec lyr2]
                ]


-- https://vega.github.io/vega-lite/examples/layer_bar_fruit.html
layerBarFruit :: VegaLite
layerBarFruit =
  let desc = "Vega-Lite version of bar chart from https://observablehq.com/@d3/learn-d3-scales."

      dvals = dataFromColumns []
              . dataColumn "name" (Strings ["🍊", "🍇", "🍏", "🍌", "🍐", "🍋", "🍎", "🍉"])
              . dataColumn "count" (Numbers [21,13, 8, 5, 3, 2, 1, 1])
              $ []

      plot1 = [ mark Bar []
              , encoding
                . color [MName "count", MmType Quantitative, MTitle "Number of fruit"]
                $ []
              ]

      plot2 = [ mark Text [ MAlign AlignRight
                          , MXOffset (-4)
                          , MAria False
                          ]
              , encoding
                . text [TName "count", TmType Quantitative]
                . color [ MDataCondition
                          [(FilterOp (FGreaterThan "count" (Number 10)), [MString "white"])]
                          [MString "black"]
                        ]
                $ []
              ]

  in toVegaLite [ description desc
                , width 400
                , dvals
                , encoding
                  . position X [PName "count", PmType Quantitative, PNoTitle]
                  . position Y [PName "name", PmType Ordinal, PSort [Descending, ByChannel ChX], PNoTitle]
                  $ []
                , layer [asSpec plot1, asSpec plot2]
                ]
