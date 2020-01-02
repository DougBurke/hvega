{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryLine.elm (from development of version
-- 1.13.0)
--
-- TODO: update to v2.0 Elm test suite

module Gallery.Line (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup, repeat)


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
            ]


line1 :: VegaLite
line1 =
    let
        des =
            description "Google's stock price over time."

        trans =
            transform . filter (FExpr "datum.symbol === 'GOOG'")

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PAxis [ AxFormat "%Y" ] ]
                . position Y [ PName "price", PmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []
        , trans []
        , mark Line []
        , enc []
        ]


line2 :: VegaLite
line2 =
    let
        des =
            description "Google's stock price over time with point markers."

        trans =
            transform . filter (FExpr "datum.symbol === 'GOOG'")

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PAxis [ AxFormat "%Y" ] ]
                . position Y [ PName "price", PmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []
        , trans []
        , mark Line [ MColor "green", MPoint (PMMarker [ MColor "purple" ]) ]
        , enc []
        ]


line3 :: VegaLite
line3 =
    let
        des =
            description "Stock prices of 5 tech companies over time."

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PAxis [ AxFormat "%Y" ] ]
                . position Y [ PName "price", PmType Quantitative ]
                . color [ MName "symbol", MmType Nominal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []
        , mark Line []
        , enc []
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
    let
        des =
            description "Google's stock price over time (quantized as a step-chart)."

        trans =
            transform . filter (FExpr "datum.symbol === 'GOOG'")

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PAxis [ AxFormat "%Y" ] ]
                . position Y [ PName "price", PmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []
        , trans []
        , mark Line [ MInterpolate StepAfter ]
        , enc []
        ]


line6 :: VegaLite
line6 =
    let
        des =
            description "Google's stock price over time (smoothed with monotonic interpolation)."

        trans =
            transform . filter (FExpr "datum.symbol === 'GOOG'")

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PAxis [ AxFormat "%Y" ] ]
                . position Y [ PName "price", PmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []
        , trans []
        , mark Line [ MInterpolate Monotone ]
        , enc []
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
                . order [ OName "year", OmType Temporal ]
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
            encoding
                . position X [ PName "date", PmType Temporal, PAxis [ AxFormat "%Y" ] ]
                . position Y [ PName "price", PmType Quantitative ]
                . size [ MName "price", MmType Quantitative ]
                . color [ MName "symbol", MmType Nominal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []
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
            configure . configuration (View [ ViewStroke Nothing ])
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
