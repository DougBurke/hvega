{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryArea.elm (from development of version
-- 1.13.0)
--
module Gallery.Area (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup)
import Data.Function ((&))


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("area1", area1)
            , ("area2", area2)
            , ("area3", area3)
            , ("area4", area4)
            , ("area5", area5)
            , ("area6", area6)
            , ("area7", area7)
            , ("lasagna", lasagna)
            ]


stockData :: Data
stockData = dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []


area1 :: VegaLite
area1 =
    let
        des =
            description "Unemployment over time (area chart)"

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PTimeUnit (TU YearMonth), PAxis [ AxFormat "%Y" ] ]
                . position Y [ PName "count", PmType Quantitative, PAggregate Sum, PAxis [ AxTitle "Count" ] ]
    in
    toVegaLite
        [ des
        , width 300
        , height 200
        , dataFromUrl "https://vega.github.io/vega-lite/data/unemployment-across-industries.json" []
        , mark Area []
        , enc []
        ]


area2 :: VegaLite
area2 =
    let
        des =
            description "Area chart with overlaid lines and point markers"

        trans =
            transform . filter (FExpr "datum.symbol === 'GOOG'")

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PAxis [ AxFormat "%Y" ] ]
                . position Y [ PName "price", PmType Quantitative ]
    in
    toVegaLite
        [ des
        , stockData
        , trans []
        , mark Area [ MPoint (PMMarker []), MLine (LMMarker []) ]
        , enc []
        ]


area3 :: VegaLite
area3 =
    let
        des =
            description "Unemployment across industries as a stacked area chart."

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PTimeUnit (TU YearMonth), PAxis [ AxFormat "%Y" ] ]
                . position Y [ PName "count", PmType Quantitative, PAggregate Sum ]
                . color [ MName "series", MmType Nominal, MScale [ SScheme "category20b" [] ] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/unemployment-across-industries.json" []
        , mark Area []
        , enc []
        ]


area4 :: VegaLite
area4 =
    let
        des =
            description "Unemployment across industries as a normalised area chart."

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PTimeUnit (TU YearMonth), PAxis [ AxDomain False, AxFormat "%Y" ] ]
                . position Y [ PName "count", PmType Quantitative, PAggregate Sum, PAxis [], PStack StNormalize ]
                . color [ MName "series", MmType Nominal, MScale [ SScheme "category20b" [] ] ]
    in
    toVegaLite
        [ des
        , width 300
        , height 200
        , dataFromUrl "https://vega.github.io/vega-lite/data/unemployment-across-industries.json" []
        , mark Area []
        , enc []
        ]


area5 :: VegaLite
area5 =
    let
        des =
            description "Unemployment across industries as a streamgraph (centred, stacked area chart)."

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PTimeUnit (TU YearMonth), PAxis [ AxDomain False, AxFormat "%Y" ] ]
                . position Y [ PName "count", PmType Quantitative, PAggregate Sum, PAxis [], PStack StCenter ]
                . color [ MName "series", MmType Nominal, MScale [ SScheme "category20b" [] ] ]
    in
    toVegaLite
        [ des
        , width 300
        , height 200
        , dataFromUrl "https://vega.github.io/vega-lite/data/unemployment-across-industries.json" []
        , mark Area []
        , enc []
        ]


area6 :: VegaLite
area6 =
    let
        des =
            description "Horizon chart with 2 layers. (See https://idl.cs.washington.edu/papers/horizon/ for more details on horizon charts.)"

        dvals =
            dataFromColumns []
                . dataColumn "x" (Numbers [1..20])
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
    toVegaLite [ des, width 300, height 50, dvals [], encX [], layer [ specLower, specUpper ], config [] ]


area7 :: VegaLite
area7 =
    let
        cars =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        trans =
            transform
                . aggregate [ opAs Count "" "count_*" ] [ "Origin", "Cylinders" ]
                . stack "count_*"
                    []
                    "stack_count_Origin1"
                    "stack_count_Origin2"
                    [ StOffset StNormalize, StSort [ WAscending "Origin" ] ]
                . window
                    [ ( [ WAggregateOp Min, WField "stack_count_Origin1" ], "x" )
                    , ( [ WAggregateOp Max, WField "stack_count_Origin2" ], "x2" )
                    , ( [ WOp DenseRank ], "rank_Cylinders" )
                    , ( [ WAggregateOp Distinct, WField "Cylinders" ], "distinct_Cylinders" )
                    ]
                    [ WFrame Nothing Nothing, WGroupBy [ "Origin" ], WSort [ WAscending "Cylinders" ] ]
                . window
                    [ ( [ WOp DenseRank ], "rank_Origin" ) ]
                    [ WFrame Nothing Nothing, WSort [ WAscending "Origin" ] ]
                . stack "count_*"
                    [ "Origin" ]
                    "y"
                    "y2"
                    [ StOffset StNormalize, StSort [ WAscending "Cylinders" ] ]
                . calculateAs "datum.y + (datum.rank_Cylinders - 1) * datum.distinct_Cylinders * 0.01 / 3" "ny"
                . calculateAs "datum.y2 + (datum.rank_Cylinders - 1) * datum.distinct_Cylinders * 0.01 / 3" "ny2"
                . calculateAs "datum.x + (datum.rank_Origin - 1) * 0.01" "nx"
                . calculateAs "datum.x2 + (datum.rank_Origin - 1) * 0.01" "nx2"
                . calculateAs "(datum.nx+datum.nx2)/2" "xc"
                . calculateAs "(datum.ny+datum.ny2)/2" "yc"

        enc1 =
            encoding
                . position X
                    [ PName "xc"
                    , PmType Quantitative
                    , PAggregate Min
                    , PTitle "Origin"
                    , PAxis [ AxOrient STop ]
                    ]
                . color [ MName "Origin", MmType Nominal, MLegend [] ]
                . text [ TName "Origin", TmType Nominal ]

        spec1 =
            asSpec [ mark Text [ MBaseline AlignMiddle, MAlign AlignCenter ], enc1 [] ]

        enc2 =
            encoding
                . position X [ PName "nx", PmType Quantitative, PAxis [] ]
                . position X2 [ PName "nx2" ]
                . position Y [ PName "ny", PmType Quantitative, PAxis [] ]
                . position Y2 [ PName "ny2" ]
                . color [ MName "Origin", MmType Nominal, MLegend [] ]
                . opacity [ MName "Cylinders", MmType Quantitative, MLegend [] ]
                . tooltips
                    [ [ TName "Origin", TmType Nominal ]
                    , [ TName "Cylinders", TmType Quantitative ]
                    ]

        spec2 =
            asSpec [ mark Rect [], enc2 [] ]

        enc3 =
            encoding
                . position X [ PName "xc", PmType Quantitative, PAxis [] ]
                . position Y [ PName "yc", PmType Quantitative, PAxis [ AxTitle "Cylinders" ] ]
                . text [ TName "Cylinders", TmType Nominal ]

        spec3 =
            asSpec [ mark Text [ MBaseline AlignMiddle ], enc3 [] ]

        cfg =
            configure
                . configuration (ViewStyle [ ViewNoStroke ])
                . configuration (Axis [ Domain False, Ticks False, Labels False, Grid False ])
                . configuration (ConcatStyle [ CompSpacing 10 ])

        res =
            resolve
                . resolution (RScale [ ( ChX, Shared ) ])
    in
    toVegaLite
        [ cfg []
        , res []
        , cars
        , trans []
        , vConcat [ spec1, asSpec [ layer [ spec2, spec3 ] ] ]
        ]


-- https://vega.github.io/vega-lite/examples/rect_lasagna.html
lasagna :: VegaLite
lasagna =
  let trans = transform
              . filter (FExpr "datum.symbol !== 'GOOG'")
              $ []

      xAxis = [ AxFormat "%Y"
              , AxLabelAngle 0
              , AxLabelOverlap ONone
              , AxDataCondition
                xCondition
                (CAxLabelColor "black" "")
              , AxDataCondition
                xCondition
                (CAxTickColor "black" "")
              ]

      xCondition = FEqual "value" (DateTime [DTMonthNum 1, DTDate 1])
                   & FilterOpTrans (MTimeUnit (TU MonthDate))
                   
      enc = encoding
            . position X [ PTimeUnit (TU YearMonthDate)
                         , PName "date"
                         , PmType Ordinal
                         , PTitle "Time"
                         , PAxis xAxis
                         ]
            .position Y [ PName "symbol"
                        , PmType Nominal
                        , PNoTitle
                        ]
            . color [ MAggregate Sum
                    , MName "price"
                    , MmType Quantitative
                    , MTitle "Price"
                    ]

  in toVegaLite [ trans
                , width 300
                , height 100
                , stockData
                , mark Rect []
                , enc []
                ]
