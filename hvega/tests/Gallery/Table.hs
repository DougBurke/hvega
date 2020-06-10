{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryTable.elm (from development of version
-- 1.13.0)
--
module Gallery.Table (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup, repeat)

import Data.Function ((&))

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("table1", table1)
            , ("table2", table2)
            , ("table3", table3)
            , ("table4", table4)
            , ("table5", table5)
            ]


table1 :: VegaLite
table1 =
    let
        des =
            description "'Table heatmap' showing engine size/power for three countries."

        enc =
            encoding
                . position X [ PName "Cylinders", PmType Ordinal ]
                . position Y [ PName "Origin", PmType Nominal ]
                . color [ MName "Horsepower", MmType Quantitative, MAggregate Mean ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , mark Rect []
        , enc []
        ]


table2 :: VegaLite
table2 =
    let
        des =
            description "Annual weather 'heatmap'"

        conf =
            configure
                . configuration (ViewStyle [ ViewStrokeWidth 0, ViewStep 13 ])
                . configuration (Axis [ Domain False ])

        enc =
            encoding
                . position X [ PName "date", PmType Ordinal, PTimeUnit (TU Date), PAxis [ AxTitle "Day", AxLabelAngle 0, AxFormat "%e" ] ]
                . position Y [ PName "date", PmType Ordinal, PTimeUnit (TU Month), PAxis [ AxTitle "Month" ] ]
                . color [ MName "temp", MmType Quantitative, MAggregate Max, MLegend [ LNoTitle ] ]
    in
    toVegaLite
        [ des
        , conf []
        , dataFromUrl "https://vega.github.io/vega-lite/data/seattle-temps.csv" []
        , mark Rect []
        , enc []
        ]


table3 :: VegaLite
table3 =
    let
        des =
            description "'Binned heatmap' comparing movie ratings."

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json"

        trans =
            transform
                . filter
                    (FCompose
                        (And
                            (FValid "IMDB_Rating" & FilterOp)
                            (FValid "Rotten_Tomatoes_Rating" & FilterOp)
                        )
                    )

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PBin [ MaxBins 60 ] ]
                . position Y [ PName "Rotten_Tomatoes_Rating", PmType Quantitative, PBin [ MaxBins 40 ] ]
                . color [ MmType Quantitative, MAggregate Count ]

        config =
            configure
                . configuration (RangeStyle [ RHeatmap "greenblue" ])
                . configuration (ViewStyle [ ViewNoStroke ])
    in
    toVegaLite
        [ des
        , width 300
        , height 200
        , dvals []
        , trans []
        , mark Rect []
        , enc []
        , config []
        ]


table4 :: VegaLite
table4 =
    let
        des =
            description "Table bubble plot in the style of a Github punched card."

        enc =
            encoding
                . position X [ PName "time", PmType Ordinal, PTimeUnit (TU Hours) ]
                . position Y [ PName "time", PmType Ordinal, PTimeUnit (TU Day) ]
                . size [ MName "count", MmType Quantitative, MAggregate Sum ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/github.csv" []
        , mark Circle []
        , enc []
        ]


table5 :: VegaLite
table5 =
    let
        des =
            description "Layering text over 'heatmap'."

        encPosition =
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

        config =
            configure
                . configuration (ScaleStyle [ SCBandPaddingInner 0, SCBandPaddingOuter 0 ])
                . configuration (TextStyle [ MBaseline AlignMiddle ])
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , encPosition []
        , layer [ specRect, specText ]
        , config []
        ]
