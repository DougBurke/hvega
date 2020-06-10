{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryError.elm (from development of version
-- 1.13.0)
--
module Gallery.Error (testSpecs) where

import Graphics.Vega.VegaLite

-- import qualified Data.Text as T

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("error1", error1)
            , ("error2", error2)
            , ("error3", error3)
            , ("error4", error4)
            ]


error1 :: VegaLite
error1 =
    let
        des =
            description "Error bars showing confidence intervals"

        encVariety =
            encoding . position Y [ PName "variety", PmType Ordinal ]

        encPoints =
            encoding
                . position X
                    [ PName "yield"
                    , PmType Quantitative
                    , PAggregate Mean
                    , PScale [ SZero False ]
                    , PAxis [ AxTitle "Barley Yield" ]
                    ]
                . color [ MString "black" ]

        specPoints =
            asSpec [ mark Point [ MFilled True ], encPoints [] ]

        encCIs =
            encoding
                . position X [ PName "yield", PmType Quantitative, PAggregate CI0 ]
                . position X2 [ PName "yield", PAggregate CI1 ]

        specCIs =
            asSpec [ mark Rule [], encCIs [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/barley.json" []
        , encVariety []
        , layer [ specPoints, specCIs ]
        ]


error2 :: VegaLite
error2 =
    let
        des =
            description "Error bars showing standard deviations"

        trans =
            transform
                . aggregate [ opAs Mean "yield" "mean", opAs Stdev "yield" "stdev" ] [ "variety" ]
                . calculateAs "datum.mean-datum.stdev" "lower"
                . calculateAs "datum.mean+datum.stdev" "upper"

        encVariety =
            encoding . position Y [ PName "variety", PmType Ordinal ]

        encMeans =
            encoding
                . position X
                    [ PName "mean"
                    , PmType Quantitative
                    , PScale [ SZero False ]
                    , PAxis [ AxTitle "Barley Yield" ]
                    ]
                . color [ MString "black" ]

        specMeans =
            asSpec [ mark Point [ MFilled True ], encMeans [] ]

        encStdevs =
            encoding
                . position X [ PName "upper", PmType Quantitative ]
                . position X2 [ PName "lower" ]

        specStdevs =
            asSpec [ mark Rule [], encStdevs [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/barley.json" []
        , trans []
        , encVariety []
        , layer [ specMeans, specStdevs ]
        ]


error3 :: VegaLite
error3 =
    let
        des =
            description "Line chart with confidence interval band."

        encTime =
            encoding . position X [ PName "Year", PmType Temporal, PTimeUnit (TU Year) ]

        encBand =
            encoding
                . position Y
                    [ PName "Miles_per_Gallon"
                    , PmType Quantitative
                    , PAggregate CI0
                    , PAxis [ AxTitle "Miles/Gallon" ]
                    ]
                . position Y2
                    [ PName "Miles_per_Gallon"
                    , PAggregate CI1
                    ]
                . opacity [ MNumber 0.3 ]

        specBand =
            asSpec [ mark Area [], encBand [] ]

        encLine =
            encoding
                . position Y
                    [ PName "Miles_per_Gallon"
                    , PmType Quantitative
                    , PAggregate Mean
                    ]

        specLine =
            asSpec [ mark Line [], encLine [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , encTime []
        , layer [ specBand, specLine ]
        ]


error4 :: VegaLite
error4 =
    let
        des =
            description "A scatterplot showing horsepower and miles per gallon for various cars with a global mean and standard deviation overlay."

        encPoints =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]

        specPoints =
            asSpec [ mark Point [], encPoints [] ]

        trans =
            transform
                . aggregate
                    [ opAs Mean "Miles_per_Gallon" "mean_MPG"
                    , opAs Stdev "Miles_per_Gallon" "dev_MPG"
                    ]
                    []
                . calculateAs "datum.mean_MPG+datum.dev_MPG" "upper"
                . calculateAs "datum.mean_MPG-datum.dev_MPG" "lower"

        encMean =
            encoding . position Y [ PName "mean_MPG", PmType Quantitative ]

        specMean =
            asSpec [ mark Rule [], encMean [] ]

        encRect =
            encoding
                . position Y [ PName "lower", PmType Quantitative ]
                . position Y2 [ PName "upper" ]
                . opacity [ MNumber 0.2 ]

        specRect =
            asSpec [ mark Rect [], encRect [] ]

        specSpread =
            asSpec [ trans [], layer [ specMean, specRect ] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , layer [ specPoints, specSpread ]
        ]
