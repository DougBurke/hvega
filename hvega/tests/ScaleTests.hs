{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ScaleTests.elm as of version 1.12.0
--
module ScaleTests (testSpecs) where

import Graphics.Vega.VegaLite hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("scale1", scale1)
            , ("scale2", scale2)
            -- , ("scale3", scale3)
            -- , ("scale4", scale4)
            -- , ("scale5", scale5)
            , ("scale6", scale6)
            -- , ("scale7", scale7)
            -- , ("scale8", scale8)
            -- , ("scale9", scale9)
            ]

scale1 :: VegaLite
scale1 =
    let
        cars =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MString "rgb(203,24,29)" ]
                . size [ MName "Acceleration", MmType Quantitative, MBin [] ]
                . opacity [ MName "Acceleration", MmType Quantitative, MBin [] ]
    in
    toVegaLite [ cars, enc [],
                 mark Point [ MFilled True, MStroke "white", MStrokeWidth 0.4 ] ]

scale2 :: VegaLite
scale2 =
    let
        conf =
            configure
                . configuration (Range [ RRamp "reds" ])

        cars =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Acceleration", MmType Quantitative, MBin [] ]
    in
    toVegaLite [ conf [], cars, enc [], mark Point [] ]


{- TODO: add ScQuantile, ScQuantize, AxNoTitle

scale3 :: VegaLite
scale3 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        enc =
            encoding
                . position Y
                    [ PName "b"
                    , PmType Nominal
                    , PSort []
                    , PAxis [ AxTicks False, AxDomain False, AxNoTitle ]
                    ]
                . size
                    [ MName "b"
                    , MmType Quantitative
                    , MScale [ SType ScQuantile ]
                    ]
                . color
                    [ MName "b"
                    , MmType Quantitative
                    , MScale [ SType ScQuantile ]
                    , MLegend [ LTitle "Quantile" ]
                    ]
    in
    toVegaLite [ dataVals [], enc [], mark Circle [] ]


scale4 :: VegaLite
scale4 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        enc =
            encoding
                . position Y
                    [ PName "b"
                    , PmType Nominal
                    , PSort []
                    , PAxis [ AxTicks False, AxDomain False, AxNoTitle ]
                    ]
                . size
                    [ MName "b"
                    , MmType Quantitative
                    , MScale [ SType ScQuantize ]
                    ]
                . color
                    [ MName "b"
                    , MmType Quantitative
                    , MScale [ SType ScQuantize, SZero True ]
                    , MLegend [ LTitle "Quantize" ]
                    ]
    in
    toVegaLite [ dataVals [], enc [], mark Circle [] ]
-}


{- TODO: add ScThreshold
scale5 :: VegaLite
scale5 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        enc =
            encoding
                . position Y
                    [ PName "b"
                    , PmType Nominal
                    , PSort []
                    , PAxis [ AxTicks False, AxDomain False, AxNoTitle ]
                    ]
                . size
                    [ MName "b"
                    , MmType Quantitative
                    , MScale
                        [ SType ScThreshold
                        , SDomain (DNumbers [ 30, 70 ])
                        , SRange (RNumbers [ 80, 200, 320 ])
                        ]
                    ]
                . color
                    [ MName "b"
                    , MmType Quantitative
                    , MScale
                        [ SType ScThreshold
                        , SDomain (DNumbers [ 30, 70 ])
                        , SScheme "viridis" []
                        ]
                    , MLegend [ LTitle "Threshold" ]
                    ]
    in
    toVegaLite [ dataVals [], enc [], mark Circle [] ]
-}


scale6 :: VegaLite
scale6 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "r" (Numbers [ 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ])

        enc =
            encoding
                . size
                    [ MName "r"
                    , MmType Quantitative
                    , MScale [ SRange (RNumbers [ 0, 80000 ]) ]
                    , MLegend []
                    ]
    in
    toVegaLite [ dataVals [], mark Point [], enc [] ]


{- TODO: add SExponent SBase
scale7 :: VegaLite
scale7 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "r" (Numbers [ 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ])

        enc =
            encoding
                . size
                    [ MName "r"
                    , MmType Quantitative
                    , MScale [ SRange (RNumbers [ 0, 80000 ]), SType ScPow, SExponent 2 ]
                    , MLegend []
                    ]
    in
    toVegaLite [ dataVals [], mark Point [], enc [] ]


scale8 :: VegaLite
scale8 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "r" (Numbers [ 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ])

        enc =
            encoding
                . size
                    [ MName "r"
                    , MmType Quantitative
                    , MScale [ SRange (RNumbers [ 0, 80000 ]), SType ScPow, SExponent 1.2 ]
                    , MLegend []
                    ]
    in
    toVegaLite [ dataVals [], mark Point [], enc [] ]


scale9 :: VegaLite
scale9 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "r" (Numbers [ 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ])

        enc =
            encoding
                .  size
                    [ MName "r"
                    , MmType Quantitative
                    , MScale [ SRange (RNumbers [ 0, 80000 ]), SType ScLog, SBase (exp 1) ]
                    , MLegend []
                    ]
    in
    toVegaLite [ dataVals [], mark Point [], enc [] ]
-}
