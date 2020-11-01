{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ScaleTests.elm as of version 1.12.0
--
module ScaleTests (testSpecs) where

import Graphics.Vega.VegaLite hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("scale1", scale1)
            , ("scale2", scale2)
            , ("scale3", scale3)
            , ("scale4", scale4)
            , ("scale5", scale5)
            , ("scale6", scale6)
            , ("scale7", scale7)
            , ("scale8", scale8)
            , ("scale9", scale9)
            , ("diverging1", diverging1)
            , ("diverging2", diverging2)
            , ("axisrange", axisrange)
            , ("axislimit", axislimit)
            , ("namedaxisrange", namedAxisRange)
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
                . configuration (RangeStyle [ RRamp "reds" ])

        cars =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Acceleration", MmType Quantitative, MBin [] ]
    in
    toVegaLite [ conf [], cars, enc [], mark Point [] ]


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


rData :: [ScaleProperty] -> VegaLite
rData scaleOpts =
  let dataVals = dataFromColumns []
                 . dataColumn "r" (Numbers [ 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ])

      enc = encoding
            . size [ MName "r"
                   , MmType Quantitative
                   , MScale (SRange (RPair 0 80000) : scaleOpts)
                   , MLegend []
                   ]

  in toVegaLite [ dataVals [], mark Point [], enc [] ]


scale6, scale7, scale8, scale9 :: VegaLite
scale6 = rData []
scale7 = rData [SType ScPow, SExponent 2]
scale8 = rData [SType ScPow, SExponent 1.2]
scale9 = rData [SType ScLog, SBase (exp 1)]


divergingData :: Data
divergingData =
  dataFromColumns []
  . dataColumn "category" (Strings ["A", "B", "C", "D", "E", "F", "G", "H", "I"])
  . dataColumn "value" (Numbers [-28.6, -1.6, -13.6, 34.4, 24.4, -3.6, -57.6, 30.4, -4.6])
  $ []

divergingEnc :: [ScaleProperty] -> PropertySpec
divergingEnc sopts =
  encoding
  . position X [ PName "category"
               , PmType Ordinal
               , PAxis [ AxLabelAngle 0, AxDomain False, AxOrient STop ]
               ]
  . position Y [ PName "value", PmType Quantitative ]
  . color [ MName "value"
          , MmType Quantitative
          , MScale ([ SScheme "redblue" [] ] ++ sopts)
          ]
  $ []

diverging1 :: VegaLite
diverging1 = toVegaLite [ divergingData
                        , divergingEnc []
                        , mark Bar []
                        ]

diverging2 :: VegaLite
diverging2 = toVegaLite [ divergingData
                        , divergingEnc [ SDomainOpt (DMid 0) ]
                        , mark Bar []
                        ]


axes :: [ScaleProperty] -> [ScaleProperty] -> VegaLite
axes xscale yscale =
  let cars = dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

      ax axis vals = [ PName axis, PmType Quantitative, PScale vals ]
      enc = encoding
            . position X (ax "Horsepower" xscale)
            . position Y (ax "Miles_per_Gallon" yscale)
            . size [ MName "Acceleration", MmType Quantitative, MBin [] ]
            . opacity [ MName "Acceleration", MmType Quantitative, MBin [] ]
            
  in toVegaLite [ cars
                , enc []
                , mark Point [ MFilled True, MStroke "white", MStrokeWidth 0.4 ]
                ]


axisrange, axislimit :: VegaLite
axisrange = axes [SRange (RWidth 50)] [SRange (RHeight 60)]
axislimit = axes
            [SDomainOpt (DMin (-10)), SDomainOpt (DMax 300)]
            [SRange (RMin 220), SRange (RMax 10)]


-- Based on https://github.com/vega/vega-lite/issues/6392
--
namedAxisRange :: VegaLite
namedAxisRange =
  let dataVals = dataFromColumns []
                 . dataColumn "col" (Strings ["X", "Y"])
                 . dataColumn "l" (Strings ["A", "B"])
                 . dataColumn "c" (Strings ["#ff0000", "#0000ff"])

      enc = encoding
            . position Y [PName "col", PmType Nominal]
            . color [MName "l", MmType Nominal, MScale [SRange (RField "c")]]

  in toVegaLite [dataVals [], mark Circle [], enc []]
