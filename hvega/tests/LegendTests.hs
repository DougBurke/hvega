{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite LegendTests.elm as of version 1.12.0
--
module LegendTests (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("legend1", legend1)
            -- , ("legend2", legend2)
            , ("legend3", legend3)
            , ("legend4", legend4)
            , ("legend5", legend5)
            -- , ("legend6", legend6)
            , ("legend7", legend7)
            , ("legend8", legend8)
            , ("legend9", legend9)
            , ("legend10", legend10)
            , ("legend11", legend11)
            ]

legendCore :: [LegendProperty] -> VegaLite
legendCore legProps =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json"

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Origin", MmType Nominal, MLegend legProps ]
                . size [ MName "Horsepower", MmType Quantitative, MLegend legProps ]
                . opacity [ MName "Weight_in_lbs", MmType Quantitative, MLegend legProps ]
    in
    toVegaLite [ width 300, height 300, dataVals [], enc [], mark Circle [] ]

legendCoreCfg :: [LegendConfig] -> VegaLite
legendCoreCfg cfg =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json"

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Origin", MmType Nominal ]
                . size [ MName "Horsepower", MmType Quantitative ]
                . opacity [ MName "Weight_in_lbs", MmType Quantitative ]
    in
    toVegaLite
        [ (configure . configuration (Legend cfg)) []
        , width 300
        , height 300
        , dataVals []
        , enc []
        , mark Circle []
        ]

legend1 :: VegaLite
legend1 = legendCoreCfg []

{- TODO: add LOTop
legend2 :: VegaLite
legend2 = legendCore [ LOrient LOTop ]
-}

legend3 :: VegaLite
legend3 = legendCore [ LOrient LOTopRight ]

legend4 :: VegaLite
legend4 = legendCore [ LOrient LORight ]

legend5 :: VegaLite
legend5 = legendCore [ LOrient LOBottomRight ]

{- TODO: add LOBottom
legend6 :: VegaLite
legend6 = legendCore [ LOrient LOBottom ]
-}

legend7 :: VegaLite
legend7 = legendCore [ LOrient LOBottomLeft ]

legend8 :: VegaLite
legend8 = legendCore [ LOrient LOLeft ]

legend9 :: VegaLite
legend9 = legendCore [ LOrient LOTopLeft ]

legend10 :: VegaLite
legend10 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json"

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Origin", MmType Nominal,
                          MLegend [ LOrient LONone, LeX 232, LeY 5 ] ]
    in
    toVegaLite [ width 300, height 300, dataVals [], enc [], mark Circle [] ]

legend11 :: VegaLite
legend11 =
    legendCoreCfg
        [ LeSymbolStrokeWidth 3
        , LeSymbolStrokeColor "black"
        , LeRowPadding 15
        , LeTitlePadding 20
        , LeStrokeColor "lightgrey"
        , LeStrokeWidth 5
        , LePadding 30
        , LeStrokeDash [ 4, 2, 6, 1 ]
        ]
