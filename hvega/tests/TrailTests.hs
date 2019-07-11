{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite TrailTests.elm as of version 1.12.0
--
module TrailTests (testSpecs) where

import Graphics.Vega.VegaLite hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("trail1", trail1)
            , ("trail2", trail2)
            ]


trail1 :: VegaLite
trail1 =
    let
        enc =
            encoding
                . position X [ PName "date", PmType Temporal
                             , PAxis [ AxFormat "%Y" ] ]
                . position Y [ PName "price", PmType Quantitative ]
                . size [ MName "price", MmType Quantitative ]
                . color [ MName "symbol", MmType Nominal ]
    in
    toVegaLite
        [ width 400
        , height 400
        , dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []
        , mark Trail []
        , enc []
        ]


trail2 :: VegaLite
trail2 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/driving.json"

        enc =
            encoding
                . position X [ PName "miles", PmType Quantitative
                             , PScale [ SZero False ] ]
                . position Y [ PName "gas", PmType Quantitative
                             , PScale [ SZero False ] ]
                . size [ MName "year", MmType Temporal, MLegend [] ]
    in
    toVegaLite [ dataVals []
               , mark Trail [ MOrder False ]
               , enc [] ]
