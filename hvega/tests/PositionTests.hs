{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite PositionTests.elm as of version 1.12.0
--

module PositionTests (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("position1", position1)
            , ("position2", position2)
            , ("position3", position3)
            , ("position4", position4)
            , ("position5", position5)
            ]


dataVals :: [DataColumn] -> Data
dataVals =
    dataFromColumns []
        . dataColumn "empty" (Numbers [ 0 ])

position1 :: VegaLite
position1 =
    toVegaLite [ dataVals []
               , mark Circle [ MX 150, MY 150, MSize 200 ] ]

position2 :: VegaLite
position2 =
    toVegaLite [ dataVals []
               , mark Bar [ MX 150, MY 150 ] ]


position3 :: VegaLite
position3 =
    toVegaLite [ dataVals []
               , mark Bar [ MX 150, MY 150, MX2 200 ] ]


position4 :: VegaLite
position4 =
    toVegaLite [ dataVals []
               , mark Bar [ MX 150, MY 150, MY2 200 ] ]


position5 :: VegaLite
position5 =
    toVegaLite [ dataVals []
               , mark Bar [ MX 150, MY 150, MX2 200, MY2 200 ] ]
