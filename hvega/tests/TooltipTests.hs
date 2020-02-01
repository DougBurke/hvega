{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite TooltipTests.elm as of version 1.12.0
--
module TooltipTests (testSpecs) where

import Graphics.Vega.VegaLite hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("tooltip1", tooltip1)
            , ("tooltip2", tooltip2)
            , ("notips", notips)
            ]


with :: BuildEncodingSpecs -> VegaLite
with encs =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C", "D", "E", "F", "G", "H", "I" ])
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        enc =
            encoding
                . position X [ PName "a", PmType Ordinal ]
                . position Y [ PName "b", PmType Quantitative ]
                . encs
    in
    toVegaLite [ dataVals [], mark Bar [], enc [] ]


tooltip1 :: VegaLite
tooltip1 = with (tooltip [ TName "b", TmType Quantitative ])


tooltip2 :: VegaLite
tooltip2 = with (tooltips
                    [ [ TName "a", TmType Ordinal ]
                    , [ TName "b", TmType Quantitative ]
                    ])


notips :: VegaLite
notips = with (tooltip [])
