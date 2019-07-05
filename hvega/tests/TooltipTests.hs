{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite TooltipTests.elm as of version 1.12.0
--
module TooltipTests (testSpecs) where

import Graphics.Vega.VegaLite hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("tooltip1", tooltip1)
            , ("tooltip2", tooltip2)
            ]
            
tooltip1 :: VegaLite
tooltip1 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C", "D", "E", "F", "G", "H", "I" ])
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        enc =
            encoding
                . position X [ PName "a", PmType Ordinal ]
                . position Y [ PName "b", PmType Quantitative ]
                . tooltip [ TName "b", TmType Quantitative ]
    in
    toVegaLite [ dataVals [], mark Bar [], enc [] ]

tooltip2 :: VegaLite
tooltip2 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C", "D", "E", "F", "G", "H", "I" ])
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        enc =
            encoding
                . position X [ PName "a", PmType Ordinal ]
                . position Y [ PName "b", PmType Quantitative ]
                . tooltips
                    [ [ TName "a", TmType Ordinal ]
                    , [ TName "b", TmType Quantitative ]
                    ]
    in
    toVegaLite [ dataVals [], mark Bar [], enc [] ]
