{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite SortTests.elm as of version 1.12.0
--
module SortTests (testSpecs) where

import qualified Data.Text as T

import Graphics.Vega.VegaLite hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("ascending", sortAsc)
            , ("descending", sortDesc)
            , ("weighted", sortWeight)
            , ("custom", sortCustom)
            , ("stack1", stack1)
            ]


sortQuant :: T.Text -> [SortProperty] -> VegaLite
sortQuant yField sps =
    let
        datavals =
            dataFromColumns []
                . dataColumn "Horsepower" (Numbers [ 1, 5, 2, 3, 4 ])
                . dataColumn "Weight_in_lbs" (Numbers [ 19, 21, 58, 12, 13 ])

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative, PSort sps ]
                . position Y [ PName yField, PmType Quantitative ]
                . order [ OName yField, OmType Ordinal ]
    in
    toVegaLite [ height 300, datavals [], enc [], mark Line [ MStrokeWidth 0.5 ] ]


sortAsc, sortDesc :: VegaLite
sortAsc = sortQuant "Horsepower" [ Ascending ]
sortDesc = sortQuant "Horsepower" [ Descending ]

sortWeight :: VegaLite
sortWeight =
    sortQuant "Weight_in_lbs" [ ByFieldOp "Weight_in_lbs" Mean ]

sortCustom :: VegaLite
sortCustom =
    let
        datavals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C", "Z", "Y", "X" ])
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53 ])

        enc =
            encoding
                . position X
                    [ PName "a"
                    , PmType Ordinal
                    , PSort [ CustomSort (Strings [ "B", "A", "C" ]) ]
                    ]
                . position Y [ PName "b", PmType Quantitative ]
    in
    toVegaLite [ datavals [], enc [], mark Bar [] ]

stack1 :: VegaLite
stack1 =
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
                    ]
                    [ WFrame Nothing Nothing, WGroupBy [ "Origin" ] ]
                . stack "count_*"
                    [ "Origin" ]
                    "y"
                    "y2"
                    [ StOffset StNormalize, StSort [ WAscending "Cylinders" ] ]

        enc =
            encoding
                . position X [ PName "x", PmType Quantitative, PAxis [] ]
                . position X2 [ PName "x2" ]
                . position Y [ PName "y", PmType Quantitative, PAxis [] ]
                . position Y2 [ PName "y2" ]
                . color [ MName "Origin", MmType Nominal ]
                . opacity [ MName "Cylinders", MmType Quantitative, MLegend [] ]
                . tooltips
                    [ [ TName "Origin", TmType Nominal ]
                    , [ TName "Cylinders", TmType Quantitative ]
                    ]
    in
    toVegaLite [ cars, trans [], enc [], mark Rect [] ]
