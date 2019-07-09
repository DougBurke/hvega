{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ViewCompositionTests.elm as of version 1.12.0
--
module ViewCompositionTests (testSpecs) where

import qualified Prelude as P

import Graphics.Vega.VegaLite

import Prelude hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("columns1", columns1)
            , ("columns2", columns2)
            , ("columns3", columns3)
            , ("columns4", columns4)
            , ("grid1", grid1)
            , ("grid2", grid2)
            , ("grid3", grid3)
            , ("grid4", grid4)
            , ("grid5", grid5)
            ]


genderChart :: [HeaderProperty] -> [HeaderProperty] -> VegaLite
genderChart hdProps cProps =
    let
        conf =
            configure . configuration (HeaderStyle cProps)

        pop =
            dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []

        trans =
            transform
                . filter (FExpr "datum.year == 2000")
                . calculateAs "datum.sex == 2 ? 'Female' : 'Male'" "gender"

        enc =
            encoding
                . column
                    [ FName "gender"
                    , FmType Nominal
                    , FHeader hdProps
                    ]
                . position X
                    [ PName "age"
                    , PmType Ordinal
                    , PScale [ SRangeStep (Just 17) ]
                    ]
                . position Y
                    [ PName "people"
                    , PmType Quantitative
                    , PAggregate Sum
                    , PAxis [ AxTitle "Population" ]
                    ]
                . color
                    [ MName "gender"
                    , MmType Nominal
                    , MScale [ SRange (RStrings [ "#EA98D2", "#659CCA" ]) ]
                    ]
    in
    toVegaLite [ conf [], pop, trans [], enc [], mark Bar [] ]


columns1, columns2, columns3, columns4 :: VegaLite
columns1 = genderChart [] []
columns2 = genderChart [ HTitleFontSize 20, HLabelFontSize 15 ] []
columns3 = genderChart [] [ HTitleFontSize 20, HLabelFontSize 15 ]
columns4 =
    genderChart
        [ HTitleFontSize 20
        , HLabelFontSize 15
        , HTitlePadding (-27)
        , HLabelPadding 40
        ]
        []

-- is this in the prelude
repeatN :: Int -> a -> [a]
repeatN n = take n . P.repeat

dataVals :: [DataColumn] -> Data
dataVals =
    let
        rows =
            Numbers $ concatMap (repeatN (3 * 5)) [ 1, 2, 3, 4 ]

        cols =
            Numbers $ concat $ repeatN 4 $ concatMap (repeatN 3) [ 1, 2, 3, 4, 5 ]

        cats =
            Numbers $ concat $ repeatN (4 * 5) [ 1, 2, 3 ]

        vals =
            Numbers $ [ 30, 15, 12, 25, 30, 25, 10, 28, 11, 18, 24, 16, 10, 10, 10 ]
                ++ [ 8, 8, 29, 11, 24, 12, 26, 32, 9, 8, 18, 28, 8, 20, 24 ]
                ++ [ 21, 15, 20, 4, 13, 12, 27, 21, 14, 5, 1, 2, 11, 2, 5 ]
                ++ [ 14, 20, 24, 20, 2, 9, 15, 14, 13, 22, 30, 30, 10, 8, 12 ]

    in
    dataFromColumns []
        . dataColumn "row" rows
        . dataColumn "col" cols
        . dataColumn "cat" cats
        . dataColumn "val" vals

cfg :: [LabelledSpec] -> (VLProperty, VLSpec)
cfg =
    -- Styling to remove axis gridlines and labels
    configure
        . configuration (HeaderStyle [ HLabelFontSize 0.1 ])
        . configuration (View [ Stroke Nothing, ViewHeight 120 ])
        . configuration (FacetStyle [ FSpacing 80, FColumns 5 ])

grid1 :: VegaLite
grid1 =
    let
        encByCatVal =
            encoding
                . position X [ PName "cat", PmType Ordinal, PAxis [] ]
                . position Y [ PName "val", PmType Quantitative, PAxis [] ]
                . color [ MName "cat", MmType Nominal, MLegend [] ]

        specByCatVal =
            asSpec [ width 120, height 120, mark Bar [], encByCatVal [] ]
    in
    toVegaLite
        [ cfg []
        , dataVals []
        , spacingRC 20 80
        , specification specByCatVal
        , facet
            [ RowBy [ FName "row", FmType Ordinal, FNoTitle ]
            , ColumnBy [ FName "col", FmType Ordinal, FHeader [ HNoTitle ] ]
            ]
        ]


grid2 :: VegaLite
grid2 =
    let
        encByCatVal =
            encoding
                . position X [ PName "cat", PmType Ordinal, PAxis [] ]
                . position Y [ PName "val", PmType Quantitative, PAxis [] ]
                . color [ MName "cat", MmType Nominal, MLegend [] ]

        specByCatVal =
            asSpec [ width 120, height 120, mark Bar [], encByCatVal [] ]

        trans =
            transform
                . calculateAs "datum.row * 1000 + datum.col" "index"
    in
    toVegaLite
        [ cfg []
        , dataVals []
        , trans []
        , columns (Just 5)
        , specification specByCatVal
        , facetFlow [ FName "index", FmType Ordinal, FHeader [ HNoTitle ] ]
        ]


-- TODO: columns Nothing maps to "columns": null, which is not to spec;
--       should it be "columns": 0?

grid3 :: VegaLite
grid3 =
    let
        encByCatVal =
            encoding
                . position X [ PName "cat", PmType Ordinal, PAxis [] ]
                . position Y [ PName "val", PmType Quantitative, PAxis [] ]
                . color [ MName "cat", MmType Nominal, MLegend [] ]

        specByCatVal =
            asSpec [ width 120, height 120, mark Bar [], encByCatVal [] ]

        trans =
            transform
                . calculateAs "datum.row * 1000 + datum.col" "index"
    in
    toVegaLite
        [ cfg []
        , dataVals []
        , trans []
        , columns Nothing
        , specification specByCatVal
        , facetFlow [ FName "index", FmType Ordinal, FHeader [ HNoTitle ] ]
        ]


grid4 :: VegaLite
grid4 =
    let
        carData =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json"

        enc =
            encoding
                . position X [ PRepeat Flow, PmType Quantitative, PBin [] ]
                . position Y [ PmType Quantitative, PAggregate Count ]
                . color [ MName "Origin", MmType Nominal ]

        spec =
            asSpec [ carData [], mark Bar [], enc [] ]
    in
    toVegaLite
        [ columns (Just 3)
        , repeatFlow [ "Horsepower", "Miles_per_Gallon", "Acceleration", "Displacement", "Weight_in_lbs" ]
        , specification spec
        ]


grid5 :: VegaLite
grid5 =
    let
        carData =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json"

        enc =
            encoding
                . position X [ PRepeat Row, PmType Quantitative, PBin [] ]
                . position Y [ PmType Quantitative, PAggregate Count ]
                . color [ MName "Origin", MmType Nominal ]

        spec =
            asSpec [ carData [], mark Bar [], enc [] ]
    in
    toVegaLite
        [ repeat
            [ RowFields [ "Horsepower", "Miles_per_Gallon", "Acceleration", "Displacement", "Weight_in_lbs" ]
            ]
        , specification spec
        ]


