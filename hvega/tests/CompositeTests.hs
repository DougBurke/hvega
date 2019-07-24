{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite CompositeTests.elm as of version 1.12.0
--
module CompositeTests (testSpecs) where

import qualified Data.Text as T

import Data.Monoid ((<>))

import Graphics.Vega.VegaLite


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("boxplot1", boxplot1)
            , ("boxplot2", boxplot2)
            , ("boxplot3", boxplot3)
            , ("errorband1", errorband1)
            , ("errorband2", errorband2)
            , ("errorbar1", errorbar1)
            , ("errorbar2", errorbar2)
            , ("errorbar3", errorbar3)
            ]

bPlot :: MarkErrorExtent -> VegaLite
bPlot ext =
    let
        pop =
            dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []

        enc =
            encoding
                . position X [ PName "age", PmType Ordinal ]
                . position Y [ PName "people", PmType Quantitative, PAxis [ AxTitle "Population" ] ]
    in
    toVegaLite [ pop, mark Boxplot [ MExtent ext ], enc [] ]

boxplot1 :: VegaLite
boxplot1 = bPlot ExRange

boxplot2 :: VegaLite
boxplot2 = bPlot (IqrScale 2)

boxplot3 :: VegaLite
boxplot3 =
    let
        pop =
            dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []

        enc =
            encoding
                . position X [ PName "age", PmType Ordinal ]
                . position Y [ PName "people", PmType Quantitative, PAxis [ AxTitle "Population" ] ]
    in
    toVegaLite
        [ pop
        , mark Boxplot
            [ MExtent (IqrScale 0.5)
            , MBox [ MColor "firebrick" ]
            , MOutliers [ MColor "black", MStrokeWidth 0.3, MSize 10 ]
            , MMedian [ MSize 18, MFill "black", MStrokeWidth 0 ]
            , MRule [ MStrokeWidth 0.4 ]
            , MTicks [ MSize 8 ]
            ]
        , enc []
        ]


-- Note: at present only called with ci or stdev arguments

eBand :: T.Text -> VegaLite
eBand ext =
    let
        cars =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        label =
            case ext of
                "ci" ->
                    "(95% CI)"

                "stdev" ->
                    "(1 stdev)"

                "stderr" ->
                    "(1 std Error)"

                "range" ->
                    "(min to max)"

                _ ->
                    "(IQR)"

        summary =
            case ext of
                "ci" ->
                    ConfidenceInterval

                "stdev" ->
                    StdDev

                "stderr" ->
                    StdErr

  {-
                "range" ->
                    ExRange
  -}
  
                _ ->
                    Iqr

        enc =
            encoding
                . position X [ PName "Year", PmType Temporal, PTimeUnit Year ]
                . position Y
                    [ PName "Miles_per_Gallon"
                    , PmType Quantitative
                    , PScale [ SZero False ]
                    , PTitle ("Miles per Gallon " <> label)
                    ]

        mopts = [ MExtent summary, MInterpolate Monotone, MBorders [] ]
    in
    toVegaLite [ cars
               , mark ErrorBand mopts
               , enc [] ]


errorband1 :: VegaLite
errorband1 =
    eBand "ci"


errorband2 :: VegaLite
errorband2 =
    eBand "stdev"


eBar :: MarkErrorExtent -> VegaLite
eBar ext =
    let
        barley =
            dataFromUrl "https://vega.github.io/vega-lite/data/barley.json" []

        enc =
            encoding
                . position X [ PName "yield", PmType Quantitative
                             , PScale [ SZero False ] ]
                . position Y
                    [ PName "variety"
                    , PmType Ordinal
                    ]

        mopts = [ MExtent ext, MTicks [ MStroke "blue" ] ]
        
    in
    toVegaLite [ barley
               , mark ErrorBar mopts
               , enc [] ]


errorbar1 :: VegaLite
errorbar1 =
    eBar ConfidenceInterval


errorbar2 :: VegaLite
errorbar2 =
    eBar StdDev


errorbar3 :: VegaLite
errorbar3 =
    let
        des =
            description "Error bars with color encoding"

        specErrorBars =
            asSpec [ mark ErrorBar [ MTicks [] ], encErrorBars [] ]

        encErrorBars =
            encoding
                . position X [ PName "yield", PmType Quantitative
                             , PScale [ SZero False ] ]
                . position Y [ PName "variety", PmType Ordinal ]
                . color [ MString "#4682b4" ]

        specPoints =
            asSpec [ mark Point [ MFilled True, MColor "black" ], encPoints [] ]

        encPoints =
            encoding
                . position X [ PName "yield", PmType Quantitative
                             , PAggregate Mean ]
                . position Y [ PName "variety", PmType Ordinal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/barley.json" []
        , layer [ specErrorBars, specPoints ]
        ]
