{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryFacet.elm (from development of version
-- 1.13.0)
--
module Gallery.Facet (testSpecs) where

import Graphics.Vega.VegaLite

-- import qualified Data.Text as T

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("facet1", facet1)
            , ("facet2", facet2)
            , ("facet3", facet3)
            , ("facet4", facet4)
            , ("facet5", facet5)
            , ("facet6", facet6)
            , ("facet7", facet7)
            , ("trellisareaseattle", trellisAreaSeattle)
            ]


facet1 :: VegaLite
facet1 =
    let
        des =
            description "A trellis bar chart showing the US population distribution of age groups and gender in 2000"

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/population.json"

        trans =
            transform
                . filter (FExpr "datum.year == 2000")
                . calculateAs "datum.sex == 2 ? 'Female' : 'Male'" "gender"

        enc =
            encoding
                . position X [ PName "age", PmType Ordinal ]
                . position Y [ PName "people", PmType Quantitative, PAggregate Sum, PAxis [ AxTitle "Population" ] ]
                . color [ MName "gender", MmType Nominal, MScale [ SRange (RStrings [ "#EA98D2", "#659CCA" ]) ] ]
                . row [ FName "gender", FmType Nominal ]
    in
    toVegaLite [ des, dvals [], trans [], mark Bar [], widthStep 17, enc [] ]


facet2 :: VegaLite
facet2 =
    let
        des =
            description "Barley crop yields in 1931 and 1932 shown as stacked bar charts"

        enc =
            encoding
                . position X [ PName "yield", PmType Quantitative, PAggregate Sum ]
                . position Y [ PName "variety", PmType Nominal ]
                . color [ MName "site", MmType Nominal ]
                . column [ FName "year", FmType Ordinal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/barley.json" []
        , mark Bar []
        , enc []
        ]


facet3 :: VegaLite
facet3 =
    let
        des =
            description "Scatterplots of movie takings vs profits for different MPAA ratings"

        enc =
            encoding
                . position X [ PName "Worldwide_Gross", PmType Quantitative ]
                . position Y [ PName "US_DVD_Sales", PmType Quantitative ]
                . column [ FName "MPAA_Rating", FmType Ordinal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , mark Point []
        , enc []
        ]


facet4 :: VegaLite
facet4 =
    let
        des =
            description "Distributions of car engine power for different countries of origin"

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative, PBin [ MaxBins 15 ] ]
                . position Y [ PmType Quantitative, PAggregate Count ]
                . row [ FName "Origin"
                      , FmType Ordinal
                      , FTitle "Origin facet"
                      ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , mark Bar []
        , enc []
        ]


facet5 :: VegaLite
facet5 =
    let
        des =
            description "Anscombe's Quartet"

        enc =
            encoding
                . position X [ PName "X", PmType Quantitative, PScale [ SZero False ] ]
                . position Y [ PName "Y", PmType Quantitative, PScale [ SZero False ] ]
                . opacity [ MNumber 1 ]
                . column [ FName "Series", FmType Ordinal ]
    in
    toVegaLite [ des, dataFromUrl "https://vega.github.io/vega-lite/data/anscombe.json" [], mark Circle [], enc [] ]


facet6 :: VegaLite
facet6 =
    let
        des =
            description "The Trellis display by Becker et al. helped establish small multiples as a 'powerful mechanism for understanding interactions in studies of how a response depends on explanatory variables'"

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/barley.json"

        enc =
            encoding
                . position X
                    [ PName "yield"
                    , PmType Quantitative
                    , PAggregate Median
                    , PScale [ SZero False ]
                    ]
                . position Y
                    [ PName "variety"
                    , PmType Ordinal
                    , PSort [ ByChannel ChX, Descending ]
                    ]
                . color [ MName "year", MmType Nominal ]
    in
    toVegaLite
        [ des
        , dvals []
        , columns 2
        , facetFlow
            [ FName "site"
            , FmType Ordinal
            , FSort [ ByFieldOp "yield" Median ]
            , FHeader [ HNoTitle ]
            ]
        , specification (asSpec [ enc [], heightStep 12, mark Point [] ])
        ]


facet7 :: VegaLite
facet7 =
    let
        des =
            description "Stock prices of five large companies as a small multiples of area charts"

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv"

        enc =
            encoding
                . position X
                    [ PName "date"
                    , PmType Temporal
                    , PAxis [ AxFormat "%Y", AxNoTitle, AxGrid False ]
                    ]
                . position Y
                    [ PName "price"
                    , PmType Quantitative
                    , PAxis [ AxNoTitle, AxGrid False ]
                    ]
                . color
                    [ MName "symbol"
                    , MmType Nominal
                    , MLegend []
                    ]
                . row
                    [ FName "symbol"
                    , FmType Nominal
                    , FHeader [ HTitle "Stock\nprice"
                              , HTitleLineHeight 20
                              , HLabelAngle 0
                              -- I don't know why the labels only align
                              -- when AlignLeft is used.
                              , HLabelAlign AlignLeft
                              , HLabelExpr "'{' + datum.label + '}'"
                              , HLabelFontStyle "italic"
                              ]
                    ]

        res =
            resolve
                . resolution (RScale [ ( ChY, Independent ) ])

        cfg =
            configure
                . configuration (ViewStyle [ ViewNoStroke ])
    in
    toVegaLite [ des, width 300, height 50, cfg [], res [], dvals [], mark Area [], enc [] ]


-- https://vega.github.io/vega-lite/examples/trellis_area_seattle.html
--
trellisAreaSeattle :: VegaLite
trellisAreaSeattle =
  let desc = description "Average temps in Seattle, by hour"

      ylabels = "hours(datum.value) == 0 ? 'Midnight' : hours(datum.value) == 12 ? 'Noon' : timeFormat(datum.value, '%I:%M %p')"

      plot = asSpec [ width 800
                    , height 25
                    , viewBackground [VBNoStroke]
                    , mark Area []
                    , encoding
                      . position X [ PName "date"
                                   , PmType Temporal
                                   , PTitle "Month"
                                   , PAxis [AxFormat "%b"]
                                   ]
                      . position Y [ PName "temp"
                                   , PmType Quantitative
                                   , PScale [SZero False]
                                   , PAxis [AxNoTitle, AxLabels False, AxTicks False]
                                   ]
                      $ []
                    ]

  in toVegaLite [ desc
                , title "Seattle Annual Temperatures" []
                , dataFromUrl "https://vega.github.io/vega-lite/data/seattle-temps.csv" []
                , transform
                  (calculateAs "(hours(datum.date) + 18) % 24" "order" [])
                , spacing 1  -- NOTE: can not specify this is just for row
                , configure (configuration (Axis [Grid False, Domain False]) [])
                , facet [ RowBy [ FName "date"
                                , FmType Nominal
                                , FTimeUnit Hours
                                , FSort [ByFieldOp "order" Max] -- don't want an operation
                                , FHeader [ HLabelAngle 0
                                          , HLabelPadding 2
                                          , HTitlePadding (-4)
                                          , HLabelAlign AlignLeft
                                          , HLabelExpr ylabels
                                          ]
                                ]
                        ]
                , specification plot
                ]
