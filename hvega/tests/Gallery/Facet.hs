{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryFacet.elm (from development of version
-- 1.13.0)
--
module Gallery.Facet (testSpecs) where

import Graphics.Vega.VegaLite

import qualified Data.Text as T

import Prelude hiding (filter, lookup, repeat)

import Data.Aeson (Value, (.=), object, toJSON)


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("facet1", facet1)
            , ("facet2", facet2)
            , ("facet3", facet3)
            , ("facet4", facet4)
            , ("facet5", facet5)
            , ("facet6", facet6)
            , ("facet7", facet7)
            , ("trellisareaseattle", trellisAreaSeattle)
            , ("facetgridbar", facetGridBar)
            , ("facet_bullet", facetBullet)
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
                                , FTimeUnit (TU Hours)
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


-- https://vega.github.io/vega-lite/examples/facet_grid_bar.html
facetGridBar :: VegaLite
facetGridBar =
  let dvals = dataFromColumns []
              . dataColumn "a" (Strings xa)
              . dataColumn "b" (Strings xb)
              . dataColumn "c" (Strings xc)
              . dataColumn "p" (Strings xp)
              $ []

      xa = [ "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1"
           , "a2", "a2", "a2", "a2", "a2", "a2", "a2", "a2", "a2"
           , "a3", "a3", "a3", "a3", "a3", "a3", "a3", "a3", "a3"
           ]
      xb = [ "b1", "b1", "b1", "b2", "b2", "b2", "b3", "b3", "b3"
           , "b1", "b1", "b1", "b2", "b2", "b2", "b3", "b3", "b3"
           , "b1", "b1", "b1", "b2", "b2", "b2", "b3", "b3", "b3"
           ]
      xc = [ "x", "y", "z", "x", "y", "z", "x", "y", "z"
           , "x", "y", "z", "x", "y", "z", "x", "y", "z"
           , "x", "y", "z", "x", "y", "z", "x", "y", "z"
           ]
      xp = [ "0.14", "0.60", "0.03", "0.80", "0.38", "0.55", "0.11", "0.58", "0.79"
           , "0.83", "0.87", "0.67", "0.97", "0.84", "0.90", "0.74", "0.64", "0.19"
           , "0.57", "0.35", "0.49", "0.91", "0.38", "0.91", "0.99", "0.80", "0.37"
           ]
        
      enc = encoding
            . position X [ PName "p"
                         , PmType Quantitative
                         , PAxis [AxFormat "%"]
                         , PNoTitle
                         ]
            . position Y [ PName "c"
                         , PmType Nominal
                         , PAxis []
                         ]
            . color [ MName "c"
                    , MmType Nominal
                    , MLegend [LOrient LOBottom, LTitleOrient SLeft]
                    , MTitle "settings"
                    ]
            . row [ FName "a"
                  , FmType Nominal
                  , FTitle "Factor A"
                  , FHeader [HLabelAngle 0]
                  ]
            . column [ FName "b"
                     , FmType Nominal
                     , FTitle "Factor B"
                     ]

  in toVegaLite [ description "A simple grid of bar charts to compare performance data."
                , dvals
                , width 60
                , heightStep 8
                , spacing 5
                , mark Bar []
                , enc []
                ]


bullets :: Value
bullets =
  let item :: T.Text -> T.Text -> [Double] -> [Double] -> [Double] -> Value
      item ttl subtitle ranges measures markers =
        object [ "title" .= ttl
               , "subtitle" .= subtitle
               , "ranges" .= ranges
               , "measures" .= measures
               , "markers" .= markers
               ]

  in toJSON [ item "Revenue" "US$, in thousands" [150,225,300] [220,270] [250]
            , item "Profit" "%" [20,25,30] [21,23] [26]
            , item "Order Size" "US$, average" [350,500,600] [100,320] [550]
            , item "New Customers" "count" [1400,2000,2500] [1000,1650] [2100]
            , item "Satisfaction" "out of 5" [3.5,4.25,5] [3.2,4.7] [4.4]
            ]


-- https://vega.github.io/vega-lite/examples/facet_bullet.html
facetBullet :: VegaLite
facetBullet =
  let facetOpts = facet [ RowBy [ FName "title"
                                , FmType Ordinal
                                , FHeader [ HLabelAngle 0
                                          , HNoTitle ]
                                ]
                        ]

      plot = asSpec [ encoding
                      (position X [ PmType Quantitative
                                  , PScale [SNice (IsNice False)]
                                  , PNoTitle
                                  ] [])
                    , layer (map asSpec plots)
                    ]

      encX fld = encoding (position X [PName fld] [])

      plots = [ [ mark Bar [MColor "#eee"]
                , encX "ranges[2]" ]
              , [ mark Bar [MColor "#ddd"]
                , encX "ranges[1]" ]
              , [ mark Bar [MColor "#ccc"]
                , encX "ranges[0]" ]
              , [ mark Bar [MColor "lightsteelblue", MSize 10]
                , encX "measures[1]" ]
              , [ mark Bar [MColor "steelblue", MSize 10]
                , encX "measures[0]" ]
              , [ mark Tick [MColor "black"]
                , encX "markers[0]" ]
              ]

      res = resolve . resolution (RScale [(ChX, Independent)])

      cfg = configure . configuration (TickStyle [MThickness 2])

  in toVegaLite [ dataFromJson bullets []
                , facetOpts
                , spacing 10
                , specification plot
                , res []
                , cfg []
                ]
