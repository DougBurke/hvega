{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryDist.elm (from development of version
-- 1.13.0)
--
module Gallery.Dist (testSpecs) where

import Graphics.Vega.VegaLite

-- import qualified Data.Text as T

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("dist1", dist1)
            , ("dist2", dist2)
            , ("dist3", dist3)
            , ("dist4", dist4)
            , ("dist5", dist5)
            , ("quantile1", quantile1)
            ]


dist1 :: VegaLite
dist1 =
    let
        des =
            description "Simple histogram of IMDB ratings."

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PBin [] ]
                . position Y [ PmType Quantitative, PAggregate Count ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , mark Bar [ MBinSpacing 0 ]
        , enc []
        ]


dist2 :: VegaLite
dist2 =
    let
        des =
            description "Cumulative frequency distribution"

        trans =
            transform
                . window [ ( [ WAggregateOp Count, WField "count" ], "cumulativeCount" ) ]
                    [ WSort [ WAscending "IMDB_Rating" ], WFrame Nothing (Just 0) ]

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative ]
                . position Y [ PName "cumulativeCount", PmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , trans []
        , mark Area []
        , enc []
        ]


dist3 :: VegaLite
dist3 =
    let
        des =
            description "A layered histogram and cumulative histogram."

        trans =
            transform
                . binAs [] "IMDB_Rating" "bin_IMDB_Rating"
                . aggregate
                    [ opAs Count "" "count" ]
                    [ "bin_IMDB_Rating", "bin_IMDB_Rating_end" ]
                . filter (FExpr "datum.bin_IMDB_Rating !== null")
                . window [ ( [ WAggregateOp Sum, WField "count" ], "cumulativeCount" ) ]
                    [ WSort [ WAscending "bin_IMDB_Rating" ], WFrame Nothing (Just 0) ]

        enc =
            encoding
                . position X
                    [ PName "bin_IMDB_Rating"
                    , PmType Quantitative
                    , PScale [ SZero False ]
                    , PAxis [ AxTitle "IMDB rating" ]
                    ]
                . position X2 [ PName "bin_IMDB_Rating_end" ]

        cdEnc =
            encoding
                . position Y [ PName "cumulativeCount", PmType Quantitative ]

        specCumulative =
            asSpec [ cdEnc [], mark Bar [] ]

        dEnc =
            encoding
                . position Y [ PName "count", PmType Quantitative ]

        specDist =
            asSpec [ dEnc [], mark Bar [ MColor "yellow", MOpacity 0.5 ] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , trans []
        , layer [ specCumulative, specDist ]
        , enc []
        ]


dist4 :: VegaLite
dist4 =
    let
        des =
            description "A vertical 2D box plot showing median, min, and max in the US population distribution of age groups in 2000."

        trans =
            transform
                . aggregate
                    [ opAs Min "people" "lowerWhisker"
                    , opAs Q1 "people" "lowerBox"
                    , opAs Median "people" "midBox"
                    , opAs Q3 "people" "upperBox"
                    , opAs Max "people" "upperWhisker"
                    ]
                    [ "age" ]

        encAge =
            encoding . position X [ PName "age", PmType Ordinal ]

        encLWhisker =
            encoding
                . position Y [ PName "lowerWhisker", PmType Quantitative, PAxis [ AxTitle "Population" ] ]
                . position Y2 [ PName "lowerBox" ]

        specLWhisker =
            asSpec [mark Rule [ MStyle [ "boxWhisker" ] ], encLWhisker [] ]

        encUWhisker =
            encoding
                . position Y [ PName "upperBox", PmType Quantitative ]
                . position Y2 [ PName "upperWhisker" ]

        specUWhisker =
            asSpec [mark Rule [ MStyle [ "boxWhisker" ] ], encUWhisker [] ]

        encBox =
            encoding
                . position Y [ PName "lowerBox", PmType Quantitative ]
                . position Y2 [ PName "upperBox" ]
                . size [ MNumber 5 ]

        specBox =
            asSpec [mark Bar [ MStyle [ "box" ] ], encBox [] ]

        encBoxMid =
            encoding
                . position Y [ PName "midBox", PmType Quantitative ]
                . color [ MString "white" ]
                . size [ MNumber 5 ]

        specBoxMid =
            asSpec [mark Tick [ MStyle [ "boxMid" ] ], encBoxMid [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []
        , trans []
        , encAge []
        , layer [ specLWhisker, specUWhisker, specBox, specBoxMid ]
        ]


dist5 :: VegaLite
dist5 =
    let
        des =
            description "A Tukey box plot showing median and interquartile range in the US population distribution of age groups in 2000. This isn't strictly a Tukey box plot as the IQR extends beyond the min/max values for some age cohorts."

        trans =
            transform
                . aggregate
                    [ opAs Q1 "people" "lowerBox"
                    , opAs Q3 "people" "upperBox"
                    , opAs Median "people" "midBox"
                    ]
                    [ "age" ]
                . calculateAs "datum.upperBox - datum.lowerBox" "IQR"
                . calculateAs "datum.upperBox + datum.IQR * 1.5" "upperWhisker"
                . calculateAs "max(0,datum.lowerBox - datum.IQR *1.5)" "lowerWhisker"

        encAge =
            encoding . position X [ PName "age", PmType Ordinal ]

        encLWhisker =
            encoding
                . position Y [ PName "lowerWhisker", PmType Quantitative, PAxis [ AxTitle "Population" ] ]
                . position Y2 [ PName "lowerBox" ]

        specLWhisker =
            asSpec [mark Rule [ MStyle [ "boxWhisker" ] ], encLWhisker [] ]

        encUWhisker =
            encoding
                . position Y [ PName "upperBox", PmType Quantitative ]
                . position Y2 [ PName "upperWhisker" ]

        specUWhisker =
            asSpec
                [mark Rule [ MStyle [ "boxWhisker" ] ], encUWhisker [] ]

        encBox =
            encoding
                . position Y [ PName "lowerBox", PmType Quantitative ]
                . position Y2 [ PName "upperBox" ]
                . size [ MNumber 5 ]

        specBox =
            asSpec [mark Bar [ MStyle [ "box" ] ], encBox [] ]

        encBoxMid =
            encoding
                . position Y [ PName "midBox", PmType Quantitative ]
                . color [ MString "white" ]
                . size [ MNumber 5 ]

        specBoxMid =
            asSpec
                [mark Tick [ MStyle [ "boxMid" ] ], encBoxMid [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []
        , trans []
        , encAge []
        , layer [ specLWhisker, specUWhisker, specBox, specBoxMid ]
        ]


quantile1 :: VegaLite
quantile1 =
  let dvals = dataFromUrl "https://vega.github.io/vega-lite/data/normal-2d.json" []

      trans = transform
              . quantile "u" [ QtStep 0.01, QtAs "p" "v" ]
              . calculateAs "quantileUniform(datum.p)" "unif"
              . calculateAs "quantileNormal(datum.p)" "norm"

      enc x y = encoding
                . position X [ PName x, PmType Quantitative ]
                . position Y [ PName y, PmType Quantitative ]

      leftSpec = asSpec [ mark Point [], enc "unif" "v" [] ]
      rightSpec = asSpec [ mark Point [], enc "norm" "v" [] ]

  in toVegaLite [ dvals, trans [], hConcat [ leftSpec, rightSpec ] ]
