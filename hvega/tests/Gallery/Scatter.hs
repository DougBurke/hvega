{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryScatter.elm (from development of version
-- 1.13.0)
--
module Gallery.Scatter (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("scatter1", scatter1)
            , ("scatter2", scatter2)
            , ("scatter3", scatter3)
            , ("scatter4", scatter4)
            , ("scatter5", scatter5)
            , ("scatter6", scatter6)
            , ("scatter7", scatter7)
            , ("scatter8", scatter8)
            , ("scatter9", scatter9)
            , ("scatter10", scatter10)
            , ("scatter11", scatter11)
            , ("smoothing1", smoothing1)
            , ("smoothing2", smoothing2)
            ]


pQuant :: PositionChannel
pQuant = PmType Quantitative


scatter1 :: VegaLite
scatter1 =
    let
        des =
            description "A scatterplot showing horsepower and miles per gallon for various cars (via point marks)."

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , mark Point [ MTooltip TTData ]
        , enc []
        ]


scatter2 :: VegaLite
scatter2 =
    let
        des =
            description "Shows the distribution of a single variable (precipitation) using tick marks."

        enc =
            encoding
                . position X [ PName "precipitation", PmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []
        , mark Tick []
        , enc []
        ]


scatter3 :: VegaLite
scatter3 =
    let
        des =
            description "Shows the relationship between horsepower and the number of cylinders using tick marks."

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Cylinders", PmType Ordinal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , mark Tick []
        , enc []
        ]


scatter4 :: VegaLite
scatter4 =
    let
        des =
            description "A scatterplot showing horsepower and miles per gallon with country of origin double encoded by colour and shape."

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Origin", MmType Nominal ]
                . shape [ MName "Origin", MmType Nominal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , mark Point []
        , enc []
        ]


scatter5 :: VegaLite
scatter5 =
    let
        des =
            description "A binned scatterplot comparing IMDB and Rotten Tomatoes rating with marks sized by number of reviews."

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PBin [ MaxBins 10 ] ]
                . position Y [ PName "Rotten_Tomatoes_Rating", PmType Quantitative, PBin [ MaxBins 10 ] ]
                . size [ MAggregate Count, MmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , mark Circle []
        , enc []
        ]


scatter6 :: VegaLite
scatter6 =
    let
        des =
            description "A bubbleplot showing horsepower on x, miles per gallons on y, and acceleration on size."

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . size [ MName "Acceleration", MmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , mark Point []
        , enc []
        ]


scatter7 :: VegaLite
scatter7 =
    let
        des =
            description "Scatterplot with Null values in grey"

        config =
            configure
                . configuration (MarkStyle [ MRemoveInvalid False ])

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative ]
                . position Y [ PName "Rotten_Tomatoes_Rating", PmType Quantitative ]
                . color
                    [ MDataCondition
                        [ ( Expr "datum.IMDB_Rating === null || datum.Rotten_Tomatoes_Rating === null"
                          , [ MString "#ddd" ]
                          )
                        ]
                        [ MString "rgb(76,120,168)" ]
                    ]
    in
    toVegaLite
        [ des
        , config []
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , mark Point []
        , enc []
        ]


scatter8 :: VegaLite
scatter8 =
    let
        des =
            description "A scatterplot showing horsepower and miles per gallon for various cars (via circle marks)."

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , mark Circle []
        , enc []
        ]


scatter9 :: VegaLite
scatter9 =
    let
        des =
            description "A bubble plot showing the correlation between health and income for 187 countries in the world (modified from an example in Lisa Charlotte Rost's blog post 'One Chart, Twelve Charting Libraries' --http://lisacharlotterost.github.io/2016/05/17/one-chart-code/)."

        enc =
            encoding
                . position X [ PName "income", PmType Quantitative, PScale [ SType ScLog ] ]
                . position Y [ PName "health", PmType Quantitative, PScale [ SZero False ] ]
                . size [ MName "population", MmType Quantitative ]
                . color [ MString "#000" ]

        sel =
            selection . select "view" Interval [ BindScales ]
    in
    toVegaLite
        [ des
        , width 500
        , height 300
        , dataFromUrl "https://vega.github.io/vega-lite/data/gapminder-health-income.csv" []
        , mark Circle []
        , enc []
        , sel []
        ]


scatter10 :: VegaLite
scatter10 =
    let
        des =
            description "Visualization of global deaths from natural disasters. Copy of chart from https://ourworldindata.org/natural-catastrophes"

        trans =
            transform
                . filter (FExpr "datum.Entity !== 'All natural disasters'")

        enc =
            encoding
                . position X [ PName "Year", PmType Ordinal, PAxis [ AxLabelAngle 0 ] ]
                . position Y [ PName "Entity", PmType Nominal, PAxis [ AxNoTitle ] ]
                . size
                    [ MName "Deaths"
                    , MmType Quantitative
                    , MLegend [ LTitle "Annual Global Deaths" ]
                    , MScale [ SRange (RNumbers [ 0, 5000 ]) ]
                    ]
                . color [ MName "Entity", MmType Nominal, MLegend [] ]
    in
    toVegaLite
        [ des
        , width 600
        , height 400
        , dataFromUrl "https://vega.github.io/vega-lite/data/disasters.csv" []
        , trans []
        , mark Circle [ MOpacity 0.8, MStroke "black", MStrokeWidth 1 ]
        , enc []
        ]


scatter11 :: VegaLite
scatter11 =
    let
        des =
            description "A scatterplot showing horsepower and miles per gallon with country of origin double encoded by colour and text symbol."

        trans =
            transform
                . calculateAs "datum.Origin[0]" "OriginInitial"

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Origin", MmType Nominal ]
                . text [ TName "OriginInitial", TmType Nominal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , trans []
        , mark Text []
        , enc []
        ]


-- combines scatter12 and scatter13 from elm
--
-- TODO: how to add a legend to indicate which line is which?
--
smoothing1 :: VegaLite
smoothing1 =
  let desc = description "How can we 'smooth' data?"
      dvals = dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []

      ltrans = transform
              . loess "IMDB_Rating" "Rotten_Tomatoes_Rating" [ LsBandwidth 0.1 ]

      rtrans = transform
              . regression "IMDB_Rating" "Rotten_Tomatoes_Rating"
                           [ RgMethod RgPoly, RgOrder 3, RgExtent 10 90 ]

      enc = encoding
            . position X [ PName "Rotten_Tomatoes_Rating", pQuant ]
            . position Y [ PName "IMDB_Rating", pQuant ]

      pointSpec = asSpec [ mark Point [ MFilled True, MOpacity 0.3 ] ]
      trendSpec = asSpec [ ltrans [], mark Line [ MColor "firebrick" ] ]
      regSpec = asSpec [ rtrans [], mark Line [ MColor "orange" ] ]

      layers = layer [ pointSpec, trendSpec, regSpec ]

  in toVegaLite [ desc, width 300, height 300, dvals, enc [], layers ]


-- smoothing1 but rename the columns in the transform
-- and use the default regression values
--
smoothing2 :: VegaLite
smoothing2 =
  let desc = description "How can we 'smooth' and rename data?"
      dvals = dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []

      ltrans = transform
              . loess "IMDB_Rating" "Rotten_Tomatoes_Rating"
                      [ LsAs "Rotten_Tomatoes_Rating" "ly" ]

      rtrans = transform
              . regression "IMDB_Rating" "Rotten_Tomatoes_Rating"
                           [ RgAs "Rotten_Tomatoes_Rating" "ry" ]

      enc y = encoding
              . position X [ PName "Rotten_Tomatoes_Rating", pQuant ]
              . position Y [ PName y, pQuant ]

      pointSpec = asSpec [ enc "IMDB_Rating" []
                         , mark Point [ MFilled True, MOpacity 0.3 ]
                         ]
      trendSpec = asSpec [ enc "ly" []
                         , ltrans []
                         , mark Line [ MColor "firebrick" ] ]
      regSpec = asSpec [ enc "ry" []
                       , rtrans []
                       , mark Line [ MColor "orange" ] ]

      layers = layer [ pointSpec, trendSpec, regSpec ]

  in toVegaLite [ desc, width 300, height 300, dvals, layers ]
