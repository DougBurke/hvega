{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryBar.elm (from development of version
-- 1.13.0)
--
module Gallery.Bar (testSpecs) where

import Graphics.Vega.VegaLite

import qualified Data.Text as T

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("bar1", bar1)
            , ("bar2", bar2)
            , ("bar3", bar3)
            , ("bar4", bar4)
            , ("bar5", bar5)
            , ("bar6", bar6)
            , ("bar7", bar7)
            , ("bar8", bar8)
            , ("bar9", bar9)
            , ("bar10", bar10)
            , ("bar11", bar11)
            , ("bar12", bar12)
            , ("bar13", bar13)
            , ("bar14", bar14)
            ]


bar1 :: VegaLite
bar1 =
    let
        des =
            description "A simple bar chart with embedded data."

        dvals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C", "D", "E", "F", "G", "H", "I" ])
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        enc =
            encoding
                . position X [ PName "a", PmType Ordinal ]
                . position Y [ PName "b", PmType Quantitative ]
    in
    toVegaLite [ des, dvals [], mark Bar [], enc [] ]

bar2 :: VegaLite
bar2 =
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


bar3 :: VegaLite
bar3 =
    let
        des =
            description "A bar chart showing the US population distribution of age groups in 2000."

        trans =
            transform . filter (FExpr "datum.year == 2000")

        enc =
            encoding
                . position X [ PName "people", PmType Quantitative, PAggregate Sum, PAxis [ AxTitle "population" ] ]
                . position Y [ PName "age", PmType Ordinal, PScale [ SRangeStep (Just 17) ], PSort [ Descending ] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []
        , mark Bar []
        , trans []
        , enc []
        ]


bar4 :: VegaLite
bar4 =
    let
        des =
            description "Grouped bar chart showing population structure by age and gender."

        trans =
            transform
                . filter (FExpr "datum.year == 2000")
                . calculateAs "datum.sex == 2 ? 'Female' : 'Male'" "gender"

        enc =
            encoding
                . position X [ PName "gender", PmType Nominal, PScale [ SRangeStep (Just 12) ], PAxis [ AxNoTitle ] ]
                . position Y [ PName "people", PmType Quantitative, PAggregate Sum, PAxis [ AxTitle "population", AxGrid False ] ]
                . column [ FName "age", FmType Ordinal ]
                . color [ MName "gender", MmType Nominal, MScale [ SRange (RStrings [ "#EA98D2", "#659CCA" ]) ] ]

        config =
            configure
                . configuration (Axis [ DomainWidth 1 ])
                . configuration (View [ ViewStroke Nothing ])
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []
        , mark Bar []
        , trans []
        , enc []
        , config []
        ]


bar5 :: VegaLite
bar5 =
    let
        des =
            description "Seattle weather stacked bar chart"

        weatherColors =
            categoricalDomainMap
                [ ( "sun", "#e7ba52" )
                , ( "fog", "#c7c7c7" )
                , ( "drizzle", "#aec7ea" )
                , ( "rain", "#1f77b4" )
                , ( "snow", "#9467bd" )
                ]

        enc =
            encoding
                . position X [ PName "date", PmType Ordinal, PTimeUnit Month, PAxis [ AxTitle "Month of the year" ] ]
                . position Y [ PmType Quantitative, PAggregate Count ]
                . color
                    [ MName "weather"
                    , MmType Nominal
                    , MScale weatherColors
                    , MLegend [ LTitle "Weather type" ]
                    ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []
        , mark Bar []
        , enc []
        ]


bar6 :: VegaLite
bar6 =
    let
        des =
            description "Barley crop yields as a horizontal stacked bar chart"

        enc =
            encoding
                . position X [ PName "yield", PmType Quantitative, PAggregate Sum ]
                . position Y [ PName "variety", PmType Nominal ]
                . color [ MName "site", MmType Nominal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/barley.json" []
        , mark Bar []
        , enc []
        ]


bar7 :: VegaLite
bar7 =
    let
        des =
            description "Population structure as a normalised stacked bar chart."

        trans =
            transform
                . filter (FExpr "datum.year == 2000")
                . calculateAs "datum.sex == 2 ? 'Female' : 'Male'" "gender"

        enc =
            encoding
                . position X [ PName "age", PmType Ordinal, PScale [ SRangeStep (Just 17) ] ]
                . position Y [ PName "people", PmType Quantitative, PAggregate Sum, PAxis [ AxTitle "Population" ], PStack StNormalize ]
                . color [ MName "gender", MmType Nominal, MScale [ SRange (RStrings [ "#EA98D2", "#659CCA" ]) ] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []
        , trans []
        , mark Bar []
        , enc []
        ]


bar8 :: VegaLite
bar8 =
    let
        des =
            description "A simple bar chart with ranged data (aka Gantt Chart)."

        dvals =
            dataFromColumns []
                . dataColumn "task" (Strings [ "A", "B", "C" ])
                . dataColumn "start" (Numbers [ 1, 3, 8 ])
                . dataColumn "end" (Numbers [ 3, 8, 10 ])

        enc =
            encoding
                . position Y [ PName "task", PmType Ordinal ]
                . position X [ PName "start", PmType Quantitative ]
                . position X2 [ PName "end" ]
    in
    toVegaLite [ des, dvals [], mark Bar [], enc [] ]


bar9 :: VegaLite
bar9 =
    let
        des =
            description "A bar chart that directly encodes color names in the data."

        dvals =
            dataFromColumns []
                . dataColumn "color" (Strings [ "red", "green", "blue" ])
                . dataColumn "b" (Numbers [ 28, 55, 43 ])

        enc =
            encoding
                . position X [ PName "color", PmType Nominal ]
                . position Y [ PName "b", PmType Quantitative ]
                . color [ MName "color", MmType Nominal, MScale [] ]
    in
    toVegaLite [ des, dvals [], mark Bar [], enc [] ]


bar10 :: VegaLite
bar10 =
    let
        des =
            description "Layered bar chart showing the US population distribution of age groups and gender in 2000."

        trans =
            transform
                . filter (FExpr "datum.year == 2000")
                . calculateAs "datum.sex == 2 ? 'Female' : 'Male'" "gender"

        enc =
            encoding
                . position X [ PName "age", PmType Ordinal, PScale [ SRangeStep (Just 17) ] ]
                . position Y [ PName "people", PmType Quantitative, PAggregate Sum, PAxis [ AxTitle "Population" ], PStack NoStack ]
                . color [ MName "gender", MmType Nominal, MScale [ SRange (RStrings [ "#e377c2", "#1f77b4" ]) ] ]
                . opacity [ MNumber 0.7 ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []
        , trans []
        , mark Bar []
        , enc []
        ]


bar11 :: VegaLite
bar11 =
    let
        des =
            description "A diverging stacked bar chart for sentiments towards a set of eight questions, displayed as percentages with neutral responses straddling the 0% mark."

        dvals =
            dataFromColumns []
                . dataColumn "question" (Strings [ "Q1", "Q1", "Q1", "Q1", "Q1", "Q2", "Q2", "Q2", "Q2", "Q2", "Q3", "Q3", "Q3", "Q3", "Q3", "Q4", "Q4", "Q4", "Q4", "Q4", "Q5", "Q5", "Q5", "Q5", "Q5", "Q6", "Q6", "Q6", "Q6", "Q6", "Q7", "Q7", "Q7", "Q7", "Q7", "Q8", "Q8", "Q8", "Q8", "Q8" ])
                . dataColumn "type" (Strings [ "Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree" ])
                . dataColumn "value" (Numbers [ 24, 294, 594, 1927, 376, 2, 2, 0, 7, 11, 2, 0, 2, 4, 2, 0, 2, 1, 7, 6, 0, 1, 3, 16, 4, 1, 1, 2, 9, 3, 0, 0, 1, 4, 0, 0, 0, 0, 0, 2 ])
                . dataColumn "percentage" (Numbers [ 0.7, 9.1, 18.5, 59.9, 11.7, 18.2, 18.2, 0, 63.6, 0, 20, 0, 20, 40, 20, 0, 12.5, 6.3, 43.8, 37.5, 0, 4.2, 12.5, 66.7, 16.7, 6.3, 6.3, 12.5, 56.3, 18.8, 0, 0, 20, 80, 0, 0, 0, 0, 0, 100 ])
                . dataColumn "percentage_start" (Numbers [ -19.1, -18.4, -9.2, 9.2, 69.2, -36.4, -18.2, 0, 0, 63.6, -30, -10, -10, 10, 50, -15.6, -15.6, -3.1, 3.1, 46.9, -10.4, -10.4, -6.3, 6.3, 72.9, -18.8, -12.5, -6.3, 6.3, 62.5, -10, -10, -10, 10, 90, 0, 0, 0, 0, 0 ])
                . dataColumn "percentage_end" (Numbers [ -18.4, -9.2, 9.2, 69.2, 80.9, -18.2, 0, 0, 63.6, 63.6, -10, -10, 10, 50, 70, -15.6, -3.1, 3.1, 46.9, 84.4, -10.4, -6.3, 6.3, 72.9, 89.6, -12.5, -6.3, 6.3, 62.5, 81.3, -10, -10, 10, 90, 90, 0, 0, 0, 0, 100 ])

        enc =
            encoding
                . position X [ PName "percentage_start", PmType Quantitative, PAxis [ AxTitle "Percentage" ] ]
                . position X2 [ PName "percentage_end" ]
                . position Y [ PName "question", PmType Nominal, PAxis [ AxTitle "Question", AxOffset 5, AxTicks False, AxMinExtent 60, AxDomain False ] ]
                . color
                    [ MName "type"
                    , MmType Nominal
                    , MLegend [ LTitle "Response" ]
                    , MScale (SType ScOrdinal
                             : categoricalDomainMap
                                [ ( "Strongly disagree", "#c30d24" )
                                , ( "Disagree", "#f3a583" )
                                , ( "Neither agree nor disagree", "#cccccc" )
                                , ( "Agree", "#94c6da" )
                                , ( "Strongly agree", "#1770ab" )
                                ]
                             )
                    ]
    in
    toVegaLite [ des, dvals [], mark Bar [], enc [] ]


bar12 :: VegaLite
bar12 =
    let
        des =
            description "A simple bar chart with embedded data labels."

        dvals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C" ])
                . dataColumn "b" (Numbers [ 28, 55, 43 ])

        enc =
            encoding
                . position X [ PName "b", PmType Quantitative ]
                . position Y [ PName "a", PmType Ordinal ]

        specBar =
            asSpec [mark Bar [] ]

        specText =
            asSpec [ mark Text [ MStyle [ "label" ] ], encoding (text [ TName "b", TmType Quantitative ] []) ]

        config =
            configure . configuration (NamedStyle "label" [ MAlign AlignLeft, MBaseline AlignMiddle, MdX 3 ])
    in
    toVegaLite [ des, dvals [], enc [], layer [ specBar, specText ], config [] ]


toRows :: T.Text -> [( T.Text, Int )] -> [DataRow] -> [DataRow]
toRows country animalFreqs oldRows =
    let
        toRow animal n =
            dataRow
                [ ( "country", Str country )
                , ( "animal", Str animal )
                , ( "col", Number (fromIntegral n) )
                ]

        fToCol ( animal, f ) =
            foldr (toRow animal) [] [1 .. f]

        newRows = concatMap fToCol animalFreqs
        
    in oldRows ++ newRows


bar13 :: VegaLite
bar13 =
    let
        cow =
            "M4 -2c0 0 0.9 -0.7 1.1 -0.8c0.1 -0.1 -0.1 0.5 -0.3 0.7c-0.2 0.2 1.1 1.1 1.1 1.2c0 0.2 -0.2 0.8 -0.4 0.7c-0.1 0 -0.8 -0.3 -1.3 -0.2c-0.5 0.1 -1.3 1.6 -1.5 2c-0.3 0.4 -0.6 0.4 -0.6 0.4c0 0.1 0.3 1.7 0.4 1.8c0.1 0.1 -0.4 0.1 -0.5 0c0 0 -0.6 -1.9 -0.6 -1.9c-0.1 0 -0.3 -0.1 -0.3 -0.1c0 0.1 -0.5 1.4 -0.4 1.6c0.1 0.2 0.1 0.3 0.1 0.3c0 0 -0.4 0 -0.4 0c0 0 -0.2 -0.1 -0.1 -0.3c0 -0.2 0.3 -1.7 0.3 -1.7c0 0 -2.8 -0.9 -2.9 -0.8c-0.2 0.1 -0.4 0.6 -0.4 1c0 0.4 0.5 1.9 0.5 1.9l-0.5 0l-0.6 -2l0 -0.6c0 0 -1 0.8 -1 1c0 0.2 -0.2 1.3 -0.2 1.3c0 0 0.3 0.3 0.2 0.3c0 0 -0.5 0 -0.5 0c0 0 -0.2 -0.2 -0.1 -0.4c0 -0.1 0.2 -1.6 0.2 -1.6c0 0 0.5 -0.4 0.5 -0.5c0 -0.1 0 -2.7 -0.2 -2.7c-0.1 0 -0.4 2 -0.4 2c0 0 0 0.2 -0.2 0.5c-0.1 0.4 -0.2 1.1 -0.2 1.1c0 0 -0.2 -0.1 -0.2 -0.2c0 -0.1 -0.1 -0.7 0 -0.7c0.1 -0.1 0.3 -0.8 0.4 -1.4c0 -0.6 0.2 -1.3 0.4 -1.5c0.1 -0.2 0.6 -0.4 0.6 -0.4z"

        pig =
            "M1.2 -2c0 0 0.7 0 1.2 0.5c0.5 0.5 0.4 0.6 0.5 0.6c0.1 0 0.7 0 0.8 0.1c0.1 0 0.2 0.2 0.2 0.2c0 0 -0.6 0.2 -0.6 0.3c0 0.1 0.4 0.9 0.6 0.9c0.1 0 0.6 0 0.6 0.1c0 0.1 0 0.7 -0.1 0.7c-0.1 0 -1.2 0.4 -1.5 0.5c-0.3 0.1 -1.1 0.5 -1.1 0.7c-0.1 0.2 0.4 1.2 0.4 1.2l-0.4 0c0 0 -0.4 -0.8 -0.4 -0.9c0 -0.1 -0.1 -0.3 -0.1 -0.3l-0.2 0l-0.5 1.3l-0.4 0c0 0 -0.1 -0.4 0 -0.6c0.1 -0.1 0.3 -0.6 0.3 -0.7c0 0 -0.8 0 -1.5 -0.1c-0.7 -0.1 -1.2 -0.3 -1.2 -0.2c0 0.1 -0.4 0.6 -0.5 0.6c0 0 0.3 0.9 0.3 0.9l-0.4 0c0 0 -0.4 -0.5 -0.4 -0.6c0 -0.1 -0.2 -0.6 -0.2 -0.5c0 0 -0.4 0.4 -0.6 0.4c-0.2 0.1 -0.4 0.1 -0.4 0.1c0 0 -0.1 0.6 -0.1 0.6l-0.5 0l0 -1c0 0 0.5 -0.4 0.5 -0.5c0 -0.1 -0.7 -1.2 -0.6 -1.4c0.1 -0.1 0.1 -1.1 0.1 -1.1c0 0 -0.2 0.1 -0.2 0.1c0 0 0 0.9 0 1c0 0.1 -0.2 0.3 -0.3 0.3c-0.1 0 0 -0.5 0 -0.9c0 -0.4 0 -0.4 0.2 -0.6c0.2 -0.2 0.6 -0.3 0.8 -0.8c0.3 -0.5 1 -0.6 1 -0.6z"

        sheep =
            "M-4.1 -0.5c0.2 0 0.2 0.2 0.5 0.2c0.3 0 0.3 -0.2 0.5 -0.2c0.2 0 0.2 0.2 0.4 0.2c0.2 0 0.2 -0.2 0.5 -0.2c0.2 0 0.2 0.2 0.4 0.2c0.2 0 0.2 -0.2 0.4 -0.2c0.1 0 0.2 0.2 0.4 0.1c0.2 0 0.2 -0.2 0.4 -0.3c0.1 0 0.1 -0.1 0.4 0c0.3 0 0.3 -0.4 0.6 -0.4c0.3 0 0.6 -0.3 0.7 -0.2c0.1 0.1 1.4 1 1.3 1.4c-0.1 0.4 -0.3 0.3 -0.4 0.3c-0.1 0 -0.5 -0.4 -0.7 -0.2c-0.3 0.2 -0.1 0.4 -0.2 0.6c-0.1 0.1 -0.2 0.2 -0.3 0.4c0 0.2 0.1 0.3 0 0.5c-0.1 0.2 -0.3 0.2 -0.3 0.5c0 0.3 -0.2 0.3 -0.3 0.6c-0.1 0.2 0 0.3 -0.1 0.5c-0.1 0.2 -0.1 0.2 -0.2 0.3c-0.1 0.1 0.3 1.1 0.3 1.1l-0.3 0c0 0 -0.3 -0.9 -0.3 -1c0 -0.1 -0.1 -0.2 -0.3 -0.2c-0.2 0 -0.3 0.1 -0.4 0.4c0 0.3 -0.2 0.8 -0.2 0.8l-0.3 0l0.3 -1c0 0 0.1 -0.6 -0.2 -0.5c-0.3 0.1 -0.2 -0.1 -0.4 -0.1c-0.2 -0.1 -0.3 0.1 -0.4 0c-0.2 -0.1 -0.3 0.1 -0.5 0c-0.2 -0.1 -0.1 0 -0.3 0.3c-0.2 0.3 -0.4 0.3 -0.4 0.3l0.2 1.1l-0.3 0l-0.2 -1.1c0 0 -0.4 -0.6 -0.5 -0.4c-0.1 0.3 -0.1 0.4 -0.3 0.4c-0.1 -0.1 -0.2 1.1 -0.2 1.1l-0.3 0l0.2 -1.1c0 0 -0.3 -0.1 -0.3 -0.5c0 -0.3 0.1 -0.5 0.1 -0.7c0.1 -0.2 -0.1 -1 -0.2 -1.1c-0.1 -0.2 -0.2 -0.8 -0.2 -0.8c0 0 -0.1 -0.5 0.4 -0.8z"

        des =
            description "Isotype bar chart inspired by this Only An Ocean Between, 1943. Population Live Stock, p.13."

        config =
            configure
                . configuration (View [ ViewStroke Nothing ])

        dvals =
            dataFromRows []
                . toRows "Great Britain" [ ( "cattle", 3 ), ( "pigs", 2 ), ( "sheep", 10 ) ]
                . toRows "United States" [ ( "cattle", 9 ), ( "pigs", 6 ), ( "sheep", 7 ) ]

        enc =
            encoding
                . position X [ PName "col", PmType Ordinal, PAxis [] ]
                . position Y [ PName "animal", PmType Ordinal, PAxis [] ]
                . row [ FName "country", FmType Nominal, FHeader [ HNoTitle ] ]
                . shape
                    [ MName "animal"
                    , MmType Nominal
                    , MScale
                        (categoricalDomainMap
                            [ ( "person", "circle" )
                            , ( "cattle", cow )
                            , ( "pigs", pig )
                            , ( "sheep", sheep )
                            ]
                        )
                    , MLegend []
                    ]
                . color
                    [ MName "animal"
                    , MmType Nominal
                    , MLegend []
                    , MScale 
                        (categoricalDomainMap
                            [ ( "person", "rgb(162,160,152)" )
                            , ( "cattle", "rgb(194,81,64)" )
                            , ( "pigs", "rgb(93,93,93)" )
                            , ( "sheep", "rgb(91,131,149)" )
                            ]
                        )
                    ]
                . opacity [ MNumber 1 ]
                . size [ MNumber 200 ]
    in
    toVegaLite [ des, config [], width 800, height 200, dvals [], mark Point [ MFilled True ], enc [] ]


bar14 :: VegaLite
bar14 =
    let
        des =
            description "Isotype bar chart using emojis for symbols"

        config =
            configure
                . configuration (View [ ViewStroke Nothing ])

        dvals =
            dataFromRows []
                . toRows "Great Britain" [ ( "cattle", 3 ), ( "pigs", 2 ), ( "sheep", 10 ) ]
                . toRows "United States" [ ( "cattle", 9 ), ( "pigs", 6 ), ( "sheep", 7 ) ]

        trans =
            transform
                . calculateAs "{'cattle': '🐄', 'pigs': '🐖', 'sheep': '🐏'}[datum.animal]" "emoji"
                . window [ ( [ WOp Rank ], "rank" ) ] [ WGroupBy [ "country", "animal" ] ]

        enc =
            encoding
                . position X [ PName "rank", PmType Ordinal, PAxis [] ]
                . position Y [ PName "animal", PmType Nominal, PAxis [], PSort [] ]
                . row [ FName "country", FmType Nominal, FHeader [ HNoTitle ] ]
                . text [ TName "emoji", TmType Nominal ]
                . size [ MNumber 65 ]
    in
    toVegaLite [ des, config [], width 800, height 200, dvals [], trans [], mark Text [ MBaseline AlignMiddle ], enc [] ]
