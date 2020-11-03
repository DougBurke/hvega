{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryBar.elm (from development of version
-- 1.13.0)
--
-- TODO: update to add new tests from Elm (v2.0)

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
            , ("bar6order", bar6order)
            , ("bar6sort", bar6sort)
            , ("bar7", bar7)
            , ("bar8", bar8)
            , ("bar9", bar9)
            , ("bar10", bar10)
            , ("bar11", bar11)
            , ("bar12", bar12)
            , ("bar13", bar13)
            , ("bar14", bar14)
            , ("agesorted", ageSorted)
            , ("explicitbins", explicitBins)
            , ("logscaled", logScaled)
            , ("signedpopulation", signedPopulation)
            , ("labeloverlay", labelOverlay)
            , ("wilkinsondotplot", wilkinsonDotPlot)
            , ("initialletter", initialLetter)
            , ("barnegative", barNegative)
            , ("baraxisspacesaving", barAxisSpaceSaving)
            , ("layer_bar_labels_grey", layerBarLabelsGrey)
            ]


pName :: T.Text -> PositionChannel
pName = PName

mName :: T.Text -> MarkChannel
mName = MName

pNominal, pOrdinal, pQuant :: PositionChannel
pNominal = PmType Nominal
pOrdinal = PmType Ordinal
pQuant = PmType Quantitative


noStroke :: [ConfigureSpec] -> PropertySpec
noStroke = configure
           . configuration (ViewStyle [ ViewNoStroke ])


seattleWeather :: Data
seattleWeather = dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []

movieData :: Data
movieData = dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []


-- bar1 in GalleryBar.elm
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


-- bar4 in GalleryBar.elm
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
        , movieData
        , mark Bar [ MBinSpacing 0 ]
        , enc []
        ]


-- bar2 in GalleryBar.elm (except I've added a sorting specifier to Y)
bar3 :: VegaLite
bar3 =
    let
        des =
            description "A bar chart showing the US population distribution of age groups in 2000."

        trans =
            transform . filter (FExpr "datum.year == 2000")

        enc =
            encoding
                . position X [ PName "people"
                             , PmType Quantitative
                             , PAggregate Sum
                             , PAxis [ AxTitle "population" ] ]
                . position Y [ PName "age"
                             , PmType Ordinal
                             , PSort [ Descending ] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []
        , mark Bar []
        , heightStep 17
        , trans []
        , enc []
        ]


-- bar7 in GalleryBar.elm
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
                . position X [ PName "gender", PmType Nominal, PAxis [ AxNoTitle ] ]
                . position Y [ PName "people", PmType Quantitative, PAggregate Sum, PAxis [ AxTitle "population", AxGrid False ] ]
                . column [ FName "age", FmType Ordinal ]
                . color [ MName "gender", MmType Nominal, MScale [ SRange (RStrings [ "#EA98D2", "#659CCA" ]) ] ]

        config = noStroke
                . configuration (Axis [ DomainWidth 1 ])
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []
        , mark Bar []
        , widthStep 12
        , trans []
        , enc []
        , config []
        ]


-- bar8 in GalleryBar.elm
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
                . position X [ PName "date"
                             , PmType Ordinal
                             , PTimeUnit (TU Month)
                             , PAxis [ AxTitle "Month of the year" ]
                             ]
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
        , seattleWeather
        , mark Bar []
        , enc []
        ]


barley :: Data
barley = dataFromUrl "https://vega.github.io/vega-lite/data/barley.json" []


barleyEnc :: [EncodingSpec] -> PropertySpec
barleyEnc = encoding
            . position X [ PName "yield", PmType Quantitative, PAggregate Sum ]
            . position Y [ PName "variety", PmType Nominal ]
            . color [ MName "site", MmType Nominal ]


-- bar9 in GalleryBar.elm, also first version of
-- https://vega.github.io/vega-lite/docs/stack.html#sorting-stack-order
bar6 :: VegaLite
bar6 =
  let des = description "Barley crop yields as a horizontal stacked bar chart"

  in toVegaLite [ des
                , barley
                , mark Bar []
                , barleyEnc []
                ]


-- tweaks to bar6 from https://vega.github.io/vega-lite/docs/stack.html#sorting-stack-order
--
bar6order :: VegaLite
bar6order =
  let enc = barleyEnc
            . order [ OName "yield", OmType Quantitative, OAggregate Sum ]

  in toVegaLite [ barley
                , mark Bar []
                , enc []
                ]


-- tweaks to bar6 from https://vega.github.io/vega-lite/docs/stack.html#sorting-stack-order
--
bar6sort :: VegaLite
bar6sort =
  let enc = barleyEnc
            . order [ OName "siteOrder" ]

      trans = transform
              . calculateAs "if(datum.site === 'University Farm', 0, if(datum.site === 'Grand Rapids', 1, 2))" "siteOrder"

  in toVegaLite [ barley
                , trans []
                , mark Bar []
                , enc []
                ]


-- bar10 in GalleryBar.elm
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
                . position X [ PName "age", PmType Ordinal ]
                . position Y [ PName "people"
                             , PmType Quantitative
                             , PAggregate Sum
                             , PAxis [ AxTitle "Population" ]
                             , PStack StNormalize
                             ]
                . color [ MName "gender"
                        , MmType Nominal
                        , MScale [ SRange (RStrings [ "#EA98D2", "#659CCA" ]) ]
                        ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []
        , trans []
        , mark Bar []
        , widthStep 17
        , enc []
        ]


-- bar11 in GalleryBar.elm
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


-- bar12 in GalleryBar.elm
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


-- bar13 in GalleryBar.elm
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
                . position X [ PName "age", PmType Ordinal ]
                . position Y [ PName "people"
                             , PmType Quantitative
                             , PAggregate Sum
                             , PAxis [ AxTitle "Population" ]
                             , PStack NoStack
                             ]
                . color [ MName "gender"
                        , MmType Nominal
                        , MScale [ SRange (RStrings [ "#e377c2", "#1f77b4" ]) ]
                        ]
                . opacity [ MNumber 0.7 ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []
        , trans []
        , mark Bar []
        , widthStep 17
        , enc []
        ]


-- bar15 in GalleryBar.elm
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


-- bar16 in GalleryBar.elm
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
            asSpec [ mark Text [ MStyle [ "label" ] ]
                   , encoding (text [ TName "b", TmType Quantitative ] [])
                   ]

        config =
            configure . configuration (MarkNamedStyles
                                       [( "label"
                                        , [ MAlign AlignLeft, MBaseline AlignMiddle, MdX 3 ])])
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


-- bar19 in GalleryBar.elm
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
    toVegaLite [ des, noStroke [], width 800, height 200, dvals [], mark Point [ MFilled True ], enc [] ]


-- bar20 in GalleryBar.elm
bar14 :: VegaLite
bar14 =
    let
        des =
            description "Isotype bar chart using emojis for symbols"

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
    toVegaLite [ des, noStroke [], width 800, height 200, dvals [], trans [], mark Text [ MBaseline AlignMiddle ], enc [] ]


-- bar3 in GalleryBar.elm, see bar3 above
ageSorted :: VegaLite
ageSorted =
  let des = description "A bar chart showing the US population distribution of age groups in 2000, sorted by population"

      dvals = dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []

      trans = transform . filter (FExpr "datum.year == 2000")

      enc = encoding
            . position X [ PName "people"
                         , PmType Quantitative
                         , PAggregate Sum
                         , PAxis [ AxTitle "population" ]
                         ]
            . position Y [ PName "age"
                         , PmType Ordinal
                         , PSort [ ByChannel ChX
                                 , Descending ]
                         ]

  in toVegaLite [ des
                , heightStep 17
                , dvals
                , trans []
                , enc []
                , mark Bar []
                ]


-- bar5 in GalleryBar.elm
explicitBins :: VegaLite
explicitBins =
  let dvals = dataFromColumns []
              . dataColumn "binStart" (Numbers [ 8, 10, 12, 14, 16, 18, 20, 22 ])
              . dataColumn "binEnd" (Numbers [ 10, 12, 14, 16, 18, 20, 22, 24 ])
              . dataColumn "count" (Numbers [ 7, 29, 71, 127, 94, 54, 17, 5 ])

      enc = encoding
            . position X [ pName "binStart"
                         , pQuant
                         , PBin [ Step 2 ] ]
            . position X2 [ pName "binEnd" ]
            . position Y [ pName "count"
                         , pQuant ]

  in toVegaLite [ dvals [], enc [], mark Bar [] ]


-- bar6 in GalleryBar.elm
logScaled :: VegaLite
logScaled =
  let des = description "Log-scaled Histogram (may improve after https://github.com/vega/vega-lite/issues/4792)"

      dvals = dataFromColumns []
              . dataColumn "x" (Numbers [ 0.01, 0.1, 1, 1, 1, 1, 10, 10, 100, 500, 800 ])

      trans = transform
              . calculateAs "log(datum.x)/log(10)" "logX"
              . binAs [] "logX" "binLogX"
              . calculateAs "pow(10,datum.binLogX)" "x1"
              . calculateAs "pow(10,datum.binLogX_end)" "x2"

      enc = encoding
            . position X [ pName "x1"
                         , pQuant
                         , PScale [ SType ScLog, SBase 10 ]
                         , PAxis [ AxTickCount 5 ]
                         ]
            . position X2 [ pName "x2" ]
            . position Y [ PAggregate Count, pQuant ]

  in toVegaLite [ des, dvals [], trans [], enc [], mark Bar [] ]


-- bar14 in GalleryBar.elm
signedPopulation :: VegaLite
signedPopulation =
  let dvals = dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []

      trans = transform
              . filter (FExpr "datum.year == 2000")
              . calculateAs "datum.sex == 2 ? 'Female' : 'Male'" "gender"
              . calculateAs "datum.sex == 2 ? datum.people : -datum.people" "signedPeople"

      enc = encoding
            . position Y [ pName "age"
                         , pOrdinal
                         , PAxis []
                         , PSort [ Descending ]
                         ]
            . position X [ pName "signedPeople"
                         , PAggregate Sum
                         , pQuant
                         , PAxis [ AxTitle "Population", AxFormat "s" ]
                         ]
            . color [ mName "gender"
                    , MmType Nominal
                    , MScale [ SRange (RStrings [ "#675193", "#ca8861" ]) ]
                    , MLegend [ LOrient LOTop, LTitle "" ]
                    ]

      cfg = noStroke
            . configuration (Axis [ Grid False ])

  in toVegaLite [ width 300
                , height 200
                , cfg []
                , dvals
                , trans []
                , enc []
                , mark Bar [] ]


-- bar17 in GalleryBar.elm
labelOverlay :: VegaLite
labelOverlay =
  let des = description "Bar chart with label overlay"

      trans = transform
              . calculateAs "isValid(datum.Major_Genre)? datum.Major_Genre : 'unclassified'" "genre"

      enc = encoding
            . position Y
                    [ pName "genre"
                    , pNominal
                    , PSort
                        [ CustomSort
                            (Strings
                                [ "Action"
                                , "Adventure"
                                , "Comedy"
                                , "Black Comedy"
                                , "Romantic Comedy"
                                , "Concert/Performance"
                                , "Documentary"
                                , "Drama"
                                , "Horror"
                                , "Musical"
                                , "Thriller/Suspense"
                                , "Western"
                                , "unclassified"
                                ]
                            )
                        ]
                    , PAxis []
                    ]

      encBar = encoding
               . position X
                    [ pName "IMDB_Rating"
                    , PAggregate Mean
                    , pQuant
                    , PScale [ SDomain (DNumbers [ 0, 10 ]) ]
                    , PTitle "Mean IMDB Ratings"
                    ]

      specBar =asSpec [ encBar [], mark Bar [ MColor "#ddd" ] ]

      encText = encoding
                . text [ TName "genre", TmType Nominal ]
                . detail [ DAggregate Count, DmType Quantitative ]

      specText = asSpec [ encText [], mark Text [ MAlign AlignCenter, MdX 5 ] ]

  in toVegaLite
        [ des
        , width 200
        , heightStep 16
        , movieData
        , trans []
        , enc []
        , layer [ specBar, specText ]
        ]


-- bar18 in GalleryBar.elm
wilkinsonDotPlot :: VegaLite
wilkinsonDotPlot =
  let desc = description "A Wilkinson dot plot"

      dvals = dataFromColumns []
              . dataColumn "data" (Numbers [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4 ])

      trans = transform
              . window [ ( [ WOp Rank, WField "rank" ], "id" ) ] [ WGroupBy [ "data" ] ]

      enc = encoding
            . position X [ pName "data", pOrdinal ]
            . position Y
                    [ pName "id"
                    , pOrdinal
                    , PAxis []
                    , PSort [ Descending ]
                    ]

  in toVegaLite
        [ desc
        , noStroke []
        , height 100
        , dvals []
        , trans []
        , enc []
        , mark Circle [ MOpacity 1 ]
        ]


-- From https://vega.github.io/vega-lite/examples/bar_month_temporal_initial.html
-- but teaked to add a few bar properties.
--
initialLetter :: VegaLite
initialLetter =
  let desc = "Using `labelExpr` to show only initial letters of month names."

  in toVegaLite [ description desc
                , width 400
                , height 300
                , seattleWeather
                , mark Bar [ MBlend BMDifference
                           , MColorGradient GrLinear
                             [ (0, "orange"), (1, "cyan") ]
                             []
                           , MCornerRadius 10
                           , MStroke "yellow"
                           , MStrokeWidth 4
                           , MTooltip TTData
                           ]
                , encoding
                  . position X [ PName "date"
                               , PmType Temporal
                               , PTimeUnit (TU Month)
                               , PAxis [ AxLabelAlign AlignLeft
                                       , AxLabelExpr "datum.label[0]"
                                       ]
                               ]
                  . position Y [ PName "precipitation"
                               , PmType Quantitative
                               , PAggregate Mean
                               ]
                  $ []
                ]


-- From https://vega.github.io/vega-lite/examples/bar_negative.html
--
barNegative :: VegaLite
barNegative =
  let desc = "A bar chart with negative values. We can hide the axis domain line, and instead use a conditional grid color to draw a zero baseline."

      barData = dataFromColumns []
                . dataColumn "a" (Strings ["A", "B", "C", "D", "E", "F", "G", "H", "I"])
                . dataColumn "b" (Numbers [-28, 55, -33, 91, 81, 53, -19, 87, 52])
                $ []

  in toVegaLite [ description desc
                , barData
                , mark Bar []
                , encoding
                  . position X [ PName "a"
                               , PmType Ordinal
                               , PAxis [ AxDomain False
                                       , AxTicks False
                                       , AxLabelAngle 0
                                       , AxLabelPadding 4 ]
                               ]
                  . position Y [ PName "b"
                               , PmType Quantitative
                               , PAxis [ AxDataCondition
                                         (Expr "datum.value === 0")
                                         (CAxGridColor "black" "#ddd")
                                       ]
                               ]
                  $ []
                ]


-- https://vega.github.io/vega-lite/examples/bar_axis_space_saving.html
barAxisSpaceSaving :: VegaLite
barAxisSpaceSaving =
  let desc = "Bar Chart with a spacing-saving y-axis"

      enc = encoding
            . position X [PAggregate Count, PmType Quantitative, PAxis [AxGrid False]]
            . position Y [ PName "Origin"
                         , PmType Nominal
                         , PScale [SPadding 0]
                         , PBand 0.5
                         , PAxis yAx
                         ]

      yAx = [ AxBandPosition 0
            , AxGrid True
            , AxDomain False
            , AxTicks False
            , AxLabelAlign AlignLeft
            , AxLabelBaseline AlignMiddle
            , AxLabelPadding (-5)
            , AxLabelOffset (-15)
            , AxTitleX 5
            , AxTitleY (-5)
            , AxTitleAngle 0
            , AxTitleAlign AlignLeft
            ]

  in toVegaLite [ description desc
                , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
                , heightStep 50
                , mark Bar [MYOffset 5, MCornerRadiusEnd 2]
                , enc []
                ]


-- https://vega.github.io/vega-lite/examples/layer_bar_labels_grey.html
layerBarLabelsGrey :: VegaLite
layerBarLabelsGrey =
  let baseEnc = encoding
                . position Y [PName "Major_Genre", PmType Nominal, PAxis []]

      plot1 = [ mark Bar [MColor "#ddd"]
              , encoding
                . position X [ PAggregate Mean
                             , PName "IMDB_Rating"
                             , PmType Quantitative
                             , PScale [SDomain (DNumbers [0, 10])]
                             , PTitle "Mean IMDB Ratings"
                             ]
                $ []
              ]

      plot2 = [ mark Text [MAlign AlignLeft, MX 5]
              , encoding
                . text [TName "Major_Genre", TmType Nominal]
                . detail [DAggregate Count, DmType Quantitative]
                $ []
              ]

  in toVegaLite [ width 200
                , heightStep 16
                , movieData
                , baseEnc []
                , layer [asSpec plot1, asSpec plot2]
                ]
