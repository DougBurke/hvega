{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryLabel.elm (from development of version
-- 1.13.0)
--
module Gallery.Label (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("label1", label1)
            , ("label2", label2)
            , ("label3", label3)
            , ("label4", label4)
            , ("label5", label5)
            , ("label6", label6)
            , ("label7", label7)
            ]

label1 :: VegaLite
label1 =
    let
        des =
            description "A simple bar chart with embedded data labels"

        dvals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C" ])
                . dataColumn "b" (Numbers [ 28, 55, 43 ])

        enc =
            encoding
                . position X [ PName "b", PmType Quantitative ]
                . position Y [ PName "a", PmType Ordinal ]

        specBar =
            asSpec [ mark Bar [] ]

        specText =
            asSpec [ mark Text [ MStyle [ "label" ] ], encoding (text [ TName "b", TmType Quantitative ] []) ]

        config =
            configure . configuration (NamedStyle "label" [ MAlign AlignLeft, MBaseline AlignMiddle, MdX 3 ])
    in
    toVegaLite [ des, dvals [], enc [], layer [ specBar, specText ], config [] ]


label2 :: VegaLite
label2 =
    let
        des =
            description "Layering text over 'heatmap'"

        encPosition =
            encoding
                . position X [ PName "Cylinders", PmType Ordinal ]
                . position Y [ PName "Origin", PmType Ordinal ]

        encRect =
            encoding
                . color [ MName "*", MmType Quantitative, MAggregate Count ]

        specRect =
            asSpec [ mark Rect [], encRect [] ]

        encText =
            encoding
                . color [ MString "white" ]
                . text [ TName "*", TmType Quantitative, TAggregate Count ]

        specText =
            asSpec [ mark Text [], encText [] ]

        config =
            configure
                . configuration (Scale [ SCBandPaddingInner 0, SCBandPaddingOuter 0 ])
                . configuration (TextStyle [ MBaseline AlignMiddle ])
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , encPosition []
        , layer [ specRect, specText ]
        , config []
        ]


label3 :: VegaLite
label3 =
    let
        des =
            description "Carbon dioxide in the atmosphere."

        trans =
            transform
                . calculateAs "year(datum.Date)" "year"
                . calculateAs "month(datum.Date)" "month"
                . calculateAs "floor(datum.year / 10)" "decade"
                . calculateAs "(datum.year % 10) + (datum.month / 12)" "scaled_date"

        encPosition =
            encoding
                . position X
                    [ PName "scaled_date"
                    , PmType Quantitative
                    , PAxis [ AxTitle "Year into decade"
                            , AxTickCount 10
                            , AxValues (Numbers [1, 2, 3, 4, 5, 6, 7, 8, 9])
                            ]
                    ]
                . position Y
                    [ PName "CO2"
                    , PmType Quantitative
                    , PScale [ SZero False ]
                    , PAxis [ AxTitle "CO2 concentration in ppm" ]
                    ]

        encLine =
            encoding
                . color [ MName "decade", MmType Nominal, MLegend [] ]

        specLine =
            asSpec [ mark Line [ MOrient Vertical ], encLine [] ]

        transTextMin =
            transform
                . aggregate [ opAs (ArgMin Nothing) "scaled_date" "aggregated" ] [ "decade" ]
                . calculateAs "datum.aggregated.scaled_date" "scaled_date"
                . calculateAs "datum.aggregated.CO2" "CO2"

        encTextMin =
            encoding
                . text [ TName "aggregated.year", TmType Nominal ]

        specTextMin =
            asSpec [ transTextMin [], mark Text [ MAlign AlignLeft, MBaseline AlignTop, MdX 3, MdY 1 ], encTextMin [] ]

        transTextMax =
            transform
                . aggregate [ opAs (ArgMax Nothing) "scaled_date" "aggregated" ] [ "decade" ]
                . calculateAs "datum.aggregated.scaled_date" "scaled_date"
                . calculateAs "datum.aggregated.CO2" "CO2"

        encTextMax =
            encoding
                . text [ TName "aggregated.year", TmType Nominal ]

        specTextMax =
            asSpec [ transTextMax [], mark Text [ MAlign AlignLeft, MBaseline AlignBottom, MdX 3, MdY 1 ], encTextMax [] ]

        config =
            configure . configuration (View [ ViewStroke Nothing ])
    in
    toVegaLite
        [ des
        , config []
        , width 800
        , height 500
        , dataFromUrl "https://vega.github.io/vega-lite/data/co2-concentration.csv" [ Parse [ ( "Date", FoUtc "%Y-%m-%d" ) ] ]
        , trans []
        , encPosition []
        , layer [ specLine, specTextMin, specTextMax ]
        ]


label4 :: VegaLite
label4 =
    let
        des =
            description "Bar chart that highlights values beyond a threshold. The PM2.5 value of Beijing observed 15 days, highlighting the days when PM2.5 level is hazardous to human health. Data source https://chartaccent.github.io/chartaccent.html"

        dvals =
            dataFromColumns []
                . dataColumn "Day" (Numbers (map fromIntegral [1::Int .. 15]))
                . dataColumn "Value" (Numbers [ 54.8, 112.1, 63.6, 37.6, 79.7, 137.9, 120.1, 103.3, 394.8, 199.5, 72.3, 51.1, 112.0, 174.5, 130.5 ])

        encBar =
            encoding
                . position X [ PName "Day", PmType Ordinal, PAxis [ AxLabelAngle 0 ] ]
                . position Y [ PName "Value", PmType Quantitative ]

        specBar =
            asSpec [ mark Bar [], encBar [] ]

        trans =
            transform
                . filter (FExpr "datum.Value >= 300")
                . calculateAs "300" "baseline"

        encUpperBar =
            encoding
                . position X [ PName "Day", PmType Ordinal, PAxis [ AxLabelAngle 0 ] ]
                . position Y [ PName "baseline", PmType Quantitative ]
                . position Y2 [ PName "Value" ]
                . color [ MString "#e45755" ]

        specUpperBar =
            asSpec [ trans [], mark Bar [], encUpperBar [] ]

        layer0 =
            asSpec [ dvals [], layer [ specBar, specUpperBar ] ]

        thresholdData =
            dataFromRows []
                . dataRow [ ( "ThresholdValue", Number 300 ), ( "Threshold", Str "hazardous" ) ]

        specRule =
            asSpec [ mark Rule [], encRule [] ]

        encRule =
            encoding
                . position Y [ PName "ThresholdValue", PmType Quantitative ]

        specText =
            asSpec [ mark Text [ MAlign AlignRight, MdX (-2), MdY (-4) ], encText [] ]

        encText =
            encoding
                . position X [ PWidth ]
                . position Y [ PName "ThresholdValue", PmType Quantitative, PAxis [ AxTitle "PM2.5 Value" ] ]
                . text [ TName "Threshold", TmType Ordinal ]

        layer1 =
            asSpec [ thresholdData [], layer [ specRule, specText ] ]
    in
    toVegaLite [ des, layer [ layer0, layer1 ] ]


label5 :: VegaLite
label5 =
    let
        des =
            description "Monthly precipitation with mean value overlay"

        encBar =
            encoding
                . position X [ PName "date", PmType Ordinal, PTimeUnit Month ]
                . position Y [ PName "precipitation", PmType Quantitative, PAggregate Mean ]

        specBar =
            asSpec [ mark Bar [], encBar [] ]

        encLine =
            encoding
                . position Y [ PName "precipitation", PmType Quantitative, PAggregate Mean ]
                . color [ MString "red" ]
                . size [ MNumber 3 ]

        specLine =
            asSpec [ mark Rule [], encLine [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []
        , layer [ specBar, specLine ]
        ]


label6 :: VegaLite
label6 =
    let
        des =
            description "Histogram with global mean overlay"

        encBars =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PBin [], PAxis [] ]
                . position Y [ PmType Quantitative, PAggregate Count ]

        specBars =
            asSpec [ mark Bar [], encBars [] ]

        encMean =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PAggregate Mean ]
                . color [ MString "red" ]
                . size [ MNumber 5 ]

        specMean =
            asSpec [ mark Rule [], encMean [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , layer [ specBars, specMean ]
        ]


label7 :: VegaLite
label7 =
    let
        des =
            description "The population of the German city of Falkensee over time with annotated time periods highlighted"

        dvals =
            dataFromColumns [ Parse [ ( "year", FoDate "%Y" ) ] ]
                . dataColumn "year" (Strings [ "1875", "1890", "1910", "1925", "1933", "1939", "1946", "1950", "1964", "1971", "1981", "1985", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014" ])
                . dataColumn "population" (Numbers [ 1309, 1558, 4512, 8180, 15915, 24824, 28275, 29189, 29881, 26007, 24029, 23340, 22307, 22087, 22139, 22105, 22242, 22801, 24273, 25640, 27393, 29505, 32124, 33791, 35297, 36179, 36829, 37493, 38376, 39008, 39366, 39821, 40179, 40511, 40465, 40905, 41258, 41777 ])

        highlights =
            dataFromColumns [ Parse [ ( "start", FoDate "%Y" ), ( "end", FoDate "%Y" ) ] ]
                . dataColumn "start" (Strings [ "1933", "1948" ])
                . dataColumn "end" (Strings [ "1945", "1989" ])
                . dataColumn "event" (Strings [ "Nazi Rule", "GDR (East Germany)" ])

        encRects =
            encoding
                . position X [ PName "start", PmType Temporal, PTimeUnit Year, PAxis [] ]
                . position X2 [ PName "end", PTimeUnit Year ]
                . color [ MName "event", MmType Nominal ]

        specRects =
            asSpec [ highlights [], mark Rect [], encRects [] ]

        encPopulation =
            encoding
                . position X [ PName "year", PmType Temporal, PTimeUnit Year, PAxis [ AxNoTitle ] ]
                . position Y [ PName "population", PmType Quantitative ]
                . color [ MString "#333" ]

        specLine =
            asSpec [ mark Line [], encPopulation [] ]

        specPoints =
            asSpec [ mark Point [], encPopulation [] ]
    in
    toVegaLite [ des, width 500, dvals [], layer [ specRects, specLine, specPoints ] ]
