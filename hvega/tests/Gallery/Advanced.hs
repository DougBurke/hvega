{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryAdvanced.elm (from development of version
-- 1.13.0)
--
module Gallery.Advanced (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("advanced1", advanced1)
            , ("advanced2", advanced2)
            , ("advanced3", advanced3)
            , ("advanced4", advanced4)
            , ("advanced5", advanced5)
            , ("advanced6", advanced6)
            , ("advanced7", advanced7)
            , ("advanced8", advanced8)
            , ("advanced9", advanced9)
            , ("density1", density1)
            , ("density2", density2)
            ]


-- helpers
--
pQuant :: PositionChannel
pQuant = PmType Quantitative

fNominal :: FacetChannel
fNominal = FmType Nominal

mNominal :: MarkChannel
mNominal = MmType Nominal


advanced1 :: VegaLite
advanced1 =
    let
        desc =
            description "Calculation of percentage of total"

        dvals =
            dataFromColumns []
                . dataColumn "Activity" (Strings [ "Sleeping", "Eating", "TV", "Work", "Exercise" ])
                . dataColumn "Time" (Numbers [ 8, 2, 4, 8, 2 ])

        trans =
            transform
                . window
                    [ ( [ WAggregateOp Sum, WField "Time" ], "TotalTime" ) ]
                    [ WFrame Nothing Nothing ]
                . calculateAs "datum.Time/datum.TotalTime * 100" "PercentOfTotal"

        enc =
            encoding
                . position X [ PName "PercentOfTotal", PmType Quantitative, PAxis [ AxTitle "% of total time" ] ]
                . position Y [ PName "Activity", PmType Nominal ]
    in
    toVegaLite
        [ desc, dvals [], trans [], mark Bar [], heightStep 12, enc [] ]


advanced2 :: VegaLite
advanced2 =
    let
        desc =
            description "Calculation of difference from average"

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json"

        trans =
            transform
                . filter (FExpr "isValid(datum.IMDB_Rating)")
                . window [ ( [ WAggregateOp Mean, WField "IMDB_Rating" ], "AverageRating" ) ]
                    [ WFrame Nothing Nothing ]
                . filter (FExpr "(datum.IMDB_Rating - datum.AverageRating) > 2.5")

        barEnc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PAxis [ AxTitle "IMDB Rating" ] ]
                . position Y [ PName "Title", PmType Ordinal ]

        barSpec =
            asSpec [ mark Bar [], barEnc [] ]

        ruleEnc =
            encoding
                . position X [ PName "AverageRating", PAggregate Mean, PmType Quantitative ]

        ruleSpec =
            asSpec [ mark Rule [ MColor "red" ], ruleEnc [] ]
    in
    toVegaLite
        [ desc, dvals [], trans [], layer [ barSpec, ruleSpec ] ]


advanced3 :: VegaLite
advanced3 =
    let
        desc =
            description "Calculation of difference from annual average"

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json"
                [ Parse [ ( "Release_Date", FoDate "%d-%b-%y" ) ] ]

        trans =
            transform
                . filter (FExpr "isValid(datum.IMDB_Rating)")
                . timeUnitAs Year "Release_Date" "year"
                . window [ ( [ WAggregateOp Mean, WField "IMDB_Rating" ], "AverageYearRating" ) ]
                    [ WGroupBy [ "year" ], WFrame Nothing Nothing ]
                . filter (FExpr "(datum.IMDB_Rating - datum.AverageYearRating) > 2.5")

        barEnc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PAxis [ AxTitle "IMDB Rating" ] ]
                . position Y [ PName "Title", PmType Ordinal ]

        barSpec =
            asSpec [ mark Bar [ MClip True ], barEnc [] ]

        tickEnc =
            encoding
                . position X [ PName "AverageYearRating", PmType Quantitative ]
                . position Y [ PName "Title", PmType Ordinal ]
                . color [ MString "red" ]

        tickSpec =
            asSpec [ mark Tick [], tickEnc [] ]
    in
    toVegaLite [ desc, dvals, trans [], layer [ barSpec, tickSpec ] ]


advanced4 :: VegaLite
advanced4 =
    let
        desc =
            description "A scatterplot showing each movie in the database and the difference from the average movie rating."

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json"

        trans =
            transform
                . filter (FExpr "isValid(datum.IMDB_Rating)")
                . filter (FRange "Release_Date" (DateRange [] [ DTYear 2019 ]))
                . window [ ( [ WAggregateOp Mean, WField "IMDB_Rating" ], "AverageRating" ) ]
                    [ WFrame Nothing Nothing ]
                . calculateAs "datum.IMDB_Rating - datum.AverageRating" "RatingDelta"

        enc =
            encoding
                . position X [ PName "Release_Date", PmType Temporal ]
                . position Y
                    [ PName "RatingDelta"
                    , PmType Quantitative
                    , PAxis [ AxTitle "Residual" ]
                    ]
    in
    toVegaLite
        [ desc
        , dvals []
        , trans []
        , enc []
        , mark Point [ MStrokeWidth 0.3, MOpacity 0.3 ]
        ]


advanced5 :: VegaLite
advanced5 =
    let
        des =
            description "Line chart showing ranks over time for thw World Cup 2018 Group F teams"

        dvals =
            dataFromColumns []
                . dataColumn "team" (Strings [ "Germany", "Mexico", "South Korea", "Sweden", "Germany", "Mexico", "South Korea", "Sweden", "Germany", "Mexico", "South Korea", "Sweden" ])
                . dataColumn "matchday" (Numbers [ 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3 ])
                . dataColumn "point" (Numbers [ 0, 3, 0, 3, 3, 6, 0, 3, 3, 6, 3, 6 ])
                . dataColumn "diff" (Numbers [ -1, 1, -1, 1, 0, 2, -2, 0, -2, -1, 0, 3 ])

        trans =
            transform
                . window [ ( [ WOp Rank ], "rank" ) ]
                    [ WSort [ WDescending "point", WDescending "diff" ], WGroupBy [ "matchday" ] ]

        enc =
            encoding
                . position X [ PName "matchday", PmType Ordinal ]
                . position Y [ PName "rank", PmType Ordinal ]
                . color [ MName "team", MmType Nominal, MScale teamColours ]

        teamColours =
            categoricalDomainMap
                [ ( "Germany", "black" )
                , ( "Mexico", "#127153" )
                , ( "South Korea", "#c91a3c" )
                , ( "Sweden", "#0c71ab" )
                ]
    in
    toVegaLite
        [ des
        , title "World Cup 2018: Group F Rankings" [ TFrame FrBounds, TFontStyle "italic" ]
        , dvals []
        , trans []
        , enc []
        , mark Line [ MOrient Vertical ]
        ]


advanced6 :: VegaLite
advanced6 =
    let
        des =
            description "Waterfall chart of monthly profit and loss"

        dvals =
            dataFromColumns []
                . dataColumn "label" (Strings [ "Begin", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "End" ])
                . dataColumn "amount" (Numbers [ 4000, 1707, -1425, -1030, 1812, -1067, -1481, 1228, 1176, 1146, 1205, -1388, 1492, 0 ])

        trans =
            transform
                . window [ ( [ WAggregateOp Sum, WField "amount" ], "sum" ) ] []
                . window [ ( [ WOp Lead, WField "label" ], "lead" ) ] []
                . calculateAs "datum.lead === null ? datum.label : datum.lead" "lead"
                . calculateAs "datum.label === 'End' ? 0 : datum.sum - datum.amount" "previous_sum"
                . calculateAs "datum.label === 'End' ? datum.sum : datum.amount" "amount"
                . calculateAs "(datum.label !== 'Begin' && datum.label !== 'End' && datum.amount > 0 ? '+' : '') + datum.amount" "text_amount"
                . calculateAs "(datum.sum + datum.previous_sum) / 2" "center"
                . calculateAs "datum.sum < datum.previous_sum ? datum.sum : ''" "sum_dec"
                . calculateAs "datum.sum > datum.previous_sum ? datum.sum : ''" "sum_inc"

        enc =
            encoding
                . position X [ PName "label", PmType Ordinal, PSort [], PTitle "Months" ]

        enc1 =
            encoding
                . position Y [ PName "previous_sum", PmType Quantitative, PTitle "Amount" ]
                . position Y2 [ PName "sum" ]
                . color
                    [ MDataCondition
                        [ ( Expr "datum.label === 'Begin' || datum.label === 'End'", [ MString "#f7e0b6" ] )
                        , ( Expr "datum.sum < datum.previous_sum", [ MString "#f78a64" ] )
                        ]
                        [ MString "#93c4aa" ]
                    ]

        spec1 =
            asSpec [ enc1 [], mark Bar [ MSize 45 ] ]

        enc2 =
            encoding
                . position X2 [ PName "lead" ]
                . position Y [ PName "sum", PmType Quantitative ]

        spec2 =
            asSpec
                [ enc2 []
                , mark Rule
                    [ MColor "#404040"
                    , MOpacity 1
                    , MStrokeWidth 2
                    , MXOffset (-22.5)
                    , MX2Offset 22.5
                    ]
                ]

        enc3 =
            encoding
                . position Y [ PName "sum_inc", PmType Quantitative ]
                . text [ TName "sum_inc", TmType Nominal ]

        spec3 =
            asSpec
                [ enc3 []
                , mark Text
                    [ MdY (-8)
                    , MFontWeight Bold
                    , MColor "#404040"
                    ]
                ]

        enc4 =
            encoding
                . position Y [ PName "sum_dec", PmType Quantitative ]
                . text [ TName "sum_dec", TmType Nominal ]

        spec4 =
            asSpec
                [ enc4 []
                , mark Text
                    [ MdY 8
                    , MBaseline AlignTop
                    , MFontWeight Bold
                    , MColor "#404040"
                    ]
                ]

        enc5 =
            encoding
                . position Y [ PName "center", PmType Quantitative ]
                . text [ TName "text_amount", TmType Nominal ]
                . color
                    [ MDataCondition
                        [ ( Expr "datum.label === 'Begin' || datum.label === 'End'"
                          , [ MString "#725a30" ]
                          )
                        ]
                        [ MString "white" ]
                    ]

        spec5 =
            asSpec [ enc5 [], mark Text [ MBaseline AlignMiddle, MFontWeight Bold ] ]
    in
    toVegaLite
        [ des
        , width 800
        , height 450
        , dvals []
        , trans []
        , enc []
        , layer [ spec1, spec2, spec3, spec4, spec5 ]
        ]


-- advanced9 in elm
advanced7 :: VegaLite
advanced7 =
    let
        des =
            description "Using the lookup transform to combine data"

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/lookup_groups.csv"

        trans =
            transform
                . lookup "person"
                    (dataFromUrl "https://vega.github.io/vega-lite/data/lookup_people.csv" [])
                    "name"
                    [ "age", "height" ]

        enc =
            encoding
                . position X [ PName "group", PmType Ordinal ]
                . position Y [ PName "age", PmType Quantitative, PAggregate Mean ]
    in
    toVegaLite [ des, dvals [], trans [], enc [], mark Bar [] ]


-- advanced14 in elm
advanced8 :: VegaLite
advanced8 =
    let
        des =
            description "Parallel coordinates plot with manual generation of parallel axes"

        cfg =
            configure
                . configuration (View [ ViewStroke Nothing ])
                . configuration (AxisX [ Domain False, LabelAngle 0, TickColor "#ccc" ])
                . configuration
                    (NamedStyles
                        [ ( "label", [ MBaseline AlignMiddle, MAlign AlignRight, MdX (-5), MTooltip TTNone ] )
                        , ( "tick", [ MOrient Horizontal, MTooltip TTNone ] )
                        ]
                    )

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/iris.json"

        trans =
            transform
                . window [ ( [ WAggregateOp Count ], "index" ) ] []
                . fold [ "petalLength", "petalWidth", "sepalLength", "sepalWidth" ]
                . joinAggregate [ opAs Min "value" "min", opAs Max "value" "max" ] [ WGroupBy [ "key" ] ]
                . calculateAs "(datum.value - datum.min) / (datum.max-datum.min)" "normVal"
                . calculateAs "(datum.min + datum.max) / 2" "mid"

        fields = ["petalLength", "petalWidth", "sepalLength", "sepalWidth"]
        encLine =
            encoding
                . position X [ PName "key", PmType Nominal ]
                . position Y [ PName "normVal", PmType Quantitative, PAxis [] ]
                . color [ MName "species", MmType Nominal ]
                . detail [ DName "index", DmType Nominal ]
                . tooltips (map (\n -> [TName n, TmType Quantitative]) fields)

        specLine =
            asSpec [ encLine [], mark Line [ MOpacity 0.3 ] ]

        encAxis =
            encoding
                . position X [ PName "key", PmType Nominal, PAxis [ AxNoTitle ] ]
                . detail [ DAggregate Count, DmType Quantitative ]

        specAxis =
            asSpec [ encAxis [], mark Rule [ MColor "#ccc" ] ]

        encAxisLabelsTop =
            encoding
                . position X [ PName "key", PmType Nominal ]
                . position Y [ PNumber 0 ]
                . text [ TName "max", TmType Quantitative, TAggregate Max ]

        specAxisLabelsTop =
            asSpec [ encAxisLabelsTop [], mark Text [ MStyle [ "label" ] ] ]

        encAxisLabelsMid =
            encoding
                . position X [ PName "key", PmType Nominal ]
                . position Y [ PNumber 150 ]
                . text [ TName "mid", TmType Quantitative, TAggregate Min ]

        specAxisLabelsMid =
            asSpec [ encAxisLabelsMid [], mark Text [ MStyle [ "label" ] ] ]

        encAxisLabelsBot =
            encoding
                . position X [ PName "key", PmType Nominal ]
                . position Y [ PHeight ]
                . text [ TName "min", TmType Quantitative, TAggregate Min ]

        specAxisLabelsBot =
            asSpec [ encAxisLabelsBot [], mark Text [ MStyle [ "label" ] ] ]
    in
    toVegaLite
        [ des
        , cfg []
        , width 600
        , height 300
        , dvals []
        , trans []
        , layer [ specLine, specAxis, specAxisLabelsTop, specAxisLabelsMid, specAxisLabelsBot ]
        ]


-- advanced15 in elm
advanced9 :: VegaLite
advanced9 =
    let
        desc =
            description "Production budget of the film with highest US Gross in each major genre."

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json"

        enc =
            encoding
                . position X
                    [ PName "Production_Budget"
                    , PmType Quantitative
                    , PAggregate (ArgMax (Just "US_Gross"))
                    ]
                . position Y [ PName "Major_Genre", PmType Nominal ]
    in
    toVegaLite [ desc, dvals [], enc [], mark Bar [] ]


{-

-- advanced7 in elm
advanced7 : Spec
advanced7 =
    let
        des =
            description "Filtering the top-k items"

        data =
            dataFromColumns []
                << dataColumn "student" (strs [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V" ])
                << dataColumn "score" (nums [ 100, 56, 88, 65, 45, 23, 66, 67, 13, 12, 50, 78, 66, 30, 97, 75, 24, 42, 76, 78, 21, 46 ])

        trans =
            transform
                << window [ ( [ wiOp woRank ], "rank" ) ]
                    [ wiSort [ wiDescending "score" ] ]
                << filter (fiExpr "datum.rank <=5")

        enc =
            encoding
                << position X [ pName "score", pQuant ]
                << position Y
                    [ pName "student"
                    , pNominal
                    , pSort [ soByField "score" opMean, soDescending ]
                    ]
    in
    toVegaLite [ des, data [], trans [], enc [], bar [] ]


advanced8 : Spec
advanced8 =
    let
        des =
            description "Top-k items with 'others'"

        data =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []

        trans =
            transform
                << aggregate [ opAs opMean "Worldwide_Gross" "aggregateGross" ] [ "Director" ]
                << window [ ( [ wiOp woRowNumber ], "rank" ) ]
                    [ wiSort [ wiDescending "aggregateGross" ] ]
                << calculateAs "datum.rank < 10 ? datum.Director : 'All Others'" "rankedDirector"

        enc =
            encoding
                << position X
                    [ pName "aggregateGross"
                    , pQuant
                    , pAggregate opMean
                    , pTitle ""
                    ]
                << position Y
                    [ pName "rankedDirector"
                    , pOrdinal
                    , pSort [ soByField "aggregateGross" opMean, soDescending ]
                    , pTitle ""
                    ]
    in
    toVegaLite
        [ des
        , title "Top Directors by Average Worldwide Gross" []
        , data
        , trans []
        , enc []
        , bar []
        ]

advanced10 : Spec
advanced10 =
    let
        des =
            description "Cumulative Frequency Distribution"

        data =
            dataFromUrl "https://gicentre.github.io/data/putneyAirQuality2018.csv"
                [ parse [ ( "NOX", foNum ) ] ]

        trans =
            transform
                << window [ ( [ wiAggregateOp opCount ], "cumulativeCount" ) ]
                    [ wiSort [ wiAscending "NOX" ] ]

        enc =
            encoding
                << position X
                    [ pName "NOX"
                    , pQuant
                    , pTitle "NOX concentration (μg/㎥)"
                    ]
                << position Y [ pName "cumulativeCount", pQuant ]
    in
    toVegaLite [ des, width 500, data, trans [], enc [], area [] ]


advanced11 : Spec
advanced11 =
    let
        des =
            description "Layered Histogram and Cumulative Histogram"

        data =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []

        trans =
            transform
                << binAs [] "IMDB_Rating" "binIMDB_Rating"
                << aggregate [ opAs opCount "" "count" ] [ "binIMDB_Rating", "binIMDB_Rating_end" ]
                << filter (fiExpr "datum.binIMDB_Rating !== null")
                << window [ ( [ wiAggregateOp opSum, wiField "count" ], "cumulativeCount" ) ]
                    [ wiSort [ wiAscending "binIMDB_Rating" ], wiFrame Nothing (Just 0) ]

        enc =
            encoding
                << position X
                    [ pName "binIMDB_Rating"
                    , pQuant
                    , pScale [ scZero False ]
                    , pTitle "IMDB Rating"
                    ]
                << position X2 [ pName "binIMDB_Rating_end" ]

        enc1 =
            encoding
                << position Y [ pName "cumulativeCount", pQuant ]

        enc2 =
            encoding
                << position Y [ pName "count", pQuant ]
    in
    toVegaLite
        [ des
        , data
        , trans []
        , enc []
        , layer
            [ asSpec [ enc1 [], bar [] ]
            , asSpec [ enc2 [], bar [ maColor "yellow", maOpacity 0.5 ] ]
            ]
        ]

-}

-- advanced12 in elm
--
-- TODO: overplot the actual values
density1 :: VegaLite
density1 =
  let dvals = dataFromUrl "https://vega.github.io/vega-lite/data/iris.json" []

      trans = transform
              . foldAs [ "petalWidth", "petalLength", "sepalWidth", "sepalLength" ] "measurement" "value"
              . density "value" [ DnBandwidth 0.3, DnGroupBy [ "measurement" ] ]

      enc = encoding
            . position X [ PName "value", pQuant, PTitle "width/length (cm)" ]
            . position Y [ PName "density", pQuant ]
            . row [ FName "measurement", fNominal ]

  in toVegaLite [ width 300, height 50, dvals, trans [], enc [], mark Area [] ]


-- advanced13 in elm
--
density2 :: VegaLite
density2 =
  let dvals = dataFromUrl "https://vega.github.io/vega-lite/data/iris.json" []

      trans = transform
              . foldAs [ "petalWidth", "petalLength", "sepalWidth", "sepalLength" ] "measurement" "value"
              . density "value"
                    [ DnBandwidth 0.3
                    , DnGroupBy [ "measurement" ]
                    , DnExtent (Number 0) (Number 8)
                    , DnSteps 200
                    ]

      enc = encoding
            . position X [ PName "value", pQuant, PTitle "width/length (cm)" ]
            . position Y [ PName "density", pQuant ]
            . color [ MName "measurement", mNominal ]

  in toVegaLite [ width 400, height 100, dvals, trans [], enc [], mark Area [ MOpacity 0.5 ] ]


{-
advanced16 : Spec
advanced16 =
    let
        desc =
            description "Plot showing average data with raw values in the background."

        data =
            dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []

        trans =
            transform << filter (fiExpr "datum.symbol === 'GOOG'")

        encRaw =
            encoding
                << position X [ pName "date", pTemporal, pTimeUnit year ]
                << position Y [ pName "price", pQuant ]

        specRaw =
            asSpec [ encRaw [], point [ maOpacity 0.3 ] ]

        encAv =
            encoding
                << position X [ pName "date", pTemporal, pTimeUnit year ]
                << position Y [ pName "price", pAggregate opMean, pQuant ]

        specAv =
            asSpec [ encAv [], line [] ]
    in
    toVegaLite [ desc, data, trans [], layer [ specRaw, specAv ] ]


advanced17 : Spec
advanced17 =
    let
        desc =
            description "Plot showing a 30 day rolling average with raw values in the background."

        data =
            dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []

        trans =
            transform
                << window [ ( [ wiAggregateOp opMean, wiField "temp_max" ], "rollingMean" ) ]
                    [ wiFrame (Just -15) (Just 15) ]

        encRaw =
            encoding
                << position X [ pName "date", pTitle "Date", pTemporal ]
                << position Y [ pName "temp_max", pTitle "Maximum temperature", pQuant ]

        specRaw =
            asSpec [ encRaw [], point [ maOpacity 0.3 ] ]

        encAv =
            encoding
                << position X [ pName "date", pTemporal ]
                << position Y [ pName "rollingMean", pQuant ]

        specAv =
            asSpec [ encAv [], line [ maColor "red", maSize 3 ] ]
    in
    toVegaLite [ desc, width 400, height 300, data, trans [], layer [ specRaw, specAv ] ]


advanced18 : Spec
advanced18 =
    let
        desc =
            description "Line chart to show benchmarking results."

        falconData =
            JE.list JE.float [ 16.81999969482422, 19.759998321533203, 16.079999923706055, 19.579999923706055, 16.420000076293945, 16.200000762939453, 16.020000457763672, 15.9399995803833, 16.280000686645508, 16.119998931884766, 16.15999984741211, 16.119998931884766, 16.139999389648438, 16.100000381469727, 16.200000762939453, 16.260000228881836, 19.35999870300293, 19.700000762939453, 15.9399995803833, 19.139999389648438, 16.200000762939453, 16.119998931884766, 19.520000457763672, 19.700000762939453, 16.200000762939453, 20.979999542236328, 16.299999237060547, 16.420000076293945, 16.81999969482422, 16.5, 16.560001373291016, 16.18000030517578, 16.079999923706055, 16.239999771118164, 16.040000915527344, 16.299999237060547, 19.399999618530273, 15.699999809265137, 16.239999771118164, 15.920000076293945, 16.259998321533203, 16.219999313354492, 16.520000457763672, 16.459999084472656, 16.360000610351563, 15.719999313354492, 16.060001373291016, 15.960000991821289, 16.479999542236328, 16.600000381469727, 16.240001678466797, 16.940000534057617, 16.220001220703125, 15.959999084472656, 15.899999618530273, 16.479999542236328, 16.31999969482422, 15.75999927520752, 15.999998092651367, 16.18000030517578, 16.219999313354492, 15.800000190734863, 16.139999389648438, 16.299999237060547, 16.360000610351563, 16.260000228881836, 15.959999084472656, 15.9399995803833, 16.53999900817871, 16.139999389648438, 16.259998321533203, 16.200000762939453, 15.899999618530273, 16.079999923706055, 16.079999923706055, 15.699999809265137, 15.660000801086426, 16.139999389648438, 23.100000381469727, 16.600000381469727, 16.420000076293945, 16.020000457763672, 15.619999885559082, 16.35999870300293, 15.719999313354492, 15.920001029968262, 15.5600004196167, 16.34000015258789, 22.82000160217285, 15.660000801086426, 15.5600004196167, 16, 16, 15.819999694824219, 16.399999618530273, 16.46000099182129, 16.059999465942383, 16.239999771118164, 15.800000190734863, 16.15999984741211, 16.360000610351563, 19.700000762939453, 16.10000228881836, 16.139999389648438, 15.819999694824219, 16.439998626708984, 16.139999389648438, 16.020000457763672, 15.860000610351563, 16.059999465942383, 16.020000457763672, 15.920000076293945, 15.819999694824219, 16.579999923706055, 15.880000114440918, 16.579999923706055, 15.699999809265137, 19.380001068115234, 19.239999771118164, 16, 15.980000495910645, 15.959999084472656, 16.200000762939453, 15.980000495910645, 16.34000015258789, 16.31999969482422, 16.260000228881836, 15.920000076293945, 15.540000915527344, 16.139999389648438, 16.459999084472656, 16.34000015258789, 15.819999694824219, 19.719999313354492, 15.75999927520752, 16.499998092651367, 15.719999313354492, 16.079999923706055, 16.439998626708984, 16.200000762939453, 15.959999084472656, 16, 16.100000381469727, 19.31999969482422, 16.100000381469727, 16.18000030517578, 15.959999084472656, 22.639999389648438, 15.899999618530273, 16.279998779296875, 16.100000381469727, 15.920000076293945, 16.079999923706055, 16.260000228881836, 15.899999618530273, 15.820001602172852, 15.699999809265137, 15.979998588562012, 16.380001068115234, 16.040000915527344, 19.420000076293945, 15.9399995803833, 16.15999984741211, 15.960000991821289, 16.259998321533203, 15.780000686645508, 15.880000114440918, 15.980000495910645, 16.060001373291016, 16.119998931884766, 23.020000457763672, 15.619999885559082, 15.920000076293945, 16.060001373291016, 14.780000686645508, 16.260000228881836, 19.520000457763672, 16.31999969482422, 16.600000381469727, 16.219999313354492, 19.740001678466797, 19.46000099182129, 15.940000534057617, 15.839999198913574, 16.100000381469727, 16.46000099182129, 16.17999839782715, 16.100000381469727, 15.9399995803833, 16.060001373291016, 15.860000610351563, 15.819999694824219, 16.03999900817871, 16.17999839782715, 15.819999694824219, 17.299999237060547, 15.9399995803833, 15.739999771118164, 15.719999313354492, 15.679998397827148, 15.619999885559082, 15.600000381469727, 16.03999900817871, 15.5, 15.600001335144043, 19.439998626708984, 15.960000991821289, 16.239999771118164, 16.040000915527344, 16.239999771118164 ]

        squareData =
            JE.list JE.float [ 24.200000762939453, 17.899999618530273, 15.800000190734863, 58.400001525878906, 151, 2523.10009765625, 245.3000030517578, 136, 72.30000305175781, 55.70000076293945, 42.400001525878906, 37.70000076293945, 30.100000381469727, 30.100000381469727, 21.799999237060547, 20.600000381469727, 21.799999237060547, 17.600000381469727, 18.200000762939453, 21, 941.7000122070313, 177.39999389648438, 2821.800048828125, 359.20001220703125, 318, 217.10000610351563, 126, 69, 57.79999923706055, 45.29999923706055, 35.599998474121094, 29.100000381469727, 23.799999237060547, 44.20000076293945, 17.700000762939453, 17.700000762939453, 15.699999809265137, 27.799999237060547, 22.799999237060547, 3853.60009765625, 91.5999984741211, 181.39999389648438, 476.29998779296875, 265.8999938964844, 254.60000610351563, 2583.199951171875, 124.80000305175781, 73.19999694824219, 56.400001525878906, 48.70000076293945, 41.599998474121094, 21.100000381469727, 20.299999237060547, 21.299999237060547, 18.299999237060547, 17.100000381469727, 19.5, 828.2000122070313, 162.1999969482422, 217.89999389648438, 205.5, 197.60000610351563, 2249.800048828125, 103.0999984741211, 71.69999694824219, 57.599998474121094, 41.400001525878906, 34.5, 22, 20.5, 21.700000762939453, 18.299999237060547, 17.299999237060547, 19.399999618530273, 666.7999877929688, 214.89999389648438, 212.3000030517578, 125.80000305175781, 67.69999694824219, 56.099998474121094, 45.79999923706055, 38.29999923706055, 33, 35.400001525878906, 22.700000762939453, 19.399999618530273, 19.899999618530273, 24.100000381469727, 19.299999237060547, 21.299999237060547, 3508.699951171875, 204.10000610351563, 125.4000015258789, 65.30000305175781, 60.79999923706055, 44.099998474121094, 36.29999923706055, 30.5, 28.600000381469727, 16.5, 18.600000381469727, 23.700000762939453, 22.299999237060547, 17.600000381469727, 19.200000762939453, 448.79998779296875, 124.4000015258789, 66.5999984741211, 53.5, 51, 45.20000076293945, 28.399999618530273, 29.200000762939453, 26.700000762939453, 25.899999618530273, 18.100000381469727, 17.600000381469727, 20.100000381469727, 25.200000762939453, 3332, 67.5, 53.599998474121094, 56.599998474121094, 39.900001525878906, 27.600000381469727, 29.600000381469727, 33.5, 17.200000762939453, 18.799999237060547, 25.200000762939453, 16.700000762939453, 16.899999618530273, 240.1999969482422, 52.400001525878906, 42.099998474121094, 33.900001525878906, 28, 28.600000381469727, 17.299999237060547, 20, 21, 22.799999237060547, 16.700000762939453, 19.200000762939453, 175.39999389648438, 43.5, 34.70000076293945, 29.700000762939453, 34.900001525878906, 25.799999237060547, 17.299999237060547, 22.600000381469727, 17.600000381469727, 17.200000762939453, 19.200000762939453, 111.80000305175781, 35.400001525878906, 27.600000381469727, 25.399999618530273, 21.899999618530273, 18.600000381469727, 18.100000381469727, 21.200000762939453, 17.899999618530273, 17, 80.5999984741211, 29.799999237060547, 30.100000381469727, 16, 26.799999237060547, 17.5, 22.299999237060547, 16.799999237060547, 22.399999618530273, 77.4000015258789, 31, 29.700000762939453, 28.700000762939453, 26, 16.899999618530273, 15.800000190734863, 19, 52.599998474121094, 25.200000762939453, 16.700000762939453, 17.899999618530273, 21, 19.799999237060547, 18.799999237060547, 46.5, 17.5, 16.799999237060547, 18.299999237060547, 18.299999237060547, 14.899999618530273, 41, 18.299999237060547, 17.299999237060547, 17, 17.5, 32.29999923706055, 22.600000381469727, 16.600000381469727, 17.899999618530273, 25.600000381469727, 17.5, 20.299999237060547, 25.200000762939453, 18.600000381469727, 17.700000762939453 ]

        transSquare =
            transform
                << window [ ( [ wiOp woRowNumber ], "row" ) ] []
                << calculateAs "1000/datum.data" "fps"
                << calculateAs "'Square Crossfilter (3M)'" "system"

        transFalcon =
            transform
                << window [ ( [ wiOp woRowNumber ], "row" ) ] []
                << calculateAs "1000/datum.data" "fps"
                << calculateAs "'Falcon'" "system"

        enc =
            encoding
                << position X [ pName "row", pQuant, pAxis [ axGrid False, axTitle "Trial" ], pScale [ scNice niFalse ] ]
                << position Y [ pName "fps", pQuant, pAxis [ axGrid False, axTitle "Frames per Second (fps)" ], pScale [ scType scLog ] ]
                << color [ mName "system", mNominal, mLegend [ leOrient loBottomRight, leTitle "System" ] ]
                << size [ mNum 1 ]

        specFalcon =
            asSpec [ dataFromSource "falcon" [], transFalcon [], line [] ]

        specSquare =
            asSpec [ dataFromSource "square" [], transSquare [], line [] ]
    in
    toVegaLite
        [ desc
        , width 400
        , height 200
        , datasets [ ( "falcon", dataFromJson falconData [] ), ( "square", dataFromJson squareData [] ) ]
        , enc []
        , layer [ specFalcon, specSquare ]
        ]

-}
