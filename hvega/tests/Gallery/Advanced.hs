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
            ]


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
