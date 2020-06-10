{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite WindowTransformTests.elm as of version 1.12.0
--
module WindowTransformTests (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("window1", window1)
            , ("window2", window2)
            , ("window3", window3)
            , ("window4", window4)
            , ("window5", window5)
            , ("window6", window6)
            , ("window7", window7)
            , ("joinAggregate1", joinAggregate1)
            , ("joinAggregate2", joinAggregate2)
            , ("joinAggregate3", joinAggregate3)
            ]

window1 :: VegaLite
window1 =
    let dataVals =
            dataFromColumns []
                . dataColumn "Activity" (Strings [ "Sleeping", "Eating", "TV", "Work", "Exercise" ])
                . dataColumn "Time" (Numbers [ 8, 2, 4, 8, 2 ])

        trans =
            transform
                . window [ ( [ WAggregateOp Sum, WField "Time" ], "TotalTime" ) ]
                         [ WFrame Nothing Nothing ]
                . calculateAs "datum.Time/datum.TotalTime * 100" "PercentOfTotal"

        enc =
            encoding
                . position X [ PName "PercentOfTotal", PmType Quantitative, PAxis [ AxTitle "% of total time" ] ]
                . position Y [ PName "Activity", PmType Nominal ]
    in
    toVegaLite [ dataVals [], trans [], mark Bar [], heightStep 12, enc [] ]


window2 :: VegaLite
window2 =
    let dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json"

        trans =
            transform
                . filter (FExpr "datum.IMDB_Rating != null")
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
    toVegaLite [ dataVals [], trans [], layer [ barSpec, ruleSpec ] ]


movieData :: Data
movieData = dataFromUrl "https://vega.github.io/vega-lite/data/movies.json"
                [ Parse [ ( "Release_Date", FoDate "%b %d %Y" ) ] ]

window3 :: VegaLite
window3 =
    let trans =
            transform
                . filter (FExpr "datum.IMDB_Rating != null")
                . timeUnitAs (TU Year) "Release_Date" "year"
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

        tickSpec = asSpec [ mark Tick [], tickEnc [] ]
            
    in toVegaLite [ movieData, trans [], layer [ barSpec, tickSpec ] ]


window4 :: VegaLite
window4 =
    let trans =
            transform
                . filter (FExpr "datum.IMDB_Rating != null")
                . filter (FRange "Release_Date" (DateRange [] [ DTYear 2019 ]))
                . window [ ( [ WAggregateOp Mean, WField "IMDB_Rating" ], "AverageRating" ) ]
                    [ WFrame Nothing Nothing ]
                . calculateAs "datum.IMDB_Rating - datum.AverageRating" "RatingDelta"

        enc =
            encoding
                . position X [ PName "Release_Date", PmType Temporal ]
                . position Y [ PName "RatingDelta", PmType Quantitative, PAxis [ AxTitle "Residual" ] ]
                
    in toVegaLite [ movieData, trans [], enc [], mark Point [ MStrokeWidth 0.3, MOpacity 0.3 ] ]


window5 :: VegaLite
window5 =
    let dataVals =
            dataFromColumns []
                . dataColumn "team" (Strings [ "Man Utd", "Chelsea", "Man City", "Spurs", "Man Utd", "Chelsea", "Man City", "Spurs", "Man Utd", "Chelsea", "Man City", "Spurs" ])
                . dataColumn "matchday" (Numbers [ 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3 ])
                . dataColumn "point" (Numbers [ 3, 1, 1, 0, 6, 1, 0, 3, 9, 1, 0, 6 ])

        trans =
            transform
                . window
                    [ ( [ WOp Rank ], "rank" ) ]
                    [ WSort [ WDescending "point" ], WGroupBy [ "matchday" ] ]

        enc =
            encoding
                . position X [ PName "matchday", PmType Ordinal ]
                . position Y [ PName "rank", PmType Ordinal ]
                . color [ MName "team", MmType Nominal, MScale teamColours ]

        teamColours =
            categoricalDomainMap
                [ ( "Man Utd", "#cc2613" )
                , ( "Chelsea", "#125dc7" )
                , ( "Man City", "#8bcdfc" )
                , ( "Spurs", "#d1d1d1" )
                ]
                
    in toVegaLite [ width 400, height 400, dataVals [], trans [], enc [], mark Line [ MOrient Vertical ] ]


window6 :: VegaLite
window6 =
    let dataVals =
            dataFromColumns []
                . dataColumn "student" (Strings [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V" ])
                . dataColumn "score" (Numbers [ 100, 56, 88, 65, 45, 23, 66, 67, 13, 12, 50, 78, 66, 30, 97, 75, 24, 42, 76, 78, 21, 46 ])

        trans =
            transform
                . window [ ( [ WOp Rank ], "rank" ) ] [ WSort [ WDescending "score" ] ]
                . filter (FExpr "datum.rank <= 5")

        enc =
            encoding
                . position X [ PName "score", PmType Quantitative ]
                . position Y
                    [ PName "student"
                    , PmType Nominal
                    , PSort [ ByFieldOp "score" Mean, Descending ]
                    ]
                    
    in toVegaLite [ dataVals [], trans [], enc [], mark Bar [] ]

window7 :: VegaLite
window7 =
    let dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        trans =
            transform
                . filter (FExpr "datum.Miles_per_Gallon !== null")
                . timeUnitAs (TU Year) "Year" "year"
                . window [ ( [ WAggregateOp Mean, WField "Miles_per_Gallon" ], "Average_MPG" ) ]
                    [ WSort [ WAscending "year" ], WIgnorePeers False, WFrame Nothing (Just 0) ]

        circleEnc =
            encoding
                . position X [ PName "Year", PmType Temporal, PTimeUnit (TU Year) ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]

        circleSpec =
            asSpec [ mark Circle [], circleEnc [] ]

        lineEnc =
            encoding
                . position X [ PName "Year", PmType Temporal, PTimeUnit (TU Year) ]
                . position Y [ PName "Average_MPG", PmType Quantitative, PAxis [ AxTitle "Miles per gallon" ] ]

        lineSpec =
            asSpec [ mark Line [ MColor "red" ], lineEnc [] ]
            
    in toVegaLite [ width 500, height 400, dataVals, trans [], layer [ circleSpec, lineSpec ] ]


joinAggregate1 :: VegaLite
joinAggregate1 =
    let dataVals =
            dataFromColumns []
                . dataColumn "Activity" (Strings [ "Sleeping", "Eating", "TV", "Work", "Exercise" ])
                . dataColumn "Time" (Numbers [ 8, 2, 4, 8, 2 ])

        trans =
            transform
                . joinAggregate [ opAs Sum "Time" "TotalTime" ] []
                . calculateAs "datum.Time/datum.TotalTime * 100" "PercentOfTotal"

        enc =
            encoding
                . position X [ PName "PercentOfTotal", PmType Quantitative, PAxis [ AxTitle "% of total Time" ] ]
                . position Y [ PName "Activity", PmType Nominal ]
                
    in toVegaLite [ dataVals [], trans [], enc [], mark Bar [], heightStep 12 ]

joinAggregate2 :: VegaLite
joinAggregate2 =
    let dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json"

        trans =
            transform
                . filter (FExpr "datum.IMDB_Rating != null")
                . joinAggregate [ opAs Mean "IMDB_Rating" "AverageRating" ] []
                . filter (FExpr "(datum.IMDB_Rating - datum.AverageRating) > 2.5")

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PAxis [ AxTitle "IMDB Rating" ] ]
                . position Y
                    [ PName "Title"
                    , PmType Nominal
                    , PAxis [ AxTitle "" ]
                    , PSort [ ByChannel ChX, Descending ]
                    ]
                    
    in toVegaLite [ dataVals [], trans [], enc [], mark Bar [] ]

joinAggregate3 :: VegaLite
joinAggregate3 =
    let dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json"

        trans =
            transform
                . filter (FExpr "datum.IMDB_Rating != null")
                . timeUnitAs (TU Year) "Release_Date" "year"
                . joinAggregate [ opAs Mean "IMDB_Rating" "AverageYearRating" ]
                    [ WGroupBy [ "year" ] ]
                . filter (FExpr "(datum.IMDB_Rating - datum.AverageYearRating) > 2.5")

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PAxis [ AxTitle "IMDB Rating" ] ]
                . position Y
                    [ PName "Title"
                    , PmType Nominal
                    , PAxis [ AxTitle "" ]
                    , PSort [ ByChannel ChX, Descending ]
                    ]
                    
    in toVegaLite [ dataVals [], trans [], enc [], mark Bar [] ]
