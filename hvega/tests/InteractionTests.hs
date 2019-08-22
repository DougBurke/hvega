{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GeoTests.elm as of version 1.12.0
--
module InteractionTests (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter)


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("interaction1", interaction1)
            , ("interaction2", interaction2)
            , ("interaction3", interaction3)
            , ("interaction4", interaction4)
            , ("interaction5", interaction5)
            , ("interaction6", interaction6)
            , ("interaction7", interaction7)
            , ("interaction8", interaction8)
            ]


crimeData :: Data
crimeData = dataFromUrl "https://gicentre.github.io/data/westMidlands/westMidsCrimesAggregated.tsv" []


cScale :: [ScaleProperty]
cScale =
    categoricalDomainMap
        [ ( "Anti-social behaviour", "rgb(59,118,175)" )
        , ( "Burglary", "rgb(81,157,62)" )
        , ( "Criminal damage and arson", "rgb(141,106,184)" )
        , ( "Drugs", "rgb(239,133,55)" )
        , ( "Robbery", "rgb(132,88,78)" )
        , ( "Vehicle crime", "rgb(213,126,190)" )
        ]


month, crimes :: BuildLabelledSpecs
month = position X [ PName "month", PmType Temporal, PNoTitle ]
crimes = position Y [ PName "reportedCrimes", PmType Quantitative, PTitle "Reported crimes" ]

enc, encHighlight :: [LabelledSpec] -> PropertySpec
enc =
    encoding
        . month
        . crimes
        . color [ MName "crimeType", MmType Nominal, MScale cScale ]


encHighlight =
    encoding
        . month
        . crimes
        . color
            [ MSelectionCondition (SelectionName "mySelection")
                [ MName "crimeType", MmType Nominal, MScale cScale ]
                [ MString "black" ]
            ]
        . opacity
            [ MSelectionCondition (SelectionName "mySelection")
                [ MNumber 1 ]
                [ MNumber 0.1 ]
            ]

interaction1 :: VegaLite
interaction1 =
    let
        sel =
            selection . select "mySelection" Single []
    in
    toVegaLite [ width 540, crimeData, sel [], encHighlight [], mark Line [] ]


interaction2 :: VegaLite
interaction2 =
    let
        sel =
            selection . select "mySelection" Multi []
    in
    toVegaLite [ width 540, crimeData, sel [], encHighlight [], mark Line [] ]


interaction3 :: VegaLite
interaction3 =
    let
        sel =
            selection
                . select "mySelection" Single [ Nearest True, Fields [ "crimeType" ] ]
    in
    toVegaLite [ width 540, crimeData, sel [], encHighlight [], mark Circle [] ]


interaction4 :: VegaLite
interaction4 =
    let
        sel =
            selection
                . select "mySelection" Interval [ Empty, Encodings [ ChX ] ]
    in
    toVegaLite [ width 540, crimeData, sel [], encHighlight [], mark Circle [] ]


interaction5 :: VegaLite
interaction5 =
    let
        sel =
            selection
                . select "mySelection" Interval [ BindScales, Encodings [ ChX ] ]
    in
    toVegaLite [ width 540, crimeData, sel [], encHighlight [], mark Circle [] ]


interaction6 :: VegaLite
interaction6 =
    let
        sel =
            selection
                . select "mySelection" Interval [ BindScales ]
    in
    toVegaLite [ width 540, crimeData, sel [], encHighlight [], mark Circle [] ]


interaction7 :: VegaLite
interaction7 =
    let
        sel =
            selection
                . select "mySelection"
                    Single
                    [ Fields [ "crimeType" ]
                    , Nearest True
                    , Bind
                        [ IRadio "crimeType"
                            [ InName " "
                            , InOptions
                                [ "Anti-social behaviour"
                                , "Criminal damage and arson"
                                , "Drugs"
                                , "Robbery"
                                , "Vehicle crime"
                                ]
                            ]
                        ]
                    ]
    in
    toVegaLite [ width 540, crimeData, sel [], encHighlight [], mark Circle [] ]


interaction8 :: VegaLite
interaction8 =
    let
        sel =
            selection
                . select "maxSlider"
                    Single
                    [ SInit [ ( "maxReported", Number 14000 ) ]
                    , Bind [ IRange "maxReported" [ InName "Max", InMin 400, InMax 14000 ] ]
                    ]
                . select "minSlider"
                    Single
                    [ SInit [ ( "minReported", Number 0 ) ]
                    , Bind [ IRange "minReported" [ InName "Min", InMax 12800 ] ]
                    ]

        trans =
            transform
                . filter (FExpr "datum.reportedCrimes >= minSlider_minReported && maxSlider_maxReported >= datum.reportedCrimes")
    in
    toVegaLite [ width 540, crimeData, trans [], sel [], enc [], mark Circle [] ]
