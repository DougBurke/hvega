{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ConditionalTests.elm as of version 1.12.0
--
module ConditionalTests (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("markCondition1", markCondition1)
            , ("markCondition2", markCondition2)
            , ("selectionCondition1", selectionCondition1)
            , ("selectionCondition2", selectionCondition2)
            , ("selectionCondition3", selectionCondition3)
            , ("selectionCondition4", selectionCondition4)
            , ("selectionCondition5", selectionCondition5)
            , ("bindScales1", bindScales1)
            , ("bindScales2", bindScales2)
            ]
            
markCondition1 :: VegaLite
markCondition1 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []

        config =
            configure
                . configuration (RemoveInvalid False)

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative ]
                . position Y [ PName "Rotten_Tomatoes_Rating", PmType Quantitative ]
                . color
                    [ MDataCondition
                        [ ( Or (Expr "datum.IMDB_Rating === null")
                            (Expr "datum.Rotten_Tomatoes_Rating === null")
                          ,  [ MString "#ddd" ]
                          )
                        ]
                        [ MString "#0099ee" ]
                    ]
    in
    toVegaLite [ config [], dataVals, mark Point [], enc [] ]


markCondition2 :: VegaLite
markCondition2 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "value" (Numbers [ 10, 20, 30, 40, 50, 60 ])

        enc =
            encoding
                . position X [ PName "value", PmType Ordinal ]
                . color
                    [ MDataCondition
                        [ ( Expr "datum.value < 40", [ MString "blue" ] )
                        , ( Expr "datum.value < 50", [ MString "red" ] )
                        , ( Expr "datum.value < 60", [ MString "yellow" ] )
                        ]
                        [ MString "black" ]
                    ]
    in
    toVegaLite [ width 400, dataVals [], mark Circle [ MSize 800 ], enc [] ]

selectionCondition1 :: VegaLite
selectionCondition1 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        sel =
            selection
                . select "alex"
                    Interval
                    [ On "[mousedown[!event.shiftKey], mouseup] > mousemove"
                    , Translate "[mousedown[!event.shiftKey], mouseup] > mousemove"
                    ]
                . select "morgan"
                    Interval
                    [ On "[mousedown[event.shiftKey], mouseup] > mousemove"
                    , Translate "[mousedown[event.shiftKey], mouseup] > mousemove"
                    , SelectionMark [ SMFill "#fdbb84", SMFillOpacity 0.5, SMStroke "#e34a33" ]
                    ]

        enc =
            encoding
                . position Y [ PName "Origin", PmType Ordinal ]
                . position X [ PName "Cylinders", PmType Ordinal ]
                . color [ MAggregate Count, MName "*", MmType Quantitative ]
    in
    toVegaLite
        [ dataVals, sel [], mark Rect [ MCursor CGrab ], enc [] ]


selectionCondition2 :: VegaLite
selectionCondition2 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        sel =
            selection
                . select "alex"
                    Interval
                    [ On "[mousedown[!event.shiftKey], mouseup] > mousemove"
                    , Translate "[mousedown[!event.shiftKey], mouseup] > mousemove"
                    ]
                . select "morgan"
                    Interval
                    [ On "[mousedown[event.shiftKey], mouseup] > mousemove"
                    , Translate "[mousedown[event.shiftKey], mouseup] > mousemove"
                    , SelectionMark [ SMFill "#fdbb84", SMFillOpacity 0.5, SMStroke "#e34a33" ]
                    ]

        enc =
            encoding
                . position Y [ PName "Origin", PmType Ordinal ]
                . position X [ PName "Cylinders", PmType Ordinal ]
                . color
                    [ MSelectionCondition
                        (And (SelectionName "alex") (SelectionName "morgan"))
                        [ MAggregate Count, MName "*", MmType Quantitative ]
                        [ MString "gray" ]
                    ]
    in
    toVegaLite
        [ dataVals, sel [], mark Rect [ MCursor CGrab ], enc [] ]


selectionCondition3 :: VegaLite
selectionCondition3 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        trans =
            transform
                . filter (FCompose (And (Selection "brush") (Expr "datum.Weight_in_lbs > 3000")))

        sel =
            selection
                . select "brush" Interval []

        enc1 =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]

        spec1 =
            asSpec [ sel [], mark Point [], enc1 [] ]

        enc2 =
            encoding
                . position X [ PName "Acceleration", PmType Quantitative, PScale [ SDomain (DNumbers [ 0, 25 ]) ] ]
                . position Y [ PName "Displacement", PmType Quantitative, PScale [ SDomain (DNumbers [ 0, 500 ]) ] ]

        spec2 =
            asSpec [ trans [], mark Point [], enc2 [] ]
    in
    toVegaLite
        [ dataVals, vConcat [ spec1, spec2 ] ]


{- TODO: add Clear -}

selectionCondition4 :: VegaLite
selectionCondition4 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        sel =
            selection
                . select "mySelection"
                    Interval
                    [ {- Clear ""
                    , -} On "[mousedown[!event.shiftKey], mouseup] > mousemove"
                    , Translate "[mousedown[!event.shiftKey], mouseup] > mousemove"
                    ]

        enc =
            encoding
                . position Y [ PName "Origin", PmType Ordinal ]
                . position X [ PName "Cylinders", PmType Ordinal ]
                . color
                    [ MSelectionCondition
                        (SelectionName "mySelection")
                        [ MAggregate Count, MName "*", MmType Quantitative ]
                        [ MString "gray" ]
                    ]
    in
    toVegaLite
        [ dataVals, sel [], mark Rect [ MCursor CGrab ], enc [] ]


{- TODO: add Clear -}

selectionCondition5 :: VegaLite
selectionCondition5 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        sel =
            selection
                . select "mySelection"
                    Interval
                    [ {- Clear "mouseup"
                    , -} Empty
                    , On "[mousedown[!event.shiftKey], mouseup] > mousemove"
                    , Translate "[mousedown[!event.shiftKey], mouseup] > mousemove"
                    ]

        enc =
            encoding
                . position Y [ PName "Origin", PmType Ordinal ]
                . position X [ PName "Cylinders", PmType Ordinal ]
                . color
                    [ MSelectionCondition
                        (SelectionName "mySelection")
                        [ MAggregate Count, MName "*", MmType Quantitative ]
                        [ MString "gray" ]
                    ]
    in
    toVegaLite
        [ dataVals, sel [], mark Rect [ MCursor CGrab ], enc [] ]


bindScales1 :: VegaLite
bindScales1 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json"

        sel =
            selection
                . select "myZoomPan" Interval [ BindScales ]

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
    in
    toVegaLite
        [ width 300, height 300, dataVals [], sel [], mark Circle [], enc [] ]


{- TODO: add Clear -}

bindScales2 :: VegaLite
bindScales2 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json"

        sel =
            selection
                . select "myZoomPan"
                    Interval
                    [ BindScales {- , Clear "click[event.shiftKey]" -} ]

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
    in
    toVegaLite
        [ width 300, height 300, dataVals [], sel [], mark Circle [], enc [] ]
