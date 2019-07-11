{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite HyperlinkTests.elm as of version 1.12.0
--
module HyperlinkTests (testSpecs) where

import Graphics.Vega.VegaLite hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("hyperlink1", hyperlink1)
            , ("hyperlink2", hyperlink2)
            , ("hyperlink3", hyperlink3)
            ]
            
hyperlink1 :: VegaLite
hyperlink1 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "label" (Strings [ "Vega", "Vega-Lite" ])
                . dataColumn "url" (Strings [ "https://vega.github.io/vega", "https://vega.github.io/vega-lite" ])

        encCircle =
            encoding
                . position X [ PName "label", PmType Nominal, PAxis [] ]
                . size [ MNumber 8000 ]
                . color [ MName "label", MmType Nominal, MLegend [] ]
                . hyperlink [ HName "url", HmType Nominal ]

        encLabel =
            encoding
                . position X [ PName "label", PmType Nominal, PAxis [] ]
                . text [ TName "label", TmType Nominal ]
                . color [ MString "white" ]
                . size [ MNumber 16 ]

        symbolSpec =
            asSpec [ mark Circle [ MCursor CPointer ], encCircle [] ]

        labelSpec =
            asSpec [ mark Text [], encLabel [] ]
    in
    toVegaLite
        [ dataVals [], layer [ symbolSpec, labelSpec ] ]

hyperlink2 :: VegaLite
hyperlink2 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative ]
                . position Y [ PName "Rotten_Tomatoes_Rating", PmType Quantitative ]
                . hyperlink [ HString "http://www.imdb.com" ]
    in
    toVegaLite [ dataVals, mark Point [ MCursor CPointer ], enc [] ]

hyperlink3 :: VegaLite
hyperlink3 =
    let
        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative ]
                . position Y [ PName "Rotten_Tomatoes_Rating", PmType Quantitative ]
                . color
                    [ MDataCondition
                        [ (Expr "datum.IMDB_Rating*10 > datum.Rotten_Tomatoes_Rating"
                          , [ MString "steelblue" ])
                        ]
                        [ MString "red" ]
                    ]
                . hyperlink
                    [ HDataCondition
                      [ (Expr "datum.IMDB_Rating*10 > datum.Rotten_Tomatoes_Rating"
                        , [ HString "http://www.imdb.com" ])
                      ]
                      [ HString "https://www.rottentomatoes.com" ]
                    ]
    in
    toVegaLite [ dataVals, mark Point [ MCursor CPointer ], enc [] ]
