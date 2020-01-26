{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite NullTests.elm as of version 1.12.0
--

module NullTests (testSpecs) where

import Data.Function ((&))
import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("axis1", axis1)
            , ("scale0", scale0)
            , ("scale1", scale1)
            , ("scale2", scale2)
            , ("scale3", scale3)
            , ("scale4", scale4)
            , ("scale5", scale5)
            , ("filter1", filter1)
            , ("filter2", filter2)
            ]


axis1 :: VegaLite
axis1 =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "x" (Numbers [ 0, 1000, 1000, 0, 0, 1000 ])
                . dataColumn "y" (Numbers [ 1000, 1000, 0, 0, 1000, 0 ])
                . dataColumn "order" (Numbers [1..6])

        enc =
            encoding
                . position X [ PName "x", PmType Quantitative, PAxis [] ]
                . position Y [ PName "y", PmType Quantitative, PAxis [] ]
                . order [ OName "order", OmType Ordinal ]
    in
    toVegaLite [ dataVals [], enc [], mark Line [] ]


scaleEncode :: (VLProperty, VLSpec) -> VegaLite
scaleEncode enc =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "x" (Numbers [ 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 ])
                . dataColumn "y" (Numbers [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ])
                . dataColumn "val" (Numbers [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ])
                . dataColumn "cat" (Strings [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j" ])
    in
    toVegaLite [ width 400, height 400, dataVals [], enc, mark Point [] ]


scale0 :: VegaLite
scale0 =
    (encoding
        . position X [ PName "x", PmType Quantitative ]
        . position Y [ PName "y", PmType Quantitative ]
        . color [ MName "val", MmType Ordinal ]
        . size [ MName "val", MmType Quantitative ]
        . shape [ MName "cat", MmType Nominal ]
    )
        []
        & scaleEncode


scale1 :: VegaLite
scale1 =
    (encoding
        . position X [ PName "x", PmType Quantitative, PScale [] ]
        . position Y [ PName "y", PmType Quantitative ]
        . color [ MName "val", MmType Ordinal ]
        . size [ MName "val", MmType Quantitative ]
        . shape [ MName "cat", MmType Nominal ]
    )
        []
        & scaleEncode


scale2 :: VegaLite
scale2 =
    (encoding
        . position X [ PName "x", PmType Quantitative ]
        . position Y [ PName "y", PmType Quantitative, PScale [] ]
        . color [ MName "val", MmType Ordinal ]
        . size [ MName "val", MmType Quantitative ]
        . shape [ MName "cat", MmType Nominal ]
    )
        []
        & scaleEncode


scale3 :: VegaLite
scale3 =
    (encoding
        . position X [ PName "x", PmType Quantitative ]
        . position Y [ PName "y", PmType Quantitative ]
        . color [ MName "val", MmType Ordinal, MScale [] ]
        . size [ MName "val", MmType Quantitative ]
        . shape [ MName "cat", MmType Nominal ]
    )
        []
        & scaleEncode


scale4 :: VegaLite
scale4 =
    (encoding
        . position X [ PName "x", PmType Quantitative ]
        . position Y [ PName "y", PmType Quantitative ]
        . color [ MName "val", MmType Ordinal ]
        . size [ MName "val", MmType Quantitative, MScale [] ]
        . shape [ MName "cat", MmType Nominal ]
    )
        []
        & scaleEncode


scale5 :: VegaLite
scale5 =
    (encoding
        . position X [ PName "x", PmType Quantitative ]
        . position Y [ PName "y", PmType Quantitative ]
        . color [ MName "val", MmType Ordinal ]
        . size [ MName "val", MmType Quantitative ]
        . shape [ MName "cat", MmType Nominal, MScale [] ]
    )
        []
        & scaleEncode


filter1 :: VegaLite
filter1 =
    let
        config =
            configure
                . configuration (MarkStyle [ MRemoveInvalid False ])

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative ]
                . position Y [ PName "Rotten_Tomatoes_Rating", PmType Quantitative ]
                . color
                    [ MDataCondition
                        [ (Expr "datum.IMDB_Rating === null || datum.Rotten_Tomatoes_Rating === null"
                          , [ MString "#ddd" ] )
                        ]
                        [ MString "rgb(76,120,168)" ]
                    ]
    in
    toVegaLite
        [ config []
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , mark Point[]
        , enc []
        ]

filter2 :: VegaLite
filter2 =
    let
        config =
            configure
                -- ensure have at least one test with remove invalid set
                . configuration (MarkStyle [ MRemoveInvalid True ])

        trans =
            transform
                . filter (FValid "IMDB_Rating")
                . filter (FValid "Rotten_Tomatoes_Rating")

        enc =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative ]
                . position Y [ PName "Rotten_Tomatoes_Rating", PmType Quantitative ]
                . color
                    [ MDataCondition
                        [ (Expr "datum.IMDB_Rating === null || datum.Rotten_Tomatoes_Rating === null"
                          , [ MString "#ddd" ])
                        ]
                        [ MString "rgb(76,120,168)" ]
                    ]
    in
    toVegaLite
        [ config []
        , trans []
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , mark Point[]
        , enc []
        ]
