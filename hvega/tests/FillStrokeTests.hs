{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite FillStrokeTests.elm as of version 1.12.0
--
-- Note that fill1, stroke1, and combined1 generate invalid Vega-Lite
-- since they create empty objects for the fill or stroke fields,
-- and that is not valid. I am leaving this as is for now.
--
module FillStrokeTests (testSpecs) where

-- import qualified Data.Text as T

import Graphics.Vega.VegaLite hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("default", defChart)
            , ("fill1", fill1)
            , ("fill2", fill2)
            , ("fill3", fill3)
            , ("stroke1", stroke1)
            , ("stroke2", stroke2)
            , ("stroke3", stroke3)
            , ("combined1", combined1)
            , ("combined2", combined2)
            , ("combined3", combined3)
            , ("geo1", geo1)
            , ("geo2", geo2)
            ]

encChart :: ([a] -> [LabelledSpec]) -> VegaLite
encChart extraEnc =
    let
        dataVals =
            dataFromColumns []
                . dataColumn "x" (Numbers [ 10, 20, 30, 36 ])
                . dataColumn "y" (Numbers [ 1, 2, 3, 4 ])
                . dataColumn "val" (Numbers [ 1, 2, 3, 4 ])
                . dataColumn "cat" (Strings [ "a", "b", "c", "d" ])

        enc =
            encoding
                . position X [ PName "x", PmType Quantitative ]
                . position Y [ PName "y", PmType Quantitative ]
                . color [ MName "cat", MmType Nominal ]
                . size [ MNumber 2000 ]
                . extraEnc
    in
    toVegaLite [ width 200, height 200, dataVals [], enc [],
                 mark Circle [ MStroke "black" ] ]


defChart :: VegaLite
defChart = encChart (const [])

fill1, fill2, fill3 :: VegaLite
fill1 = encChart (fill [])
fill2 = encChart (fill [ MName "y", MmType Ordinal ])
fill3 = encChart (fill [ MString "red" ])

stroke1, stroke2, stroke3 :: VegaLite
stroke1 = encChart (stroke [])
stroke2 = encChart (stroke [ MName "y", MmType Ordinal ])
stroke3 = encChart (stroke [ MString "red" ])

combined1, combined2, combined3 :: VegaLite
combined1 = encChart (stroke [] . fill [])
combined2 = encChart (stroke [ MName "y", MmType Ordinal ] . fill [ MString "red" ])
combined3 = encChart (stroke [ MString "red" ] . fill [ MName "y", MmType Ordinal ])

geo1 :: VegaLite
geo1 =
    let
        geojson =
            geoFeatureCollection
                [ geometry (GeoPolygon [ [ ( -2, 58 ), ( 3, 58 ), ( 3, 53 ), ( -2, 53 ), ( -2, 58 ) ] ]) []
                , geometry (GeoLine [ ( 4, 52 ), ( 4, 59 ), ( -3, 59 ) ]) []
                ]
    in
    toVegaLite
        [ width 300
        , height 300
        , dataFromJson geojson []
        , mark Geoshape []
        ]

geo2 :: VegaLite
geo2 =
    let
        geojson =
            geoFeatureCollection
                [ geometry (GeoPolygon [ [ ( -2, 58 ), ( 3, 58 ), ( 3, 53 ), ( -2, 53 ), ( -2, 58 ) ] ]) []
                , geometry (GeoLine [ ( 4, 52 ), ( 4, 59 ), ( -3, 59 ) ]) []
                ]

        -- NOTE: There is a bug in Vega-Lite that prevents nested geometry from being read correctly.
        enc =
            encoding . color [ MName "features.geometry.type",
                               MmType Nominal ]
    in
    toVegaLite
        [ width 300
        , height 300
        , enc []
        , dataFromJson geojson []
        , mark Geoshape []
        ]
