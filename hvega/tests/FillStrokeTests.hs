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
            , ("gradient1", gradient1)
            , ("gradient2", gradient2)
            , ("gradient3", gradient3)
            , ("gradientr1", gradientr1)
            , ("gradientr2", gradientr2)
            , ("gradientr3", gradientr3)
            ]

encChart :: ([a] -> [EncodingSpec]) -> VegaLite
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
fill1 = encChart (fill [])  -- this produces invalid output
fill2 = encChart (fill [ MName "y", MmType Ordinal ])
fill3 = encChart (fill [ MString "red" ])

stroke1, stroke2, stroke3 :: VegaLite
stroke1 = encChart (stroke []) -- this produces invalid output
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


gradientTest :: PropertySpec -> VegaLite
gradientTest markType =
  let dvals = dataFromColumns []
              . dataColumn "cat" (Strings [ "a", "b", "c", "d" ])
              . dataColumn "value" (Numbers [ 10, 5, 20, 8 ])

      enc = encoding
            . position X [ PName "cat", PmType Nominal ]
            . position Y [ PName "value", PmType Quantitative ]

  in toVegaLite [ width 200, dvals [], enc [], markType ]


gradient1 :: VegaLite
gradient1 =
  let markType = mark Bar [ MColorGradient GrLinear stops opts ]
      stops = [ (0, "red"), (0.4, "orange"), (1, "blue") ]
      opts = [ GrX1 1
             , GrX2 1
             , GrY1 0
             , GrY2 1
             ]

  in gradientTest markType


gradient2 :: VegaLite
gradient2 =
  let markType = mark Bar [ MColorGradient GrLinear stops opts ]
      stops = [ (0, "red"), (1, "blue") ]
      opts = [ GrX1 0
             , GrX2 1
             , GrY1 1
             , GrY2 1
             ]

  in gradientTest markType


gradient3 :: VegaLite
gradient3 =
  let markType = mark Bar [ MColorGradient GrLinear stops opts ]
      stops = [ (1, "red"), (0, "blue") ]
      opts = [ GrX1 0
             , GrX2 1
             , GrY1 0
             , GrY2 1
             ]

  in gradientTest markType


gradientr1 :: VegaLite
gradientr1 =
  let markType = mark Circle [ MSize 1000
                             , MColorGradient GrRadial stops opts ]
      stops = [ (0, "red"), (0.5, "white"), (1, "blue") ]
      opts = [ ]

  in gradientTest markType


-- these are the defaults, so they should be the same as r1
gradientr2 :: VegaLite
gradientr2 =
  let markType = mark Circle [ MSize 1000
                             , MColorGradient GrRadial stops opts ]
      stops = [ (0, "red"), (1, "blue"), (0.5, "white") ]
      opts = [ GrX1 0.5
             , GrX2 0.5
             , GrY1 0.5
             , GrY2 0.5
             ]

  in gradientTest markType


-- not sure if these options make sense, but they are valid according to
-- the Vega-Lite 4.0.2 spec so leave them be.
--
gradientr3 :: VegaLite
gradientr3 =
  let markType = mark Circle [ MSize 1000
                             , MColorGradient GrRadial stops opts ]
      stops = [ (0, "red"), (1, "blue"), (0.5, "white") ]
      opts = [ GrX1 0.2
             , GrX2 0.8
             , GrY1 0.7
             , GrY2 0.3
             , GrR1 0.2
             , GrR2 0.4
             ]

  in gradientTest markType
