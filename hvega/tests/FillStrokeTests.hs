{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Based on the Elm VegaLite FillStrokeTests.elm as of version 1.12.0
--
-- Note that fill1, stroke1, and combined1 generate invalid Vega-Lite
-- since they create empty objects for the fill or stroke fields,
-- and that is not valid. I am leaving this as is for now.
--
module FillStrokeTests (testSpecs) where

import qualified Data.Text as T

import Data.Aeson (Value)
import Data.Aeson.QQ.Simple (aesonQQ)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

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
            , ("vrounded", vrounded)
            , ("hrounded", hrounded)
            , ("strokedash1", strokeDash1)
            , ("strokedash2", strokeDash2)
            , ("strokedash3", strokeDash3)
            , ("strokedash4", strokeDash4)
            , ("strokedash5", strokeDash5)
            , ("strokedash6", strokeDash6)
            , ("strokedash7", strokeDash7)
            , ("strokedash8", strokeDash8)
            , ("strokedash9", strokeDash9)
            , ("fillopacity", fillopacity)
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


{-
From https://vega.github.io/vega-lite/docs/bar.html#bar-chart-with-rounded-corners

{
  "data": {
    "values": [
      {"a": "A", "b": 28}, {"a": "B", "b": 55}, {"a": "C", "b": 43},
      {"a": "D", "b": 91}, {"a": "E", "b": 81}, {"a": "F", "b": 53},
      {"a": "G", "b": 19}, {"a": "H", "b": 87}, {"a": "I", "b": 52}
    ]
  },
  "mark": {"type": "bar", "cornerRadiusEnd": 4},
  "encoding": {
    "x": {"field": "a", "type": "ordinal"},
    "y": {"field": "b", "type": "quantitative"}
  }
}

-}

barData :: Value
barData = [aesonQQ|
[
  {"a": "A", "b": 28}, {"a": "B", "b": 55}, {"a": "C", "b": 43},
  {"a": "D", "b": 91}, {"a": "E", "b": 81}, {"a": "F", "b": 53},
  {"a": "G", "b": 19}, {"a": "H", "b": 87}, {"a": "I", "b": 52}
]
|]

rounded :: Position -> Position -> VegaLite
rounded h v =
  toVegaLite [ dataFromJson barData []
             , mark Bar [ MCornerRadiusEnd 4 ]
             , encoding
               . position h [ PName "a", PmType Ordinal ]
               . position v [ PName "b", PmType Quantitative ]
               $ []
             ]

vrounded :: VegaLite
vrounded = rounded X Y

hrounded :: VegaLite
hrounded = rounded Y X


stockData :: Data
stockData = dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []

xDate, yPrice :: BuildEncodingSpecs
xDate = position X [ PName "date", PmType Temporal ]
yPrice = position Y [ PName "price", PmType Quantitative ]

strokeDashTest :: [MarkChannel] -> VegaLite
strokeDashTest sOpts =
  let enc = encoding
            . xDate
            . yPrice
            . strokeDash ([ MName "symbol", MmType Nominal ] ++ sOpts)

      lineOpts = [MStrokeWidth 1, MOpacity 0.6]

  in toVegaLite [ width 350, stockData, enc [], mark Line lineOpts ]


strokeDash1 :: VegaLite
strokeDash1 = strokeDashTest []

strokeDash2 :: VegaLite
strokeDash2 = strokeDashTest [ MScale
                           [ SDomain (DStrings [ "AAPL", "AMZN", "GOOG", "IBM", "MSFT" ])
                           , SRange (RNumberLists [ [ 1, 0 ], [ 3, 1 ], [ 2, 3 ], [ 4, 4 ], [ 5, 6 ] ])
                           ]
                         ]

pName :: T.Text -> PositionChannel
pName = PName

mName :: T.Text -> MarkChannel
mName = MName

pQuant :: PositionChannel
pQuant = PmType Quantitative

mNominal, mOrdinal :: MarkChannel
mNominal = MmType Nominal
mOrdinal = MmType Ordinal


sDash :: [MarkChannel] -> VegaLite
sDash sdOpts =
  let dvals = dataFromColumns []
              . toCol "x" [ 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1 ]
              . toCol "y" [ 100, 100, 90, 90, 80, 80, 70, 70, 60, 60, 50, 50, 40, 40, 30, 30, 20, 20, 10, 10 ]
              . toCol "cat" [ 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10 ]

      toCol l = dataColumn l . Numbers

      encBase = encoding
                . position X [ pName "x", pQuant ]
                . position Y [ pName "y", pQuant ]

      enc1 = encBase
             . strokeDash ([ mName "cat", mNominal ] ++ sdOpts)

      spec1 = asSpec [ title "Nominal" [], width 200, enc1 [], mark Line [] ]

      enc2 = encBase
             . strokeDash ([ mName "cat", mOrdinal ] ++ sdOpts)

      spec2 = asSpec [ title "Ordinal" [], width 200, enc2 [], mark Line [] ]

      res = resolve
            . resolution (RScale [ ( ChStrokeDash, Independent ) ])

      cfg = configure
            . configuration (Axis [NoTitle, Grid False])
            . configuration (LineStyle [MStrokeWidth 1, MColor "orange", MOpacity 0.6])

  in toVegaLite [ dvals [], cfg [], res [], vlConcat [ spec1, spec2 ] ]


d0, d1, d2, d3, d4, d5, d6, d7, d8, d9 :: [Double]
d0 = [ 1, 0 ]
d1 = [ 16, 4 ]
d2 = [ 10, 4 ]
d3 = [ 8, 4 ]
d4 = [ 8, 4, 4, 4 ]
d5 = [ 6, 4 ]
d6 = [ 5, 4 ]
d7 = [ 4, 6 ]
d8 = [ 2, 4 ]
d9 = [ 1, 3 ]

strokeDash3, strokeDash4 :: VegaLite
strokeDash3 = sDash []
strokeDash4 = sDash [ MScale
                      [ SDomain (DNumbers [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
                      , SRange (RNumberLists [d0, d6, d8, d4, d9, d1, d5, d3, d7, d2])
                      ]
                    ]


scaledStrokeDash :: Double -> VegaLite
scaledStrokeDash dashScale =
  let scaleDash = map (map (dashScale *))

      dvals = dataSequenceAs 0 100 0.1 "x0"

      trans = transform
              . calculateAs "abs(sin(datum.x0+random()))" "y0"
              . calculateAs "datum.x0 %10" "x"
              . calculateAs "floor(datum.x0 / 10)" "cat"
              . calculateAs "datum.y0 + datum.cat" "y"

      enc = encoding
            . position X [ pName "x", pQuant, PAxis [ AxGrid False ] ]
            . position Y [ pName "y", pQuant, PAxis [ AxGrid False ] ]
            . strokeDash
                    [ mName "cat"
                    , mOrdinal
                    , MScale
                        [ SDomain (DNumbers [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
                        , SRange (RNumberLists (scaleDash [ d0, d1, d2, d3, d4, d5, d6, d7, d8, d9 ]))
                        ]
                    ]

  in toVegaLite [ title ("Dash scale " <> T.pack (show dashScale)) []
                , width 300
                , height 300
                , dvals
                , trans []
                , enc []
                , mark Line []
                ]


strokeDash5, strokeDash6, strokeDash7, strokeDash8, strokeDash9 :: VegaLite
strokeDash5 = scaledStrokeDash 0.2
strokeDash6 = scaledStrokeDash 0.5
strokeDash7 = scaledStrokeDash 1
strokeDash8 = scaledStrokeDash 2
strokeDash9 = scaledStrokeDash 4


-- found in a bug report and thought would be a good test
fillopacity :: VegaLite
fillopacity =
  let dvals = dataFromColumns []
              . dataColumn "a" (Strings ["A", "B"])
              . dataColumn "b" (Numbers [10, 20])
              $ []

      enc = encoding
            . position X [PName "a", PmType Ordinal, PAxis [AxLabelAngle 0]]
            . position Y [PName "b", PmType Quantitative]
            . fillOpacity [MName "b", MmType Quantitative]

  in toVegaLite [ dvals
                , mark Bar []
                , enc []
                ]
