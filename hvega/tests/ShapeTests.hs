{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ShapeTests.elm as of version 1.12.0
--

module ShapeTests (testSpecs) where

import qualified Data.Map as M
import qualified Data.Text as T

import Data.Maybe (fromMaybe)
import Graphics.Vega.VegaLite

import Prelude hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("defNominal", scatter1)
            , ("defOrdinal", scatter2)
            , ("size1", scatter3)
            , ("size2", scatter4)
            , ("size3", scatter5)
            , ("multi1", scatter6)
            , ("multi2", scatter7)
            , ("multi3", scatter8)
            , ("multi4", scatter9)
            , ("multi5", scatter10)
            , ("multi6", scatter11)
            , ("custom1", scatter12)
            , ("custom2", scatter13)
            , ("custom3", scatter14)
            , ("custom4", scatter15)
            , ("isotype1", personGrid)
            , ("point1", point1)
            , ("point2", point2)
            , ("point3", point3)
            , ("point4", point4)
            , ("point5", point5)
            , ("point6", point6)
            , ("point7", point7)
            , ("rounded1", rounded1)
            , ("rounded2", rounded2)
            , ("rounded3", rounded3)
            , ("rounded4", rounded4)
            , ("rounded5", rounded5)
            , ("rounded6", rounded6)
            , ("symbols1", symbols1)
            , ("symbols2", symbols2)
            , ("windvector", windvector)
            ]


chart :: T.Text -> ([a] -> [EncodingSpec]) -> VegaLite
chart des enc =
    toVegaLite
        [ description des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , (transform
            . calculateAs "year(datum.Year)" "YearOfManufacture"
            . filter (FExpr "datum.YearOfManufacture == 1970")
          )
            []
        , mark Point [ MFilled True ]
        , (encoding
            . position X [ PName "Horsepower", PmType Quantitative ]
            . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
            . opacity [ MNumber 0.6 ]
            . enc
          )
            []
        ]


pointChart :: T.Text -> Bool -> Symbol -> VegaLite
pointChart des filled sym =
    toVegaLite
        [ description des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , (transform
            . calculateAs "year(datum.Year)" "YearOfManufacture"
            . filter (FExpr "datum.YearOfManufacture == 1970")
          )
            []
        , mark Point [ MFilled filled, MShape sym ]
        , (encoding
            . position X [ PName "Horsepower", PmType Quantitative ]
            . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
            . opacity [ MNumber 0.6 ]
          )
            []
        ]


unitSquare, largeSquare, square, tri, cross :: T.Text
unitSquare = "M -0.5 -0.5 h 1 v 1 h -1z"
largeSquare = "M -5 -5 h 10 v 10 h -10z"
square = "M -1.5 -1.5 h 3 v 3 h -3z"
tri = "M -1.5 -1.5 h 3 l -1.5 3z"
cross = "M -1.5 -1.5 m1 0 h 1 v 1 h 1 v 1 h -1 v 1 h -1  v -1 h -1 v -1 h 1z"


scatter1 :: VegaLite
scatter1 =
    chart "Default nominal shapes."
        (shape [ MName "Origin", MmType Nominal ])


scatter2 :: VegaLite
scatter2 =
    chart "Default ordinal shapes."
        (shape [ MName "Cylinders", MmType Ordinal ])


scatter3 :: VegaLite
scatter3 =
    chart "Enlarged shapes (but legend shapes should remain same size)"
        (shape [ MName "Origin", MmType Nominal ]
            . size [ MNumber 200 ]
        )


scatter4 :: VegaLite
scatter4 =
    chart "Reduced shapes (but legend shapes should remain same size)"
        (shape [ MName "Origin", MmType Nominal ]
            . size [ MNumber 20 ]
        )


scatter5 :: VegaLite
scatter5 =
    chart "Fixed shape, sized by number of cylinder category"
        (size [ MName "Cylinders", MmType Ordinal ])


scatter6 :: VegaLite
scatter6 =
    chart "Sized by number of cylinders, shape by origin"
        (shape [ MName "Origin", MmType Nominal ]
            . size [ MName "Cylinders", MmType Ordinal ]
        )


scatter7 :: VegaLite
scatter7 =
    chart "Sized and shaped by number of cylinders (should only have a single set of legend items)"
        (shape [ MName "Cylinders", MmType Ordinal ]
            . size [ MName "Cylinders", MmType Ordinal ]
        )


scatter8 :: VegaLite
scatter8 =
    chart "Sized, shaped and coloured by number of cylinders (should only have a single set of legend items)"
        (shape [ MName "Cylinders", MmType Ordinal ]
            . size [ MName "Cylinders", MmType Ordinal ]
            . color [ MName "Cylinders", MmType Ordinal ]
        )


scatter9 :: VegaLite
scatter9 =
    chart "Custom-shaped and coloured by origin (should only have a single set of legend items)"
        (shape [ MName "Origin", MmType Nominal ]
            . color [ MName "Origin", MmType Nominal ]
        )


scatter10 :: VegaLite
scatter10 =
    chart "Custom-shaped and coloured by origin (should only have a single set of legend items)"
        (shape
            [ MName "Origin"
            , MmType Nominal
            , MScale (categoricalDomainMap
                      [ ( "Europe", square )
                      , ( "Japan", cross )
                      , ( "USA", tri )
                      ])
            ]
            . color [ MName "Origin", MmType Nominal ]
        )


scatter11 :: VegaLite
scatter11 =
    chart "Sized, shaped and coloured by number of cylinders (should have two sets of legend items)"
        (shape [ MName "Cylinders", MmType Ordinal ]
            . size [ MName "Cylinders", MmType Ordinal ]
            . color [ MName "Origin", MmType Nominal ]
        )


scatter12 :: VegaLite
scatter12 =
    chart "Custom nominal shape with unit area."
        (shape [ MPath unitSquare ])


scatter13 :: VegaLite
scatter13 =
    chart "Custom nominal shape with unit area sized by Cylinders."
        (shape [ MPath unitSquare ]
            . size [ MName "Cylinders", MmType Ordinal ]
        )


scatter14 :: VegaLite
scatter14 =
    chart "Custom nominal shape with area of 10x10 pixel units."
        (shape [ MPath largeSquare ]
            . color [ MName "Origin", MmType Nominal ]
        )


scatter15 :: VegaLite
scatter15 =
    chart "Custom shape sets encoding origin."
        (shape
            [ MName "Origin"
            , MmType Nominal
            , MScale (categoricalDomainMap
                      [ ( "Europe", square )
                      , ( "Japan", cross )
                      , ( "USA", tri )
                      ])
            ]
        )


isotypes :: M.Map T.Text T.Text
isotypes =
    let
        cow =
            "M4 -2c0 0 0.9 -0.7 1.1 -0.8c0.1 -0.1 -0.1 0.5 -0.3 0.7c-0.2 0.2 1.1 1.1 1.1 1.2c0 0.2 -0.2 0.8 -0.4 0.7c-0.1 0 -0.8 -0.3 -1.3 -0.2c-0.5 0.1 -1.3 1.6 -1.5 2c-0.3 0.4 -0.6 0.4 -0.6 0.4c0 0.1 0.3 1.7 0.4 1.8c0.1 0.1 -0.4 0.1 -0.5 0c0 0 -0.6 -1.9 -0.6 -1.9c-0.1 0 -0.3 -0.1 -0.3 -0.1c0 0.1 -0.5 1.4 -0.4 1.6c0.1 0.2 0.1 0.3 0.1 0.3c0 0 -0.4 0 -0.4 0c0 0 -0.2 -0.1 -0.1 -0.3c0 -0.2 0.3 -1.7 0.3 -1.7c0 0 -2.8 -0.9 -2.9 -0.8c-0.2 0.1 -0.4 0.6 -0.4 1c0 0.4 0.5 1.9 0.5 1.9l-0.5 0l-0.6 -2l0 -0.6c0 0 -1 0.8 -1 1c0 0.2 -0.2 1.3 -0.2 1.3c0 0 0.3 0.3 0.2 0.3c0 0 -0.5 0 -0.5 0c0 0 -0.2 -0.2 -0.1 -0.4c0 -0.1 0.2 -1.6 0.2 -1.6c0 0 0.5 -0.4 0.5 -0.5c0 -0.1 0 -2.7 -0.2 -2.7c-0.1 0 -0.4 2 -0.4 2c0 0 0 0.2 -0.2 0.5c-0.1 0.4 -0.2 1.1 -0.2 1.1c0 0 -0.2 -0.1 -0.2 -0.2c0 -0.1 -0.1 -0.7 0 -0.7c0.1 -0.1 0.3 -0.8 0.4 -1.4c0 -0.6 0.2 -1.3 0.4 -1.5c0.1 -0.2 0.6 -0.4 0.6 -0.4z"

        pig =
            "M1.2 -2c0 0 0.7 0 1.2 0.5c0.5 0.5 0.4 0.6 0.5 0.6c0.1 0 0.7 0 0.8 0.1c0.1 0 0.2 0.2 0.2 0.2c0 0 -0.6 0.2 -0.6 0.3c0 0.1 0.4 0.9 0.6 0.9c0.1 0 0.6 0 0.6 0.1c0 0.1 0 0.7 -0.1 0.7c-0.1 0 -1.2 0.4 -1.5 0.5c-0.3 0.1 -1.1 0.5 -1.1 0.7c-0.1 0.2 0.4 1.2 0.4 1.2l-0.4 0c0 0 -0.4 -0.8 -0.4 -0.9c0 -0.1 -0.1 -0.3 -0.1 -0.3l-0.2 0l-0.5 1.3l-0.4 0c0 0 -0.1 -0.4 0 -0.6c0.1 -0.1 0.3 -0.6 0.3 -0.7c0 0 -0.8 0 -1.5 -0.1c-0.7 -0.1 -1.2 -0.3 -1.2 -0.2c0 0.1 -0.4 0.6 -0.5 0.6c0 0 0.3 0.9 0.3 0.9l-0.4 0c0 0 -0.4 -0.5 -0.4 -0.6c0 -0.1 -0.2 -0.6 -0.2 -0.5c0 0 -0.4 0.4 -0.6 0.4c-0.2 0.1 -0.4 0.1 -0.4 0.1c0 0 -0.1 0.6 -0.1 0.6l-0.5 0l0 -1c0 0 0.5 -0.4 0.5 -0.5c0 -0.1 -0.7 -1.2 -0.6 -1.4c0.1 -0.1 0.1 -1.1 0.1 -1.1c0 0 -0.2 0.1 -0.2 0.1c0 0 0 0.9 0 1c0 0.1 -0.2 0.3 -0.3 0.3c-0.1 0 0 -0.5 0 -0.9c0 -0.4 0 -0.4 0.2 -0.6c0.2 -0.2 0.6 -0.3 0.8 -0.8c0.3 -0.5 1 -0.6 1 -0.6z"

        sheep =
            "M-4.1 -0.5c0.2 0 0.2 0.2 0.5 0.2c0.3 0 0.3 -0.2 0.5 -0.2c0.2 0 0.2 0.2 0.4 0.2c0.2 0 0.2 -0.2 0.5 -0.2c0.2 0 0.2 0.2 0.4 0.2c0.2 0 0.2 -0.2 0.4 -0.2c0.1 0 0.2 0.2 0.4 0.1c0.2 0 0.2 -0.2 0.4 -0.3c0.1 0 0.1 -0.1 0.4 0c0.3 0 0.3 -0.4 0.6 -0.4c0.3 0 0.6 -0.3 0.7 -0.2c0.1 0.1 1.4 1 1.3 1.4c-0.1 0.4 -0.3 0.3 -0.4 0.3c-0.1 0 -0.5 -0.4 -0.7 -0.2c-0.3 0.2 -0.1 0.4 -0.2 0.6c-0.1 0.1 -0.2 0.2 -0.3 0.4c0 0.2 0.1 0.3 0 0.5c-0.1 0.2 -0.3 0.2 -0.3 0.5c0 0.3 -0.2 0.3 -0.3 0.6c-0.1 0.2 0 0.3 -0.1 0.5c-0.1 0.2 -0.1 0.2 -0.2 0.3c-0.1 0.1 0.3 1.1 0.3 1.1l-0.3 0c0 0 -0.3 -0.9 -0.3 -1c0 -0.1 -0.1 -0.2 -0.3 -0.2c-0.2 0 -0.3 0.1 -0.4 0.4c0 0.3 -0.2 0.8 -0.2 0.8l-0.3 0l0.3 -1c0 0 0.1 -0.6 -0.2 -0.5c-0.3 0.1 -0.2 -0.1 -0.4 -0.1c-0.2 -0.1 -0.3 0.1 -0.4 0c-0.2 -0.1 -0.3 0.1 -0.5 0c-0.2 -0.1 -0.1 0 -0.3 0.3c-0.2 0.3 -0.4 0.3 -0.4 0.3l0.2 1.1l-0.3 0l-0.2 -1.1c0 0 -0.4 -0.6 -0.5 -0.4c-0.1 0.3 -0.1 0.4 -0.3 0.4c-0.1 -0.1 -0.2 1.1 -0.2 1.1l-0.3 0l0.2 -1.1c0 0 -0.3 -0.1 -0.3 -0.5c0 -0.3 0.1 -0.5 0.1 -0.7c0.1 -0.2 -0.1 -1 -0.2 -1.1c-0.1 -0.2 -0.2 -0.8 -0.2 -0.8c0 0 -0.1 -0.5 0.4 -0.8z"

        person =
            "M1.7 -1.7h-0.8c0.3 -0.2 0.6 -0.5 0.6 -0.9c0 -0.6 -0.4 -1 -1 -1c-0.6 0 -1 0.4 -1 1c0 0.4 0.2 0.7 0.6 0.9h-0.8c-0.4 0 -0.7 0.3 -0.7 0.6v1.9c0 0.3 0.3 0.6 0.6 0.6h0.2c0 0 0 0.1 0 0.1v1.9c0 0.3 0.2 0.6 0.3 0.6h1.3c0.2 0 0.3 -0.3 0.3 -0.6v-1.8c0 0 0 -0.1 0 -0.1h0.2c0.3 0 0.6 -0.3 0.6 -0.6v-2c0.2 -0.3 -0.1 -0.6 -0.4 -0.6z"
    in
    M.fromList [ ( "cow", cow ), ( "pig", pig ), ( "sheep", sheep ), ( "person", person ) ]



{- Grid of selectable person icons.
   Inspired by Alan Smith's D3 example http://bl.ocks.org/alansmithy/d832fc03f6e6a91e99f4
   and based around Amit Kapoor's unit chart example https://bl.ocks.org/amitkaps/d6648bd8ddb1c1e3706d7530126d1e2b
-}


personGrid :: VegaLite
personGrid =
    let
        config =
            configure
                . configuration (ViewStyle [ViewNoStroke])
                . configuration (Axis [Disable True])

        dataVals =
            dataFromColumns []
                . dataColumn "id" (Numbers [1 .. 100])

        trans =
            transform
                . calculateAs "ceil (datum.id/10)" "col"
                . calculateAs "datum.id - datum.col*10" "row"

        sel =
            selection
                . select "highlight" Interval []

        mpath = M.lookup "person" isotypes
        enc =
            encoding
                . position X [ PName "col", PmType Ordinal]
                . position Y [ PName "row", PmType Ordinal]
                . shape [ MPath (fromMaybe "circle" mpath) ]
                . color
                    [ MSelectionCondition (SelectionName "highlight")
                        [ MString "rgb(194,81,64)" ]
                        [ MString "rgb(167,165,156)" ]
                    ]
                . size [ MNumber 90 ]
    in
    toVegaLite
        [ config []
        , width 400
        , height 400
        , dataVals []
        , trans []
        , mark Point [ MFilled True ]
        , enc []
        , sel []
        ]


point1 :: VegaLite
point1 = pointChart "square-unfilled" False SymSquare


point2 :: VegaLite
point2 = pointChart "cross-filled" True SymCross


point3 :: VegaLite
point3 = pointChart "diamond-unfilled" False SymDiamond


-- not going to try all the triangles
point4 :: VegaLite
point4 = pointChart "triangle-unfilled" False SymTriangle


point5 :: VegaLite
point5 = pointChart "stroke-unfilled" False SymStroke


point6 :: VegaLite
point6 = pointChart "arrow-filled" True SymArrow


point7 :: VegaLite
point7 = pointChart "wedge-unfilled" False SymWedge


rectTest :: [MarkProperty] -> VegaLite
rectTest mps =
  let dvals = dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
      enc = encoding
            . position X [ PName "Origin", PmType Nominal ]
            . position Y [ PName "Cylinders", PmType Ordinal ]

      -- using MSize here returns the warning
      -- 'Cannot apply size to non-oriented mark "rect".'
      -- from Vega Embed. If we take it out the visualization doesn't
      -- "look as good".
      --
      rect = mark Rect (MSize 30 : MOpacity 0.6 : mps)

  in toVegaLite [ width 200, height 200, dvals, enc [], rect ]


rounded1, rounded2, rounded3, rounded4, rounded5, rounded6 :: VegaLite
rounded1 = rectTest [ MCornerRadius 8 ]
rounded2 = rectTest [ MCornerRadiusTL 8 ]
rounded3 = rectTest [ MCornerRadiusTR 8 ]
rounded4 = rectTest [ MCornerRadiusBL 8 ]
rounded5 = rectTest [ MCornerRadiusBR 8 ]
rounded6 = rectTest [ MCornerRadius 16, MCornerRadiusBL 0, MCornerRadiusTR 0 ]


symbols1 :: VegaLite
symbols1 =
  let dvals = dataFromColumns []
              . dataColumn "x" (Numbers [ 0 ])

      shapeSpec sym = asSpec [ mark Point [ MFilled True
                                          , MStroke "black"
                                          , MSize 400
                                          , MShape sym
                                          , MStrokeWidth 0.5
                                          ]
                             ]

      shapeList = [ SymCircle, SymSquare, SymDiamond, SymCross
                  , SymTriangleUp, SymTriangleDown, SymTriangleLeft
                  , SymTriangleRight, SymTriangle, SymArrow
                  , SymWedge, SymStroke, SymPath "M -1 -1 L 1 1" ]

      shapes = map shapeSpec shapeList

  in toVegaLite [ dvals []
                , columns 4
                , vlConcat shapes
                ]

symbols2 :: VegaLite
symbols2 =
  chart "Legend using non-default bordered square symbols"
        (color [ MName "Origin"
               , MmType Nominal
               , MLegend [ LSymbolType SymSquare
                         , LSymbolStrokeColor "black"
                         , LSymbolStrokeWidth 0.5
                         ]
               ]
        )


windvector :: VegaLite
windvector =
  let dvals = dataFromUrl "https://vega.github.io/vega-lite/data/windvectors.csv" []

      enc = encoding
            . position X [PName "longitude", PmType Ordinal, PAxis []]
            . position Y [PName "latitude", PmType Ordinal, PAxis []]
            . color [ MName "dir"
                    , MmType Quantitative
                    , MScale [SDomain (DNumbers [0, 360]), SScheme "rainbow" []]
                    , MLegend []
                    ]
            . angle [ MName "dir"
                    , MmType Quantitative
                    , MScale [SDomain (DNumbers [0, 360]), SRange (RPair 180 540)]
                    ]
            . size [MName "speed", MmType Quantitative]

  in toVegaLite [ dvals
                , mark Point [MShape SymWedge]
                , enc []
                , configure
                  . configuration (ViewStyle [ ViewFill "black"
                                             , ViewStep 10 ])
                  $ []
                ]
