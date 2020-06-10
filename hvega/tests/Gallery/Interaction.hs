{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryInteraction.elm (from development of version
-- 1.13.0)
--
module Gallery.Interaction (testSpecs) where

import qualified Data.Text as T

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("interaction1", interaction1)
            , ("interaction2", interaction2)
            , ("interaction3", interaction3)
            , ("interaction4", interaction4)
            , ("interaction5", interaction5)
            , ("interaction6", interaction6)
            , ("interaction7", interaction7)
            , ("interaction8", interaction8)
            , ("interaction9", interaction9)
            , ("interaction10", interaction10)
            , ("interaction11a", interaction11a)
            , ("interaction11b", interaction11b)
            , ("interaction11c", interaction11c)
            , ("interaction11d", interaction11d)
            , ("interactivelinehover", interactiveLineHover)
            ]


pQuant :: T.Text -> [PositionChannel]
pQuant n = [ PName n, PmType Quantitative ]


interaction1 :: VegaLite
interaction1 =
    let
        des =
            description "A bar chart with highlighting on hover and selecting on click. Inspired by Tableau's interaction style."

        config =
            configure
                . configuration (ScaleStyle [ SCBandPaddingInner 0.2 ])

        dvals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C", "D", "E", "F", "G", "H", "I" ])
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        sel =
            selection
                . select "highlight" Single [ On "mouseover", Empty ]
                . select "select" Multi []

        enc =
            encoding
                . position X [ PName "a", PmType Ordinal ]
                . position Y [ PName "b", PmType Quantitative ]
                . fillOpacity [ MSelectionCondition (SelectionName "select") [ MNumber 1 ] [ MNumber 0.3 ] ]
                . strokeWidth
                    [ MDataCondition
                        [ ( And (Selection "select") (Expr "length(data(\"select_store\"))"), [ MNumber 2 ] )
                        , ( Selection "highlight", [ MNumber 1 ] )
                        ]
                        [ MNumber 0 ]
                    ]
    in
    toVegaLite
        [ des
        , config []
        , dvals []
        , sel []
        , mark Bar [ MFill "#4C78A8", MStroke "black", MCursor CPointer ]
        , enc []
        ]


interaction2 :: VegaLite
interaction2 =
    let
        des =
            description "Scatterplot with external links and tooltips"

        trans =
            transform
                . calculateAs "'https://www.google.com/search?q=' + datum.Name" "url"

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Origin", MmType Nominal ]
                . tooltip [ TName "Name", TmType Nominal ]
                . hyperlink [ HName "url", HmType Nominal ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , trans []
        , mark Point []
        , enc []
        ]


interaction3 :: VegaLite
interaction3 =
    let
        des =
            description "Drag out a rectangular brush to highlight points"

        sel =
            selection . select "myBrush" Interval []

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color
                    [ MSelectionCondition (SelectionName "myBrush")
                        [ MName "Cylinders", MmType Ordinal ]
                        [ MString "grey" ]
                    ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , mark Point []
        , sel []
        , enc []
        ]


interaction4 :: VegaLite
interaction4 =
    let
        des =
            description "Area chart with rectangular brush"

        trans =
            transform
                . filter (FSelection "myBrush")

        sel =
            selection . select "myBrush" Interval [ Encodings [ ChX ] ]

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PTimeUnit (TU YearMonth) ]
                . position Y [ PName "count", PmType Quantitative, PAggregate Sum ]

        specBackground =
            asSpec [ mark Area [], sel [] ]

        specHighlight =
            asSpec [ mark Area [ MColor "goldenrod" ], trans [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/unemployment-across-industries.json" []
        , enc []
        , layer [ specBackground, specHighlight ]
        ]


interaction5 :: VegaLite
interaction5 =
    let
        des =
            description "Mouse over individual points or select multiple points with the shift key"

        sel =
            selection . select "myPaintbrush" Multi [ On "mouseover", Nearest True ]

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . size
                    [ MSelectionCondition (SelectionName "myPaintbrush")
                        [ MNumber 300 ]
                        [ MNumber 50 ]
                    ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , mark Point []
        , sel []
        , enc []
        ]


interaction6 :: VegaLite
interaction6 =
    let
        des =
            description "Drag to pan. Zoom in or out with mousewheel/zoom gesture."

        sel =
            selection . select "myGrid" Interval [ BindScales ]

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative, PScale [ SDomain (DNumbers [ 75, 150 ]) ] ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative, PScale [ SDomain (DNumbers [ 20, 40 ]) ] ]
                . size [ MName "Cylinders", MmType Quantitative ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , mark Circle []
        , sel []
        , enc []
        ]


interaction7 :: VegaLite
interaction7 =
    let
        des =
            description "Drag the sliders to highlight points"

        trans =
            transform
                . calculateAs "year(datum.Year)" "Year"

        sel1 =
            selection
                . select "CylYr"
                    Single
                    [ Fields [ "Cylinders", "Year" ]
                    , SInit [ ( "Cylinders", Number 4 ), ( "Year", Number 1977 ) ]
                    , Bind
                        [ IRange "Cylinders" [ InName "Cylinders ", InMin 3, InMax 8, InStep 1 ]
                        , IRange "Year" [ InName "Year ", InMin 1969, InMax 1981, InStep 1 ]
                        ]
                    ]

        encPosition =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]

        enc1 =
            encoding
                . color
                    [ MSelectionCondition (SelectionName "CylYr")
                        [ MName "Origin", MmType Nominal ]
                        [ MString "grey" ]
                    ]

        spec1 =
            asSpec [ sel1 [], mark Circle [], enc1 [] ]

        trans2 =
            transform
                . filter (FSelection "CylYr")

        enc2 =
            encoding
                . color [ MName "Origin", MmType Nominal ]
                . size [ MNumber 100 ]

        spec2 =
            asSpec [ trans2 [], mark Circle [], enc2 [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
        , trans []
        , encPosition []
        , layer [ spec1, spec2 ]
        ]


interaction8 :: VegaLite
interaction8 =
    let
        des =
            description "Drag over bars to update selection average"

        sel =
            selection . select "myBrush" Interval [ Encodings [ ChX ] ]

        encPosition =
            encoding . position Y [ PName "precipitation", PmType Quantitative, PAggregate Mean ]

        enc1 =
            encoding
                . position X [ PName "date", PmType Ordinal, PTimeUnit (TU Month) ]
                . opacity
                    [ MSelectionCondition (SelectionName "myBrush")
                        [ MNumber 1 ]
                        [ MNumber 0.7 ]
                    ]

        spec1 =
            asSpec [ sel [], mark Bar [], enc1 [] ]

        trans =
            transform
                . filter (FSelection "myBrush")

        enc2 =
            encoding
                . color [ MString "firebrick" ]
                . size [ MNumber 3 ]

        spec2 =
            asSpec [ des, trans [], mark Rule [], enc2 [] ]
    in
    toVegaLite
        [ dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []
        , encPosition []
        , layer [ spec1, spec2 ]
        ]


interaction9 :: VegaLite
interaction9 =
    let
        desc =
            description "Displays tooltips for all stock prices of the hovered time"

        enc1 =
            encoding
                . position X [ PName "date", PmType Temporal ]
                . position Y [ PName "price", PmType Quantitative ]
                . color [ MName "symbol", MmType Nominal ]

        spec1 =
            asSpec
                [ enc1 []
                , layer
                    [ asSpec [ mark Line [] ]
                    , asSpec [ mark Point [], sel1_2 [], enc1_2 [] ]
                    ]
                ]

        enc1_2 =
            encoding
                . opacity [ MSelectionCondition (Expr "myTooltip") [ MNumber 1 ] [ MNumber 0 ] ]

        sel1_2 =
            selection
                . select "myTooltip"
                    Single
                    [ Nearest True
                    , On "mouseover"
                    , Encodings [ ChX ]
                    , Empty
                    ]

        spec2 =
            asSpec [ trans2 [], layer [ spec2_1, spec2_2 ] ]

        trans2 =
            transform . filter (FSelection "myTooltip")

        spec2_1 =
            asSpec [ mark Rule [ MColor "gray" ], enc2_1 [] ]

        enc2_1 =
            encoding . position X [ PName "date", PmType Temporal ]

        spec2_2 =
            asSpec [ mark Text [ MAlign AlignLeft, MdX 5, MdY (-5) ], enc2_2 [] ]

        enc2_2 =
            encoding
                . position X [ PName "date", PmType Temporal ]
                . position Y [ PName "price", PmType Quantitative ]
                . text [ TName "price", TmType Quantitative ]
                . color [ MName "symbol", MmType Nominal ]
    in
    toVegaLite
        [ width 800
        , height 400
        , desc
        , dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" [ Parse [ ( "date", FoDate "" ) ] ]
        , layer [ spec1, spec2 ]
        ]


interaction10 :: VegaLite
interaction10 =
    let
        desc =
            description "Multi Series Line Chart with Tooltip"

        config =
            configure
                . configuration (AxisY [ MinExtent 30 ])

        timeUnit = TU YearMonthDate
        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PTimeUnit timeUnit ]
                . tooltips
                    [ [ TName "date", TmType Temporal, TTimeUnit timeUnit ]
                    , [ TName "temp_max", TmType Quantitative ]
                    , [ TName "temp_min", TmType Quantitative ]
                    ]

        enc1 =
            encoding
                . position Y [ PName "temp_max", PmType Quantitative ]

        spec1 =
            asSpec [ mark Line [ MColor "orange" ], enc1 [] ]

        enc2 =
            encoding
                . position Y [ PName "temp_min", PmType Quantitative ]

        spec2 =
            asSpec [ mark Line [ MColor "red" ], enc2 [] ]

        sel =
            selection
                . select "hover" Single [ On "mouseover", Empty ]

        enc3 =
            encoding
                . color
                    [ MSelectionCondition (Not (SelectionName "hover"))
                        [ MString "transparent" ]
                        []
                    ]

        spec3 =
            asSpec [ sel [], mark Rule [], enc3 [] ]
    in
    toVegaLite
        [ config []
        , desc
        , dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []
        , enc []
        , layer [ spec1, spec2, spec3 ]
        ]


-- From https://vega.github.io/vega-lite/examples/interactive_brush.html
-- and added to test SInitInterval
--
initInterval :: Maybe (DataValue, DataValue) -> Maybe (DataValue, DataValue) -> VegaLite
initInterval mx my =
  let desc = "Drag out a rectangular brush to highlight points."

      sel = selection
              . select "brush" Interval [ SInitInterval mx my ]

      enc = encoding
              . position X (pQuant "Horsepower")
              . position Y (pQuant "Miles_per_Gallon")
              . color [ MSelectionCondition
                          (SelectionName "brush")
                          [ MName "Cylinders", MmType Ordinal ]
                          [ MString "grey" ]
                      ]

  in toVegaLite
     [ description desc
     , dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
     , mark Point []
     , enc []
     , sel []
     ]


xInit, yInit :: (DataValue, DataValue)
xInit = (Number 55, Number 160)
yInit = (Number 13, Number 37)

-- Note: the Vega-Lite spec (circa version 4) does not support the
--       "Nothing Nothing" case - aka interaction11a - so this is a
--       check to make sure we handle this case correctly.
--
interaction11a, interaction11b, interaction11c, interaction11d :: VegaLite
interaction11a = initInterval Nothing Nothing
interaction11b = initInterval (Just xInit) Nothing
interaction11c = initInterval Nothing (Just yInit)
interaction11d = initInterval (Just xInit) (Just yInit)


-- https://vega.github.io/vega-lite/examples/interactive_line_hover.html
interactiveLineHover :: VegaLite
interactiveLineHover =
  let desc = "Multi-series line chart with labels and interactive highlight on hover.  We also set the selection's initial value to provide a better screenshot"

      dvals = dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []

      baseEnc = encoding
                . color [ MSelectionCondition (SelectionName "hover")
                          [MName "symbol", MmType Nominal, MLegend []]
                          [MString "grey"]
                        ]
                . opacity [ MSelectionCondition (SelectionName "hover")
                            [MNumber 1]
                            [MNumber 0.2]
                          ]

      lyr1 = [ encoding
               . position X [PName "date", PmType Temporal, PTitle "date"]
               . position Y [PName "price", PmType Quantitative, PTitle "price"]
               $ []
             , layer [asSpec lyr11, asSpec lyr12]
             ]

      lyr11 = [ description "transparent layer to make it easier to trigger selection"
              , selection
                . select "hover" Single [ On "mouseover"
                                        , Empty
                                        , Fields ["symbol"]
                                        , SInit [("symbol", Str "AAPL")]
                                        ]
                $ []
              , mark Line [MStrokeWidth 8, MStroke "transparent"]
              ]
      lyr12 = [mark Line []]

      lyr2 = [ encoding
               . position X [PName "date", PmType Temporal, PAggregate Max]
               . position Y [PName "price", PmType Quantitative, PAggregate (ArgMax (Just "date"))]
               $ []
             , layer [asSpec lyr21, asSpec lyr22]
             ]

      lyr21 = [mark Circle []]
      lyr22 = [ mark Text [MAlign AlignLeft, MdX 4]
              , encoding
                . text [TName "symbol", TmType Nominal]
                $ []
              ]

  in toVegaLite [ description desc
                , dvals
                , transform
                  . filter (FExpr "datum.symbol!=='IBM'")
                  $ []
                , baseEnc []
                , layer (map asSpec [lyr1, lyr2])
                , configure
                  . configuration (ViewStyle [ViewNoStroke])
                  $ []
                ]
