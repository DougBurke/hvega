{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite LegendTests.elm as of version 1.12.0
--
module LegendTests (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("legend1", legend1)
            , ("legend2", legend2)
            , ("legend3", legend3)
            , ("legend4", legend4)
            , ("legend5", legend5)
            , ("legend6", legend6)
            , ("legend7", legend7)
            , ("legend8", legend8)
            , ("legend9", legend9)
            , ("legend10", legend10)
            , ("legend11", legend11)
            , ("legend11d", legend11direct)
            , ("corners1", corners1)
            , ("corners2", corners2)
            , ("limits1", limits 1)
            , ("limits2", limits 2)
            , ("limits3", limits 3)
            , ("disablelegend", disableLegend)
            ]


carsData :: Data
carsData = dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []


xHorse, yMPG :: BuildEncodingSpecs
xHorse = position X [ PName "Horsepower", PmType Quantitative ]
yMPG = position Y [ PName "Miles_per_Gallon", PmType Quantitative ]


-- here we don't want [] to mean "no legend"
noNull :: [LegendProperty] -> [MarkChannel]
noNull [] = []
noNull lps = [MLegend lps]

origin, hp, weight :: [LegendProperty] -> [MarkChannel]
origin lps = noNull lps ++ [ MName "Origin", MmType Nominal ]
hp lps = noNull lps ++ [ MName "Horsepower", MmType Quantitative ]
weight lps = noNull lps ++ [ MName "Weight_in_lbs", MmType Quantitative ]


legendCore :: [LegendProperty] -> VegaLite
legendCore legProps =
  let enc = encoding
            . xHorse
            . yMPG
            . color (origin legProps)
            . size (hp legProps)
            . opacity (weight legProps)

  in toVegaLite [ width 300, height 300, carsData, enc [], mark Circle [] ]


legendCoreCfg :: [LegendConfig] -> VegaLite
legendCoreCfg cfg =
    let enc =
            encoding
                . xHorse
                . yMPG
                . color [ MName "Origin", MmType Nominal ]
                . size [ MName "Horsepower", MmType Quantitative ]
                . opacity [ MName "Weight_in_lbs", MmType Quantitative ]
    in
    toVegaLite
        [ (configure . configuration (LegendStyle cfg)) []
        , width 300
        , height 300
        , carsData
        , enc []
        , mark Circle []
        ]

legend1 :: VegaLite
legend1 = legendCoreCfg []

legend2 :: VegaLite
legend2 = legendCore [ LOrient LOTop ]

legend3 :: VegaLite
legend3 = legendCore [ LOrient LOTopRight ]

legend4 :: VegaLite
legend4 = legendCore [ LOrient LORight ]

legend5 :: VegaLite
legend5 = legendCore [ LOrient LOBottomRight ]

legend6 :: VegaLite
legend6 = legendCore [ LOrient LOBottom ]

legend7 :: VegaLite
legend7 = legendCore [ LOrient LOBottomLeft ]

legend8 :: VegaLite
legend8 = legendCore [ LOrient LOLeft ]

legend9 :: VegaLite
legend9 = legendCore [ LOrient LOTopLeft ]

legend10 :: VegaLite
legend10 =
    let enc =
            encoding
                . xHorse
                . yMPG
                . color [ MName "Origin", MmType Nominal,
                          MLegend [ LOrient LONone, LeX 232, LeY 5 ] ]
    in
    toVegaLite [ width 300, height 300, carsData, enc [], mark Circle [] ]

-- change a number of items using the legend configure
legend11 :: VegaLite
legend11 =
    legendCoreCfg
        [ LeSymbolStrokeWidth 3
        , LeSymbolStrokeColor "black"
        , LeRowPadding 15
        , LeTitlePadding 20
        , LeStrokeColor "lightgrey"
        , LeStrokeWidth 5
        , LePadding 30
        , LeStrokeDash [ 4, 2, 6, 1 ]
        ]


-- and now directly, although it looks like Vega Lite 4.0.2
-- doesn't allow all combinations (e.g. stroke width or dash)
-- to be specified.
--
legend11direct :: VegaLite
legend11direct =
    legendCore
        [ LSymbolStrokeWidth 3
        , LSymbolStrokeColor "black"
        , LRowPadding 15
        , LTitlePadding 20
        , LStrokeColor "lightgrey"
        -- , LStrokeWidth 5
        , LPadding 30
        -- , LStrokeDash [ 4, 2, 6, 1 ]
        ]


-- Show off the corner support
corners1 :: VegaLite
corners1 =
    legendCoreCfg
        [ LeSymbolStrokeWidth 1
        , LeSymbolStrokeColor ""
        , LeRowPadding 15
        , LeTitlePadding 20
        , LeStrokeColor "orange"
        , LeStrokeWidth 2
        , LePadding 30
        , LeStrokeDash [ 4, 2, 6, 1 ]
        , LeCornerRadius 5
        , LeFillColor "pink"
        , LeLabelOpacity 0.6
        , LeLabelFontSize 20
        , LeLabelColor "fireBrick"
        , LeLabelFontWeight W600
        , LeLabelBaseline AlignTop
        , LeLabelAlign AlignRight
        , LeLabelOverlap OGreedy
        ]

corners2 :: VegaLite
corners2 =
  legendCore
        [ LSymbolStrokeWidth 1
        , LSymbolStrokeColor ""
        , LRowPadding 15
        , LTitlePadding 20
        , LStrokeColor "orange"
        -- , LStrokeWidth 2
        , LPadding 30
        -- , LStrokeDash [ 4, 2, 6, 1 ]
        , LCornerRadius 5
        , LFillColor "pink"
        , LLabelOpacity 0.6
        , LLabelFontSize 20
        , LLabelColor "fireBrick"
        , LLabelExpr "'<' + datum.label + '>'"
        , LLabelFontWeight W600
        , LLabelBaseline AlignTop
        , LLabelAlign AlignRight
        , LLabelOverlap OGreedy
        , LSymbolLimit 5
        ]


-- trying to combine symbollimit and unselectedopacity
limits :: Int -> VegaLite
limits n =
  let enc = encoding
            . xHorse
            . yMPG
            . shape (origin [])
            . color (origin [])
            . size [ MSelectionCondition (SelectionName "sel")
                     [ MNumber 100 ]
                     [ MNumber 20 ]
                   ]

      cfg = [ LeSymbolLimit n
            , LeUnselectedOpacity 0.1 ]

      selOpts = [ On "click"
                , BindLegend (BLFieldEvent "Origin" "dblclick") ]

  in toVegaLite [ width 300
                , height 300
                , configure (configuration (LegendStyle cfg) [])
                , selection (select "sel" Multi selOpts [])
                , carsData
                , enc []
                , mark Point [MFilled True]
                ]


-- Vega-Lite 4.8
--
-- At the time of writing (V-L 4.8.1 has just neen released), the JS display libs
-- don't do anything with this setting. I assume this is a bug or they haven't been
-- updated yet.
--
disableLegend :: VegaLite
disableLegend = legendCoreCfg [ LeDisable True ]
