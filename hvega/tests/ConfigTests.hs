{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ConfigTests.elm as of version 1.12.0
--
--  - Padding has been removed as the resulting spec does not validate
--    against v3.3.0
--
--  - the vbTest output is not valid since the spec description says that
--    style can be a string or array-of-strings, but the type is only
--    string.
--

module ConfigTests (testSpecs) where

import Data.Function ((&))
import Graphics.Vega.VegaLite

import Prelude hiding (filter)


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("default", defaultCfg)
            , ("dark", darkCfg)
            , ("mark1", markCfg1)
            , ("mark2", markCfg2)
            , ("padding", paddingCfg)
            , ("vbTest", vbTest)
            , ("axisCfg1", axisCfg1)
            ]

singleVis :: ([a] -> (VLProperty, VLSpec)) -> VegaLite
singleVis config =
    let
        cars =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        scatterEnc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Cylinders", MmType Ordinal ]
                . shape [ MName "Origin", MmType Nominal ]
    in
    toVegaLite [ title "Car Scatter" [], config [], cars, width 200, height 200, mark Point [ MSize 100 ], scatterEnc [] ]


{- TODO: padding causes spec to be invalid -}
compositeVis :: ([a] -> (VLProperty, VLSpec)) -> VegaLite
compositeVis config =
    let
        cars =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        scatterEnc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Cylinders", MmType Ordinal ]
                . shape [ MName "Origin", MmType Nominal ]

        scatterSpec =
            asSpec [ title "Car Scatter" [], width 200, height 200
                   {- , padding (PSize 20) -}
                   , mark Point [ MSize 100 ], scatterEnc [] ]

        barEnc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PAggregate Count, PmType Quantitative ]
                . color [ MName "Origin", MmType Nominal ]

        streamEnc =
            encoding
                . position X [ PName "Year", PmType Temporal, PTimeUnit Year ]
                . position Y [ PAggregate Count, PmType Quantitative, PStack StCenter, PAxis [] ]
                . color [ MName "Origin", MmType Nominal ]

        barSpec =
            asSpec [ title "Car Histogram" [], width 200, height 200
                   {- , padding (PSize 20) -}
                   , mark Bar [], barEnc [] ]

        streamSpec =
            asSpec [ title "Car Streamgraph" [], width 200, height 200
                   {- , padding (PSize 20) -}
                   , mark Area [], streamEnc [] ]

        res =
            resolve
                . resolution (RScale [ ( ChColor, Independent ), ( ChShape, Independent ) ])
    in
    toVegaLite [ config [], cars, hConcat [ scatterSpec, barSpec, streamSpec ], res [] ]


vbTest :: VegaLite
vbTest =
    let
        cars =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        scatterEnc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color [ MName "Cylinders", MmType Ordinal ]
                . shape [ MName "Origin", MmType Nominal ]

        barEnc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PAggregate Count, PmType Quantitative ]
                . color [ MName "Origin", MmType Nominal ]

        streamEnc =
            encoding
                . position X [ PName "Year", PmType Temporal, PTimeUnit Year ]
                . position Y [ PAggregate Count, PmType Quantitative, PStack StCenter, PAxis [] ]
                . color [ MName "Origin", MmType Nominal ]

        scatterSpec =
            asSpec
                [ title "Car Scatter" [TColor "brown", TFontSize 12]
                , width 200
                , height 200
                , mark Point [ MSize 100 ]
                , scatterEnc []
                ]

        barSpec =
            asSpec
                [ title "Car Histogram" []
                , width 200
                , height 200
                , viewBackground
                    [ VBFill (Just "white")
                    , VBCornerRadius 18
                    , VBStroke (Just "red")
                    , VBStrokeWidth 4
                    , VBStrokeCap CRound
                    , VBStrokeDash [ 10, 10 ]
                    , VBStrokeJoin JBevel
                    ]
                , mark Bar []
                , barEnc []
                ]

        cfg =
            configure
                . configuration
                    (NamedStyles
                        [ ( "myStyle", [ MFill "red", MFillOpacity 0.1, MStrokeOpacity 1 ] )
                        , ( "mySecondStyle", [ MFill "black", MStroke "blue" ] )
                        ]
                    )
                . configuration (View [ ViewBackgroundStyle [ VBFill (Just "#feb") ] ])

        streamSpec =
            asSpec
                [ title "Car Streamgraph" []
                , width 200
                , height 200
                , viewBackground [ VBStyle [ "myStyle", "mySecondStyle" ] ]
                , mark Area []
                , streamEnc []
                ]

        res =
            resolve
                . resolution (RScale [ ( ChColor, Independent ), ( ChShape, Independent ) ])
    in
    toVegaLite
        [ cfg []
        , background "yellow"
        , cars
        , hConcat [ scatterSpec, barSpec, streamSpec ]
        , res []
        ]


defaultCfg :: VegaLite
defaultCfg =
    configure
        & compositeVis

darkCfg :: VegaLite
darkCfg =
    configure
        . configuration (Background "black")
        . configuration (TitleStyle [ TFont "Roboto", TColor "#fff" ])
        . configuration (Axis [ DomainColor "yellow", GridColor "rgb(255,255,200)", GridOpacity 0.2, LabelColor "#fcf", TickColor "white", TitleColor "rgb(200,255,200)", LabelFont "Roboto", TitleFont "Roboto" ])
        . configuration (Legend [ LeFillColor "#333", LeStrokeColor "#444", LeTitleColor "rgb(200,200,200)", LeLabelColor "white", LeSymbolFillColor "red", LeGradientStrokeColor "yellow", LeLabelFont "Roboto", LeTitleFont "Roboto" ])
        & compositeVis


markCfg1 :: VegaLite
markCfg1 =
    configure
        . configuration (MarkStyle [ MFilled False ])
        & compositeVis


markCfg2 :: VegaLite
markCfg2 =
    configure
        . configuration (MarkStyle [ MFilled True, MFill "black", MOpacity 1 ])
        . configuration (BarStyle [ MFilled True ])
        . configuration (AreaStyle [ MFilled False ])
        . configuration (PointStyle [ MFilled True, MStroke "white", MStrokeOpacity 0.2 ])
        & compositeVis


paddingCfg :: VegaLite
paddingCfg =
    configure
        . configuration (Autosize [ AFit ])
        . configuration (Padding (PEdges 90 60 30 0))
        & singleVis


axisCfg1 :: VegaLite
axisCfg1 =
    configure
       . configuration (Axis [ TitleFontStyle "italic"
                             , TitleFont "serif"
                             , LabelFontStyle "bold"
                             , LabelFont "serif"
                             , TitleAnchor AEnd
                             ])
       & singleVis
