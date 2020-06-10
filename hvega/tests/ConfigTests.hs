{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ConfigTests.elm as of version 1.12.0
--
--  - Padding has been removed as the resulting spec does not validate
--    against v3.3.0
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
            , ("paddingx", paddingXCfg)
            , ("paddingy", paddingYCfg)
            , ("paddingContent", paddingCntCfg)
            , ("paddingNone", paddingNoneCfg)
            , ("paddingPad", paddingPadCfg)
            , ("paddingPadding", paddingPaddingCfg)
            , ("paddingResize", paddingResizeCfg)
            , ("vbTest", vbTest)
            , ("axisCfg1", axisCfg1)
            , ("fontCfg", fontCfg)
            , ("titleCfg1", titleCfg1)
            , ("titleCfg2", titleCfg2)
            , ("titleCfg3", titleCfg3)
            , ("breaklinecfg", breakLineCfg)
            , ("headerlabels", headerlabels)
            ]


carData :: Data
carData = dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []


xHorse, yMPG :: BuildEncodingSpecs
xHorse = position X [ PName "Horsepower", PmType Quantitative ]
yMPG = position Y [ PName "Miles_per_Gallon", PmType Quantitative ]

yCount :: [PositionChannel] -> BuildEncodingSpecs
yCount opts = position Y ([ PAggregate Count, PmType Quantitative ] ++ opts)


mCylinders, mOrigin :: [MarkChannel]
mCylinders = [ MName "Cylinders", MmType Ordinal ]
mOrigin = [ MName "Origin", MmType Nominal ]


singleVis :: ([a] -> (VLProperty, VLSpec)) -> VegaLite
singleVis config =
  let scatterEnc =
        encoding
        . xHorse
        . yMPG
        . color mCylinders
        . shape mOrigin

  in toVegaLite [ title "Car Scatter" [], config [], carData, width 200, height 200, mark Point [ MSize 100 ], scatterEnc [] ]


{- TODO: padding causes spec to be invalid -}
compositeVis :: ([a] -> (VLProperty, VLSpec)) -> VegaLite
compositeVis config =
    let scatterEnc =
            encoding
                . xHorse
                . yMPG
                . color mCylinders
                . shape mOrigin

        scatterSpec =
            asSpec [ title "Car Scatter" [], width 200, height 200
                   {- , padding (PSize 20) -}
                   , mark Point [ MSize 100 ], scatterEnc [] ]

        barEnc =
            encoding
                . xHorse
                . yCount []
                . color mOrigin

        streamEnc =
            encoding
                . position X [ PName "Year", PmType Temporal, PTimeUnit (TU Year) ]
                . yCount [ PStack StCenter, PAxis [] ]
                . color mOrigin

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
    toVegaLite [ config [], carData, hConcat [ scatterSpec, barSpec, streamSpec ], res [] ]


vbTest :: VegaLite
vbTest =
    let scatterEnc =
            encoding
                . xHorse
                . yMPG
                . color mCylinders
                . shape mOrigin

        barEnc =
            encoding
                . xHorse
                . yCount []
                . color mOrigin

        streamEnc =
            encoding
                . position X [ PName "Year", PmType Temporal, PTimeUnit (TU Year) ]
                . yCount [ PStack StCenter, PAxis [] ]
                . color mOrigin

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
                    [ VBFill "white"
                    , VBCornerRadius 18
                    , VBStroke "red"
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
                    (MarkNamedStyles
                        [ ( "myStyle", [ MFill "red", MFillOpacity 0.1, MStrokeOpacity 1 ] )
                        , ( "mySecondStyle", [ MFill "black", MStroke "blue" ] )
                        ]
                    )
                . configuration (ViewStyle [ ViewBackgroundStyle [ VBFill "#feb" ] ])
                . configuration (LegendStyle [ LeNoTitle ])

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
        , carData
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
        . configuration (BackgroundStyle "black")
        . configuration (TitleStyle [ TFont "Roboto", TColor "#fff" ])
        . configuration (Axis [ DomainColor "yellow", GridColor "rgb(255,255,200)", GridOpacity 0.2, LabelColor "#fcf", TickColor "white", TitleColor "rgb(200,255,200)", LabelFont "Roboto", TitleFont "Roboto" ])
        . configuration (LegendStyle [ LeFillColor "#333", LeStrokeColor "#444", LeTitleColor "rgb(200,200,200)", LeLabelColor "white", LeSymbolFillColor "red", LeGradientStrokeColor "yellow", LeLabelFont "Roboto", LeTitleFont "Roboto"
                                     , LeDirection Horizontal
                                     ])
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


paddingTest :: Autosize -> VegaLite
paddingTest a =
  configure
  . configuration (AutosizeStyle [ a ])
  . configuration (PaddingStyle (PEdges 90 60 30 0))
  & singleVis


paddingCfg, paddingXCfg, paddingYCfg, paddingCntCfg, paddingNoneCfg,
  paddingPadCfg, paddingPaddingCfg, paddingResizeCfg:: VegaLite
paddingCfg = paddingTest AFit
paddingXCfg = paddingTest AFitX
paddingYCfg = paddingTest AFitY
paddingCntCfg = paddingTest AContent
paddingNoneCfg = paddingTest ANone
paddingPadCfg = paddingTest APad
paddingPaddingCfg = paddingTest APadding
paddingResizeCfg = paddingTest AResize


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


fontCfg :: VegaLite
fontCfg =
    configure
       . configuration (FontStyle "Comic Sans MS")
       & singleVis


titleOpts :: [PropertySpec]
titleOpts =
  [ carData
  , width 200
  , height 200
  , mark Circle []
  , encoding . xHorse . yMPG $ []
  ]


titleCfg1 :: VegaLite
titleCfg1 =
  toVegaLite
    (title "Car\nScatter" [ TSubtitle "A subtitle\nalso over two lines" ]
     : titleOpts)


cfgOpts :: [TitleConfig]
cfgOpts =
  [ TAnchor AEnd
  , TSubtitleColor "red"
  , TSubtitleFont "serif"
  , TSubtitleFontSize 10
  , TSubtitleFontStyle "italic"
  , TSubtitleFontWeight W900
  , TSubtitleLineHeight 18
  , TSubtitlePadding 60
  , TLineHeight 20
  , TdX (-30)
  , TdY 10
  ]

subtitle :: TitleConfig
subtitle = TSubtitle "A subtitle\nalso over two lines"

titleCfg2 :: VegaLite
titleCfg2 =
  toVegaLite
    (title "Car\nScatter" (subtitle : cfgOpts)
      : titleOpts)


titleCfg3 :: VegaLite
titleCfg3 =
  let cfg = configure
                . configuration
                    (TitleStyle cfgOpts)

  in toVegaLite
     ( [ cfg []
       , title "Car\nScatter" [ subtitle ]
       ] ++ titleOpts )


breakLineCfg :: VegaLite
breakLineCfg =
  let dvals = dataFromColumns []
               . dataColumn "x" (Numbers [5, 10, 15])
               . dataColumn "y" (Numbers [10, 5, 30])
               . dataColumn "l" (Strings ["xXx", "x x", "xxXxXxx"])

      enc = encoding
            . position X [PName "x", PmType Quantitative]
            . position Y [PName "y", PmType Quantitative]
            . text [TName "l", TmType Nominal]

  in toVegaLite [ configure (configuration (LineBreakStyle "X") [])
                , dvals []
                , enc []
                , mark Text []
                ]


headerlabels :: VegaLite
headerlabels =
  let conf = configure
             . configuration (HeaderStyle [HLabel False])
             $ []

      dvals = dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []

      trans = transform
              . filter (FExpr "datum.year === 2000")
              . calculateAs "datum.sex === 2 ? 'Female' : 'Male'" "gender"
              $ []

      enc = encoding
            . position X [PName "age", PmType Ordinal]
            . position Y [PName "people", PmType Quantitative, PAggregate Sum]
            . column [FName "gender", FmType Nominal]
            . color [MName "gender", MmType Nominal]
            $ []

  in toVegaLite [conf, dvals, trans, enc, mark Bar []]
