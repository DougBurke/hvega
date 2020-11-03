{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- random mark-related tests
--

module MarkTests (testSpecs) where

import qualified Data.Text as T

import Data.List (intercalate)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Text.Printf (printf)

import Graphics.Vega.VegaLite

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("blendmode", blendMode)
            , ("pieChart", pieChart)
            , ("pieChartWithLabels", pieChartWithLabels)
            , ("donutChart", donutChart)
            , ("radialChart", radialChart)
            , ("pyramidChart", pyramidChart)
            , ("histogram_binned_no_x2", histogramBinnedNoX2)
            ]


-- How does blend-mode work (added in Vega-Lite 4.6.0)?
-- This is based on
-- https://developer.mozilla.org/en-US/docs/Web/CSS/mix-blend-mode#Examples
--

blendData :: Data
blendData =
  dataFromColumns []
  . dataColumn "x" (Numbers [0])
  . dataColumn "y" (Numbers [0])
  $ []


-- rotate an ellipse about the origin; it would be better if I learned
-- how to use the arc path segment
--
-- angle in degrees
ellipse :: Double -> Symbol
ellipse ang =
  let rad = ang * pi / 180
      cosRot = cos rad
      sinRot = sin rad
      
      rmajor = 1.0
      rminor = 0.3

      p = printf "%.2f"
            
      -- could learn to use the Arc path segment, but just do it manually
      pair t =
        let x = rmajor * cos t * cosRot - rminor * sin t * sinRot
            y = rmajor * cos t * sinRot + rminor * sin t * cosRot
        in "L " <> p x <> " " <> p y

      thetas = [0, 0.25 ..  2 * pi]
      path = intercalate " " (map pair thetas)
      
  in SymPath (T.pack path)
  

-- Is there a better way to do this?
blendMode :: VegaLite
blendMode =
  let ax t f = position t [ PName f
                          , PmType Quantitative
                          , PScale [SDomain (DNumbers [-5, 5])]
                          ]

      -- randomly trying to get similar results to the Mozilla page
      props 0 = [GrX1 0.5, GrX2 0.5, GrY1 1, GrY2 0] 
      props 1 = [GrX1 0.7, GrX2 0.3, GrY1 0.1, GrY2 1]
      props _ = [GrX1 1, GrX2 0, GrY1 1, GrY2 0]
      
      gradient f =
        let c | f == 0 = "rgb(0,255,0)"
              | f == 1 = "rgb(255,0,0)"
              | otherwise = "rgb(0,0,255)"
              
        in MFillGradient GrLinear [(0, "white"), (1, c)] (props f)

      lyr bm f =
        let a | f == 0 = 0
              | f == 1 = 45
              | otherwise = -45
              
        in asSpec [ mark Point [ MShape (ellipse a)
                               , MBlend bm
                               , gradient f
                               ]
                  ]

      createLayer (bm, ttl) =
        asSpec [ encoding . ax X "x" . ax Y "y" $ []
               , layer (map (lyr bm) [0::Int .. 2])
               , title ttl []
               ]

      layers = map createLayer [ (BMNormal, "Normal")
                               , (BMMultiply, "Multiply")
                               , (BMScreen, "Screen")
                               , (BMOverlay, "Overlay")
                               , (BMDarken, "Darken")
                               , (BMLighten, "Lighten")
                               , (BMColorDodge, "Color-Dodge")
                               , (BMColorBurn, "Color-Burn")
                               , (BMHardLight, "Hard-Light")
                               , (BMSoftLight, "Soft-Light")
                               , (BMDifference, "Difference")
                               , (BMExclusion, "Exclusion")
                               , (BMHue, "Hue")
                               , (BMSaturation, "Saturation")
                               , (BMColor, "Color")
                               , (BMLuminosity, "Luminosity")
                               ]
      
  in toVegaLite [ configure
                  . configuration (Axis [Domain False, Labels False, Ticks False, NoTitle])
                  . configuration (PointStyle [MOpacity 1, MSize 40000, MStroke ""])
                  -- note the interesting background
                  . configuration (BackgroundStyle "rgba(255,255,255,0)")
                  $ []
                , blendData
                , columns 4
                , vlConcat layers
                ]


pieChart :: VegaLite
pieChart =
  let desc = description "A simple pie chart with embedded data."
      dvals = dataFromColumns []
              . dataColumn "category" (Numbers [1, 2, 3, 4, 5, 6])
              . dataColumn "value" (Numbers [4, 6, 10, 3, 7, 8])
              $ []
      
  in toVegaLite [ desc
                , dvals
                , mark Arc []
                , encoding
                  . position Theta [PName "value", PmType Quantitative]
                  . color [MName "category", MmType Nominal]
                  $ []
                , viewBackground [VBNoStroke]
                ]


pieChartWithLabels :: VegaLite
pieChartWithLabels =
  let desc = description "A simple pie chart with labels."
      dvals = dataFromColumns []
              . dataColumn "category" (Strings ["a", "b", "c", "d", "e", "f"])
              . dataColumn "value" (Numbers [4, 6, 10, 3, 7, 8])
              $ []

      plot = [mark Arc [MOuterRadius 80]]
      label = [ mark Text [MRadius 90]
              , encoding (text [TName "category", TmType Nominal] [])
              ]

  in toVegaLite [ desc
                , dvals
                , encoding
                  -- can not get stack: true, but should be the same
                  . position Theta [PName "value", PmType Quantitative, PStack StZero]
                  . color [MName "category", MmType Nominal, MLegend []]
                  $ []
                , layer [asSpec plot, asSpec label]
                , viewBackground [VBNoStroke]
                ]


donutChart :: VegaLite
donutChart =
  let desc = description "A simple donut chart with embedded data."
      dvals = dataFromColumns []
              . dataColumn "category" (Numbers [1, 2, 3, 4, 5, 6])
              . dataColumn "value" (Numbers [4, 6, 10, 3, 7, 8])
              $ []
      
  in toVegaLite [ desc
                , dvals
                , mark Arc [MInnerRadius 50]
                , encoding
                  . position Theta [PName "value", PmType Quantitative]
                  . color [MName "category", MmType Nominal]
                  $ []
                , viewBackground [VBNoStroke]
                ]


radialChart :: VegaLite
radialChart =
  let desc = description "A simple radial chart with embedded data."
      dvals = dataFromColumns []
              . dataColumn "data" (Numbers [12, 23, 47, 6, 52, 19])
              $ []

      plot = [mark Arc [MInnerRadius 20, MStroke "#fff"]]
      label = [ mark Text [MRadiusOffset 10]
              , encoding (text [TName "data", TmType Quantitative] [])
              ]

  in toVegaLite [ desc
                , dvals
                , encoding
                  -- can not get stack: true, but should be the same
                  . position Theta [PName "data", PmType Quantitative, PStack StZero]
                  . position R [ PName "data"
                               , PmType Quantitative
                               , PScale [ SType ScSqrt
                                        , SZero True
                                        , SRange (RPair 20 100)
                                        ]
                               ]
                  . color [MName "data", MmType Nominal, MLegend []]
                  $ []
                , layer [asSpec plot, asSpec label]
                , viewBackground [VBNoStroke]
                ]


-- https://vega.github.io/vega-lite/examples/arc_pie_pyramid.html
--
pyramidChart :: VegaLite
pyramidChart =
  let desc = description "Reproducing http://robslink.com/SAS/democd91/pyramid_pie.htm"
      dvals = dataFromColumns []
              . dataColumn "value" (Numbers [75, 10, 15])
              . dataColumn "order" (Numbers [3, 1, 2])
              . dataColumn "category" (Strings ["Sky", "Shady side of a pyramid", "Sunny side of a pyramid"])
              $ []

      cmap = [ ("Sky", "#416D9D")
             , ("Shady side of a pyramid", "#674028")
             , ("Sunny side of a pyramid", "#DEAC58")
             ]

  in toVegaLite [ desc
                , dvals
                , mark Arc [MOuterRadius 80]
                , encoding
                  . position Theta [ PName "value"
                                   , PmType Quantitative
                                   , PScale [SRange (RPair 2.35619449 8.639379797)]
                                   -- How to get "stack": True???
                                   , PStack StZero
                                   ]
                  . color [ MName "category"
                          , MmType Nominal
                          , MScale (categoricalDomainMap cmap)
                          , MLegend [ LOrient LONone
                                    , LNoTitle
                                    , LColumns 1
                                    , LeX 200
                                    , LeY 80
                                    ]
                          ]
                  . order [OName "order"]
                  $ []
                , viewBackground [VBNoStroke]
                ]


-- https://github.com/vega/vega-lite/pull/6473
-- as part of https://github.com/vega/vega-lite/issues/6086
histogramBinnedNoX2 :: VegaLite
histogramBinnedNoX2 =
  let dvals = dataFromColumns []
              . dataColumn "bin_start" (Numbers [8, 10, 12, 14, 16, 18, 20, 22])
              . dataColumn "count" (Numbers [7, 29, 71, 127, 94, 54, 17, 5])
              $ []

  in toVegaLite [ dvals
                , mark Bar []
                , encoding
                  . position X [ PName "bin_start"
                               , PmType Quantitative
                               , PBin [AlreadyBinned True, Step 2]
                               ]
                  . position Y [PName "count", PmType Quantitative]
                  $ []
                ]
