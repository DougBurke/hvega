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
testSpecs = [("blendmode", blendMode)]


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
