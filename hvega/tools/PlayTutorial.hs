{-# LANGUAGE OverloadedStrings #-}

--
--
-- Usage:
--    ./playtutorial dname

-- Run through the tutorials, creating output files for each visualization:
--
--     <dname>/<name>.html
--     <dname>/<name>.vg.json
--
-- These files are written to the indicated directory, which will be created
-- if necessary, and will over-write any existing files. The directory name
-- can contain multiple new directories (e.g. "foo/bar" will create "foo"
-- if needed).
--
-- The default schema is used.
--
-- The idea is that the vega-cli routines [1] can be used to convert the
-- specification to PNG or SVG, the the HTML version provides a test
-- case (and also access to the Vega Editor).
--
-- [1] https://vega.github.io/vega/usage/#cli
--
-- Of course, we now need to manage npm/node with nix
-- https://unix.stackexchange.com/questions/379842/how-to-install-npm-packages-in-nixos
--
-- Should add to default.nix
--
--     nix-shell -p nodejs
--     npm install vega-cli
--     npm install canvas
--     ./node_modules/.bin/vg2png stripplot.vg.json stripplot.png
--
-- but this fails. Unclear why, tickets suggest installing 'node-canvas',
-- which I did. Can run
--
--     ./node_modules/.bin/vg2svg -h stripplot.vg.json stripplot.svg
--
-- but the output is meaningless (no data), even when -b is given.

module Main where

import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Graphics.Vega.Tutorials.VegaLite as VL

import Control.Monad (forM_)
import Data.Aeson ((.=), object)
import Graphics.Vega.VegaLite hiding (name)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Prelude hiding (filter)

-- Could use TH to get at the tutorials, but for now hard code the names.

-- The simple example used in the README
--
carData :: VegaLite
carData =
  let cars =  dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []

      enc = encoding
            . position X [ PName "Horsepower", PmType Quantitative ]
            . position Y [ PName "Miles_per_Gallon", PmType Quantitative, PTitle "Miles per Gallon" ]
            . color [ MName "Origin" ]

      bkg = background "rgba(0, 0, 0, 0.05)"

  in toVegaLite [ bkg, cars, mark Circle [MTooltip TTEncoding], enc [] ]


-- The intro visualization used in the API documentation (not in
-- the tutorial).
--
-- How does Betelgeuse vary with time?
--
betelgeuse :: VegaLite
betelgeuse =
  let desc = "How has Betelgeuse's brightness varied, based on data collated by AAVSO (https://www.aavso.org/). You should also look at https://twitter.com/betelbot and https://github.com/hippke/betelbot. It was all the rage on social media at the start of 2020."

      titleStr = "Betelegeuse's magnitude measurements, collated by AAVSO"

      -- height and width of individual plots
      w = width 600
      h = height 150

      pos1Opts fld ttl = [PName fld, PmType Quantitative, PTitle ttl]
      x1Opts = pos1Opts "days" "Days since January 1, 2020"
      y1Opts = pos1Opts "magnitude" "Magnitude" ++ [PSort [Descending], yRange]
      yRange = PScale [SDomain (DNumbers [-1, 3])]

      filtOpts = [MName "filterName"]
      filtEnc = color (MLegend [ LTitle "Filter", LTitleFontSize 16, LLabelFontSize 14 ] : filtOpts)
                . shape filtOpts

      circle = mark Point [ MOpacity 0.5, MFilled False ]

      encOverview = encoding
                    . position X x1Opts
                    . position Y y1Opts
                    . filtEnc

      selName = "brush"
      pos2Opts fld = [PName fld, PmType Quantitative, PNoTitle,
                     PScale [SDomainOpt (DSelectionField selName fld)]]
      x2Opts = pos2Opts "days"
      y2Opts = pos2Opts "magnitude" ++ [PSort [Descending]]

      encDetail = encoding
                  . position X x2Opts
                  . position Y y2Opts
                  . filtEnc

      xlim = (Number (-220), Number 100)
      ylim = (Number (-0.5), Number 2.5)
      overview = asSpec [ w
                        , h
                        , encOverview []
                        , selection
                          . select selName Interval [ Encodings [ChX, ChY]
                                                    , SInitInterval (Just xlim) (Just ylim)
                                                    ]
                          $ []
                        , circle
                        ]

      detailPlot = asSpec [ w
                          , h
                          , encDetail []
                          , circle
                          ]

      headerOpts = [ HLabelFontSize 16
                   , HLabelAlign AlignRight
                   , HLabelAnchor AEnd
                   , HLabelPadding (-24)
                   , HNoTitle
                   , HLabelExpr "'Filter: ' + datum.label"
                   ]

      details = asSpec [ columns 1
                       , facetFlow [ FName "filterName"
                                   , FHeader headerOpts
                                   ]
                       , spacing 10
                       , specification detailPlot
                       ]

  in toVegaLite [ description desc
                , title titleStr [ TFontSize 18 ]
                , dataFromUrl "https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/data/betelgeuse-2020-03-19.json" []
                , transform
                  -- concentrate on the two filters with a reasonable number of points
                  . filter (FExpr "datum.filterName[0] === 'V'")
                  -- remove some "outliers"
                  . filter (FExpr "datum.magnitude < 4")
                  -- subtract Jan 1 2020 (start of day, hence the .0 rather than .5)
                  . calculateAs "datum.jd - 2458849.0" "days"
                  $ []
                , vConcat [overview, details]
                , configure
                  . configuration (Axis [ TitleFontWeight Normal, TitleFontSize 16, LabelFontSize 14 ])
                  $ []
                ]


-- Associate a name with each visualization
--
vl :: [(String, VegaLite)]
vl = [ ("api-betelgeuse", betelgeuse)
     , ("api-cardata", carData)
     , ("stripplot", VL.stripPlot)
     , ("stripplotwithbackground", VL.stripPlotWithBackground)
     , ("stripploty", VL.stripPlotY)
     , ("stripplotwithcolor", VL.stripPlotWithColor)
     , ("stripplotwithcolor2", VL.stripPlotWithColor2)
     , ("stripplotwithcolorordinal", VL.stripPlotWithColorOrdinal)
     , ("piechart", VL.pieChart)
     , ("piechartwithcounting", VL.pieChartWithCounting)
     , ("parallaxbreakdown", VL.parallaxBreakdown)
     , ("parallaxhistogram", VL.parallaxHistogram)
     , ("gmaghistogram", VL.gmagHistogram)
     , ("yloghistogram", VL.ylogHistogram)
     , ("gmaghistogramwithcolor", VL.gmagHistogramWithColor)
     , ("gmaglinewithcolor", VL.gmagLineWithColor)
     , ("yhistogram", VL.yHistogram)
     , ("starcount", VL.starCount)
     , ("starcount2", VL.starCount2)
     , ("densityparallax", VL.densityParallax)
     , ("densityparallaxgrouped", VL.densityParallaxGrouped)
     , ("pointplot", VL.pointPlot)
     , ("posplot", VL.posPlot)
     , ("skyplot", VL.skyPlot)
     , ("smallmultiples", VL.smallMultiples)
     , ("smallmultiples2", VL.smallMultiples2)
     , ("densitymultiples", VL.densityMultiples)
     , ("baseplot", VL.basePlot)
     , ("layeredplot", VL.layeredPlot)
     , ("layereddiversion", VL.layeredDiversion)
     , ("layeredcount", VL.layeredCount)
     , ("skyplotwithgraticules", VL.skyPlotWithGraticules)
     , ("concatenatedplot", VL.concatenatedPlot)
     , ("concatenatedplot2", VL.concatenatedPlot2)
     , ("concatenatedskyplot", VL.concatenatedSkyPlot)
     , ("repeatplot", VL.repeatPlot)
     , ("splomplot", VL.splomPlot)
     , ("choroplethlookuptogeo", VL.choroplethLookupToGeo)
     , ("choroplethlookupfromgeo", VL.choroplethLookupFromGeo)
     , ("singleselection", VL.singleSelection)
     , ("nearestselection", VL.nearestSelection)
     , ("multiselection", VL.multiSelection)
     , ("eventselection", VL.eventSelection)
     , ("intervalselection", VL.intervalSelection)
     , ("intervalselectiony", VL.intervalSelectionY)
     , ("transformselection", VL.transformSelection)
     , ("legendselection", VL.legendSelection)
     , ("widgetselection", VL.widgetSelection)
     , ("bindscales", VL.bindScales)
     , ("coordinatedviews", VL.coordinatedViews)
     , ("coordinatedviews2", VL.coordinatedViews2)
     , ("contextandfocus", VL.contextAndFocus)
     , ("crossfilter", VL.crossFilter)
     , ("loessexample", VL.loessExample)
     , ("regressionexample", VL.regressionExample)
     , ("errormanual", VL.errorManual)
     , ("errorauto", VL.errorAuto)
     , ("errorbars", VL.errorBars)
     , ("errorband", VL.errorBand)
     , ("errorbox", VL.errorBox)
     , ("comparingerrors", VL.comparingErrors)
     , ("combinedplot", VL.combinedPlot)
     , ("duplicateaxis", VL.duplicateAxis)
     , ("comparecounts", VL.compareCounts)
     , ("parallaxview", VL.parallaxView)
     , ("skyplotaitoff", VL.skyPlotAitoff)
     , ("clustercenters", VL.clusterCenters)
     ]


run :: FilePath -> IO ()
run outdir = forM_ vl $ \(name, spec) -> do
  putStrLn (" . " <> name)
  let htmlFile = prefix <> ".html"
      specFile = prefix <> ".vg.json"
      prefix = outdir </> name

      opts = object ["downloadFileName" .= name]

  toHtmlFileWith (Just opts) htmlFile spec
  BL.writeFile specFile (AP.encodePretty (fromVL spec))


usage :: IO ()
usage = do
  pName <- getProgName
  hPutStrLn stderr ("Usage: " <> pName <> " directory")
  exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    [dname] -> do
      createDirectoryIfMissing True dname
      run dname

    _ -> usage
