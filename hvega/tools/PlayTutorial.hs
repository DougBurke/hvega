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
import qualified Data.Text as T

import qualified Graphics.Vega.Tutorials.VegaLite as VL

import Control.Monad (forM_)
import Data.Aeson ((.=), object)
import Graphics.Vega.VegaLite hiding (name)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath ((</>), FilePath)
import System.IO (hPutStrLn, stderr)

-- Could use TH to get at the tutorials, but for now hard code the names.

-- Associate a name with each visualization
--
vl :: [(String, VegaLite)]
vl = [ ("stripplot", VL.stripPlot)
     , ("stripplotwithbackground", VL.stripPlotWithBackground)
     , ("stripploty", VL.stripPlotY)
     , ("stripplotwithcolor", VL.stripPlotWithColor)
     , ("stripplotwithcolorordinal", VL.stripPlotWithColorOrdinal)
     , ("parallaxhistogram", VL.parallaxHistogram)
     , ("gmaghistogram", VL.gmagHistogram)
     , ("yloghistogram", VL.ylogHistogram)
     , ("gmaghistogramwithcolor", VL.gmagHistogramWithColor)
     , ("gmaglinewithcolor", VL.gmagLineWithColor)
     , ("yhistogram", VL.yHistogram)
     , ("pointplot", VL.pointPlot)
     , ("posplot", VL.posPlot)
     , ("skyplot", VL.skyPlot)
     , ("smallmultiples", VL.smallMultiples)
     , ("smallmultiples2", VL.smallMultiples2)
     , ("baseplot", VL.basePlot)
     , ("layeredplot", VL.layeredPlot)
     , ("layereddiversion", VL.layeredDiversion)
     , ("concatenatedplot", VL.concatenatedPlot)
     , ("concatenatedplot2", VL.concatenatedPlot2)
     , ("repeatplot", VL.repeatPlot)
     , ("splomplot", VL.splomPlot)
     , ("singleselection", VL.singleSelection)
     , ("nearestselection", VL.nearestSelection)
     , ("multiselection", VL.multiSelection)
     , ("eventselection", VL.eventSelection)
     , ("intervalselection", VL.intervalSelection)
     , ("intervalselectiony", VL.intervalSelectionY)
     , ("transformselection", VL.transformSelection)
     , ("widgetselection", VL.widgetSelection)
     , ("bindscales", VL.bindScales)
     , ("coordinatedviews", VL.coordinatedViews)
     , ("coordinatedviews2", VL.coordinatedViews2)
     , ("contextandfocus", VL.contextAndFocus)
     , ("crossfilter", VL.crossFilter)
     , ("errormanual", VL.errorManual)
     , ("errorauto", VL.errorAuto)
     , ("errorbars", VL.errorBars)
     , ("errorband", VL.errorBand)
     , ("errorbox", VL.errorBox)
     , ("comparingerrors", VL.comparingErrors)
     , ("combinedplot", VL.combinedPlot)
     ]


run :: FilePath -> IO ()
run outdir = forM_ vl $ \(name, spec) -> do
  putStrLn (" . " <> name)
  let htmlFile = prefix <> ".html"
      specFile = prefix <> ".vg.json"
      prefix = outdir </> name

      opts = object [T.pack "downloadFileName" .= name]
      
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
