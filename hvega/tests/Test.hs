{-# LANGUAGE OverloadedStrings #-}

{-

Tests for hvega, using tasty-golden.

The idea is that we check the JSON serialization of a specification
against a previously-generated value, stored as a separate file. This
allows external checks of the spec (e.g. by validating against the
Vega-Lite schema), and tests in visualizers. That is, the main aim
is to perform a series of regression tests, rather than unit or
property tests. Each module - based on the Elm VegaLite tests -
provides a labelled-list of VegaLite specifications, as

  testSpecs :: [(String, VegaLite)]

and these labels are used as the file name of the "golden" file (with
a ".vl" suffix) in a sub-directory for the module.

To make the output easier to compare against, the Aeson pretty-printer
is used. This relies on the pretty-printer being deterministic (i.e.
the key/value pairs of dictionaries get displayed in the same order).

-}

import qualified Data.ByteString.Lazy.Char8 as BL8

import Data.Aeson.Encode.Pretty (encodePretty)

import System.FilePath ((</>), (<.>))

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Prelude hiding (filter, repeat)

import Graphics.Vega.VegaLite

-- Test specifications
--
-- The commented-out tests are those which we currently don't support
-- in hvega.
--
import qualified ColorTests as ColT
import qualified CompositeTests as CompT
import qualified ConditionalTests as CondT
import qualified ConfigTests as ConfT
import qualified DataTests as DT
import qualified FillStrokeTests as FST
import qualified GeoTests as GT
import qualified HyperlinkTests as HT
import qualified LegendTests as LT
import qualified NullTests as NT
import qualified PositionTests as PT
import qualified ProjectionTests as PjT
import qualified ScaleTests as ScT
import qualified ShapeTests as ShT
import qualified SortTests as SoT
import qualified TextFormatTests as TfT
import qualified TimeTests as TmT
import qualified TooltipTests as TT
import qualified TrailTests as TrT
import qualified ViewCompositionTests as VT
import qualified WindowTransformTests as WT

-- extend from the "tests" to gallery plots
import qualified Gallery.Advanced as GADV
import qualified Gallery.Area as GA
import qualified Gallery.Bar as GB
import qualified Gallery.Dist as GD
import qualified Gallery.Error as GE
import qualified Gallery.Facet as GF
import qualified Gallery.Geo as GG
import qualified Gallery.Interaction as GI
import qualified Gallery.Multi as GM
import qualified Gallery.Repeat as GR

-- The "golden" output is "tests/specs/<label>/<name>.vl"
-- where label can now itself contain sub-directories, so should
-- rework this.
--
toTest :: VegaLite -> IO BL8.ByteString
toTest = pure . encodePretty . fromVL

toFP :: String -> String -> FilePath
toFP label tname = "tests" </> "specs" </> label </> (tname <.> ".vl")

-- types don't nicely match up with toFP here, but FilePath and String
-- are aliases so it doesn't cause a problem yet.
--
gallery :: String -> String
gallery = ("gallery" </>)

toTests :: String -> String -> [(String, VegaLite)] -> TestTree
toTests lbl dname tests = testGroup lbl
  [ goldenVsString tname (toFP dname tname) (toTest tspec)
  | (tname, tspec) <- tests
  ]

goldenTests :: TestTree
goldenTests = testGroup "tests"
  [ toTests "Color" "color" ColT.testSpecs
  , toTests "Composite" "composite" CompT.testSpecs
  , toTests "Conditional" "conditional" CondT.testSpecs
  , toTests "Config" "config" ConfT.testSpecs
  , toTests "Data" "data" DT.testSpecs
  , toTests "FillStroke" "fillstroke" FST.testSpecs
  , toTests "Geo" "geo" GT.testSpecs
  , toTests "Hyperlink" "hyperlink" HT.testSpecs
  , toTests "Legend" "legend" LT.testSpecs
  , toTests "Null" "null" NT.testSpecs
  , toTests "Position" "position" PT.testSpecs
  , toTests "Projection" "projection" PjT.testSpecs
  , toTests "Scale" "scale" ScT.testSpecs
  , toTests "Shape" "shape" ShT.testSpecs
  , toTests "Sort" "sort" SoT.testSpecs
  , toTests "TextFormat" "textformat" TfT.testSpecs
  , toTests "Time" "time" TmT.testSpecs
  , toTests "Tooltip" "tooltip" TT.testSpecs
  , toTests "Trail" "trail" TrT.testSpecs
  , toTests "ViewComposition" "viewcomposition" VT.testSpecs
  , toTests "WindowTransform" "windowtransform" WT.testSpecs
  , toTests "GalleryAdvanced" (gallery "advanced") GADV.testSpecs
  , toTests "GalleryArea" (gallery "area") GA.testSpecs
  , toTests "GalleryBar" (gallery "bar") GB.testSpecs
  , toTests "GalleryDist" (gallery "dist") GD.testSpecs
  , toTests "GalleryError" (gallery "error") GE.testSpecs
  , toTests "GalleryFacet" (gallery "facet") GF.testSpecs
  , toTests "GalleryGeo" (gallery "geo") GG.testSpecs
  , toTests "GalleryInteraction" (gallery "interaction") GI.testSpecs
  , toTests "GalleryMulti" (gallery "multi") GM.testSpecs
  , toTests "GalleryRepeat" (gallery "repeat") GR.testSpecs
  ]

main :: IO ()
main = defaultMain goldenTests
