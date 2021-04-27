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

import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (Config(confCompare), encodePretty', defConfig)

import System.FilePath ((</>), (<.>))

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Prelude hiding (filter, repeat)

import Graphics.Vega.VegaLite

-- Test specifications
--
import qualified AxisTests as AxT
import qualified ColorTests as ColT
import qualified CompositeTests as CompT
import qualified ConditionalTests as CondT
import qualified ConfigTests as ConfT
import qualified DataTests as DT
import qualified EncodingTests
import qualified FillStrokeTests as FST
import qualified FilterTests
import qualified GeoTests as GT
import qualified HyperlinkTests as HT
import qualified ImageTests as ImT
import qualified InteractionTests as IT
import qualified LegendTests as LT
import qualified MarkTests
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
import qualified TransformTests
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
import qualified Gallery.Histogram as GalleryHistogram
import qualified Gallery.Interaction as GI
import qualified Gallery.Label as GLBL
import qualified Gallery.Layer as GLYR
import qualified Gallery.Line as GLN
import qualified Gallery.Multi as GM
import qualified Gallery.Repeat as GR
import qualified Gallery.Scatter as GS
import qualified Gallery.Table as GTBL


-- Ensure we have a repeatable ordering for the output.
-- Hopefully this is repeatable enough (brought on by
-- changes in hashable 0.3.1.0 but it makes sense to
-- do this here as I hadn't realised that the default
-- encodePretty didn't actually apply any sorting to
-- the keys).
--
encodePretty :: Value -> BL8.ByteString
encodePretty = encodePretty' config
  where
    config = defConfig { confCompare = compare }


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

-- these are placed in the "gallery" sub-directory
toGTests :: String -> String -> [(String, VegaLite)] -> TestTree
toGTests lbl dname = toTests lbl (gallery dname)


baseTests :: TestTree
baseTests = testGroup "base"
  [ toTests "Axis" "axis" AxT.testSpecs
  , toTests "Color" "color" ColT.testSpecs
  , toTests "Composite" "composite" CompT.testSpecs
  , toTests "Conditional" "conditional" CondT.testSpecs
  , toTests "Config" "config" ConfT.testSpecs
  , toTests "Data" "data" DT.testSpecs
  , toTests "Encoding" "encoding" EncodingTests.testSpecs
  , toTests "FillStroke" "fillstroke" FST.testSpecs
  , toTests "Filter" "filter" FilterTests.testSpecs
  , toTests "Geo" "geo" GT.testSpecs
  , toTests "Hyperlink" "hyperlink" HT.testSpecs
  , toTests "Image" "image" ImT.testSpecs
  , toTests "Interaction" "interaction" IT.testSpecs
  , toTests "Legend" "legend" LT.testSpecs
  , toTests "Mark" "mark" MarkTests.testSpecs
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
  , toTests "Transform" "transform" TransformTests.testSpecs
  , toTests "ViewComposition" "viewcomposition" VT.testSpecs
  , toTests "WindowTransform" "windowtransform" WT.testSpecs
  ]

galleryTests :: TestTree
galleryTests = testGroup "Gallery"
  [ toGTests "Advanced" "advanced" GADV.testSpecs
  , toGTests "Area" "area" GA.testSpecs
  , toGTests "Bar" "bar" GB.testSpecs
  , toGTests "Dist" "dist" GD.testSpecs
  , toGTests "Error" "error" GE.testSpecs
  , toGTests "Facet" "facet" GF.testSpecs
  , toGTests "Geo" "geo" GG.testSpecs
  , toGTests "Histogram" "histogram" GalleryHistogram.testSpecs
  , toGTests "Interaction" "interaction" GI.testSpecs
  , toGTests "Label" "label" GLBL.testSpecs
  , toGTests "Layer" "layer" GLYR.testSpecs
  , toGTests "Line" "line" GLN.testSpecs
  , toGTests "Multi" "multi" GM.testSpecs
  , toGTests "Repeat" "repeat" GR.testSpecs
  , toGTests "Scatter" "scatter" GS.testSpecs
  , toGTests "Table" "table" GTBL.testSpecs
  ]

allTests :: TestTree
allTests = testGroup "Vega-Lite" [baseTests, galleryTests]

main :: IO ()
main = defaultMain allTests
