{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.Tutorials.VegaLite
Copyright   : (c) Douglas Burke, 2019-2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

This tutorial is inspired by - in that it starts off as a close copy of - the
<https://github.com/gicentre/elm-vegalite/tree/master/docs/walkthrough Elm Vega-Lite walkthrough>
created by Jo Wood, and
converted as necessary for the differences between @hvega@ and
<https://package.elm-lang.org/packages/gicentre/elm-vegalite/latest elm-vegalite>.
The Elm tutorial is based on the talk given by
<https://youtu.be/9uaHRWj04D4 Wongsuphasawat et al at the 2017 Open Vis Conf>.

The tutorial targets version 4 of the Vega-Lite specification and
the functionality provided in version @0.12.0.0@ of hvega (although
a number of examples could be simplified by removing the
now-optional type information as of Vega-Lite 4.14).

-}

module Graphics.Vega.Tutorials.VegaLite (

  -- * A Grammar of Graphics
  --
  -- $intro

  -- ** How many Haskell extensions do you need?
  --
  -- $intro-extensions

  -- ** A note on type safety
  --
  -- $intro-type-safety

  -- ** Comparing hvega to Elm Vega-Lite
  --
  -- $compare-to-elm

  -- * What data are we using?
  --
  -- $datasource

  -- * Creating the Vega-Lite visualization
  --
  -- $output

  -- * A Strip Plot
  --
  -- $singleview-stripplot

  -- ** Our first hvega plot
  --
  stripPlot

  -- ** Backgrounds
  --
  -- $background-note

  , stripPlotWithBackground

  -- ** Challenging the primacy of the x axis
  --
  , stripPlotY

  -- ** Data sources
  --

  , gaiaData

  -- ** Adding color as an encoding

  , stripPlotWithColor
  , stripPlotWithColor2

  -- ** Comparing Ordinal with Nominal data types
  --
  -- $stripplot-mmtype

  , stripPlotWithColorOrdinal

  -- * A Pie Chart
  --
  -- $pie-chart

  , pieChart
  , pieChartWithCounting

  -- * Adding an axis
  --
  -- $add-axis

  , parallaxBreakdown

  -- ** Creating a value to plot: aggregating data
  --
  -- $histogram

  , simpleHistogram
  , parallaxHistogram
  , gmagHistogram

  -- ** Changing the scale of an axis
  --
  -- $ylog-histogram

  , ylogHistogram

  -- ** Stacked Histogram

  , gmagHistogramWithColor
  , gmagLineWithColor

  -- ** You don't have to just count
  --
  -- $histogramChoice

  , yHistogram

  , starCount
  , starCount2

  , densityParallax
  , densityParallaxGrouped

  -- ** Plotting with points
  --
  -- $intro-points

  , pointPlot

  -- * Making a map
  --
  -- $intro-map

  , posPlot

  -- ** Using a projection

  , skyPlot

  -- ** Choropleth with joined data

  -- $intro-choropleth

  , choroplethLookupToGeo

  -- * Layered and Multi-View Compositions
  --
  -- $intro-layered

  , smallMultiples
  , smallMultiples2

  , densityMultiples

  -- ** One plot, two plot, red plot, blue plot
  --
  -- $intro-multiplot

  , basePlot

  -- ** Composing layers

  , layeredPlot
  , layeredDiversion
  , layeredCount
  , skyPlotWithGraticules

  -- ** Concatenating views

  , concatenatedPlot
  , concatenatedPlot2
  , concatenatedSkyPlot

  -- ** Repeated views
  --
  -- $intro-repeat

  -- *** Varying fields field

  , repeatPlot

  -- *** Repeating Choropleths

  , choroplethLookupFromGeo

  -- *** Rows and Columns
  , splomPlot

  -- * Interactivity
  --
  -- $intro-interactivity

  , selectionProperties
  , singleSelection
  , nearestSelection
  , multiSelection
  , eventSelection
  , intervalSelection
  , intervalSelectionY

  -- ** Selection Transformations
  --
  -- $intro-selection-transforms

  , transformSelection

  -- *** Selection and bindings
  --
  -- $intro-selection-binding

  , legendSelection
  , widgetSelection
  , bindScales

  -- *** Multiple Coordinated Views
  --
  -- $intro-coordinated-views

  , coordinatedViews
  , coordinatedViews2

  , contextAndFocus

  -- *** Cross-filtering
  --
  -- $intro-crossfilter

  , crossFilter

  -- * Smoothing and Regressing
  --
  -- $intro-smoothing

  , loessExample
  , regressionExample

  -- * Errors: lines, bars, bands, and boxes
  --
  -- $intro-error

  , errorManual
  , errorAuto

  , errorBars
  , errorBand
  , errorBox

  , comparingErrors

  -- * Dashboard-esque
  --
  -- $intro-dashboard

  , combinedPlot

  -- * The end
  --
  -- $otherstuff

  , duplicateAxis
  , compareCounts
  , parallaxView

  -- ** Aitoff projections
  --
  -- $aitoff

  , skyPlotAitoff
  , clusterCenters

  ) where

import qualified Data.Text as T

import Prelude hiding (filter, lookup, repeat)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Graphics.Vega.VegaLite

-- $intro
-- hvega is a wrapper for the [Vega-Lite visualization
-- grammar](https://vega.github.io/) which itself is based on Leland
-- Wilkinson's [Grammar of
-- Graphics](http://www.springer.com/gb/book/9780387245447).  The
-- grammar provides an expressive way to define how data are
-- represented graphically. The seven key elements of the grammar as
-- represented in hvega and Vega-Lite are:
--
-- [@Data@]: The input to visualize. Example functions: 'dataFromUrl', 'dataFromColumns', and 'dataFromRows'.
--
-- [@Transform@]: Functions to change the data before they are visualized. Example functions: 'filter', 'calculateAs', 'binAs', 'pivot', 'density', and
-- 'regression'. These functions are combined with 'transform'.
--
-- [@Projection@]: The mapping of 3d global geospatial locations onto a 2d plane . Example function: 'projection'.
--
-- [@Mark@]: The visual symbol, or symbols, that represent the data. Example types, used with 'mark': 'Line', 'Circle', 'Bar', 'Text', and 'Geoshape'. There are also ways to specify the shape to use for the 'Point' type, using the 'MShape' setting and the 'Symbol' type.
--
-- [@Encoding@]: The specification of which data elements are mapped to which mark characteristics (commonly known as channels). Example functions: 'position', 'shape', 'size', and 'color'. These encodings are combined with 'encoding'.
--
-- [@Scale@]: Descriptions of the way encoded marks represent the data. Example settings: 'SDomain', 'SPadding', and 'SInterpolate'.
--
-- [@Guides@]: Supplementary visual elements that support interpreting the visualization. Example setings: 'AxDomain' (for position encodings) and 'LeTitleColor' (for legend color, size, and shape encodings).
--
-- In common with other languages that build upon a grammar of graphics
-- such as D3 and Vega, this grammar allows fine grain control of
-- visualization design. Unlike those languages, Vega-Lite - and
-- hvega in turn - provide practical default specifications for most of the
-- grammar, allowing for a much more compact high-level form of
-- expression.
--
-- The [Vega-Lite Example Gallery](https://vega.github.io/vega-lite/examples/)
-- provides a large-number of example visualizations that show off
-- the capabilities of Vega-Lite. Hopefully, by the end of this
-- tutorial, you will be able to create most of them.

-- $intro-extensions
-- The 'Graphics.Vega.VegaLite' module exports a large number of symbols,
-- but does not use any complex type machinery, and so it can be loaded
-- without any extensions, although the extensive use of the 'Data.Text.Text'
-- type means that using the @OverloadedStrings@ extension is __strongly__
-- advised.
--
-- The module does export several types that conflict with the Prelude,
-- so one suggestion is to use
--
--     > import Prelude hiding (filter, lookup, repeat)

-- $intro-type-safety
-- The interface provided by @hvega@ provides __limited__ type safety. Various
-- fields such as 'PmType' are limited by the type of the argument (in this
-- case 'Measurement'), but there's no support to check that the type makes
-- sense for the particular column (as @hvega@ itself does not inspect the
-- data source). Similarly, @hvega@ does not stop you from defining
-- properties that are not valid for a given situation - for instance
-- you can say @'toVegaLite' []@ even though the output is not a
-- valid Vega-Lite specification (i.e. it does not validate against
-- the [Vega-Lite schema](https://github.com/vega/schema)).
--
-- Version @0.5.0.0@ did add some type safety for a number of functions -
-- primarily 'encoding' and 'transform' - as the types they accept
-- have been restricted (to @['EncodingSpec']@ and @['TransformSpec']@
-- respectively), so that they can not be accidentally combined.

-- $compare-to-elm
-- @hvega@ started out as a direct copy of
-- [elm-vegalite](https://package.elm-lang.org/packages/gicentre/elm-vegalite/latest),
-- and has been updated to try and match the functionality of that package.
-- However, @hvega@ has not (yet?) followed @elm-vegalite@ into using
-- functions rather than data structures to define the options: for
-- example, @elm-vegalite@ provides @pQuant n@ which in @hvega@ is the
-- combination of @'PName' n@ and @'PmType' 'Quantitative'@ in @hvega@.
-- The top-level functions - such as 'dataFromUrl', 'encoding', and
-- 'filter' - are generally the same. As the VegaLite schema has expanded
-- over time the differences between the two approaches has also grown.
--
-- Version @0.5.0.0@ does introduce more-significant changes, in that
-- there are now separate types for a number of functions - such as
-- 'encoding', 'transform', and 'select' - to help reduce the
-- chance of creating invalid visualizations.

-- $datasource
-- Rather than use the Seattle weather dataset, used in the Elm walkthrough
-- (if you go through the [Vega-Lite Example Gallery](https://vega.github.io/vega-lite/examples/)
-- you may also want to look at different data ;-), I am going to use a
-- small datset from the [Gaia satellite](http://sci.esa.int/gaia/),
-- which has - and still is, as of early 2020 - radically-improved our knowledge
-- of our Galaxy. The data itself is from the paper
-- \"Gaia Data Release 2: Observational Hertzsprung-Russell diagrams\"
-- [(preprint on arXiV)](https://arxiv.org/abs/1804.09378)
-- [(NASA ADS link)](https://ui.adsabs.harvard.edu/#abs/arXiv:1804.09378).
-- We are going to use Table 1a, which was downloaded from the
-- [VizieR archive](http://vizier.u-strasbg.fr/viz-bin/VizieR-3?-source=J/A%2bA/616/A10/tablea1a)
-- as a tab-separated file (aka 'TSV' format).
--
-- The file contains basic measurements for a number of stars in
-- nine open clusters that all lie within 250 parsecs of the Earth
-- (please note, a parsec is a measure of distance, not time, no matter
-- what [some ruggedly-handsome ex-carpenter](https://en.wikipedia.org/wiki/Han_Solo)
-- might claim). The downloaded file is called
-- @gaia-aa-616-a10-table1a.no-header.tsv@, although I have
-- manually edited it to a \"more standard\" TSV form (we Astronomers like
-- our metadata, and tend to stick it in inappropriate places, such as the
-- start of comma- and tab-separated files, which really mucks up
-- other-people's parsing code). The first few rows in the file are:
--
--
-- +-------------------+---------+-----------+-----------+--------+--------+-------+
-- | Source            | Cluster | RA_ICRS   | DE_ICRS   | Gmag   | plx    | e_plx |
-- +===================+=========+===========+===========+========+========+=======+
-- | 49520255665123328 | Hyades  | 064.87461 | +21.75372 | 12.861 | 20.866 | 0.033 |
-- +-------------------+---------+-----------+-----------+--------+--------+-------+
-- | 49729231594420096 | Hyades  | 060.20378 | +18.19388 | 5.790  | 21.789 | 0.045 |
-- +-------------------+---------+-----------+-----------+--------+--------+-------+
-- | 51383893515451392 | Hyades  | 059.80696 | +20.42805 | 12.570 | 22.737 | 0.006 |
-- +-------------------+---------+-----------+-----------+--------+--------+-------+
-- | ...               | ...     | ...       | ...       | ...    | ...    | ...   |
-- +-------------------+---------+-----------+-----------+--------+--------+-------+
--
-- The @Source@ column is a numeric identifier for the star in the Gaia database,
-- in this particular case the [\"DR2\" release](https://www.cosmos.esa.int/web/gaia/dr2),
-- the @Cluster@ column tells us which [Star Cluster](https://en.wikipedia.org/wiki/Star_cluster)
-- the star belongs to, @RA_ICRS@ and @DE_ICRS@
-- [locate the star on the sky](https://en.wikipedia.org/wiki/Celestial_coordinate_system)
-- and use the [Equatorial coordinate system](https://en.wikipedia.org/wiki/Equatorial_coordinate_system)
-- (the @ICRS@ term has a meaning too, but it isn't important for our
-- purposes),
-- @Gmag@ measues the "brightness" of the star (as in most-things Astronomical,
-- this is not as obvious as you might think, as I'll go into below),
-- and the @plx@ and @e_plx@ columns give the measured
-- [parallax of the star](https://en.wikipedia.org/wiki/Equatorial_coordinate_system)
-- and its error value, in units of
-- milli [arcseconds](https://en.wikipedia.org/wiki/Minute_and_second_of_arc).
-- And yes, I do realise after complaining about popular-culture references
-- confusing distances and time, I am now measuring distances with angles.
-- I think I've already mentioned that Astronomy is confusing...

-- $output
-- The function 'toVegaLite' takes a list of grammar specifications,
-- as will be shown in the examples below, and creates a single JSON object
-- that encodes the entire design. As of @hvega-0.5.0.0@ this targets
-- version 4 of the Vega-Lite schema, but this can be over-ridden with
-- 'toVegaLiteSchema' if needed (although note that this just changes the
-- version number in the schema field, it does not change the output to
-- match a given version).
--
-- There is no concept of ordering to these specification lists, in that
-- @[ dataFromUrl ..., encoding ..., mark ...]@;
-- @[ encoding ..., dataFromUrl ..., mark ... ]@;
-- and
-- @[ encoding ..., mark ..., dataFromUrl ... ]@
-- would all result in the same visualization.
--
-- The output of 'toVegaLite' can be sent to the Vega-Lite runtime to
-- generate the Canvas or SVG output. @hvega@ contains the helper
-- routines:
--
--  * 'fromVL', which is used to extract the JSON contents from 'VegaLite'
--    and create an Aeson 'Data.Aeson.Value';
--
--  * 'toHtml', which creates a HTML page which uses the
--    <https://github.com/vega/vega-embed Vega Embed> Javascript
--    library to display the Vega-Lite visualization;
--
--  * and 'toHtmlFile', which is like 'toHtml' but writes the output
--    to a file.

-- $singleview-stripplot
-- In this section we shall concentrate on creating a single
-- plot. Later on we shall try combining plots, after branching
-- out to explore some of the different ways to visualize
-- multi-dimensional data sets.
--
-- In the examples I link to symbols that have not been used in
-- previous visualizations, to make it easier to see the use
-- of new functionality.

{-|

We could encode one of the numeric data fields as a strip plot where
the horizontal position of a tick mark is determined by the value
of the data item. In this case I am going to pick the "@plx@" column:

<<images/vl/stripplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4IgtghgTg1iBcIAuBLAxnANCAJhJECoArlADYIgAWSSADgM7wD0zUEA7gHQDmKSVYgCNiDAKZQ0AewB2SMXK7SwzACJTiPAEKkYY5lQBuYnhGaQG8qAeOnmeAs1MoIAWghuAbAEZP77wAMrgRCZGLeEFwyUq5UYhA4ElxIDIYg2ABmUlCQSETIAJ50YpQpaQC+5dgAJAxocZCUNPRMrLaRfALCXChSzHUNZu2uZPz6hgAsXABWDLLpIArSOCgyPPkAHvkZKGJkOJR0ZFvYSEUliACOxBBy-PgoxiCV5UA Open this visualization in the Vega Editor>

@
'toVegaLite'
    [ 'dataFromUrl' "https:\/\/raw.githubusercontent.com\/DougBurke\/hvega\/master\/hvega\/data\/gaia-aa-616-a10-table1a.no-header.tsv" ['TSV']
    , 'mark' 'Tick' []
    , 'encoding' ('position' 'X' [ 'PName' "plx", 'PmType' 'Quantitative' ] [])
    ]
@

Notice how there is __no explicit definition__ of the axis details,
color choice or size. These can be customised, as shown in examples
below, but the default values are designed to follow good practice in
visualization design.

Three grammar elements are represented by the three functions
'dataFromUrl', 'mark', and 'encoding'.

The 'encoding' function takes as a single parameter, a list of
specifications that are themselves generated by other functions. In
this case we use the function 'position' to provide an encoding of the
@\"plx\"@ field as the x-position in our plot. The precise way in which
the data value (parallax) is mapped to the x-position will depend on the type of
data we are encoding. We can provide a hint by declaring the
measurement type of the data field, here 'Quantitative' indicating a
numeric measurement type. The final parameter of position is a list of
any additional encodings in our specification. Here, with only one
encoding, we provide an empty list.

As we build up more complex visualizations we will use many more
encodings. To keep the coding clear, the idiomatic way to do this with
hvega is to chain encoding functions using point-free
style. The example above coded in this way would be

@
let enc = encoding
            . position X [ PName "plx", PmType Quantitative ]

in toVegaLite
    [ dataFromUrl "https:\/\/raw.githubusercontent.com\/DougBurke\/hvega\/master\/hvega\/data\/gaia-aa-616-a10-table1a.no-header.tsv" [TSV]
    , mark Tick []
    , enc []
    ]
@

-}

stripPlot :: VegaLite
stripPlot = toVegaLite
    [ dataFromUrl "https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/data/gaia-aa-616-a10-table1a.no-header.tsv" [TSV]
    , mark Tick []
    , encoding (position X [ PName "plx", PmType Quantitative ] [])
    ]

-- $background-note
-- The default background color for the visualization, at least in the
-- Vega-Embed PNG and SVG output, is white (in Vega-Lite version 4;
-- prior to this it was transparent). In many cases this is
-- perfectly fine, but an explicit color can be specified using the
-- 'BackgroundStyle' configuration option, as shown here, or with the
-- 'background' function, which is used in the choropleth examples
-- below ('choroplethLookupToGeo').

{-|

The 'configure' function allows a large number of configuration
options to be configured, each one introduced by the
'configuration' function. Here I set the color to be a light gray
(actually a very-transparent black; the 'Color' type describes the
various supported color specifications, but it is generally safe to assume
that if you can use it in HTML then you can use it here).

<<images/vl/stripplotwithbackground.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4Igxg9gdgZglgcxALlAIwIZgNYIE4QCuUAJiiHgpgBQAMANAAQPNO0B0AjAJQgC+9EAFsMebOQAucHCEEkMEjClCE8AG3IALCRIAOAZ2QB6I3gwB3dgjgTNhNIX0BTPJCgSn79pCFGAIkQIAEKq2E5GmgBuTggYRiL6HngR0bFG8opGsXAYALQYeQBsnIX5nLS5imhqTpwY7FAQuZpOGCQu7BL6kbIgMBB4IhLKIBIAnrpOkt38AiAAJPpgLSJaOgbGRqn11rb27HAQRksrcdu5ajbhkQAs7ABW+tC9npAkcFBIqCAAHiPwTjUZGQIF0aj+gnGk3IAEdCBh3DYFHBorM+EA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"plx\", PmType Quantitative ]

    conf = 'configure'
             . 'configuration' ('BackgroundStyle' "rgba(0, 0, 0, 0.1)")

in toVegaLite
    [ dataFromUrl \"https:\/\/raw.githubusercontent.com\/DougBurke\/hvega\/master\/hvega\/data\/gaia-aa-616-a10-table1a.no-header.tsv\" [TSV]
    , mark Tick []
    , enc []
    , conf []
    ]
@

If you want a transparent background (as was the default with Vega-Lite 3
and earlier), you would use

@
'configuration' ('BackgroundStyle' "rgba(0, 0, 0, 0)")
@

-}

stripPlotWithBackground :: VegaLite
stripPlotWithBackground = toVegaLite
    [ dataFromUrl "https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/data/gaia-aa-616-a10-table1a.no-header.tsv" [TSV]
    , mark Tick []
    , encoding (position X [ PName "plx", PmType Quantitative ] [])
    , configure (configuration (BackgroundStyle "rgba(0, 0, 0, 0.1)") [])
    ]


{-|
There is nothing that forces us to use the x axis, so let's
try a vertical strip plot. To do so requires changing only
__one__ character in the specifiction, that is the first argument to
'position' is now 'Y' rather than 'X':

<<images/vl/stripploty.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4IgtghgTg1iBcIAuBLAxnANCAJhJECoArlADYIgAWSSADgM7wD0zUEA7gHQDmKSVYgCNiDAKZQ0AewB2SMXK7SwzACJTiPAEKkYY5lQBuYnhGaQG8qAeOnmeAs1MoIAWghuAbAEZP77wAMrgRCZGLeEFwyUq5UYhA4ElxIDIYg2ABmUlCQSETIAJ50YpQpaQC+5dgAJAxocZCUNPRMrLaRfALCXChSzHUNZu2uZPz6hgAsXABWDLLpIArSOCgyPPkF+RkoYmQ4lHRkAB4LSEUliACOxBBy-PgoxiCV5UA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position 'Y' [ PName "plx", PmType Quantitative ]

in toVegaLite
    [ dataFromUrl \"https:\/\/raw.githubusercontent.com\/DougBurke\/hvega\/master\/hvega\/data\/gaia-aa-616-a10-table1a.no-header.tsv\" [TSV]
    , mark Tick []
    , enc []
    ]
@
-}
stripPlotY :: VegaLite
stripPlotY = toVegaLite
    [ dataFromUrl "https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/data/gaia-aa-616-a10-table1a.no-header.tsv" [TSV]
    , mark Tick []
    , encoding (position Y [ PName "plx", PmType Quantitative ] [])
    ]


{-|
Since we are going to be using the same data source, let's define it here:

@
gaiaData =
  let addFormat n = (n, 'FoNumber')
      cols = [ \"RA_ICRS\", \"DE_ICRS\", \"Gmag\", \"plx\", \"e_plx\" ]
      opts = [ 'Parse' (map addFormat cols) ]

  in dataFromUrl "https:\/\/raw.githubusercontent.com\/DougBurke\/hvega\/master\/hvega\/data\/gaia-aa-616-a10-table1a.no-header.tsv" opts
@

The list argument to 'dataFromUrl' allows for some customisation of
the input data.  Previously I used @['TSV']@ to specify the data is in
tab-separated format, but it isn't actually needed here (since the
file name ends in \".tsv\"). However, I have now explicitly defined how
to parse the numeric columns using 'Parse': this is because the columns
are read in as strings for this file by default, which actually doesn't
cause any problems in most cases, but did cause me significant problems
at one point during the development of the tutorial! There is limited
to no feedback from the visualizer for cases like this (perhaps I should
have used the Javascript console), and I only realised the problem thanks
to the @Data Viewer@ tab in the
<https://vega.github.io/editor/#/ Vega Editor>
(after a
<https://twitter.com/pkgw/status/1167127390880968707 suggestion from a colleague>).

Data can also be defined algorithmically - using 'dataSequence' and
'dataSequenceAs' - or inline - with 'dataFromColumns' or
'dataFromRows' - or directly from JSON (as a 'Data.Aeson.Value') using
'dataFromJson'.

Examples showing 'dataFromColumns' are the 'pieChart' and 'skyPlotWithGraticules' plots,
but let's not peak ahead!

-}

gaiaData :: Data
gaiaData =
  let addFormat n = (n, FoNumber)
      cols = [ "RA_ICRS", "DE_ICRS", "Gmag", "plx", "e_plx" ]
      opts = [ Parse (map addFormat cols) ]

  in dataFromUrl "https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/data/gaia-aa-616-a10-table1a.no-header.tsv" opts


{-|

One question would be how the parallaxes vary by cluster: as parallax is measuring distance,
then are the clusters similar distances away from us, or is there a range of values? A
first look is to use another \"channel\" to represent (i.e. encode) the cluster:

<<images/vl/stripplotwithcolor.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4IgtghgTg1iBcIAuBLAxnANCAJhJECoArlADYIgAWSSADgM7wD0zUEA7gHQDmKSVYgCNiDAKZQ0AewB2SMXK7SwzACJTiPAEKkYY5lQBuYnhGaQG8qAeOnmeAs1MoIAWghuAbAEZP77wAMrgRCZGLeEFwyUq5UYhA4ElxIDIYg2ABmUlCQSEQgdNDi+QDikDyUMsRgQhLpIABKAIIA+gCSAMINAMqV1bVQ9XRkAB59NXXYqgCi7V29iFUTg9hiLcNji-11AL572AAkDGhxkJQ09EystpF8AsJcKFLMx6dmN65k-PqGACxcACsGLJ6gppDgUDIKvBQNIyNl8hkUGIyDhKB0yKIrPUkABPOhiSpSMCQiAUHbYTagJEotGIDY4-GExAAR2IEDk-HwKGM9QgIxQDHyqCQYUoAAVoGSyPyAAQACgsAEoQHs9kA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"plx\", PmType Quantitative, 'PAxis' [ 'AxTitle' \"Parallax (mas)\" ] ]
            . 'color' [ 'MName' \"Cluster\", MmType 'Nominal' ]

in toVegaLite
    [ gaiaData
    , mark Tick []
    , enc []
    ]
@

Now each tick mark is colored by the cluster, and a legend is automatically
added to indicate this mapping. Fortunately the number of clusters in the
sample is small enough to make this readable! The 'color' function has
added this mapping, just by giving the column to use (with 'MName') and
its type ('MmType'). The constructors generally begin with @P@ for
position and @M@ for mark, and as we'll see there are other property
types such as facet and text.

Vega-Lite supports several data types, represented
by the 'Measurement' type. We have already seen 'Quantitative', which
is used for numeric data, and here we use 'Nominal' for the clusters,
since they have no obvious ordering.

The labelling for the X axis has been tweaked using 'PAxis', in this
case the default value for the label (the column name) has been
over-ridden by an explicit value.

-}

stripPlotWithColor :: VegaLite
stripPlotWithColor =
  let enc = encoding
            . position X [ PName "plx", PmType Quantitative, PAxis [ AxTitle "Parallax (mas)" ] ]
            . color [ MName "Cluster", MmType Nominal ]

  in toVegaLite
     [ gaiaData
     , mark Tick []
     , enc []
     ]

{-|

As of Vega-Lite version 4.14 we can now drop the type information when
it can be inferred. I am a little hazy of the rules, so I am going to
include the information (as it also means I don't have to change
the existing code!). However, as an example, we don't need to
add the @MmType Nominal@ setting to the 'color' channel, since the
following creates the same visualization as 'stripPlotWithColor':

<https://vega.github.io/editor/#/url/vega-lite/N4IgtghgTg1iBcIAuBLAxnANCAJhJECoArlADYIgAWSSADgM7wD0zUEA7gHQDmKSVYgCNiDAKZQ0AewB2SMXK7SwzACJTiPAEKkYY5lQBuYnhGaQG8qAeOnmeAs1MoIAWghuAbAEZP77wAMrgRCZGLeEFwyUq5UYhA4ElxIDIYg2ABmUlCQSEQgdNDi+QDikDyUMsRgQhLpIABKAIIA+gCSAMINAMqV1bVQ9XRkAB59NXXYqgCi7V29iFUTg9hiLcNji-11AL572AAkDGhxkJQ09EystpF8AsJcKFLMx6dmN65k-PqGACxcACsGLJ6gppDgUDIKvBQNIyNl8hkUGIyDhKB0yKIrCAdthNqAkSi0YgNvVUEgwpQAArQCBkMgQEYAAgAFBYAJRkgCedDElAAjsQIHJ+PgUMYcXsgA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"plx\", PmType Quantitative, 'PTitle' \"Parallax (mas)\" ]
            . 'color' [ 'MName' \"Cluster\" ]

in toVegaLite
    [ gaiaData
    , mark Tick []
    , enc []
    ]
@

Note that as well as removing @MmType Nominal@ from the 'color' encoding, I have
switched to the 'PTitle' option (which is the same as @PAxis [AxTitle ...]@.

-}

stripPlotWithColor2 :: VegaLite
stripPlotWithColor2 =
  let enc = encoding
            . position X [ PName "plx", PmType Quantitative, PTitle "Parallax (mas)" ]
            . color [ MName "Cluster" ]

  in toVegaLite
     [ gaiaData
     , mark Tick []
     , enc []
     ]


-- $stripplot-mmtype
-- It is instructive to see what happens if we change the mark type for
-- the color encoding from 'Nominal' to 'Ordinal'.

{-|

<<images/vl/stripplotwithcolorordinal.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4IgtghgTg1iBcIAuBLAxnANCAJhJECoArlADYIgAWSSADgM7wD0zUEA7gHQDmKSVYgCNiDAKZQ0AewB2SMXK7SwzACJTiPAEKkYY5lQBuYnhGaQG8qAeOnmeAs1MoIAWghuAbAEZP77wAMrgRCZGLeEFwyUq5UYhA4ElxIDIYg2ABmUlCQSEQgdNDi+QDikDyUMsRgQhLpIABKAIIA+gCSAMINAMqV1bVQ9XRkAB59NXXYqgCi7V29iFUTg9hiLcNji-11AL572AAkDGhxkJQ09EystpF8AsJcKFLMx6dmN65k-PqGACxcACsGLJ6gppDgUDIKvBQNIyNl8hkUGIyDhKB0yKIrPUkABPOhiSjZCEyCAUHbYTagJEotGIDY4-GExAAR2IEDk-HwKGM9QgIxQDHyqCQYUoAAVoGSyPyAAQACgsAEoQHs9kA  Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"plx\", PmType Quantitative, PAxis [ AxTitle \"Parallax (mas)\" ] ]
            . color [ MName \"Cluster\", MmType 'Ordinal' ]

in toVegaLite
    [ gaiaData
    , mark Tick []
    , enc []
    ]
@

As can be seen, the choice of color scale has changed to one more
appropriate for an ordered set of values.
-}
stripPlotWithColorOrdinal :: VegaLite
stripPlotWithColorOrdinal =
  let enc = encoding
            . position X [ PName "plx", PmType Quantitative, PAxis [ AxTitle "Parallax (mas)" ] ]
            . color [ MName "Cluster", MmType Ordinal ]

  in toVegaLite
     [ gaiaData
     , mark Tick []
     , enc []
     ]


-- $pie-chart
-- Before adding a second axis, let's temporarily look at another
-- \"one dimensiona" chart, namely the humble pie chart.
-- The 'Arc' mark type allows you to create pie charts, as well as more
-- complex visualizations which we won't discuss further in this
-- tutorial.


{-|

In this example we embed the data for the pie chart - namely the number
of stars per cluster - in the vsualization itself (using
'dataFromColumns' to create column data labelled \"cluster\" and
\"count\"). The 'position' encoding is set to 'Theta', which is
given the star counts, and the 'color' is set to the
Cluster name.

<<images/vl/piechart.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4IgtghgTg1iBcJoGMQBoQBMIBcINADcIAbAVwFMBnBAbVGXKpwqgSRIAcALCAAgAKrdCGQB7MgDscCAOwAWAAwBfNAyYs2iAEIkIk8XwCMI8VJnx5ADgCcq9WWbDEAYTGQ+24RjPSERgFYAZntRDWcQAAkATwhMalMJP3gAwNDGR012AEkXPgAmIJsTHySLIPyA9PCtEFyCgDZFfMTzBHkbfOrMiIA5AHE8-PlU1uSlFTUwntqBEgoASziE0rb4IwqG7qdZqAhqCk4KMYsbIKtlAF1VEAASKmRuCkh2bhwcTip4AHpvwgoAOYQAB0AIWOG4ZAARsCFmJvg8npA-oCIABaEjgih-eTAgBWVDEkhEFAMYkwC0kAIIIAhFDwNIAZgsKCRMOxfDIMDhokd2ABHMj6HDg3ALf4gG7iEhiLSgZms9mIDI7EQ8vmISTuSmkSXKZRAA Open this visualization in the Vega Editor>

@
let manualData = 'dataFromColumns' []
                 . 'dataColumn' "cluster" ('Strings' clusters)
                 . dataColumn "count" ('Numbers' counts)
                 $ []

    clusters = [ \"alpha Per\", \"Blanco 1\", \"Coma Ber\", \"Hyades\", \"IC 2391\"
               , \"IC 2602\", \"NGC 2451\", \"Pleiades\", \"Praesepe\"]
    counts = [ 740, 489, 153, 515, 325, 492, 400, 1326, 938]

    enc = encoding
          . position 'Theta' [PName "count", PmType Quantitative]
          . color [MName "cluster", MmType Nominal]

in toVegaLite
   [ manualData
   , mark 'Arc' []
   , enc []
   ]
@

-}

pieChart :: VegaLite
pieChart =
  let manualData = dataFromColumns []
                   . dataColumn "cluster" (Strings clusters)
                   . dataColumn "count" (Numbers counts)
                   $ []

      clusters = [ "alpha Per", "Blanco 1", "Coma Ber", "Hyades", "IC 2391"
                 , "IC 2602", "NGC 2451", "Pleiades", "Praesepe"]
      counts = [ 740, 489, 153, 515, 325, 492, 400, 1326, 938]

      enc = encoding
            . position Theta [PName "count", PmType Quantitative]
            . color [MName "cluster", MmType Nominal]

  in toVegaLite
     [ manualData
     , mark Arc []
     , enc []
     ]


{-|

There are three main changes to 'pieChart':

 - 'MInnerRadius' is used to impose a minimum radius on the pie slices
   (so leaving a hole in the center);

 - the 'ViewStyle' configuration is used to turn off the plot edge;

 - and the count value is calculated automatically by the 'PAggregate'
   method (summing over the \"Cluster\" column), rather than having a
   hand-generated table of values encoded in the visualization.

<<images/vl/piechartwithcounting.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4Igxg9gdgZglgcxALlANzgUwO4tAZwBcAnCAa0xSgFcAbWgXwYBoQBbAQ2LLxEIE8ADpWQguYEKzhQomYgCUOAEzjV8KAEwAGFiCUdCHXtWK0UIABaFCg-MgD094h2wA6BHEIXqAIzVzIKEJMINdINnsAEQhqBAAhEwp7CzRMBA57TiI5ZNT0+31De3S4DgBaDnKANgBGKoqarTLDH1pMGo5XKAgyi0xlOVdCfDRJEBgIYk5CXkEufBFQAHFOJFEaNh85MfkAQQB9AEkAYXkAZXMNreIxwVoAD0vqTe3WSIBRI9OL9efrscw+zuj1+LxuTF0ABJ8GA+pxzFYbHZHHlOh4vL5XHAIPYYXCMqiyrRPJh7GgACyuABW+GgAKgkBUUDWoC8mEMvA4CAQxDSBhE4BiQTGAmE5gAjtQOEFPAY4KkQLpILRJrx4JhaEpzMdaGpgjdWKKBd02NIOGYIUA Open this visualization in the Vega Editor> 

@
let enc = encoding
          . position Theta ['PAggregate' 'Count', PmType Quantitative]
          . color [MName "Cluster", MmType Nominal]

in toVegaLite
   [ gaiaData
   , mark Arc ['MInnerRadius' 20]
   , enc []
   , configure (configuration ('ViewStyle' ['ViewNoStroke']) [])
   ]
@

-}


pieChartWithCounting :: VegaLite
pieChartWithCounting =
  let enc = encoding
            . position Theta [PAggregate Count, PmType Quantitative]
            . color [MName "Cluster", MmType Nominal]

  in toVegaLite
     [ gaiaData
     , mark Arc [MInnerRadius 20]
     , enc []
     , configure (configuration (ViewStyle [ViewNoStroke]) [])
     ]


-- $add-axis
-- While the strip plot shows the range of parallaxes, it is hard to
-- make out the distribution of values, since the ticks overlap. Even
-- changing the opacity of the ticks - by adding an encoding channel
-- like @'opacity' [ 'MNumber' 0.6 ]@, or by setting the 'MOpacity'
-- property of the 'mark' - only helps so much. Adding a second
-- axis is easy to do, so let's see how the parallax distribution
-- varies with cluster membership.

{-|

The 'stripPlotWithColor' visualization can be changed to show two
variables just by adding a second 'position' declaration, which
shows that the 7 milli-arcsecond range is rather crowded:

<<images/vl/parallaxbreakdown.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAXAlgY2QGnCgBNZ1ZUxQIJIBXeAGwsgAt10AHAZxQHpf4sAO4A6AOaZ0LWgCNaXAKbxsAewB26BRpGrovACIraYgEL1EC3iwBuCsbF5wum+Fdv3eJMr3uZYAWlgAgDYARmDA0IAGfzIZBgVQ2BE1FX8WBVgiJRF0LmtIAmooADMVeDh0CipiqA4ERWrCWqgAcTgxZjVaaBklQubayAAlAEEAfQBJAGFhgGUunr74AZaaDgYAD0Xe-qK1yH0AUSnZhbRIbt2V-ZbIBXGN7Yur5chBiABfQe+v-cgACRcbAZODMNicHj8dzJCRSWQiTAqXjA0EOGH+BiSSzWAAsIgAVlx1KsoFpVERMGpOmgajRVAxytVICVMAoGERmNMGPIXIUMABPDgKLoqaBU2BMT63KDPSgfUpsjnMJ6k4qQdBCkUXACOtFgGkkpEwtjV1EgsE2mC4zKw6ASzAACghJQxLWAABROACUkF+1Glg0gAuZrPZnIu3N5e0FwtF4rUkr9hED9PUrJplG+nyAA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"plx\", PmType Quantitative, PAxis [ AxTitle \"Parallax (mas)\" ] ]
            . position Y [ PName \"Cluster\", PmType Nominal ]
            . color [ MName \"Cluster\", MmType Nominal ]

in toVegaLite
    [ gaiaData
    , mark Tick []
    , enc []
    ]
@

I have left the color-encoding in, as it makes it easier to compare to
'stripPlotWithColor', even though it replicates the information provided
by the position of the mark on the Y axis. The 'yHistogram' example
below shows how the legend can be removed from a visualization.

-}

parallaxBreakdown :: VegaLite
parallaxBreakdown =
  let enc = encoding
            . position X [ PName "plx", PmType Quantitative, PAxis [ AxTitle "Parallax (mas)" ] ]
            . position Y [ PName "Cluster", PmType Nominal ]
            . color [ MName "Cluster", MmType Nominal ]

  in toVegaLite
     [ gaiaData
     , mark Tick []
     , enc []
     ]


-- $histogram
-- We can also \"create\" data to be plotted, by aggregating data. In this
-- case we can create a histogram showing the number of stars with the
-- same parallax value (well, a range of parallaxes).

{-|

Since sensible (hopefully) defaults are provided for unspecified settings, it
is relatively easy to write generic representations of a particular
visualization. The following function expands upon the previous
specifications by:

 * taking a field name, rather than hard coding it;

 * the use of @'PBin' []@ to ask for the x-axis values to be binned;

 * the addition of a second axis ('Y') which is used for the
   aggregated value ('Count', which means that no column has
   to be specified with @PName@);

 * and the change from 'Tick' to 'Bar' for the 'mark'.

Note that we did not have to specify how we wanted the histogram
calculation to proceed - e.g. the number of bins, the bin widths,
or edges - although we could have added this, by using a non-empty
list of 'BinProperty' values with 'PBin', if the defaults are not
sufficient.

@
simpleHistogram :: T.Text -> VegaLite
simpleHistogram field =
  let enc = encoding
              . position X [ PName field, PmType Quantitative, 'PBin' [] ]
              . position Y [ PAggregate Count, PmType Quantitative ]

  in toVegaLite
       [ gaiaData
       , mark 'Bar' []
       , enc []
       ]
@

-}

simpleHistogram :: T.Text -> VegaLite
simpleHistogram field =
  let enc = encoding
              . position X [ PName field, PmType Quantitative, PBin [] ]
              . position Y [ PAggregate Count, PmType Quantitative ]

  in toVegaLite
     [ gaiaData
     , mark Bar []
     , enc []
     ]

{-|

With 'simpleHistogram' it becomes easy to get a histogram of the parallax
values:

<<images/vl/parallaxhistogram.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAjBkA04oBNYAXWVMUCCSAV3gBszIALIogBwGcUB6b+WAO4A6AOYBLIk2rpqHAKbwAxgHsAdkTnqhK6NwAiy6iIBCtRHO5MAbnJGxucDhviWbd7oRLc7Y2AFpYfwA2AEYggJCABj8SdDo5ENghVWU-JjlYfAUhIg4rHDwqADNleDgiMgpKKjYEeUrC6qgAcTgRRlVqaHQFAqbKSAAlAEEAfQBJAGFBgGUOrp74Pv6oNjoAD3nu3twVqD0AUQnpubRITu2l3ZXIOVG1zbOLxchGiABfRs+P68gAEg4inScEYLHYXF4biS4kk0iEYmU3EBwPsUL8dAkFisABYhAArDhqZa3VQqfBiVTtNBVKiPciQIpiOR0fCMB44DAUshEeDUOTYKBEACebDkjAAjtRYOoJMQxDZIO9rlQhZVILARCJ4LZiGKzipqOoOZBhaKJVKZSQiPKxd8wEq8JAVKpGVTyJ93kA Open this visualization in the Vega Editor>

@parallaxHistogram = 'simpleHistogram' \"plx\"@

We can see that although parallaxes around 20 to 25 milli-arcseconds
dominated the earlier visualizations, such as 'stripPlotWithColor',
__most__ of the stars have a much-smalled parallax, with values
in the range 5 to 10.

-}

-- TODO XXX explain parallax

parallaxHistogram :: VegaLite
parallaxHistogram = simpleHistogram "plx"

{-|

A different column (or field) of the input data can be viewed,
just by changing the name in the specification:

<<images/vl/gmaghistogram.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAjBkA04oBNYAXWVMUCCSAV3gBszIALIogBwGcUB6b+WAO4A6AOYBLIk2rpqHAKbwAxgHsAdkTnqhK6NwAiy6iIBCtRHO5MAbnJGxucDhviWbd7oRLc7Y2AFpYfwA2AEYggJCABj8SdDo5ENghVWU-JjlYfAUhIg4rHDwqADNleDgiMgpKKjYEeUrC6qgAcTgRRlVqaHQFAqbKSAAlAEEAfQBJAGFBgGUOrp74Pv6oNjoAD3nu3twVqD0AUQnpubRITu2l3ZXIOVG1zbOLxchGiABfRs+P68gAEg4inScEYLHYXF4biS4kk0iEYmU3EBwPsUL8dAkFisABYhAArDhqZa3VQqfBiVTtNBVKiPciQIpiOR0fCMVqwdrYDAUshEeDUORcyBEACebDkjAAjtRYOoJMQxDZIO9rlQRZVIByRPBbMQJWcVNR1DgoKLxVKZXKSERFRLvmAVXhICpVIyqeRPu8gA Open this visualization in the Vega Editor>

@gmagHistogram = simpleHistogram \"Gmag\"@

Here we can see that the number of stars with a given magnitude
rises up until a value of around 18, and then drops off.

-}

-- TODO XXX explain magnitudes

gmagHistogram :: VegaLite
gmagHistogram = simpleHistogram "Gmag"



-- $ylog-histogram
-- In the case of 'parallaxHistogram', the data is __dominated__ by
-- stars with small parallaxes. Changing the scale of the
-- Y axis to use a logarithmic, rather than linear, scale /might/
-- provide more information:

{-|

<<images/vl/yloghistogram.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFALYCGATgNYZjCQDOK5A9pbDZIuaQJ6T5QAZvAA2Ijs24A7ROwGQUPAA7tMkAEYVIAX0IRIAE1IpSNUBH0BXcuLWoUS+ugD0z7gHcAdIngpol9Ut6WHIAY2YpFFhIz3DiZwARZktEACFrNmdoADdYRFJnMkYQrNz85yMTZ3z4UgBaUnqANgBGJoaWnDqTdRFYFtJPKWY6uFIDEM8Uemz+In1BSTI0THMLfSUKYLN59agAcTJEDilLYnUQub2LSAAlAEEAfQBJAGFbgGUTs4vyK+uoEoRAAPb7nS56AGQBIAURe7y+alO4L+kOukFgjyBoKRP0uuwg2l2RMJkMgABJ6KE4GQOPZHC5nGVBj4-AFPPBmM4qTSCsy6iJfLAmQAWTwAK3oEX+Cl8fQ4+1ItTAAAUKKQxKRgbB6DLouEDPAZDt1pAcbQCUJ4LARAYONj-qb1EaaExLLA0TdFCoOABHSykSK+YzwXKOm5a+C61aylDytRq7ia4FgAAURQAlDpiZ7IHxVpaGKENapaAplKXICJmMddIXSIguHljJXwpZIuH9N7K-7Ayhg-2w8SiHX9OEpMJjqsidogA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName "plx", PmType Quantitative, PBin [], PAxis [ AxTitle "Parallax (mas)" ] ]
            . position Y [ PAggregate Count, PmType Quantitative, 'PScale' [ 'SType' 'ScLog' ] ]

in toVegaLite
   [ gaiaData
   , mark Bar [ 'MFill' "orange", 'MStroke' "gray" ]
   , enc []
   , 'height' 300
   , 'title' "Gaia Parallaxes" []
   ]
@

There are four new changes to the visualization created by 'simpleHistogram' (since 'PAxis'
has been used above):

 1. an explicit choice of scaling for the Y channel (using 'PScale');

 2. the fill ('MFill') and edge ('MStroke') colors of the histogram bars are different;

 3. the height of the overall visualization has been increased;

 4. and a title has been added.

If you view this in the Vega Editor you will see the following warning:

@
A log scale is used to encode bar's y. This can be misleading as the height of the bar can be arbitrary based on the scale domain. You may want to use point mark instead.
@

-}
ylogHistogram :: VegaLite
ylogHistogram =
  let enc = encoding
              . position X [ PName "plx", PmType Quantitative, PBin [], PAxis [ AxTitle "Parallax (mas)" ] ]
              . position Y [ PAggregate Count, PmType Quantitative, PScale [ SType ScLog ] ]

  in toVegaLite
     [ gaiaData
     , mark Bar [ MFill "orange", MStroke "gray" ]
     , enc []
     , height 300
     , title "Gaia Parallaxes" []
     ]


{-|

A color encoding can also be added. When used with the 'Tick' mark -
'stripPlotWithColor' - the result was that each tick mark was colored
by the \"Cluster\" field, but for the 'Bar' mark the result is that
the bars are stacked together. I have also taken the opportunity to
widen the plot ('width'); define the binning scheme used, with @'Step'
1@; and configure the location of the x axis tick marks, using
'AxValues'.

<<images/vl/gmaghistogramwithcolor.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFALYCGATgNYZQBGFkhEkAJqSqTaBMwK7kAbGjBQoADgGd0AemnlSAdwB0ieCmi9avCbHIBjAPYA7FLBNLDxaQBEDvRACF+lWNOgA3WIlLSyE0+Runt7SbBzS3vCkALSkMQBsAIzxsYk40Ry0ArCJpEpGBtFwpCy6SigS7oxEzABmBuRkaJjcPMxiFDpcNW1QAOJkiMJGvMS0utW9PJAASgCCAPoAkgDCMwDKw6Pj5JNTUGICAB5bYxNM+5DWAKLLa5uYkCNnuxdTkLALhyePzzuQPQgAF8eiDgRdIAp4Cx1DQACx4CEAEgkejgZGEqHEUlkwTyqnUmiU8AM0lR6J8eOiAjUrnccKUACsJMY9h8jIYWPAjEMWj1IIYBA0uJBavBYAIWMIVgJtAFGFAUABPMSwYYGYjc0hCIFvKA-MCtNqi8WS4QDUhDPXMWjckX+WBiGiJXWAxUqtWPACOvFIJjU7Hgnj2xtIR3gEhF7m1vFgkcwAG0CGAAKz4MBpdOJNNgABMOAAumCeK7jUqRZbEOQvOxPVBDLwTArIMrVcIfX6UAGu8Hi6X68YxbzDSCgUA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"Gmag\", PmType Quantitative, binning, axis ]
            . position Y [ PAggregate Count, PmType Quantitative ]
            . color [ MName \"Cluster\", MmType Nominal ]

    binning = PBin [ 'Step' 1 ]
    axis = PAxis [ 'AxValues' (Numbers [ 0, 5 .. 20 ]) ]

in toVegaLite
   [ gaiaData
   , mark Bar []
   , enc []
   , height 300
   , 'width' 400
   ]
@

Note that @hvega@ will allow you to combine a 'color' encoding with a 'ScLog'
scale, even though a Vega-Lite viewer will not display the
resulting Vega-Lite specification, saying

@Cannot stack non-linear scale (log)@

-}
gmagHistogramWithColor :: VegaLite
gmagHistogramWithColor =
  let enc = encoding
              . position X [ PName "Gmag", PmType Quantitative, binning, axis ]
              . position Y [ PAggregate Count, PmType Quantitative ]
              . color [ MName "Cluster", MmType Nominal ]

      binning = PBin [ Step 1 ]
      axis = PAxis [ AxValues (Numbers [ 0, 5 .. 20 ]) ]

  in toVegaLite
     [ gaiaData
     , mark Bar []
     , enc []
     , height 300
     , width 400
     ]

{-|

Notice how we never needed to state explicitly that we wished our bars
to be stacked. This was reasoned directly by Vega-Lite based on the
combination of bar marks and color channel encoding. If we were to
change just the mark function from 'Bar' to 'Line', Vega-Lite produces an
unstacked series of lines, which makes sense because unlike bars,
lines do not occlude one another to the same extent.

<<images/vl/gmaglinewithcolor.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFALYCGATgNYZQA28AdrJIRJACakqk2gRsBXcrRowUKAA4BndAHpZ5UgHcAdIngpoAgEYCpscgGMA9gxSwzKk8VkARYwMQAhIZVizoAN1iJSsslLm5B7evrKc3LK+8KQAtKRxAGwAjInxyTix3Nq0sMmkKgzGsXCk7AYqKFKeLERsAGbG5GRomHz8bBIU+rx1HVAA4mSIogwCxNoGtf38kABKAIIA+gCSAMJzAMqj45Pk0zNQErQAHjsTU6yHkLYAoqsb25iQYxf7VzOQsEvHZ8+ve0gfQgAF8+mDQVdIEp4OxNDQACx4KEAEikhjgZFEqEkMnkoQK6k0OhU8GMsnRmL8BNi9HMsk8CJUACspKYDl8GCZ2IwRm0+pATLQmrxIPV4LBaOxRGtaHogiwoCgAJ4SZj-YzERikEQgj5QP5gdodMUSqWiIakEb6tjaRiiwKwCQ0ZJ64FK1XqqAARwEpDMGi48G8BxNpBO8Ckos8OoEsCjmAA2gQwABWfBgDIZ5LpsAAJhwAF0Ifw3SblaKrYhyD4uF7BQ4zIrICq1aJff6UIGuyGS2WoCYGOK+UawSCgA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"Gmag\", PmType Quantitative, binning, axis ]
            . position Y [ PAggregate Count, PmType Quantitative ]
            . color [ MName \"Cluster\", MmType Nominal ]

    binning = PBin [ Step 1 ]
    axis = PAxis [ AxValues (Numbers [ 0, 5 .. 20 ]) ]

in toVegaLite
   [ gaiaData
   , mark 'Line' []
   , enc []
   , height 300
   , width 400
   ]
@

-}

gmagLineWithColor :: VegaLite
gmagLineWithColor =
  let enc = encoding
              . position X [ PName "Gmag", PmType Quantitative, binning, axis ]
              . position Y [ PAggregate Count, PmType Quantitative ]
              . color [ MName "Cluster", MmType Nominal ]

      binning = PBin [ Step 1 ]
      axis = PAxis [ AxValues (Numbers [ 0, 5 .. 20 ]) ]

  in toVegaLite
     [ gaiaData
     , mark Line []
     , enc []
     , height 300
     , width 400
     ]


-- $histogramChoice
-- The previous histogram visualizations have taken advantage of Vega-Lite's
-- ability to bin up ('Count') a field, but there are a number of aggregation
-- properties (as defined by the 'Operation' type). For example, there
-- are a number of measures of the \"spread\" of a population, such as
-- the sample standard deviation ('Stdev').
--
-- You can also synthesize new data based on existing data, with the
-- 'transform' operation. Unlike the 'encoding' function, the order
-- of the arguments to 'transform' do matter, as they control the
-- data flow (e.g. you can not filter a data set if you have not
-- created the field to be filtered).

{-|
The aim for this visualization is to show the spread in the @Gmag@ field
for each cluster, so we now swap the axis on which the aggregate is
being applied (so that the cluster names appear on the y axis),
and hide the legend that is applied (using @'MLegend' []@) since
we can read off the color mapping from the y axis.

<<images/vl/yhistogram.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuMxIHsAOsDGAlgC4CeqYADAHQBsANFKdgKbmQBGCkAvneFABNYRWOVAQIkAK7wANmwAWRItgDOKAPQb4sAO5UA5sQVT2U1c3h5MAOyLM7Va9A0ARTFIMAhGYmYaFADdmA1gNOFV7eADg0I0hEQ1QglgAWlg0mgBGGnSsilSRdllmLNgqG0xUhWZYAUsqIlVAyD4JKAAzTHg4IjF+dshceAt+9sGAcTgDNhspaHZLVoHxyAAlAEEAfQBJAGE1gGVZ+cX4ZfGJIdkADxOFpbbLqFcAUV2D47RIOYfzp8ukGYW2wt3uZ0gKwg3BWMOhT0gABJVHganBFMo1JoNLFykYiCZ2FQCJgNCi0WFcalZMR-IEACxUABWqlsFygDmsAgINhmaHEV2ssm6YkgHQIzFkAjYe1k5iirUYJBYs0w0B5sHkDEgJQMDmlaDmslkvBWkDu-LFEqlbCmsBm2vtBngIWErG+kXqLW1THdUAAjlJYHZiMICMEeACoGRLeLJQaoLL5Y8lSrvpV1TZNTx+KbJNYbOK+egYdwgA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"Gmag\", PmType Quantitative, PAggregate 'Stdev' ]
            . position Y [ PName \"Cluster\", PmType Nominal ]
            . color [ MName \"Cluster\", MmType Nominal, 'MLegend' [] ]

in toVegaLite
   [ gaiaData
   , mark Bar [ 'MOpacity' 0.6 ]
   , enc []
   ]
@

The bar opacity is reduced slightly with 'MOpacity 0.6' so that the
x-axis grid lines are visible. An alternative would be to change the
'AxZIndex' value for the 'X' encoding so that it is drawn on top of
the bars.

-}

yHistogram :: VegaLite
yHistogram =
  let enc = encoding
              . position X [ PName "Gmag", PmType Quantitative, PAggregate Stdev ]
              . position Y [ PName "Cluster", PmType Nominal ]
              . color [ MName "Cluster", MmType Nominal, MLegend [] ]

  in toVegaLite
     [ gaiaData
     , mark Bar [ MOpacity 0.6 ]
     , enc []
     ]



{-|

Aggregation can happen in the position channel - as we've seen with
the 'PAggregate' option - or as a 'transform', where we create
new data to replace or augment the existing data. In the following
example I use the 'aggregate' transform to calculate the number of
rows in the original dataset per cluster with the
'Count' operation. This effectively replaces
the data, and creates a new one with the fields @\"Cluster\"@ and
@\"count\"@.

The other two major new items in this visualization are that the
X axis has been ordered to match the Y axis (using 'ByChannel' and
'PSort' in the 'position' encoding), and I have specified my own SVG
definition for the symbols with 'SymPath' and 'MShape'.

<<images/vl/starcount.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxhIBzWdAVwAcAjATz0MgGEAba5aAU1kgBdADRR45Sr3Lw+LAmXS0WkAMY1E0SKMjxkytdQ2QAvoOPERxSFniwA1i1AkoPKnd4B1AJYATaAAsWAEZhYghIZC8AL15ggAY40OcIuHR3ZXoOeBUHJJJIVC8ODmVGXmL0AHctMJd-eFpY-EgAWTBEgFogsAAZdoA6ACYAZmEOuKHh3rAQ8cnpieGADmEJwe6+iYA2QdX+gE4AFgW9w-3puZ29o4vFlbWNsC6xtam+uZGX+c2x7qia5LQRiNZS0dBeIzEYx5SA+GTwRy1SDUWAlZr+aDQWjIXAAelxCEq-XIXgC1Ho3H4ag0vA0-TUWFxABEaOQAEIo9y4-wANyk8FxNh4-G5fOkuLh0AF0i88A68DlWyCW3lQTiHSlmV4QXg-UQ6A6-l48B8-H60GQPIB+Qw2BkiOc4VotmQTVItWSAHEbORlIhqFh6PxrY6oAAlACCAH0AJJsMMAZT9AaDAjyocgtA4AA9k4Hg+nHZAmQBRWPxpPNf35tMe-K8KNZ3NVlPBuvmZwdsDQqwAEmQKiNNmUGKxOPxYt1JLJ9H6XnQuIHQ4Fk46HFJvFxPMO-QAVsh0IhrZBaWofBDffgnPlm6QCl5yj5lJxuHw0y5MJoryfEGeL6UTG0IEQSrdAsAheASh7ZJmCvOt70ffR1E0QtwmAt1IAAR2oJBoFJGQvD5EN8ngbMvD0b88OgDgMIAOVbWAwHQVAwB4F0TFqLtoKgalCkvUhzGMIA Open this visualization in the Vega Editor>

@
let trans = 'transform'
            . 'aggregate' [ 'opAs' 'Count' \"\" \"count\" ]
                        [ \"Cluster\" ]

    enc = encoding
          . position X [ PName \"Cluster\"
                       , PmType Nominal
                       , 'PSort' [ 'ByChannel' 'ChY' ]
                       ]
          . position Y [ PName \"count\"
                       , PmType Quantitative
                       , PAxis [ AxTitle \"Number of stars\" ]
                       ]

    star = 'SymPath' \"M 0,-1 L 0.23,-0.23 L 1,-0.23 L 0.38,0.21 L 0.62,0.94 L 0,0.49 L -0.62,0.94 L -0.38,0.21 L -1,-0.23 L -0.23,-0.23 L 0,-1 z\"

in toVegaLite [ gaiaData
              , trans []
              , enc []
              , mark 'Point' [ 'MShape' star
                           , MStroke \"black\"
                           , 'MStrokeWidth' 1
                           , MFill \"yellow\"
                           , 'MSize' 100
                           ]
              ]
@

Notes:

- the star design is based on a
<https://upload.wikimedia.org/wikipedia/commons/a/a5/Star_with_eyes.svg Wikipedia design>,
after some hacking and downsizing (such as losing the cute eyes);

- when using 'Count' with 'opAs', the first 'FieldName' argument is ignored,
so I set it to the empty string @\"\"@ (it's be great if the API were such
we didn't have to write dummy arguments, but at present @hvega@
doesn't provide this level of safety);

- although the order of operations of 'transform' is important, here
  I only have one (the 'aggregate' call);

- and the order of the arguments to 'toVegaLite' does not matter (so you
  can have the 'transform' appear before 'encoding' or after it).

-}

starCount :: VegaLite
starCount =
  let trans = transform
              . aggregate [ opAs Count "" "count" ]
                          [ "Cluster" ]

      enc = encoding
            . position X [ PName "Cluster"
                         , PmType Nominal
                         , PSort [ ByChannel ChY ]
                         ]
            . position Y [ PName "count"
                         , PmType Quantitative
                         , PAxis [ AxTitle "Number of stars" ]
                         ]

      -- based on https://upload.wikimedia.org/wikipedia/commons/a/a5/Star_with_eyes.svg
      star = SymPath "M 0,-1 L 0.23,-0.23 L 1,-0.23 L 0.38,0.21 L 0.62,0.94 L 0,0.49 L -0.62,0.94 L -0.38,0.21 L -1,-0.23 L -0.23,-0.23 L 0,-1 z"

  in toVegaLite [ gaiaData
                , trans []
                , enc []
                , mark Point [ MShape star
                             , MStroke "black"
                             , MStrokeWidth 1
                             , MFill "yellow"
                             , MSize 100
                             ]
                ]


{-|

I've shown that the number of stars per cluster increases when
ordered by increasing count of the number of stars per cluster,
which is perhaps not the most informative visualization. How about
if I ask if there's a correlation between number of stars and
distance to the cluster (under the assumption that objects further
away can be harder to detect, so there /might/ be some form of
correlation)?

To do this, I tweak 'starCount' so that we also calculate the
parallax to each cluster in the 'transform' - in this case taking
the median value of the distribution thanks to the 'Median' operation - and
then using this new field to order the X axis with 'ByFieldOp'. Since
parallax is inversely correlated with distance we use the
'Descending' option to ensure the clusters are drawn from near to
far. We can see that there is no obvious relation with distance.

<<images/vl/starcount2.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxQSTIBzWdAVwAcAjATz0MgGEAbW5aAU1iQAugBpi5SPErV+leALZFy5YJHT02kAMZ1E0SKKjxkWxLSyNBkAL7iVJNRq1Z+AEwCWSQ8dP5I9FwAHgD6rm4+kKge-FwR-oFBthIkwik2xGLEkFjwsADWbGQUfDT5-ADqHm7QABZsAIz2JR4AXvyNAAydzRCQpejlWoxc8NqFvVDRXFxazLFc6ADuhin9tfD0Hf4AsmA9ALQNYAAy+wB0AEwAzKIHnVfXp2BN94-PD9cAHKIPl8dnB4ANkuv3OAE4ACwfMGQ8HPN4gsFQhGfH5-AFgI53P5PM5vG4496Au7HVqrSTQZhbLT0dAefSQYh2bJuBTwIprWiwWb+WrQaD0ZC4AD0IoQS3OlA8dVojF4gl0+n4+nOuiwIoAInRKAAhbnlEW1ABucngItyfEERtN8hFbOg5vkXgO8HgByBDSBroanQOjpG-Aa8HOiHQB1q-HgbkE52gyGNFIoGGwCk5DgCeWQ21IKUkAHFcpQzBYrEJJpIAEoAQRCAEkOJWAMolyzWCsURKtstJjOagCi9cbLf85jb5bzFH4IS7o9L1knGXIS7ALL6ABJkNpI7ktPzBcKxbaQ9LZYxzh50CKtzvzceDlwZfwRcbIecAFbIdCIJOQFW6TxEGLfBij6JIQMnKIYjiLRuF4AQJwzL9YAMED1E0fxciSIwoNieIoESMJ3EiTAYyEfwYy3FVAOLNcVBgakc0gMMsAZeBZnSSZIFYCCM2iPDu3bSCqRpfwAEdaCQaAZQUDxTV7Ch4CCDw-FIGAZS4JiADl51gMB0FQMA+CzZJl2ZZodG-aJgNIDIbCAA Open this visualization in the Vega Editor>

@
let trans = transform
            . aggregate [ opAs Count \"\" \"count\"
                        , opAs 'Median' \"plx\" \"plx_med\"
                        ]
                        [ \"Cluster\" ]

    enc = encoding
          . position X [ PName \"Cluster\"
                       , PSort [ 'ByFieldOp' \"plx_med\" 'Max'
                               , 'Descending'
                               ]
                       ]
          . position Y [ PName \"count\"
                       , PmType Quantitative
                       , PAxis [ AxTitle \"Number of stars\" ]
                       ]

    star = SymPath \"M 0,-1 L 0.23,-0.23 L 1,-0.23 L 0.38,0.21 L 0.62,0.94 L 0,0.49 L -0.62,0.94 L -0.38,0.21 L -1,-0.23 L -0.23,-0.23 L 0,-1 z\"

in toVegaLite [ gaiaData
              , trans []
              , enc []
              , mark Point [ MShape star
                           , MStroke \"black\"
                           , MStrokeWidth 1
                           , MFill \"yellow\"
                           , MSize 100
                           ]
              ]
@

Notes:

- I find the \"Data Viewer\" section of the
<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxQSTIBzWdAVwAcAjATz0MgGEAbW5aAU1iQAugBpi5SPErV+leALZFy5YJHT02kAMZ1E0SKKjxkWxLSyNBkAL7iVJNRq1Z+AEwCWSQ8dP5I9FwAHgD6rm4+kKge-FwR-oFBthIkwik2xGLEkFjwsADWbGQUfDT5-ADqHm7QABZsAIz2JR4AXvyNAAydzRCQpejlWoxc8NqFvVDRXFxazLFc6ADuhin9tfD0Hf4AsmA9ALQNYAAy+wB0AEwAzKIHnVfXp2BN94-PD9cAHKIPl8dnB4ANkuv3OAE4ACwfMGQ8HPN4gsFQhGfH5-AFgI53P5PM5vG4496Au7HVqrSTQZhbLT0dAefSQYh2bJuBTwIprWiwWb+WrQaD0ZC4AD0IoQS3OlA8dVojF4gl0+n4+nOuiwIoAInRKAAhbnlEW1ABucngItyfEERtN8hFbOg5vkXgO8HgByBDSBroanQOjpG-Aa8HOiHQB1q-HgbkE52gyGNFIoGGwCk5DgCeWQ21IKUkAHFcpQzBYrEJJpIAEoAQRCAEkOJWAMolyzWCsURKtstJjOagCi9cbLf85jb5bzFH4IS7o9L1knGXIS7ALL6ABJkNpI7ktPzBcKxbaQ9LZYxzh50CKtzvzceDlwZfwRcbIecAFbIdCIJOQFW6TxEGLfBij6JIQMnKIYjiLRuF4AQJwzL9YAMED1E0fxciSIwoNieIoESMJ3EiTAYyEfwYy3FVAOLNcVBgakc0gMMsAZeBZnSSZIFYCCM2iPDu3bSCqRpfwAEdaCQaAZQUDxTV7Ch4CCDw-FIGAZS4JiADl51gMB0FQMA+CzZJl2ZZodG-aJgNIDIbCAA Vega Editor>
rather useful when creating new data columns or structures,
as you can actually see what has been created (I find Firefox works much
better than Chrome here);

- the use of 'ByFieldOp' here is a bit un-settling, as you need to
give it an aggregation-style operation to apply to the data field,
but in this case we have already done this with 'opAs' (so I pick
'Max' as we just need something that copies the value over).

We revisit this data in 'layeredCount'.

-}


starCount2 :: VegaLite
starCount2 =
  let trans = transform
              . aggregate [ opAs Count "" "number"
                          , opAs Median "plx" "plx_med"
                          ]
                          [ "Cluster" ]

      enc = encoding
            . position X [ PName "Cluster"
                         , PmType Nominal
                         -- I think I want a ByField back
                         , PSort [ ByFieldOp "plx_med" Max
                                 , Descending
                                 ]
                         ]
            . position Y [ PName "number"
                         , PmType Quantitative
                         , PAxis [ AxTitle "Number of stars" ]
                         ]

      star = SymPath "M 0,-1 L 0.23,-0.23 L 1,-0.23 L 0.38,0.21 L 0.62,0.94 L 0,0.49 L -0.62,0.94 L -0.38,0.21 L -1,-0.23 L -0.23,-0.23 L 0,-1 z"

  in toVegaLite [ gaiaData
                , trans []
                , enc []
                , mark Point [ MShape star
                             , MStroke "black"
                             , MStrokeWidth 1
                             , MFill "yellow"
                             , MSize 100
                             ]
                ]



{-|

Vega-Lite supports a number of data transformations, including
several \"pre-canned\" transformations, such as a
kernel-density estimator, which I will use here to
look for structure in the parallax distribution. The earlier
use of a fixed-bin histogram - 'parallaxHistogram' and 'ylogHistogram' -
showed a peak around 5 to 10 milli-arcseconds, and a secondary
one around 20 to 25 milli-arcseconds, but can we infer anything more
from the data?

I have already shown that the 'transform'
function works in a similar manner to 'encoding', in that
it is applied to one or more transformations. In this
example I use the 'density' transform - which is __new to Vega Lite 4__ -
to \"smooth\" the data without having to pre-judge the data
(although there are options to configure the density estimation).
The transform creates new fields - called \"value\" and \"density\"
by default - which can then be displayed as any other field. In this
case I switch from 'Bar' or 'Line' to use the 'Area' encoding, which
fills in the area from the value down to the axis.

<<images/vl/densityparallax.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hIATAUxQEtoBPPKABwBsAPSAXwF0AacKLeLADW9UBAiR0DeAGNqdfAAYAdAHZe4qMjjohZepABGTWSI3jI22LrIAReMgAW9AgCZuYACw8+E2g318SEEyeEg+dnNSeGgw-DELAFdYJgNHaGgGZFwAehyEAHdlAHNqR0TDROQyWBl0RGgKaGU6rBzbdETigCFkvRzHADcyYvgcgW0ageHRnJIYsdHKeABaeFWANgBGDbWtxRXY4zIt+GVEdBXHUPJYZWhkQchzCQxsGNFfC2lYas-NTSQADiAmKBkQiSwhhqzy+gIASgBBAD6AEkAMLwgDK4Mh0NgsIB31YuKhMJeAMgtgAomjMTighCyQSKYCyMjmGxGXiYXCwOwvgKIJE+JAACTIGTXARpDJZXI5GZnUrQcqGZSUdA5SXSsZKlZMahkRWeZQAK2Q9UJUAodRIlEQYPiX0gXLACUBqEoZCYJAMg3gTES+lZUH8gSgAEdEkhoNQYpRhtbAfAWJRkKIYNQmBHID9AyY2ELxCLAQp3ZAvT6-UFyFRaM8wzQAgZo7H43Gk8XS1A6ogvU73QL2EA Open this visualization in the Vega Editor>

@
let trans = transform
            . 'density' \"plx\" []

    enc = encoding
          . position X [ PName \"value\"
                       , PmType Quantitative
                       , PAxis [ AxTitle \"parallax\" ]
                       ]
          . position Y [ PName \"density\", PmType Quantitative ]

in toVegaLite
     [ gaiaData
     , mark 'Area' [ MOpacity 0.7
                 , MStroke \"black\"
                 , 'MStrokeDash' [ 2, 4 ]
                 , ]
     , trans []
     , enc []
     ]
@

The parallax distribution shows multiple peaks within the
5 to 10 milli-arcsecond range, and separate peaks at 12
and 22 milli-arcseconds.

The properties of the area mark are set here to add a black,
dashed line around the edge of the area. The 'DashStyle' configures
the pattern by giving the lengths, in pixels,
of the \"on\" and \"off" segments, so here the gaps are twice the
length of the line segments. This was done more to show that it
can be done, rather than because it aids this particular visualization!

-}

densityParallax :: VegaLite
densityParallax =
  let trans = transform
              . density "plx" []

      enc = encoding
            . position X [ PName "value"
                         , PmType Quantitative
                         , PAxis [ AxTitle "parallax" ]
                         ]
            . position Y [ PName "density", PmType Quantitative ]

  in toVegaLite
       [ gaiaData
       , mark Area [ MOpacity 0.7
                   , MStroke "black"
                   , MStrokeDash [ 2, 4 ]
                   ]
       , trans []
       , enc []
       ]


{-|

The density estimation can be configured using 'DensityProperty'.
Here we explicitly label the new fields to create (rather than
use the defaults), and ensure the calculation is done per cluster.
This means that the data range for each cluster is used to
perform the KDE, which in this case is useful (as it ensures the
highest fidelity), but there are times when you may wish to ensure
a consistent scale for the evaluation (in which case you'd use
the 'DnExtent' option, as well as possibly 'DnSteps', to define
the grid). The final change is to switch from density estimation
to counts for the dependent axis.

<<images/vl/densityparallaxgrouped.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxQSTIBzWdAVwAcAjATz0MgGEAbW5aAU1iQAugBpi5SPGRsCkAB4BrACb9IoqMxVqxEigGM6iaDPxxa-ceQiRVKAJbRW+SPS7zIegL7FdNrPCwimzAkOj08PqOzmAADAB0AOwaMMz0ai6B-PCQXlZQyvDQOfhkFLSwXGyQABbQ0PTIuAD0zQgA7vGUjjW0jLyChsb8xvGGWM0AInSUAEIVivzNNQBu-JTwzQF8gstrG82Fxc0b9vAAtPAXAGwAjNeXt7HnxYxc-Lfw8Yjo5zXZqlg8RMK3UekgGGwRRCehsEVgyAypFhFAA4gFKNVELQsIxBGDrBQAEoAQQA+gBJDhEgDKWJxeKE+UJrnc9Nx+OZ1kgkwAopTqXSXNiOUyUTZ+GS3B5hQz8SifORFWA8sRIAASZD6f4Bap1BpNVr7L7daC9RjxezoZpanWbY3nLiOJYrAAs8QAVsh0IgCVARoZlPZEJjSuDDFxMCEIfZ+FxlNVuLwBEyoE50lj0Fhg-AqqrJDLkSzULH49UlKo-ZJ00jIABHWhIaCOIr2NZVijweT2UykGCOd7VAAKgVzXC7uW8XMgMVCJbjCZcWkrKRr1QbTZbzfbKS7Pejzegg5cHCMJlyyvzUCGJdDpB8XiAA Open this visualization in the Vega Editor>

@
let trans = transform
            . density "plx" [ 'DnAs' \"xkde\" \"ykde\"
                            , 'DnGroupBy' [ \"Cluster\" ]
                            , 'DnCounts' True
                            ]

    enc = encoding
          . position X [ PName \"xkde\"
                       , PmType Quantitative
                       , PAxis [ AxTitle \"Parallax\" ]
                       ]
          . position Y [ PName \"ykde\"
                       , PmType Quantitative
                       , PAxis [ AxTitle \"Counts\" ]
                       ]
          . color [ MName \"Cluster\"
                  , MmType Nominal
                  ]

in toVegaLite
     [ gaiaData
     , mark Area [ MOpacity 0.7 ]
     , trans []
     , enc []
     ]
@

Note how the clusters separate out in pretty cleanly, but - as
also shown in the 'pointPlot' visualization below - it is pretty
busy around 7 milli arcseconds.

The counts here (the Y axis) are __significantly larger__ than
seen than the actual count of stars, shown in 'starCount'. It
appears that the @'DnCounts' True@ option is interpreted as
<https://vega.github.io/vega-lite/docs/density.html#example-stacked-density-estimates multiplying the density values by the number of values in a group>,
which means that there is a bin-width effect. This is explored
further in the 'compareCounts' plot below.

-}

densityParallaxGrouped :: VegaLite
densityParallaxGrouped =
  let trans = transform
              . density "plx" [ DnAs "xkde" "ykde"
                              , DnGroupBy [ "Cluster" ]
                              , DnCounts True
                              ]

      enc = encoding
            . position X [ PName "xkde"
                         , PmType Quantitative
                         , PAxis [ AxTitle "Parallax" ]
                         ]
            . position Y [ PName "ykde"
                         , PmType Quantitative
                         , PAxis [ AxTitle "Counts" ]
                         ]
            . color [ MName "Cluster"
                    , MmType Nominal
                    ]

  in toVegaLite
       [ gaiaData
       , mark Area [ MOpacity 0.7 ]
       , trans []
       , enc []
       ]


-- $intro-points
-- At this point we make a signifiant detour from the Elm Vega-Lite
-- walkthtough, and look a bit more at the 'Point' mark, rather than creating
-- small-multiple plots. Don't worry, we'll get to them later.
--
-- I apologize for the alliterative use of point here.

{-|

Here I use the 'Point' mark to display the individual
@Gmag, plx@ pairs, encoding by __both__ 'color' and 'shape.
Since the encoding uses the same field of the data (the @Cluster@
name), Vega-Lite is smart enough to only display one legend,
which contains the point shape and color used for each cluster.

Since the parallax values are bunched together at low values,
a logarithmic scale ('ScLog') is used for the y axis, along with
commands to define the actual axis domain - by turning off the
'IsNice' support and listing the minimum and maximum values
for the axis with 'SDomain'.

<<images/vl/pointplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMAWADDgNOFALYCGATgNYZQAOA9vAHZqESQAmpKpNoE7AK7kANjRgoUtAM7oA9HPKkA7gDpE8FNEEAjQdNjkAxvRawWqk8TkAReoMQAhYZVhzoAN1iJScstJRDdy8fOS4eOR94UgBaUliANgBGBLiknBieHRFYJNJVJnoYuFIOQ1UUaQ9INgFIADN6cjI0TH4BdloKAz4iDvYAcTJEcSZBYh1DGr7+yAAlAEEAfQBJAGE5gGVR8cnyaf662hEADx2JqdrDyBsAUVWN7cxIMYv9q9nYJeOz59e9yAzCAAXxmoJBtUgyngHC0NFwBCIkAAJNIjHAyOJUFJZAoQvkNFpdKp4PQ5GiMb58TERJo3B4sKoAFbSUwHKDmEwcZgjNozSAmERNPgNeCwEQccRrET6QLvKAoACetFgo3oxGYpDEwI+kGk0FIKpF9TFEqlMoClwVytVf3Vmu1ut+YGAovFkueQ1II3w1qNzwAjoJSCxNNx4F5IDr+YretcTe7xD92R09UYtbaXZx1aRmDQANoAZlUAFZfYWAEwAXV9kCV-qgQp9UCY8CMmfqWoM0eu9czkCDIZQYeHkbBRB7UBMTBNvJdoOBQA Open this visualization in the Vega Editor>

@
let enc = encoding
                    . position X [ PName \"Gmag\", PmType Quantitative ]
                    . position Y [ PName \"plx\", PmType Quantitative, PScale scaleOpts ]
                    . color cluster
                    . shape cluster

    scaleOpts = [ SType ScLog, 'SDomain' ('DNumbers' [3.5, 32]), 'SNice' ('IsNice' False) ]
    cluster = [ MName \"Cluster\", MmType Nominal ]
, 
in toVegaLite [ gaiaData
              , mark Point []
              , enc []
              , width 400
              , height 400
              ]
@

We can see that each cluster appears to have a separate parallax
value (something we have seen in earlier plots, such as 'parallaxBreakdown'),
and that it doesn't really vary with Gmag. What this is telling
us is that for these star clusters, the distance to each member star
is similar, and that they are generally at different distances
from us. However, it's a bit hard to tell exactly what is going
on around 5 to 6 milli arcseconds, as the clusters overlap here.

This line of thinking leads us nicely to map making, but before we
try some cartography, I wanted to briefly provide some context for
these plots. The
<https://lco.global/spacebook/distance/parallax-and-distance-measurement/ parallax of a star>
is a measure of its distance from us, but it is an inverse relationship,
so that nearer stars have a larger parallax than those further from us.
The @Gmag@ column measures the apparent brightness of the star, with the
@G@ part indicating what
<https://www.cosmos.esa.int/web/gaia/iow_20180316 part of the spectrum>
is used (for Gaia, the @G@ band is pretty broad, covering much of
the visible spectrum), and the @mag@ part is because optical Astronomy
tends to use

 * <https://www.skyandtelescope.com/astronomy-resources/the-stellar-magnitude-system/ the logarithm of the measured flux>

 * and then subtract this from a constant

so that larger values mean fainter sources. These are also
<https://en.wikipedia.org/wiki/Apparent_magnitude apparent magnitues>,
so that they measure the flux of the star as measured at Earth,
rather than its intrinsic luminosity (often defined as an object's
<https://en.wikipedia.org/wiki/Absolute_magnitude absolute magnitude>).

We can see that the further the cluster is from us - that is, as we
move down this graph to smaller parallax values - then the
smallest stellar magnitude we can see in a cluster tends to increase,
but that there are stars up to the maximum value (20) in each cluster.
This information can be used to look at the
distribution of absolute magnitudes of stars in a cluster, which
tells us about its evolutionary state - such as is it newly formed or old -
amongst other things. However, this is straying far from the
aim of this tutorial, so lets get back to plotting things.

-}

pointPlot :: VegaLite
pointPlot =
  let enc = encoding
              . position X [ PName "Gmag", PmType Quantitative ]
              . position Y [ PName "plx", PmType Quantitative, PScale scaleOpts ]
              . shape cluster
              . color cluster

      scaleOpts = [ SType ScLog, SDomain (DNumbers [3.5, 32]), SNice (IsNice False) ]
      cluster = [ MName "Cluster", MmType Nominal ]

  in toVegaLite [ gaiaData
                , mark Point []
                , enc []
                , width 400
                , height 400
                ]

-- $intro-map
-- We have some hint that the different clusters are distinct objects
-- in space, in that they appear to be different distances from us,
-- but where does the \"cluster\" in the name \"Stellar Cluster\"
-- come from? Well, we can try plotting up the position of each star
-- on the sky - using the @RA_ICRS@ and @DE_ICRS@ fields - to find out.

{-|

The following specification should only contain one new feature - other
than sneakily switching from 'Point' to 'Circle' type for the mark - and
that is displaying the x axis (namely Right Ascension) in reverse (using
@'PSort' [ 'Descending' ]@. This is needed because Right Ascension
is measured from right to left. I like to explain it by talking about
oranges, and how we are at the center of an orange looking out at its
skin, and so have the direction reversed to if you were outside, looking
in. This may be why I don't get invited to too many parties.
You can see that we also have one cluster that straddles the
0 and 360 degrees Right Ascension meridian, which will lead to some
fun later ('clusterCenters').

<<images/vl/posplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFALYCGATgNYZQDG85tANrJIRJACakqk2gQOAV3JMaMFCgAOAZ3QB6eeVIB3AHSJ4KaEIBGQmbEYB7AHYpY5tbWPF5AEWNDEAIRGVY86ADdYiUvJkMhbkXr7+8ty88v7wpAC0pAkAbACMyYmpOPG8uiyppGqmxvFwpJxGaigy3mxEHABmxuRkaJgCghxSFIb89Z1QAOJkiOKmQsS6RnUDgpAASgCCAPoAkgDC8wDKYxNT5DOzUFJMAB67k9PsR5D2AKJrmzuYkOOXB9ezkLDLJ+cvb32kH6EAAvv1wWDrpAVPBONoaAAWPDQgAkMlocDI4lQ0jkinChU02j0angxnkGKxAUJ8SYWk83kRagAVjIzIdvqYbJx4KZRu1+pAbExmvxIA14LAmJxxOsmAYQmwoCgAJ5SVgA2x80hiUGfKD-MAdToSqUy8RLR7bQ6mjG6zXGri2Uh8mgAbQI2GSOAAuvgoKZ4LRHQ1dYZ9SCoOzyG0oBUMZZefzbXM1RrxABHISkcxaHjwXypjikU7wGTilBaFiWpCoMCLROmGTk0xgAAUFUQAEpIJDBJHTaq+l9JdLZS97tadgaOPaa+1nWQ3Zh3fEAJxezf+wPB0Ph2CDgaQdOOyDZ3NV3hVouzqCl8uV6tn+ywZg6qtmDtd3v9sFEI9hTMSUBWNcFQSAA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X (axOpts \"RA_ICRS\" \"Right Ascension (deg)\" ++ [ raScale, 'PSort' [ 'Descending' ] ])
            . position Y (axOpts \"DE_ICRS\" \"Declination (deg)\" ++ [ decScale ])
            . color [ MName \"Cluster\", MmType Nominal ]

    axOpts field lbl = [ PName field, PmType Quantitative, PAxis [ AxTitle lbl ]]

    scaleOpts minVal maxVal = [ SDomain (DNumbers [ minVal, maxVal ]), SNice (IsNice False) ]
    raScale = PScale (scaleOpts 0 360)
    decScale = PScale (scaleOpts (-90) 90)

in toVegaLite [ gaiaData
              , mark 'Circle' []
              , enc []
              , width 400
              , height 300
              ]
@

We can see that these clusters are indeed localised on the sky,
with <https://en.wikipedia.org/wiki/Hyades_(star_cluster) Hyades>
looking like it covers the largest area. However, we should be
careful and __not forget__ either
<https://www.youtube.com/watch?v=E9IuXEwpU7U Grover's hard work>
or
<https://www.youtube.com/watch?v=MMiKyfd6hA0 Father Ted's explanation to Father Dougal>,
since these clusters are different distances from us, which
makes size a tricky thing to measure from this plot.

There is also the fact that I have used possibly the worst
way of displaying the Right Ascension and Declination data. Although
the night sky is not the same as the Earth's surface, the issues
when trying to display the Globe on a flat surface also apply to
displaying up the sky. For this plot the distortions near the
pole are huge, although fortunately we don't have any clusters
too close to either pole.

-}

posPlot :: VegaLite
posPlot =
  let enc = encoding
              . position X (axOpts "RA_ICRS" "Right Ascension (deg)" ++ [ raScale, PSort [ Descending ] ])
              . position Y (axOpts "DE_ICRS" "Declination (deg)" ++ [ decScale ])
              . color [ MName "Cluster", MmType Nominal ]

      axOpts field lbl = [ PName field, PmType Quantitative, PAxis [ AxTitle lbl ]]

      scaleOpts minVal maxVal = [ SDomain (DNumbers [ minVal, maxVal ]), SNice (IsNice False) ]
      raScale = PScale (scaleOpts 0 360)
      decScale = PScale (scaleOpts (-90) 90)

  in toVegaLite [ gaiaData
                , mark Circle []
                , enc []
                , width 400
                , height 300
                ]


{-|
Vega-Lite supports a large number of projections - via the
'Projection' type - which we use below to create
a similar visualization to 'posPlot'. Here I use the
'Longitude' and 'Latitude' channels, along with a
'Mercator' 'projection', to display the data.

The trick in this case is that longitude runs from -180 to 180
degrees, but the data has Right Ascension going from 0
to 360 degrees. Here we take advantage of Vega Lite's
__data transformation__ capabilities and create a new
column - which I call @longitude@ - and is defined as
\"Right Ascension - 360\" when the Right Ascension is
greater than 180, otherwise it is just set to the
Right Ascension value. The "expression" support
is essentially a sub-set of Javascript, and the @datum@
object refers to the current row. The new data column
can then be used with the 'Longitude' channel.
Thankfully the 'Latitude' channel can use the Declination values
without any conversion.

As can be seen, this flips the orientation compared to
'posPlot', and makes the center of the plot have a
longiture (or Right Ascension), of 0 degrees.

<<images/vl/skyplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxQSTJ5k8oAbdRAcwEtoBXAEwFNIAaY8pADG8WkLa140HvkgcpbLADoASgEEA+gEkAwioDKYAHxgAjAA4ADGAD8Yee2XrtewwFowAZgBs1-A8VVTV0DSAEwAF9iAF1+CEgACy5mRgToGgAWS0s4qCx4WABrGmFmWCFaHly5KXgaMgo2WFoStOgAB2RcAHpuhAB3JRZoBLYAIzZkLnKGaURoJSF0LG6AEXQ2RgAhJsKuboSANy5GeG785GlYA+PT7ocz0+Z4N3gX71NvV9NLN2h4MaVUzwJSIdBuJLwbiwJTQZCHPjhSAYbBSerheLtApTdHkQQAcXyjBKiEUY2miLxFGcIX0JLJFNyVMg7VoAA96VhybBKVSoKsAKIuUKyUlcxkYihcDSsjmihk8yVRcjKyLVfrMDgjGieACsOWIkAAJMghEl8q1oB0ur1biDhqMxkpmOhuqbzWc7W5aKx9ocMkoAFbIBi8lmwdCBrhCaAuxD1GAAT3aMjy0xE0EwkAi1S4iCWHGYTFxFCW9B5+AaglQzC4tA4JVlvMEptEqeAkHdXCwqcghzKmuY1F4UGgyd79GJOclSZTJQAjmwkLH-rHjs2KJVGHmG5WYKxKo2CqJJBzVRBp4JJCvOO3kbX6yVBcK6SPZ73F8vWFJmOvLxRM3QWhY3aBMazrXcoB0WhJiuPhR3HElliLURsyZOgGGGW8wIfSDIHoJhWGwt8xznWRP3mb81x4VV-2EBga2JSsogiIA Open this visualization in the Vega Editor>

@
let trans = transform
                . 'calculateAs'
                  "datum.RA_ICRS > 180 ? datum.RA_ICRS - 360 : datum.RA_ICRS"
                  "longitude"

    axOpts field = [ PName field, PmType Quantitative ]

    enc = encoding
            . position 'Longitude' (axOpts "longitude")
            . position 'Latitude' (axOpts \"DE_ICRS\")
            . color [ MName \"plx\"
                    , MmType Quantitative
                    , 'MScale' [ SType ScLog
                             , 'SScheme' \"viridis\" []
                             ]
                    , MLegend [ 'LTitle' "parallax" ]
                    ]
            . 'tooltip' [ 'TName' \"Cluster\", 'TmType' Nominal ]

in toVegaLite [ width 400
              , height 350
              , 'projection' [ 'PrType' 'Mercator' ]
              , gaiaData
              , trans []
              , enc []
              , mark Circle []
              ]
@

The other major change made to 'posPlot' is that the stars are now
color-encoded by the log of their parallax value
rather than cluster membership,
and the color scheme has been changed to use the \"viridis\" color
scale.
The 'LTitle' option is set for the legend (on the
'color' channel) rather than use the default (which in
this case would be @\"plx\"@).

Since parallax is a numeric value, with ordering (i.e. 'Quantitative'),
the legend has changed from a list of symbols to a gradient bar.
To account for this lost of information, I have added a 'tooltip'
encoding so that when the pointer is moved over a star its cluster
name will be displayed. This is, unfortunately,
/only/ visible in the interactive version of the visualization.

__Note that__ the tooltip behavior changed in Vega Lite 4 (or in the
code used to display the visualizations around this time), since
prior to this tooltips were on by default. Now tooltips have to be
explicitly enabled (with 'tooltip' or 'tooltips').

From this visualization we can see that the apparent size of the cluster
(if we approximate each cluster as a circle, then we can think of the radius
of the circle as a measure of size) depends on parallax, with larger
sizes having larger parallaxes. This is because the distance to a star
is inversely-dependent on its parallax, so larger parallaxes mean the
star is closer to us. However, there is no reason that the intrinsic
size - that is its actual radius - of each cluster is the same.
We can see that although the Hyades and Pleiades clusters overlap
on the sky, they have significantly-different parallaxes (as can
be seen in 'pointPlot' for example), with Hyades being the closer
of the two.

It is possible to add graticules - with the aptly-named
'graticule' function - but this requires the use of layers,
which we haven't covered yet. If you are impatient you can jump
right to 'skyPlotWithGraticules'!

If you want to see how to "create your own projection", see
'skyPlotAitoff', which uses the
<https://en.wikipedia.org/wiki/Aitoff_projection Aitoff projection>
(which is unfortunately not available to
Vega-Lite directly).

-}

skyPlot :: VegaLite
skyPlot =
  let trans = transform
                . calculateAs
                  "datum.RA_ICRS > 180 ? datum.RA_ICRS - 360 : datum.RA_ICRS"
                  "longitude"

      axOpts field = [ PName field, PmType Quantitative ]

      enc = encoding
              . position Longitude (axOpts "longitude")
              . position Latitude (axOpts "DE_ICRS")
              . color [ MName "plx"
                      , MmType Quantitative
                      , MScale [ SType ScLog
                               , SScheme "viridis" []
                               ]
                      , MLegend [ LTitle "parallax" ]
                      ]
              . tooltip [ TName "Cluster", TmType Nominal ]
              -- note: opacity doesn't really help here

  in toVegaLite [ width 350
                , height 400
                , projection [ PrType Mercator ]
                , gaiaData
                , trans []
                , enc []
                , mark Circle []
                ]


-- $intro-choropleth
-- There are some things vega-lite can do, don't fit as well into the
-- flow of looking at astronomy data!  But having examples is helpful.
-- So we bring our eyes back to earth, and demonstrate a basic
-- "choropleth", a map - in the sense of pictures of bounded geographical
-- regions - with data for each location indicated by color.
--
-- Don't worry, we'll soon be back staring at the stars!
--
-- The choropleth examples (there's another one later on)
-- use a map of the United States as the data source, which we abstract
-- out into a helper function:
--
-- @
-- usGeoData :: T.Text -> Data
-- usGeoData f = dataFromUrl \"https:\/\/raw.githubusercontent.com\/vega\/vega\/master\/docs\/data\/us-10m.json\" ['TopojsonFeature' f]
-- @
--
-- The argument gives the \"topological\" feature in the input file to
-- display (via 'TopojsonFeature'). You can read more information on this
-- in the <https://vega.github.io/vega-lite/docs/data.html#topojson Vega-Lite documentation>.
--
-- This section was contributed by Adam Conner-Sax. Thanks!

usGeoData :: T.Text -> Data
usGeoData f = dataFromUrl "https://raw.githubusercontent.com/vega/vega/master/docs/data/us-10m.json" [TopojsonFeature f]


{-|

Our first choropleth is based on the
<https://vega.github.io/vega-lite/examples/geo_choropleth.html Choropleth>
example from the Vega-Lite
<https://vega.github.io/vega-lite/examples/ Example Gallery>.

The key elements are:

   * Using the 'TopojsonFeature' feature for the data source (thanks to @usGeoData@).
   * Choosing the correct "feature" name in the geographic data, here @\"counties\"@
     in the argument to our @usGeoData@ helper function.
   * Performing a Vega-Lite lookup to join the data to be plotted (the unemployment rate)
     to the geographic data.  In this case, the column name in the unemployment data - @\"id\"@
     given as the first argument to 'lookup' - is the same as the column name in the
     geographic data, the third argument to 'lookup'. Those can be different.
   * Specifying a projection, that is a mapping from (longitude, latitude) to (x,y)
     coordinates. Since we are looking at data for the main-land United States of
     America we use 'AlbersUsa' (rather than looking at the whole globe, as we did
     in earlier visualizations), which lets us view the continental USA as well as Alaska and Hawaii.
   * Using the 'Geoshape' mark.

<<images/vl/choroplethlookuptogeo.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxQSTJVZ0d8zyKATeaePU4higV1gBt2kABbRoAB2S4A9FIQB3AHQBzAJbQh3AEbdkAU1gBjdImi6TCo1ikA3XUvg27DrPGSnYUxugPJPLB9yIulhifOgAnlhm0ArQyNaQnAwAvgA0SeSQANa64YIqjJDpXBCUKrp8jMjsBJAIppAAuhlgaS2QYehZ3GL5hUnJxI3FUEK6Kkoi7ADMAAyzI5AusFmCSrroyELwYrpFxJDMrOz0pbwC+MKiEtKy8Iqq6lo6+kYm0RY0jvbfzq7unm8viOAWQAFoAIyzLAKABWyGM+0yGGwLBOlF0LF4e0uRkC0HK1VSUGg4V2gmg6DE6HhiMGEDapTkBXU7AArPNFgASZAGMYuQQicSSGS2ezKNQaTQKFToKS8-kOMXwMF8NS6GwAFjhCMQSKgYmosN0BgJiLoMDJOKg8D4mn0yAAqsg2IyoGYjIwVIglCckpAjGFYH6uGUKoVLvU9iNMrzbdbgJAFcFrZAxLxQhsED69m7MqTyZcAI7cJAE1gE2yJcj01qLTTwAxZJTUQIRm0AL2x+oDxlQExOg2SQA Open this visualization in the Vega Editor>

@
let unemploymentData = dataFromUrl \"https:\/\/raw.githubusercontent.com\/vega\/vega\/master\/docs\/data\/unemployment.tsv\" []

in toVegaLite
   [ usGeoData \"counties\"
   , transform
     . 'lookup' \"id\" unemploymentData \"id\" ('LuFields' [\"rate\"])
     $ []
   , projection [PrType 'AlbersUsa']
   , encoding
     . color [ MName \"rate\", MmType Quantitative, MScale [ SScheme "purpleorange" [] ] ]
     $ []
   , mark Geoshape []
   , width 500
   , height 300
   , 'background' "azure"
   ]
@

So, we have seen how to join data between two datasets - thanks to
'lookup' - and display the unemployment rate (from one data source)
on a map (defined from another data source).

I have chosen a
<https://vega.github.io/vega/docs/schemes/index.html#diverging diverging color scheme>
for the rate, mainly just because I can, but also because I wanted to see how
the areas with high rates were clustered. I've also shown how the 'background'
function can be used (it is simpler than the 'configuration' approach
used earlier in 'stripPlotWithBackground').

Our next choropleth - 'choroplethLookupFromGeo' - will show how we can join
multiple fields across data sources, but this requires understanding how
Vega-Lite handles multiple views, which is fortunately next in our
tutorial.

-}

choroplethLookupToGeo :: VegaLite
choroplethLookupToGeo =
  let unemploymentData = dataFromUrl "https://raw.githubusercontent.com/vega/vega/master/docs/data/unemployment.tsv" []

  in toVegaLite
     [ usGeoData "counties"
     , transform
       . lookup "id" unemploymentData "id" (LuFields ["rate"])
       $ []
     , projection [PrType AlbersUsa]
     , encoding
       . color [ MName "rate", MmType Quantitative, MScale [ SScheme "purpleorange" [] ] ]
       $ []
     , mark Geoshape []
     , width 500
     , height 300
     , background "azure"
     ]


-- $intro-layered
-- The Stacked-Histogram plot - created by 'gmagHistogramWithColor' - showed
-- the distribution of the \"Gmag\" field by cluster, but it was hard to
-- compare them. A common approach in this situation is to split up
-- the data into multiple plots -
-- the <https://en.wikipedia.org/wiki/Small_multiple small multiple>
-- approach (also known as trellis plots) - which we can easily achieve in
-- Vega Lite. It also gets us back on track with the Elm walkthrough.

{-|

Our first attempt is with the 'column' function, which tells
Vega-Lite to create a plot for each @Cluster@ field (and introduces
us to the @F@ family of 'FacetChannel' constructors).

The legend has been turned off with @'MLegend' []@, since it doesn't
add anything to this visulization (as the individual plots, labelled
by the cluster name, provide the same information).

<<images/vl/smallmultiples.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAjBkA04oBNYAXWVMUCCSAV3gBszIALIogBwGcUB6b+WAO4A6AOYBLIk2rpqHAKbwAxgHsAdkTnqhK6NwAiy6iIBCtRHO5MAbnJGxucDhviWbd7oRLc7Y2AFpYfwA2AEYggJCABj8SdDo5ENghVWU-JjlYfAUhIg4rHDwqADNleDgiMgpKKjYEeUrC6qgAcTgRRlVqaHQFAqbKSAAlAEEAfQBJAGFBgGUOrp74Pv6oNjoAD3nu3twVqD0AUQnpubRITu2l3ZXIOVG1zbOLxchGiABfRs+P68gAEg4inScEYLHYXF4biS4kk0iEYmU3EBwPsUL8dAkFisABYhAArDhqZa3VQqfBiVTtNBVKgqOilSqQIpiOR0fCMSZ0WTOHBQIgATzYcg6ymgFNgDGwUHiIk07LQnTodHe1yoj3ITJZbMYrVg7SlkHQFLIRHg1DkBoFQsYAEdqLB1BJiGIbJAVY1IHSuqpGczWfKoJzuTs+YLhU9ReKGO7qpB+Q1+pA9SJ4LZiOGoCpqOplrGrRnIHaHUQnSXXaqBrB1mIOIyS0R4owAHILBRgZRFMAzEjwWvfSj9mOZtTMqnkT7vIA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"Gmag\", PmType Quantitative, PBin [] ]
            . position Y yAxis
            . color [ MName \"Cluster\", MmType Nominal, MLegend [] ]
            . 'column' [ 'FName' \"Cluster\", 'FmType' Nominal ]

    yAxis = [ PAggregate Count
            , PmType Quantitative
            , PAxis [ AxTitle \"Number of Stars\" ]
            ]

in toVegaLite
     [ gaiaData
     , mark Bar []
     , enc []
     ]
@

Since we have nine clusters in the sample, the overall visualization is
too wide, unless you have a very-large monitor. Can we do better?

-}

smallMultiples :: VegaLite
smallMultiples =
  let enc = encoding
              . position X [ PName "Gmag", PmType Quantitative, PBin [] ]
              . position Y yAxis
              . color [ MName "Cluster", MmType Nominal, MLegend [] ]
              . column [ FName "Cluster", FmType Nominal ]

      yAxis = [ PAggregate Count
              , PmType Quantitative
              , PAxis [ AxTitle "Number of Stars" ]
              ]

  in toVegaLite
       [ gaiaData
       , mark Bar []
       , enc []
       ]


{-|

The number of columns used in small-multiple can be defined using the
'columns' function. However, this requires us to:

 * move the facet definition out from the encoding and into the top-level,
   with the 'facetFlow' function;

 * and define the plot as a separate specification, and apply it
   with 'specification' and 'asSpec'.

The actual syntactic changes to 'smallMultiples' are actually
fairly minor:

@
let enc = encoding
            . position X [ PName \"Gmag\", PmType Quantitative, PBin [] ]
            . position Y yAxis
            . color [ MName \"Cluster\", MmType Nominal, MLegend [] ]

    yAxis = [ PAggregate Count
            , PmType Quantitative
            , PAxis [ AxTitle \"Number of Stars\" ]
            ]

in toVegaLite
     [ gaiaData
     , 'columns' 4
     , 'facetFlow' [ FName \"Cluster\", FmType Nominal ]
     , 'specification' ('asSpec' [ mark Bar [], enc [] ])
     ]
@

<<images/vl/smallmultiples2.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCE6BiYZLA4AgXpRZgA4iYshJBiWLLcgqmVVZAASgCCAPoAkgDCXQDKDU0tbWlVGWR4AB4Tza047bOYCgCigyPjqI0r0x1FkPx980sHk6uQJ2AAvh1PEA8zUBRk-EIVpyY4Gga3FK6wyUlE0DYYnqqEKp1EeByBSybH4eGgDSGeD45lSUHgAE8vhMRLIobACCkoMEWFIMagmng8G97pArmgUWj6VBarB6lTINwoYR4DgsPwBYTiQcAI5YWASDgINj6SAs2aQAm-DaQPksHAGBD8BqiLASUEaqXG2XyxWIeAq43vDWwBZsCjIh3wYINAByRxwYBEmTAo0QZTV9xeGGj6qgABIKEJoiYGiRyFRaFYwuxODwmGwRNQkymjNmnHgONpdAAWJgAKwo4lBkARzTEHtQNfeWVgQn4+VhnPRmOxZmm+KJ1qgETJYgpap7ojEmTYMLQTweQA Open this visualization in the Vega Editor>

Note that Vega Lite does support a @\"facet\"@ field in its encodings,
but hvega follows Elm VegaLite and requires you to use this
<https://github.com/gicentre/elm-vegalite/issues/5#issuecomment-514501218 wrapped facet> approach.

I chose 4 columns rather than 3 here to show how "empty" plots
are encoded. You can see how a 3-column version looks in the
next plot, 'densityMultiples'.

-}

smallMultiples2 :: VegaLite
smallMultiples2 =
  let enc = encoding
              . position X [ PName "Gmag", PmType Quantitative, PBin [] ]
              . position Y yAxis
              . color [ MName "Cluster", MmType Nominal, MLegend [] ]

      yAxis = [ PAggregate Count
              , PmType Quantitative
              , PAxis [ AxTitle "Number of Stars" ]
              ]

  in toVegaLite
       [ gaiaData
       , columns 4
       , facetFlow [ FName "Cluster", FmType Nominal ]
       , specification (asSpec [ mark Bar [], enc [] ])
       ]


{-|

Earlier - in 'densityParallaxGrouped' - I used the Kernel-Density
Estimation support in Vega Lite 4 to show smoothed parallax
distributions, grouped by cluster. We can combine this with
the 'facetFlow' approach to generate a plot per cluster
of the parallax distribution. I have used 'DnExtent' to ensure
that the density estimation is done on the same grid for
each cluster.

The most important thing in this example is that I have
used a sensible number of columns (ending up in a three by three grid)!
The other significant changes to 'smallMultiples2' is that I have
used the 'FHeader' option to control how the facet headers
are displayed: the title (which in this case was @\"Cluster\"@)
has been hidden, and the per-plot labels made larger, but moved
down so that they lie within each plot. I am not 100% convinced
this is an intended use of 'HLabelPadding', but it seems to work!

<<images/vl/densitymultiples.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCE6BiYZLA4AgXpRZgA4iYshJBiWLLcgqmVVZAASgCCAPoAkgDCXQDKDU0tbWlVGWR4AB4Tza047bOYCgCigyPjqI0r0x1FkPx980sHk6uQJ2AAvh1PEA8zUBRk-EIVp-D0Ygo2VyhAA2vdCrNICwcMoyNwAJ5gyBDPB8cyQAC67yhpmRCw0cVSUARhP4WJxnX4C0kEjBvhSYAAzL5sfdMHFARwkQdLndZi8MGzTiYcBoGqUYusMlJRNA2GJ6qhIRlRHgcgUsmx+HhoA1UejplB4AivhMRLIFbACIzIMEWFI9agmng8G97pArmh2VBMtrdQ0CUTKX9TeSDgBHLCwCQcBBsfTS3ELNgUTXwDjBBoABVK1rwsCWgqK7qhPO9G19-qdJLJSc6JrNkejscQGcTIYyhdT6cz4agCikFG5kGLGDHgtLUAAJBQhNETA0SOQqLQrGF2JweEw2CJqHOF0Z1048BxtLoACxMABWFHE0sgauagMITPeWVgQn4+WVHS1OprFE0TMY5TmiWI2l-KEC1aPAejEecNQOR160wGCdVzaB5UVQgnAAJgvTs7VgWCADFxHgUY2AAL37LwiIzeAs2dLBXSI9C4NPFgxAaHA2BYEh+XHSlIEbftGgtK0CHSKdH3EP0lTQJ4HiAA Open this visualization in the Vega Editor>

@
let trans = transform
            . density "plx" [ DnAs \"xkde\" \"ykde\"
                            , DnGroupBy [ \"Cluster\" ]
                            , 'DnExtent' 0 30
                            ]

    enc = encoding
          . position X [ PName \"xkde\"
                       , PmType Quantitative
                       , PAxis [ AxTitle \"Parallax\" ]
                       ]
          . position Y [ PName \"ykde\"
                       , PmType Quantitative
                       , PAxis [ AxTitle \"Density\" ]
                       ]
          . color [ MName \"Cluster\"
                  , MmType Nominal
                  , MLegend []
                  ]

    headerOpts = [ 'HLabelFontSize' 16
                 , 'HLabelAlign' 'AlignRight'
                 , 'HLabelAnchor' 'AEnd'
                 , 'HLabelPadding' (-24)
                 , 'HNoTitle'
                 ]

    spec = asSpec [ enc []
                  , trans []
                  , mark Area [ ]
                  ]

in toVegaLite
     [ gaiaData
     , columns 3
     , facetFlow [ FName \"Cluster\"
                 , FmType Nominal
                 , 'FHeader' headerOpts
                 ]
     , specification spec
     ]
@

-}

densityMultiples :: VegaLite
densityMultiples =
  let trans = transform
              . density "plx" [ DnAs "xkde" "ykde"
                              , DnGroupBy [ "Cluster" ]
                              , DnExtent 0 30
                              ]

      enc = encoding
            . position X [ PName "xkde"
                         , PmType Quantitative
                         , PAxis [ AxTitle "Parallax" ]
                         ]
            . position Y [ PName "ykde"
                         , PmType Quantitative
                         , PAxis [ AxTitle "Density" ]
                         ]
            . color [ MName "Cluster"
                    , MmType Nominal
                    , MLegend []
                    ]

      headerOpts = [ HLabelFontSize 16
                   , HLabelAlign AlignRight
                   , HLabelAnchor AEnd
                   , HLabelPadding (-24)
                   , HNoTitle
                   ]

      spec = asSpec [ enc []
                    , trans []
                    , mark Area [ ]
                    ]

  in toVegaLite
       [ gaiaData
       , columns 3
       , facetFlow [ FName "Cluster"
                   , FmType Nominal
                   , FHeader headerOpts
                   -- do not have access to plx field here
                   -- and xkde,ykde is not useful here as want to
                   -- sort by the xkde value when ykde is max
                   -- , FSort [ ByFieldOp "xkde" Median ]
                   ]
       , specification spec
       ]


-- $intro-multiplot
-- There are four ways in which multiple views may be combined:
--
-- * The __facet operator__ takes subsets of a dataset (facets) and
--   separately applies the same view specification to each of
--   those facets (as seen with the 'column' function above).
--   Available functions to create faceted views:
--   'column', 'row', 'facet', 'facetFlow', and 'specification'.
--
-- * The __layer operator__ creates different views of the data but
--   each is layered (superposed) on the same same space; for example
--   a trend line layered on top of a scatterplot.
--   Available functions to create a layered view: 'layer' and 'asSpec'.
--
-- * The __concatenation operator__ allows arbitrary views (potentially
--   with different datasets) to be assembled in rows or columns.
--   This allows \'dashboards\' to be built.
--   Available functions to create concatenated views:
--   'vConcat', 'hConcat', and 'asSpec'.
--
-- * The __repeat operator__ is a concise way of combining multiple views
--   with only small data-driven differences in each view.
--   Available functions for repeated views: 'repeat' and 'specification'.


{-|

We start with a \"basic\" plot for the dataset: the median value
of the parallax of the stars in each cluster.

<<images/vl/baseplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAjBkA04oBNYAXWVMUCCSAV3gBszIALIogBwGcUB6b+WAO4A6AOYBLIk2rpqHAKbwAxgHsAdkTnqhK6NwAiy6iIBCtRHO5MAbnJGxucDhviWbd7oRLc7Y2AFpYfwA2AEYggJCABj8SdDo5ENghVWU-JjlYfAUhIg4rHDwqADNleDgiMgpKKjYEeUrC6qgAcTgRRlVqaHQFAqbKSAAlAEEAfQBJAGFBgGUOrp74Pv6oNjoAD3nu3twVqD0AUQnpubRITu2l3ZXIOVG1zbOLxchGiABfRs+P68gAEg4inScEYLHYXF4biS4kk0iEYmU3EBwPsUL8dAkFisABYhAArDhqZa3VQqfBiVTtNBVKiPciQIpiOR0fCMSZ0WTOHBQIgATzYcg6ymgFNgDHe1yovMqDKZLMYD25kFgIhE8FsxEFZ2gcnJsFUSr5AsYAEdqPqiBJiGIbJBvmAJXhICpVIyqeRPu8gA Open this visualization in the Vega Editor>

@
let plx = position Y [ PName \"plx\", PmType Quantitative, PAggregate Median ]
    cluster = position X [ PName \"Cluster\", PmType Nominal ]
    enc = encoding . cluster . plx

in toVegaLite
      [ gaiaData
      , mark Bar []
      , enc []
      ]
@
-}

basePlot :: VegaLite
basePlot =
  let plx = position Y [ PName "plx", PmType Quantitative, PAggregate Median ]
      cluster = position X [ PName "Cluster", PmType Nominal ]
      enc = encoding . cluster . plx

  in toVegaLite
        [ gaiaData
        , mark Bar []
        , enc []
        ]


{-|

We start our exploration by combining two visualizations, layering
one on top of the other. The base plot shows the same data as
'basePlot', and then on top we will show a horizontal line that
indicates the median parallax for all the stars in the sample.

<<images/vl/layeredplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCE6BiYZLA4AgXpRZgA4iYshJBiWLLcgqmVVZAASgCCAPoAkgDCXQDKDU0tbWlVGWR4AB4Tza047bOYCgCigyPjqI0r0x1FkPx980sHk6uQJ2AAvh1PEA8zUAAkFELRJg0k5CotCsYXYnB4TDYImo31+RhBTjwHG0ugALEwAFYUcTrKB4WAATzaqAA2h1gJATDgNA1uKVUlApKJoGwxPVUBSrmgsmx+HhoA0hng+OYGZB4ASyPwJiJZKzYAQHkr3hAKVSaQccFhgpAXmAALrvM5iZms9loDqQAkFHl8gUHS5i2AsFg4AwIaUHWT8FmwMRiiVShoARywfvgHAQbH0uvSb3SkFEYkybHNwCeDyAA Open this visualization in the Vega Editor>

@
let plx = position Y [ PName \"plx\", PmType Quantitative, PAggregate Median ]
    cluster = position X [ PName \"Cluster\", PmType Nominal ]

    perCluster = [ mark Bar [], encoding (cluster []) ]
    allClusters = [ mark 'Rule' [] ]

in toVegaLite
      [ gaiaData
      , encoding (plx [])
      , 'layer' (map 'asSpec' [perCluster, allClusters])
      ]
@

For this visualization, the specification starts with the data
source and an encoding, but __only__ for the y axis (which means
that all layered plots use the same encoding for the axis). The
'layer' function introduces the different visualizations that
will be combined, each as there own \"specification\" (hence
the need to apply 'asSpec' to both @perCluster@ and @allClusters@).
Note that there is no x-axis encoding for the 'Rule', since the
data applies to all clusters (i.e. it should span the
whole visualization).

-}
layeredPlot :: VegaLite
layeredPlot =
  let plx = position Y [ PName "plx", PmType Quantitative, PAggregate Median ]
      cluster = position X [ PName "Cluster", PmType Nominal ]

      perCluster = [ mark Bar [], encoding (cluster []) ]
      allClusters = [ mark Rule [] ]

  in toVegaLite
        [ gaiaData
        , encoding (plx [])
        , layer (map asSpec [perCluster, allClusters])
        ]


{-|

This example is similar to 'layeredPlot' but includes an x-axis
encoding for the second layer. We use this to show the range of the
data - so the minimum to maximum parallax range of each cluster - with
the 'Rule' type. The difference to the previous plot is that an
extra positional encoding is added ('Y2') to define the end point
of each line ('Y' is used as the start point).

<<images/vl/layereddiversion.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFACYCGKpGYoEEkArgE4A2VMKKADgM7oD0fRqQDuAOkTwU0egCN63WIwDGAewB2KWBtGqAtnwAiK+ogBCTANaw+0AG6xEpPrtLdNjG-cd8yFPo-hSAFpSYIA2AEYwkIicIIoZZlgI0lE1FSC4UmJFURRuW0hCWigAMxVGFzRMGhKoTlJGBSpauqgAcRdENjV6XRlFIqI2yAAlAEEAfQBJAGFRgGUevoHGIba6TmYAD2X+weKNyAMAURn5pcxIXv21w5HYSa3dq5vVyGHaAF9Pn4gvw6QYTwYhSKi4AhESAAEm4SjgLjYqC4vAEXlSEikslE8BUfDhCKc6KCzEk1lsABZRAArbjqdZQZikACeg0wAG1Pq1aJAXIwLC1INx4AAvWBUABMBCgKGZnHFVyU8GUSUgAM+dC0qmI8DU3RqGp5zMFpXgsGYxDYzyKUFIiEQjAc5AVUF0sB1pDUNsgsvlbAAjvRPShJOR4PY1Ya-t97tRDbzGgKrox6KrY5q1Nrdfq4xsoMyJSazRarTtvXaHU7NGwXLt1XnIMaapBTebLVdrfhbfbHY5q1ddLrvb6XZBA8HQyGI9GSjOwNGALqArUqHV6wUvagt4vtqCzZjydzDuWj9KDtSkVhfetQVRqU054A-L5AA Open this visualization in the Vega Editor>

@
let plx op = position Y [ PName \"plx\", PmType Quantitative, PAggregate op ]
    cluster = position X [ PName \"Cluster\", PmType Nominal ]

    median = [ mark Circle [ 'MSize' 20 ]
             , encoding (plx Median [])
             ]
    range = [ mark Rule [ ]
            , encoding
                . plx 'Min'
                . position 'Y2' [ PName "plx", PAggregate 'Max' ]
                $ []
            ]

in toVegaLite
      [ gaiaData
      , encoding (cluster [])
      , layer (map asSpec [ median, range ])
      , width 300
      , height 300
      ]
@

The 'MSize' option is used to change the size of the circles so that they
do not drown out the lines (the size value indicates the area of the mark,
and so for circles the radius is proportional to the square root of this
size value; in practical terms I adjusted the value until I got something
that looked sensible).

Note that the y axis is automatically labelled with the different
operation types that were applied - median, minimum, and maximum -
although there is no indication of what marks map to these operations.

-}
layeredDiversion :: VegaLite
layeredDiversion =
  let plx op = position Y [ PName "plx", PmType Quantitative, PAggregate op ]
      cluster = position X [ PName "Cluster", PmType Nominal ]

      median = [ mark Circle [ MSize 20 ], encoding ( plx Median []) ]
      range = [ mark Rule [ ], encoding . plx Min . position Y2 [ PName "plx", PAggregate Max ] $ [] ]

  in toVegaLite
        [ gaiaData
        , encoding (cluster [])
        , layer (map asSpec [ median, range ])
        , width 300
        , height 300
        ]


{-|

In this example (adapted from an example provided by Jo Wood)
I display the same data as in 'starCount', but
as two layers: the first is a histogram (using the 'Bar' mark),
and the second displays the count value as a label with the
'Text' mark.

<<images/vl/layeredcount.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxhIBzWdAVwAcAjATz0MgGEAba5aAU1kgBdADRR45Sr3Lw+LAmXS0WkAMY1E0SKMjxkytdQ2QAvoOPERxSAAteAS3LXN+AEwBWAAzCraxKgcsZABudrwA7oGQPFQA1rwsiNQcHMbG3hCQACYy8IHEGdSwHMpO0LTIuAD0lQhhAHTkdtDW1PTc-L58GnVqWJUAIjTkAEKFcZXWQVLwlVi6fLATU9KV2dAz0nbwALTwOwBsAIz7u4ce2+v0HLyH8HWI6Nu28Jn8ddDIQVr5UBjYMnkSCRILR4LBkPF8KAgUDIABxObkZSJLD0fjfGHAgBKAEEAPoASTYWIAysjqKj0elMVBaBwAB7kykCamYyD9ACihOJZPwkBRaJZP1hvDxdMZfIF6OFYHMQLlsupkDCdkyzRYAGYPF4rAASZAqWxzErQMoVarLO6NZqtOp2dCVA1GmaW7YcJq8SpBAAsdQAVsh0IgMVBoE1rpE+PTnFAAHIUwVgdCoMA8MHIMC0fhgFRcHhU35B6AkuwAL0hYEOAA40lYOPBGOj8EQgdCYZA5rAYsp6GCQyLEGpMnZEEioTKMmoOJhATSMv5eBxMspONwFv2aVFMDGyLxB+hh6PlMxtJhXgI+a8DXvD0ja3OMtBGFnkegsCP4MVWZvruQb5FQUyW8WDObRonQOI2HQacLygSgGxMCdFSQyAJVIJD51CJcVzzddvzZQNYB3SA9yHEcxygE8oDPJsoCvFQb3Ikx8PbJ8X0lN8Py-DCxHpOw9HwRJkiQ+9N2YKFIAXbC+QMIxtDYitIAAR2oJAw3WMMpi0Xj+ISJIUhlBUIFEiA21hTtuwkzJxLAbZ9nk59FKjTQTOBUiDyY8c5xgXho0iKTlxk9RNAc9ioBUtSmhkOwtNc2E0LMzcApwtcqR4rciMidzgL5KjIBo2Csl4a9EGAuLWMc1930QT8NzZeA+IEsAhI4ESWIyGyyGSoLDBC0NKr5CKNCizT4m0BrdME-SjJIGaFUEEBjCAA Open this visualization in the Vega Editor>


@
let trans = transform
            . aggregate [ opAs Count \"\" \"count\" ]
                        [ \"Cluster\" ]

    chanSort = [ ByChannel ChY, Descending ]

    baseEnc = encoding
              . position X [ PName \"Cluster\"
                           , PmType Nominal
                           , PSort chanSort
                           , PAxis []
                           ]
              . position Y [ PName \"count\"
                           , PmType Quantitative
                           , PAxis []
                           ]

    barEnc = baseEnc
             . color [ MName \"Cluster\"
                     , MmType Nominal
                     , MLegend [ 'LStrokeColor' \"gray\"
                               , 'LPadding' 10
                               ]
                     , MSort chanSort
                     ]

    labelEnc = baseEnc
               . 'text' [ TName \"count\", TmType Quantitative ]

    barSpec = asSpec [ barEnc [], mark Bar [] ]
    labelSpec = asSpec [ labelEnc [], mark 'Text' [ 'MdY' (-6) ] ]

    cfg = configure
          . configuration (ViewStyle [ViewNoStroke])

in toVegaLite [ width 300
              , height 250
              , cfg []
              , gaiaData
              , title \"Number of stars per cluster\" [ TFontSize 18 ]
              , trans []
              , layer [ barSpec, labelSpec ]
              ]
@

Both axes have been dropped from this visualization since
the cluster name can be found from the legend and the
count is included in the plot. The same sort order is
used for the X axis and the color mapping, so that its
easy to compare (the first item in the legend is the
cluster with the most counts). Note that this changes the
color mapping (cluster to color) compared to previous
plots such as 'parallaxBreakdown'.

-}

layeredCount :: VegaLite
layeredCount =
  let trans = transform
              . aggregate [ opAs Count "" "count" ]
                          [ "Cluster" ]

      chanSort = [ ByChannel ChY, Descending ]

      baseEnc = encoding
                . position X [ PName "Cluster"
                             , PmType Nominal
                             , PSort chanSort
                             , PAxis []
                             ]
                . position Y [ PName "count"
                             , PmType Quantitative
                             , PAxis []
                             ]

      barEnc = baseEnc
               . color [ MName "Cluster"
                       , MmType Nominal
                       , MLegend [ LStrokeColor "gray"
                                 , LPadding 10
                                 ]
                       , MSort chanSort
                       ]

      labelEnc = baseEnc
                 . text [ TName "count", TmType Quantitative ]

      barSpec = asSpec [ barEnc [], mark Bar [] ]
      labelSpec = asSpec [ labelEnc [], mark Text [ MdY (-6) ] ]

      cfg = configure
            . configuration (ViewStyle [ViewNoStroke])

  in toVegaLite [ width 300
                , height 250
                , cfg []
                , gaiaData
                , title "Number of stars per cluster" [ TFontSize 18 ]
                , trans []
                , layer [ barSpec, labelSpec ]
                ]


-- TODO: can I plot the mean / median of the parallax for each cluster

{-|

As promised earlier (in 'skyPlot'), now that we have layers, we can
add graticules to a projection. In this case I create two graticule layers,
the \"base\" layer (@grats@), which creates the grey lines that cover
the map - using a spacing of 60 degrees (4 hours) for longitude and
15 degrees for latitude - and then an extra layer (@grats0@), which shows blue lines
at longitude seprations of 180 degrees
and latitude spacings of 90 degrees. In this case the central horizontal and
vertical lines represent 0 degrees, and the one at the left shows
-180 degrees. There are no latitude lines for -90 or +90 since the
default is to stop at ±85 degrees (see 'GrExtent' for a way to
change this).

I added the second graticule layer to see if I could get by without
labels for the grid lines, but decided this did not work out too well,
so ended with two layers, one each for the Right Ascension and
Declination values, using 'dataFromColumns' to manually create the
label positions and label content to display with the 'Text' mark.

<<images/vl/skyplotwithgraticules.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMAWADDgNOFAO7wAmK0GYAzAKwFGQAkAzgMZwC2AhtTChQAHVugD0YgG6xEPAHSJ4lAK4AjOfAD2Yjtx5SZPALQAbJbClY5AK1aaAdpEIRIQgE6brsdii2PMwJAoAJ5CsPxcsG7sPCiabpAAvs5QJjzBUdQA2kQQoBAFULxuANbU+YUFkKwoHiWwAOrklNQ4cnQplVA1deGYkIhusMFOuZXVtZr1APJCPOxKI5htHWOFQaF9UIiwmqzQPGGQa2DJJ5BksXwBA26x8OzKJluBNbBC2QBsBGAAjHQAXUSiTGZ0KgWKZX6Oz2ByO+CglxQ1zAgUG90ez3KE3e2V+AA4fgBOHBA4GdVHnWo8eysABm8S42ROeRZVR4rH4JgcihQyjI4QpXUgMRMmNiWwusWUXDkACUAIIAfQAkgBhOUAZTAAD4-oSwAB+MBImXy5XqrVgIy0b5gTCm2WK1UazXHLoQEFdAFCoo8Ur8BbRLG+qXI8psyDKNwmfioYSiCR3YgKJTQNTKVhRdgOFCwewoOQ5rhiAAimmUiAAQtH6mJoNJZGJeG83PXG-okfpZPBjDxjJ9fp8jDxfjgjMjVM9fvJ7JojHAeAK3HIUKxJKMPVAGW5eGgAmyXHM3FmI1v1gBxXiIfj2GWqTK+4XOy1u-p3rgPhJP8ZCEwAD1ve9H0PKpSwAURdLUgM-EDzyqWAlT-QD32AhJQK9D1MMKMFxnzHMyHgewbwPLcRU0bkElI89IDpeBYBMMh+GQzcaI4HgsRuXRYEifhJHgNxyHgTkEQ2I5+m5G9cLIkJxKgABHZQaV8ZFfGkViyOeHZ7CYm4VM4qBjw4tJAOwyppPGNIVP5F5aPoxj+AgqC3VE2TJUU5SlHudSLPWOIKN8D4bjohjdKgNUTEzPNvygNzb00LgiI4pIf0gbliKUGzsRChyJJ5TKBScWLNn4DyCy8tTwjMz1QQpCp1khbEyCWMAOigDikH8KB2HzaKirEyU83-NBRNUDkGKIwbNA+XzESuM8PUgSQOOUWBOUwHJ4MCEwp34IcqFElDrV+AAmH5IBanBZvBNLdv6M6DqgI6jG+UTLuugptruqAsEeyAjteqB3p-PJbtjfp8T+o7TvO962QBE5ZsgfDNEI4iFuFIb91ROzQq5XbXJK98EqS2MPrS+4+UK4L7LCi7+ri-oypU7yqtS9LeSymm8f6QDCbkyBmYq+AfMR2qxnqqpGuo4UyGe1YyI6xAusgQTkBGyNGdi2Bho08YxqzMx7ElRKyDILFEdDLsMfGZbIrW5ktrB-gjCwOh+qO2HqFdugPtBnbwZ+93DtaN7qDd6qCgRrokZRtGSMpGSdexwIcrpgOGaJqA50S+xkvJqyCtstP+BGfn3KU8rVJFtnIw5ovstp-g+eKgWher0WY9BIgfSYHN7DohPgBBRIgA Open this visualization in the Vega Editor>

@
let trans = transform
                . calculateAs
                  "datum.RA_ICRS > 180 ? datum.RA_ICRS - 360 : datum.RA_ICRS"
                  "longitude"

    axOpts field = [ PName field, PmType Quantitative ]

    enc = encoding
            . position Longitude (axOpts "longitude")
            . position Latitude (axOpts \"DE_ICRS\")
            . color [ MName \"plx\"
                    , MmType Quantitative
                    , MScale [ SType ScLog
                             , SScheme \"viridis\" []
                             ]
                    , MLegend [ LTitle "parallax" ]
                    ]
            . tooltip [ TName \"Cluster\", TmType Nominal ]

    stars = asSpec [ gaiaData, trans [], enc [], mark Circle [] ]
    grats = asSpec [ 'graticule' [ 'GrStep' (60, 15) ]
                   , mark 'Geoshape' [ MStroke "grey"
                                   , 'MStrokeOpacity' 0.5
                                   , 'MStrokeWidth' 0.5
                                   ]
                   ]
    grats0 = asSpec [ graticule [ GrStep (180, 90)
                                ]
                    , mark Geoshape [ ]
                    ]

    raData = dataFromColumns []
                 . dataColumn "x" (Numbers [ -120, -60, 60, 120 ])
                 . dataColumn "y" (Numbers [ 0, 0, 0, 0 ])
                 . dataColumn "lbl" (Strings [ "16h", "20h", "4h", "8h" ])

    decData = dataFromColumns []
                 . dataColumn "x" (Numbers [ 0, 0 ])
                 . dataColumn "y" (Numbers [ -45, 45 ])
                 . dataColumn "lbl" (Strings [ "-45", "45" ])

    encLabels = encoding
                . position Longitude (axOpts "x")
                . position Latitude (axOpts "y")
                . text [ TName "lbl", TmType Nominal ]

    raLabels = asSpec [ raData []
                      , encLabels []
                      , mark Text [ 'MAlign' 'AlignCenter'
                                  , 'MBaseline' 'AlignTop'
                                  , MdY 5
                                  ]
                      ]
    decLabels = asSpec [ decData []
                       , encLabels []
                       , mark Text [ MAlign AlignRight
                                   , MBaseline 'AlignMiddle'
                                   , 'MdX' (-5)
                                   ]
                      ]

in toVegaLite [ width 400
              , height 350
              , projection [ PrType Mercator ]
              , layer [ grats, grats0, stars, raLabels, decLabels ]
              ]
@

The layers are drawn in the order they are specified, which is why the
grid lines are drawn under the data (and labels).

You can see the distortion in this particular projection (the
<https://en.wikipedia.org/wiki/Mercator_projection Mercator projection>),
as the spacing between the latitude lines increases as you move towards the
bottom and top of the plot. There are a number of other projections you
can chose from, such as the 'Orthographic' projection I use in
'concatenatedSkyPlot'.

-}

skyPlotWithGraticules :: VegaLite
skyPlotWithGraticules =
  let trans = transform
                . calculateAs
                  "datum.RA_ICRS > 180 ? datum.RA_ICRS - 360 : datum.RA_ICRS"
                  "longitude"

      axOpts field = [ PName field, PmType Quantitative ]

      enc = encoding
              . position Longitude (axOpts "longitude")
              . position Latitude (axOpts "DE_ICRS")
              . color [ MName "plx"
                      , MmType Quantitative
                      , MScale [ SType ScLog
                               , SScheme "viridis" []
                               ]
                      , MLegend [ LTitle "parallax" ]
                      ]
              . tooltip [ TName "Cluster", TmType Nominal ]
              -- note: opacity doesn't really help here

      stars = asSpec [ gaiaData, trans [], enc [], mark Circle [] ]
      grats = asSpec [ graticule [ GrStep (60, 15) ]
                     , mark Geoshape [ MStroke "grey"
                                     , MStrokeOpacity 0.5
                                     , MStrokeWidth 0.5
                                     ]
                     ]
      grats0 = asSpec [ graticule [ GrStep (180, 90)
                                  ]
                      , mark Geoshape [ ]
                      ]

      -- hmmm, tried to use "ᴴ" / \u1D34 but this does not get converted
      -- properly (is there a UTF-8/16 issue going on, or am I
      -- just failing to understand modern text encoding yet again?)
      --
      -- this also affects ° / \u00b0 so not sure what's going on
      raData = dataFromColumns []
                   . dataColumn "x"
                     (Numbers [ -120, -60, 60, 120 ])
                   . dataColumn "y"
                     (Numbers [ 0, 0, 0, 0 ])
                   . dataColumn "lbl"
                     (Strings [ "16h", "20h", "4h", "8h" ])

      decData = dataFromColumns []
                   . dataColumn "x"
                     (Numbers [ 0, 0 ])
                   . dataColumn "y"
                     (Numbers [ -45, 45 ])
                   . dataColumn "lbl"
                     (Strings [ "-45", "45" ])

      encLabels = encoding
                  . position Longitude (axOpts "x")
                  . position Latitude (axOpts "y")
                  . text [ TName "lbl", TmType Nominal ]

      raLabels = asSpec [ raData []
                        , encLabels []
                        , mark Text [ MAlign AlignCenter
                                    , MBaseline AlignTop
                                    , MdY 5
                                    ]
                        ]
      decLabels = asSpec [ decData []
                         , encLabels []
                         , mark Text [ MAlign AlignRight
                                     , MBaseline AlignMiddle
                                     , MdX (-5)
                                     ]
                        ]

      -- don't know how to change the center

  in toVegaLite [ width 350
                , height 400
                , projection [ PrType Mercator ]
                , layer [ grats, grats0, stars, raLabels, decLabels ]
                ]



{-|

Instead of layering one view on top of another (superposition), we can
place them side by side in a row or column (juxtaposition). In
Vega-Lite this is referred to as /concatenation/:

<<images/vl/concatenatedplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCE6BiYZLA4AgXpRZgA4iYshJBiWLLcgqmVVZAASgCCAPoAkgDCXQDKDU0tbWlVGWR4AB4Tza047bOYCgCigyPjqI0r0x1FkPx980sHk6uQJ2AAvh1PEA8zULqiYkIIhADaHUKpxMOA0DW4pXWnSkomgbDE9VQQM6VzQWTY-Dw0AaQzwfHMqSg8AAnmR+BMRLJ4bACG97phiQV0ZjsQdLoTILAWCwcAYEOSDrJ+HDYGIOSSyQ0AI5YUXwDgINj6SAvKqq17vCDIzAgsEHCFrTUZGEiOEIiobKCo4DMrE4vFmaZE0kCqARKliGkqo2nRlI22sqC1Lkcrk8vmSBpCkVilLOyUHGVyhXy5XqjDp1UAXXekAAJBQhNETA0SOQqLQrGF2JweEw2CJqIXi0Yq048BxtLoACxMABWFHEUMgX0ybERaCeDyAA Open this visualization in the Vega Editor>

@
let enc field = encoding
                 . position X [ PName \"Cluster\", PmType Nominal ]
                 . position Y [ PName field, PmType Quantitative, PAggregate Median ]

    parallaxes = [ mark Bar [], enc \"plx\" [] ]
    magnitudes = [ mark Bar [], enc \"Gmag\" [] ]

    specs = map asSpec [ parallaxes, magnitudes ]

in toVegaLite
      [ gaiaData
      , 'vConcat' specs
      ]
@

The 'hConcat' function would align the two plots horizontally,
rather than vertically (and is used in 'concatenatedSkyPlot').

Note that as the axes are identical apart from the field for the y axis,
the encoding has been moved into a function to enforce this constraint
(this ensures the x axis is the same, which makes it easier to visually
compare the two plots). However, there is no requirement that the
two plots be "compatible" (they could use different data sources).

-}


concatenatedPlot :: VegaLite
concatenatedPlot =
  let enc field = encoding
                   . position X [ PName "Cluster", PmType Nominal ]
                   . position Y [ PName field, PmType Quantitative, PAggregate Median ]

      parallaxes = [ mark Bar [], enc "plx" [] ]
      magnitudes = [ mark Bar [], enc "Gmag" [] ]

      specs = map asSpec [ parallaxes, magnitudes ]

  in toVegaLite
        [ gaiaData
        , vConcat specs
        ]


{-|

The alignment of the plots can be adjusted with 'spacing', which we
use here to remove the vertical gap between the two plots (the
example is written so that we can see the only difference between
the two plot specifications is the addition of @'PAxis' []@ to the
parallax plot).

<<images/vl/concatenatedplot2.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCE6BiYZLA4AgXpRZgA4iYshJBiWLLcgqmVVZAASgCCAPoAkgDCXQDKDU0tbWlVGWR4AB4Tza047bOYCgCigyPjqI0r0x1FkPx980sHk6uQJ2AAvh1PEA8zULqiYkIIhADaHUKpxMOA0DW4pXWnSkomgbDE9VQQM6VzQWTY-Dw0AaQzwfHMqSg8AAnmR+BMRLJ4bACCkoLAFmwKIQmng8G97phiQV0ZjsQdLoTILAWCwcAYEOSDrJ+HDYGIhSSyQ0AI5YeXwDgINj6SAvKr617vCDIzAgsEHCFrY0ZGEiOEIiobKCo4C8rE4vFmaZE0lSqARKliGl6m2nblI938qC1EVCkViiWSBoyuUKumQJX+yBqjVazW6w0YIv6gC670gABIKEJoiYGiRyFRaFYwuxODwmGwRNQa3WjK2nHgONpdAAWJgAKwo4ihkAoJSE8MRYF8Fa+mTYK+ATweQA Open this visualization in the Vega Editor>

@
let enc field flag = encoding
                       . position X ([ PName \"Cluster\", PmType Nominal ] ++
                                     if flag then [ PAxis [] ] else [])
                       . position Y [ PName field, PmType Quantitative, PAggregate Median ]

    parallaxes = [ mark Bar [], enc \"plx\" True [] ]
    magnitudes = [ mark Bar [], enc \"Gmag\" False [] ]

    specs = map asSpec [ parallaxes, magnitudes ]

in toVegaLite
      [ gaiaData
      , 'spacing' 0
      , vConcat specs
      ]
@

Even though we set 'spacing' to @0@ there is still a small gap between
the plots: this can be removed by using @'bounds' 'Flush'@, but we'll
leave using that until the grand finale.

-}

concatenatedPlot2 :: VegaLite
concatenatedPlot2 =
  let enc field flag = encoding
                         . position X ([ PName "Cluster", PmType Nominal ] ++
                                       if flag then [ PAxis [] ] else [])
                         . position Y [ PName field, PmType Quantitative, PAggregate Median ]

      parallaxes = [ mark Bar [], enc "plx" True [] ]
      magnitudes = [ mark Bar [], enc "Gmag" False [] ]

      specs = map asSpec [ parallaxes, magnitudes ]

  in toVegaLite
        [ gaiaData
        , spacing 0
        , vConcat specs
        ]


{-|

In 'skyPlotWithGraticules' I used the 'Mercator' projection to display
the stars on the sky, but promised I would also show you data using the
<https://en.wikipedia.org/wiki/Orthographic_projection_in_cartography Orthographic projection>.

The main specification (that is, the argument of 'toVegaLite') starts
with a change to the plot defaults, using 'configure' to ensure
that no border is drawn around the plot (note that in 'combinedPlot'
I do the same thing, but by setting the stroke color to
@Just \"transparent\"@ rather than @Nothing@). The default data
stream is set up, to ensure we have \"longitude\" and
\"DE_ICRS" values to display. It then has three
versions of the same visualization, varying only on rotation angle and
label, stacked horizontally with 'hConcat'.

Each plot - created with the @rSpec@ helper function - defines
a plot size, uses the 'Orthographic' projection with the
given rotation (the @lambda@ term of 'PrRotate') to change the
center of the display, and then the plot itself is formed from
four layers:

1. 'sphere' is used to indicate the area of the plot covered by the sky
   (filled with a blue variant);

2. graticules are drawn at every 30 degrees (longitude, so 2 hours
   in Right Ascension) and 15 degrees (latitude);

3. the stars are drawn using color to encode the parallax of the
   star and the symbol shape the cluster membership (although the density
   of points is such that it can be hard to make the shapes out);

4. and a label is added at the center of the plot to indicate the
   Right Ascension (the label could be determined automatically from
   the rotation angle, but it was easier to just specify it directly).

Since the data values have two different encodings - 'color' and 'shape' -
there are two legends added. I place them in different locations using
'LOrient': the parallax goes to the right of the plots (which is the
default) and the symbol shapes to the bottom. Both use larger-than-default
font sizes for the text (title and label).

<<images/vl/concatenatedskyplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxQSTJ5k8oAbdRAcwEtoBXAEwFNIAaY8pADG8WkLa140HvkgcpbLADoASgEEA+gEkAwioDKYAHxgAjAA4ADGAD8Yee2XrtewwFowAZgBs1-A8VVTV0DSAEwAF9iAF1+CEgACyEGEWgaInIycigErmZGBLT8T0tLOOzIAHdmDmgEmhKy8PiAB1h0ACsuIWhmBhpgGABPFpkoTDr0RgQWhOYhPih26CkxgjdTACYysB3LaIjywUkhrlh05pIs7IoseFgAawHIVGZaWhpKWnmuACNaNg8XhQaAjMaQRhcdDIBLwUaQQ6XQQOeDPZCzM5jOCAqI3CCIvGkJG3e5PfDXQlQZBwdAPLgAdRqdRoliUAFYjpTINT2nTPtMuEM+MTBDzaVwAPIteBCVhC-Csjkiiig+GySHQ2Hw5UErko57TKTzCRjQbUrgtACyzEQmHSJWBpjZB1xeN1NwpN0gd0enxa6BtaU5Xq4iGSHBtjAGyviyXo53JMYory4tA4ftoAA9hZSKsgRLRTdyhLksOCAG7MWA1ZjUYHDNV0KYI4Nc1XggCObCQvRWvXLQKT8ULkMQ6cTua9mGYoaKS3yhRzk+O8F+qYAYgxoPpmAAvMamAAsrdzMFYhb991EkmzJ7b564m8Q273B+8Q8iSfdXMkvc4RZTNNPgAEQAURcUJ63bT4ux7VgjQHFsk25LVTQ-F4ZyA2QdABc1zjvL1oNkW0sBtUQl2XSAR1DcciWXQRp1nT5fnQaBoHQHACLxKjVw3Lcd33Ggjy4wiH0+IRcOkfCP3iXtCyfF9BPwUx33oz9l2-QkYHQdBaF6FpnkA2jIBwtg8MWBtwRIsiPk07j6CYVh-0MzDjIclh2G4CyiKgWDn3g-seFdQlguyOyIE9CofTJOjJzkeUwCVSjRHyRBxNnM4KLbMFPmkTMg2Q34qFTG1wXYgydS4uQpFRclIHLURAWofACEGWh-k+cx6nrbNlO2esEssCIDiq0Nw0jaNKLyudBiMz52o+KCcuIjibKQyjfycry6rm2QhSWxtID83sEKCkSm0czyANcz5bxBZbfO7fy+2YRDQpud6SE+6Jwm-SLEjyAo50aVsqiZepilKUG2k6bpen6OqfMgCYEimGY5gWetllWdI9mBfY7J404E0IYlIsEaKXPeT4UqEP4ASBe7Do1GE4TO5VqpWNEMVgLFYBxYlwtirTKYnOKxTpRlagh3Z2XO7kaT5dVef25CJclaVZVBFk5eQpGWdQsJcyF+J9Tqw1enEC86vNK0bTtFqHTMZ0Ik+-E73JklfVkf1AyykMw3QCMmEmuK4wd4W4t2qAWizf2uXzUQi3zUsKyrGs6yZ8F6CjE2KiR46Atewc1KorhR1oz2tMY59PmrIH49PSQ11oBSBIPY8ZJBMSfavd54FvLuz2geT+NfITVPot2wvlzarpc1NjLAiD9G8h6jqek7AvW8XDbFyjo5MyTMvO2T1+sxByNPuhy5o0PS5rudIBYtiOMbn9eNbselLMTu1NknuUAJJmSku-LSclHzfzfB+ae5A86yR0npZgBkdo3WwsffCWdPgX3IvAi6HlnKoMXvNBgBCvIHU7JvIub0dSCw9hzUWkctIcASklOKKVGBpVkHTZ8J89br2mmAigRVkAlUQGVdAFVjZVTNqQeqjUuDNUIG1DqsgtjdSgL1XYA0WTDTzpAMaQcJr7zbFwfKC8sJ0A6hQ7Bq1L62VnkaeeRDLGQH2lg2QhcXpvVnqQra11iGyDupZGCVDvFBVoSFS4P04Gcn+rkBcwMoaXDBtLBoySvQwy6D0PoXC5FIxRmjOEGMLLY2kOkLYeNdgjRSScTKLUyYMNJFTD4shab00BGvZmUJWbwn0bIs0PM+YCxCvQ08jCq4UHVlLZkCpdaUXVvyFWQj4jqylDKOUOs2Gnn1j0w2lUOYDIhAgS2Jo0TSDtraEmBAnZOhdHQxp4zmk+wDLXUagdg5RhMVpcOJNJmCEPrHQepdE7WzkSnLgZZPiVmrBGTOITZA5x3lNdeXjTorIoNRMc996LI2rExWQ9dFzX2HJ-Nu49lJ-3-sPMFkBpQIH7sC6lEDyU-xUjAr8ji-zbTkYfZeIRV42M8WE9FeCUJsxxaeQ+plzLXwRVAHBi10JYsrkPR+zFWLsU4kPZufFnztyElS0uEDxIYIxYIFlUCJ4co0vLdiul9IWOMjK0BQqFV2Nwb4y6hDeVoPwf4rplC4LhIRJEt0YyuQTOQiwmg2yuQcLycIDK0kUWHUEfLERYiJFSMpP0mqzwGoMyUa1KiqioCHg0ZALRGx+pQEGno9540Q7fO4oIlxblrEePdaRexyKP7coCa49x8qN7BtFV6shg7jLBILiK7esD1Jumib9GI5RhAMFeF8uRlYuCVHObyMYiAJC0FdmugZ4RIBsFgK0nIbEWjIFwAAekfQgSoSgPIJDYL8MyZxki8OfEoZIWBH3AXQGwRgAAhK9dJH0JAHIweAj67h4Vg-BxDKJH0IeYPANw8AcPeBUrh0wlg3ArH+FwUw8AlC2jcLkeA3BYBKGgMgcsWUXiYDuDNFJ9LRGSooAAcTuFuyAR6sBrhTdxZwArsGKHE+-OlcdiKyb4VpflrgZNiZU9xLgGggUabk5cUKrpdSQAACQQruJ8Qo0B71PsfWh99rBP2-CUH0R9FnENobcN8aQ9nDxKA6MgfoIAIhAA Open this visualization in the Vega Editor (although the link is long, and may not work with Internet Explorer)>

@
let trans = transform
              . calculateAs
                \"datum.RA_ICRS > 180 ? datum.RA_ICRS - 360 : datum.RA_ICRS\"
                \"longitude\"

    axOpts field = [ PName field, PmType Quantitative ]
    legend ttl o = MLegend [ LTitle ttl
                           , 'LOrient' o
                           , 'LTitleFontSize' 16
                           , 'LLabelFontSize' 14
                           ]
    enc = encoding
            . position Longitude (axOpts \"longitude\")
            . position Latitude (axOpts \"DE_ICRS\")
            . color [ MName \"plx\"
                    , MmType Quantitative
                    , MScale [ SType ScLog
                             , SScheme \"viridis\" []
                             ]
                    , legend \"parallax\" 'LORight'
                    ]
            . shape [ MName \"Cluster\"
                    , MmType Nominal
                    , legend \"cluster\" 'LOBottom'
                    ]
            . tooltip [ TName \"Cluster\", TmType Nominal ]

    stars = asSpec [ enc [], mark Point [] ]
    grats = asSpec [ graticule [ GrStepMinor (30, 15) ]
                   , mark Geoshape [ MStroke \"grey\"
                                   , MStrokeOpacity 0.5
                                   , MStrokeWidth 0.5
                                   ]
                   ]

    lblData r h0 =
      let r0 = -r
          lbl = h0 <> \"h\"
      in dataFromColumns []
         . dataColumn \"x\" (Numbers [ r0 ])
         . dataColumn \"y\" (Numbers [ 0 ])
         . dataColumn \"lbl\" (Strings [ lbl ])

    encLabels = encoding
                . position Longitude (axOpts \"x\")
                . position Latitude (axOpts \"y\")
                . text [ TName \"lbl\", TmType Nominal ]
    labels r h0 = asSpec [ lblData r h0 []
                         , encLabels []
                         , mark Text [ MAlign AlignCenter
                                     , MBaseline AlignTop
                                     , MdY 5
                                     ]
                         ]

    bg = asSpec [ 'sphere', mark Geoshape [ MFill \"aliceblue\" ] ]

    rSpec r h0 = asSpec [ width 300
                        , height 300
                        , projection [ PrType 'Orthographic'
                                     , 'PrRotate' r 0 0
                                     ]
                        , layer [ bg, grats, stars, labels r h0 ]
                        ]

    s1 = rSpec (-120) \"8\"
    s2 = rSpec 0 \"12\"
    s3 = rSpec 120 \"4\"

    setup = configure . configuration (ViewStyle [ ViewNoStroke ])

in toVegaLite [ setup []
              , gaiaData
              , trans []
              , 'hConcat' [ s1, s2, s3 ] ]
@

-}

concatenatedSkyPlot :: VegaLite
concatenatedSkyPlot =
  let trans = transform
                . calculateAs
                  "datum.RA_ICRS > 180 ? datum.RA_ICRS - 360 : datum.RA_ICRS"
                  "longitude"

      axOpts field = [ PName field, PmType Quantitative ]
      legend ttl o = MLegend [ LTitle ttl
                             , LOrient o
                             , LTitleFontSize 16
                             , LLabelFontSize 14
                             ]
      enc = encoding
              . position Longitude (axOpts "longitude")
              . position Latitude (axOpts "DE_ICRS")
              . color [ MName "plx"
                      , MmType Quantitative
                      , MScale [ SType ScLog
                               , SScheme "viridis" []
                               ]
                      , legend "parallax" LORight
                      ]
              . shape [ MName "Cluster"
                      , MmType Nominal
                      , legend "cluster" LOBottom
                      ]
              . tooltip [ TName "Cluster", TmType Nominal ]

      stars = asSpec [ enc [], mark Point [] ]
      grats = asSpec [ graticule [ GrStepMinor (30, 15) ]
                     , mark Geoshape [ MStroke "grey"
                                     , MStrokeOpacity 0.5
                                     , MStrokeWidth 0.5
                                     ]
                     ]

      -- make the user work out what the new central location is
      lblData r h0 =
        let r0 = -r
            lbl = h0 <> "h"
        in dataFromColumns []
           . dataColumn "x" (Numbers [ r0 ])
           . dataColumn "y" (Numbers [ 0 ])
           . dataColumn "lbl" (Strings [ lbl ])

      encLabels = encoding
                  . position Longitude (axOpts "x")
                  . position Latitude (axOpts "y")
                  . text [ TName "lbl", TmType Nominal ]
      labels r h0 = asSpec [ lblData r h0 []
                           , encLabels []
                           , mark Text [ MAlign AlignCenter
                                       , MBaseline AlignTop
                                       , MdY 5
                                       ]
                           ]

      bg = asSpec [ sphere, mark Geoshape [ MFill "aliceblue" ] ]

      rSpec r h0 = asSpec [ width 300
                          , height 300
                          , projection [ PrType Orthographic
                                       , PrRotate r 0 0
                                       ]
                          , layer [ bg, grats, stars, labels r h0 ]
                          ]

      s1 = rSpec (-120) "8"
      s2 = rSpec 0 "12"
      s3 = rSpec 120 "4"

      setup = configure . configuration (ViewStyle [ ViewNoStroke ])

  in toVegaLite [ setup []
                , gaiaData
                , trans []
                , hConcat [ s1, s2, s3 ] ]


-- $intro-repeat
-- Creating the same plot but with a different field is common-enough
-- that Vega-Lite provides the 'repeat' operator.


{-|

The 'concatenatedPlot' example can be extended to view the
distribution of several fields - in this case Right Ascension,
Declination, parallax, and magnitude:

<<images/vl/repeatplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEBOCmAOsCGAXSAuMwYHsDuGYA2pAEoCCA+gJIDCpAypADRQAiAojfU65PABsAHiygBxALZIA5pAC6AX2bgoAZ0QBjQqAgRIU6AGtCkAEZJoLFXoAmqJNuu7IAV2gCTACxQp4q9AD0AdBIeAB00gCWKJ4upi6qsNAaOAB2KLDpYSkSAWw4LtIAQm6GsAGeAG6w0kgBUqoZ0BXVtQF2KHW1kUgAtEh9AGwAjIP9wwAMvZ2mArDDSGGpOL2eyDZJYSiqlVa6zgBmONBSaJg6+87wFomOl5eQkjImqS4Spkl7984U3Iwvbw+lmU3yuwgB70+INB7C4dH+mEgr0hwKc90gsEoghEiORQMgaN0CkJxP2SicGNSKRskVSsnOhMgOKwkAOkVgAhsJloAgSTVEkBQAE9EC8cBJaUgPOT0UK7t9WezOdoYAhkGcoNB8JAZQqZNI4LUMiYJLAaUhUl90cLRYiAI4uC0oaKoSLVAn3UlElS6yAAElUGjWUi8Pj8gQCrUWURicTCkRwAUDwbqUd6Ami5UqABYwgArVRpL6QFKpNn0rDEhRAA Open this visualization in the Vega Editor>

@
let enc = encoding
           . position X [ PName \"Cluster\", PmType Nominal ]
           . position Y [ 'PRepeat' 'Row', PmType Quantitative, PAggregate Median ]

    spec = asSpec [ gaiaData
                  , mark Bar []
                  , enc [] ]

    rows = [ \"RA_ICRS\", \"DE_ICRS\", \"plx\", \"Gmag\" ]

in toVegaLite
      [ 'repeat' [ 'RowFields' rows ]
      , 'specification' spec
      ]
@


This more compact specification replaces the data field name
(for example @'PName' \"plx\"@) with a reference to the repeating field
('PRepeat') either as a 'Row' or 'Column' depending on the desired
layout. We then compose the specifications by providing a set of
'RowFields' (or 'ColumnFields') containing a list of the fields to which
we wish to apply the specification (identified with the function
'specification' which should follow the repeat function provided to
toVegaLite).

-}

repeatPlot :: VegaLite
repeatPlot =
  let enc = encoding
             . position X [ PName "Cluster", PmType Nominal ]
             . position Y [ PRepeat Row, PmType Quantitative, PAggregate Median ]

      spec = asSpec [ gaiaData
                    , mark Bar []
                    , enc [] ]

      rows = [ "RA_ICRS", "DE_ICRS", "plx", "Gmag" ]

  in toVegaLite
        [ repeat [ RowFields rows ]
        , specification spec
        ]


{-|

If we want to plot more than one map from the same table of data
we need to do the lookup in the other order, using lookup to add the
geographic data to the data table. Charting this way requires
specifiying a few things differently than in the previous
choropleth example ('choroplethLookupToGeo'):

  * We're using 'LuAs' in 'lookup', rather than 'LuFields', which lets
    us use all the fields (columns) in the source rather than a specified
    subset.
  * We use a different set of geographic features (state rather than county
    outlines) from @usGeoData@.
  * The plot is defined as a 'specification', but does not directly refer
    to the value being displayed. This is set \"externally\" with the
    call to 'repeat'. Since we have just had an example with
    'RowFields', this time we use 'ColumnFields' to stack the maps
    horizontally.
  * Since the different fields have vastly-different ranges (a maximum of
    roughly 0.01 for \"engineers\" whereas the \"population\" field is
    a billion times larger), the color scaling is set to vary per field
    with 'resolve'.

<<images/vl/choroplethlookupfromgeo.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEBOCmDOB7ANgN1pAXGYl4GMBDZDbXfFRaLKASwDsATWAB1ifYBdIBfHgGnBQ4bQtzKQKyAK4BbejQDakFohbTkY2ogX8o7AOYNYsaPEh7IAC2nRotIvQSQAugKF42+GqAgRInNCE9PAAZlSySkJ+ONExUITm2JAGsIgWcTGQodCIkWSZ8ZCMYoQ+hfFQtsg01pycLPCYAPTNQQDuAHRGnDYARtLwphT0nFydFLLN6AaE07CzzbKJY9DNjIj48OulzYMAtACMAAyynQBWSLoVReHQy+I42bBitqRQ8JxizpacAJ5sWqcNSIS46Xg3CDuSpZADWsD+tVojEgN2hMMgyEQiFh0hYSJRhR4cRcgiyy2gsJ8eECOPekD6mnwVMsn1y8IA8ixCPhaP8aMdOgAmX4A+mpRDwKyEQHo-wlL7lSqQaq1Kz1RotNqELo9fqDYY6MajCZ5eaLGZzZafUzrTbbBVzVTqTScbT0AD6hmMpngHpsdgcwQQE3gqFRMTlUHayN6NAAzMdjmS-CpcudYPg3eCJP9AcliH1fQBVeBlKOQdgURgMAxKopSKj15WhWiwZAoiQiF6PSQoOQKKNZPP0gCO0mCbq+bvQGRhUBIqSY1KobdGQLUFigBiCNa4ABlDHHsInjsTKkOPtL809W+3O9u0luAmLahKwYO4uewOjIAASAgrFgZY1Q1JpWktbo+X6TptGaQDgLmS19mQPlYGmAAWC4rjnKARlbOsyGJHggA Open this visualization in the Vega Editor>

@
let popEngHurrData = dataFromUrl \"https:\/\/raw.githubusercontent.com\/vega\/vega\/master\/docs\/data\/population_engineers_hurricanes.csv\" []

    plotWidth = 300

    viz = [ popEngHurrData
          , width plotWidth
          , transform
            . lookup \"id\" (usGeoData \"states\") \"id\" ('LuAs' \"geo\")
            $ []
          , projection [PrType AlbersUsa]
          , encoding
            . shape [MName \"geo\", MmType GeoFeature]
            . color [MRepeat 'Column', MmType Quantitative, MLegend [LOrient 'LOTop', 'LGradientLength' plotWidth]]
            $ []
          , mark Geoshape [MStroke \"black\", MStrokeOpacity 0.2]
          ]

in toVegaLite
   [ specification $ asSpec viz
   , resolve
     . resolution (RScale [(ChColor, Independent)])
     $ []
   , repeat ['ColumnFields' [\"population\", \"engineers\", \"hurricanes\"]]
   ]
@

By moving the legend to the top of each visualization, I have taken
advantage of the fixed with (here 300 pixels) to ensure the
color bar uses the full width (with 'LGradientLength').

-}

choroplethLookupFromGeo :: VegaLite
choroplethLookupFromGeo =
  let popEngHurrData = dataFromUrl "https://raw.githubusercontent.com/vega/vega/master/docs/data/population_engineers_hurricanes.csv" []

      plotWidth = 300

      viz = [ popEngHurrData
            , width plotWidth
            , transform
              . lookup "id" (usGeoData "states") "id" (LuAs "geo")
              $ []
            , projection [PrType AlbersUsa]
            , encoding
              . shape [MName "geo", MmType GeoFeature]
              . color [MRepeat Column, MmType Quantitative, MLegend [LOrient LOTop, LGradientLength plotWidth]]
              $ []
            , mark Geoshape [MStroke "black", MStrokeOpacity 0.2]
            ]

  in toVegaLite
     [ specification $ asSpec viz
     , resolve
       . resolution (RScale [(ChColor, Independent)])
       $ []
     , repeat [ColumnFields ["population", "engineers", "hurricanes"]]
     ]


{-|

We can combine repeated rows and columns to create a grid of
views, such as a scatterplot matrix, adding in color
encoding to separate out the clusters:

<<images/vl/splomplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEBOCmAOsCGAXSAuMoIRgewHcMwBtSAJQEEB9ASQGFyBlSAGigBEBROxl9yPAA2ADzZQA4gFskAc0gBdVuByQAxniEBXKQDtiZKr2bjI3Y-yjCxA6XMUqAvstwBnRGuLZVM6AGsvSFcASwAvWGIAVgEUAE9EYkE8YN00ZxVcABNUJC8M1S1oIUSACxQUeFd0AHpq6CQCADpZYJQSrQAjLVdYaA1U2FTGjSlqjjwtWQAhQr9YapKAN1hZJGqZVxReheXV6uyUNdXgpABaJDOANgBGS-PrgAZTw46hWGukRt08U5LkTN6jRQrkWbHyuAAZnhoDI0JhvDhVPAkNAenlERjJDJ5JhILodB1emDMYiKDQGCZcfipIToMSSbhrIlqbT6QyzDwKSwqQSiS4GVBYNQmTyaUTwThHBKpYj0qTBhpMikcVgJepNNDAhDgrAhJlEvRtJs+VA4gkqXgpCkkMU5RjIGJ4ZBtbr9U64IhUIkNNo9JBnKb4hFcQBHLRIVKtVDBZb+-mk2JanV6wIe5BwqDQQhxwPmqBhiMoKNF2MyyVOfmQAAkrjUfxkpXKlRq1V2nxabU6jWCeGqtfrazbpyErXmiwALI0AFauPD6Sv9bUq4BSxxAA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PRepeat Column, PmType Quantitative ]
            . position Y [ PRepeat Row, PmType Quantitative ]
            . color [ MName \"Cluster\", MmType Nominal ]

    spec = asSpec [ gaiaData
                  , mark Point [ MSize 5 ]
                  , enc [] ]

    fields = [ \"RA_ICRS\", \"DE_ICRS\", \"plx\", \"Gmag\" ]

in toVegaLite
      [ repeat [ RowFields fields, ColumnFields fields ]
      , specification spec
      ]
@

To be honest, this is not the best dataset to use here, as
there is no direct correlation between location (the @RA_ICRS@
and @DE_ICRS@ fields) and the other columns, but it's the
dataset I chose, so we are stuck with it.

Once you have sub-plots as a specification, you can combine
them horizontally and vertically to make a dashboard style
visualization. Interested parties should check out the
<https://github.com/gicentre/elm-vegalite/tree/master/docs/walkthrough#building-a-dashboard-1240 Building a Dashboard> section of the
Elm Vega-Lite Walkthrough for more details.

-}

splomPlot :: VegaLite
splomPlot =
  let enc = encoding
             . position X [ PRepeat Column, PmType Quantitative ]
             . position Y [ PRepeat Row, PmType Quantitative ]
             . color [ MName "Cluster", MmType Nominal ]

      spec = asSpec [ gaiaData
                    , mark Point [ MSize 5 ]
                    , enc [] ]

      fields = [ "RA_ICRS", "DE_ICRS", "plx", "Gmag" ]

  in toVegaLite
        [ repeat [ RowFields fields, ColumnFields fields ]
        , specification spec
        ]


-- $intro-interactivity
-- Interaction is enabled by creating /selections/ that may be combined with
-- the kinds of specifications already described. Selections involve three
-- components:
--
--  * __Events__ are those actions that trigger the interaction such as
--    clicking at a location on screen or pressing a key.
--
--  * __Points of interest__ are the elements of the visualization with
--    which the interaction occurs, such as the set of points selected
--    on a scatterplot.
--
--  * __Predicates__ (i.e. Boolean functions) identify whether or not
--    something is included in the selection. These need not be limited
--    to only those parts of the visualization directly selected through
--    interaction.

{-|

The next several plots show different types of selection -
select a single point, a range of plots, or follow the mouse - and
all have the same basic structure. To avoid repetition, and mistakes,
I am going to introduce a helper function which creates the
plot structure but without the selection definition, and then
use that to build up the plots.

The helper function, 'selectionProperties', takes two arguments, which are
the selection name and the plot title. The selection name is used
to identify the selection, as a visualization can support multiple
selections, and the plot title has been added mainly to show some
minor customization (the use of 'TOrient' to move the title to the
bottom).

The definition of this helper function is:

@
selectionProps selName label =
  let posOpts field = [ PName field
                      , PmType Quantitative
                      , PScale [ 'SZero' False ]
                      ]

      enc = encoding
               . position X (posOpts \"Gmag\")
               . position Y (posOpts \"plx\")

               . color [ 'MSelectionCondition' ('SelectionName' selName)
                           [ MName \"Cluster\", MmType Nominal ]
                           [ 'MString' "grey" ]
                       ]

               . 'opacity' [ MSelectionCondition (SelectionName selName)
                            [ 'MNumber' 1.0 ]
                            [ MNumber 0.3 ]
                         ]

               . 'size' [ MSelectionCondition (SelectionName selName)
                            [ MNumber 40 ]
                            [ MNumber 5 ]
                         ]

      trans = transform
                 . 'filter' ('FExpr' \"datum.DE_ICRS < -20\")

  in [ gaiaData
     , trans []
     , mark Point []
     , enc []
     , title label [ TOrient SBottom ]
     ]
@

The three non-selection-related features added here are that
'SZero' is used to tell Vega Lite that we do not need 0 displayed
on either axis, which leads to a \"tight\" bounding box around
the data, a 'filter' is used to select a subset of rows, namely
only those with a declination less than -20 (via 'FExpr'),
and the plot title is moved to the bottom with 'TOrient'.

The main change is that the selection is used in the encoding section,
identified by name, using 'SelectionName' and the supplied
argument. It is used as a filter for the encoding section, where
'MSelectionCondition' defines the properties to use
when the selection occurs (the first list of properties)
and when it does not (the second list). This is used for
three different encodings:

 - 'color', where the selected star is labelled by its
   cluster color, and all the other are grey;

 - 'opacity', so that the selected star is fully opaque
   whereas un-selected stars are partially transparent;

 - and 'size', so that the selected star is much bigger
   than the others.

When no selection has been made - such as when the visualization
is first created - then all points are encoded with the
\"selected\" case (so colorful, fully opaque, and large in this
case).

-}

selectionProperties ::
  T.Text
  -- ^ The selection name
  -> T.Text
  -- ^ The title for the plot
  -> [PropertySpec]
selectionProperties selName label =
  let posOpts field = [ PName field
                      , PmType Quantitative
                      , PScale [ SZero False ]
                      ]

      enc = encoding
               . position X (posOpts "Gmag")
               . position Y (posOpts "plx")

               . color [ MSelectionCondition (SelectionName selName)
                           [ MName "Cluster", MmType Nominal ]
                           [ MString "grey" ]
                       ]

               . opacity [ MSelectionCondition (SelectionName selName)
                            [ MNumber 1.0 ]
                            [ MNumber 0.3 ]
                         ]

               . size [ MSelectionCondition (SelectionName selName)
                            [ MNumber 40 ]
                            [ MNumber 5 ]
                         ]

      trans = transform
                 . filter (FExpr "datum.DE_ICRS < -20")

  in [ gaiaData
     , trans []
     , mark Point []
     , enc []
     , title label [ TOrient SBottom ]
     ]


{-|

The actual plot just requires the selection information to be
defined and then added to the plot properties:

<<images/vl/singleselection.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjt1h0NtWJ2HY6gAcS1g-URWSJo1k72hAEF+YRErm7uLB6gDcgAPD5YW60b4PSDcV6iIEg+4-SBUHh-QH4SDXYF3XYQRS7HHY76QAAkyDisy0bg8Xl8PnqE1C4SibFI6B8JLJFVpUnIYUKxQALGwAFbIRJwyCxUZxaDMxB2QykOIFehy6AATwMVH0yBMwVGSkUBOl0D1+GI1H+ZlRIiokugYHgYCMJjMqigmFIyUtUEi6A86BwBo0CMQCXoOqOewS5EwEeOkGK8HIzE1qOCsCoqrhcYSiDD0tFpqxezINuVqIESeqXyLlglVClMv0BgVSqzJxg6pTUCmWBMicgRbxDkDce1AC8u8QE0muwBWV2QHN5xum+OJ5N0PlyBd1hsF34tqjKxQjvboAzwOJhTOr6cb-ByNgAZgXS7CK7AU-XXe6O5t9fzWVUWbRUj31MEoBRT8SA9cgyzOC5zCgElE0nSAJ1gdA6FQRNYgNKA1Q1fQAEdmCQI0mFIUolAgyAb2gks4KbAEkPFOJULlDCsPwHDyDwhdCK7SBSPIsJKOoodT0XRIyEuU0cUUIA Open this visualization in the Vega Editor>

@
let selLabel = "picked"
    sel = 'selection'
            . 'select' selLabel 'Single' []

in toVegaLite (sel [] : 'selectionProperties' selLabel "Select a point")
@

The 'selection' function is used to define the selection, via one or
more applications of the 'select' function. The form of 'select' is
that the selection is named, in this case we use @\"picked\"@, and the
type is given (a 'Single' click), and then options, which in our case
there aren't any, so an empty list is used.

Note that @hvega@ does not track the selection names, and will allow
you to use a name that you have not defined.

-}

singleSelection :: VegaLite
singleSelection =
  let selLabel = "picked"

      sel = selection
              . select selLabel Single []

  in toVegaLite (sel [] : selectionProperties selLabel "Select a point")


{-|

The only change here is to add a property to the selection - that
is @'Nearest' True@ - which means that the nearest point to the
click will be highlighted.

<<images/vl/nearestselection.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjt1h0NtWJ2HY6gAcS1g-URWSJo1k72hAEF+YRErm7uLB6gDcgAPD5YW60b4PSDcV6iIEg+4-SBUHh-QH4SDXYF3XYQRS7HHY76QAAkyDisy0bg8Xl8PnqE1C4SibFI6B8JLJFVpUnIYUKxQALGwAFbIRJwyCxUZxaDMxB2QykOIFehyxBzWBUap0ODMKiqKDQACeBio+mQJmCoyUigJ0uglvwxGo-zMqJEVEl0DAqu0Gs9RhMZj1kEwpGSLqgkXQHnQOGtGgRiAS9HNRz2CXImFTx0gxXg5B1+mC6oNcOzCUQyeloodWL2ZHdytRAnz1S+tcsEqoUpl+gMCqVpZOMCNJtRUywJjzkFreIccezZoAXqOwMRc-mVwBWIPlys9h05vMF-B8uRBzvd6u-ftUZWKed7dAGeBxMIlg-r49gORsADMO8SPcrzXI8V26c93S7KtZVRPtFVvK0wSgFFVxIUNyEbM4LnMKASTzFdiGXWB0DoVA81ia19RHfQAEdmCQW0mFIUolCQyB31Q+sMN7AEcPFOJ8LlIiSPwMjyAooNDWNWj6NMMImJY2cH0gcsyEuB0cUUIA Open this visualization in the Vega Editor>

@
let selLabel = "picked"
    sel = selection
            . select selLabel Single [ 'Nearest' True ]

in toVegaLite (sel [] : selectionProperties selLabel "Select nearest point")
@

One consequence of this change is that once a point has been selected you
can not remove this (i.e. un-select the point). This is in contrast to
'singleSelection', where clicking on an area with no stars would remove the
previous selection. The 'Clear' property can be added to the list
to define a way to clear the selection.

-}

nearestSelection :: VegaLite
nearestSelection =
  let selLabel = "picked"

      sel = selection
              . select selLabel Single [ Nearest True ]

  in toVegaLite (sel [] : selectionProperties selLabel "Select nearest point")


{-|

The selection can easily be changed to allow multiple stars
to be selected, using shift-click, by swapping from 'Single' to 'Multi'.

<<images/vl/multiselection.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjt1h0NtWJ2HY6gAcS1g-URWSJo1k72hAEF+YRErm7uLB6gDcgAPD5YW60b4PSDcV6iIEg+4-SBUHh-QH4SDXYF3XYQRS7HHY76QAAkyDisy0bg8Xl8PnqE1C4SibFI6B8JLJFVpUnIYUKxQALGwAFbIRJwyCxUZxaDMxB2GCuUjIMCKsCCmLQMDwMDkeC3Fz4YjQACeBio+iwzEopCUigJ0ugozl1H+ZlRIgVqA1cW5cR0YGg6DAEqoUrARhMy3MUEwpGSrqgkXQHnQOFtGgRiAS9BMlwNu0gCXImCOJ0gxXg5GYZtRwVgVCNcOOBcS2elorzPygZCo5Ho+gEleqXyxe2DUpl+nCKpVauqmu1up7jdLxtNVxTJgrkBHeIcaabyFIAC9q2B7KXy5XTwBWMF7BKIVsTg1litVuh8uSqKBjtuy1FTkqM7qvOOp6kouJ3pA6AGPAcRhA2HYXm+p5yGwADMd6WA+T7tmer5XnQ3TfuKPYhn+k4KkBSqzhqWpgUuu74vmKL4d2vb6Oc8CXCRJIVqexAnrA6B0KgFaxLaUCrqekAAI7MEg9pMKQpRKFBiFsbGHGosiUbinE-FykJIn4GJ5ASSR0n6PJilhMpqm7vuUAPmQuZnjiihAA Open this visualization in the Vega Editor>

@
let selLabel = "this is just a label"
    sel = selection
            . select selLabel 'Multi' []

in toVegaLite (sel [] : selectionProperties selLabel "Shift click to select points")
@

-}

multiSelection :: VegaLite
multiSelection =
  let selLabel = "this is just a label"

      sel = selection
              . select selLabel Multi []

  in toVegaLite (sel [] : selectionProperties selLabel "Shift click to select points")


{-|

We can take advantage of browser event by using 'On' to define which
event to use, such as mouse movement over points:

<<images/vl/eventselection.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjt1h0NtWJ2HY6gAcS1g-URWSJo1k72hAEF+YRErm7uLB6gDcgAPD5YW60b4PSDcV6iIEg+4-SBUHh-QH4SDXYF3XYQRS7HHY76QAAkyDisy0bg8Xl8PnqE1C4SibFI6B8JLJFVpUnIYUKxQALGwAFbIRJwyCxUZxaDMxB2QykOIFehy6AATwMVH0WGYlFI5igotRWCysXQpVoikUBOl0FGKqo-zMqIAsmaqGBwu6jCYah70GAJVQpWB4GBvaZ9ZBMKRkk6oJF0B50DgrRoEYgEvQTJd8PY9glyJgjidIMV4ORmJrUcFYFRVXDjpAEogs9LDWA8+CyFRyMrUQIK9Uvli9oGpTL9AYFUqGyW1RqrsmTOXICO8Q5U43kKQAF5Vjul8uVugAVlUUGbrYnucPFf3fLk5-FPaDbdlqKniqoystYINBngOIwnrG8yzvOg5DYABmJ9LzCa8DzA498G6J8xzfSdp2-JRNz2FED27Xt9HOeBLjQuJy33Yg91gdA6FQctYitKB533SAAEdmCQG0mFIUolD-SAQIImMiI-AFIxJSi5Rouj8AY8gmKfVj9E47iwl4-j11wptEjIHMOxxRQgA Open this visualization in the Vega Editor>

@
let selLabel = "picked"
    sel = selection
            . select selLabel Multi [ 'On' "mouseover" ]

in toVegaLite (sel [] : selectionProperties selLabel "Move the pointer to select a point")
@

The supported list of events is described in the
<https://vega.github.io/vega/docs/event-streams/#selector Vega Event-Stream Selectors> documentation.

The addition of @Nearest True@ to the list of properties
sent to 'select' would avoid the flickering, as the mouse moves
between the stars.

-}
eventSelection :: VegaLite
eventSelection =
  let selLabel = "picked"

      sel = selection
              . select selLabel Multi [ On "mouseover" ]

  in toVegaLite (sel [] : selectionProperties selLabel "Move the pointer to select a point")


{-|

The final 'Selection' value is 'Interval',
which lets you drag a rectangle to select the interior points:

<<images/vl/intervalselection.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjt1h0NtWJ2HY6gAcS1g-URWSJo1k72hAEF+YRErm7uLB6gDcgAPD5YW60b4PSDcV6iIEg+4-SBUHh-QH4SDXYF3XYQRS7HHY76QAAkyDisy0bg8Xl8PnqE1C4SibFI6B8JLJFVpUnIYUKxQALGwAFbIRJwyCxUZxaDMxB2NHwLAmYJgUjIMCubT0OXQACeBio+hMNWK8BcikUBOl0FG2qo-zMqI4CGV8DAsCoUqQwVGYGg6DAEo90DARiNyHMUEwpGSDqgkXQHnQOAtGgRiAS9CVRz2CXImGzx0gJvIzANqOC7p1cMLCUQmelovw9nhZCo5C1qIEJeqXyxe0DUplVwVSpVao1sC1YJOMD1ZagU0ViFNkD7eIcKcLyFIAC9583C8XS3QAKzTyy1+tDptF03H-B8uSqKADhuy1HLpfK1XqzVKXHTpA6AGPAcRhFWTZ9reJbznIbAAMznlAl5hNeYDEEe87dM+4ptkGaHyl+Y6-pO-7HJuewouhJDRu2+jnPAlw4SSpr7pAe6wOgdCoKasQWlAur6voACOzBIFaTCkKUSiARB1GtnRqLIhG4pxKxcocVx+A8eQfE4YJ86QKJ4lhJJ0nrhRkC1mQlxNjiihAA Open this visualization in the Vega Editor>

@
let selLabel = "naming is hard"
    sel = selection
            . select selLabel 'Interval' [ ]

in toVegaLite (sel [] : selectionProperties selLabel "Drag a rectangle to select points")
@

-}

intervalSelection :: VegaLite
intervalSelection =
  let selLabel = "naming is hard"

      sel = selection
              . select selLabel Interval [ ]

  in toVegaLite (sel [] : selectionProperties selLabel "Drag a rectangle to select points")


{-|

The default interval option is to select a rectangle, but it can be restricted -
such as to select all items within a range along a given axis
using 'Encodings':

<<images/vl/intervalselectiony.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjt1h0NtWJ2HY6gAcS1g-URWSJo1k72hAEF+YRErm7uLB6gDcgAPD5YW60b4PSDcV6iIEg+4-SBUHh-QH4SDXYF3XYQRS7HHY76QAAkyDisy0bg8Xl8PnqE1C4SibFI6B8JLJFVpUnIYUKxQALGwAFbIRJwyCxUZxaDMxBHPaIeBYEzBMCkZBgaoUchgVzaeh2BGIBL0ZXIOgESAAT0gaig0EtBio+hMNWK8BceLAigJ0ugowN1H+ZlRHAQKug6A1VEl0DARhd6silrj2nd5HggNUUEwpGSwagkXQHnQOG9GkNxuVcssCXImGrezd5GYTtRwVgVGtYL2CUQJulovw9nhZGj+tRAmb1S+WL2EqoUplV0VytV6s15G1utg+u7xxgDtbUCmSoVLlnnvxu3FpAAXkfh-umy26ABWPdQXv9pdDyDPo98nIWbitGC4DrKqIKqeKpqhq0qbjqepKLi3aQOgBjwHEYTWkOs5-u6L74HIbAAMwfpAX5hD+YDEP+dDdMB86LoOx4rogMHrvBW5IZeXqoSiNEkLm5DjmcFzmFAJLug+kD3rA6B0Kg7qxN6dqHvoACOzBIL6TCkKUSioThgmjiJ+jIhJ4pxNJBpyQp+BKeQKnAfajqadpphhHpBmemWNaJGQlxDjiihAA Open this visualization in the Vega Editor>

@
let selLabel = "naming is still hard"
    sel = selection
            . select selLabel Interval [ 'Encodings' [ 'ChY' ] ]

in toVegaLite (sel [] : selectionProperties selLabel "Drag to select points by parallax")
@

We'll come back to further things to do with interval selections
when we get to interactive plots below (see 'bindScales').

-}


intervalSelectionY :: VegaLite
intervalSelectionY =
  let selLabel = "naming is still hard"

      sel = selection
              . select selLabel Interval [ Encodings [ ChY ] ]

  in toVegaLite (sel [] : selectionProperties selLabel "Drag to select points by parallax")


-- $intro-selection-transforms
-- Simple selections as described above create sets of selected data marks
-- based directly on what was interacted with by the user. Selection
-- transformations allow us to /project/ that direct selection onto other
-- parts of our dataset.

{-|

For example, we can adjust the visualization to select all stars in the
same cluster, which is useful in this case since the Blanco1 and
IC2391 clusters occupy the same space in the magnitude-parallax
plane.

This is invoked simply by adding the 'Fields' constructor to the select
parameters naming the fields onto which we wish to project our selection.
Additionally, we have set the default selection with 'Empty' so that if
nothing is selected, the selection is empty
(as we have previously seen, without this the default selection is the
entire encoded dataset).

<<images/vl/transformselection.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjt1h0NtWJ2HY6gAcS1g-URWSJo1k72hAEF+YRErm7uLB6gDcgAPD5YW60b4PSDcV6iIEg+4-SBUHh-QH4SDXYF3XYQRS7HHY76QAAkyDisy0bg8Xl8PnqE1C4SibFI6B8JLJFVpUnIYUKxQALGwAFbIRJwyCxUZxaDMxBHPYGUhxPT4ewnNFzWBUap0ODMKhgvZULAGaAATyuiSocOOMFNBitqOQJmCo2tezIVHI9GQdAIkAE5BiNUgylxGkUBOl0FdKpgVH+ZlRIk9VClYHgYCMJmgqjAEtT0HTYDigeqXygmFIyUTUEi6A86BwEY0CMQCXozrllgS5EwXb2xXggYdUGCmvNBu7iQ70tFsY9Xv0AaD5fFKalMv0CqV5igZvtFqwJiHSlxBvFpAAXiPiIPh3QAKy5yAJRAzzexu96uh8uTP-MbnOvyKnoijNja6AGPAcRhOan5Dt++ByGwADMz6vu+QG3ghI7dP+66zrKqLbqB4F7CiYDEAu9D6Oc8CXP+cRDjekDXrA6B0KgQ6xBGe52iOkAAI7MEgUZMKQpRKOecGUSQVaLsRAK7uKTExrJbEcfgXHkDxz77gJwmiWE4mSXiYBkS+iRkJcKo4ooQA Open this visualization in the Vega Editor>

@
let sel = selection
            . select "pick" Single [ 'Fields' [ \"Cluster\" ]
                                   , 'Empty'
                                   , Nearest True
                                   ]

in toVegaLite (sel [] : selectionProperties "pick" "Select a point, select a cluster")
@

-}

-- TODO: why did I call this transformSelection?

transformSelection :: VegaLite
transformSelection =
  let sel = selection
              . select "pick" Single [ Fields [ "Cluster" ]
                                     , Empty
                                     , Nearest True
                                     ]

  in toVegaLite (sel [] : selectionProperties "pick" "Select a point, select a cluster")


-- TODO: can I think of anything to do with lookupSelection


-- $intro-selection-binding
-- Selection need not be limited to direct interaction with the
-- visualization marks. We can also /bind/ the selection to other
-- user-interface components.

{-|

New in Vega Lite 4 is the ability to interact with the legend via
the 'BindLegend' option. In this case
selecting on a cluster in the legend will highlight that cluster in
the visualization (but not vice versa). Notice how the legend now
also follows the 'MSelectionCondition' rules (that is, the unselected
items in the image below are also drawn in grey and are partially
transparent).

<<images/vl/legendselection.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjt1h0NtWJ2HY6gAcS1g-URWSJo1k72hAEF+YRErm7uLB6gDcgAPD5YW60b4PSDcV6iIEg+4-SBUHh-QH4SDXYF3XYQRS7HHY76QAAkyDisy0bg8Xl8PnqE1C4SibFI6B8JLJFVpUnIYUKxQALGwAFbIRJwyCxUZxaDMxBHPajYLJeh2SCREzK1EKpXmKDQACeBio+mQJmCox1JFIVHI9GQdAIkAE5BiNUgyjxYEUBOl0HN+GI1H+ZlRImtVClYHgYC1iHoYB5OFUUEwVtM+ki6A86BwXo0CMQCXoprllgS5EwJb2xXgzqNqOCsCoerhx0gCVjYRlldbZGtGqgTpdXyxewl4elos1ZW1YJOMANdagUywJhrkBHHvxu3FpAAXoviNXa3QAKxJtuJIsT2X+yBH5iLvlyc9jqVdqeK2NKXOt9AGeBxGEza3vei5yGwADM57tle75gIeNYPnQ3QvmGb6TlAMbKooP57Ci8GWn2+jnPAlwvnENYHpA+6wOgdCoDWsRerqC76AAjswSA+kwpClEos6QMBBG9ja+jIhaJKUSqNF0fgDHkEx576oa7GcaYYQ8XxHq4ReiBkJc-o4ooQA Open this visualization in the Vega Editor>

@
let sel = selection
          . select "pick" Single [ 'BindLegend'
                                   ('BLField' \"Cluster\")
                                 ]

in toVegaLite (sel [] : selectionProperties "pick" "Select a legend item")
@

-}

legendSelection :: VegaLite
legendSelection =
  let sel = selection
            . select "legend" Single [ BindLegend
                                       (BLField "Cluster")
                                     ]

  in toVegaLite (sel [] : selectionProperties "legend" "Select a legend item")


{-|

The Elm Vega-Lite walkthrough uses a dataset which has a
column for which a range-slider makes sense. The dataset I'm
using is less rich, and so I am going to use a HTML select
widget - a drop-down list of values - instead. This lets
the user select all stars from a given cluster, and is
introduced with the 'Bind' and 'ISelect' constructors.

The 'InOptions' list is given the values of the Cluster column that
can be selected: I start with a value not in the list (@none@) just to
indicate that no values are selected, and then the list of clusters in
this sub-sample (remembering that 'selectionProperties' applies a
declination cut off). Eagle-eyed readers will note that the cluster
names in this list (the @clusters@ variable) end in spaces: this is
because the input data file has the cluster names stored in an
eight-character field, even though it is a tab-separated file.
This surprised me when I first tried this visualization, and
using the value \"Blanco1\" did not select anything! Isn't working
with data so much fun!

<<images/vl/widgetselection.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoAY3SIyAczrEARvE0BrPbHTNE9OpCNVSeqkvURIWeLDP2ADuikiNCQ7gxM8IYaHsyw5PYAFtDQfsi4APTpCADubHqk0AnMRszINNrBVMFs2ljpHNZ6AEKxZlTpCQBuVHrw6V7I1LAd3b3pjNB9vaTwUvCzAGwAjAtzS3JSk0bkVEvwbIjoUglU8PQ0bNDInaHRUBjYTFEQL1B+3mXPry+QAOJeBnwkEQrEctDC3ygQgAgvxhCJ7CCsGDbpCPH5yAAPRGgmiotGQbhw0Q45F4iHfSBUHgY7FApEou4QRR3FnMsKQAAkyE0Jy8iWSqQy6VG+3yhWKbFI6HSPL5fVFUnIBXanQALGwAFbIHT4yBlHaaaDSxBfdGkcxUOz4UCQqlYPzQACeiJ0rgpHiMQWtYFtBIE5FKQzNBKCfmYISBBqoRvxaKg6EdJuQdAIwLdoSgTXISG0SygqigghkAGYAJz5gtFgQyBZyGQeQuQAByvxraoArPnIMomS82d9FB6oM6-K4o0E9Ds4z8yFRyPQU-g0wGg3je68B0ONDACtObTAqJjI1AAAo7eBlMDRo1geBgTSBwbkhOwUhVE8OdDJdA4bceKptHoScQ0gbRyEwEMPE6eBA3HKBLCoF1hzAnRgONXUbT7Dw5wXexV2fcFsKgG8MNNIE-AtNo7GHDxR3g9MsCCWDID7Ad2TufVSAAL3g4gYLgugOybCp0JNQxIAE5h4LVOQm1I8SKKoq0lH-H5E1MAoXQPKT4LkNgSxEtCCkU31JNg6S6CWeT5xjMj-GUuxFDUjw6TM3CfT+AFM31TRYL4yBeKsOhUFgsohxHJ0x3sABHZgkGNSZjW6NxOO09z3zwiisR8nl-IkoL0BCsKqAimAooYuKEoKJhSBSrcQEUIA Open this visualization in the Vega Editor>

@
let picked = "picked"

    clusters = [ \"none\", \"Blanco1 \", \"IC2391  \", \"IC2602  \", \"NGC2451 \" ]
    sel = selection
            . select picked Single [ Fields [ \"Cluster\" ]
                                   , 'Bind' [ 'ISelect' \"Cluster\" [ 'InOptions' clusters ] ]
                                   , Empty
                                   ]

   conf = configure
            . configuration (BackgroundStyle "beige")

in toVegaLite (conf [] :
               sel [] :
               selectionProperties picked \"Please select a cluster\")
@

Originally this example had the selection working both ways - that is
the HTML widget can be used to select a cluster and clicking on a point on
the visualization updated the HTML widget. However, this no-longer happens
and I don't know whether it is a change in Vega-Lite or I changed
something in the visualization!

Unlike the other plots shown in the tutorial, this is a screen grab
rather than a PNG file created by Vega Embed. The background color was
changed - following the approach used in
'stripPlotWithBackground' - to show where the visualization "ends" and
the HTML select element starts. It also shows the Vega Embed "drop-down"
menu in the top-right corner, namely the three dots in a circle.

-}

widgetSelection :: VegaLite
widgetSelection =
  let picked = "picked"

      clusters = [ "none", "Blanco1 ", "IC2391  ", "IC2602  ", "NGC2451 " ]
      sel = selection
              . select picked Single [ Fields [ "Cluster" ]
                                     , Bind [ ISelect "Cluster" [ InOptions clusters ] ]
                                     , Empty
                                     ]

      conf = configure
               . configuration (BackgroundStyle "beige")

  in toVegaLite (conf [] :
                 sel [] :
                 selectionProperties picked "Please select a cluster")



{-|

The selection can also be bound to an axis (or both axes, as in this
case), using 'BindScales' (applying it to the 'intervalSelectionY' plot).

<<images/vl/bindscales.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjt1h0NtWJ2HY6gAcS1g-URWSJo1k72hAEF+YRErm7uLB6gDcgAPD5YW60b4PSDcV6iIEg+4-SBUHh-QH4SDXYF3XYQRS7HHY76QAAkyDisy0bg8Xl8PnqE1C4SibFI6B8JLJFVpUnIYUKxQALGwAFbIRJwyCxUZxaDMxBHPYGUhxAr0OyQSImFWoknwUbIcxQZIJegmYJ6-AESAAT31kEBaig0EtBio+hMNWKOqUGkUBOl0FGquo-zMqI4CGCYEwYAAXuh0FgwOEqGB4P8qHrVFBMKRkiGoJF0B540oCYb0MbEJd8PY9glyJg5cdIB7yMwXajgrAqNawbXEsbpaLq1i9mQqORNVABK3ql8R5YJVQpTL9AqlVQVb2m47nVd4yZPSO8Q4fbtxaRo+2wMQW226ABWTOQBKIAcr6vNnV3-B8uRPxfLkOvyKsqSink26AGPAcRhNaH63lechsAAzE+L5vkBN5fle3T-uOS6DrKqJrqBijgXsKLXiQOYTvo5zwJc-5xDqV7EJesDoHQqA6rEPoOk6V6QAAjswSB+kwpClCWZ5wVRY60cRAI2tqAYfuxnH4Nx5C8U+O6CSJYlhBJUnHuRz6JGQVbXjiihAA Open this visualization in the Vega Editor>

@
let picked = "picked"

    sel = selection
            . select picked Interval [ Encodings [ 'ChX', ChY ], 'BindScales' ]

in toVegaLite (sel [] : selectionProperties picked "Drag or zoom the axes")
@

The image here was created after panning and zooming into the data.

-}

bindScales :: VegaLite
bindScales =
  let picked = "picked"

      sel = selection
              . select picked Interval [ Encodings [ ChY, ChX ], BindScales ]

  in toVegaLite (sel [] : selectionProperties picked "Drag or zoom the axes")


-- $intro-coordinated-views
-- One of the more powerful aspects of selection-based interaction is in
-- coordinating different views – a selection of a data subset is projected
-- onto all other views of the same data.

{-|

The following plot doesn't contain anything new, but allows us to
select a rectangular-range on one plot, and see the same selection
automatically applied to the other plots. This is achieved by
combining the 'repeat' fuction with the 'selection'; this causes the
selection to be projected across all views as it is duplicated.

For this example we use all the clusters, rather than the subset of
Southern ones. One trick I use is to convert the Right Ascension
values (which have a domain of 0 to 360 degrees, and wrap around at
the 0\/360 mark), into their cosine values (remembering to convert to
radians first), and display that instead. This ensures the \"Blanco1\"
cluster members are spatially co-located on this axis - with values
close to 1 - rather than appearing near 0 and 360. This is more to
show things you /can/ do with Vega-Lite, rather than necesarily
things you __should__ do :-)

<<images/vl/coordinatedviews.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEBOCmAOsCGAXSAuMwYHsDuGYA2pAMY4DOASgIKQA0UAIgKID6AkgMJUDKkAXUZkcAGwCuAWwB2hEvFEAPBlADikpAHNBAX3rgoFRKUKgIESCmhJpFAGY5okuQfNZISCoRHU6w0kiipOKiqLDe5BQAFAAmqFIAdLScPLxgAFRgAAocYAD0YACMABwADACUkDquYEI1kBrQANYRAJbQpKLh+m6QcShIpjUW4tCi3gAWKCjwFOh5edZ4CZqtKBPiAEbiFLAdONIosIcJ5JJ5TDjimgBCo02weRMAbrCaSHkaFEfQT6-veX6H3erSQAFokOCAGyFKEQwqlMEDTZdQpIBLSHBgibIGJ7BIoCjPBjDKAOJyoIZuXrwJDQXZU6m9dRabzSKSbPYkpm9ZLcPhsjlcno8iwKZSYSDsySc6Dc0XMdj8-iS6Wy+WiyCwNjiwUyrmkiDVJnG8x6eq7LqkFCtA6mSDwVqkB4xe0oACeiG8rUOe2egSq5t6x3IMR92kwZmpIlEjkZTMg-ok4Ulmjg7o10fI0jDNrtkcN5kgdlasFErslXAk32FhYsltg1ttskljudsFdIs1Hq9qpwkh9AcLprcQejEvcJbLFfccEQlMl5AkMiqwh7KagAEdxDYbQMba9V6TIBnI8XS+X7XPkGhJdB8KuoOvvNvd2tUK1DyOwKax5AACQUKQOIaJM0yzPMeT-OiqzrFsCS2nkQEgR80FgqIayPM8AAsCQAFYUHaIoiNIJYRlg1Q6EAA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PRepeat Column, PmType Quantitative ]
            . position Y [ PRepeat Row, PmType Quantitative ]
            . color
                [ MSelectionCondition (SelectionName \"picked\")
                  [ MName \"Cluster\", MmType Nominal ]
                  [ MString \"grey\" ]
                ]

    sel = selection
            . select \"picked\" Interval [ ]

    trans = transform
            . calculateAs "cos(datum.RA_ICRS * PI / 180)" "cosRA"

    spec = asSpec
             [ gaiaData
             , trans []
             , mark Circle []
             , enc []
             , sel []
             ]

in toVegaLite
     [ repeat
       [ RowFields [ \"cosRA\", \"DE_ICRS\" ]
       , ColumnFields [ \"plx\", \"Gmag\" ]
       ]
     , specification spec
     ]
@

-}

coordinatedViews :: VegaLite
coordinatedViews =
  let enc = encoding
              . position X [ PRepeat Column, PmType Quantitative ]
              . position Y [ PRepeat Row, PmType Quantitative ]
              . color
                  [ MSelectionCondition (SelectionName "picked")
                    [ MName "Cluster", MmType Nominal ]
                    [ MString "grey" ]
                  ]

      sel = selection
              . select "picked" Interval [ ]

      trans = transform
                . calculateAs "cos(datum.RA_ICRS * PI / 180)" "cosRA"

      spec = asSpec
               [ gaiaData
               , trans []
               , mark Circle []
               , enc []
               , sel []
               ]

  in toVegaLite
       [ repeat
         [ RowFields [ "cosRA", "DE_ICRS" ]
         , ColumnFields [ "plx", "Gmag" ]
         ]
       , specification spec
       ]


{-|

If the interval selection is bound the the axes with 'BindScales'
then we can zoom and pan the related plots - i.e. changing the
range displayed in one plot will also change the two plots that
it shares an axis with in this two by two arrangement. The conditional
encoding of the 'color' channel has also been removed.

<<images/vl/coordinatedviews2.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4IgTgpgDhCGAuIBcowHsDuyDaIBKAggPoCSAwngMogA0IAIgKKkXUC6dAxmgDYCuAWwB2OEFB4APWiADiA2AHMQbAL50AzjE7JQ8sAGtkITgEswnHhGkATBLB0g+YHkYAW8eFHVIA9D7CwGAB0CibwrnwARnzqEOZoQvAQiUHcAj70aHwKAEJO+hA+rgBuEAqwPvLqSWBFpeU+tvAV5SawALSwHQBsAIzdnb0ADO3NkZa9sEFCaO2ucNZxQfDqxdIAZmhg8ogoYrBgsQ5yikZCgpFx0oQsVGcXV3TiUkgg5wKXYNJMt9Sv759pBAiM97h8ripIRoIJZOPATAkHFATJwCtYHJETEJ0a91JxYJZ1NJ4ABPGBGLE1YoEkBQkDJbjWLFKPbcHhbBzrEwwnEgMj8aqPECk8n-NACLE0tQgF6gLk8hyQGAIIxswQiaUiqyvACOfFgiTCCBMpVpdBJnO5PBxqGgcF24EwZuFZO1ID1BvhzXhpsh0oAJHj5vI3B4vL4fPUpqFwlEggifEGIPJI2UOjwwoVigAWIIAK3UiJUQA Open this visualization in the Vega Editor>

The image was captured after panning and zooming in the
\"parallax-RA_ICRS\" plot.

@
let enc = encoding
            . position X [ PRepeat Column, PmType Quantitative ]
            . position Y [ PRepeat Row, PmType Quantitative ]
            . color [ MName \"Cluster\", MmType Nominal ]

    sel = selection
            . select \"picked\" Interval [ BindScales ]

    spec = asSpec
             [ gaiaData
             , mark Circle []
             , enc []
             , sel []
             ]

in toVegaLite
     [ repeat
       [ RowFields [ \"RA_ICRS\", \"DE_ICRS\" ]
       , ColumnFields [ \"plx\", \"Gmag\" ]
       ]
     , specification spec
     ]
@

The \"cosine\" transformation has been removed in comparison to
'coordinatedViews'.

-}

coordinatedViews2 :: VegaLite
coordinatedViews2 =
  let enc = encoding
              . position X [ PRepeat Column, PmType Quantitative ]
              . position Y [ PRepeat Row, PmType Quantitative ]
              . color [ MName "Cluster", MmType Nominal ]

      sel = selection
              . select "picked" Interval [ BindScales ]

      spec = asSpec
               [ gaiaData
               , mark Circle []
               , enc []
               , sel []
               ]

  in toVegaLite
       [ repeat
         [ RowFields [ "RA_ICRS", "DE_ICRS" ]
         , ColumnFields [ "plx", "Gmag" ]
         ]
       , specification spec
       ]


{-|

The ability to determine the scale of a chart based on a selection is
useful in implementing a common visualization design pattern, that of
'context and focus' (or sometimes referred to as 'overview and detail
on demand'). We can achieve this by setting the scale of one view
based on the selection in another. The detail view is updated whenever
the selected region is changed through interaction:

<<images/vl/contextandfocus.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCE6BiYZLA4AgXpRZgA4iYshJBiWLLcgqmVVZAASgCCAPoAkgDCXQDKDU0tbWlVGWR4AB4Tza047bOYCgCigyPjqI0r0x1FkPx980sHk6uQJ2AAvh1PEA8zULqiYkIIhADaHUKp2ibBYJEIAA5fO9TiYcBoGmQRGwJOtOgw2NBOIQACy+aH3SACYJCeBscQFSDcHB8IiUqSiaAolgUf6QACekAAuikoPB2WR+A0UeZdLACA83oSyfBgg1RvwSfAwLAwABNMD0MQsfhgeAiMAALxEIlkYBRYFaeBEDDRpwZIiZ2oqGygVzQ91OmTYiugDVqsHqMI2RJ+ctQwEghsEIkImXFAilrsw-MFDQAjlhYBIOAg2Po7q6kyHORGsj68H6DpdUnyBUKDpnszK8wWXlV269g0CMnCEdXkajg5gMVi6ag8QTZmdvo7mS6Q6JrWsy97fQ0hng+OZa5BUw2oBFZCjxZBi9P3T3p2vK-66naQxQwwfI9GcLHUPG8Inh519xmsxzRAyQLX9TlgBY2FZMsZXDMAmjwPBOw7MCoFLD1kygG8qygGtUMwJ9xRfGBTVgFFKWJfhSXJMQGmpWkz3PEN-0bQCWxAoVPUee5kPbHl0kgAASJ9ohMBoSHIKhaCsMJ2E4HgmHJagRP4ExqBkpw8A4bRdBxJgACsKApd5IC+b16gjJ4HiAA Open this visualization in the Vega Editor>

@
let sel = selection . select \"brush\" Interval [ Encodings [ ChY ] ]

    encContext = encoding
                   . position X [ PName \"Gmag\", PmType Quantitative, PScale [ SZero False ] ]
                   . position Y [ PName \"plx\", PmType Quantitative ]

    specContext = asSpec [ width 400
                         , height 80
                         , sel []
                         , mark Point []
                         , encContext []
                         , title \"Select a Y range to zoom in below\" []
                         ]

    encDetail = encoding
                  . position X [ PName \"Gmag\"
                               , PmType Quantitative
                               , PScale [ SZero False ]
                               , PAxis [ 'AxNoTitle' ]
                               ]
                  . position Y [ PName \"plx\"
                               , PmType Quantitative
                                 -- prior to 0.11.0.0 this was 'SDomain'
                               , PScale [ 'SDomainOpt' ('DSelection' \"brush\") ]
                               ]
                  . color [ MName \"Cluster\", MmType Nominal ]

  specDetail =
      asSpec [ width 400, mark Point [], encDetail [] ]

in toVegaLite
     [ gaiaData
     , vConcat [ specContext, specDetail ]
     ]
@

Not shown here, but selecting a range of y-values in the top plot
(@specContext@) will cause the second plot (@specDetail@) to zoom
in on that range, as the selection is bound to the y axis of this
plot via 'DSelection'.

-}

contextAndFocus :: VegaLite
contextAndFocus =
  let
    sel =
        selection . select "brush" Interval [ Encodings [ ChY ] ]

    encContext =
        encoding
            . position X [ PName "Gmag", PmType Quantitative, PScale [ SZero False ] ]
            . position Y [ PName "plx", PmType Quantitative ]

    specContext =
        asSpec [ width 400, height 80, sel [], mark Point [], encContext []
               , title "Select a Y range to zoom in below" []
               ]

    encDetail =
        encoding
            . position X
                [ PName "Gmag"
                , PmType Quantitative
                , PScale [ SZero False ]
                , PAxis [ AxNoTitle ]
                ]
            . position Y [ PName "plx"
                         , PmType Quantitative
                         , PScale [ SDomainOpt (DSelection "brush") ]
                         ]
                  . color [ MName "Cluster", MmType Nominal ]

    specDetail =
        asSpec [ width 400, mark Point [], encDetail [] ]

  in
    toVegaLite
      [ gaiaData
      , vConcat [ specContext, specDetail ]
      ]


-- $intro-crossfilter
-- The final example in this section brings together ideas of view
-- composition and interactive selection with data filtering by implementing
-- /cross-filtering/: the selection of a subset of the data in one
-- view then only displaying that data in the other views.

{-|

Here we show distributions of the four main numeric quantities
in the dataset - position, magnitude, and prallax - using the
@totalEnc@ encoding, and add a second layer which repeats this
data but with a different color (@selectedEnc@), and that is tied to
the interval-selection along the x axis ('ChX').

<<images/vl/crossfilter.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEBOCmAOsCGAXSAuMxIGMD2ANgK4C2AdhmANqQBKAggPoCSAwrQMqQA0UAIgFEW7Lr0gBxEkgDmPKPAIAPSAF0Avt3BQAzohyVQECJAAmqJAa1GoRaAUqQAFihTxt6APQfoSAO4A6aQBLFEciACMibVhofDIUWHj-fBIPPjwiaQAhWwBrWA9HADdYaSQPKW0E6EKSso8zFHKyoKQAWiR2gDYARi6OnoAGNqbwglgepH8yPDbHZBMY-xRtIp4rI0gAMzxoKTRMQ2tN+CRoaMtj44kpWUxIMlJwmPWr6zomNk4HR5Jn6Feb2MCmU91+-0BQMggmE3zBTxemiBxlgjBBPwRAI21jU2LAuJxSM2BCQAE8XpgqHijldIFJoLkHOEzpDrol8CYgmQ7lg8e9QVhtkFYAQTAYYAhkAcoPhiORIBooChSYgHABHIhIeIhVBBEoKolQ0nimTSOBlBIOfBEeJySDK1X3DValA6136glvT2E6l8pU+MjaHZ7ShUbBbIIEari6LjHCuvAUe7haBRRwKtQqQ3XemM5Ms7PvWOweNBRPilNp8XsvCc7naUOQZRZpUq2AOLnVIpIexqDR+yA1us8mlvXCEXbi7vEdv3aSERZkaC1g1+4wC8PC0XiuCIVBWwikCiK+1t9Wa7VNd3t-vIqDGw6QU3m1CzmUZW1iB1vyDOy+6j0-W9IxgJULRbygAASbQcHmKQHGcVx3C8OopmCUIIn8MsPBguDylQtoCBCAoigAFn8AArbRyyJccyAjEdcTUIA Open this visualization in the Vega Editor>

Selecting a small range of parallax values in the fourth plot
highlights the associated data in the other three plots.

@
let sel = selection . select "brush" Interval [ Encodings [ ChX ] ]

    filterTrans = transform . filter ('FSelection' "brush")

    -- borrow a function from Elm
    pQuant = PmType Quantitative

    totalEnc = encoding
                . position X [ PRepeat Column, pQuant ]
                . position Y [ PAggregate Count, pQuant ]

    selectedEnc = totalEnc
                    . color [ MString "goldenrod" ]

in toVegaLite
     [ repeat [ ColumnFields [ \"RA_ICRS\", \"DE_ICRS\", \"Gmag\", \"plx\" ] ]
     , specification $
         asSpec
           [ gaiaData
           , layer
             [ asSpec [ mark Bar [], totalEnc [] ]
             , asSpec [ sel [], filterTrans [], mark Bar [], selectedEnc [] ]
             ]
           ]
     ]
@

-}

crossFilter :: VegaLite
crossFilter =
 let
    sel =
        selection . select "brush" Interval [ Encodings [ ChX ] ]

    filterTrans =
        transform
            . filter (FSelection "brush")

    -- borrow a function from Elm
    pQuant = PmType Quantitative

    totalEnc =
        encoding
            . position X [ PRepeat Column, pQuant ]
            . position Y [ PAggregate Count, pQuant ]

    selectedEnc =
        totalEnc
            . color [ MString "goldenrod" ]

 in
 toVegaLite
    [ repeat [ ColumnFields [ "RA_ICRS", "DE_ICRS", "Gmag", "plx" ] ]
    , specification $
        asSpec
            [ gaiaData
            , layer
                [ asSpec [ mark Bar [], totalEnc [] ]
                , asSpec [ sel [], filterTrans [], mark Bar [], selectedEnc [] ]
                ]
            ]
    ]


-- $intro-smoothing
-- Vega Lite 4 introduces several ways to \"smooth\" or \"fit\" your
-- data. I've already played around with kernel-density estimation
-- - via the 'density' transform that was used in 'densityParallax' -
-- so now I get to try out 'loess' and 'regression'.

{-|

The 'loess' transform will generate new coordinate pairs for the
independent and dependent values based on an existing pair.
The name stands for
<https://vega.github.io/vega-lite/docs/loess.html locally-estimated scatterplot smoothing>,
and here I use it to look for any possible relationship between the
magnitude and parallax of each star in a cluster. I don't expect
there to really be any (as we've seen before, the distribution
is pretty flat), but it's the data I have to play with in this
tutorial.

<<images/vl/loessexample.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4IgLgTghgdgzgMwPYQLYgFwG1QIJYA2YAphJiABQAmUYArqgHQAiAogPoCSAwgEoDKAAgB8AXkEAGAJSCAZIOq0GLDjwGCAPOIAs0kAF8AugBoQNMFEyg6EAuQAWYMAAc4GAPTvoAd0YBzPDB7OgAjOjhSAGMkGBJYxmjUd2YkOj8AIRsAa2J3ewA3Yj8od1QoOBIIPMLi93MS4rwoAFooFoA2AEZ21s6JZosQgmJOqEYYJGb7YigqUkYwOHyQU2Q0WisQZygICM2AcTK-chgGENIVkF4AQS4+fhOzi9NnAgAPR9RzslM2O4FPt9LsR2K8PhgQKcvhd9LDTHBnMRIptpng-I5MAAmACsElM3jwVCCWNxpgIUAAnhdsKAyhAsuRnEg8LFgTBolQWccMKBogQUJt8MQCFRyNwCOFKpdhn5iDBRRhTgQCPpTODcHhhQqQIcoMd4ZEoMNNgAvUhITAII0RVXgCmI8gARzosDAgVoeEKBlMFMFmpFjPelzghuNPJAZogFowVoINtMYHtxCdLti7rdXthqtAkFgiBQ6BpID8UbozhCvuwIHFkouJhA-OIcDggY+phi5F1xyMpjpDPDFSjOQA6oTiRhMfDIEgcuQhlBIgyE0nyAQWcnbXKOVzNuqQEKAxCu8HQ8nw5Ho7H43aHRDna7056Nz6-VrWyejWfQBfLdbiLbE1vEB7zTCwMw3WFDFtAASENpjKBwnFcDx3BqMYAiCUJGDwJB3Dg4gylQooWjXEhUO0RgACs4A7Uw+QYeAsVWBdiDAV9D2rCUKmeG8z0hJBUBZI0DH0IA Open this visualization in the Vega Editor>

@
let simplify = transform
               . filter (FExpr \"(datum.DE_ICRS >= 0) & (datum.DE_ICRS <= 40)\")

    baseEnc = encoding
             . position X [ PName \"Gmag\"
                          , PmType Quantitative
                          , PScale [ SZero False ]
                          ]
             . position Y [ PName \"plx\"
                          , PmType Quantitative
                          , PScale [ SZero False ]
                          ]

    rawEnc = baseEnc
            . color [ MName \"Cluster\"
                    , MLegend []
                    ]

    rawLayer = asSpec [ rawEnc [], mark Point [] ]

    trans = transform
            . 'loess' \"plx\" \"Gmag\" [ 'LsGroupBy' [ \"Cluster\" ] ]

    trendLayer = asSpec [ trans []
                        , baseEnc []
                        , mark Line [ MStroke \"black\"
                                    , MStrokeWidth 2
                                    ]
                        ]

    frameSpec = asSpec [ width 250
                       , height 250
                       , layer [ rawLayer, trendLayer ] ]

in toVegaLite
     [ gaiaData
     , simplify []
     , columns 2
     , facetFlow [ FName \"Cluster\", FmType Nominal ]
     , specification frameSpec
     ]
@

The data is filtered to select only four clusters, ensuring that
the two closest (i.e. they have the largest parallax values) are
included as they are likely to be the most-interesting to look
at (because of the spread of parallax values).

The 'LsGroupBy' option is used to ensure the calculation is done
per cluster, and then multiple layers are used to compare the
raw with the "smoothed" data in a faceted display.

-}

loessExample :: VegaLite
loessExample =
  let simplify = transform
                 . filter (FExpr "(datum.DE_ICRS >= 0) & (datum.DE_ICRS <= 40)")

      baseEnc = encoding
                . position X [ PName "Gmag"
                             , PmType Quantitative
                             , PScale [ SZero False ]
                             ]
                . position Y [ PName "plx"
                             , PmType Quantitative
                             , PScale [ SZero False ]
                             ]

      rawEnc = baseEnc
               . color [ MName "Cluster"
                       , MLegend []
                       ]

      rawLayer = asSpec [ rawEnc [], mark Point [] ]

      trans = transform
              . loess "plx" "Gmag" [ LsGroupBy [ "Cluster" ] ]

      trendLayer = asSpec [ trans []
                          , baseEnc []
                          , mark Line [ MStroke "black"
                                      , MStrokeWidth 2
                                      ]
                          ]

      frameSpec = asSpec [ width 250
                         , height 250
                         , layer [ rawLayer, trendLayer ] ]

  in toVegaLite
       [ gaiaData
       , simplify []
       , columns 2
       , facetFlow [ FName "Cluster", FmType Nominal ]
       , specification frameSpec
       ]


{-|

This is the same data as 'loessExample', but using a linear
regression model to try and explain the data. Practically,
the only things that have changed are switching from
'loess' to 'regression', and displaying all the data in
a single visualization.

<<images/vl/regressionexample.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4IgLgTghgdgzgMwPYQLYgFwG1QIJYA2YAphJiABQAmUYArqgHQAiAogPoCSAwgEoDKAAgB8AXkEAGAJSCAZIOq0GLDjwGCAPOIAs0kAF8AugBoQAC2J4A5mbCYAzBImmaYKJlB0IBcrbAAHOAwAemDoAHdGKzwwMzoAIzo4UgBjJBgSDMY01GDmJDorACEvAGtiYLMAN2IrKGDUKDgSCEqauuDXerq8KABaKH6ANgBGIYGRiT63eIJiEahGGCQ+iygqUkYwOCqQU2Q0Wg8QfygIZOOAcUarchgGeNI9kF4AQS4+fjuHp9N-AgAHt9UI8yKY2B8BMDQc9iOx-kCMCB7iCnvp0aZwngqLEHE5TAASOApCyNXxgAJBULtRbRWIJRh4JDBYmk+o0voEGIVKraRgAKzg6WeBCgAE8nthQI0IKVyP4kHgMrCYGkqErbhhQGkCChjvhiAQqORuAQki0DKZEbg8IbjUjrlBbqZiVA5scAF6kJCYBBu5L6UxgMX+YjkACOdFgYBitDwNUtIDF+ttRvlgOervdWpAXogPowfoIAaDIbDSMj0djMYT6MDoEgsEQKHQUpAVnzdH88WT2BApvNTxMIAgtVHcDgTJg6aBpmFDpuBmHMrlOea+fKAHVsbiMAAmF2QJDlcizKApOWl0PkLkwMOBkDEVVIdUwTXapC6sg5g1ppED5o0StFM7XIR1nRALNy1APMCyLEtwDLCMowyat43vUxextUCkQRTMUjdaDc29X1-WIB9g2vCsUJjNwa3vdFDH0IA Open this visualization in the Vega Editor>

@
let simplify = transform
               . filter (FExpr \"(datum.DE_ICRS >= 0) & (datum.DE_ICRS <= 40)\")

    axis pos lbl = position pos [ PName lbl
                                , PmType Quantitative
                                , PScale [ SZero False ]
                                ]
    enc = encoding
          . axis X \"Gmag\"
          . axis Y \"plx\"
          . color [ MName \"Cluster\" ]

    rawLayer = asSpec [ enc [], mark Point [] ]

    trans = transform
            . 'regression' \"plx\" \"Gmag\" [ 'RgGroupBy' [ \"Cluster\" ] ]

    trendLayer = asSpec [ trans []
                        , enc []
                        , mark Line [ MStroke \"black\"
                                    , MStrokeWidth 2
                                    ]
                        ]

in toVegaLite
     [ width 300
     , height 300
     , gaiaData
     , simplify []
     , layer [ rawLayer, trendLayer ]
     ]
@

In this example I used the default method - 'RgLinear' - but other
options are possible (set with the 'RgMethod' option).

-}

regressionExample :: VegaLite
regressionExample =
  let simplify = transform
                 . filter (FExpr "(datum.DE_ICRS >= 0) & (datum.DE_ICRS <= 40)")

      axis pos lbl = position pos [ PName lbl
                                  , PmType Quantitative
                                  , PScale [ SZero False ]
                                  ]
      enc = encoding
            . axis X "Gmag"
            . axis Y "plx"
            . color [ MName "Cluster" ]

      rawLayer = asSpec [ enc [], mark Point [] ]

      trans = transform
              . regression "plx" "Gmag" [ RgGroupBy [ "Cluster" ] ]

      trendLayer = asSpec [ trans []
                          , enc []
                          , mark Line [ MStroke "black"
                                      , MStrokeWidth 2
                                      ]
                          ]

  in toVegaLite
       [ width 300
       , height 300
       , gaiaData
       , simplify []
       , layer [ rawLayer, trendLayer ]
       ]


-- XXX TODO: add an example showing R2 in plot title


-- $intro-error
-- Here we dive into some of the ways for representing the spread
-- of a value, focussing on the \"error\" of a variable.

{-|
We have already seen "error bars" in the 'layeredDiversion' plot, where
the 'Rule' type was used to draw a line between the 'Y' and 'Y2'
encodings. In that example the two positions were calculated \"on the
fly\" by Vega-Lite (using the 'Min' and 'Max' aggregation operations).
In this example I use the data to calculate the display range,
namely @plx - e_plx@ to @plx + e_plx@. These are mapped to the
'X' and 'X2' channels (not because it makes a better visualization,
but just to show you can create lines along the x axis), and a
small-multiples approach is used to separate out the clusters, but
only after a filter designed to select the two clusters - with the
\"most interesting\" data for this plot - has been applied.

<<images/vl/errormanual.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxhJUBLAG2gFNY8oATeaAVywDoBhKt5OrAIAGALpgAvBLABybjLAAfRWBbsuvfoJHipsgBIzIAXwA0xCGXjJGkAA5UAHgH0q6SKagBjeFS9sqVlpbNQ5OB0cwAFpVVjDaZwiTcxJSSGtbCOcACwoPb19-QLoQuK4IsABqWPVOBKTjYlEUqCx4WABrW1gA4JbINXhGUFTINlgqW2zoaDtkXAB6BYQAd04AcwpobLYAI356L3REOhPOI6wFgBF0NnWAIXGO2gXsgDdadfgFtoF6V4+XwWgwWXwo8Ci8AhADYAIzQyGw4RRaDwXZUWiw+CcRDoKLZWjwJj0TjQZBvDwWKAYbCsYZUiD2drIYL4EapEiQADibXWtkQHF29EpHM5ACUAILOACS3DFAGV+YLhS1RfYnEqsEKGKqOZArgBRGVyxX4SACrUqhmc+oas0W7WQa2NVIusBmYiQAAkyC8BLaUxmc0WC0B2M22z2nAo6AWvv93zDUSoWxebwALJwAFbIY4iqAsjFeaAxxDDewULzPJjl3YURA1s2+3y0GyeGAATzsrKg9cEb18Jg9jNoiCOTHrfLZVMgjgATOXKLQqI2oFlcskZ456WqlyvMk5XO5dYzmxjywAvejufCoXws4d66BdnuQACObCQJdRJY++dG8COBQNhsjAWznmadjtL4gSRAAFL8ACUJhUo+jJHHwWBlqBe6rpAmh-DqUDPt2-LoFg9aDmhUAdouFDLnhPLwHy7Yka+H5flsrAUH+bqPpARyIJQU6kI0xhAA Open this visualization in the Vega Editor>

@
let trans = transform
              . filter (FExpr \"datum.Cluster[0] == \'C\' || datum.Cluster[0] == \'H\'\")
              . calculateAs \"datum.plx - datum.e_plx\" \"plx_lo\"
              . calculateAs \"datum.plx + datum.e_plx\" \"plx_hi\"

    errorEnc = encoding
                 . position X [ PName \"plx_lo\"
                              , PmType Quantitative
                              , PScale [SZero False]
                              , PAxis [ AxTitle "parallax (mas)" ]
                              ]
                 . position 'X2' [ PName \"plx_hi\" ]
                 . position Y [ PName \"Gmag\", PmType Quantitative ]
                 . column [ FName \"Cluster\", FmType Nominal ]

    sel = selection
            . select "picked" Interval [ BindScales ]

in toVegaLite [ gaiaData
              , trans []
              , errorEnc []
              , mark Rule []
              , sel []
              ]
@

For the interested reader, it was the calculation of the @\"plx_hi\"@
column that lead me to the discovery that the columns were being
read in as a string, and the introduction of the 'Parse' option
to 'gaiaData'.

As can be seen, the @e_plx@ terms are generally very small. This is
good for anyone using the data, as we want precise measurements, but
makes it harder for me to come up with meaningful visualizations! I
have taken advantage of the 'BindScales' interaction to zoom in on
a subset of stars which show larger parallax errors:

<<images/vl/errormanual-zoomed.png>>

-}

errorManual :: VegaLite
errorManual =
  let trans = transform
                . filter (FExpr "datum.Cluster[0] == 'C' || datum.Cluster[0] == 'H'")
                . calculateAs "datum.plx - datum.e_plx" "plx_lo"
                . calculateAs "datum.plx + datum.e_plx" "plx_hi"

      errorEnc = encoding
                  . position X [ PName "plx_lo"
                               , PmType Quantitative
                               , PScale [SZero False]
                               , PAxis [ AxTitle "parallax (mas)" ]
                               ]
                  . position X2 [ PName "plx_hi" ]
                  . position Y [ PName "Gmag", PmType Quantitative ]
                  . column [ FName "Cluster", FmType Nominal ]

      sel = selection
              . select "picked" Interval [ BindScales ]

  in toVegaLite [ gaiaData
                , trans []
                , errorEnc []
                , mark Rule []
                , sel []
                ]

{-|

Alternatively, I could have made life simpler for myself and used
the 'ErrorBar' (or 'ErrorBand') mark type, together with 'XError'
(or 'YError') to indicate that the channel gives the offset from
the central value. For this visualization I restrict to a single
cluster (since I now know there's only one in this sample which
begins with @C@), but retain the 'column' encoding as a means
to getting a useful title. I've also switched things so that the
errors are back along the y axis.

<<images/vl/errorauto.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoBhc55a2AgAwBdMAF5RYAOQdJkAL5CANOChZ4sANZ1INWJgBG6yMoiRG0eHVARTzWOW0ALaNAAOyXAHpPCAO5sAc1JoR2Z9HhoAY3REali2aKxPABF0ZgCAITsNKk9HADcqAPhPNV4aPMLiz3MS4tJ4AFp4JoA2AEZW5vaBRot9cip2+DZEdEbHKnh6GjZoZHzjFVMMbCYrZZtIV3VkKg2bQ6gAcTUA7URWfRolo62AJQBBAH0ASQ57gGULq5uTO6grnIAA8flhrrR-ndIMkAKJvD7ffCQS7gv6bQ46Z5A0HI1EQyAYsByTYkiByf6QAAkyEikzUThc7i8niqIyCITCbFI6E8tPpJTZjXIwVy+QALGwAFbIGK3KBURDReikRDnfDWLYAT1hsD0tA1JFIVHI9G0VGxIPkUKguLAxDIJrNyNO8HOiig0C1rn2yIAjswkNBgkxSIVrZtINFuFhEFYjU7tFweHxjJ7vb6oGMsKr4A4KZGtQcjgnTdocfLMbS85niAAvGjoOioPN7AvQr0+7QBoMh4Phm2meDA0jIePB6CDcvqPPkYdgAAUZQAlPJSSp21BoogyOr7SS5EA Open this visualization in the Vega Editor>

@
let trans = transform
              . filter (FExpr \"datum.Cluster[0] == \'C\'\")

    errorEnc = encoding
                . position Y [ PName \"plx\"
                             , PmType Quantitative
                             , PScale [SZero False]
                             , PAxis [ AxTitle \"parallax (mas)\" ]
                             ]
                . position 'YError' [ PName \"e_plx\" ]
                . position X [ PName \"Gmag\", PmType Quantitative ]
                . column [ FName \"Cluster\", FmType Nominal ]

in toVegaLite [ gaiaData
              , trans []
              , errorEnc []
              , mark 'ErrorBar' []
              ]
@
-}

errorAuto :: VegaLite
errorAuto =
  let trans = transform
                . filter (FExpr "datum.Cluster[0] == 'C'")

      errorEnc = encoding
                  . position Y [ PName "plx"
                               , PmType Quantitative
                               , PScale [SZero False]
                               , PAxis [ AxTitle "parallax (mas)" ]
                               ]
                  . position YError [ PName "e_plx" ]
                  . position X [ PName "Gmag", PmType Quantitative ]
                  . column [ FName "Cluster", FmType Nominal ]

  in toVegaLite [ gaiaData
                , trans []
                , errorEnc []
                , mark ErrorBar []
                ]



{-|

In this plot the error range is calculated by Vega-Lite,
and is taken from the standard deviation of the @Gmag@ field
('StdDev'). The 'MTicks' and 'MRule' constructors are used to
color the different parts of the error bars. Since the error bar
does not reference the central value, a separate layer is used
to add a square symbol ('SymSquare') at the average ('Mean')
value of the distribution.

<<images/vl/errorbars.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFACYCGKpGYoEEkArgE4A2VMKKADgM7oD0fRqQDuAOkTwU0egCN63WIwDGAewB2KWBtGqAtnwAiK+ogBCTANaw+0AG6xEpPrtLdNjG-cd8yFPo-hSAFpSYIA2AEYwkIicIIoZZlgI0lE1FSC4UmJFURRuW0hCWigAMxVGFzRMGhKoTlJGBSpauqgAcRdENjV6XRlFIqI2yAAlAEEAfQBJAGFRgGUevoHGIba6TmYAD2X+weKNyAMAURn5pcxIXv21w5HYSa3dq5vVyGHaAF9Pn4gvw6QYTwYhSKi4AhESAAEm4SjgLjYqC4vAEXlSEikslE8BUfDhCKc6KCzEk1lsABZRAArbjqdZQZikACeg0wAG1Pq1aJAXIwLC1PiVIIx6EkWpBVMwKmxNKRWAChTzYNtNBo2G4coV7nVIChmZxYGxFIwKjJGgyRih4EoLNwJVKZVdOEwtka-nVFQ81KpiPA1N0akq6MyJaV4LBmMQ2J1SN18FA4fKjTVIAAvRQqKileUKAFQfWGtgAR3opA0knI8HskA9JTrYC9tG5dD5AtT3HgGaoACYCInoKQi1duKXGkaE3qDSn6ip-Wgm8KtL7-YHqMGoKHU+HI9GrrH41A44hGA5yDPebBy0UC9OS2WKxRrTWG-9fkQALqA5cqP0BiUvNQkA7lGbCzMw8juDeU7DlA6S6P68q1k2krqOGa7AD8XxAA Open this visualization in the Vega Editor>

@
let cluster = position X [ PName \"Cluster\", PmType Nominal ]

    barOpts = [ 'MExtent' 'StdDev'
              , 'MTicks' [ 'MColor' \"purple\" ]
              , 'MRule' [ MColor \"teal\" ]
              ]
    range = [ mark ErrorBar barOpts
            , encoding
                . position Y [ PName \"Gmag\"
                             , PmType Quantitative
                             , PScale [ SZero False ]
                             ]
                $ []
            ]

    center = [ mark Point [ 'MShape' 'SymSquare', 'MSize' 20 ]
             , encoding
                 . position Y [ PName \"Gmag\"
                              , PmType Quantitative
                              , PmType 'Mean'
                              ]
                 $ []
             ]

in toVegaLite
      [ gaiaData
      , encoding (cluster [])
      , layer (map asSpec [ range, center ])
      , width 300
      , height 300
      ]
@

-}

errorBars :: VegaLite
errorBars =
  let cluster = position X [ PName "Cluster", PmType Nominal ]

      barOpts = [ MExtent StdDev
                , MTicks [ MColor "purple" ]
                , MRule [ MColor "teal" ]
                ]
      range = [ mark ErrorBar barOpts
              , encoding
                  . position Y [ PName "Gmag"
                               , PmType Quantitative
                               , PScale [ SZero False ]
                               ]
                  $ []
              ]

      point = [ mark Point [ MShape SymSquare, MSize 20 ]
              , encoding
                  . position Y [ PName "Gmag"
                               , PmType Quantitative
                               , PAggregate Mean
                               ]
                  $ []
              ]

  in toVegaLite
      [ gaiaData
      , encoding (cluster [])
      , layer (map asSpec [ range, point ])
      , width 300
      , height 300
      ]


{-|
The next plot shows the 'ErrorBand' mark, which fills the area between
the chosen range with a color, and optional borders. Here the
blue band shows the calculated standard deviation - as used in
'errorBars' - and the gray band with borders shows the inter-quartile
range. On top of these are drawn the median (blue) and median (green
dashed) lines.

<<images/vl/errorband.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFACYCGKpGYoEEkArgE4A2VMKKADgM7oD0fRqQDuAOkTwU0egCN63WIwDGAewB2KWBtGqAtnwAiK+ogBCTANaw+0AG6xEpPrtLdNjG-cd8yFPo-hSAFpSYIA2AEYwkIicIIoZZlgI0lE1FSC4UmJFURRuW0hCWigAMxVGFzRMGhKoTlJGBSpauqgAcRdENjV6XRlFIqI2yAAlAEEAfQBJAGFRgGUevoHGIba6TmYAD2X+weKNyAMAURn5pcxIXv21w5HYSa3dq5vVyGHaAF9Pn4gvw6QYTwYhSKi4AhESAAEm4SjgLjYqC4vAEXlSEikslE8BUfDhCKc6KCzEk1lsABZRAArbjqdZQFCSJJsWYqXQNRjwNSIMBCHmwbgMyDMUgAT0GmAA2p9WrRIC5GBYWpBYNtNBo2G4coV8IyxZxYGxFIwKjJSGpiJAAZ86FpVMRud0arb5WKVaV4LBmFarp1SN09ZA4aQWTVIAAvRQqKilUMKAH6w1sACO9AtTIoTPs1tdf2+92oroVjWVLo2UBkFRyTRVblNVgMrmgVClYT1ACYALo2iuQVTMCpsRBCd2Fkqq9VaapQeApu6uugoA1Gq4ms0Wq158dQe0qR08lqLqDu8Oe72+jpdIpQENh6iR6Ox+OwROQZfJq5pjOScjwHP5iUgFgL2tBynQipllApJqEaO6qmoDpOkefankWFZ0OePpsP6gbHnQd6rg+Uams+zAJjuE4BiODjkERCqwI6FoMn2H70d+Gi-tmRrHsB-y-IW4FQJBKoDkOVw0VoN7BigDawE23AttK7ZgB2eoUp2XZBmxbAwUaoETnuB7OuhRxoUJE5YZekC4SxRyESqJExpgcbka+lHytRjC0ZobC6LAzEeUuK6pumnFZv+PEVnxIG-EQWlQkZyHhi8D5WayzDyO40k6a87LcqG1qgf26ieiZwA-F8QA Open this visualization in the Vega Editor>

@
let posY extra = position Y ([ PName \"Gmag\"
                             , PmType Quantitative
                             , PScale [ SZero False ]
                             ] ++ extra) []

    bands = [ [ encoding (posY [])
              , mark 'ErrorBand' [ MExtent StdDev ]
              ]
            , [ encoding (posY [])
              , mark ErrorBand [ MExtent 'Iqr'
                               , 'MBorders' [ 'MStrokeDash' [ 6, 2 ] ]
                               , MColor "gray"
                               ]
              ]
            , [ encoding (posY [ PAggregate Median ])
              , mark Line []
              ]
            , [ encoding (posY [ PAggregate Mean ])
              , mark Line [ MColor \"green\"
                          , MStrokeDash [ 6, 2, 4, 2 ]
                          ]
              ]
            ]

in toVegaLite
    [ gaiaData
    , encoding (position X [ PName \"Cluster\", PmType Nominal ] [])
    , layer (map asSpec bands)
    , width 300
    , height 300
    , title "Comparing ranges" []
    ]
@

Note that I don't think this is a good visualization
for this /particular/ dataset, since it implies there's a
connection or correlation between clusters, as given by the
x-axis ordering, but the aim here is to show how to use @hvega@
rather than creating sensible plots!

-}

errorBand :: VegaLite
errorBand =
  let posY extra = position Y ([ PName "Gmag"
                               , PmType Quantitative
                               , PScale [ SZero False ]
                               ] ++ extra) []

      bands = [ [ encoding (posY [])
                , mark ErrorBand [ MExtent StdDev ]
                ]
              , [ encoding (posY [])
                , mark ErrorBand [ MExtent Iqr
                                 , MBorders [ MStrokeDash [ 6, 2 ] ]
                                 , MColor "gray"
                                 ]
                ]
              , [ encoding (posY [ PAggregate Median ])
                , mark Line []
                ]
              , [ encoding (posY [ PAggregate Mean ])
                , mark Line [ MColor "green"
                            , MStrokeDash [ 6, 2, 4, 2 ]
                            ]
                ]
              ]

  in toVegaLite
      [ gaiaData
      , encoding (position X [ PName "Cluster", PmType Nominal ] [])
      , layer (map asSpec bands)
      , width 300
      , height 300
      , title "Comparing ranges" []
      ]


{-|

An alternative visualization of a distribution is the \"box and
whiskers\" plot, which can be achieved in @hvega@ with the
'Boxplot' mark. The example below shows the default settings, but
the various components can be controlled with 'MBox', 'MMedian',
'MOutliers', and 'MTicks'.

<<images/vl/errorbox.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFALYCGATgNYZQBGA9gB4AOANvWoRJACakqkaoCNwCu5VjRgoUzAM7oA9IvKkA7gDpE8FNFG1Rc2OQDG9AHYpYljWeKKAIvVGIAQuMqxF0AG6xEpIpkclbk3n4BinwCigHwpAC0pIkAbACMKUlpOAkCtKywaaQa5vQJcKQ8xhoocj6QXCKQAGb05GRomMIi3MwURkJEPdwA4mSIUuaixLTGDUPDkABKAIIA+gCSAMJLAMqT07Pk88NNbIwHM3ONp5AOAKKbO-uYkFNXxzeLsGvnl0eQBYQAC+C1BIMakDU8B4uhouAIREgABI5CY4GQpKhZAplBFitpdPoNPB6Io0RjAviEqwdF4fAAWDQAKzkFhOUGsZh48HMEy6C0gFy6LXgsFYPCkW1YhlCDSgKAAnsxYJN6MReaRJMCvpBFUJReLJa8xqQJvgFcrVa8AI6iUiWHT8eB+SDgsA6pFmczNJBCUHAoA Open this visualization in the Vega Editor>

@
toVegaLite
    [ gaiaData
    , encoding
        . position X [ PName \"Cluster\", PmType Nominal ]
        . position Y [ PName \"Gmag\", PmType Quantitative ]
        $ []
    , mark 'Boxplot' []
    , width 300
    , height 300
    ]
@

The 'Boxplot' option supports two different \"ranges\":

 * the default is the Tukey Box plot, where the whiskers span
   a range @Q1 - k * IQR@ to @Q3 + k * IQR@, @IQR = Q3 - Q1@,
   @Q1@ and @Q3@ are the lower and upper inter-quartile values
   (so 25 and 75 per cent of the distribution), and @k@
   defaults to 1.5 but can be changed with 'IqrScale';

 * or 'ExRange', which uses the full range of the data (i.e.
   minimum to maximum values).

-}

errorBox :: VegaLite
errorBox =
  toVegaLite
      [ gaiaData
      , encoding
          . position X [ PName "Cluster", PmType Nominal ]
          . position Y [ PName "Gmag", PmType Quantitative ]
          $ []
      , mark Boxplot []
      , width 300
      , height 300
      ]


{-|

Here I combine 'errorBox' with 'smallMultiples2' so we can compare
the distribution (from the histogram) with that from the box plot.

<<images/vl/comparingerrors.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCE6BiYZLA4AgXpRZgA4iYshJBiWLLcgqmVVZAASgCCAPoAkgDCXQDKDU0tbWlVGWR4AB4Tza047bOYCgCigyPjqI0r0x1FkPx980sHk6uQJ2AAvh1PEA8zUBRk-EIVp3iwAE82qgANr3QqzSAmHAaBrcUrrDZnMSiaBsMT1VAQpGiPA5ApZNj8PDQBpDPB8cypKDwAFfCYiWTo2AEFJQYIsKSk1BNPB4N73U5XNCE4ncqC1WD1NmQbjowjwHBYfgy2n0g4ARywsAkHAQbH0kAFGwyAN+JoyUpYOAMCH4DVEWAkiItNLp9s12t1iHgBvt71dkFgCzYFAJvvgwQaADkjjgwCJMmBRogykbBRgXhss0VjVVsadobCsRnMLJ+GidQTcfiDkFYEJYXmTbKRMLgJAzDgRFoGgwiBx7c2kcpI0S06hMiyBAGkWqPVBuG35iJ8hnh6cpKj0Zi0KWoDW1ljRSSyRSzNM3eqoBEmWIWdTIByuYRefzZ5D2yfxZBJdKrwukBajqEb6oaG6dGax66CyyqEAAHL4OZVMhrwdAAuukeaQAAJBQQjRCYDQkOQVC0FYYTsJwPBMGwIjUPhhFGBRTh4IO1C6AALEwABWFDiIikC4s0YhhqgADM7xZA2-D5MemREqeBzkpSl6QPODJ3g+2GiGICm7sATwPEAA Open this visualization in the Vega Editor>

@
let histEnc = encoding
                . position X [ PName \"Gmag\", PmType Quantitative, PBin [] ]
                . position Y yAxis
                . color [ MName \"Cluster\", MmType Nominal, MLegend [] ]

    errEnc = encoding
               . position X [ PName \"Gmag\", PmType Quantitative ]
               . position Y [ 'PNumber' 80 ]
               . color [ MName \"Cluster\", MmType Nominal, MLegend [] ]

    yAxis = [ PAggregate Count
            , PmType Quantitative
            , PAxis [ AxTitle \"Number of Stars\" ]
            ]

    boxOpts = [ 'MMedian' [ MColor \"black\" ]
              , 'MBox' [ MStroke \"white\" ]
              , 'MNoOutliers'
              ]

    histSpec = asSpec [ mark Bar [], histEnc [] ]
    errSpec = asSpec [ mark Boxplot boxOpts, errEnc [] ]

    combinedSpec = asSpec [ layer [ histSpec, errSpec ] ]

in toVegaLite
    [ gaiaData
    , columns 3
    , facetFlow [ FName \"Cluster\", FmType Nominal ]
    , specification combinedSpec
    ]
@

The main additions here are the configuration of the box plot - with
'MMedian', 'MBox' (used to ensure the box is visually distinct from
the bar for the Pleiades cluster, where they overlap), and 'MNoOutliers'
(to turn off the display of the outliers) - and the use of 'PNumber'
to define the location on the y axis of the boxplot visualization.
Note that 'PNumber' is defined in pixel units, with 0 being the
top of the visualization and 80 was found by trial and error.

-}

comparingErrors :: VegaLite
comparingErrors =
 let histEnc = encoding
                . position X [ PName "Gmag", PmType Quantitative, PBin [] ]
                . position Y yAxis
                . color [ MName "Cluster", MmType Nominal, MLegend [] ]

     errEnc = encoding
                . position X [ PName "Gmag", PmType Quantitative ]
                . position Y [ PNumber 80 ]
                . color [ MName "Cluster", MmType Nominal, MLegend [] ]

     yAxis = [ PAggregate Count
             , PmType Quantitative
             , PAxis [ AxTitle "Number of Stars" ]
             ]

     boxOpts = [ MMedian [ MColor "black" ]
               , MBox [ MStroke "white" ]
               , MNoOutliers
               ]

     histSpec = asSpec [ mark Bar [], histEnc [] ]
     errSpec = asSpec [ mark Boxplot boxOpts, errEnc [] ]

     combinedSpec = asSpec [ layer [ histSpec, errSpec ] ]

 in toVegaLite
     [ gaiaData
     , columns 3
     , facetFlow [ FName "Cluster", FmType Nominal ]
     , specification combinedSpec
     ]


-- $intro-dashboard
-- In the following visualization I try to combine as many of the
-- concepts we have explored in this tutorial into one. There are
-- layers, combined visualizations, and a
-- selection that ties the different plots together! How much more
-- could you want?

{-|

This is based on the
<https://vega.github.io/vega-lite/examples/concat_marginal_histograms.html Marginal Histogram>
example from the Vega-Lite
<https://vega.github.io/vega-lite/examples/ Example Gallery>.
There is very-little new in this plot, in that pretty-much everything
has been shown before. However, there are some interesting wrinkles,
such as

  * combining multiple plots, in this case the \"top\" area - which is
    a histogram on top of a plot which is itself a \"map\" and
    a histogram - and \"bottom" area - which is just a point plot -
    requires judicious use of 'asSpec';

  * selection works in __both__ the main plots - the \"map\" and \"point\"
    plots - to highlight all stars in the same cluster, and I was
    pleasantly surprised to find out I could just use the same selection
    specification (@selCluster@) in both (hopefully I am not just
    enjoying a hole in the Vega-Lite specification);

  * I have been perhaps too defensive in defining the Right Ascension
    and Declination axes in the relevant plots, as I want to make sure
    the histogram bins and plot axes are well aligned (that is the
    @'Nice' False@ statements may not be needed when defining the
    histogram axes);

  * I am not 100% sure I understand what is going on with the
    grid labels on the Declination axis, as I had thought I was
    asking for marks every 15 degrees, but the plot shows them
    every 30 degrees (however, if I change the @deTicks@ array then
    the marks change in ways I currently do not understand);

  * and I have decided to display Right Ascension in hours, rather than
    degrees, because why have one way to measure a value when you can
    have many!

<<images/vl/combinedplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJ5k8oAlAQUgBooBjeAGwYFdn5oBTcyAEy5ssAOmoB9AJIBhCgGUwAejABGAKyQAvgF1a4RukSoAlgHNyxAG7HuAdwuRkcdAGte+GAhQAHeLG6I0FqaehACXPAW+mFssMx8ABbQ0N7IuIqKCLYipsbQCWwARmzI3LAMhjyBIhVYigAi6GymAEKxbooJltym8IpYpDywnd29ioLQfb3G8AC08HMAbCqL8yoADLOThczcKvAiiOizCdzw-GUi0MiWdNFQGNhcURCvUL6wpS9vr5AA4gNzB5EMJCmU7j9fhIZPI+CCsGDYBDIe9mAAPOGg8GhFGQeoAUSksjkmIR2Pub0g3HE3nRpMRkApYE09xZEBC+kglgqiCYQXwBHuoEhXJ5fPIgpRYGFUqgpzMSXIG3WOKlkFsxn4+XIABZ1iqmZTOABPcECw0-GWy34DWAuBwVZiYPimBDGuhQaDG7zuKCFPxaVXWqABCr8YyIIHSi1qjH4K3BykmbjMfh8ajIxOUwoRhzcNFVfmEFVgABMOt0UCc3G85FL9EgiGMDF9qBYpQ5WZ+jkwRYE3GQLcQ4cjma7nu9vsgAEc2EhoHkuMZumOuyQ0cYyPgQcxmDGUZ3x5B3fH92rByxfcR+OgBrmBSWAMz69baQ-jqDwUyunpcKcVNhAlXLMYEnPhZ3nRcFxXIM13gDct2lGA8l2cgd2YNlx0wxNsNld9ZQTa1PCQNBMBwAViBMZghgcUpdgYBdDD4bxm3tTQdFg2VIFte0PH9JFOLVUN0BHKNCODSBHWdU8PzCSwWDYKcf3dQSJJ5cNGMQb5ZIeGxUz4aRmBKGjVMTRwU24BjjCYjwWIYe1TIkr0fThW8IxYRkdNwnDHJFONox0yBk30jwM183Ecy0+MqQLAIiwIEty0rRweFrfB6ygJsW3INtmA7cKRWQXs+AuQcAlE4CzOcqcIMCKDl14Aru3gzc0I4PcP3wsyTwC2THCYVDopvO8ouLehn31N8mspL8f16Hg+AAoDpt+arwLnOrJmgxqz1xFrEOIBdoEGsB0O84NzqlS6fmuiBtAtLrXnEykEjFZ5zUTZ7cXlUxFXwPUDTXHiHHQXwGDyHr1hENQGzWjxwfKVDHrVDUtQSXV9Sa8z6M0hw7N4pC4arCNTFQhtgv4RCCEgQzjPBN9kdxYSKpko8pKRVmPy5BSlP8FTdpFdS8ms0avrMimDKM6sBIF3E6Ms3HbNYyqQKJxs3MQDzZbeW6DxWyB-LFriJdCmgVrCC8TuvW94HvMaywrBsstbdtuEZoiitgPtSqHCrzYnFyPFqo6lxg7Xfn2hwjpOyAKAVaAwCoMqUBFsAAAoEiaT4AEotHD93cR6o21RNqACSJWF-f6y8HGG23RoIWYAE4Sxb5LnZy12C7VNXg-qsPAsjzm+vkoyBwlcPIWbwGdNeWYAHYYcnn5ZkWGfZ7AWYdSXjeIFmZ9-beWZ1EP1519nk-l7eA+r9ebfT4gNeH7ARfn5b2-dFv5DjqnepLOYdyml04XB-AOXOy9daQkga8aB3depmR+n9MAAMsao21PgZUWMTRmkIOHYuuJgbRXZi6N0HpQKBz9AGOBIpmYk20n1Q2t8LYDSvAIG2dsErjRfFNJhn5vz+Hmv+Joy1eHkJqhtEO20VZwQQlHFCvozq32oYXehgVS54kJDCEkz9ICRTzLFICApp70Dbg2asaVVAw0ys2F2eU3Y6N7hI-uO1d6flkdudqSiIH5xWvgkUcASKPHIoQSixhqI4OIPLKyNl3jK3Yp-QKhDKEy0CrQ0cw8ubEIySPHmJDuD81cZJQwGkRaqNnkFPSaYPC02ltIo8UTFaxPsnUtcasjhYHcvEXh0CrqnwNmUrmltWF1w4U+bhyi1SzQEX+RawiggOLAkHJxW0GotIkkPQm8i2q7h6XrL+RdRHqPLlotZRE9HRXzIWCUxiwCmKrKlJUVjGw2M7nYiZuJHGQRWQPXe65WoeN3F4ry+dZb3RwvuBJXFkBgzoelc+IpCjCMpnwVAUt0YPSZJCyk0L4Dg3SWWeFfokWISCmizyMCgziW4n4AmxBQa4ohuQKGTy1YIwYEjTi6pNToLAGoUshLsYK1KdFfGUdFnE0jGTXSKZkUChplLGiDNOVpLEhaIpToObwNxKPRSeSClcSFo0-BFSZWSzpikiSDThVNIcrtMRrkOmay6daa6jN+nZKTJU5idJHLVytvajwTpzBOxefgXK+V9wBqgH3b5LiuIbMOls2yfgWCcDROnDpu5jDzHKKUdSudXWqWPAM346iARfjqVGmcyzQ5xsme4zZP8+B-DAICJs0A2AXDzldJkuFMKQsgAAEkHKcAYiRkipHSIoUYBxcj5CKCIayigR3cAGNO38swAE8GnTqEQAArIqWkcTf39TwAszbbbwDABMK9qBYC3jAH4AAGsuXAKgAAc6xd3rCbo+ee76yFIAYJnTV3FNT8CRiATQQA Open this visualization in the Vega Editor (although the link is long, and may not work with Internet Explorer)>

@
let trans = transform
              . calculateAs \"datum.RA_ICRS / 15\" \"RA\"

    quant n = [ PName n, PmType Quantitative ]

    big = 400
    small = 100
    wmain = width big
    hmain = height big
    wsub = width small
    hsub = height small
    noTitle = PAxis [ AxNoTitle ]

    raAxis = [ PScale [ SDomain (DNumbers [ 0, 24 ])
                      , SNice (IsNice False)
                      ]
             , PSort [ Descending ]
             , PAxis [ AxTitle \"Right Ascension (hours)\" ]
             ]

    deMin = -90
    deMax = 90
    deStep = 15

    -- we do not get ticks/grids at all these values, but it does
    -- something (e.g. if do not specify the axis ticks are different)
    --
    deTicks = Numbers [ deMin, deMin + deStep .. deMax ]
    deAxis = [ PScale [ SDomain (DNumbers [ deMin, deMax ])
                      , SNice (IsNice False)
                      ]
             , PAxis [ AxTitle \"Declination (degrees)\"
                     , AxValues deTicks
                     ]
             ]

    colorEnc = color [ MSelectionCondition (SelectionName \"pick\")
                         [ MName \"Cluster\", MmType Nominal ]
                         [ MString \"grey\" ]
                     ]
    mapEnc = encoding
               . position X (quant \"RA\" ++ raAxis)
               . position Y (quant \"DE_ICRS\" ++ deAxis)
               . colorEnc

    circleMark = mark Circle [ MOpacity 0.5 ]

    mapSpec = asSpec [ mapEnc []
                     , circleMark
                     , wmain
                     , hmain
                     , selCluster []
                     ]

    -- histogram of the RA values
    --
    raBinning = [ PBin [ 'Extent' 0 24
                       , Step 2
                       , Nice False
                       ]
                , PSort [ Descending ]
                , PAxis []
                ]

    -- histogram of the Declination values
    --
    deBinning = [ PBin [ Extent deMin deMax
                       , Step deStep
                       , Nice False
                       ]
                , PAxis []
                ]

    histAxis = [ PAggregate Count
               , PmType Quantitative
               , noTitle
               , PScale [ SDomain (DNumbers [ 0, 3000 ]) ]
               ]

    raEnc = encoding
              . position X (quant \"RA\" ++ raBinning)
              . position Y histAxis

    deEnc = encoding
              . position Y (quant \"DE_ICRS\" ++ deBinning)
              . position X histAxis

    allRA = [ raEnc []
            , mark Bar [ MColor \"gray\" ]
            ]
    filtRA = [ filterCluster []
             , raEnc
                 . colorEnc
                 $ []
             , mark Bar []
             ]

    allDE = [ deEnc []
            , mark Bar [ MColor \"gray\" ]
            ]
    filtDE = [ filterCluster []
             , deEnc
                 . colorEnc
                 $ []
             , mark Bar []
             ]

    raSpec = asSpec [ wmain, hsub, layer [ asSpec allRA, asSpec filtRA ] ]
    deSpec = asSpec [ hmain, wsub, layer [ asSpec allDE, asSpec filtDE ] ]

    borderSpacing = 20

    mapAndDecSpec = asSpec [ spacing borderSpacing
                           , 'bounds' 'Flush'
                           , hConcat [ mapSpec, deSpec ]
                           ]

    histSpecs = [ raSpec, mapAndDecSpec ]

    -- select the cluster which the star belongs to; do not use
    -- \"nearest click\" as that means a user can not cancel the
    -- selection.
    --
    pick = \"pick\"
    selCluster = selection
                   . select pick Single [ Fields [ \"Cluster\" ] ]

    filterCluster = transform
                      . filter (FSelection pick)

    plxOpts = [ PScale [ SType ScLog, SNice (IsNice False) ]
              , PAxis [ AxTitle \"parallax (milli-arcsecond)\" ]
              ]
    gmagOpts = [ PAxis [ AxTitle \"G magnitude\" ] ]

    encData = encoding
                . position X (quant \"plx\" ++ plxOpts)
                . position Y (quant \"Gmag\" ++ gmagOpts)

    parallaxSpec = asSpec [ width (big + borderSpacing + small)
                          , encData
                              . colorEnc
                              $ []
                          , circleMark
                          , selCluster []
                          ]

    allSpecs = [ asSpec [ spacing borderSpacing
                        , bounds Flush
                        , vConcat histSpecs
                        ]
               , parallaxSpec ]


in toVegaLite
   [ gaiaData
   , trans []
   , vConcat allSpecs
     -- remove the "other" axis (e.g. top of Y, right for X)
   , configure
       . configuration (ViewStyle [ 'ViewStroke' \"transparent\" ])
       $ []
   , title \"Gaia data from arXiv:1804.09378\" [ 'TAnchor' 'AMiddle' ]
   ]
@

Here is the visualization after selecting a star:

<<images/vl/combinedplot-selected.png>>

-}

combinedPlot :: VegaLite
combinedPlot =
  let trans = transform
                . calculateAs "datum.RA_ICRS / 15" "RA"

      quant n = [ PName n, PmType Quantitative ]

      big = 400
      small = 100
      wmain = width big
      hmain = height big
      wsub = width small
      hsub = height small
      noTitle = PAxis [ AxNoTitle ]

      raAxis = [ PScale [ SDomain (DNumbers [ 0, 24 ])
                        , SNice (IsNice False)
                        ]
               , PSort [ Descending ]
               , PAxis [ AxTitle "Right Ascension (hours)" ]
               ]

      deMin = -90
      deMax = 90
      deStep = 15

      -- we do not get ticks/grids at all these values, but it does
      -- something (e.g. if do not specify the axis ticks are different)
      --
      deTicks = Numbers [ deMin, deMin + deStep .. deMax ]
      deAxis = [ PScale [ SDomain (DNumbers [ deMin, deMax ])
                        , SNice (IsNice False)
                        ]
               , PAxis [ AxTitle "Declination (degrees)"
                       , AxValues deTicks
                       ]
               ]

      colorEnc = color [ MSelectionCondition (SelectionName "pick")
                           [ MName "Cluster", MmType Nominal ]
                           [ MString "grey" ]
                       ]
      mapEnc = encoding
                 . position X (quant "RA" ++ raAxis)
                 . position Y (quant "DE_ICRS" ++ deAxis)
                 . colorEnc

      circleMark = mark Circle [ MOpacity 0.5 ]

      mapSpec = asSpec [ mapEnc []
                       , circleMark
                       , wmain
                       , hmain
                       , selCluster []
                       ]

      -- histogram of the RA values
      --
      raBinning = [ PBin [ Extent 0 24
                         , Step 2
                         , Nice False
                         ]
                  , PSort [ Descending ]
                  , PAxis []
                  ]

      -- histogram of the Declination values
      --
      deBinning = [ PBin [ Extent deMin deMax
                         , Step deStep
                         , Nice False
                         ]
                  , PAxis []
                  ]

      histAxis = [ PAggregate Count
                 , PmType Quantitative
                 , noTitle
                 , PScale [ SDomain (DNumbers [ 0, 3000 ]) ]
                 ]

      raEnc = encoding
                . position X (quant "RA" ++ raBinning)
                . position Y histAxis

      deEnc = encoding
                . position Y (quant "DE_ICRS" ++ deBinning)
                . position X histAxis

      allRA = [ raEnc []
              , mark Bar [ MColor "gray" ]
              ]
      filtRA = [ filterCluster []
               , raEnc
                   . colorEnc
                   $ []
               , mark Bar []
               ]

      allDE = [ deEnc []
              , mark Bar [ MColor "gray" ]
              ]
      filtDE = [ filterCluster []
               , deEnc
                   . colorEnc
                   $ []
               , mark Bar []
               ]

      raSpec = asSpec [ wmain, hsub, layer [ asSpec allRA, asSpec filtRA ] ]
      deSpec = asSpec [ hmain, wsub, layer [ asSpec allDE, asSpec filtDE ] ]

      borderSpacing = 20

      mapAndDecSpec = asSpec [ spacing borderSpacing
                             , bounds Flush
                             , hConcat [ mapSpec, deSpec ]
                             ]

      histSpecs = [ raSpec, mapAndDecSpec ]

      -- select the cluster which the star belongs to; do not use
      -- "nearest click" as that means a user can not cancel the
      -- selection.
      --
      pick = "pick"
      selCluster = selection
                     . select pick Single [ Fields [ "Cluster" ] ]

      filterCluster = transform
                        . filter (FSelection pick)

      plxOpts = [ PScale [ SType ScLog, SNice (IsNice False) ]
                , PAxis [ AxTitle "parallax (milli-arcsecond)" ]
                ]
      gmagOpts = [ PAxis [ AxTitle "G magnitude" ] ]

      encData = encoding
                  . position X (quant "plx" ++ plxOpts)
                  . position Y (quant "Gmag" ++ gmagOpts)

      parallaxSpec = asSpec [ width (big + borderSpacing + small)
                            , encData
                                . colorEnc
                                $ []
                            , circleMark
                            , selCluster []
                            ]

      allSpecs = [ asSpec [ spacing borderSpacing
                          , bounds Flush
                          , vConcat histSpecs
                          ]
                 , parallaxSpec ]


  in toVegaLite
     [ gaiaData
     , trans []
     , vConcat allSpecs
     -- remove the "other" axis (e.g. top of Y, right for X)
     , configure
         . configuration (ViewStyle [ ViewStroke "transparent" ])
         $ []
     , title "Gaia data from arXiv:1804.09378" [ TAnchor AMiddle ]
     ]


-- $otherstuff
-- The tutorial ends not with a bang, but a few random visualizations
-- I thought of and couldn't find a better place to put them!

{-|

This visualization started out when I asked myself if I could
repeat the X axis at the top of the plot. I started off by
trying to use 'configuration' with the 'AxisTop' constructor,
but this didn't work (perhaps I didn't turn on the necessary
option), so I ended up with the following. It does show off
the use of 'AxLabelExpr' and 'AxDataCondition', but is not
perhaps the most-digestible visualization one could create!

As I could not work out how to duplicate the X axis with only
a single layer, I got creative and duplicated the data and
in the second layer moved the X axis to the top of the plot
with 'AxOrient', and ensured the data would not be displayed
(by setting the 'Text' value to the empty string).

The axis labels and the tick marks for the two X axes make
use of the @datum.index@ field, which is in the range 0 to 1 inclusive,
which I multiply by 8 (one less than the total number of clusters) and
check if the result is odd or even (ignoring the possibility
of floating-point inaccuracies in the conversion). The odd values are displayed
on the bottom axis and the even values on the top (the first
cluster, in this case @Blanco1@, has an index value of 0,
so is displayed on the top axis). The 'AxLabelExpr' option
is used to determine the label contents (if the condition
holds then it uses a trimmed and truncated version of the
default label, otherwise it is blank), and
'AxDataCondition' is used to control the opacity of the
tick marks. I had hoped to show some of the label-overlap
strategies in play here - controlled by 'AxLabelOverlap' - but
they didn't work well with the data and visualization size,
and I realised I could play with the new-to-Vega-Lite-4
'AxLabelExpr' and 'AxDataCondition' capabilities.

Normally a grid is not drawn for 'Nominal' axes, but I turn
it on (for the first layer) with 'AxGrid' just to help guide
the eye.

<<images/vl/duplicateaxis.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxhIBzWdAVwAcAjATz0MgGEAba5aAU1kgBdADRR45Sr3Lw+LAmXS0WkAMY1E0SKMjxkyxNSz1+kAL6DTxEcUgATGfBagSUarA7KAFtGi1kuAHoAhAB3ADpyAEtoT2p6bn41DV4NMLUsAIARGnIAITcAa14AzwA3KXgArF0+WBLy6QD7aErpSPgAWnhOgDYARh6uvoAGDpb6Dl4++DDEdA7PXnhbfjDoZFKtYghIDGwZJ22SSFp4WGReQ5cXSABxavJ9Q2MBYSObgCUAQQB9AEk2B8AMpPIwmN7XY60DgAD1BLy2kJ2mQAov9ASD8JADGDXu9jrwftC4VicQj8ZYXJSwKYIVAACTIFSLapeHx+QIBBozKIxOJhSLoAJMlmVbkdDjRYqlAAsYQAVsh0IhEVAOPBGCZ8EQXM5rpBqrACsoVJFYCpJqqbik1LZIohHvg9ZDICTSPibqhIrwOLZlJxuLUrUiYIxaJdSegsPb4B46UixDDInonR79erjBwvg7LfhhvGE1BMN6NMp6OgfFHgwnIBmfSiYbQBFjIqgABRt5qGAWIFYwsAAKjAAA4AJRgACkYAATGAALxzvqiODURAqGS8NtwSJYTsybt1jij0QAVmPYAA5BfR9WQ9BIioCgB5U6m6DMVOF-WlWPUCNgfM0xdJI7XvZUnEgH8uH-JcoD4HhlD3aBu3tPtBxHccp1nBc+jMICSFpfCdkoSI-XwFdeALGt72gXMoADHgTHw6lIUImsP3dL9dm9X14XBfDQ3DZQAEdqCQGiZEicpbxueAkxTUgYGiOjIAAOWefgwHQVAwB4M49BY65DIgQy2JIZ1jkNY0nRgXgYU0LEtDgsN-1s+yzCoqAbXQO0HSuEM3Qsl0vR9Mj6K4Ri8S498hMjaNEFjGTjjk5N-K4w9s3IOjAK-HZixSBy4MUJKbkPBsm2UVsOy7LAezQocx0nGd5znfMwBXNcNy3WAdyQg94Ezc8z1EK8b08l170fF94DfDigsLSDf3-HLcp2EDokFFUbKgv8WFg2yEKxPratQuz0MarCWuGPDVrMriSLC1BYwucb9RouiDA4DhmLTO79TmgSQt40kNKihaYtc0TxOiSTpNenYUoUsh3v-T6OGMgiKSOalrDW5UvUdUhLFMIA Open this visualization in the Vega Editor>

@
let trans = transform
            . aggregate [ opAs Count \"\" \"number\" ]
                        [ \"Cluster\" ]

    xAxis f = position X [ PName \"Cluster\"
                         , PmType Nominal
                         , PAxis [ 'AxLabelAngle' 0
                                 , 'AxOrient' (if f then SBottom else STop)
                                 , if f
                                   then AxTitle \"Cluster\"
                                   else AxNoTitle
                                 , 'AxLabelExpr' (xlabels f)
                                 , 'AxDataCondition'
                                   (Expr (xticks f))
                                   ('CAxTickOpacity' 1 0)
                                 , 'AxGrid' f
                                 ]
                         ]

    xlabels f =
      let v = if f then \"1\" else \"0\"
      in \"if((datum.index * 8) % 2 ==\" <> v <> \", truncate(trim(datum.label), 5), '')\"

    xticks f =
      let v = if f then \"1\" else \"0\"
      in \"(datum.index * 8) % 2 ==\" <> v

    yAxis f = position Y [ PName \"number\"
                         , PmType Quantitative
                         , PAxis [ if f
                                   then AxTitle \"Number of stars\"
                                   else AxNoTitle ]
                         ]

    -- f is True indicates first Layer (bottom X axis, should display
    -- the Y axis).
    enc f = encoding
            . xAxis f
            . yAxis f

    dataLayer = asSpec [ enc True []
                       , mark Circle []
                       ]

    axLayer = asSpec [ enc False []
                     , mark Text [ MText \"\" ]
                     ]

in toVegaLite [ gaiaData
              , trans []
              , layer [ dataLayer, axLayer ]
              ]
@

If anyone can come up with a simpler way to duplicate the X axis I'm all
ears!

-}

duplicateAxis :: VegaLite
duplicateAxis =
  let trans = transform
              . aggregate [ opAs Count "" "number" ]
                          [ "Cluster" ]

      -- Encode the X axis, where True indicates the lower axis
      -- and False the upper axis.
      --
      xAxis f = position X [ PName "Cluster"
                           , PmType Nominal
                           , PAxis [ AxLabelAngle 0
                                   , AxOrient (if f then SBottom else STop)
                                   , if f
                                     then AxTitle "Cluster"
                                     else AxNoTitle
                                   , AxLabelExpr (xlabels f)
                                   , AxDataCondition
                                     (Expr (xticks f))
                                     (CAxTickOpacity 1 0)
                                   , AxGrid f
                                   ]
                           ]

      -- For the bottom want every "odd" value, top every "even" value,
      -- so that the are alternated, but how do we encode this with the
      -- Vega expression syntax? There is a datum.index which is 0 to 1
      --
      -- How about just subsetting the strings for now
      xlabels f =
        let v = if f then "1" else "0"
        in "if((datum.index * 8) % 2 ==" <> v <> ", truncate(trim(datum.label), 5), '')"

      -- had hoped could use datum.label but there's no obvious guarantee
      -- of ordering of operations (or when the AxLabelExpr expression is
      -- evaluated), so repeat the logic.
      --
      xticks f =
        let v = if f then "1" else "0"
        in "(datum.index * 8) % 2 ==" <> v

      yAxis f = position Y [ PName "number"
                           , PmType Quantitative
                           , PAxis [ if f
                                     then AxTitle "Number of stars"
                                     else AxNoTitle ]
                           ]

      enc f = encoding
              . xAxis f
              . yAxis f

      dataLayer = asSpec [ enc True []
                         , mark Circle []
                         ]

      axLayer = asSpec [ enc False []
                       -- create an "invisible" mark
                       , mark Text [ MText "" ]
                       ]

  in toVegaLite [ gaiaData
                , trans []
                , layer [ dataLayer, axLayer ]
                ]


{-|

Way back in the tutorial I noted - in 'densityParallaxGrouped' - that
setting the 'density' option 'DnCounts' to @True@ resulted in
counts that were too high. This is because the values need to be
divided by the bin width, as shown in this visualization, where I:

- use an explicit grid for the density calculation, choosing the
  'DnExtent' and 'DnSteps' parameters to create a bin width of
  0.1 in parallax;

- sum up the resulting KDE (the @\"ykde\"@ field) to create @\"ycounts\"@;

- normalize the counts by the bin with using 'calculateAs' to
  create the @\"count\"@ field;

- which is plotted as a diamond, on top of a line showing the
  actual counts (calculated following 'starCount').

<<images/vl/comparecounts.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCE6BiYZLA4AgXpRZgA4iYshJBiWLLcgqmVVZAASgCCAPoAkgDCXQDKDU0tbWlVGWR4AB4Tza047bOYCgCigyPjqI0r0x1FkPx980sHk6uQJ2AAvh1PEA8zUAAkFELRJg0k5CotCsYXYnB4TDYImo31+RhBTjwHG0ugALEwAFYUcTrKB4WAATzaqAA2h1Cqd4PQxBRsrlCGSNmhICwcMoyNwCQzIEM8HxzJAALopKCwFisgwIfgM4CQERkBqiLASVKiiiK5Qqh6Cl5VYX3SAmHAaBpIsTS96nKSiaBsMT1VAUzpXZmZNj8PDQBq8-nTKDwAlkaXXESyO2wAhve6YLmO6MZN0er0HJUqy0bSABoMNACOWFgEg4CDY+lxTMgsAWbHVjszHGC3s1iUgutmrYw7ajRSdmCpBdpOVkDPjPc6rPZnO5PrMbX1TMwpm5Cw0cVVkAJK+lc-nkFTzdQVKw-HTszOC0kKtJAGYRVffNvyzPKIQAEwAdl8J86cRpHFjUEuO4mS7DZR1OccsA5f8SR5PkZzWB8MzFCVDEkGU5QVA4KGaNdFwOAk93VEUsndT0Gg3VdtXjEDZllPCoD3NchAjIQsHxNCDlsZomAIpsKDAagwD8Ft7kQzAjRNONHzYAAvYMwBfXxP3jSAzDZLQGiCWAhBNL9MAoIhYGzTjHFkcQvT0-1A3kyAyBEO18nuGiMmtERbXtCpyxdWVEzIg5pwFYisxsiIwzECMW0s9dPJ3XzkwYpsywzYLc3zQtEHgEsLXjBcqxrZlMvgBt-L4kTgKc550m3XdxDdB00CeB4gA Open this visualization in the Vega Editor>

@
let densTrans = transform
            . density \"plx\" [ DnAs \"xkde\" \"ykde\"
                            , DnGroupBy [ \"Cluster\" ]
                            , DnCounts True
                            , DnExtent 3 30
                            , 'DnSteps' 270
                            ]
            . aggregate [ opAs Sum \"ykde\" \"ycounts\" ]
                        [ \"Cluster\" ]
            . calculateAs \"datum.ycounts / 10\" \"count\"

    enc = encoding
          . position X [ PName \"Cluster\"
                       , PmType Nominal
                       ]
          . position Y [ PName \"count\"
                       , PmType Quantitative
                       , PAxis [ AxTitle \"Counts\" ]
                       ]

    densLayer = asSpec [ densTrans []
                       , enc []
                       , mark Point [ MShape 'SymDiamond'
                                    , MStroke \"black\"
                                    , MSize 200
                                    ]
                       ]

    countTrans = transform
                 . aggregate [ opAs Count \"\" \"count\" ] [ \"Cluster\" ]

    countLayer = asSpec [ countTrans [], enc [], mark Line [] ]

in toVegaLite
     [ gaiaData
     , layer [ countLayer, densLayer ]
     ]
@

Note that the same encoding specification is used on both layers,
since I arranged the data transforms to create two columns -
@\"Cluster\"@ and @\"count\"@ - in both cases.

-}

compareCounts :: VegaLite
compareCounts =
  let densTrans = transform
              . density "plx" [ DnAs "xkde" "ykde"
                              , DnGroupBy [ "Cluster" ]
                              , DnCounts True
                              , DnExtent 3 30
                              , DnSteps 270
                              ]
              . aggregate [ opAs Sum "ykde" "ycounts" ]
                          [ "Cluster" ]
              . calculateAs "datum.ycounts / 10" "count"

      enc = encoding
            . position X [ PName "Cluster"
                         , PmType Nominal
                         ]
            . position Y [ PName "count"
                         , PmType Quantitative
                         , PAxis [ AxTitle "Counts" ]
                         ]

      densLayer = asSpec [ densTrans []
                         , enc []
                         , mark Point [ MShape SymDiamond
                                      , MStroke "black"
                                      , MSize 200
                                      ]
                         ]

      countTrans = transform
                   . aggregate [ opAs Count "" "count" ] [ "Cluster" ]

      countLayer = asSpec [ countTrans [], enc [], mark Line [] ]

  in toVegaLite
       [ gaiaData
       , layer [ countLayer, densLayer ]
       ]


{-|

In this example I compare the parallax values

- as the raw distribution, using the ticks display we saw in
  the very first plot, 'stripPlot', (although with a few
  adjustments)

- against a smoothed version of the distribution, calculated using
  the 'regression' transform (e.g. 'densityParallax').

The only new things here are configuration options
for the X axis - that is, the use of 'AxLabels', along with
'AxNoTitle', to ensure the X axis of the density plot only has
grid lines - and the legend options were set to
center the title.

<<images/vl/parallaxview.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCE6BiYZLA4AgXpRZgA4iYshJBiWLLcgqmVVZAASgCCAPoAkgDCXQDKDU0tbWlVGWR4AB4Tza047bOYCgCigyPjqI0r0x1FkPx980sHk6uQJ2AAvh1PEA8zULqiYkIIhADaHUKp3g9DEFGyuX+9wgwEgLBwyjI3AAnv9IEM8HxzJAALopKCmNG6WCY-ipKBxMEcVF4ilSCjUhqXSAvKp4+6QEw4DQNPBsMRk96nBhsaCcQhuXy+IUZATBITwNjiCobKDBFhSaAFSDcflag7qzXkyDwZFkMkHBliFjBY2ZNj8PDQChojFYto41lFN4cqSiaD8+qoIGdUR4HIq1VQe2O-VQN1mY5RqAUH624OQVPRWQWqA-SQsHLIvwsmWzE1m3ONESyfkk9ZRyCGsRxkOqyA5B0SBrcESkGsN5MmjjpqCjR38BVgWBgISYxNrMsbYfwYI9b5ECMHWvQaC26EYL1VH3tkQlISM4MHzDE0mEXxMADMS9OXwDiuVGdvWFzHnxmYnBUlTEXkDCNI9vRfKArjQa9owdJ0Gm-QU4MzNNc1haAa1gfl-mfMBH18WkK3NXkRHqf8xDYIRc0yEkBBPRtTVIg4AEcsFgCQOAQNh9EHZdYAWNgXQzPBYFaPARLAOjJP4f9FVXXMmjwPAIMPKDIFRK8hxjRCDkpBlTX48tmKrdjOIUni+KgzBBOE7UFNHSAFHpRk1Nee4j0YmEOS5HkDkVIQeSXSARTFIgJSlEK-REANrUjcswy3NAsgQuN0XnbF5MrCYazrAh-2bONlNUjSz1gC8jO0xtkPvJhfAAVhsvNxHfYDtVq1AH3wgD5Q-ECDTAlsWQPbzOhgttOl09LmWatCSQwmBsNw1A-nwwjiNMsiKKgKiaMIGSGLmra2I4rjEEVazULsqTYUcqsAAVShJMSFjAAAKEwKAAShG1U1K89J2UwAASLN+BMBoSHIKhaCsMJ2E4HgmCVahwZMah4acPlJExgAWJgACsKGVd5M3PQN7zJ3ssBbKSsnnCKya+e0gzQJ4HiAA Open this visualization in the Vega Editor>

@
let plxScale = PScale [ SType ScLog
                      , SNice (IsNice False)
                      , SDomain (DNumbers [3, 30])
                      ]

    opacityEnc ounsel osel =
      opacity [ MSelectionCondition (SelectionName selName)
                [ MNumber osel ]
                [ MNumber ounsel ]
              ]

    tickEnc = encoding
              . position X [ PName \"plx\"
                           , PmType Quantitative
                           , plxScale
                           , PAxis [ AxTitle \"Parallax (mas)\" ]
                           ]
              . color [ MName \"Cluster\"
                      , MmType Nominal
                      , MLegend []
                      ]
              . opacityEnc 0.05 0.3

    plotWidth = width 600

    tickLayer = asSpec [ plotWidth
                       , tickEnc []
                       , mark Tick [ ] ]

    densTrans = transform
                . density \"plx\" [ DnGroupBy [ \"Cluster\" ]
                                  , DnAs "value" "density"
                                  ]
    densEnc = encoding
              . position X [ PName \"value\"
                           , PmType Quantitative
                           , plxScale
                           , PAxis [ AxNoTitle
                                   , 'AxLabels' False
                                   ]
                           ]
              . position Y [ PName \"density\"
                           , PmType Quantitative
                           , PAxis [ AxTitle \"Density\" ]
                           ]
              . color [ MName \"Cluster\"
                      , MmType Nominal
                      , MLegend [ LOrient LOBottom
                                , 'LTitleAnchor' AMiddle
                                , LTitle \"Select a cluster\"
                                ]
                      , MScale [ SScheme "category10" [] ]
                      ]
              . opacityEnc 0.3 1

    densLayer = asSpec [ plotWidth
                       , densTrans []
                       , densEnc []
                       , sel []
                       , mark Line [ ]
                       ]

    selName = \"legend\"
    sel = selection
          . select selName Single [ BindLegend (BLField \"Cluster\") ]

in toVegaLite
    [ gaiaData
    , spacing 0
    , bounds Flush
    , vConcat [ densLayer, tickLayer ]
    ]
@

I have also changed the color scheme to @\"category10\"@, which isn't
necessarily any better than the default (@\"tableau10\"@), but is at least
different (I was hoping to get a better separation in color space for
the IC2391 and IC2602 clusters, but quickly gave up after
<https://vega.github.io/vega/docs/schemes/index.html#categorical trying out a few options>).

Here is the visualization after selecting the label \"@NGC2451@\"
in the legend:

<<images/vl/parallaxview-selected.png>>

-}

parallaxView :: VegaLite
parallaxView =
  let plxScale = PScale [ SType ScLog
                        , SNice (IsNice False)
                        , SDomain (DNumbers [3, 30])
                        ]

      opacityEnc ounsel osel =
        opacity [ MSelectionCondition (SelectionName selName)
                  [ MNumber osel ]
                  [ MNumber ounsel ]
                ]

      tickEnc = encoding
                . position X [ PName "plx"
                             , PmType Quantitative
                             , plxScale
                             , PAxis [ AxTitle "Parallax (mas)" ]
                             ]
                . color [ MName "Cluster"
                        , MmType Nominal
                        , MLegend []
                        ]
                . opacityEnc 0.05 0.3

      plotWidth = width 600

      tickLayer = asSpec [ plotWidth
                         , tickEnc []
                         , mark Tick [ ] ]

      densTrans = transform
                  . density "plx" [ DnGroupBy [ "Cluster" ]
                                  , DnAs "value" "density"
                                  ]
      densEnc = encoding
                . position X [ PName "value"
                             , PmType Quantitative
                             , plxScale
                             , PAxis [ AxNoTitle
                                     , AxLabels False
                                     ]
                             ]
                . position Y [ PName "density"
                             , PmType Quantitative
                             , PAxis [ AxTitle "Density" ]
                             ]
                . color [ MName "Cluster"
                        , MmType Nominal
                        , MLegend [ LOrient LOBottom
                                  , LTitleAnchor AMiddle
                                  , LTitle "Select a cluster"
                                  ]
                          -- fortunately setting the color scheme
                          -- here applies it to the tick encoding too
                        , MScale [ SScheme "category10" [] ]
                        ]
                . opacityEnc 0.3 1

      densLayer = asSpec [ plotWidth
                         , densTrans []
                         , densEnc []
                         , sel []
                         , mark Line [ ]
                         ]

      selName = "legend"
      sel = selection
            . select selName Single [ BindLegend (BLField "Cluster") ]

      -- TODO: select the lines as well (and ticks?), not sure
      -- how as VL docs suggest both On and Clear need to be set
      -- but I couldn't quickly get it to work

  in toVegaLite
      [ gaiaData
      , spacing 0
      , bounds Flush
      , vConcat [ densLayer, tickLayer ]
      ]

{-
See https://github.com/DougBurke/hvega/issues/88
-}

aitoffTrans :: FieldName -> FieldName -> BuildTransformSpecs
aitoffTrans lng lat =
  calculateAs (lng <> ">180?(" <> lng <> "-360)*PI/-180 : " <> lng <> "*PI/-180") "lambda"
  . calculateAs (lat <> "*PI/180") "phi"
  . calculateAs "acos(cos(datum.phi)*cos(datum.lambda/2))" "alpha"
  . calculateAs "datum.alpha == 0 ? 1 : sin(datum.alpha) / datum.alpha" "sincAlpha"
  . calculateAs "360*cos(datum.phi)*sin(datum.lambda/2)/(PI*datum.sincAlpha)" "x"
  . calculateAs "180*sin(datum.phi)/(PI*datum.sincAlpha)" "y"


graticuleData :: Double -> Double -> [DataColumn] -> Data
graticuleData lngStep latStep =
  let lngVals = [-180, lngStep - 180 .. 180]
      latVals = [-90, latStep - 90 .. 90]

      nlng = length lngVals
      nlat = length latVals

      lng = concat (replicate nlat lngVals)
      lat = concatMap (replicate nlng) latVals

  in dataFromColumns []
     . dataColumn "lng" (Numbers lng)
     . dataColumn "lat" (Numbers lat)


graticuleSpec :: [VLSpec]
graticuleSpec =
  let trans = transform
              . aitoffTrans "datum.lng" "datum.lat"

      enc = encoding
            . position X [ PName "x", PmType Quantitative, PAxis [] ]
            . position Y [ PName "y", PmType Quantitative, PAxis [] ]

      encParallel = enc
                    . detail [ DName "lat", DmType Nominal ]
      encMeridian = enc
                    . detail [ DName "lng", DmType Nominal ]
                    . order [ OName "lat", OmType Quantitative ]

      spec lngStep latStep encs =
        asSpec [ graticuleData lngStep latStep []
               , trans []
               , encs []
               , mark Line [ MStrokeWidth 0.1, MStroke "black" ]
               ]

      specParallel = spec 30 10 encParallel
      specMeridian = spec 10 2 encMeridian

  in [ specParallel, specMeridian ]


aitoffConfig :: [ConfigureSpec] -> PropertySpec
aitoffConfig =
  configure
  . configuration (ViewStyle [ ViewNoStroke ])
  . configuration (FacetStyle [ CompSpacing 0 ])
  . configuration (HeaderStyle [ HLabelAngle 0 ])
  . configuration (LegendStyle [ LeOrient LOBottom, LeNoTitle ])
  . configuration (Axis [ Domain False
                        , Grid False
                        , Labels False
                        , Ticks False
                        , NoTitle
                        ])



-- $aitoff
--
-- Thanks to Jo Wood for
-- <https://github.com/gicentre/litvis/blob/master/examples/gaia.md coming up with these examples>.
-- They are similar to 'skyPlot', but instead of using one of the pre-defined
-- projections, they creates their own: the
-- <https://en.wikipedia.org/wiki/Aitoff_projection Aitoff projection>.
--
--  I follow Jo's example and break out four helper routines:
--
--  @
--  aitoffTrans :: 'FieldName' -> 'FieldName' -> 'BuildTransformSpecs'
--  aitoffTrans ra dec =
--    calculateAs (ra <> \">180?(\" <> ra <> \"-360)*PI/-180 : \" <> ra <> \"*PI/-180\") \"lambda\"
--    . calculateAs (dec <> \"*PI/180\") \"phi\"
--    . calculateAs \"acos(cos(datum.phi)*cos(datum.lambda/2))\" \"alpha\"
--    . calculateAs \"datum.alpha == 0 ? 1 : sin(datum.alpha) / datum.alpha\" \"sincAlpha\"
--    . calculateAs \"360*cos(datum.phi)*sin(datum.lambda/2)/(PI*datum.sincAlpha)\" \"x\"
--    . calculateAs \"180*sin(datum.phi)/(PI*datum.sincAlpha)\" \"y\"
--  @
--
--  This is used to convert position values to diplay coordinates.
--  The first two calculations convert the angles into radians, first ensuring right
--  ascension is scaled between -180 and 180 degrees rather than 0 to 360 degrees
--  and flipped so we are looking \'out\' from the centre the sphere not \'in\' from outside
--  (we've seen this before, but not in such a condensed form).
--  The next two calculate the intermediate alpha value and its cardinal sine.
--  The final pair use lambda, phi and alpha to calculate the projected x and y coordinates.
--
--  @
--  graticuleData :: Double -> Double -> ['DataColumn'] -> 'Data'
--  graticuleData lngStep latStep =
--    let lngVals = [-180, lngStep - 180 .. 180]
--        latVals = [-90, latStep - 90 .. 90]
--
--        nlng = length lngVals
--        nlat = length latVals
--
--        lng = concat (replicate nlat lngVals)
--        lat = concatMap (replicate nlng) latVals
--
--    in dataFromColumns []
--       . dataColumn \"lng\" (Numbers lng)
--       . dataColumn \"lat\" (Numbers lat)
--  @
--
--  This routine just sets up a bunch of points which indicite the grid lines,
--  and is used in the following function.
--
--  @
--  graticuleSpec :: ['VLSpec']
--  graticuleSpec =
--    let trans = transform
--                . aitoffTrans \"datum.lng\" \"datum.lat\"
--
--        enc = encoding
--              . position X [ PName \"x\", PmType Quantitative, PAxis [] ]
--              . position Y [ PName \"y\", PmType Quantitative, PAxis [] ]
--
--        encParallel = enc
--                      . 'detail' [ 'DName' \"lat\", 'DmType' Nominal ]
--        encMeridian = enc
--                      . detail [ DName \"lng\", DmType Nominal ]
--                      . 'order' [ 'OName' \"lat\", 'OmType' Quantitative ]
--
--        spec lngStep latStep encs =
--          asSpec [ graticuleData lngStep latStep []
--                 , trans []
--                 , encs []
--                 , mark Line [ MStrokeWidth 0.1, MStroke \"black\" ]
--                 ]
--
--        specParallel = spec 30 10 encParallel
--        specMeridian = spec 10 2 encMeridian
--
--    in [ specParallel, specMeridian ]
--  @
--
--  We then project the lines of longitude and latitude using our Aitoff transformation
--  and combine them as two layers. Note the use of the 'detail' channel to separate the
--  coordinates that make up each line of constant longitude (meridian) and
--  latitude (parallel) and the 'order' channel to sequence the coordinates
--  of each meridian line in latitude order.
--
--  @
--  aitoffConfig :: ['ConfigureSpec'] -> 'PropertySpec'
--  aitoffConfig =
--    configure
--    . configuration (ViewStyle [ ViewNoStroke ])
--    . configuration ('FacetStyle' [ 'CompSpacing' 0 ])
--    . configuration ('HeaderStyle' [ 'HLabelAngle' 0 ])
--    . configuration ('LegendStyle' [ 'LeOrient' LOBottom, 'LeNoTitle' ])
--    . configuration ('Axis' [ 'Domain' False
--                          , 'Grid' False
--                          , 'Labels' False
--                          , 'Ticks' False
--                          , 'NoTitle'
--                          ])
--  @
--
--  The configuration hides the border line and tweaks a number of settings,
--  some of which we have seen applied directly to the marks themselves.

{-|

With the helper routines, the actual plot is not very different to other
plots (but note that unlike 'skyPlot' we __do not__ use 'projection' since
we are doing it all ourselves).

<<images/vl/skyplotaitoff.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMAmAHAVgBpwoBjAewDsAzJDMUCCGWAQwBNYAnO4SAGxYAjWHwCCFRH1h0ADAF8ijKADd4sAO49IAZxScyAa2mYKAVz58FxJlRYlYaTL20AHO-AmyrSyCwAe8No81oyQbGQAtiwedLZ82rCKSlACwvGxLPGJIUyInPBsGVlJPijwKFJ0ZhYloWUkBkGYcQkh3qFSiLAUhU6QZPndjlCCZCgokZAEUGUVxmDVlsTtYSwoLME+ppx8dDDjLtroAPTHnCzqAHSI5dCmgqYJnOQUKEOX5BHHACJkpogAIW2RmO0GUsEQLGOUV0XFB4MhxzYayhkPgLAAtCxMQA2ACMOKxeJkGPWgikeJYlwoZAxcHYXEuKG0yimIUgVAGUWGDGSkDcnASm2STAA4lFEHszBFhNxaj4AEqiAD6AEkAMIKgDKUtMMq4bJFTBcfD8uv1cpyoW+AFE1ZqdZhINLZYajZBYMqTWanS6DVa5G1lklIOoCihoHQ8AB2GQhgAk2hIcCie1QKEOJ2OCKpNwj90u8DIxyTKahOYxfHKsGzABZLgArbSUN38FgATwNmAA2iFeaVzhRtJzOBE6L2jfQrT4WE0UiwZci3ZPICRMiRzGt5qsUHrLkr7dqAHx4nAyAD8AApkbuIvuVRrtRiAMw4mQASgAVAAFVXHDGnjIYCYDee4Ho+Wo-n+AFnpA05gO0Iq8LOewuNA8BTKQ66bm8eygXetqHpBv7HIBkCIck-buihTqZGhGzynya58BuAi4bR5DaJenHXmse5ofAX48fhlwCIuUJYO+75wZOFFKFRIq+HOOgeCQoh8PRy7usxrFbnhfF3nR0AsGAAC8plgEB55gHiwFgNoHi8belxGSw75gMcYAia5MlGnJjAKXyNFQGajE+DpOHbq+MifsJBmXAJX4ORQTl7mJgjIscknHJev6fiJyVqRpxnSfB-kQMhyntphq7YWx26AZ+yWpXeiU5XlBWqep9HSYGIoALqMZAUScAYWgOQAXvMACc0yQCg7YuNuJDwM8lT+R6FDkGwHiSk48GrmQfADFoNAiL0UDqnwjxvHKMyLduNIRB4mTkWFIWnWofAXZAoVQEmmTzLwFDwPYRQJAo91LXsACOpgsK85RrPA4JvQd1V9Gd317NVc0A5UfQg2DzSZBDc0LdDTpwwjszI6jfXJAzEAUYF82DsOXLjvBgUzsp6VLu9TARfV+nOXwEgnmeV4ieLiAvm+X4kTBQEgfFstQf+ZFle9lWoehNXC3pToy2sGtkeVU6TkwwW+MVDHwULdVG1AdhkFxcXOYlsVuy1okLhlElSb5IoWzzoQ24V3XGVpimG+xUDeXbZkWVZNl2c1ic9R5XnxT52vcw7LvKaFhe1SxkV7NF3tcSJXsZ2r-uZdluWqvl8WR3bpWyTrSk4wbTvx5AjX1576Hvu1redVtUdueRVqDVaw0sKN416IYsAAOrhpGmAyJceJ42vRh7OSdhjeTD17FWFDSBtN4bPtK7KJkpiwHOE5W7wbF0Bi01xiknhMDKwthVNswxf7-34IAsAAE8DyEFvQMBP8-5zVlj-PEWB4GFy-msZBkC0FAL-iAxB39CH4OgRiN8xCcHgJQQAvaMDnxYKtiQ3BZDUHQOYZ-JB7D6F0CYdQnhMC6FQIYVQhBNC8EcIYUQiRQiIHSLoBgrhk5JG8NEUouBgjSHCPIQwwC2i2EwLPIooBBi5E6IxCYvhZitEWKMVYvR6DMGGPAdYjRZDXE-3cQQmB4jsHyJ8RQgR9i3FON3l4oBQSGEhICZY6JdB-EsLUcY8JYBZFxIcQkzAyjImpNMTZOxmSwkFPMcUn+sYCnANCRUtJsCVFGhSRiSpNiYG5JqUAlpHjdF5OaXUpJ3DLFdN8S+BpSF5HDM4b0yZMSxmUQmWkgZqiFkFIycklZrT2nlM6WkvERT1lDN2WeXpb4qllIOQ405rT6knLqVsi54CrndIgbcqpSzGnyKeSM2JDyf5fKmR0vxaSfmDMuYsuZ8lPlpLWaCx5uyXGAsobs-ZsK-lHIhQFeRcCznHMRdi65eyMWgMsfi559zUVANJSMmFyySX9KJaw8BVLgkMqacyhhrKsXAs5XSgp7zxm8taTSj5grunktpQ49lmieWSvRb02sdzcXbJgQqs5KKJXgNVQShFyqMRauecKgVDj9UjP5fMyxJqWXyrSTKzV3LrV8ttT-S1MinVAJdUonVvz3XIrdSquViKmE4r9aMtVIag3avDXUw15qHERueWayFlj43fKjQUtNrSQUap-im6BibMXJuhRmsVXqKWMN9b03N+ilXepgZg4NvT60EvVSKhxTayWluzUA9t1KQ09oofm4lba6lZtbeA-tHLG32sRROxJfai1TtKZ2sdP9Z05JbUa8dAbdXEgbYi3dzaQ0Ho7Ue6Np63nnuuaOzd6CbW9OPb469sbwEPrzZe7pMak0ONffo5dN6zEVv3du2tiq-V3I3c+2QZy-2Qd3lUz9BajH0rySOsD6aUMFKfV+4Y4KMNCrQ5smD2GoObIg8RuDmya1lp-egqjXabLgb9TRsxRHEPDGYz0wFHGkV5O41htjSi71cencqjjg7GWCdWUx+F0nSlkYEzk4DZa11tLoyuzAKmbmAs0+K9T2Az15M0+JlJmn+NDuGCpv1KmzMSY07h7TC6HNLqs4B5VKnzllqrbRv1XnbE+buax8z-CDOAt80CvJYXRkRaE8qsLNmUlheM0IsLCGguYDC7p-9YAMvybS9lpT9GPVmLU1lorbTcu2bAGVgCgXKvVdS3V5DgLqvxaEWVv1ZXWs6LK0l7rjnlVlcy7Bqrrna2DZK8NqVxW-VTfKzNgL834OLeub1oxs2ouAtm8t7pXW1v2eVbNhrKTZtDfI2AE7FXjsFb0-8hh1TlW3fQZdoRj2WN+te5xh7TWvtVN28MD772RO1o+6t-7-XgcybyR9wlUPrtZZmd5vJCO-NI4W6jpb6OVt+uR+WzH3TsdA7LTj0HdAcdHaETj07CmwCU+ezoynE2zvZNU365nWnlVs6p3ltn5OdFs5J5gNnf26DM9Z4T+jzOBdgGZ7zoxzOueVfl3TuXcPhsiJGR5+j6uKEw8Bdru7CuUn67wXk43QCpdm9x3rmLtbLfC8wJbi34Oy2W8N0I13yvhiu+OfBfqAYhrdG2rtYUikfSIKxj9P681L5U3hojdYZRUZzX8IEKo5hLDvTCA4aIuxMZfR+t-C+lMoBPRersC2kAMbh-z33Iv25qbx7ptIZPAQ5yLCZowDvLNF56ARhzUcXNVGlxtvze2LCy66UHjLCWgFpZqwkPLD8GtlZ2Wn4gZfWtu7c17k6AS-dy4i2Ng3FAZtYKh2H8pHyCCJ8Vw4j7D2-Ex7V19qPrKQd85D-HxHLqdsY5MQHtuJnMZMnJZGANZLZJgCPHuK5O5J5EARsB-o0hfnsCXOPnHFFG+M-rXE-lAXeK-s3B1O3D-j1MHIzD3DbLjFhAfs7EPGeE1I5NgYJBPG3M5B3CQR3hAAvIpCNGNH0LoPoEYFvGwBGLIPvIfAIduKfA0DVBTNuNfLfENPfCHnyM-NdG-IPmWgogSoztTloWSrGL0pbgBFLnoRrp7lIgSgqoYXcvbrotBiGkYXiMSNYWcjIA4SFrqkYToXlqYRQgYYikYSYUYeYeoiMlYQEahi4dcrVk0o4e4ehhEaUvEa0jEfInbskd0uEZ4WkiEXYa0kEWkv4dkQUt4ZVr4a6lEWKm4ZUb4k4RkbUakZYq7rYeUUolkbWq0euvUdAgSN0fokUR0d7iGjgDgHurqiMWcgMZoRMQSiYTMWSrkVYqMZYcMcsWSi0fMRro0VkmsRrs4YipsTrtUQcbsRQrLm4qcXdqUU0ocXdlMfRksRer0rcT-IsS8T6s8ZcTmqsVUtsRca4Z8QkeMV8YpoCSkT8ZmhCZkVCb4m8SCWAHMfCfcXpo8a0tcYEvCecd4vCcSDCT0fscCc5mCWKhsTie0dMTiXCaUoiaUsiVlqiWKuifEjiGMR0TgCyQSnScNlYhyWSnMbyWYcMQKTruSQ8eyWchscKQbn8d4lKeggSWyXKQBkKRjgcUqaks8eqX0pqU8WqVUm8VqaKSieKVeiqdEWaWShab4laT0TaQwjKVEuqZKQUkafSSad0gaY6jqa0lyWdjySUXaXQFiY6UkYGTkgqdMeqW7syaUs6Zsq6dye6bUZ6ZsvybSWGTZEyVkrWKydMTmZycMfmXyYWWGs8UWRrgmX6TgOWTrhsTWdKSWQShGWKfWbeo2Qau2SMlmW4q2TsmWbqeMb2TAm8UOXqp2VagcaOQ6cYqOc2caUOeOfoouXQNOVYkOXWS6cuZgCOV6ZOQUr6bodWQGf2fhieVUVuTZHOW6UOdGdmbGReXiJWYeTeTuamQ+QeT4UeZRsMVgLmWKb+QWc8QBcWUBaWQccBRWT+RKVBZGqBU2TBZaQhb2nBc8t2d4hBX4UhQOlhXdm8RhXdk+Z+fhd8ShSMquTgMRQBqRQCuMZRQxtRfaThcFgxXQIRWURRQUnhbubRfuUxYLnxekgJbiSxeGUJeRXRXiBsRJWxTcRJVxW+SJTZB+exRJWhVEqBs8XcspTcXcnMYxppWcjJYEjYcMWjgcXcleYmXcsceMR4WyXUmpfktctpcZQOfZfqaZVUkZfEpEeZb8Z5QSgFfjgZZskFb4uReLsaWkt5VkjkWFW+iFd0i5T5ceX5aeWleeYlbUZZVWZDhlbURsbsjFSUqRvFfonpemVlT0Y5c0vCfdh0dGHVXiMlQ4o1WciYW1YetMk1cVRUk1S0Z1Set1WcjlboYNRrjZQ1XVcGTAuNRQjVXNXcSGotWisNdcosStR8YiptVbrqjtRiKuftaNT4TtctTiWdQUodfCQNfCb1Z0vCRtfCR1UiRdWia9R+u9bUZNZoTtXUWtSWp9T0TdaUndbNZSYDeVRDUoi1eAr9QtVqZriidGAjTDRUgjR1QjRtQjaDc0gjQNQjYdQjcdWUcja4ctVqTNbjVUvDVUqjZ0lqRjR5dMoaeTb9qzeaczQCdteqezVUZzeCfzTtrzb4jjaTa0ljdxQ1eqXTbNeqTTelXtVGd9Q8WLWKsTU0qrQ0cLUDdrfoqLVGRLQpdzZVcbd+dMrOQtbOTLc0rOR1bORtbOaLbOQNbOYdbOerRMrOcrUjaOZTdGKOZbbTctaOXbUzdtaOU7Wzebf5dHYFbHcFeHUkfHeFcHZhqna0pHeLend0qHT6dnb4oHQrQ1Ted7fSf7aUh7UMjeW7fecnT0ZnWKg7dSfnT0dbeXWbdtXRfVT9V3c1ctb3R1b3Rtb3aLb3QNb3Ydb3ZXa1b3aXdydGF3X7V3QtV3W3V3YPWHXtV3aPVHZ3THXvXHQfQnVvUnUfSndMnRePZuRfZxf3ZLT3bxTfW9U-R9S-V9XfZstPbDRJZPbXWffXR-Y3YA7URvZsmvaUgtRpdtVpctbpbA2BXtXcqLSZdMmZYgyNfA4FZgx2ag9TdgyMm3d9g1XUhtXUsg7vegxzdA1zZQ8fcQ6fbQ+fdQ5Cbgxnfg9AqQ-fSrYUewwwpA1Jqw5lcw2rbw56qIzkgNUVeI4UtI70YI7UYQxAyGjiE1TVSo5Mco01SYeo11Yijo2Sjjfoxri0UYzrquaYwbl-X8k1XPX6RYybno3VWo3VdbfY+bpo5vR0W4-6icnVSY3VeYzY740CV4+dcEwLY42neE9CdE7CR4-kfE0lYkwXck9ApTd48JZE5-ak7+jk0ov4yDXk10bEz0dozia4ziWowjVUxoycujco5jQ04ZU0wSiYwTS02SlY+bgjbY7oTiBTR012YM5hXU25Zof054+MyzaM6aTM88uYz03M9aUs7aSs4xWs8xXo+qYY+qYsRMwkxs5gK43LcMxUVs6GYc5eac2I5c5Jdczkjs3JvczZNo1Gcc0oychbco1bd8+1b87o7qjiI7f8+sSC1sWCzrl034l7RC3dukwHbC7Up82Mw8UC5M6ixHYi0AiY1OVi20ni3i1C5QkOeY+uXi4Y0OXs0Odo0Oa40OWo0OfCxc3ozeUS2i4RoSzi4U8i6VTy2KjSybYCzeWo73SK7U3owPco0PVK80ycmPTK7BRKxg3K2TSq9cuk8vQq88q4+vVqyMns9vXqxOYC13eY7PWq3Q+MxJUa+s0qyw3azEw63Exa74to3RTq6lSawI067aTa0omyz-X6xI0GzZIY3JSG3Iz6-0RG2o1A4CzA68rMco-pXo0g8m9BYm0Nam8q9m1g5m8hbm6hem85cWwmqW-q+WxQoY75fG-vbW4ffW5a6i7spW7a424+q26xZ29ud2wib2642krG96+276-m-ib27eXCn-SO3rRO3s7sto7sgOx83iqoyGngE1dbRu3870tuwCx0XuwY+u-1ce-Ybu01US4exNae+qze0W+e0HQ+1jk+88osVe1W3e6mi+2RZ+0cd+zRQe2E6u5db+7Mv+wwjje+wwm+09aB6TnB-xeB0GQhwxih39cBxy0h8G1h6G2hzB83Th33YRzVXgNU+uyjeRzu3io07u9jZR60-R1m7qqRzm8xz04xwW2x3g7R4+9RyiyiSx+tRxx+zx7M3x1Q1xw2wezzaJ3zeJ90quYJ0LbJyLcJ9B2p3Oip9Aluyc1p2c5J0IwZ9lRpzkop1GS0Up7UZBwbSZy87Z0R-J7USR187uz8651R8x-beu8C+5wx750xwe+7d56q3ir7cF9cs57x55-x-SXgKOW+5i-51+6F3W4FzQ2l60uF3J9FxEzl8pyl2w0lxw1l66yV9p2V3wxV8h0V0uVV6JTVzcwVySXV7hw18U01yAy1w53l05+u6K31+K8x5K7u9KyN7K3ivK2N4q0N6xwe7PQN7e1N-exN1F3NzF9yXgF3W+4a0t8lzNxJ2t1J5oZtyE8d9a7t9Aop5fQt46-tx6Td6Vxdwwlu3RSR3RZTSd6FQ9+O09413dwVd97O79+1-96U4D9DeDzkiR3Gwewm3inA7uym8x2m4jxm-D2e+j-Baj3m5jzg7j0M9j9q+u0Q8dyQ8T15eT2J8j6l6T+l7T5l4T19Yz5d5T-l9T4V-j8V8z2Iqz74lu4O7z2k4L7V9z-68L397D9O5L-GeLyD9L-y7L0pYr4jfSbWGu-KpuyGmrx5x0dr-u5oXr0exr2j7qob+C8b1j4imb0cVr9Nbb9x1by4-b8+47+iyidbwRc788i0R76ul7xNRb02+70B6b-Caub7+lv79AjjRH+dlHzz4H3z-HyLsnw7qn6h4nz9675h9n815n0D7n8men5G6H4K7r5U1r2R-KhR9XzrwbzR1b3R7X3543xj6b0TZXyF+36qd3xF53yW832W4PxW8PyJ631T7r1qeH4s+P0H6r0raP225P1E7P6p4v1GP3znZv0n+v4h6v0L7vxn-v-okS7WFGdP1L-X9y8f9KofyX8v2A9v9VVry51b252-3Xw8bWF5-Kj5x-y36b1dov9ZuBvGFr-x7668EW4Agfv-yH6wCR+8AsfoAIoaQCaeX-L2tALn7clv+DDVAblzwFs8CBa-RAep0wGPcSB8HMgdAhqo4Ci6oA5lsgOyZUDcmzA-JsAJl6sC5e9Ao2owLFTW1aBjJLXv13lS91+Bw3K3qNwkHjdTek3KQdN115T0hBXfBQRAIN6asRBq3NQetz9K1gtuSg65DHy7o+9TW+gy0hoIZ5yDsuKg-AVoPtYyDr6lg51o4ISrODnupglJuYNfquDBM7grPvYJz7+C8+3gh5r4P0SLFdBBHYIUr08G9cHU2hLXnD1N4I8reSPXXijxSEm80hbfLIZbySHKCDedlAoQ7zyEwCShcAsoQgIqFICch3vBIWgPd7WU4hWAnQS2yaFMMqhYHDIRzw6Eb82hLgnoUczqHP0uhXggYUfzGHocJh4fXZD7ykZ9CwhQwhXvMIh7LCoe4adXoGk16VotG6whBh0WfA9VdhAA-YU1VXIHCQBDxc4Tj11RXC8eNwpxkcKJ7bDtBuhW4ZUJOEU9nhE-TQm8IoRnCgmmw07pcJD4fCbBwIlfvcIcGQis6XwrfrCJ36Ajhh0I0YaCMM6ojjO8Inov8Mv7giOBiIoBpiMhqEiVh+I2IYGir7kjBu+w+ppWgb43Cm+lI44T8Paa0iLhKJV8PkMuEDNWRffHkU8MZHlDqRbvekhyIMHhotSLRUUfM3FF08uRQI9kQvwFHtChRdglUbdzVFOD6RXDBUY-SVHUCZRdAuUV9z5EYi9RLAs0WwJNEAMrRCwm0UohMJSiFGBowQZWlf43D3+7oz-uyJ-6Bo-+nopkZcKAGui2RIosAb6NUGBjih+w0ctbWfAh1w08XBMZ8PDHfDIxB3H4RgJTHNDXhJdYMWCO9EQjoxUIosTCKzHkD-RSTPMd0hqpxjh2JYtERmIrpJiAh9YgHlWKs7NiCRZYsHu2NbqdiyRNw4QYGlEHhpxBg4vYT8JHqjjMhk47IbONyH7D5ulaRetON5HDjNBlw3VsuOFHclnwO3dcamPZEmDtxR3TcfKJFHncDxCnVcYQPnHqi7xmoxcdqIvG6jxxSIp8YaKPEMCPxIjE8QDT-FtirxHYgCT0UWJ7jIhb4vgTeIHH7CYePwxIbBK9EijUh8E6QYhIDHsi0G6EzpsWmvaVpChlwhyrhJGaBoSehEncX6Uiw41IskoupGcMaGkTzxu41oYxPzHITCxqE7odhMfGcS4RrEysfxI8GCSD+wkkXqJLF74SiS4k7DtJNa6yTdcNwhdsROjaSSO6uqXAH+RRIaTAKM6HYY2iaqLFtJRvXSTOIeJGTzeJkhcZoXMk299JEYrSQ8LsmlCOiNku7CYVcmvE+0dVHGh5OxZeT6h9JXyfiycnZifCQUolkFNXJBSWiQUnyQ9X8kHNLJAk9SfCRqpBTKa4U3pmFJxIRScSUUnEjFLJIJSuxKUiCS5PKbFSYJ1kikepJr4zoaR9UicWZIZG1TTJWklkY1KsnNTOR7U+yYFK1JpStS1tLAAzT7RalDJ0zTqbUMbRT8xpso3qRYNamhUZpIHFaaqOqnFiNppYpaXxJ2kIi9p+otaSiK2kNjupTAqaVrSOmASDpBfG6bfwuk9iHpKkp6UojSluiXJHoj6UhO5JYAfR6kv0V9IwmBSgxM6ILo2jDH-S+pP0qAaDI3FaT4x4MiiboV+nJjIZh44GQFOhnzSMZi0wGVYOskks+0ZLRGVxIJm30SZu0vGftKpmHTYZn4nGadPhlNiKZl0umUELRliofJL5ImWVLJmP8WZz-RtEOPUkjihZ30v0lgEkEiy0J1k2QdLPkGyyQxP0pcTOhXFiy1x8s-kZrNNR9o9B6s55D5KMG6zMZEs81qrKYmmzcBisticrI4lmS6Khs8mebMSnazyu+soSa7P04uTrWxs86Z7Il7Wz2Z3s6-v7K4H2zeZ4c-mc7JdEzo4JZkhCdZOSHqSUJ8cmWanKBk-SsJicpWRLOsrzooZucqMdnOcnFzBRpc94eXOqGVy7sMUuifnNPFaSbc1cwTIuhtmFz1p6c+8Z3J4ndz+hLknhq3OrH1zjpvcsScnOZmxypJ48zZLXJDn9ynmg8zrovL7HLzq0R6DYTui2H7o9J28pqSiVPBpz95J7e9KcPXk5zdCB864R0Uvl3Dr5jk3eSXIeI3ydZJ8pGT4WflVyn5fjM+emK-nYzuSH8ydA-Pxl-y25F866j-K7lHynZm858QApeqvz3xmhQBdV2AXv1EFv4tBViMgXXS75c85BeDQwVLysFz02BWpOvk1SKFVI5BQ1J3R0iqFKxe9HjSPQdS6F589+exyYUFyL5A0lhXDPpIEgXhHCt+WUUEViiuF6MgBbNIkVmD90MnORVbKfnqlVyYi28Uos2nqLtpDCymTQtfHaKPZ+ikSWwuNEKK-Zhi80cYqDm6K8RliovjIuIW2KV5pimOTunenILPp7i8WRfL+nXyAZnijOX6UfJzin5YM-dBDN8XcL35MM1xfwoAUIywlIippEEvEUJLJFgS3FvekzExLcZ-ikBfvMJmZK7Z+SjRcUq0W5LyxESvOoUqQUhK6x5S0dqkswXZL-xjS3BfUtumVKSpnShxd0qcXNKqpT84WdfNFn7oxxwyveQIqnH3o5Z4yhWYMvYWiKVZO6NWaMqLnzLH5+8rcasqE7TLUZsy6adsulG7KG5kyi2RfMvHLLVphyjtscqgWnKyl6ynRY8upnILXuR6d7u8u-GvKJ5lylsd8pnmfKbF+yuxdcsem-KoJtygZfvLjnQrqFT8pOdfJTmwrGFQGNqQIqznwqFlSSvOfegInIrluO6OpNbW4wmFuMixbjDjT4zvof2uK-+YEqbmYrlpqKsBe-MiroqSl7Kh5fioqXIKB5zKoebSvpkAKW21KvwYiqnniqAVgqzmaKttH8qelvKsvoqvIXTFNJbpOFcaS8WfkJliZQ+eqoCWHlglmqrqcasQrPFIl7FNZaaoIbDEhFlqnZQcT2Wqq0lhq3+dar-aOqclYpRRe6qX7Oq1Fvqrtuaq5X6qnlgawYcGoFWeqR54a8YWyR+Xxq-l3qqVdGuAmprQJtqngYmohXpq15JyNVdyTqmAstVZReheMz1WFq0VlauZSiQ775qr55axbnoytX0khpyjO1U0nGntqUlxal1T4WkXNqTl1a0KaWp9WtqrlvagNeOtJmosYFXjOBXYz0WNqo1k63xOky+WzqzFy61maup1r1qZV+6kFbuqJGDqc1x616VrwLU6CNVqvEtU0h8UG8K116g1T4RBmm8sV8icJY+qbXvqNZuvWJc+rLlf9Eln6p1cBr7VlEMlVvOlboQXLyox12Aidf+o7nu8OViGkNehrDW3qql0GmpahqFWAb0FuGppchpaW-qrF4Gg9cRqPWkaFVlGp0fBpVVmSr1yMm9T9LvXyIpZLkp9axpfVlEJ6faD9ZYiWXcaf1omgleJptWNoO1nGkDcJrA1aSUB1kk2bxtkXqSzlYUhDRLKQ3KaUNgUtDdpow2GasN7GnDeprw36aCNqmojeZpI26ayNkmvdTOnwXMbeWzmrNfZoY3uaXFIGFjXlgTn0YEVZaJFVlnSHKoUGgKDFXpgsoEYzVkWi1SkiIl4YtZvml3uFrk1IYFNoWpTYFpU3+aYN+Wr1dFq03U40k05NlcNmiqxae5xWl2aluSn1aDFwWupblpMXpat1rWhzc1pTXtaqNvWmjd1qWHxao5-WwWdpj82VYRlbmDjToi43KYeNeWGZfNprVZZFBhmHqatoS1CJ1B42jZZtrS21o9Zu2g2VZhy16Zjxx2gPpdoA7La8l+268etr03DYHZp2ozdTjopS53Wr2ldYdpa3naN1-2jrYDq630YJKthaSt9oG2g6I5wOrzdNqY16Y3FhWNjWdltodYdVqOhbXVirWY6Vtw2UJQNg2346tt3WFtcTr23k6gNiOjLcMES7NYztpWPLdjqHW46R1KSXMfTpZXM6p1lO4gYTre15ZqW6OszWNk9ai6rNgugHYzqB3S6Qd1OnreLr62K6od8uobfzrPXK6xtD2CbSkiLXA4ZtRiMtfRhan66+Nuuo1VljrWApOF1uknYbrJ1nY21UOGTToi7XO6e1pug5drrdWW6CtlWeRd7ru3DZlF72Ipb7pnU3Y51ZadUlLmlqh6LNwev7eHsZnJ7TRgendZ7raXG6XNketzenrBWZ6+lhevNYCg3m1ot5HOA3cMAMms4sdKSY+aXot3DYL2tehtRLjt3V6Hd1OJ3nkjqpS46quRbya3q93l6md9ev3ePqK1ZYQRZaMPsPpuWN6I90+qPe3rq2z6l1q+n7evol2K4pdzehNdvrl3L6Fdh+tNZXrz2j6PNm+uHZfp83UYYVWWbjN11rRP6pcT+3Ik-tBpP7bCT+6ck-qhZP6splWbjDNW4yOVuMMtElbJgdWiYyG0BkfffrH1CJADvGBlXpg4wAGyt8BhfbAYM3U4OMH+-bC-r5W4GE9Z2DjKAZFWoGD99GDA3-pmHYHrRwmBecwav3oGl2jBkvaJiGXUYptL+sZbwYx34GplXGJbbQYE28Y1tohoneQZWXcGu9eWDBABuEMu6jEShmA-way3DZ1DCB8Q0gZ0Q6Grt8hqfdoYuWaGudKSQw9Ah-0vbJDK+9Ax9qYxfa7DZBlQzvssM+yXDdmvQ0mocM4i-DQKwQxfqCNq7zDGukI1Csf1I70DHi2g2jt4wPq4jde5A87SYxvqX9BOjIzIfwNhcEjCh4AzGLSOqH2MiYvIx7uoyjkf9UG0TFkqyMmHyDHOmozpqSNPaGjeBxQ5SyKNr6WjDWio2Lr6PuGUje+tozLu0PV0ujFGmIznqiMsGmjbBmYyNrqN37aDlC6jHrrWNV77RQhxQybo2Nm7kDrCl-VbtEw26TjHerY3+r2MparjL8rjG7ruNaHyDEopjAOrOMs78DAeo4yVp2PNH0DTpF4+0eAO7MAT3Rv4xvrBOuGfjgxgw0rRBPeGITR+7Q+ZzhNn6vjwRlYzDsf1vMUTWul-WXuowV68TmxqHtseAOHDeMDe0TKfIpNCa1DNjJjHbxpOXHaDPerjH3oZM07Xpjx-A9-KZNHK2TE+5A-CQ5NMqqTvxx-RAr5N86iTAuskwup5PgmJT-Rlk0nu0PnURT8JpU74a1OTGdTqJgk7MZlOhGDTixk08sfO0P7ntAWi08Sf0ykmTMYWw7RFrcxRbVtMWwzDiqcxiblMSWr09cdB1kSbTGhn09ycW01snT+hoxDphcwabJtLEl0+KatNh6kzS+lM-dw9PymwziptM01oDOqmzs7mIAw6ZoNBnETBZhgxmcCN5n0TpZujbWaL0hmEdq2ng6Dr4PKYBDrZ+09tpENuYxD52iQ9pikO9nsji2uQ4dp23DmKdBZrZZOdfZWZ9xs5vbuOcjMWYzZi5m7Z2dFPLmLD3Z5M9OcBMmY6KuRSWZmcm3OHBzyp-s-mfe2eGLzox-c9qee1g75z0xp84afbOYm3zppzc+adW3RG-zKO97fEe0yJHztfi0HakcMzpHlMmRmCyOcm25GQL+RkzIUagvFGVypRpC+UYgsM7nt1Rw7bUbgv1GgL3xhC4mYLPEysLdyvC-YYAugm6LvRnC5CbIvQmozJdKzKyw4sSqiLupmi1WbAvvmmLxpoSzfp4uRHntqx0Hesaku2mRpXZ2bbsZkv7GFLTegs8ccO2nGNL5xjTLwsMxO7tMo0vS5yZ0uhnJtzxoyz7okuCmVLxFxbYqLcwh6LLPOtSwee23AmnLPKpS4xfO26cDL15uy8Mfe1RkoWclx8y5ZP1eX9TkVjNR5YL3KZsTsVrg4dvxOg7CTymHeW5hr2GZyT2mSk8ldUvvaW92V+CyZkZO5XkL221k5lfQsaYB9VmIfcVYg2lWVzK5AEdVdsuTaZ9qV8i4Vb3O9XUzBZ+KY1ZM2DXszo15i81dYsWZ1Tw1tPflbCv9XeL41pXelcEvnacSn2iqbNdxOeZLTZ2SLM-t2u2mDruRA66DQOu2EDr05A61CwOtFnkseKrLJFkcqRYZakWKXJFlOtwHosuF-a3XJ+vvG8sYWfzKRYSxYGAbzl6nGFnOtxUIbnlvTGFjesC84bRi2tBlnus6IMst1vKrFgrOhY5h+Nta09aUko3SFaN1XPtZbMI22z9GcCUmwixzbabPZtG32aesDnYsQ5lmyVeSxjnPME5rm1OahszmBbc5hm6ZYSxGyxbllym9ZaMR022dPN0G4rZ3OY3ruUt6izLdlMS3Tz2tkXXzcvNs3-LlWeWzZpFtzX9bC1oG8+fVtRXqbRN4bCbbitM2lVztpsw7f-Pu3ALQN4C7FlAtPXwLCNyC6Fmgu03YLod7m5jcQu+2Krkd5Q97ZqvZZML0d7C4Hd+tQ38LnmQi+HY6sJZGjaNgpcHb6vx2Br6d2i57fovl3vL-tg25XbXU+Z2LEWTi43e4vZ2lrpd-i9XZrOd3hLqd7873d-MO3JLCN6S8PeOtG7R7KK2LMwoiyHHPM6luexHblvcjQsulle3HeNuGW17wZ2m5NKntp2gbrxtGzPz3s53ks9lo+z1YPtF2N7rlzG+5a3sjWobcemezXf2vqkZqjohpSfc1OD2W7E9tu1fY7u-2u7wDnu09YSsP3xL+1lKwjbSu02MraNrK6FhyuxY8rnmak8g9pPDBfhBuDG3LbKuoOY7+D9ewlnZMRY6r5D8W8ll5OYPpbUNtq4g9jOkOlbmNufZQ9aP0Pb7xDrW9Q51u8O9b8D1+5w6mv8IZrtDn+9A7-tPWCpPmIqew66XoPPzkjvu9I5XYDY9r1OarIdcKxBadH8loxFo9BpaPbCWj6clo6hZaO8HtOx6-jt9PqOSHbWQM6VjJ55JqsRj8M2WmqxmOGJ9j0+31hYcGPwbzWCrajqq2uPYbwTvh-44EeI7kbkTkR5gEGxWOu2JZ5x1I9sf+G0nQD0J-bZyfzGMnKjgpwPdR1U3SsNNxHR2Yqf6PadzNzx6zfx3s2xsnNup4vZqfaWqs-Nlp4LcF3C2unot5rAuaaf726sF2gbGuaGd+ODHZhvp8qJmfWGOsthgZ2XZKdROpnY1zR28tccfKtngVnp6k4afpOVnEVqp9k42e5Ozn+To5+EcKyqUOsHt1HbEcR0+2xsft-HQHdKxB2BsIdp51g9YpfrCsUdl50Q9p2oXms8Sr58Zaqx06IXTVtrBnYBey2QXTDuFwE6Rcq2DHlFmFxrc0edHXHQuvF+s8F30s7nRt9nQ3bBf7OHnhznF5k7eevmqXIDhl2A7peFOmXUDzR0PdKwj2uXtps-tU9YqKXEd095rLPcKzz2xXrTgV+075fMmhXDj7rJvYGz3GlXVDhV8M-Z2H3PHx9sbJ8a1eoupX6L2nf8dcfbMOs99lVxXdR3P2RXQjwXe-bNe7O6swVh15bade0urX9Ljl+c7tdKOvXrLv18U80cwPSscDxHQg88dIOBsKDsbGg8KwYOo3vzxJ-SdccEOY3wL1ilVbTcHaI3kL2PmORTfqu2sATDrAw5zeTPadXVsN5fbqySnmst1Et8s6DerOK3hLmt7a7bcJOqsYjhN-eabeuv2dMjgt6c8F2EK63vrkdy7ardu2zs62FXpNmtNZZZ3UuWd7kVneg1Z3thWd9OVndQtZ3yTntu0-WyOV1sMtdbMu5cebZvrl7wtzonWzbufHtaLbHkhOzbZZn9GWbJu7CfXueHt7og2Wlmynu4nB2Z3O+6oPfuJH1OE7Pe-deQeCbwH715VhOznvJ3i7im5B9KeTZyni7yp9h-5fblan77+pzO8af-vmnhHyV-h8PedPyP3TxD705o-9ODsgz0jze7WyjPH34zlj+W6jDTOGPb7vTJ90ht5ZBP0prj+mc2yOHn355pj+2+OzbOJPjruT5S-Q-UvhP1thT8O7o8IelPlzlT-67U9qPH39zyD488XfPP-3rzmd+88myfOjPBV4T2HYE--OnPh76JXZ9o-HZwX7nxj957CIzZKj-nlq9uSzsufuPwX-V+F8NdRhMXvn6PoF5-drZ8Xm2WlvF633vvGWqX02xZ+U8OfVPiHm8pu5vLrvuZz7m8su5vKnvhWM2Tl5Nm5e1fbTlnHXKu8FeLvhXB2UVwJ-FedeKP52Ze+1-Tfbl9L-X7N+++VePvd7431j8MEa93Zt32r-97q9G8Rfev1b47Ma82ymvn35ryb5a8g-WvhvaX7r525m8+CtvOXxD+f2q8wfhPUZddzZzO+6ebvKH+r9O8g-BvJsobxd+G-feRvH30b-97G4E-xu-vib87Mm82ypuAfA3sH-K7WxkOIfkLqDs6hmw0ODsxb596W5+-Ivb3lbr76t6ERI-I+GPrh9N6GsI-dvwnhBeT8O94-jv3bkH728p95fjsg76n7bdp8KOsfj3xD1tbZ87XjdGjvLB9gAgy1hf9-aPSFuD2OmJfOO6nGL+nJi+oWYv-d323afC-HKwv0X046l+QvhfoNYX7YWF8K+H3MvsL6r63Om+Ht1ukJ3L6-c-YEvYOZt3Olbe66gPEOUly9jA-2+IPQvnG+7+OeW64P-vznzdhJvW-nvjutD0L4w+O6sPwenD-H7w8IkCPN2Ij3L5I-G6yPqfnrziF5uZ-ofuf2H-9no-Z-t7pfk7VDklvW72P0ezj-n7N+F+Lf9fq3w9jVvV+SfiSI8+9kk-t+YnluzZ735jX9-FPL2CSkr8DaV-rv-uiHZP8Zfp-x30-iP-P9e9C-jPq-r2-7vM-G7LPcv6z47ts-R7vnluxz8f5z+AvD-Bf0Fw9i88X+y-p-lO-f90M3Z4Xz-xF4kjg3W687t-lv8Dhi-f-RP2-o27r+FPpv4u+L2MS5Q4GXpAEj+ruk3af+zPuAFT+uukV7vYJXvAHc+yAUv7ABgbkL41ejunV74BtpvswLE72C17B6bXsDgdelul17UBOfn16UBBfkN4MBI3jdhje0ehN7sBU3okiauxuvN68B2Pobrn2nAVF6YAxATgbMB2LrgFAB-ujHqkBYAa7q+WD2Pa5Q4sJioHneuupd5qBAfuQGeuUgXP56BzLoQH6eMgYZ7R673o7qfewet943Yv3mYHJGruoD6W6wPnYEmqTgTn6Q+xuvfIPYmbi4FU6bgXf5WBqrobqo+wOOj7W6mPjYECB-2Lj6BBIgQiS1u3gR36iBZPokEgBuulT6pBNPrEFD+2QVl6eB6gS9h5S72Kz6ZB7PrkExW4QQv7pBWAf7oV8eOBriOUOOCL4E4tpk0EKStaG0Gg0bQbYRtB05G0FQsbQSr5NBM1E0GNBRKi0EsB8OBe7KoTQV0EeO9GE0F9BJvgsFoGUwct444SwdfYpIOOHMHSB2wX+4rB8gUYg44YwR7704Xvh0G7IAwX75E4eNjMFB+twdp4U4YfvcE1B+waYELBMftTgL0G-tsEJ+Z2D8H68nwfYHHBafnliAhAXEThZ+8OCJpQh7ThCEE8gKAiEkSMwSX4whkLsiGe8SOFX6ohQXjTh1+emJiFeASIbx6EhdFH0Ft+uISXbghXftiFO+RzNJ4dBA-lSE5BAIbeYsh5tsCH9uFOOp4chTBnyFyqAofaLY4ElDLREhawkjhr+lWO3T6E2OFv6EhO-uCF7+3wQf4LBR-sNgyhexHKE9emoWcTahsrvDhX+HQTf5qhGIdC7GhXAUcwv+hoW-5WhUQaThf+poXEG6hnQjMFDkXQbi5IhSXm6FHBwwC6Ep8koWcHHB5Lj6GM+0oeMaBh2gQCEoBkYSH42hGARTgVe+oTgHSheAd8EEB6Ya0Hj28OGQEAhFAUThUBGoTQFFhOofQEFh0PprQohHQYq7VhGIRwELB5lkiE8BhIXwEth9oUcxCBDYfj70463jMGbeTYXsEU4sgUjj7etYRNZDhQYX6GqBA4WGHbBmgTOEAOqYboFLh+gSuGGBmYcYFzhHwYSHmB3wZYEAh1gfDi2BCwf94nhsvuCHOBZ4a4Eah4PjMEeBO4RWE+BV4X4E3hGIQ1ZIhIQUThhBd4baE04wpkjgxBB4d2HHBCQR0H1uAEYOH04sHBBF9+r4eOFQRk4aTj0+X4QUEIR3IWhGLh2wXI4fhTwRhF1mR4W8EU4dQaXqC+iuAu7N6ujnpic4g+tL4S4zpuXqumFEaD6c4Kvjzhi40Pmzgy0-OOxEBBTOFe4c48wVRH-WJEb+Gi4vevGYMRwEdXo2+3OHb6SRDvinz0h0uCQbyRWQXxGIR-FDAEq4qEdpHoRukZhFCI8uKDRK4PEeuGyRhEXzhR+iuF8Hc4cfkzj-B3eozZURKftPpgh1kfZ7uR14fZE9eHFN6YS41Hs5FF+IuGiHN6R2hzjMe-kZaHS4NfpFGA2nkQrZ84pIa5FSRwUVsGGRizuFGQRcuD36ZRsEd5HwR2URpHRRWkdXpj+rOBP6l6vIeXoz+lUbhGFRCYYlEWR9USmH16Uoa1G-BhkfKHT6ioYrjKh3OKqFUR6oUzgn+zes57dR7Tl+QSas+kaHTRCdpNEVyEuOaGzRsLnzjWho0aJEf+HOI6GDRKUYLh-+i0UkHS4noVtFKR80W7Kl6EARdFFRZ0WPLl6cASdF6R1egV6s4MYVdGrh7UQ1Fy4SYb3pVevemmHc4GYQDG2mSZE16s4uYd3r5hEuIWFM4xYTDE+RZYVDEcRTAbPo1hKMZC4gxWIaXqNhHOM2HT6rYXjHth0uJ2FURjlljFpRfOP2E4xWUdXrDhZMXlEQxsnoZHKBdMSnrN6zrn9EIBFMUgFMxy4fXr3eLMfhFsxTUTTHbh0+ruHc4+4d3qHhzeseFURp4XLHnhiuJeEKxXkVLE+R94WLEcRT4SrEvhTOBQ6l674RzifhEuN+Hl6EQZrFm+7xPRQGxu0dLigRs+uBE2xCkYLjQRTsVXYyxjMXziYkrOMhEmxOkdXpFBveiUFmxvMYZGjuRsVUFhxwsSLjERyqI4SOUjhDLSOETuEn6OEoNI4S2EjhNOSOEULHER+oRhDNReE+ceMGm42vmdjBExcUEFe4gkVlhGE2ccsF6YluJXFN+jcUE5xxMkZViW46cRE7txSkZbhJxbvi7ggercSVHIc-sWPGPRE8QZE6IruN3F1R1cZHEzxHBqXEfMvuP7iLwgeGQA7Q0CGHC-Qn0OdAoEMhDHhQADeLTCJ4zeC7Ct4aeBYAV4AwBwDcAeeAfFOgheFDD14ceGfEowChAdAcA6wPAC541eE-F8IdeFKCRAZeGjArgVeLwAR4teK-Gww78UjDnxNUCnht46eBwQIQQYJwQgAcgEAA Open this visualization in the Vega Editor (although the link is long, and may not work with Internet Explorer)>

@
let trans = transform
            . aitoffTrans \"datum.RA_ICRS\" \"datum.DE_ICRS\"

    enc = encoding
          . position X [ PName \"x\", PmType Quantitative, PScale [ SNice (IsNice False) ] ]
          . position Y [ PName \"y\", PmType Quantitative, PScale [ SNice (IsNice False) ] ]
          . color [ MName \"Cluster\", MmType Nominal ]

    spec = asSpec [ trans [], enc [], mark Circle [ MSize 9 ] ]

in toVegaLite [ aitoffConfig []
              , width 570
              , height 285
              , gaiaData
              , layer (spec : graticuleSpec)
              ]
@

Since we <https://sciencefictional.net/2018/04/23/we-will-control-the-horizontal-we-will-control-the-vertical/ control the hotizontal and the vertical>,
it is possible to \"rotate\" the data to move a different location to the
center of the plot (this version has Right Ascension of 0 at the middle).
I leave that addition for your entertainment!

-}

skyPlotAitoff :: VegaLite
skyPlotAitoff =
  let trans = transform
              . aitoffTrans "datum.RA_ICRS" "datum.DE_ICRS"

      enc = encoding
            . position X [ PName "x", PmType Quantitative, PScale [ SNice (IsNice False) ] ]
            . position Y [ PName "y", PmType Quantitative, PScale [ SNice (IsNice False) ] ]
            . color [ MName "Cluster", MmType Nominal ]

      spec = asSpec [ trans [], enc [], mark Circle [ MSize 9 ] ]

  in toVegaLite [ aitoffConfig []
                , width 570
                , height 285
                , gaiaData
                , layer (spec : graticuleSpec)
                ]


{-|

If we want, we can treat each cluster as a point, and calculate an \"average\" location.
The following visualization presents the average location of each cluster, where we
calculate the <https://en.wikipedia.org/wiki/Mean_of_circular_quantities circular mean>
of the Right Ascension values (to account for possible wrapping around 0/360 degrees).
To see the effect of this correction, we
overlay the simple average as unfilled circles: for all clusters except Blanco1, which
spans 0 degree meridian, the two match.

<<images/vl/clustercenters.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMAmAHAVgBpwoBjAewDsAzJDMUCCGWAQwBNYAnO4SAGxYAjWHwCCFRH1h0ADAF8ijKADd4sAO49IAZxScyAa2mYKAVz58FxJlRYlYaTL20AHO-AmyrSyCwAe8No81oyQbGQAtiwedLZ82rCKSlACwvGxLPGJIUyInPBsGVlJPijwKFJ0ZhYloWUkBkGYcQkh3qFSiLAUhU6QZPndjlCCZCgokZAEUGUVxmDVlsTtYSwoLME+ppx8dDDjLtroAPTHnCzqAHSI5dCmgqYJnOQUKEOX5BHHACJkpogAIW2RmO0GUsEQLGOUV0XFB4MhxzYayhkPgLAAtCxMQA2ACMOKxeJkGPWgikeJYlwoZAxcHYXEuKG0yimIUgVAGUWGDGSkDcnASm2STAA4lFEHszBFhNxaj4AEqiAD6AEkAMIKgDKUtMMq4bJFTBcfD8uv1cpyoW+AFE1ZqdZhINLZYajZBYMqTWanS6DVa5G1lklIOoCihoHQ8AB2GQhgAk2hIcCie1QKEOJ2OCKpNwj90u8DIxyTKahOYxfHKsGzABZLgArbSUN38FgATwNmAA2iFeaVzhRtJzOBE6L2jfRfE1SGRtEqpqRMiRzGt5pByNoABTIlB6y5K+3asAAKjAAAVVWBjmA8TgZABKSDtEW8FgznQeBfTDfL1dvPZtA8Hc1n3Q8NWPM9L2vW97yfF9kn7d08j+FxBHbcdIHVPhHjebgAF15T5FhEDyCE13HK1X36Fw9giVgKEXac9k3JUZCYmgRF6Wd51EZ8iOoshaKdeiWEYn930Ar9RHYn9OL4bjPwoBcEMnXghLohimMkp11H0CRlXOWSoHkxTwIdfiqMQmjNLE7SPw4EhjI5NQFL2W0jx1QNJwgfCrNUxgkJFZinSMt1J1-PgVwEACnRRCgsBAvcIkuIDlJkghd33ViZIfU9YJkS4ZBgy9IH8gT6CspgdJSFgZWRcL3RIP8YvXLKUqMgA+O8ZAAfiS-cjIxABmHFHxPS9jgxHqwEwdrLiMibVSmnqysnAKIDfD8XGgeAmOaqL-za0CUscmQluOVaNsqnyoBq3w+B2jYKqYA7ooouLNy3L75p2+AHxPH6TsuAR6qhLAHyfcqrKC4iPzSkhREe6BnqqpdDtavZ5syJ6wAAXjxsBit629ZrANKBpSnGUbym9seRjZobUtGQqgM0XvR97YqgUbzqB5LLj+gGKfm0HBGRY4IeOLdLxPeaEaRp6ofWiqtr2DCfzeo69h6k8ReBoXpdl+WPERhn4KtQirUgKJOAMLQgIAL3mABOOMZnbFx1xIeBnkqAKPQocg2A8SUnCsjcyD4AYtFMvZsNwg0fxQT31xpCIPEyJjOm6bjFmuyAfSnOOnXZqAk0yeZeAoeB7CKBIFA9r29gAR1MMTZjWeBwUsiKML6EuoA18uDqr51a-mFpYEbyAU+bp02478ou577zkjXiAENh2fB2HLlKLU1nI94-aWo+njKYPFUIK1fLoJvHr4NVo+0u-TntadfWBfMyCLyvB+4K9yNLDHwKFTBoX7mAbsWEcKwgIhzXwpFODkW5hOW6U4NIiS0hJD8OVnKD2PipDmm0bJYLsjgqS6V8GuUUq-Pi10lDqWElAUS4k7ofj0pQRAhkWDUK4nsH+XliEYOYTbbB7CsawCchxGh7k7Q32fGjPyKsYYs3umFBBWtMZxXWAlS+dC4zzTwXlM8PUiolVVGtI010QGhHumLBqmiz7c1WALThBkuo9X6vNdx3Chq8wBpNaa94yY+P0n43hF1gnsSZsAo+f1T4Y3Pq4-cZ0LpXVVmoj81NUa3Uilzdcdg5zfWKb9XaAN+b7gceDSGViRQ2KyZQs2T1GrBS0ck+muMCZEzACTPEZMv77hybTMAnSUZ1PXpkvJ912Ys3aS43mgNSkG3KXrYCos6rixqUbVUctgYK3NhMpQNij7D3yR-KAutBkpUNjLXZJsg6KxpookUVtgq23tn0J28wsAyHdrPVOLFfYkEqHJeAFhYDcSngHbowdQ7CjaVHGOA9ZFOgTnApic806REzrsH8OcehVHMJYCqhdY6orZkxCulQ+g1zrs0TIDdk6AoXu3V4y8yir1JZA3gBCznUrHnSyejLp7MvnlARe7L1icukBvRgcqt7Wz0GJPeo4D5xPujlRJBSWLLO-tfB0d9-4FSfjDF+0ltUXKUpfQRRqYKPyAa+FmYCIGYXRXhSAbyfKILIpCVBaMSGYJYeIo+eCZH8KdFqhhgVSHBvIRIz+0k+FuUTelR1PkmG2TYUfXxPDk1mQNdqdNh8g1iPjUfM64aU1QA8gouVrzYlOumR+DRcznGFN0YlB5bFMrA2MflMxxUbylUbYhRpTpqmtL5PM46AtPH3m8cDfxY1AnLWiaEpdkSgmrVHYw+Ju1LXaKgPNNJk0MmqObXsHJU6fAzqvV9SpNzVmPpBpsiWENlbWKmd6+6ByWlOKSS4sZLB8aE2JqTTA1zLjDJgsBo58rv0RRmTe0Id6nSLJfULNZFBL7VMlg+HZeyBZ-uebumN90zlocufebDl9bnG32abJ5LALavKIjbFgdstBsCLjgCSVZECMQnbAKgaAxXrjeH4NAMKg5kBDp4cOEVyDR24CiiNUB3VJybtijOFAs74ohLnIlFgC6SZ5C5dTMDE5ym01KHFendgFyLryilZKfwCq0EK+uorbOsqXtK7usruXkss-y0enmJ7eZnli1ubLO4ypeevNotRt7KqHCOMcPYL0-o-JOgDOqnSiwkN1BduGJAjRXVEmac1gZ8AkFV+88HN7P3ugkzW7asa1bWOkxrDTL1xQZih16HXPrFMw8+vVVS301M-fUxD7pf1McG-lq1wHQM9L6QM9ZwMYN0x24Nsjm1x2UpW0eyAGHJtPv+rRjZYN8OEYec00jKjD4UcPckq522Bb0fuYxx5hz61gC9T4D5Ds9CGFgAAdXDJGTAhU8TufB0YPY5I7D23E3sKsFAgvW13BsRT7plCZFMLAGcaDD4xToBiN2+KFNgGidG3glPMDU-+XVsO9O8R4HkBzJnawqc05SHT6avzGdtmGKz2nHPWdi+Z-TwX-BhdjVl-zlnCv2dU+GjztGfOJfq7p9r26uuBds7p1rlXevTcc+V7z8XJupd0Ddhb+3QuOd4lF7buXkvXd0C54bjNdu1dW99-eZ3LP7wO5Zz1MP9OI8+6j9zmPGI4+K+l+7-3FPVex+D0HpPKeNcs5tzrwP2fI-0-N57rPyec9EzzzXivxevf57p0Xo3Jfq9l6d5XiXze3ce8b1X3vvvE-d6p0PzA0fR8s9jGXhnU-6cz-j5zkfA+JeL9T1T9PSf18F-lxnuJXud9K-39RQ-NeRon+smfsvl+93X6Xw3tv9+N+YFbwH5-u+u+r6p0fvvt+Y0f505+7b416T7f6F7n5gFP5V5jSz7AHz4YiwFL4i7-4kJe5IEv575J4YG76IGoFTjoHn6P7v4wE174HG4QFl7EGZ4S44Et7kHt50Ec5f7QG0GgH96sFU5MHD4MGEFl5QEkES7c5wGh4IHCHIHwHgH07iGYEoFJ4yG4EsGCFU4KHH7yFEG8FV6qEc6aFCH166EqE15v40GGGd4GEs7aG+4cHKEWGgEr6cG2H8GiFSEYi1iQHOEOH05uFwH2E2FeGQHWEmEs7eHIFKFBH+Gz7GEH5V4hGyHUHRES6xG77mEREP4pGuFGHpFJF05hEJFU7ZF-5J4FE8FFGgEeF+EX4iHpFa4+HVEBF1Gz65Gn5V41HIFRHNES6tFxENFL49GYHxEdGa6ZFJ5dGf59G75b4IGjFAG+HhGVFL4CFzG-JVFJ7LESGzF5Es5rGyGTEuHbGKHpH7FqEIFHHS4DFX5V6nFeAnH6GrHDE3FmF3H8GBGbH05XET4bGDFbFlHpHEgrEIF-HrG-H1FJ6AmyFNEXES5gm4HtGQmb4aGglkGIlUHAll6wl35V7Qk5GokLEvFfGc52E4mYGLGvHuEpGQGfFwlw5wF4lUlEyNHkmRGMnIHnEYnDBInz63GclonMmYEQlsmyDPG8kTGUkCnUkLHlHhFYlp6SmvHSmb6ikAFZ7ylR60lim3jn78lKnDAqn07onam+4Inz66kpG6mskGkT73FSG6laloHKnsGmmEkx66kkn4nvGc6ylukUkpHulyHz6+m2kEFZ6+n6l2nDC+nmlhl0Duk+lclSHumhlBnhk16BkUHYAOkx7umSGeFZmel0nTEykpEFkKlFkgnz7Fm57lnn6JlpkVnl6lk34x51mRlJl0B1k1kl51mpmdkZlVn8GKlRmYB1mul0nFFR55nqljnL4pFTl+lSGzndly6zkdlLlGnzkcnrkokx5TkrlZ5TmLl7m9mbkLEDmtmYBTkjnqmWHjkpHXnTkx53lzmeGPkHnDCPm7lvlrnPkbnflbnz53kflRgpm3lHm-knkgVOEpHcE3kx7QX3nz5wVPl+GIWvl0CIWAWv5fnIU-nYV-lSFwUYVgBwWoWYVCmwVOkIU-Ex6-6b4TkWlgA0UJ4pGMWc5qn0UsUy7z4cWEUcUtlpksXMVxmeEsU8XAXUWgV+EsXZmSVUXz7j4ekpHyXTSnlplKW7GeFKUkVgBKWEVKV8Ul7yWKVCV+HyW6ViVyUSXhHyXSVWWyVSH66Fkx4OUllOVln2WakpHOUQGuWz76Vy5eW17z4BV+VZ4BWEUBVaUBXqV+FRUqUl5RWh5WTKKTLWywpybwoE7BTOYWbVpua+YSpxYcqBbaQBAzj5ykocDrDgoha5WU4Y6+j2ZZwFw8o5WKRnIxZ+ZSorzSASSlXGZLD1LJZ9hKq7wZbqpNo5aY7TZDbvxnZFaIAlZ9RlaIAVbjTbohI1YCzs4NYxIvYarbQHrtaAazpTYoA9bsR9aTUDb-ptrHX3pjaXaCwTbbi3ZbL4azYpXMz9blxLY3V5JUYpJUwMzrbgb9KQZfZDLmywb7YtKHY3RXUna3UFY8xjRLIvUrLXZQZ4ZSx3JEb7gkYsZNZgAnJvZHXI2QCfY4ZlL-QPZ-ZPaE2A7A6hCg5fJI5Q4w6yCXAI7lxs0o4CANCYosopAeA47BR44Ip8hE44Sk7jWvHe47F0WDl75wGxhJ4BXTSEXy24E2Vy3q14huFq2QEhWW40npF63EiG1wF-KW2hFm3n6K1nnK3IGq0IHq2a3q1xVe7q0G2u1YVzHq1sVK1a3C4W2+2Nlh0LF21l6B2O3B0c7G0u6YE+0uEBWe1V5hVR1L4u0p014O1plx2O6Z3EnW0R3Emh051kWl0TEJ1B78HJ2eEF0fFF0THu2gHZ0N0JXpE4B8YSF53t7d0q1d0907Ga0D1Al57D3a310VFj07E12x6T0h0x352z3a3l0N2r0h0l0uGb3S5aXJ6L3S591N6H0-5D1MkT2z5p096n3BHn0sn32yHL39230EmX29Hv3F2f277P0n14Ub2v3T1zG71AXf30FgMc7t0z2v3H2D6v370gOWmP0THr3QOV072v14jz0H113IMzF4Nu6j2YNQPAOYOwM944j-E72UMSEkNy04A0Mj1d2MPa3X1j4sMh1AP0McNp7YMMOm1548Ob6oPANCNR7b0b1iNYEIH8PIHkPsOz50P4nJ5SN4GCNX3MOz5cPKOyPdHqPIG-2D6qMiPcPh3UP8GaNL6GMUP-0z1SPaN0kqNl5sPh5SOj1SNKOOO6O77yOuOPEyNSPEiWNl3BMTHWMKMLF8OBMOPqlOPgX6PEnuP8GeOxPeNAG+Ox61hUMb1ZO0Nd25NMN54FOsP5NwExP0XJ7FMh18NVNp7hPh61PCOlMSESMz2NOVk73tPZ5FOKPNOyGj1dMYguOZNaN9O4E1Oz71MjMtM9Mf0yNdMmM6NdNTOVO2PANdPlNB04BdPDOrNL4DNl4pMVPbNl4ZN7N8ljNAGtPrP8GLNeMLMrMnOROXNu6bOx1PPEm7MfMt0vO+5HNbMLNnM4BYDZMz0gt5N57guFMyNQslOQtlNd2wvVOIsCMwtwF3OpNItp7XP0NYsm7wtyMovO1Ev9Mku4FfN4t30Et6NosGNktb3UvJGMtXPMsc6POUtgB8MctvMr0csUs8mst0D-PvMctAscsIMctBOCsT4YvHOSvsv8Fcu4PSu3j8sLGj2SvCu8uQV16gvAOQFav92QGj3em6sSE8tGtwF8NuUb2QGytbOQE4s6MeVmuyFAvn6GtN7Vld3n5fPn4Wteu+U+uTPBszMyM4X6sWOus-2hv9Gxu74BuD415+sCvhuHPxt07uv+M72gFOteOgH2vvOWX0OgHWvKtpvxMVuJMZtu6etJs6sIHRiv1z4uFNuD3b7NsEjpFtvj2NuduJtr6dvYM9s7ErMjtr3dudt5uxPjvC772ztH2Tu9MdsX19saMrvIEDs-7NvDvNtjtTsbuYFLuR2Hsxuntm7HtJ2Xu767MLtoXXt051uDunMPvMGvuGnvsyufu3j7uKvfv63-u3uYOa13sT5PvbsNutvGNnPRjGPgfT7GMgfGO3vGNbsIdWvdvGNjvGOFv52wdW2YcMnb6qMweqPwcL6qNIfruNuqNocUdBvEchuMdhtQdmMN34cns0fR2EdpHMdXt8c3s8eYFUdZ1Cc+NifYkCdXMSdu64ft4cfEnYd-tSevMyclFcfqtqdgdacFTb6DOXkVPRj6fkcYhGdwEgf6e3v6d0emf6fDv6djv6dyeH76fTuGeDPzuDMweDMmdmdtHduDNWejN6cMeNuDOOdW0hdzOtsLMBfcdRdxsJcJtxdL5Bf7MpeYG+ddPefZvsexcZcoMFdAERfPNJdAE2d+efNFeEPVd-O1cT4wccstvsdNd4i+etcgete3utcVetfDutdjutfOdV7RitdudB2jdEeNtNeNfLvTeruttNfdfBfzcP3b5NeDeRerdHvrdRvbdnv7cXu7dL69fOPdscudfpvHeYGzdL7zuSvjex2TcLHDdr7yvnfKeHeqfXcikfeac-dAHteQfsdkkgHtuNvGvdumsQ8Itg8SHDs2sVEcV4ivdn0Edw-glQ+z4wcetY-+cY-kt4+yEVd+1y0cVjuOsE8G5U9u5E8HetvGVzEsUk9nc0-3ts+YC+e510+Scw+R08+ycC9WFC8T4I-lsM-9ki+3ggdt1S8GdB04idtnOK-g8uEq8SGa3q87G7Na9T3pG6-IvYGdsrMG9p6o+F5Tv6-Nv72m9j5W9zdq-Nua-Ns6-Ns2e28s7YMe9vH28scN3e++9f0IHe8m+v1e+AOB+CdG+puO9XfB8wOR+8+x-8-R8veJ+FHx+ffJ-Enu+YOu-8HO-JPp8h76-Qel+q-++Ifl+9tq+ofV9z31-a0m84eN9b2t973t92-YFked-eXB+qM6+0e9-1nd9Mf99be19scVE4iBPD-N9rNy0z9l7u9SOD8x+V9x+T9L7K9SM2+z-D8o8H-z+lfj8LEr+S+j--en-EkmdL8Sn6-6fK-GcP-mcv81-+-Wdv8N-YEOdf8Ts--o9g+HnP-sLif4O8P+C3CAcgR16DN3egzL3uFxAHYsAB0XKAUHzV7LMkBbZLAeeRwFgAYB6-afl01v7Zc8BNvfLigJCaUCwmeArBrQLgEX8gBBfWgSQOB7T9WuyvVrrfw6768uuvA2HsHz678CJCJvIbsIJ2KPd86OIJrjbxm7iDcC3AyAewOo5q8mu7vJrl7w27yCGWggqfnMWkF7dVB8XXQbxxMH8czBUfCweAysGQNtBHOTgbl2UEp8bBvuc3nqXe7YFJWmg8Xv70lY69JWmvTVnYJL7YFQewfA1vr0h6hDai0Q81pEIw6xDR28QiQm4LwKAC1eLrcIdj2SGyFb+3rRIYTwKHC53epPZRohRN6U8shqA6frmyKFsschu+L3jXhKGs8qhwnBoY+w6H2CuhhdOoR+z6FfsBhP7HoaLxGG3gWhlbDIcwKGFtcxh8vWOngCV7pFFhFfCoisI17LDO2uzdYTsRs47Dta2DfYUvU2HosThLTM4Zj3kLNszmRw6XCZ1uFcELhhQsQm7yeHC5Dhe7N4cgJeF6C5aDwy0lcOMEuF-hnLL4XQD2Gv1thr9TWiCPuEJ9ARd3MEUgwRFUCfhuJJEbeA+E+C1hefDEV2xRETE4RbAuYngDL7yE4OywqvuSJiFiE6+1I+HpSNRbAiW+9IiQYyNtqsjcCNwnvpyOOLMiVBDdUkSt35FrdaRY-EURIN5E6EpRhpGUZgBWZCjTBEo5LnKPwHsj2hqo+4VI25GOCSR+-VUYfwNEKjAmhw6JuqN+4GiYRgTLUcSL+GP9lhz-eQvpxhGWcHRAg4EXZzdEiCvROxVIXgFc4+iDiTo7IcGOJahjSW4Y54R6OFGCj4BgY4XAqNc6RjqeYhWLsmPqHpjsBmY3AdmLVG5iXRm-WMS+1zH71-RFjeMYL1zHRUSRCzQ4Qsz2ELNthCzAsQsXuGAtlhHAjsasJJE8D5CfAvse6MFFCCBx3okcb6K7HnCxxQYsQnIKnHC57hTXGEUtwnHE8Vx4zNcQmI3HfDgRHLLcbKJnFAihxC-ZRngG5Z7jMA2wi7ueIYrXibh4ra8VKwPFp85xGfHcVnyPFn8Hxl46YU+Jv4PibhYQ4EREPUKv8QJ7-NYZAT2FG1lhiPEkXaxgnpDBRmQoCSGLEK48wJEYtCQKIgkxicJoolCXSwwna0EJ1QuCYYKQmHi8JiXLCSdxImYFthVpCiaJyImZs6JYxFibTzYlAE-RxbE8aWy4nfcaJVXDib7hhGy8RJDXLIksKKKdsTOtYTtprXkk0iXCSkuITJISEIFVJSQ9SSkKkmISKiWk6cSpOuF6Swxmkp3qZNkK7NDJxQyyeuJ0lP07JOg4yb8OUY2TaeDk+ng3XclZjzJy-JyRzmsnQiApQrEKZgDOY+TMA+9SKRqTCm3hUhMU6sXLUSnYNEpNnRKUFJ-EuTWxcU+YfnVrBkjNJFIoolSKKnKTvJdIsqQyJKlMiKppwmqZOKqmXCmpXIrIjyJal8i6p0AtqbhLmIFTQuKk1Ris36l+8DJUjHqZx0GmUS+pUjVKfYwmn0SFpu+RSR4yWmsSGpFzDaRMUkHt4RpqIqaeiK2lAE5p2ImaYwIOnVsjptbNaW7gin2iiijozSc6KyKuiHpg4gyZ6Lemjinp9Un6Y1JUnACvpbrF6eAI+lKC+pgXEGZuyhk0sAZ4o7yUmL+k7ckZ20oGV5LBlKiEZ-ktGXTmsldNFJxAmGeJxxlvsSZ-QlGdxKJnFcqZbuVKfWJpnqc4ZV-JmX+LJmSSiinYjmd2OSm9jNJ-Yvme9L6nDiBZ30lSWIK5n-TvJMgrIrOJFm5CZZ4MnmdhKFm9SlZ+EqWfDIMljcJZyMsWeRK1nTS1Z1EvWbRJ1mWCTZGouWbvjkmisFZiIs2dJwdmVirZ1Mp2b7lSmSt0pfgu2ZdJdnXS3Z7MzSYBO8nASg5oEsOeBL6mQSsi0E0orVIMnwS45kshOVNxUn20Y5oMqOYrLcm+sM50MpObDJDmays5o0kubrKLmTSK5mBYaYz2SnNC85i0guctIbnWyW560iOZtI7nbS25zstOegyrnV0e5vudKRRT7nMyB5gPIeYHJcLDRpJUxWSdUQUmLzypFRWeYLLlprzqp88+OXMU3njiRilvA+anIbp7zWpR8syTPIsnnyrJy8-OdvPVmrzPh184ic-JTGXz9Zu81+is1PlHd755g9+al1vmWyAFmXIBcTL-nsSIFjsqBb3JPmYNv5mDbBj-MEkgKLRMC0SWAsnmvzbp1RQqTPOKlTFSp+ClebvMqnEKt55C7SYQt+mULiJuC4+avJI70KL5J8yjswpvkjEh+nCgaawuLkbzjG7CpltQsrmMLDZyjUaMeMcYSLTZwixubIubncLmJ8i9ubQqT68LnBqi2BaIsOnKLaZgi8rvordy7NpFvszRXV0UXEkzmw0e6VMUekzznpIxV6bYvXniLPpzi0WSfKc7VEAxjihhbvK87eLM5G8wZprWsXKzglqs1xTwtXmIDfFpciJaRISXoDPFYiqRV0yQUbNAlgCuJcApSVKL7FxY9xZ3IKUaK8l+0spTQJyWDyqlBimpUYqyWmKKlWCopRMSsWcypiXA6orzJnn8yelLiqRcLL6UeLV54sjpfpN3nSyRissoZfLKmXZyBl4S8RWoK6XRKJlfCpZRPxPm7i5lIitZVYxWWYyRl2MsZdkpOW5Kjl+SrZYUpmWQKbl0Cu5Vor2WKcDlxJJBZ7JeVoKHlGCnZazLOWtLxiIdKxaHJnlRCpi0PEFf0tiZ1lpoSC2CRvMTlgrxl8KvxcisJYjF0JiK-Hpio4XYrcCNnaFbCvWVSLKhEKxJeItqG4q6c382ueSuOWkq5F9KhRZStsHorrlJ88yoyvuXsrbmAK18dypP6cqUF-K4Scyu+WirtOrK+-qsTnl7EF50q8ObKpIVy1cAkKipiqooUN11VVCxVbpPlXJyli1vQ4iZL1WzKTiV8s1YsscZaq8VRq1ZcqqfkWr4lyja1W-M1WYNbV+yk1Y0I9X-y3VrQnVecoNWFiKiLq7oV6rUUhr3VPqwruGr5WRr3x8az8bGsZl+rx5ia35QGv+WrE8FmqghXsSIW5qlVzqshYWo1UhqsOhxFkScQEWVqUVxa1CfmqCX1qsVja7qdmsiVWrVG2DLAENNrVOrO1rkgdbsuVVSMVmPayRbE3HUyLW1DK0tYGpHXBqli2ovtcUrnXdz21z46tf3PLUJql1p0hdZMLXVAFNaU6jNUepwWrEbFexOxZqocUnEnF161VUHSwBuLH1wypYl4svVIrnVgM+9Q2tvVNqrVISw4pDK-V3y31hckNbEr-X9rJ1XTEDR-OVWYCwNxsgDdOrQ2zqoNhAj9YuqQ1sqsN9smDcXQQ2bqINlSojdUoo21KqN9SlDT8zo3NKyN6TQ4u0r2KdLVi3SzVb0q41PrY6WAQZTxvfXKrRlbG79VasmUnFplgm01aJpbXSaox8m2yRxrtXOqtBym2DWqu2WSbENqm1JZOo5bdqzx6mzDUsSvHGbW55mlRYptJnabSlIayVqkP43brTNu64Tfut02Hr7NWU6zeYts2WLDiwckNcCs1Wgq9i4KkLbxvzq+k6BTxITc6oRVhaxNk65CRFrRUPEWFQW+ZclstXZbwNqWyDUsXPxjqSV+WoRYluHXxa9NaqmlVavrmxaTNyqxiZlsuXNabuAW3UY1vLH1aY16W55d1uOntak1vWz5aVuPWDaz1rWrNQCRlUN07w3M5RnNo2Ggkthvxftqto0kuFFt2q2bZ21SFba6Fy2utY4322gD1tGWuYidulya1LthhQ7XloqI3bPeZ2xyXdslHTbB1sTR7RqVe3oyLtYfZ7SqPe2nLNtwUn7Z0LB1hqgdq6h7VGoh2uCAdrsqHZRpB3ubjtuIuHRPmu3EMEdF6gEjmoe15rZtBagnUWuO0lqSdZai7RWtBJVrNtNamnUds+1MKGd52uWgSCy0VN2dbavHR2qZ0qaydRKvnRpqDpc7y5FO5JeLt+1s7ZpvxeaSzoa0La3Gsu3DYrvw1U6OtquuzertI1E7nN0u1zZrpz7K7PN2uxpZLsY266pVAJK9bNpvUPa71m2h9bbsi3t59aG253XFuO2frrdSWznb+sd3-r7dgGz7cBtBKgafd92i7XGLD2C6-dmyoPWSq906ak9nqiPahoT2+qM95sj3fOoW2EyY92-X4l033pu6tdbOhZntoeZF6DdKeo3QXpFUB601UeovvXqm2bbWNs29jQCU40PbuNvel3V7ndzu7+9nuz7SJs72+6RdEm9vYHou1NcTOQ+uTSPpxUz7I9bOjQb8TU3d749c+j7Zzoe6gkOWKzRfent33obl92ei-dYNX2gLD9au9fRruO0PdN9PKu-Topv3I6J9Q2j-fgzf1m6z9E2gA23tm2BaLtwWh7aFpAOk7Pt0c5EpTrZ1wqFtCWqA-qoQOM7Od6cuAzJpQOYTNtucrAzaoIPvCiSuBY-SVogN76RdFKvA1VqoMTqMDdKnA5frAPYa0DLWlg4XqIM2aaDZepA6-oBK8Tjt-Erg8PJIO-6BD3migzlJEPTyN6erehvNq8YKq5DkchQ2pJkbD7gGO8tQ-vI0OoGdG6BrZrPp0MKCu6HOow9zp3q87jm-O1JrHosNvarDieuwxVqUOp6nDp+kw3Ths6QizD6XPPCrrcNtaAjhGjw+ur0PlKZ6eugwwKpUN16IjI2qI03q8P+yEjzG7AvIbKGKHYmxO-QdAYqbk68j8BrI6PoKM0L-ek+2Oqoxt7GGSj2B6fuYaqM5ayja+uo-ZOD72Gmjwuro2LqKMS6+jUuto7-LV6MGGjwOio-4Y6PsHF+9+oY9wYmPEaMjOusY31qmOvL9eqOnI+dIWP0a1jhIjY1bpUmZHHGduvqcoYMn5Gg6n-IopoeSnaG3J5Ri4-oZOOGHY6ASm46zoeNL6zjlh7ydYauO2GKm0Go490fymUG3jye2Jshs0n0GAT5+n4wrpeOTGQTt+mE5wbRPQ6ET4RlEz1pxOI68Teij4-EYJMpqnj-+u49IYxPAGQ1xxyddkbVXnGlilxvjQPsuK3HnV9xq1Y8aZPPG6THIk4rUa5OfGhTuBzVc0efX-GWTD8nky9oFOgn28WmvYpCYZPuGxThymU4DqVPjGaTyJtU6ia1PBG5TmJ5VV1qNO4m9T5Gg05-p1PEmLT4hq02NtWIt6zT6R7kr3WFIh1yOkBvwuFp9Osn2S7JukogaDPcnSSlRtMilr9NpapCGKmM40YjPinHa-rD02cRTNU4pm5B8IhGzDOuH1SNeDM7CaTOjGsz-qzwk1pLPTH8S3PGPByrLOmm4zyxis6sYbPrGazmx+iqPLrPJGqzzpls9SfCId6-CXe+MoydeJ96Bz-p6MoGfVIDcfSoZt0uGZLzT6cygpmc8HvoqLi5zvxoc5KbTIb7MynRvczvrHPgmjzuZjc7QcdoGatzmeic9qbvO566SHLcjrbIPOhGVzvBp8-wZHPv6PzrZ-0u2aVrey3z5Jhc5SZ-P+btytJ+iqcdeIO7PCTuvwtcfnyvqELnJycvOdHKLmlyrxtMu8ZQvrmlaoegi9ufCKwCZygJoi4eZLyIz5yp5mi8qaouqmkLhZvC8WbgulmWLupri-qbQvom6L75ni9ib4uRGyL0RrC7EaEvUaBLDekS7sZkv7GoLhxzwvjvCKE7kKo5-EshygqTnX805+itToQqYX1S9Ooy-yfwqrmDLhFx2mwtgqJm0yXCsywVteK9q7L8puXONLcv9GXLl5hy6xZLxy6nLzBny9xbUuBGTLsxukrvx0ufmIrjZkK82ZUs164rtpjSyboSvyWkr4FrK5BbkrQWlacqvK0tqKva9FKul7SvpYKvoX6Ku2sq7yZqu4WDKllqq8KfVLmqpCLvOq6uJjw7surpBvq85I0r0W5c7qnq75aavqnXiEfMa-eamusH8Sr9cjvCJKu3KhrsVhq-FYWviW2ryVja6lasrbGTKkhg69laOvKW-CupZrhdfANSlvTt15k2mUusAdnSsc40sGfVJPW3BT1naXLkutaVLrGTS6+R0uuEVLrwzS63R0uvz1LrUzb6y9eGv2lGLjtXUrDZq0fW6tb1zi-ddCtyka8wN6s5jcEvY3FjhN0S7je2v0UXS0N0ApDc7PXXjr5N068Tf7NylBzUpYc54RP3a1Qb451m+Va5uG9jSs550uPoutjdTSy5sW81eRvz6Jb8ZkvALelzg3llItyizLeou-XtZQthGzqQP3a3mL7N-y5rfYv4lFboDfW4+Y+vPm5b-Fzm-eNVvrWlaZtwYRbctN23drTt95Q7dktS3uzdJZ27eGBuStAbNui67BdNvwWw7D1hW8hetKoWo7pRp297rjvYXlS-uzm-hZTutXKbxFrOyvozu7mY7atx68CYLvuW07Ot4eUjZLvjXfr6S00pkudI7MG781-2-nuNKkCm7j+j6xQI7vfmy7iVhO-+bzsTFIbjYlu37Z7u9mB7LN026pblLqWpSuRhe9Hd+uFGV7xR-24ZetK07Obplne41bXvS3Hr7Ug+98Y3v52Lrjls+85bnsa3lSAi50p5eNKz8n7tdh+0bY-vwmL7wVu+zjb-u8Wr7kVj69FbfuO3kbgTL64E1hsmjTSZosBz7aXsM2AHill+7aNNszaLrhV60kvOdIra8H-NodqaWN7EPjLlNw+caUNUEPoznN5tsDfau0P7LCt14dQ9vv+2HVOD48xg8rsNdq7zD9+zqX+2sPNTjD2a9w--vsPwr5D4B9I6JtylYdlD-u1g4ptO3EFpD7-aI8QfyPkHkjpm9o-OsDnQDY5m68Y80tPnfThj8q9FvnrRapm0WtwdFp+vBlIzljmh0OdjM5l8h-pfA948Lty4IysZe+8mS4fmOeH6ZPh-4-zOBPJrXpE26E7EfxPLbG5vG9E8NPxlaz7j+s546UeuPB7uT60-k+kvZP0rsTye8k+nuZODHY5tm9U-pNAWe9A53m26R64+kBNQ54W-6VFuNPU74ZSW907cf9P6jgzmEq06Yf+OVbnT4u0ua33xktbszsJ05vPP1OBH0Za85mSM2TOEnM5szZs8AfDOrN7T7uxuef3rOcntT384c8kv7OhVlzrR8050fbOKn1zkIf6VDsDnw7T5yO+89XvBlY7OZeO988TtXnk7-znp9GXTtDnM7oL7O0BdzvQvL7gL7q686mf+PS7kLkJzOfg2Zk0xyLg22OfrvYu4nmLrG-i9bvEvKznzmR7C6OfUvwHe5ivT6Sr2EurnpLjR+i5KeUuyntLwA6y9yvxl57bpRe2OeXuCufn4Zde6K83sznt7OZXe0Of3uyvD7wZZnf6VPuKvz7krhF8K78fKuUXurwiaq4xcbnn7-LiJ-q+rk+kZdmZQK6a62fGuyX9ril9K6pdXnQHhrul0ucgeWuVHrrj2767ZcDnAmwzU9ZlfldPPtXVTt0pg4HPYOcyuD-0vg4TdWOiHmZEh6m7IdAWKH8ZKh0m4Gdjm6HPpBh0Oc6vpvWjT53q6W9lPZujXmbhZ6NdzcWvK33qpt94cLckuo3Dr2t0643PLXq3cjjtx6-8eYMHH8Cttyy4Hf7X83h1mNw857fhuJ3s9-MkY-xLQrZhTZO668VXfDNV3dHVd-PVXdTNV3bg1d04+GDQqtK0KjJtCvI7QrCK0K7d8mfXd6uz3RWhsuXZfdhPhyb7xt32RidLuiX9FOsg+87eO06yN7gm1IS7LfvhLfhYcse8EPqlhy+7mm9B-tOeFhyd78Sb+75foeanK7jm7B4aebumn+ZFp02TafhFhoHTyD10+I9guhyfTuj3m-w-WXaym58j2M6zxUedXZ7-c+WRme4ea3oHxU4J6WfCeVnDHz+7x4A9K1uPdr2Tzs5o9SOFPLrtjzS-E+DuuPDmosh4P49+u1PAbpj3c9I+zuVPejlj5G-zJvPN3HzxD185s9iu2yfz2DwC4c9AvayILlz-R85YQvKPULrzzC9A9wuAvWrld+RabLR7yyaLvz0J48+fucXkH6E4l6k9OeZPQX9t1Z5A9xfu3snzu1F-U-ZeSbyXza5l4ucxfx3pXyd2F+ndufQLlX1B8V5w+weBX+ZIVyu5FetfHPQ5CV516leAeZXzXjN6B4VeDfzL6HlV5B7VejfRT03hTbN6U3lku1RZVy4t9i+dkTX43s12e9HXLeUv3XtL7WVX67eJHiH1aU2WXXneCv63rJ-N8eWbvoHx34e5t8M-teavr3rl8N-ne9emvlH6N5u9jewf43kHxN8D-KvILN8+7tN+WVqtNks36HnN6D+Y-5kC3sP+W3LnB+2FUfZbxDxW+h9BO2yB7PH84cA-1vEfP7sn826J+3n-vGXnH1l87KLWiyvb+H1d-R8KOKflMrH3k5p8Ve6fL35H294F8ffay2Orn4u8nLLvRyJj-ErOXxEoWLHcF2A-L8quO1ZfUzWX24Nl+nvwRLjxX8fZoseOkLXj48qF6l88edfz7i3wa5N8vzlfxPoi9QYQtRPtyaNmCxjZt+4yZy5ZvXzl9V8QenfrPw8jd7IsFsvfPrvC8Ibt9VezfHLiX6Z799fe4-lnycnh9HIEeyLRHmXyR5T-lXawFHuC9R4Qu0es-3nvP0q+GBl+kfOfwL3hfY8oXlx25CZ-OT4-N-8f55OZ0X7CeV-vLJfvF7388P9+RHSFvljOUU+d-fftf1TzRftv1-g-Bfs54P-xPj+nvw-wC6r+Auz-hf0-xPzBeDszlrPMv2zzBfs+H+uvYAWsM57IuufT-7nmi556v+l-fPcF-zw-5r93+0fe5cPfOXC8oXIv3-tv+f60WCFli6-+W3uCJJewAXt6ABB3u-7yeqvvjL7+ynvAFT+S5MXqIBmnhX4Mu25Ey6gBvPsf5r+eFuPbYB8foQE7+RFu2LbkLXpORteo5B17UBZ-ntJ68lASr54WA3mRZyu7AaX7VGM5BN4IWU3pwEautAZx4V+19nwGW+55Ct7zkj9ihYbeSFq-YyBEnuf5WuCgd-Yy+R3swEneMFmd4qBaTmIH9uQgRgHgiXrhoHc+agfp40W8DjoGJGAgXV70B5ngYE-ecFn94y+APmRZA+CFiD4eBufim4oWUPvOQw+vgaX4I+XgVX4wWKPoEGCBk5CW4RBSLv4HiB5-hw4hBjhkkFlaKQSywxBgxqORCOGQa27bkvhnkH0+S5Iz4FBttkhbwMM5Oz5pBd3i4Hh+NFmo4lB0flEGC+TQVv5FBZAar5kMEFO6YPk0vnSSPkcvlIT9BwzP0F0c-QfPT9BUzP0FuC-Qdr4XiuvviSPkGTI+Tkc75F0Gm+V5I+7-kpQn0GvuPQWt5y4d5GsGoyWwYoF3k4we75gUiJhsGFBWeHeQrB-vn4R3kWlHeSPigwaH57BZXq8QvB5wQQEl4LwcMGgEhFC8H3B6Dn0Gp+V5On5fBmfmCFn+p4uVZwhfXkrQIh22o8HF+MIfVZIhjHgsFSaqIax5-BdfoMEN+-5E37PkLfqSEAByIbb6EhYTpSHpB1IX37ohA-oyFD+4RLSGBSt5GP64hE-viEoBtwTP70hRXuSEleEIbUEHBXghyG-BYoc0H0UbIWKoChDXkKGOBCwQf59BR-kiEn+qobCGX+Xwdf6aht-gcH3+Ood55liY3o8Ev+RoW-4GhH-m+Rf+z5D-6DBf-naEUhQAWaE0hCXk6EMhV5AS7-kjdj6G0+MoQgEPk7dg6G8hNoYH5hhhgReJYBfoZ8HKh5gVaH8+XodKHqhJAX8ELMKwRQH-kVATKE0BV5HQE5hsIT155hLAX8FsBXwRwHlhxodwEPkvAY8H8BlYZEEFhpFg2GxBz5Et41hFIdIGDBsgayHyB3YacHKB-YaoF9B6gVmE3Bb5NoFDhugXWHhhUYPqJjhwoU2GmBI4fGG3BlgVOHWBLYaG69h7QWmSKiSoX0HOBh4XU6O0IIhrS3kngY8FraD5D4GDBfgc+QBBd4caHBBV4fr4HB4QU+GNhSIdEGfhrYa+HSmXwYkH-hyQcBGpBoEXSEPhpwdkG-hLIYBH+h34eOFRgxQf+TM+4EfMZoR5MjBGc+KEaKG3B9QThGJhMoejoERtgURG7hfwZ0HkU+VjZa9BJlhu5aWCvgxHlWiFDFqUU1VkrQsRbgixGzBRFPMFRWmBmxGWhWeOhRQUPjvhSbB4kfEHlCokfsHCR2ZkxFieDls76CRTIXREwBHlsmwyRmgRxEpOVEaUFqWGTgZFz+CkWTYmRbtrhR4BOkZKFyRyYTRGphGkeREORyfgZbghLkSeEOW0ISZbZ+bkeob4U+flpaF+yFGiFeR3ngYKmhaljiERReIR5YEhKlkSF+R5vq-hkhQUQAFhRIEVFH2+NlnrYJRnoT5GqReUbBEBR8EVlGIRSUUgEeRoYWhT8hcUcZFRW2nrBS6eOUSv4ZRjQQVFoeKUa0HCRQQg1GghJliqF9R7kQFYahA0aoZaW2oWNElhHlr-ywUhoRNEYhNlk-5zRwMjNHRRwkcF5qWtochT2hKlo6FbRqUS6EbRYTjiDuhe0blEcR3ofhS+hl0cVEOWgYQhTBhO0ZVGv4aAStGRhRFNGHXRsYVFa1iUFPTKvRxniNGkR50Y5FrRvUQZbZhHEbmHgxZjiZbaWsFEWHQxiITZZlhWlhWGoxoUdWEIUtYWpb1h6MV+HIxwgWhSiByFO2FYxqUV2EqWPYS5Z9hlMYoF38+UZDHqRwkaOH4USuvDHlRAVhd5kx+gbDG1RvMYuGMxX0fzHNR1MdZHDA9MZuF4xQMQTH2BwseL4GWR4SZauBLlu4HIUl4WpbXhCFLeEqW94WrFDeDlnD56x4USrFvhwkR+E6x1oWhQ-hFsdj4KxUkUBEaxskeLGv0UFKT42x5Pu7GU++FNNZaxN0QFag6vsdyEeWqEY7E8xdsW9He8hooHHLhSsauHOxYsVbG2RBsfZFmxIMfHHORStMjwZMyPORzI8olGf7I8dHMjzz0yPFMzI8bgsjw8RHFFpQcU2cYb7hE3FIJSExnPBJHCU2weqTk8Tce+6hSYTlJRdxHsTJRQBzPP3G-2dJCJQjx4OlxQPBDcYZGvEfcRPHVBY8Qh7sUkflIRSURcXTYzxKccMBSUucXZTCUrkZnGQh+JM9zQsa8d5FHx5VqfEHCzFIFENxwUexTi21FFiFjxkUXPGy2z8ZbGc88UQfGJRDFMlH3xAAdfGDWklCJ6gJYAd-GKBwCfHS3xTMTvEj+n8dpGO00CaFKIJ04YAlhxl8W9EoJLtufG4R8CXHFCsXtlxQb+eCZ1EEJssY-Fgxmcf1HsUaocgnDRdCQXHjRY8bqEdx00VxSzRrCd56VcRksJTmhJ8T5zMU60XPGbRDcdtGSUu0eIlAJB0aIm9xJ0dIlnRDCUPFXR-CX7Fy4vCdfpqJQcVniaJkOmvEvRnCXzFMJAscolCxJiSLGCJCcZzxEBRieQlCs6YcInUJyCRDEuJg0Ron5hmcXDFcUCMV4mTRuiSjFjxaMUEk8JmMWvHYxc8bjEhJ+MfxQD8zFMTENxpMeElAJFMZJRUxJ8TTFpJUCYOHCUNrrknqJASaVEMUk4fkn6RkSbOGc884ckmmJsSfglCssDtRTrhpSQDHsUgTDxTWi8Sc4n8UisexTKxJ8arENx6sXPGaxa8drGSUusYMn6xJeKBwEkzFC+GTJoQZnHmx4yV-EMU1sSsm2xSyfEEzJGIGXGE+oyb3FuxGyQPELJDMcgk+xByXAHdJRSTMm5xIccMkVJaycYlbJNSdMmju1FPhGXJrUS8ktJPyduEPJlCX8mumHVpL5tWtETVb0RdJGpTDMalHRxqU89GpRTMalG4JqUPEZpRGUpscMBKUS1sb4aUYkXil-xelBinW+BKd3HhS8kVCmO+JlMpEgpUAfJRwpmkT1be+C1rpEWUT0dpSzxLKc8mO01lCinLxBVqvGkp3yTymbxU1oCHEpPLlykZxPKYfEyp7iVnjAsMMTVYXxcqb5EaU-kVCl3xU1g-EFWT8XJQvxbVm-ELWH8fqmrJiqc2HGphKQAnapAAeanpRNqZlGqU2UeqmKBdqV7Eupw4YakFJWKZyFWU1tj1avmpqZgmqpgoSZT1RQaTHHKphCeFLEJHVqQkep0sU6lpxdAG6nApGlLQkFW9CapSMJmaWf7fMnDIpRsJNVhwkdWXCW1Y+KclItFQpAidWmrRWKSIkLWYiVNYSJVlFInNptqbImNpYTvmkeSlaa6kXR6aXAkppzdj1Z3RpaRzEjWeXuOnBp2adymzpryZOl1J4Uj9Gjp1idpS2J06YmkGUjiaunSpqlK4n7p8qVimeJPKd4kdWviaen+Jx6exGXpuqnJQjeVlGEkaUESQtZRJbVrZb3pzcdpQJJU1kknPptqakmPp3aZklAZSiYel-u76UOnhSLMf+lIJ4GXs6-p7KWkzoRoGaGmoZ5piZQPePVg0mfphEQVZBuilG0mEZyadBldJBlD0kFWfSVCkDJU1kMkLWIyRpRjJVlBMm0ZUySNaGxzGeX4ppxqnJTLJnGTEnkZX6Ygz+EPVrj4dWDsaxlkp2lC7GiZECdJmup0EYxlQZ8mVcmCZcGWpkIZ9GUhnlBsmW9HCZUceJlLpKmZYnUZa6fpkwpwHIpSi+vGWRn+UoKfRR605HHrThUjEXSR60dHHrTz0etFMzm0nlI6x+ZXGVFICR7lHWmF0qyR7QBZmyY7Tq0XmbsFBUmZq8QBUkWT35uZNKZ4TBUyWYVGpZ3qWFnqZdmROmhU08Ylmcp2WW9FRUbglFQ+ZgqTFQoePlLH4OZ4qXVmSppWZBZJUAYOxhpU8mBzi2IZKGpi5UZcACjiokAJKjxYxVL1SBA-VAXADAHAKpjFwrmHVT5Uw2YVQBYXKBHCVU0QLsB9ZikBrj1UUAOnC4oxaKEAtUfKILRDZI2UVQ9w42WVTEogOAqjEAfkHIBAAA Open this visualization in the Vega Editor (although the link is long, and may not work with Internet Explorer)>

@
let aggTrans = transform
               . calculateAs \"cos(datum.RA_ICRS * PI / 180)\" \"cosRA\"
               . calculateAs \"sin(datum.RA_ICRS * PI / 180)\" \"sinRA\"
               . aggregate
                   [ opAs Mean \"cosRA\" \"cosRA0\"
                   , opAs Mean \"sinRA\" \"sinRA0\"
                   , opAs Mean \"RA_ICRS\" \"wrong_ra0\"
                   , opAs Mean \"DE_ICRS\" \"dec0\"
                   ]
                   [ \"Cluster\" ]
              . calculateAs \"atan2(datum.sinRA0,datum.cosRA0) * 180.0 / PI\" \"ra0\"

    clusterTrans = aggTrans
                   . aitoffTrans \"datum.ra0\" \"datum.dec0\"

    pos ax field = position ax [ PName field
                               , PmType Quantitative
                               , PScale [SNice (IsNice False)]
                               ]
    enc = encoding
          . pos X \"x\"
          . pos Y \"y\"
          . color [ MName \"Cluster\", MmType Nominal, MLegend [] ]
    encText = enc
              . text [ TName \"Cluster\", TmType Nominal ]

    clusterSpec =
        asSpec [ clusterTrans [], enc [], mark Circle [ MSize 90 ] ]

    clusterLabelSpec =
        asSpec [ clusterTrans [], encText [], mark Text [ MAlign 'AlignLeft'
                                                        , MdX 8 ] ]

    uncorrectedTrans = aggTrans
                       . aitoffTrans \"datum.wrong_ra0\" \"datum.dec0\"

    uncorrectedSpec =
        asSpec [ uncorrectedTrans [], enc [], mark Circle [ MSize 200, MFilled False ] ]

in toVegaLite  [ aitoffConfig []
               , width 570
               , height 285
               , gaiaData
               , layer (clusterSpec : uncorrectedSpec : clusterLabelSpec : graticuleSpec)
               ]
@

-}

clusterCenters :: VegaLite
clusterCenters =
  let aggTrans = transform
                 . calculateAs "cos(datum.RA_ICRS * PI / 180)" "cosRA"
                 . calculateAs "sin(datum.RA_ICRS * PI / 180)" "sinRA"
                 . aggregate
                     [ opAs Mean "cosRA" "cosRA0"
                     , opAs Mean "sinRA" "sinRA0"
                     , opAs Mean "RA_ICRS" "wrong_ra0"
                     , opAs Mean "DE_ICRS" "dec0"
                     ]
                     [ "Cluster" ]
                . calculateAs "atan2(datum.sinRA0,datum.cosRA0) * 180.0 / PI" "ra0"

      clusterTrans = aggTrans
                     . aitoffTrans "datum.ra0" "datum.dec0"

      pos ax field = position ax [ PName field
                                 , PmType Quantitative
                                 , PScale [SNice (IsNice False)]
                                 ]
      enc = encoding
            . pos X "x"
            . pos Y "y"
            . color [ MName "Cluster", MmType Nominal, MLegend [] ]
      encText = enc
                . text [ TName "Cluster", TmType Nominal ]

      clusterSpec =
          asSpec [ clusterTrans [], enc [], mark Circle [ MSize 90 ] ]

      clusterLabelSpec =
          asSpec [ clusterTrans [], encText [], mark Text [ MAlign AlignLeft
                                                      , MdX 8 ] ]

      uncorrectedTrans = aggTrans
                         . aitoffTrans "datum.wrong_ra0" "datum.dec0"

      uncorrectedSpec =
          asSpec [ uncorrectedTrans [], enc [], mark Circle [ MSize 200, MFilled False ] ]

  in toVegaLite  [ aitoffConfig []
                 , width 570
                 , height 285
                 , gaiaData
                 , layer (clusterSpec : uncorrectedSpec : clusterLabelSpec : graticuleSpec)
                 ]

{-

Other things to do:

 - add some description of magnitude / parallax meanings
   (since talk about astronomy being awkward)

 - tooltips (show multiple fields)

 - for errors section, could look at quantile plots?

-}
