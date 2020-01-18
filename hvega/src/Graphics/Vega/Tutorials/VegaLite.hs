{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.Tutorials.VegaLite
Copyright   : (c) Douglas Burke, 2019-2020
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
the functionality provided in version @0.5.0.0@ of hvega.

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

  -- ** Comparing Ordinal with Nominal data types
  --
  -- $stripplot-mmtype

  , stripPlotWithColorOrdinal

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

  , skyPlot

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
  , skyPlotWithGraticules

  -- ** Concatenating views

  , concatenatedPlot
  , concatenatedPlot2

  -- ** Repeated views
  --
  -- $intro-repeat

  , repeatPlot
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
-- [@Guides@]: Supplementary visual elements that support interpreting the visualization. Example setings: 'AxDomain' (for position encodings) and 'LeTitle' (for legend color, size, and shape encodings).
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

-- $compare-to-elm
-- @hvega@ started out as a direct copy of
-- [elm-vegalite](https://package.elm-lang.org/packages/gicentre/elm-vegalite/latest),
-- and has been updated to try and match the functionality of that package.
-- However, @hvega@ has not (yet?) followed @elm-vegalite@ into using
-- functions rather than data structures to define the options: for
-- example, @elm-vegalite@ provides @pQuant n@ which in @hvega@ is the
-- combination of @'PName' n@ and @'PmType' 'Quantitative'@ in @hvega@.
-- The top-level functions - such as 'dataFromUrl', 'encoding', and
-- 'filter' - are generally the same.

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAXAlgY2QGnCgBNZ1ZUxQIJIBXeAGwsgAt10AHAZxQHpf4sAO4A6AOaZ0LWgCNaXAKbxsAewB26BRpGrovACIraYgEL1EC3iwBuCsbF5wum+Fdv3eJMr3uZYAWlgAgDYARmDA0IAGfzIZBgVQ2BE1FX8WBVgiJRF0LmtIAmooADMVeDh0CmBIdABPDgVmPIKAX0JWoqgAEi5sDLhmNk4efndkiSlZEUwVXj6Bh3H-BklLawBmEQArLnVCwkgtVSJMNTFqyAAPS5LMBQYiZg4GG7wMBqa0SABHWlgNJJSJhbJBWp1Dqo1HcLmhgO1WkA Open this visualization in the Vega Editor>

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
data we are encoding. We can provide a hint by delcaring the
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
-- 'Background' configuration option.

{-|

The 'configure' function allows a large number of configuration
options to be configured, each one introduced by the
'configuration' function. Here I set the color to be a light gray
(actually a very-transparent black; the 'Color' type describes the
various supported color specifications, but it is generally safe to assume
that if you can use it in HTML then you can use it here).

<<images/vl/stripplotwithbackground.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEDGD2B2AzAlgc0gLjMSAjAhtANaoBOsArvACaZSmoEAUADADRjuccsB0AjAEpIAXzbgoAW3ykidSABdkxSOIiRq+BfjqgI6iqQA28gBYKFABwDOGAPR3S+AO69UyBaYq4K1gKakcPAKfsG8cJJ2ACKUqABChkR+dqYAbn6o+HbS1iGkKemZdpradpnI+AC0+FUAbPy11fwsldq4Rn78+LzwsJWmfvjUAbwK1qmqEuqIsKTSCrqKAJ6WfvJjEyISYhKQACTW0APSZhY29naF3e6e3rzIsHaHx1lXlUYeyakAzLwAVtYEJN1KE4NRkPB0FgcAAPRYoPxGWhYSCWIxwjjLVbyACOFHwwQ8WmQ6VEWxEQA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"plx\", PmType Quantitative ]

    conf = 'configure'
             . 'configuration' ('Background' "rgba(0, 0, 0, 0.1)")

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
'configuration' ('Background' "rgba(0, 0, 0, 0)")
@

-}

stripPlotWithBackground :: VegaLite
stripPlotWithBackground = toVegaLite
    [ dataFromUrl "https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/data/gaia-aa-616-a10-table1a.no-header.tsv" [TSV]
    , mark Tick []
    , encoding (position X [ PName "plx", PmType Quantitative ] [])
    , configure (configuration (Background "rgba(0, 0, 0, 0.1)") [])
    ]


{-|
There is nothing that forces us to use the x axis, so let's
try a vertical strip plot. To do so requires changing only
__one__ character in the specifiction, that is the first argument to
'position' is now 'Y' rather than 'X':

<<images/vl/stripploty.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAXAlgY2QGnCgBNZ1ZUxQIJIBXeAGwsgAt10AHAZxQHpf4sAO4A6AOaZ0LWgCNaXAKbxsAewB26BRpGrovACIraYgEL1EC3iwBuCsbF5wum+Fdv3eJMr3uZYAWlgAgDYARmDA0IAGfzIZBgVQ2BE1FX8WBVgiJRF0LmtIAmooADMVeDh0CmBIdABPDgVmPIKAX0JWoqgAEi5sDLhmNk4efndkiSlZEUwVXj6Bh3H-BklLawBmEQArLnVCwkgtVSJMNTFqyDrLkswFBiJmDgYAD0KMBqa0SABHWlgNJJSJhbJBWp1Dqo1LcLmhgO1WkA Open this visualization in the Vega Editor>

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

Although not used in this tutorial, data
can also be defined algorithmically - using 'dataSequence' and
'dataSequenceAs' - or inline - with 'dataFromColumns' or
'dataFromRows' - or directly from JSON (as a 'Data.Aeson.Value') using
'dataFromJson'.

-}

gaiaData :: Data
gaiaData =
  let addFormat n = (n, FoNumber)
      cols = [ "Gmag", "plx", "e_plx" ]
      opts = [ Parse (map addFormat cols) ]

  in dataFromUrl "https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/data/gaia-aa-616-a10-table1a.no-header.tsv" opts


{-|

One question would be how the parallaxes vary by cluster: as parallax is measuring distance,
then are the clusters similar distances away from us, or are there a range of values? A
first look is to use another \"channel\" to represent (i.e. encode) the cluster:

<<images/vl/stripplotwithcolor.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAXAlgY2QGnCgBNZ1ZUxQIJIBXeAGwsgAt10AHAZxQHpf4sAO4A6AOaZ0LWgCNaXAKbxsAewB26BRpGrovACIraYgEL1EC3iwBuCsbF5wum+Fdv3eJMr3uZYAWlgAgDYARmDA0IAGfzIZBgVQ2BE1FX8WBVgiJRF0LmtIAmooADMVeDh0CmBIDgRFasgAcTgxZjVaaBklQqgOBgAPds7u+F7IBQB9fqG0SA6unoBfJcIloqgAEi5sDLhmNk4efndkiSlZEUwVXh29h1P-BklLawBmEQArLnVCwgm1KoiJg1G00FRqJBVAxyo0SpgFAwiMwAMIMeQucboACeHAU7RU0BBsCY60INFmlHJkPhiORcxmf2KNBxeOYAEdaLANJJSJhbEzmZBYANMFxGlh0AlmAAFBAkhgisAACicAEpIKtilqwGSaKo1PCwZRVksgA Open this visualization in the Vega Editor>

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


-- $stripplot-mmtype
-- It is instructive to see what happens if we change the mark type for
-- the color encoding from 'Nominal' to 'Ordinal'.

{-|

<<images/vl/stripplotwithcolorordinal.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAXAlgY2QGnCgBNZ1ZUxQIJIBXeAGwsgAt10AHAZxQHpf4sAO4A6AOaZ0LWgCNaXAKbxsAewB26BRpGrovACIraYgEL1EC3iwBuCsbF5wum+Fdv3eJMr3uZYAWlgAgDYARmDA0IAGfzIZBgVQ2BE1FX8WBVgiJRF0LmtIAmooADMVeDh0CmBIDgRFasgAcTgxZjVaaBklQqgOBgAPds7u+F7IBQB9fqG0SA6unoBfJcIloqgAEi5sDLhmNk4efndkiSlZEUwVXh29h1P-BklLawBmEQArLnVCwgm1KoiJg1G00FRqJBVAxyo0SpgFAwiMwAMIMeQucboACeHAUzHKwLUsCY60INFmlHJkPhiORcxmf2KNBxeOYAEdaLANJJSJhbEzmZBYANMFxGlh0AlmAAFBAkhgisAACicAEpIKtilqwGSaKo1PCwZRVksgA Open this visualization in the Vega Editor>

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
variables just by adding a second 'position' declaration:

<<images/vl/parallaxbreakdown.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAXAlgY2QGnCgBNZ1ZUxQIJIBXeAGwsgAt10AHAZxQHpf4sAO4A6AOaZ0LWgCNaXAKbxsAewB26BRpGrovACIraYgEL1EC3iwBuCsbF5wum+Fdv3eJMr3uZYAWlgAgDYARmDA0IAGfzIZBgVQ2BE1FX8WBVgiJRF0LmtIAmooADMVeDh0CmBIDgRFasgAcTgxZjVaaBklQqgOBgAPds7u+F7IBQB9fqG0SA6unoBfJcIloqgAEi5sDLhmNk4efndkiSlZEUwVXh29h1P-BklLawBmEQArLnVCwgm1KoiJg1G00FRqJBVAxyo0SpgFAwiMwAMIMeQucboACeHAU7RU0BBsCY60INFmlHJkPhiORcxmf2KNBxeOYAEdaLANJJSJhbEzmZBYANMFxGlh0AlmAAFBAkhgisAACicAEpIKtimTipBsXCEUjUejnD08Bhcfi5qkiWoSZq1hsoep4WDKKslkA Open this visualization in the Vega Editor>

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
simpleHistogram field =
  let enc = encoding
              . position X [ PName field, PmType Quantitative, 'PBin' [] ]
              . position Y [ 'PAggregate' 'Count', PmType Quantitative ]

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAjBkA04oBNYAXWVMUCCSAV3gBszIALIogBwGcUB6b+WAO4A6AOYBLIk2rpqHAKbwAxgHsAdkTnqhK6NwAiy6iIBCtRHO5MAbnJGxucDhviWbd7oRLc7Y2AFpYfwA2AEYggJCABj8SdDo5ENghVWU-JjlYfAUhIg4rHDwqADNleDgiMmBINgR5SsgAcTgRRlVqaHQFHCg2OgAPVvbO+G7IOQB9XoG0SDaOroBfBbwF3CoAEg5FdLhGFnYuXjck8UlpITFlbi2d+2O-OgkLKwBmIQArDjUCqk0VfDEqhaaAolEg03IkCKYjkdHwjCmo3QgLIRHg1Dk2CgRAAnmw5IwAI7UWDqCTEMQ2SCrQpQHH1WAiETwWzEAkzFTUdSjXH4okkskkIiUgnLCA0qgqVTQ4HkZYLIA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAjBkA04oBNYAXWVMUCCSAV3gBszIALIogBwGcUB6b+WAO4A6AOYBLIk2rpqHAKbwAxgHsAdkTnqhK6NwAiy6iIBCtRHO5MAbnJGxucDhviWbd7oRLc7Y2AFpYfwA2AEYggJCABj8SdDo5ENghVWU-JjlYfAUhIg4rHDwqADNleDgiMmBINgR5SsgAcTgRRlVqaHQFHCg2OgAPVvbO+G7IOQB9XoG0SDaOroBfBbwF3CoAEg5FdLhGFnYuXjck8UlpITFlbi2d+2O-OgkLKwBmIQArDjUCqk0VfDEqhaaAolEg03IkCKYjkdHwjCasBa2AwgLIRHg1DkKMgRAAnmw5IwAI7UWDqCTEMQ2SCrQpQPH1JEieC2YhEmYqajqUb4wkkskUkhEalE5YQOlUFSqaHA8jLBZAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFALYCGATgNYZjCQDOK5A9pbDZIuaQJ6T5QAZvAA2Ijs24A7ROwGQUPAA7tMkAEYVIAX0IRIAE1IpSNUBH0BXcuLWoUS+ugD0z7gHcAdIngpol9Ut6WHIAY2YpFFhIz3DiZwARZktEACFrNmdoADdYRFJnMkYQrNz85yMTZ3z4UgBaUnqANgBGJoaWnDqTdRFYFtJPKWY6uFIDEM8Uemz+In1BSTI0TDolCmCzSABxMkQOKUtidRD+KCURAA8Do5PyM8hYAH0L67VD49PtbSJdIkgACT0UJwMgceyOFzOMqDHx+AKeeDMZzA0EFGF1ES+WDQrCeABW9Aic30KF8fQ421ItTAAAUKKQxKRLrB6CSoNFwgZ4DIzPMoG9aPyFvBYCIDBxXuyLFB1DyaExLLA9DKoIoVBwAI6WUiRXzGeC5aUWSDM+Bs1YKcmqKD07hMy5gAAURQAlDp+X8ZZA+KthVBgYybXR1TbICJmPsvarTYguHljGHwpZIsbScow9rdWSTGSjZ7firIOEpMJ9qsftogA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFALYCGATgNYZQBGFkhEkAJqSqTaBMwK7kAbGjBQoADgGd0AemnlSAdwB0ieCmi9avCbHIBjAPYA7FLBNLDxaQBEDvRACF+lWNOgA3WIlLSyE0+Runt7SbBzS3vCkALSkMQBsAIzxsYk40Ry0ArCJpEpGBtFwpCy6SigS7oxEzABmBuRkaJjAkGIUOlyQAOJkiMJGvMS0uoxQYgIAHgNDI+RjkLAA+hPTmJCDw6MAvttE20xQCvAs6jQALHiHkAAkEnpwZMKo4lKywXmq6ppK8AbS90ePg+0QEalc7iwSgAVhJjNVmGZDCx4EZ+i0alBDAIGl1avBYAIWMIAMICbQBBYoACeYlgAwMxFRpCEB0xkDWYG4PDqBKJwl6pH6hx5kFoqK6-lgYhoiTZPOYNLpwgAjrxSCY1Ox4J4EQrIKRJvAJF13CzeLATZgANoEMAAVnwYDSTsSjrAACYcABdPY8+U8SDUrpCxDkLzsenrQy8ExU2lRqBqjUoLWp3V+sAByCGIz49FcvbbIA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PName \"Gmag\", PmType Quantitative, binning, axis ]
            . position Y [ PAggregate Count, PmType Quantitative ]
            . color [ MName \"Cluster\", MmType Nominal ]

    binning = PBin [ 'Step' 1 ]
    axis = PAxis [ 'AxValues' ('Numbers' [ 0, 5 .. 20 ]) ]

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFALYCGATgNYZQA28AdrJIRJACakqk2gRsBXcrRowUKAA4BndAHpZ5UgHcAdIngpoAgEYCpscgGMA9gxSwzKk8VkARYwMQAhIZVizoAN1iJSsslLm5B7evrKc3LK+8KQAtKRxAGwAjInxyTix3Nq0sMmkKgzGsXCk7AYqKFKeLERsAGbG5GRomMCQEhT6vJAA4mSIogwCxNoGLFAStAAeQyNj5BOQsAD6U7OYkMOj4wC+u0S7rFBK8OyaNAAseMeQACRShnBkoqiSMvKhBeqaOirwxlkj2efi+sXo5lkniwKgAVlJTLU2BYTOxGIM2nUoCZaE0evV4LBaOxRABhWh6IJLFAATwkzE2RWIjFIIiOWMgGzAfH4DUJxNE-VIg2OvMg2kYPUCsAkNGS7N5bFp9NEAEcBKQzBouPBvEjFZBSNN4FIep5WQJYKbMABtAhgACs+DAGWdySdYAATDgALoHXkK-iQGk9YWIcg+LgM7EOMzUunRyDqzUobWpvX+sCByAmBgEjHcg67IA Open this visualization in the Vega Editor>

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
-- You can also syntehsize new data based on existing data, with the
-- 'transform' operation.

{-|
The aim for this visualization is to show the spread in the @Gmag@ field
for each cluster, so we now swap the axis on which the aggregate is
being applied (so that the cluster names appear on the y axis),
and hide the legend that is applied (using @'MLegend' []@) since
we can read off the color mapping from the y axis.

<<images/vl/yhistogram.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuMxIHsAOsDGAlgC4CeqYADAHQBsANFKdgKbmQBGCkAvneFABNYRWOVAQIkAK7wANmwAWRItgDOKAPQb4sAO5UA5sQVT2U1c3h5MAOyLM7Va9A0ARTFIMAhGYmYaFADdmA1gNOFV7eADg0I0hEQ1QglgAWlg0mgBGGnSsilSRdllmLNgqG0xUhWZYAUsqIlVAyD4JKAAzTHg4IjFIXHgLfoBxOAM2GylodktWqGxZAA9J6dn4echmAH1FlbRIKZm57m5+Xn5IABJVPBq4RWU1TQ1Y8qMiE3YqAkwNW-uYTeqVkxH8gQAzFQAFaqWytS4OawCAg2CZocQSSDWWTdfodAjMWQCNgAYVk5iimyYrAOlWgqNg8gYkBKBgcJLQU1ksgu7Ug+3QkAJRM5UDGsAmLMlBngIWEtKgkXqLRZNLYAEcpLA7MRhARgjw2liyBjhYTiWSKZE5mqSCxJpgGTYmTxzsbsbYCej0GduEA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxhIBzWdAVwAcAjATz0MgGEAba5aAU1kgBdADRR45Sr3Lw+LAmXS0WkAMY1E0SKMjxkytdQ2QAvoOPERxSFniwA1i1AkoPKnd4B1AJYATaAAsWAEZhYghIZC8AL15ggAY40OcIuHR3ZXoOeBUHJJJIVC8ODmVGXmL0AHctMJd-eFpY-EgAWTBEgFogsAAZdoA6ACYAZmEOuKHh3rAQ8cnpieGADmEJwe6+iYA2QdX+gE4AFgW9w-3puZ29o4vFlbWNsC6xtam+uZGX+c2x7qia5LQRiNZS0dBeIzEYx5SA+GTwRy1SDUWAlZr+aDQWjIXAAelxCEq-XIXgC1Ho3H4ag0vA0-TUWFxABEaOQAEIo9y4-wANyk8FxNh4-G5fOkuLh0AF0i88A68DlWyCW3lQTiHSlmV4QXg-UQ6A6-l48B8-H60GQPIB+Qw2BkjkgtFsyCapEgAHEbORlIhqFh6PwtFBaBwAB4+v0BgTaXgAfRD4eavv9geM5gg0KsABJkCojTZlBisTj8WLdSSyfR+l50Ljc-mBWWOhxSbxcTzDv0AFbIdCIa2QWlqHwQ734Jz5RNuwrlHzKTjcPjRlyYTTjweIYej0ombRAkFJ9BYCHwEqZ5LMce1cIzjhz5oGIx5QHA12QACO1CQ0FJMi8fOtZJ4FDLw9HXH9oA4N8ADlI34MB0FQMAeGdExanTMBzygalCjHUhzGMIA Open this visualization in the Vega Editor>

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

- and when using 'Count' with 'opAs', the first 'FieldName' argument is ignored,
so I set it to the empty string @\"\"@ (it's be great if the API were such
we didn't have to write dummy arguments, but at present @hvega@
doesn't provide this level of safety).

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxQSTIBzWdAVwAcAjATz0MgGEAbW5aAU1iQAugBpi5SPErV+leALZFy5YJHT02kAMZ1E0SKKjxkWxLSyNBkAL7iVJNRq1Z+AEwCWSQ8dP5I9FwAHgD6rm4+kKge-FwR-oFBthIkwik2xGLEkFjwsADWbGQUfDT5-ADqHm7QABZsAIz2JR4AXvyNAAydzRCQpejlWoxc8NqFvVDRXFxazLFc6ADuhin9tfD0Hf4AsmA9ALQNYAAy+wB0AEwAzKIHnVfXp2BN94-PD9cAHKIPl8dnB4ANkuv3OAE4ACwfMGQ8HPN4gsFQhGfH5-AFgI53P5PM5vG4496Au7HVqrSTQZhbLT0dAefSQYh2bJuBTwIprWiwWb+WrQaD0ZC4AD0IoQS3OlA8dVojF4gl0+n4+nOuiwIoAInRKAAhbnlEW1ABucngItyfEERtN8hFbOg5vkXgO8HgByBDSBroanQOjpG-Aa8HOiHQB1q-HgbkE52gyGNFIoGGwCiKATyyG2pEgAHFcpQzBYrEIjAFgkXLNYy-wQolKyXbBkICy+gASZDaSO5LT8wXCsW2kPS2WMc4edAizvd81Dg5cGX8EXGyHnABWyHQiCTkBVuk8iEL+GKfSSx5SfWisXiUG4vAEpYvUE3sAMx-Umn8uSSZavcVpwRhO4kSYDGQj+DGnYqgehatioMDUtmkBhlgDLwLM6STJArDng4UQxP+-jmFWj54VSNL+AAjrQSDQDKCgeKaSaSPAQQeH4OZ0dAXBIQAcsWghgOgqBgHwmbJOQzZgHBOhbtER6kBkNhAA Open this visualization in the Vega Editor>

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
<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxQSTIBzWdAVwAcAjATz0MgGEAbW5aAU1iQAugBpi5SPErV+leALZFy5YJHT02kAMZ1E0SKKjxkWxLSyNBkAL7iVJNRq1Z+AEwCWSQ8dP5I9FwAHgD6rm4+kKge-FwR-oFBthIkwik2xGLEkFjwsADWbGQUfDT5-ADqHm7QABZsAIz2JR4AXvyNAAydzRCQpejlWoxc8NqFvVDRXFxazLFc6ADuhin9tfD0Hf4AsmA9ALQNYAAy+wB0AEwAzKIHnVfXp2BN94-PD9cAHKIPl8dnB4ANkuv3OAE4ACwfMGQ8HPN4gsFQhGfH5-AFgI53P5PM5vG4496Au7HVqrSTQZhbLT0dAefSQYh2bJuBTwIprWiwWb+WrQaD0ZC4AD0IoQS3OlA8dVojF4gl0+n4+nOuiwIoAInRKAAhbnlEW1ABucngItyfEERtN8hFbOg5vkXgO8HgByBDSBroanQOjpG-Aa8HOiHQB1q-HgbkE52gyGNFIoGGwCiKATyyG2pEgAHFcpQzBYrEIjAFgkXLNYy-wQolKyXbBkICy+gASZDaSO5LT8wXCsW2kPS2WMc4edAizvd81Dg5cGX8EXGyHnABWyHQiCTkBVuk8iEL+GKfSSx5SfWisXiUG4vAEpYvUE3sAMx-Umn8uSSZavcVpwRhO4kSYDGQj+DGnYqgehatioMDUtmkBhlgDLwLM6STJArDng4UQxP+-jmFWj54VSNL+AAjrQSDQDKCgeKaSaSPAQQeH4OZ0dAXBIQAcsWghgOgqBgHwmbJOQzZgHBOhbtER6kBkNhAA Vega Editor>
rather useful when creating new data columns or structures,
as you can actually see what has been created (I find Firefox works much
better than Chrome here);

- the use of 'ByFieldOp' here is a bit un-settling, as you need to
give it an aggregation-style operation to apply to the data field,
but in this case we have already done this with 'opAs' (so I pick
'Max' as we just need something that copies the value over).

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hIATAUxQEtoBPPKABwBsAPSAXwF0AacKLeLADW9UBAiR0DeAGNqdfAAYAdAHZe4qMjjohZepABGTWSI3jI22LrIAReMgAW9AgCZuYACw8+E2g318SEEyeEg+dnNSeGgw-DELAFdYJgNHaGgGZFwAehyEAHdlAHNqR0TDROQyWBl0RGgKaGU6rBzbdETigCFkvRzHADcyYvgcgW0ageHRnJIYsdHKeABaeFWANgBGDbWtxRXY4zIt+GVEdBXHUPJYZWhkQchzCQxsGNFIaVhqz4BxATFAyIRJYQw1Z6MVjA0Hg2CQyBkAD6zDYQRBYIh7HYESiABJkDJrgI0hksrkcjMzqVoOVDMpKOgcoTiWMqSsmNQyJTPMoAFbIerPPiIxB1EiURBA+K+KBosAJTSQVCUMhMEgGQbwJiJfQvCz+QJQACOiSQ0GoMUow2Fmgk8BYlGQnwt0CYRq+gm1JjYOM0kVlkAUCuVqvVBnIVFoCMNBlN5stFptfrAAYkdUQKulCpx7CAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxQSTIBzWdAVwAcAjATz0MgGEAbW5aAU1iQAugBpi5SPGRsCkAB4BrACb9IoqMxVqxEigGM6iaDPxxa-ceQiRVKAJbRW+SPS7zIegL7FdNrPCwimzAkOj08PqOzmAADAB0AOwaMMz0ai6B-PCQXlZQyvDQOfhkFLSwXGyQABbQ0PTIuAD0zQgA7vGUjjW0jLyChsb8xvGGWM0AInSUAEIVivzNNQBu-JTwzQF8gstrG82Fxc0b9vAAtPAXAGwAjNeXt7HnxYxc-Lfw8Yjo5zXZqlg8RMK3UekgGGwRRCrkCyAypEgAHEApRqohaFhGIJ1FA3B4XBisTiUvwAPr49GY7FCLw+CB5YiQAAkyH0-wC1TqDSarX2X260F6jHi9nQzTZHM2-POXEcSxWABZ4gArZDoRBgmwjQzKeyINGlcGGLiYGGoez8LjKarcXgCIQpJzpdHoLD6+BVRmSAmkPQ2C1Wm0uJSqLXWVIulwAR1oSGgjiK9jW4ck8Hk9lMiIT0He1QACoFPVx07lvPkbDFQoHrdUtGGnWkEZBY-HEwmUyl05mYTm8y4OEYTLl6WBvVAhhbDaQfF4gA Open this visualization in the Vega Editor>

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
appears that @'DnCounts' True@ option is interpreted as
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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMAWADDgNOFALYCGATgNYZQAOA9vAHZqESQAmpKpNoE7AK7kANjRgoUtAM7oA9HPKkA7gDpE8FNEEAjQdNjkAxvRawWqk8TkAReoMQAhYZVhzoAN1iJScstJRDdy8fOS4eOR94UgBaUliANgBGBLiknBieHRFYJNJVJnoYuFIOQ1UUaQ9INgFIADN6cjI0TGBIWgoDPkgAcTJEcSZBYh1DGroRAA8hkbHyCchYAH1aadnR8YBfLaIt2shleA4tGlwCIkgAEmkjODJxVClZBRD8jS1dVXh6OVv73xvGIiTRuDwAZlUACtpKYapdzCYOMxBm0iOwTCImj16vBYCIOOIAMIifSBBb4KAoACetFgQ3oxGYpDE+3RUGk0FIdJxeIJxNJAXGlMgNJ5mEghSZTBZkDZAigMzaDT5hIl-VIgxFYvpEoAjoJSCxNNx4F45bV2NS+Oz2Lj8WrJjNLXVbizdWB2hxGaRmDQANqQgCslPBACYALra2keyBYrVQJjwIwe+osgzyhVUmPiA1GlAmgvm9m7CCZyAmJi41Ge3ZbIA Open this visualization in the Vega Editor>

@
let enc = encoding
                    . position X [ PName \"Gmag\", PmType Quantitative ]
                    . position Y [ PName \"plx\", PmType Quantitative, PScale scaleOpts ]
                    . color cluster
                    . shape cluster

    scaleOpts = [ SType ScLog, 'SDomain' ('DNumbers' [3.5, 32]), 'SNice' ('IsNice' False) ]
    cluster = [ MName \"Cluster\", MmType Nominal ]

in toVegaLite [ gaiaData
              , mark Point []
              , enc []
              , width 400
              , height 400
              ]
@

We can see that each cluster appears to have a separate parallax
value, and that it doesn't really vary with Gmag. What this is telling
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
in. You can see that we also have one cluster that straddles the
0 and 360 degrees Right Ascension barrier, which will lead to some
fun later.

<<images/vl/posplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFALYCGATgNYZQDG85tANrJIRJACakqk2gQOAV3JMaMFCgAOAZ3QB6eeVIB3AHSJ4KaEIBGQmbEYB7AHYpY5tbWPF5AEWNDEAIRGVY86ADdYiUvJkMhbkXr7+8ty88v7wpAC0pAkAbACMyYmpOPG8uiyppGqmxvFwpJxGaigy3mxEHABmxuRkaJjAkFIUhvyQAOJkiOKmQsS6RmxQUkwAHsOj4+STkLAA+tNzmJAjYxMAvntEe+xQKvCc2jQALHgnkAAkMrRwZOKo0nKK4YWa2npq8GM8ieLwC33iTC0nm8WDUACsZGY6hxLDZOPBTEN2vU6MYmM1eg14LAmJxxABhJgGELLFAATykrC2xWIGNIYmOOMgmzAAkEjWJpPEACUAIKrACS5OFAGVkfyoE92UzeVxbKQMTQANoEbDJHAAXXwUFM8FoKoa7MMnIViuabSgFSelnRmPl-Mg9MZ4gAjkJSOYtDx4L53YJIKQZvAZL0UFoWCKkKgwKLnaYZIDTGAABQVRAASkgh35NvDdP4OIFJLJW3sAFFJdK5ScPUqE+01WRNZgtfEAJy6gdGk1mi1W2Clj1elWQP0BuO8OOhlvhyPR2Pxmf2WDMNlxsw5vOF4uCE+lyA2UxErG8w57IA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxQSTJ5k8oAbdRAcwEtoBXAEwFNIAaY8pADG8WkLa140HvkgcpbLADoASgEEA+gEkAwioDKYAHxgAjAA4ADGAD8Yee2XrtewwFowAZgBs1-A8VVTV0DSAEwAF9iAF1+CEgACy5mRgToGgAWS0s4qCx4WABrGmFmWCFaHly5KXgaMgo2WFoStOgAB2RcAHpuhAB3JRZoBLYAIzZkLnKGaURoJSF0LG6AEXQ2RgAhJsKuboSANy5GeG785GlYA+PT7ocz0+Z4N3gX71NvV9NLN2h4MaVUzwJSIdBuJLwbiwJTQZCHPjhSAYbBSeqQdoFKbogDi+UYJUQijG0z4UHatAAHoTiaTeFAuBoKdTZESsCTYJAIlEIBFqv1mBwRjRPABWHLESAAEmQQiS+Va0A6XV6txBw1GYyUzHQ3Vl8rOarctFY+0OGSUACtkAxEfF2rB0JauEJoDrEOjoABPdoyPLTETQTBc6pcRBLDjMJj1JFLeic-ANQSoZhcWgcErMu3kKCy0R+4CQfVcLB+yCHMqC5jUekwH1l+gEvnheLe30lACObCQbv+buO2cElUYYYziZgrEqmYKokk1J5JGbQ6krE4BeRqfTJVWAFEXKFa22y12e6wVwOlxQg+haG72uiU2mx1AdLRJlcyXX26zllHRCGkXoJhV24B9N2fSAgOGNdPyPTtu3mM9+x4BdL2EBgUwJRMogiIA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAjBkA04oBNYAXWVMUCCSAV3gBszIALIogBwGcUB6b+WAO4A6AOYBLIk2rpqHAKbwAxgHsAdkTnqhK6NwAiy6iIBCtRHO5MAbnJGxucDhviWbd7oRLc7Y2AFpYfwA2AEYggJCABj8SdDo5ENghVWU-JjlYfAUhIg4rHDwqADNleDgiMmBINgR5SsgAcTgRRlVqaHQFHCg2OgAPVvbO+G7IOQB9XoG0SDaOroBfBbwF3CoAEg5FdLhGFnYuXjck8UlpITFlbi2d+2O-OgkLKwBmIQArDjUCqk0VfDEqhaaAolEgKjopXqRTEcjo+EYAGE6LJnKMiABPNhyVrKaCA2AMbBQeIiTQItBtOh0VaFKDTciQGFwilQJqwFrEyDoQFkIjwahyLmY7GMACO1Fg6gkxDENkgtMoUAh7VU0Nh8KRKKcXWFWJxMxS+NUhIVazBGMqdKoHJE8FsxANysM6h+SqgIqdkAlUqIMr98vNSsgsD6Yg49T9RHijAAckMFGBlEUwABlEjwCPLJXZsCK52qGHA8jLBZAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCEwJBksDgCBZAA4iYshJBiWLLcgqlQZHgAHrX1jc0pUPwA+m2dqHUNTTiQAL5T6VNpmBRk-EIF6ZgmOBq13CWp6-1iotBsYjWo6BiYong55Zls-HjQtQDCeHzmLZDwAJ7LXREslOsAIfUgwRYUheqHqeDw8wOmBGaCyj2etSqsBq4O4p0I8BwWH44L+ANGAEcsLAJBwEGx9NMFlcoL81iyMtiWDgDAh+LVRFgJPsOZgyfzKdTaYh4Az+cyOZBYO02BRyrL4MFagA5caCMAiTJgADKiFK0yRYFmV2tVuZkAAJBQhNETLUSOQqLQrGF2JweEw2CJqM7XUYfU48BxtLoAMxMABWFHEIqgNwaYjVqAALPbMrAhPx8hc0U8YVB3p9elBxYDgWJQUz0pBRGIHuc0LMpkA Open this visualization in the Vega Editor>

Note that Vega Lite does support a @\"facet\"@ field in its encodings,
but hvega follows Elm VegaLite and requires you to use this
<https://github.com/gicentre/elm-vegalite/issues/5#issuecomment-514501218 wrapped facet> approach.

I chose 4 columns rather than 3 here to show how "empty" plots
are encoded. You can see how a 3-column version looks in the
nex plot, 'densityMultiples'.

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

Perhaps the most important thing in this example is that I haveThis is


used a sensible number of columns (ending up in a three by three grid)!
The other significant changes to 'smallMultiples2' is that I have
used the 'FHeader' option to control how the facet headers
are displayed: the title (which in this case was @\"Cluster\"@)
has been hidden, and the per-plot labels made larger, but moved
down so that they lie within each plot. I am not 100% convinced
this is an intended use of 'HLabelPadding', but it seems to work!

<<images/vl/densitymultiples.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCEwJBksDgCBZAA4iYshJBiWLLcgqlQZHgAHrX1jc0pUPwA+m2dqHUNTTiQAL5T6VNpmBRk-EIF6Zjw9GIU2bmEANrrGOgYp5AsOMpk3ACeB5AAwnh85pAAugunGab37RpxLUgN3+-Heny+UFEWAkFEImyw-HBX0g-HakgkB18fQAzL4PkcznFthw7qNhpACWBZqd8WcTDgNLUSjFUkcUWJRNA2GIaqgTmdRHgcuVMmx+HhoLUni9elB4Ddll0RLJubACH1IMEWFJJah6ng8PNKZARmhKZhReLdVA-gCkWd5YrRgBHLCwCQcBBsfSsiGYWDtNiwvmQeAcYK1AAKJTVeAD00pRohQLWfqgloltWBdvNcoVoJdbo9iDDPvt30DwbQofDBagD2UMITEOpGFbSagABIKEJoiZaiRyFRaFYwuxODwmGwRNQe32jKOnHgONpdAAWJgAKwo4l9kJEz1k20I2M+WVgQn4+T5bIz1sezzMvTZ0VizRvybjTTwAEEOURhVGHU9zOL9xWjaAuR5QgnAAJjXctNVgb8ADFxHgABlNgAC86y8RCw3gCM9SwA1ELA39lxYMRahwNgWBIClTg7DZ8yVFUxDVJiqTPUQxFFXk0FmKYgA Open this visualization in the Vega Editor>

@
let trans = transform
            . density "plx" [ DnAs \"xkde\" \"ykde\"
                            , DnGroupBy [ \"Cluster\" ]
                            , DnCounts True
                            , 'DnExtent' 0 30
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
                  , MLegend []
                  ]

    headerOpts = [ 'HLabelFontSize' 16
                 , 'HLabelAlign' 'AlignRight'
                 , 'HLabelAnchor' 'AEnd'
                 , 'HLabelPadding' (-24)
                 , 'HNoTitle'
                 ]

in toVegaLite
     [ gaiaData
     , columns 3
     , facetFlow [ FName \"Cluster"
                 , FmType Nominal
                 , 'FHeader' headerOpts
                 ]
     , specification (asSpec [ enc [], trans [], mark Area [ ] ])
     ]
@

-}

densityMultiples :: VegaLite
densityMultiples =
  let trans = transform
              . density "plx" [ DnAs "xkde" "ykde"
                              , DnGroupBy [ "Cluster" ]
                              , DnCounts True
                              , DnExtent 0 30
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
                    , MLegend []
                    ]

      headerOpts = [ HLabelFontSize 16
                   , HLabelAlign AlignRight
                   , HLabelAnchor AEnd
                   , HLabelPadding (-24)
                   , HNoTitle
                   ]

  in toVegaLite
       [ gaiaData
       , columns 3
       , facetFlow [ FName "Cluster"
                   , FmType Nominal
                   , FHeader headerOpts
                   ]
       , specification (asSpec [ enc [], trans [], mark Area [ ] ])
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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEC2CGBOBrSAuKAjBkA04oBNYAXWVMUCCSAV3gBszIALIogBwGcUB6b+WAO4A6AOYBLIk2rpqHAKbwAxgHsAdkTnqhK6NwAiy6iIBCtRHO5MAbnJGxucDhviWbd7oRLc7Y2AFpYfwA2AEYggJCABj8SdDo5ENghVWU-JjlYfAUhIg4rHDwqADNleDgiMmBINgR5SsgAcTgRRlVqaHQFHCg2OgAPVvbO+G7IOQB9XoG0SDaOroBfBbwF3CoAEg5FdLhGFnYuXjck8UlpITFlbi2d+2O-OgkLKwBmIQArDjUCqk0VfDEqhaaAolEg03IkCKYjkdHwjAAwnRZM5RkQAJ5sOStZTQQGwBirQpQdH1aGw+EzKajWAiETwWzEbEzaByAGwVRozHMqAAR2oHKIEmIYhskGWECJVBUqmhwPIywWQA Open this visualization in the Vega Editor>

@
let plx = position Y [ PName \"plx\", PmType Quantitative, PAggregate 'Median' ]
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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCEwJBksDgCBZAA4iYshJBiWLLcgqlQZHgAHrX1jc0pUPwA+m2dqHUNTTiQAL5T6VNpmAAkFELRJrUk5FS0VmHsnDxMbCLUK2tGu054HNq6AMxMAFYU4qnpkHiwAJ7NqADa6QghRMOA0tW4JRakCkomgbDENVQhRGaCybH4eGgtQAwng+OYofAvmR+F0RLJ4bACDN5oDUSCwaMcFhgtN0gBdBb9MSw+GItB0yBfcqZdGY2rDKGwFgsHAGBCk0ayfhw2BiQnExVQACOWDV8A4CDY+jZEFpmFEYlF-OAsymQA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFACYCGKpGYoEEkArgE4A2VMKKADgM7oD0fRqQDuAOkTwU0egCN63WIwDGAewB2KWBtGqAtnwAiK+ogBCTANaw+0AG6xEpPrtLdNjG-cd8yFPo-hSAFpSYIA2AEYwkIicIIoZZlgI0lE1FSC4UmJFURRuW0hCWigAMxVGFzRMYEhOUkYFKlqAcRdENjV6XRlFIqhOZgAPTu7exn7IWAB9QZHMSC6evoBfFaIV4qhheGIpKlwCIkgAEm4lOBc2VC5eAS9UiSlZUXgVPnPLpweg5klrWxYUQAK246iKx2YpAAnn1MABtIgQGglKAuRgWZqQbjwABesCoACYCFAUNDOASFkp4MokpBNkjaFM1KpiPA1B0aoySpBoVjSvBYMxiGw5pNSIhEIwHORKWjYGzSGpJmSKWwAI70JUoSTkeD2encsDrEoMkoonnozELRj0OlbHlaVnsznUI10aGE-mC4Wi4biyXSxyaNguEZm1Ee71CkULMX4KASqUykMLXTslXkuWQTXa3U6g0m1FFiBFgC6W2Zzo5WPm1EgApjbAAwsx5O5M2qFul02pSKw1pXVGoBa7gOsVkA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMAWADDgNOFAO7wAmK0GYAzAKwFGQAkAzgMZwC2AhtTChQAHVugD0YgG6xEPAHSJ4lAK4AjOfAD2Yjtx5SZPALQAbJbClY5AK1aaAdpEIRIQgE6brsdii2PMwJAoAJ5CsPxcsG7sPCiabpAAvs5QJjzBUdQA2kQQoBAFULxuANbU+YUFkKwoHiWwAOrklNQ4cnQplVA1deGYkIhusMFOuZXVtZr1APJCPOxKI5htHWOFQaF9UIiwmqzQPGGQa2DJJ5BksXwBA26x8OzKJluBNbBC2QBsBGAAjHQAXUSiTGZ0KgWKZX6Oz2ByO+CglxQ1zAgUG90ez3KE3e2V+AA4fgBOHBA4GdVHnWo8eysABm8S42ROeRZVR4rH4JgcihQyjI4QpXUgMRMmNiWwusWUXDkACUAIIAfQAkgBhOUAZTAAD4-oSwAB+MBImXy5XqrVgIy0b5gTCm2WK1UazXHLoQEFdAFCoo8Ur8BbRLG+qXI8psyDKNwmfioYSiCR3YgKJTQNTKVhRdgOFCwewoOQ5rhiAAimmUiAAQtH6mJoNJZGJeG83PXG-okfpZPBjDxjJ9fp8jDxfjgjMjVM9fvJ7JojHAeAK3HIUKxJKMPVAGW5eGgAmyXHM3FnsQBxXiIfj2GWqTII1wmAAe19v96gsCVQmfr64d4SXoeoBhRguM+Y5mQ8D2FeB5biKmjcgksFbtu8CwCYZD8N+L6+usHA8FiNy6LAkT8JI8BuOQ8Ccg+IRHP03JXqBcF0ZKACOyg0r4yK+NIm4oZAzw7PYmE3NxhFQMeBFpC+wGVMx4xpNx-IvJAdJoRh-ClgAoi6WpOFArH8BxXFKPcfEKescQIb4Hw3Op6GiVAaomJmeYJLRmzXpoXBQQRSS4akPJKCp2IOZpDHBXyAoGRs9FQCZBZmbx4RyZ6oIUhU6yQtiZBLGAHRQARSD+FA7D5u5sVGf0eZPmgD6qBy6FQZKcQfJZiJXBGcGSARyiwJymA5ChqKCVO-BDlQD4vpgRi-AATD8kD5TgHXgmNsb9ItU1QDN1rfA+K1rQUgQmON-RYDtkB7QdUBHYFeQbfw+JXXtC1LUdbIAicHWQOBmiQdB3UekEsB1WFGlOU9nnxZAc6+fY-nHYJ9zRap4VQyMMPsZxSU8fAFmBYJUWhfZkP8Dhhlef0iXceZqU-RlYxZVUOXIcKZB7UYqxwcViClZAlHIPVkbVYZYMi3BjVZmY9iSr5ZBkFiP2hl2wPCr1rkDcyI2nedUBGFgdCxXtH3UIbdDHY9Z2bVARsm60h3UEbaUFN9XS-f9gMwZSLESxDjlcuN2PeQjSNE0pIUxWTgf9FjVOw7TyUE6lEck9Ho0YxTVXUwluN0ylSSMyBRA+kwOb2OpPvACCiRAA Open this visualization in the Vega Editor>

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

    raData = 'dataFromColumns' []
                 . 'dataColumn' "x" (Numbers [ (-120), (-60), 60, 120 ])
                 . dataColumn "y" (Numbers [ 0, 0, 0, 0 ])
                 . dataColumn "lbl" ('Strings' [ "16h", "20h", "4h", "8h" ])

    decData = dataFromColumns []
                 . dataColumn "x" (Numbers [ 0, 0 ])
                 . dataColumn "y" (Numbers [ (-45), 45 ])
                 . dataColumn "lbl" (Strings [ "-45", "45" ])

    encLabels = encoding
                . position Longitude (axOpts "x")
                . position Latitude (axOpts "y")
                . text [ TName "lbl", TmType Nominal ]

    raLabels = asSpec [ raData []
                      , encLabels []
                      , mark 'Text' [ 'MAlign' 'AlignCenter'
                                  , 'MBaseline' 'AlignTop'
                                  , 'MdY' 5
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
bottom and top of the plot.

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
                     (Numbers [ (-120), (-60), 60, 120 ])
                   . dataColumn "y"
                     (Numbers [ 0, 0, 0, 0 ])
                   . dataColumn "lbl"
                     (Strings [ "16h", "20h", "4h", "8h" ])

      decData = dataFromColumns []
                   . dataColumn "x"
                     (Numbers [ 0, 0 ])
                   . dataColumn "y"
                     (Numbers [ (-45), 45 ])
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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCEwJBksDgCBZAA4iYshJBiWLLcgqlQZHgAHrX1jc0pUPwA+m2dqHUNTTiQAL5T6VNpmLqiYkIIhADa6RDoGJgmOBq13CWpWxlSotBsYjWoO7uYI2hZbPx40LUAwnh85i2Q8AAnmR+F0RLJrrACPMzrtIIDyplXu9asN-rAWCwcAYEKDRrJ+FdYGJ-kCQbUAI5YYnwDgINj6aawsCzXYw3b3DL7Q6jY6TBYPSAXERXG4FZmPRHIj6jb6-XpQMl4qARCFiKHTAUPKAIu4vN4yqBVDHozHYwySWoEokkvoA4HKyBUml02mM1kPD0QD0AXQFkAAJBQhNETLUSOQqLQrGF2JweEw2CJqMHQ0YY048BxtLoAMxMABWFHEp0wyyRtzQsymQA Open this visualization in the Vega Editor>

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
rather than vertically (and is used in 'combinedPlot').

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCEwJBksDgCBZAA4iYshJBiWLLcgqlQZHgAHrX1jc0pUPwA+m2dqHUNTTiQAL5T6VNpmLqiYkIIhADa6RDoGJgmOBq13CWpWxlSotBsYjWoO7uYI2hZbPx40LUAwnh85i2Q8AAnmR+F0RLJrrACH1ILB2mwKIR6ng8PMzrtIIDyplXu9asN-rAWCwcAYEKDRrJ+FdYGJ-kCQbUAI5YWnwDgINj6abosCzXZo3b3DL7Q6jY6TBYPSAXERXG4FXmPbG4j6jb6-XpQBkUqARCFiKHTKUPKBYu4vN5qqBVImE4mkwySWpUml0mE65msiQc9nc-kPAMQAMAXSlkAAJBQhNETLUSOQqLQrGF2JweEw2CJqNHY0YU048BxtLoAMxMABWFHEp0wFGKQmutzAvnDyxxzeAsymQA Open this visualization in the Vega Editor>

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


-- $intro-repeat
-- Creating the same plot but with a different field is common-enough
-- that Vega-Lite provides the 'repeat' operator.


{-|

The 'concatenatedPlot' example can be extended to view the
distribution of several fields - in this case Right Ascension,
Declination, parallax, and magnitude:

<<images/vl/repeatplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEBOCmAOsCGAXSAuMwYHsDuGYA2pAEoCCA+gJIDCpAypADRQAiAojfU65PABsAHiygBxALZIA5pAC6AX2bgoAZ0QBjQqAgRIU6AGtCkAEZJoLFXoAmqJNuu7IAV2gCTACxQp4q9AD0AdBIeAB00gCWKJ4upi6qsNAaOAB2KLDpYSkSAWw4LtIAQm6GsAGeAG6w0kgBUqoZ0BXVtQF2KHW1kUgAtEh9AGwAjIP9wwAMvZ2mArDDSGGpOL2eyDZJYSiqlVa6zgBmONBSaJg6+87wFonakJIyJqkuEqZJovzCTy9vlnywlEEIkwkGer3eCicEEh+yUTkgmRSNkiqVk5yhUGBWEgB0isAENhMtAECSaHxQAE9EE8cBIUUgPHDLlAKY5mXpcfjCecYAhkGcoNB8JAmezIDJpHBahkTBJYMikKk9mLKdSQQBHFyKlDRVCRaqQDEw3TG0WQAAkqg0aykXh8fkCAVaiyiMTiYUiOACVptdWdvQE0XKlQAzGEAFaqNLKyApVK4tFYSEKIA Open this visualization in the Vega Editor>

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

We can combine repeated rows and columns to create a grid of
views, such as a scatterplot matrix, adding in color
encoding to separate out the clusters:

<<images/vl/splomplot.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEBOCmAOsCGAXSAuMoIRgewHcMwBtSAJQEEB9ASQGFyBlSAGigBEBROxl9yPAA2ADzZQA4gFskAc0gBdVuByQAxniEBXKQDtiZKr2bjI3Y-yjCxA6XMUqAvstwBnRGuLZVM6AGsvSFcASwAvWGIAVgEUAE9EYkE8YN00ZxVcABNUJC8M1S1oIUSACxQUeFd0AHpq6CQCADpZYJQSrQAjLVdYaA1U2FTGjSlqjjwtWQAhQr9YapKAN1hZJGqZVxReheXV6uyUNdXgpABaJDOANgBGS-PrgAZTw46hWGukRt08U5LkTN6jRQrkWbHyuAAZnhoDI0JhvDhVPAkNAeoE7PJMJBdDoOr1TNZEjipHjoKZYNRCVjiaTII5wWB6Yj0ojIIMNJkUpisAz1JpoYEIcFYEJMol6NpNviYvEItS8FIUkhiizEVAxPDIEKRWLNXBEKhEhptHo6TKEliAI5aJCpVqoYLLM282KC4WiwL65BwqDQQhmqBxC1Qa22lD28NOpk4aOqyAAElcaj+MlK5UqNWqu0+LTanUawTw1STKbW2dOQla80WAGZGgArVx4fQuKD9IXc4D0xxAA Open this visualization in the Vega Editor>

@
let enc = encoding
            . position X [ PRepeat 'Column', PmType Quantitative ]
            . position Y [ PRepeat Row, PmType Quantitative ]
            . color [ MName \"Cluster\", MmType Nominal ]

    spec = asSpec [ gaiaData
                  , mark Point [ MSize 5 ]
                  , enc [] ]

    fields = [ \"RA_ICRS\", \"DE_ICRS\", \"plx\", \"Gmag\" ]

in toVegaLite
      [ repeat [ RowFields fields, 'ColumnFields' fields ]
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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjtDbVidgHEtYP1EVkiacygDcgAPU-PL1SgqHhv7-EgzrAvaRUUNIoLFAACTIOKzLRuDxeXw+eoTULhKJsUjoHzgyEVBFSchhQrFADMbAAVshEmtLLFRnFoGjEDsDKQ4gV6DtoABPAxUfTIEzBUZKIEaGBhQX4YjUW5mT4iKg06BgeBgIwmMzPSCYUjJGVQSLoDzoHDCyzJBL0fl2dZQBLkTBWhwOSDFeDkZg8z7BWBUDmUx02xIWukUiXWx0kbXkNmfARu6pPMNO6lUWn0-RMllUNnA-1QTnc05GkyuyCJgGOk1J0gALw9YGILrddYArBqEogg2mJc7Xe66AAWOQa5OpkPXZmsoU5qDoAzwOJhX3dxt9-ByNiEtuBsJd+s9pt0brD+Up4MMz4Zyf-aeQD57sjy6NQI7wE7DuKuuvEWuwdB0VCurEQJ5lydaQAAjswSB0jYdKlEoN5LvekZPoYdxXJA4KfjsP5-vgAHkEBGr5mBkHQWETCkPB5ZgJWkDtmQJwSgCihAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjtDbVidgHEtYP1EVkiacygDcgAPU-PL1SgqHhv7-EgzrAvaRUUNIoLFAACTIOKzLRuDxeXw+eoTULhKJsUjoHzgyEVBFSchhQrFADMbAAVshEmtLLFRnFoGjEDsDKQ4gV6DtEHNYFRqnQ4MwqM8YABPAxUfTIEzBUZKIEaGBhaX4YjUW5mT4iKg06BgDnabnaowmMyCzCkZJqqCRdAedA4WWWZIJeiSuzrKAJciYV0OByQYrwcj8-TBLlCyk+92JZ10ilKt0+khm8hsz4CQPVJ7x33Uqi0+n6Jksqhs4ERqDQEViz5TLAmAOQLMAn327OkABeVbAxH9gc7AFZBQlENH80q-QGg-gACxyQU5vOx67M1ky0tQdAGeBxMJhsc9ydgORsQmDqNhUdd8e9ujdOea3MxhmfQsr-5ryAfS9kTUpqBHeAnHOcQBp2xAdrA6B0KgAaxEC5aVvoACOzBIHSNh0qUSjvruX5Jr+hh3FckDgiBOzgZB+DQeQsGChWopIShphhEwpCYU2YAtpAQ5kCcSoAooQA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjtDbVidgHEtYP1EVkiacygDcgAPU-PL1SgqHhv7-EgzrAvaRUUNIoLFAACTIOKzLRuDxeXw+eoTULhKJsUjoHzgyEVBFSchhQrFADMbAAVshEmtLLFRnFoGjEDtwqRkGBmWASTFoGB4GByPALi58MRoABPAxUfRYZiUUhKIEaGBhUaMqi3MyfESuUioLlxPFxHRgaDoMDUqi0sBGEzLK6QTCkZLqqCRdAedA4eWWZIJegmE5C9ZQBLkTB2QOWYrwcjMCWfYKwKgiykOSwJRC+ukUgMplMkB3kej6ATR6pPcMOSBm2n0-RMllsjnVbm8-lUFzAnNQUXi07ukxRyDlgEpz0V5CkABesbA9hzkEj0enAFYOxW0xma0L51GY3QACxyZ6VtvmzMMz511ksxtcnl8gVKQOjyzoAzwOJhJPZucL3f4ORsISq6pokG5ZjO26LnQ3RHlWZ61lq9bXpyzb3m2j4jqukAfBBZBtoWnxHPAJywXEUbTsQU6wOgdCoFGsRAl2YrTpAACOzBIHSNh0qUShYV+uH5gR1x3La4LkTsVE0fgdHkAxR7dix7GcWETCkLxw5gKOkBpmQ-ozgCihAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjtDbVidgHEtYP1EVkiacygDcgAPU-PL1SgqHhv7-EgzrAvaRUUNIoLFAACTIOKzLRuDxeXw+eoTULhKJsUjoHzgyEVBFSchhQrFADMbAAVshEmtLLFRnFoGjEDsDKQ4gV6DtoABPAxUfRYZiUUhXSAUz5YLKxdClP5AjQwMKjdlUW5mT4AWUlVDA4U1RhMNS16DA1KotLA8DAutMQswpGSKqgkXQHnQOBllmSCXoJhO+HsDkgCXImDs639xXg5GYPM+wVgVA5lIclgSiC9dJFYD9SY2tvIbM+Akj1SeoaTkGNtPp+iZLKobOB2agnO5pxdJgjkFLYABSbd-uQpAAXtHM5Bw5GRwBWZ4BxJpqu+scRqN0AAschnFfTDM+NdZSj7lnQBngcTCCcX45X+DkbEJM5T84zxCvI+6m6oNO31eZ+-+DagD5RzIT98ygI54BOTc4gjEdiGHWB0DoVAI1iIEmy5EdIAAR2YJA6RsOlSiUADIAvYDczAww7iFcFYJ2BCkPwFDyDQmdmyw3D8LCJhSGIntu2BWdEDIH1MwBRQgA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjtDbVidgHEtYP1EVkiacygDcgAPU-PL1SgqHhv7-EgzrAvaRUUNIoLFAACTIOKzLRuDxeXw+eoTULhKJsUjoHzgyEVBFSchhQrFADMbAAVshEmtLLFRnFoGjEDtEPAsCZgmBSMgwK5tPQdtAAJ4GKj6Ew1YrwFz-YEwMKjPlUW5mT4cBBs+BgWBUWlIYKjMDQdBgala6BgIyi5BXSCYUjJJVQSLoDzoHBAjSQZIJeisuzrKAJciYX0OByQcXkZjCz7BTX8ykh-2Jb10in4ewJja28i8z4CCPVJ5+kOQY20+mnZms9mc7mwXnAjMwQVRqBTFlMlxFsAAkNu4vIUgALxb6eL4cjdAArA3QwlEMny2mwxKJ-gACxyZ4lqg0lMMz5M9tsjlcnlKP190PoAzwOJhONprvLiMtuRsQkzyxzhepsDEcctt0W6lnuFZHtWp51uevYzpAHx-iQWY5lARzwCcwFxBKI6QMOsDoHQqASrEQJQAKQr6AAjswSB0jYdKlEosEPghZA7shhh3Fa4JYTsuH4fghHkMRW5kS2kBUTRYRMKQDE9t20pzmQJxpgCihAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjtDbVidgHEtYP1EVkiacygDcgAPU-PL1SgqHhv7-EgzrAvaRUUNIoLFAACTIOKzLRuDxeXw+eoTULhKJsUjoHzgyEVBFSchhQrFADMbAAVshEmtLLFRnFoGjEHZ1lBEPAsCZgmBSMgwNUKOQwK5tPQdskEvR2cg6ARIABPSBqKDQGUGKj6Ew1YrwFwAiBAjQwMKjHbUW5mT4cBAc6DoHlUGnQMBGdXcyIyx3aLXkeD3Z6QTCkZJmqCRdAedA4PWWUXocWIE74ewOSAJciYRkOJOa8jMVWfYKwKhy4EZ5OJcV0ikJpklsh24WfATZ6pPatJ6lUWn006s9mc7m88j8wWwYXFjOK5W55nhkxayCtnUOSNt0gALyniZLWZzdAArGPLAlEOWuwnINupwAWOS+9udyvMntxvs8umDgVCpRM5eWdAGeBxGEcpVuOUAXnQchsISB5QEeJ4PsQ4H4N0t52h2FYMp8LJss+XKvnyH4jl+GY-lAHxgMQtbkPWUBHPAJy3nEWobpA66wOgdCoFqsRAhOKr6AAjswSB0jYdKlEoY6yjsVE0YYdxXJA4LMTsbEcfgXHkDxvpKvxnxCSJYRMKQEmLsupaIGQ8YUQCihAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjtDbVidgHEtYP1EVkiacygDcgAPU-PL1SgqHhv7-EgzrAvaRUUNIoLFAACTIOKzLRuDxeXw+eoTULhKJsUjoHzgyEVBFSchhQrFADMbAAVshEmtLLFRnFoGjEHZ1tdSHE9Ph7A5LIg5rAqNU6HBmFRgZzIFQsAZoABPU6JKiUzlQaUGeWfZAmYKjBWishUcj0ZB0AiQATkGI1SDKJkAiBAjQwMJa9kwKi3MyfER6qi0sDwMBGEzQZ7U73QX1gOJm6pPKCYUjJd1QSLoDzoHB2yzJBL0DWM0UJciYPOKyDFeBm1VQYK8mUihyQBKIHN0inO3X6-Sm80xyAh2n0-QGFl6Z4wKUq2VYEzlpTWuu90gAL0rxDLFboAFZR43mwPnWuhXQACxyUd9lsMz5D1lKDP19AGeBxMIy-flw-4ORsQnbxK71tgKu76Vt0Z5ev2AGGMOt7zh8gEkPGHafEc8AnGecTliukDLrA6B0Kg5axECSrjpWkAAI7MEgdI2HSpRKPOr7we29CDncVy9hhTrwTheH4AR5BEaOypkZR1FhEwpD0TaYB3g2iRkCc7IAooQA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjtDbVidgHEtYP1EVkiacygDcgAPU-PL1SgqHhv7-EgzrAvaRUUNIoLFAACTIOKzLRuDxeXw+eoTULhKJsUjoHzgyEVBFSchhQrFAAsbAAVshEmtLLFRnFoGjEHZ1tdSHE9PhiJETPR9KNgsluc8YABPAxUfTIEzBUZXEikKjkejIOgESACcgxGqQZQAiBAjQwMLS9kwKi3MyfETyqi0sDwMC8-lgfE4QWYOWmfSRdAedA4PWWZIJeiSxkOKAJciYUNhqDFeDqsWfYKwKhCykxyAJRDBukU41keXcz5qjVPKDU625hmfAwsvSC6AixNQKZYEzxpRM-0OSASgBezeIcYTdAArIKszn6Tth8xm4S5IKK7TpzW60pu5Z0AZ4HEwmnjbPm3I2ABmCeJKd5sBD+NzujdJdWlfXwzr-7AywfG+ywv6I7wCcS5xPGg6QAOsDoHQqDxrEQJQI2or6AAjswSB0jYdKlEon5QAeP4Fgq+jvDK4KgTsEFQfgMHkHBDZNihaGmGETCkNhOpgN2maJGQJzsgCihAA Open this visualization in the Vega Editor>

@
let sel = selection
          . select "pick" Single [ 'BindLegend'
                                   [ 'BLField' \"Cluster\" ]
                                 ]

in toVegaLite (sel [] : selectionProperties "pick" "Select a legend item")
@

-}

legendSelection :: VegaLite
legendSelection =
  let sel = selection
            . select "legend" Single [ BindLegend
                                       [ BLField "Cluster" ]
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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoAY3SIyAczrEARvE0BrPbHTNE9OpCNVSeqkvURIWeLDP2ADuikiNCQ7gxM8IYaHsyw5PYAFtDQfsi4APTpCADubHqk0AnMRszINNrBVMFs2ljpHNZ6AEKxZlTpCQBuVHrw6V7I1LAd3b3pjNB9vaTwUvCzAGwAjAtzS3JSk0bkVEvwbIjoUglU8PQ0bNDInaHRUBjYTIaQft5lzwDiXgb4kIisjloqigfnIAA97P8sIDQlAqDxQRDflCYYpFBpFGFIAASZCaE5eRLJVIZdKjfb5QrFNikdDpPEEvrkqTkArtToAZjYACtkDpbh4yjtNNBaYgohAPH5SOYqHZ8KBJZLIFQsH5oABPSE6VxhJUOILysCKpXKgTkUpDCWm-VBPzMEK-IVUEUCm3K9DqsXIOgEP462GQJrkJDaJZQYGQQQyDkATnDEag0YWchkHkjADkPgIZAAWACs4cgyjuSvRNsxpagmr8ridQT0Ozd+rIVHI9B9+D95stNGLd3LYErHlF0CbCpgVDBjqgAAUdvAymBnSKwPAwJoLYM+5HMKQqjOHOhkugcMO4YhtPQG9aPNpyJhb8rOvALXWoJYqFq9cqKtfRfyCpVh4rbtvYPbbkCwFQCuAHir80qynYP76jW77+lgQSvpAVaDhA56CqQABe77EC+b50PmkZ-gUYrPORzDvrmciRrBdEITKbR2GiP6QJ6pgFFqE4Me+chsBy1E6P+7HGpAIl0EsrFti6cH+JxcpKARUBIrJoFGpAXzwAYrGaK+pGQCRVh0Kgr5lJi1YarW9gAI7MEgo5MKQ3RuHckBCbp+5gQh4KBniZnPJZ6DWbZVD2TAjnoa57kFJ53mDuiihAA Open this visualization in the Vega Editor>

@
let picked = "picked"

    clusters = [ \"none\", \"Blanco1 \", \"IC2391  \", \"IC2602  \", \"NGC2451 \" ]
    sel = selection
            . select picked Single [ Fields [ \"Cluster\" ]
                                   , 'Bind' [ 'ISelect' \"Cluster\" [ 'InOptions' clusters ] ]
                                   , Empty
                                   ]

   conf = configure
            . configuration (Background "beige")

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
               . configuration (Background "beige")

  in toVegaLite (conf [] :
                 sel [] :
                 selectionProperties picked "Please select a cluster")



{-|

The selection can also be bound to an axis (or both axes, as in this
case), using 'BindScales' (applying it to the 'intervalSelectionY' plot).

<<images/vl/bindscales.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoARAUQH0BJAMIAlAMpgAPGAC0AJgAMkAL4BdADTgoWeLADWdSAAd0pRNEjqIkRtHh1QES81jl9AC2jQDyXAHofCAHc2AHNSaFdmACNmZBoAY3RTKlM2BKwfDnRmYIAhJx0qH1cANypg+B8tZGpYItLyn2sK8tJ4KXg2gDYARk727rkpG0jyKm74NkR0KVcqeHoaNmhkYvMNSwxsJjtDbVidgHEtYP1EVkiacygDcgAPU-PL1SgqHhv7-EgzrAvaRUUNIoLFAACTIOKzLRuDxeXw+eoTULhKJsUjoHzgyEVBFSchhQrFADMbAAVshEmtLLFRnFoGjEHZ1tdSHECvQdpETOzPuD4KNkFdIMkEvQTMEBfgCJAAJ6C+5qKDQaUGKj6Ew1Yp8pSA4EwMKjHbUW5mT4cBDBMCYMAAL3Q6CwYHCVDA8FuVAFz0gmFIyRNUEi6A89qUuuF6FFiBO+HsDkgCXImEZDljmvIzFVn2CsCosuBybjiVFdIp0aZ+bIVHI3KgAjT1SeZdj1KotPp+gMLLZlOTsaVKtO9pMWsbAOTQKZkGQpGtGbAxFT6boAFZPQlEEW29HIAvZwAWOSe5utkvM1lUdn-PNQdAGeBxMKyrc7uhyNiE1eFsKbufbvmL-DdIelYtsWDKfB2Z4XuO+YfD+FZVvoRzwCch5xHys7EDOsDoHQqB8rEQKKsqs6QAAjswSB0jYdKlCGE6PnBPoIeBdyCryBpblhOH4Hh5AEZ6fYkeRlFhEwpC0aOYDQVAa5kFGc4AooQA Open this visualization in the Vega Editor>

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
close to 1 - rather than appearing near 0 and 360.

<<images/vl/coordinatedviews.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEBOCmAOsCGAXSAuMwYHsDuGYA2pAMY4DOASgIKQA0UAIgKID6AkgMJUDKkAXUZkcAGwCuAWwB2hEvFEAPBlADikpAHNBAX3rgoFRKUKgIESCmhJpFAGY5okuQfNZISCoRHU6w0kiipOKiqLDe5BQAFAAmqFIAdLScPLxgAFRgAAocYAD0YACMABwADACUkDquYEI1kBrQANYRAJbQpKLh+m6QcShIpjUW4tCi3gAWKCjwFOh5edZ4CZqtKBPiAEbiFLAdONIosIcJ5JJ5TDjimgBCo02weRMAbrCaSHkaFEfQT6-veX6H3erSQAFokOCAGyFKEQwqlMEDTZdQpIBLSHBgibIGJ7BIoCjPBjDKAOJyoIZuXrwJDQXamSDqLTeaRSTZ7FSQBTKTCQNmSDnQLmwNg81nsznValgaXmPT1XZdUgoVoHRnwVqkB4xRkoACeiG8rUOe2egSqCt6x3IMRN2kwZmpIlEjipMqg5ok4T5mjg+pJHqg5GkdtV6sdpN6dlasFEur5XAk305PSDhjjsBVatkfM12tgurTQcshp9UExkhNFqjstJVudvPcMbjCfccEQlL55AkMiqwgNRr5AEdxDZVQNVa9+6TIAHHZAW-HGR3kGg+dB8P2oIPy5BR+O1qhWtO5RA5Q3IAASCikHEaSbTWbzPL-dGrdZbBJqvK3+8fN8wVENZHmeABmBIACsKHVNMRGkGMHSwaodCAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEBOCmAOsCGAXSAuMwYHsDuGYA2pAEoCCA+gJIDCpAypADRQAiAojfUwLquQAxjgA2AVwC2AO0Il4IgB4soAcQlIA5pF4BfZuCgBnRIMKgIESOugBrQkICW0QSNgsDlgCaokZjxcgxaBF7AAsUFHhDdAB6GOgkPAA6DQcUULEAIzFDWGccKRRYQqThCRi2HDENACEgm1gY0IA3WA0kGPVDIugm1vaY7xQO9ockAFokCYA2AEZpydmABnHhzNdZpCSpHHHQ5E88pJRDZvcLAIAzHGh1NExzC4D4JGhcs0g1TXspSUy85SQeRKTCQX4Sf7QQGwSjAn5-AE6fwQJEXPT+SC5VyCFAOAofeAOQQNTwfTIOKSk0GGQRIVyGQEoACeiHsFJ6zTpkB06IukGKwk8FK0D2RUGEIhuH0uDlgIipUFo4m6AIEzNZoJ2EgpXN5T0gIKwkBlcoV2DgiFQ9glkhkeig6rcoIAjmIkIU0qgHK1ufonlAmdLZfKPhbkPcoNB8L6HSynVBXe7ccNcT7URZ03rIAASGn7dRhCJRWIxfpbVLpLJJPExPOwdSltoTERpRrNADMSQAVoZ8X7xQUZSKsEidEA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCEwJBksDgCBZAA4iYshJBiWLLcgqlQZHgAHrX1jc0pUPwA+m2dqHUNTTiQAL5T6VNpmLqiYkIIhADa6RDoGJjRbCwkhAAcvgu7UCY4GrVkImwSqVsZDGzQnIQALL5nz5gCwSE8DY4nK3BwfCI5SkomgDxYFA2kAAnpAALp9SDwZFkfi1B7mXSwAgzc67LEcYK1ADK-EB8DAsDAAE0wPQxCx+GB4CIwAAvEQiWRgB5gJp4EQMJ4XforERwjkFP4ZEZoZXkzJsOnQWpVWA1MkyqAUVZU1CFPmCESETLEgTzdUZbG42oARywsAkHAQbH0kEdDqNUFR5qyWrwOtGwxaWJxeNG7s9wMQwL9sxl6YwgYwO3JVxuUfuj0NmFe7yhqG+vxlkBh8vhSqDkFEEsmoc12tqAGE8HxzDHnfGoBFZA9idMSyrG0GoB2I7rqtKZ5ATcShxarTa7fxs03B26PV6U768ZPybB2mxEaHk2awPU8HhMxmz8Hp0255HWh0l03V3fCmgIVYAecoAX4IEQTEWpwUhaZdyNWMXQTQ9kx9P0A2VZ8wEzDF0kgAASE1ohMWoSHIKhaCsMJ2E4HgmBBahiP4ExqGopw8A4bRdAAZiYAArChQXOZtxE1GpzVmKYgA Open this visualization in the Vega Editor>

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
                               , PScale [ SDomain ('DSelection' \"brush\") ]
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
                         , PScale [ SDomain (DSelection "brush") ]
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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEBOCmAOsCGAXSAuMxIGMD2ANgK4C2AdhmANqQBKAggPoCSAwrQMqQA0UAIgFEW7Lr0gBxEkgDmPKPAIAPSAF0Avt3BQAzohyVQECJAAmqJAa1GoRaAUqQAFihTxt6APQfoSAO4A6aQBLFEciACMibVhofDIUWHj-fBIPPjwiaQAhWwBrWA9HADdYaSQPKW0E6EKSso8zFHKyoKQAWiR2gDYARi6OnoAGNqbwglgepH8yPDbHZBMY-xRtIp4rI0gAMzxoKTRMQ2tN+CRoaIMJKVlMSDJScJi5SAVlW-uSR+hn2EZXhw+X0gag2EBB1g0G0gBCQAE8npgqKCsMjjFJoLkHOEzutjptEvgTEEyDcUXjrJA3lhtkFYAQTJc4IhUA58MRyMCxChYYgHABHIhIeIhVBBEqc1GbWGXGTSOBlBKsjLxZ7c3m3AVClAi7Xi8Hk-UQzR4o54yAoHxkbQ7PaUKjYLZBAjVS7RcY4bV4Ci3cLQKKOYFqFTG8mQdGYn04kNmt2wD1BL2XX3+y4EvBEknaO2U1RcnmwBzE6pFJD2NSQ8lQNMZ0mm0Ns3aXEvEAu3aSERZkaDpiWV4xUh20+mMhDIA5QNmkCgaKBq1tQTXCpq6gsVvuQaWHSCy+Woee4ZVoPPqheCpeivWSw1Ga8qLRryAAEm0OHmUgczlc7i8dSmwVCET+AmHgvm+5S-m0BAhAURQAMz+AAVtoiYhgeZCOrWIJqEAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oAKAE3mgFcsA6AEQFEB9AJIBhAEoBlMAD4AvGAAMASjAAyMExbtu-YeLAAeWQBZFkAL4BdADTgozaPDqgIESK1jk6kABbRoAB2RcAHoghAB3DgBzUmgvVgAjVmQaAGN0RGoMjjSsIK50VkiAITcAayogrwA3Kkj4IKx4ZGpYSpq6oLt6utJ4AFp4foA2AEYhgZG5Pvt48ioR+A5EdD6vKnhGGg5oZCrIa2coDGwWR0g-eFhks4BxRsjPRHZ4mn2oP3IAD0fn18soKh8D7ffCQJ5YF60UymGymA5QZB+KgpRw2FxrUiRHx0ABMAFY5PCXGFSIxYriCUSoOR4ABPV74Aho5xOQ7OSCNWClTx+dCkDL7ZmHSBURBpRj8h74Vls9lpciYM5kKjkRieITkJItN4wWlIx7oLD8+Aef6QOaRUVq-BPcjkOFCtmQEFgGWy9nK1WeO7wB5U90IlImqhnABeNHQdFQJuSDoDwugepDoIAjqwkNAYixSDVII7DnH45Baaj4x7SCrre8voKyy5kEG5mGI1GY1RC2XdfrU+mMlnM7n884YQGR2yOyyhzAEChjjhGUOIG6A5BIrACn54iXGZANVrXlZF+ymnQCM6dSXD3XqegqMhkDya-73ZB0t77nmy2PZVeX5zudKkDNOu5QAOqkuS+A4mawHoOUnizPAKTcmaibdtS-IhhOwqiuKkqlkWLrEJ6VbnqhSaeGmGb9jmIZmvAnykA+NqsHa2FOturokBWXqgiW5HoZAVF9vYA50VADFMXQtr2kO37Dsy5iwvCkAACQNmsjSeD4-iBCE7SLNEsQJBwpDoEEGlUI0QQGX05AxBUVSGBwABWyBvip8rsCguIqdGKRUNASo8aRe7NH8UBocmUDLEaiAmmYnnpGQUqujCphAA Open this visualization in the Vega Editor>

@
let simplify = transform
               . filter (FExpr \"(datum.DE_ICRS >= 0) & (datum.DE_ICRS <= 40)\")

    rawEnc = encoding
            . position X [ PName \"Gmag\"
                         , PmType Quantitative
                         , PScale [ SZero False ]
                         ]
            . position Y [ PName \"plx\"
                         , PmType Quantitative
                         , PScale [ SZero False ]
                         ]
            . color [ MName \"Cluster\"
                    , MmType Nominal
                    , MLegend []
                    ]

    rawLayer = asSpec [ rawEnc [], mark Point [] ]

    trans = transform
            . 'loess' \"plx\" \"Gmag\" [ 'LsAs' \"x\" \"y\"
                                 , 'LsGroupBy' [ \"Cluster\" ] ]

    trendAx pos lbl = position pos [ PName lbl
                                   , PmType Quantitative
                                   , PAxis []
                                   ]
    trendEnc = encoding
               . trendAx X \"x\"
               . trendAx Y \"y\"

    trendLayer = asSpec [ trans []
                        , trendEnc []
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

      rawEnc = encoding
              . position X [ PName "Gmag"
                           , PmType Quantitative
                           , PScale [ SZero False ]
                           ]
              . position Y [ PName "plx"
                           , PmType Quantitative
                           , PScale [ SZero False ]
                           ]
              . color [ MName "Cluster"
                      , MmType Nominal
                      , MLegend []
                      ]

      rawLayer = asSpec [ rawEnc [], mark Point [] ]

      trans = transform
              . loess "plx" "Gmag" [ LsAs "x" "y"
                                   , LsGroupBy [ "Cluster" ] ]

      trendAx pos lbl = position pos [ PName lbl
                                     , PmType Quantitative
                                     , PAxis []
                                     ]
      trendEnc = encoding
                 . trendAx X "x"
                 . trendAx Y "y"

      trendLayer = asSpec [ trans []
                          , trendEnc []
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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oAKAE3mgFcsA6AEQFEB9AJIBhAEoBlMAD4AvGAAMASjAAyMExbtu-YeLAAeWQBZFkAL4BdADTgoACyqkA5reh0AzHLnWIkZtHh0oBA+rLDkdJAu0AAOyLgA9PEIAO4cjqTQtqwARqzINADG6IjUJRxFWPFc6KyOAEKhANZU8bYAblSO8PFY8MjUsK0dXfF+3V2k8AC08NMAbACMczMLclP+2eRUC-AciOhT9vCMNBzQyG2Q3sEkmL2u+MTR8LD5gZAA4r2OEYjs2TQrlBouQAB6-f6AyxQKh8EHg-CQP5YAG0UymGyma6QZKkRiZdyebEAEmQBXsvQiUViCXiw126UyOQ4pHQ8TJFO69Km5AyLTahg4ACtkMUrjZIOR4ABPQH4Ag2CBBYI+XqwRoRaLoUglcUqnxURBFRg6n6PRX6yBFciYd5kKjkRgRITkPIDIEwaXRKi-dBYHXwcJYi0qyAIsDK-U3e2OiJfeA-a5RnxkwM+x6QABeNHQdFQgfyweTN2gXvTUAAjqwkNAMixSB1ICHgkXk5BpXbSA6nYj4R7U1t3tnYLn8PnyIXoZ7vREqzW67XGxio8uW0mI82YAgUBhsHQFcXI23HCPWNFsh35ZAXW7AVZm6G+vuwx6O-fiz5YJ0v8hkKzEJqYJ6h+kBioi8Y-A+YCriq76WmqGoZv0I7NAA6niBL4AATFOyHoM0ESbPABQalOpYzoivKID6rahoaxqmoEUFWugNq0BmMY9lAN79FCUDkeWSJ+gGQbrqG4bEJxETgmRZaztWJQLg2PpTvAoKkMgdB-OQ5C0Zal4RiQXaxoiHayRRlYKbW-iLipUBqRpWmsDpMEthaq5wSxiBkGaEYYqYQA Open this visualization in the Vega Editor>

@
let simplify = transform
               . filter (FExpr \"(datum.DE_ICRS >= 0) & (datum.DE_ICRS <= 40)\")

    rawAx pos lbl = position pos [ PName lbl
                                 , PmType Quantitative
                                 , PScale [ SZero False ]
                                 ]
    cluster = color [ MName \"Cluster\"
                    , MmType Nominal
                    ]

    rawEnc = encoding
             . rawAx X \"Gmag\"
             . rawAx Y \"plx\"
             . cluster

    rawLayer = asSpec [ rawEnc [], mark Point [] ]

    trans = transform
            . 'regression' \"plx\" \"Gmag\" [ 'RgAs' \"x\" \"y\"
                                      , 'RgGroupBy' [ \"Cluster\" ] ]

    trendAx pos lbl = position pos [ PName lbl
                                   , PmType Quantitative
                                   , PAxis []
                                   ]
    trendEnc = encoding
               . trendAx X \"x\"
               . trendAx Y \"y\"
               . cluster

    trendLayer = asSpec [ trans []
                        , trendEnc []
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

      rawAx pos lbl = position pos [ PName lbl
                                   , PmType Quantitative
                                   , PScale [ SZero False ]
                                   ]
      cluster = color [ MName "Cluster"
                      , MmType Nominal
                      ]

      rawEnc = encoding
               . rawAx X "Gmag"
               . rawAx Y "plx"
               . cluster

      rawLayer = asSpec [ rawEnc [], mark Point [] ]

      trans = transform
              . regression "plx" "Gmag" [ RgAs "x" "y"
                                        , RgGroupBy [ "Cluster" ] ]

      trendAx pos lbl = position pos [ PName lbl
                                     , PmType Quantitative
                                     , PAxis []
                                     ]
      trendEnc = encoding
                 . trendAx X "x"
                 . trendAx Y "y"
                 . cluster

      trendLayer = asSpec [ trans []
                          , trendEnc []
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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxhJUBLAG2gFNY8oATeaAVywDoBhKt5OrAIAGALpgAvBLABybjLAAfRWBbsuvfoJHipsgBIzIAXwA0xCGXjJGkAA5UAHgH0q6SKagBjeFS9sqVlpbNQ5OB0cwAFpVVjDaZwiTcxJSSGtbCOcACwoPb19-QLoQuK4IsABqWPVOBKTjYlEUqCx4WABrW1gA4JbINXhGUFTINlgqW2zoaDtkXAB6BYQAd04AcwpobLYAI356L3REOhPOI6wFgBF0NnWAIXGO2gXsgDdadfgFtoF6V4+XwWgwWXwo8Ci8AhADYAIzQyGw4RRaDwXZUWiw+CcRDoKLZWjwJj0TjQZBvDwWKAYbCsYb2drIYL4MgAcTa61siA4u3o+XsTi5PL5nkg9UF+Eg3KwvIYxkaEDMxEgABJkF4CW0pjM5osFoDsZttntOBR0At1ZrvgaolQti83gBmTgAK2Qx0pEEgTIxXmgZsQ9LsFC8zyY9N2FEQ4cl6t8tBsougAE87MyoFHBG9fCYlV7aIgjkwo5yWVTII4AEz0yi0KgxqBZXLJcuOYZUr21+uZJyudwtUZxjH0gBe9Hc+FQviZedSUBTadsAEc2Eh-aj-R9PXP0o4KDYWTAtsPJXZ2r5ApEABS-ACUJips69Rz4WEDh67Dcgmj+DCTqfTKV0CwKMcyfKBkxrCg6y-dl4E5f9F0lFc1y2VgKC3BUwFnSAjkQShS1IRpjCAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJUBLAG2gFNY8oATeaAVywDoBhc55a2AgAwBdMAF5RYAOQdJkAL5CANOChZ4sANZ1INWJgBG6yMoiRG0eHVARTzWOW0ALaNAAOyXAHpPCAO5sAc1JoR2Z9HhoAY3REali2aKxPABF0ZgCAITsNKk9HADcqAPhPNV4aPMLiz3MS4tJ4AFp4JoA2AEZW5vaBRot9cip2+DZEdEbHKnh6GjZoZHzjFVMMbCYrSFd1ZCoNgHE1AO1EVn0aYyhXcgAPY9PzxSgqAH0r2-xIE6wz2jk5FTkJigABJkJFJmonC53F5PFURkEQmE2KR0J4wRCSvDGuRgrl8gBmNgAK2QMSWpioiGi9FIiCO+GsNkgAE8AKKwPS0RkkUhUcj0bQvN7yIGmd5gYhkfmCj4HeBHR4wFmuXYfACOzCQ0GCTFIhVFyyg0W4WEQG2lAu0XB4fAuytVx3QWDp8AcgKNrKsRpWfKtHxFYuZYLdaslkAAXjR0HRUG6dh6bMzoCqw5BNdrdTqDUHTPBrqRkBsddBBtotghyOR82AABRlACU8iN-wgieNMTIDMl-zkQA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFACYCGKpGYoEEkArgE4A2VMKKADgM7oD0fRqQDuAOkTwU0egCN63WIwDGAewB2KWBtGqAtnwAiK+ogBCTANaw+0AG6xEpPrtLdNjG-cd8yFPo-hSAFpSYIA2AEYwkIicIIoZZlgI0lE1FSC4UmJFURRuW0hCWigAMxVGFzRMYEhOUkYFKlqAcRdENjV6XRlFIqhOZgAPTu7exn7IWAB9QZHMSC6evoBfFaIV4qhheGIpKlwCIkgAEm4lOBc2VC5eAS9UiSlZUXgVPnPLpweg5klrWxYUQAK246iKx2YpAAnn1MABtIgQGglKAuRgWZpI1GQRj0JLNSCqZgVNiaUisTbYkpTIaaDRsNw5QpbVFQFDQziwNiKRgVGQNCFsugoeBKCzcQnE0kLThMQbc9ZsqlsqZqVTEeBqDo1am0SDQwmleCwZjENhtUgdfBQc4U7k1SAAL0UKiopQpCk27M5DqgAEd6KQNJJyPB7JAlaioxAVbQUTT0ZjHdx4C6qAAmAi26CkLmMwMNbk2yAc-OylRatBxmlaDVanXUPV0Q2O42m80LS3WqBWxCMBzkP2QXSwYOTMvDwshiiiiMx2gLqMAXS2avr2sJ82okHbZrYAGFmPJ3BPfZ0VLotRTI3GiepjY3gOsVkA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFACYCGKpGYoEEkArgE4A2VMKKADgM7oD0fRqQDuAOkTwU0egCN63WIwDGAewB2KWBtGqAtnwAiK+ogBCTANaw+0AG6xEpPrtLdNjG-cd8yFPo-hSAFpSYIA2AEYwkIicIIoZZlgI0lE1FSC4UmJFURRuW0hCWigAMxVGFzRMYEhOUkYFKlqAcRdENjV6XRlFIqhOZgAPTu7exn7IWAB9QZHMSC6evoBfFaIV4qhheGIpKlwCIkgAEm4lOBc2VC5eAS9UiSlZUXgVPnPLpweg5klrWxYUQAK246iKxxQkiSbAAwipdPVGPA1IgwEJUbBuBC6MxSABPPqYADaRAgNBKUBcjAszSmQ00GjYbhyhXwUBQ+M4sDYikYFRkpDUxEgmzJtCmalUxBRHRq4pKkHxdNK8FgzBFCzapA67Mg51IMJqkAAXooVFRSoaFJsOVyeQsAI70IVQihQ+yihVgdYlMUlCmK6m0+WUiUyCo5Rp0tz8qwGVzQKjEsLsgBMAF1-WGoKpmBU2IghMqtmH6YzqlB4I6JqXKZBOdzeYx+YxBcLIN7s4qtNLZc1vXRlcbVerNVBtbqoAajdRTebLdbYLaG-a2M7XZJyPBPb7KXuIN3qN7IMG2H81Dy63ReyoZaiBzmoMPj0+6KONWxJzi3-qlIaHTnM1+UXZgbWvesdSLBxyEA09YBlIUfyfVcmydF0NC3D0eUHH0u3FI9AwlM9jTzAsFmgrRJljFR40TZNUzANN2QAFnTDM9UbOCLx5I8bylO9+1DHMlUfX8P3HSBvwgxUZ0A2pgItTArTA5cZIlKDGBgzQ2F0WAkPUuguPXDC3W3T1cIPWgrL3Djjlve85TneY5wkuFmHkdxJmMhZ0l0FFDVFbNIFUNRVSc4B1hWIA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAWCmCWBzaAXSAuMBmADDgNOFALYCGATgNYZQBGA9gB4AOANvWoRJACakqkaoCNwCu5VjRgoUzAM7oA9IvKkA7gDpE8FNFG1Rc2OQDG9AHYpYljWeKKAIvVGIAQuMqxF0AG6xEpIpkclbk3n4BinwCigHwpAC0pIkAbACMKUlpOAkCtKywaaQa5vQJcKQ8xhoocj6QXCKQAGb05GRomMCQzBRGQpAA4mSIUuaixLTGDVBsjGMTU+QzkLAA+nMLk9MAvjtEO42QavA8ujS4BESQACRyJnBkUqiyCsoRxdq6+hrw9Ir3R6BD4JVg6Lw+LAaABWcgsDWu1jMPHg5lGXSI3HmXRa8FgrB4UgAwqxDKEVigAJ7MWBjejEVGkSSHTFQSkDZp4glSYakUb4KBUmlSACOolIlh0-Hgfkg+wgLO4ZnMnPRYGA+x2QA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCEwJBksDgCBZAA4iYshJBiWLLcgqlQZHgAHrX1jc0pUPwA+m2dqHUNTTiQAL5T6VNpmBRk-EIF6Zh4sACezagA2usY6BgnkCY4GrXcJamHp1Ki0GxiNajHJ6eieDnlmWz8eGgtQAwng+OYWpB4Ftll0RLJnrACH1IMEWFIgah6ng8PM7h9ICM0Fl-oDalVYDUUdxnoR4DgsPwUdDYaMAI5YWASDgINj6aYLD6nLZrIVCyCUlg4AwIfi1URYCS3MUElly9mc7mIeB8uWClWYWDtNgUco6+DBWoAOXGgjAIkyYAAyohStN8SdZiqvR88UL3gTzpc3h6MrJ+E8ueUvj9RkFYEJLn6DZBuCIiYUzDgRFpagwiBw5cmVZBlBb-m7UJkkQJ9WKoTD1VA0+02iJ8qHiwSHiIni9RSmY5M3iSAZioKDwb0oGq4QixEjIWiMYRsbi6+KM6OyaMKVSZ43ahyuebefyu+KRSPdEjGYQABy+H1C58YV8AXTmgsgABIKEJohMWoSHIKhaCsMJ2E4HgmDYERqH-QCjAgpw8ELahdAAFiYAArChxGVKAvgaMRTVQABmb9qyEfh8hHP4xxBMEzGnBtWSgCJ50XZNIFEMQ-leNBZimIA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1hJ5k8oAlAQUgBooBjeAGwYFdn5oBTcyAEy5ssAOmoB9AJIBhCgGUwAejABGAKyQAvgF1a4RukSoAlgHNyxAG7HuAdwuRkcdAGte+GAhQAHeLG6I0FqaehACXPAW+mFssMx8ABbQ0N7IuIqKCLYipsbQCWwARmzI3LAMhjyBIhVYigAi6GymAEKxbooJltym8IpYpDywnd29ioLQfb3G8AC08HMAbCqL8yoADLOThczcKvAiiOizCdzw-GUi0MiWdNFQGNhcDr6wpQ4A4gPmHojChWU6FBvMwAB58P5YAGwIGQbjiEHg37-QGaTT6EL6SCWCqIJhBfAEe6gCCkqA4wz48hEslkkm0smQU5mJLkDbrUIM0mQWzGfj5cgAFnWHPuDMgnAAnoDCWKufSubTIANYC4HBVmJg+KYEJLYdBJd53FBCn4tJzFYyAhV+MZED8wArLYykY65c6wiZuMx+HxqHcPVzIIU7Q5uKCqgTCBywAAmQW6KBObjecix+iQRDGBjG1AsUqYwPi5CYKMCbjIHOIW32gNFxkGo18ACObCQ0DyXGM3Tr9bC8FBxjI+D+zGY7sthb7kD1+Cd9ccTF2Dn46AGocJMYAzCL1top32oPBTDqelxjZAKmxAr3p42L63252Oz2LdOB0OHB3oMuRxxmOih6AUWwHOgelrzpanhIGgmA4ISxAmMwQwOKUuwMB2hh8N42ZqmiugTuKKpqh4powm+zpwni6A1g6kGURqWpzoRUGWCwbAXqeeoUUWl6GLamGIFEh6Ml6Pp8NIzAlChPELmh3AYcYWEeDhDBqrJvH3hCa52iwkAsYqoEgRpiqQK69EepAYm+h4-omVBIZCXOcIRgEUYEDG8aJo4PCpvg6ZQFmObkHmzAFvZpklrAZYXJWAS0beC5aR4j6BM+3a8BFQYfsOYCjuOQFZYys5uiJSZLsaxCruuTnRvQO4ivuRVKsep69DwfBXjezUNoaD5tmlkwvplBnZYOuXEN+v55f+RmBnNk6jQtZLaBO4F0ixTK4lSsqBhZ4rMqYrL4MKooLsRDjoL4DB5CV6wiGoGbJYwxjlMu61Qby-IJEKIrNY43oKYJLy4V+fV8MgdqmMuGbWblBCQJJ0mAvuH2mdaNFQ8J06MTCzFldi7Gcf43GjeKuICUptX7ZZ1kSVJybkWTQbyYpynAqDPVKs9mY6YgenM2Sy2TlzZnY4eVk2OJtk0FzYSViwlUCGu8AbnVcYJhmQW5vm3Bo5ZUUxRWVYJXLUA86l35dq+gvcjlX55NNkAUCy0BgFQcUoFTYAABQJE0bwAJRaLb+umSVNOmXTHj1AAolIshyIlvEK9NVUq2rBCzAAnDGufedrIW62HUEWwNVvDcngYkON4siYTUkVtStsMjnZ1laSswAOyPS3tKzIs7cd2AsyCr3w8QLMO5m2SszqDPpJDx38992S0+r6SY8LxAg-b2APd77nG8EcPMCOxe9QKcwumCb7FynhWwd98LhnMy-pIlxAkfcodx1gKd-0voCnwOyf6UoZSEFtt-IifgSKOj4pqPGUAdTwG4ubcGpEzSfyDBjBK+MO5i3wafVOStqqq1qh5equ4mob37Cefw7ULxdSCHvGAGCoCW3SjbCeR5a7OSmsafK78wILxnHXAm0coBxwTvIKu05HJhlcjeQkbd6D5wzMmPyqhHqBWzDrMKetWFlyfENDKciFz2z-GODewiuS2KFlzaB3MvCwWwNSRCxhkIQOIKzYGKlQb4TNsqWBfAyLmKgrgrGRD6643EfXNijdtQk3CdXCmeQqZxIkVLGyUAkaMxSSnQGbNaqQFUupWh3IeZHCwLpeIFT7F2NEeZCpi5FYrgzhQ7c1DsGUVagw88nUmjdRacYwa1sRo8Jrp+fh59yBCJsaIiOLTJGQGkTIWRrCFHOXDJGakqiwDqKTL5NkOjMx6KLgYnppd2GQE4aY7hE8pm5XyjY5+odmarXmixE+UFkDXSiXGJe4pChDP4LlKyDMfprTlD8xkfz4A3VrP5IFJpQXgtQJC-SDjiRymCaqS6-yDTkHuqcnmN03q8A+jyPkwCwBqFjCigG6E-HwLKWDJsHhIb2hhg8bJ8NEYMxQqjHiVEbQAosggpipVWJEySdwUmDF+LpPZt-SW3ockCuRkzauviMn+LUgUthHLAp8wFmBNaGlCHSqjtk7CYJwmtLTkai8mpzBa3OfgUK4VNqjIrmY+yTyHY-gvK8FgnBQS+xqWOYw8xyilApsHYW+sxHRPFCsr4x4HW+q4RMqClj4ECL4B8MA3wszQDYBcEOr8GRGWAj8yAAASSspwBiJGSKkdIihRgHFyPkIoIglKKGbdwAYXazyzGvjwLtW4RAACsSxCU5GfYNX4dlFtVvAMAExN2oFgGuMAfgAAa3ZcAqAABzrEFCIdY2ctxdzPbCJADB-ZIOVHyfg70QCaCAA Open this visualization in the Vega Editor (although the link is long, and may not work with Internet Explorer)>

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
                           , 'hConcat' [ mapSpec, deSpec ]
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
       . configuration ('View' [ 'ViewStroke' (Just \"transparent\") ])
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
         . configuration (View [ ViewStroke (Just "transparent") ])
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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAuBOCGA7AzgMwPawLaQFxgG1wIxhIBzWdAVwAcAjATz0MgGEAba5aAU1kgBdADRR45Sr3Lw+LAmXS0WkAMY1E0SKMjxkyxNSz1+kAL6DTxEcUgATGfBagSUarA7KAFtGi1kuAHoAhAB3ADpyAEtoT2p6bn41DV4NMLUsAIARGnIAITcAa14AzwA3KXgArF0+WBLy6QD7aErpSPgAWnhOgDYARh6uvoAGDpb6Dl4++DDEdA7PXnhbfjDoZFKtYghIDGwZJ0haeFhkXkOAcWryfUNjAW1aDgAPW6MTbV4AfSfX-EgDO8BKZLBBTMIbAASZAqRbVLw+PyBAINGZRGJxMKRdABGFwyqojocaLFUoAFjCACtkOhEFsdhx4IwTPgiC5nC4oNVYAVlCpIrAVJN6ZzICk1LZIogbvgOZyoH9SNt5VBUJFeBxbMpONxaiKVVBoIxaOd-nMsFL4B4IQadvBnpE9LLlQbIIzjBwAILS4X4YY2207TDqjTKejoHzoHABwNu+AegCiz1oAn+kVQAAoM81DFjECtnmAAFRgAAcAEowABSMAAJjAAF4G31RHBqIgVDJeBm4JEsNmZLn3Rry6IAKyjsAAcin5f1tpgkRUBQA8sd+UanC6F6UrdRTWB-dvXUlJdBsXTZZBd1wDy3DbweMoB9Bc1KC8Wy5Wa-Wm30zMenLgoBLgULAkRavgba8DGC7ntAvpQDqPAmCBYCgiqwGuswzqxmqGqQVAgL3PO8owMaB6QAAjtQSDwTIkTlKRor2o6hzwYhkAAHJ3PwYDoKgYA8CcegYfKYkkBJWEkHKJCQNyvJXnwzyaP8WiGhRyjKZo0mgeK6CStKW6uoqslkfhmralwKEPGh5EmvoUaWtadmsU6SqBgy8Yat65CIUenlQMGKSqYaijMWRw4cEmKbKOmWY5lgeYfiWFbVnWjYNv6YBth2XY9uB-aJWEUWThOogznOsGuuey5rvAG44R5gVQDe+4sAFLWqLSZ4XocbV3tofBPv8L5vvmvCFql34ZU2wwAYFukLpQEEsKgVpnNVKqLghB4GBwHBoRJLhLaBTVmaKFmEQCvG2bGRoOf8NF0dEDFMVtoFuex0SIftHDHZJ25SVYAbdYgaoyqQlimEAA Open this visualization in the Vega Editor>

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

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCEwJBksDgCBZAA4iYshJBiWLLcgqlQZHgAHrX1jc0pUPwA+m2dqHUNTTiQAL5T6VNpmAAkFELRJrUk5FS0VmHsnDxMbCLUK2tGu054HNq6ACxMAFYU4qnpkHiwAJ7NqADa6Qg6AwmHg9DEFGyuUIAJBIMKLBwyjI3C+MMgAGE8HxzJAALp9SCwFiIgwIfgwwoiMi1URYCQtIkUWnKBlTPGzOFgAmAjImHAaWrXMQUhZwyBSUTQNhiGqoYFcyAjNBZNj8PDQWpYnG9KDwL5kCmjCKyGWwAjzXkgyBo+VW8WZNUaln0-Jirmgg1GqAARywsAkHAQbH0bw9GVg7TYzPlkHgHGCWtZiWm9ognLhGfT7rQ9rj4MhOVkMLTufDmERyNR6O1ZmaPPLUFM6PaGjijK+bYpDfLkDpEhjYDBWH4OcV-HakgZ-wAzH0Z74e+HIHXKIQAEwAdl8Y-FcQhHFtrQ6kFLlvDCo9kErWBRR7+mOxdcmS6vxNJhkklMg1NqFAajLNqMXz9imhKOuqmrAV20x4meu5AkyLoMoSQjmkIWCfF+oy2A0TAgcmFBgNQYB+KmXKvpg-KCnay4UGwABe3rrr4O6liuYIiFotRBLAQiCghUAUEQsCGrU0qwLI4iaoJcZerUZAiDK+T2ue4qSiI0qygU7HKoUEHOqMta4oS+picaIimmI5rTLJR6XoqBlQVAoFhr2ZnepAfoBvGiDxqGgmYJG0blL5iZGYR5EelmGAxRmPZ9uIjpymgsxTEAA Open this visualization in the Vega Editor>

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
grid lines - and the legend options to move the legend from the
same plot to the bottom of the combined visualization, and to
center the title.

<<images/vl/parallaxview.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABGBEAmCGAutIC4yghSBXATgGxSgAt54AHAZ2QHpqdYB3AOgHMBLeIrAIywoFMcAYwD2AO3j8JTUQFtqAERFYWAIVwBrftSIA3fi1jVZsCpJw79h6nETVDbWAFpYzgGwBGNy48AGJ4jcePwesExiIk5E-LDQgkzwFLqQADTgGJAAZiI4JvCEwJBksDgCBZAA4iYshJBiWLLcgqlQZHgAHrX1jc0pUPwA+m2dqHUNTTiQAL5T6VNpmLqiYkIIhADa6RDoGJjw9GIU2bkbW7toZ+dQLDjKZNwAnhuQAMJ4fOaQALoLVxmmz10sHe-BakFEWAkFG+vz+UAhUMI+yw-FhfxgUgoHCeo2GkEuGFmVx+BMgJhwGlqeDYYlBaMwDDY0E4hDcvl89KgAmCQngbHEBQJmGCLCk0HK3Bp4tGIrFYPgDzIoNGWLELGCYMybH4eGg0NQ61e7zMzS+RPO81JUlE0BpNVQOyu4JEeBygrhmC1OulUDeH16QoyFFWGodkGD0VkyvhCAMOQefmmnN2kAVSq6IlkNOBqUDwoMcodeYyOW1Elq3BEpEzuY9GT58FDUAAyjr+LywLAwEJjZ9k+dUxxggBBFZEN2jLPQaAa4vmq6WuGQETFITY90eyBAkGEXxMADM-cwy1tfIFYe3KMIHj64bbvP5YipBbE4vnFv7kBGFzrWW1utqS86WLcMQ2jQpoEzWAaQ2Q8wH3XwfigNNo0gV0alvMQ2CEaNMmBARF03FDagARywWAJA4BA2H0WtN1gdo2H1NA0NgJo8GYvCONRZCh2jeo8Dwd9dkIp0cR-TcvQA0YEUSOil2I0YyIohtqNoo8oAYpjygbJtIAAOXGQQwBETIwDMEpoWEwkCXfUSJJTclKVGPkhEpTlIEZZkiFZdkPOtERbTVDcnVEV1JjDKSfSNf1JlvRSoAiLMxBzW9ZVfQgBKEz8V1gNcFRC9EgN3JhfAAVg051Xw4R9ymK1A9zgu8eTPJ8ZRfN9A3slNv0ddEotqPFKuDYFwJgKCYINOCEKQ1NFVQ9CwSwnDCC4gjKoSyBlMoxA+XUkCtOYwpdNQgAFEpgTwBiwAAChMCgAEppi62yznNElMAAEgjfgTFqEhyCoWgrDCdhOB4Jh+WoH6TGoEGnGpSQ4YAFiYAArCgBV+cNVztXdscrSE9VqTJjR87Hli1e00FmKYgA Open this visualization in the Vega Editor>

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
                      , MLegend [ 'LOrient' 'LOBottom'
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
          . select selName Single [ BindLegend [ BLField \"Cluster\" ] ]

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
            . select selName Single [ BindLegend [ BLField "Cluster" ] ]

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

Other things to do:

 - add some description of magnitude / parallax meanings
   (since talk about astronomy being awkward)

 - tooltips (show multiple fields)

 - for errors section, could look at quantile plots?

-}
