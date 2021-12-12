{-|
Module      : Graphics.Vega.VegaLite
Copyright   : (c) Douglas Burke, 2018-2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings, TupleSections

This is a port of the
<http://package.elm-lang.org/packages/gicentre/elm-vegalite/latest Elm Vega Lite module>,
written by Jo Wood of the giCentre at the City
University of London. It was originally based on version @2.2.1@ but
it has been updated to match later versions.  This module allows users
to create a Vega-Lite specification, targeting __version 4__ of the
<https://vega.github.io/schema/vega-lite/v4.json JSON schema>.
Version 0.12 of @hvega@ supports version 4.15 of the Vega-Lite specification.

Although this was based on the Elm module, there have been a number of
changes - on both sides.

Please see "Graphics.Vega.Tutorials.VegaLite" for an introduction
to using @hvega@ to create visualizations. The
<http://hackage.haskell.org/package/ihaskell-hvega ihaskell-hvega>
package provides an easy way to embed Vega-Lite
visualizations in an IHaskell notebook (using
<https://vega.github.io/vega-lite/usage/embed.html Vega-Embed>).

== Examples

Note that this module exports several symbols that are exported
by the Prelude, such as 'VL.filter', 'VL.lookup',
and 'VL.repeat'; to avoid name clashes it's therefore advised
to either import the module qualified, for example:

@
import qualified Graphics.Vega.VegaLite as VL
@

or to hide the clashing names explicitly:

@
import Prelude hiding (filter, lookup, repeat)
@

In the following examples, we'll assume the latter.

=== Example: viewing columns from a file

The [Vega-Lite example gallery](https://vega.github.io/vega-lite/examples/) contain
a number of visualizations of the \"cars.json\" dataset (and many other
datasets ;-), which has a number of
columns to display, such as \"Horsepower\", \"Miles_per_Gallon\", and \"Origin\". The
following code will create a visualization that plots the efficiency of the
cars (the \"mpg\") as a function of its Horsepower, and color-code by the
origin of the car:

@
let cars =  'VL.dataFromUrl' \"https:\/\/vega.github.io\/vega-datasets\/data\/cars.json\" []

    enc = 'VL.encoding'
            . 'VL.position' 'VL.X' [ 'VL.PName' \"Horsepower\", 'VL.PmType' 'VL.Quantitative' ]
            . 'VL.position' 'VL.Y' [ 'VL.PName' \"Miles_per_Gallon\", 'VL.PmType' 'VL.Quantitative', 'VL.PTitle' \"Miles per Gallon\" ]
            . 'VL.color' [ 'VL.MName' \"Origin\" ]

    bkg = 'VL.background' \"rgba(0, 0, 0, 0.05)\"

in 'VL.toVegaLite' [ bkg, cars, 'VL.mark' 'VL.Circle' ['VL.MTooltip' 'VL.TTEncoding'], enc [] ]
@

When viewed with a Vega-Lite viewer (normally some form of a browser), you
can view the result. For instance:

 - the 'VL.fromVL' function will create the JSON representation of the
   visualization, which can then be passed to a Vega-Lite viewer;

 - a routine like 'VL.toHtmlFile' can be used to create a HTML file
   that will display the visualization using the
   <https://github.com/vega/vega-embed Vega-Embed> Javascript library;

 - users of the @Jupyter notebook@ environment can make use of the
   automatic display of the `VL.VegaLite` type, using @ihaskell-hvega@,
   to view an in-browser version of the plot (generated via Vega-Embed);

 - and users of @Jupyter lab@ can use the @vlShow@ method (from @ihaskell-hvega@),
   but be aware that it is currently limited to only supporing features
   from Vega-Lite version 2.

The visualization can be viewed in the Vega Editor, which lets you
interact with the plot and modify its contents, as shown for
<https://vega.github.io/editor/#/url/vega-lite/N4IgtghgTg1iBcoAuB7FAbJBLADg0AxigHZICmpCIFRAJlsQOYgC+ANCEgJ45lUFYoBdH3YhaEJBHwgArlHRUAFkiQ4AzvAD0WgG5lGEAHSMsSJbIBGRrCj0GIAWglT1ZJOq0uIWgtHVGAFbqJKwcACTqBEpkkMqqGtr2hiZmFta2WlExkMlO6GZkegAsQSHEIBw0KPRMMkToKFAyAGZYZOi0VADyUFimFWIAHq3tnVQAEk1uOCgA7mTNHNy8VACOshCkZpJY+mEgXKMdXfAgALJYIuoA+rxQNwDiEOiNFctmIlSX1wAE979nq9QsseHwzhsttgpNh9iwxJYIAQYIwoChZMRTiAoIxEQAKAAMbF+RJJxIJRgJAFYAJSsIA this example>.

It can also be viewed as a PNG version:

<<images/example-car.png>>

=== Example: faceting, data transformation, and interaction

The following example is rather lengthy, as it includes
data tranformation (sub-setting the data and creating a
new column), automatic faceting (that is, creating separate plots
for unique values of a data column), interactive elements
(the ability to filter a plot by selecting a subset in
another element), and some basic configuration and styling
(primarily to change the text sizes). The
"Graphics.Vega.Tutorials.VegaLite" tutorial should be
reviewed to understand how the plot works!

It's aim is to show the recent community measurements of the
brightness of
<https://en.wikipedia.org/wiki/Betelgeuse the star Betelgeuse>,
which caused much interest in the Astronomical world at the
start of 2020 as it became much fainter than normal
(although it is massive enough to go supernova, it is not
expected to happen for quite a while yet). The data shown
is based on data collated by the
<https://www.aavso.org/ AAVSO>, and converted to JSON format,
with the primary columns of interest being \"@jd@\" (the
date of the observation, in the
<https://en.wikipedia.org/wiki/Julian_day Julian day> system),
\"@magnitude@\" (the brightness of the star, reported
as an
<https://en.wikipedia.org/wiki/Apparent_magnitude apparent magnitude>),
and \"@filterName\"@ (the filter through which the measurement was
made). For display purposes we are only going to use the
\"@Vis.@\" and \"@V@\" filters (the former is a by-eye estimate,
which is less accurate but has the advantage of having been used
for a long time, and the second is measured from a in image
taken by a <https://en.wikipedia.org/wiki/Charge-coupled_device CCD detector>,
which is more accurate and repeatable, but more costly to obtain),
and the date field is going to be converted into the number of
days since the start of 2020 (via a little bit of subtraction).
For \"historical reasons\", the magnitude system used by Astronomers
to measure how bright a system is reversed, so that larger magnitudes
mean fainter systems. For this reason, the magnitude axis is reversed
in this visualization, so that as Betelgeuse dims the values drop.

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}

betelgeuse =
  let desc = \"How has Betelgeuse's brightness varied, based on data collated by AAVSO (https:\/\/www.aavso.org\/). \" ++
             \"You should also look at https:\/\/twitter.com\/betelbot and https:\/\/github.com\/hippke\/betelbot. \" ++
             \"It was all the rage on social media at the start of 2020.\"

      titleStr = \"Betelegeuse's magnitude measurements, collated by AAVSO\"

      -- height and width of individual plots (in pixels)
      w = 'VL.width' 600
      h = 'VL.height' 150

      -- Define the properties used for the "position" channels. For this example
      -- it makes sense to define as functions since they are used several times.
      --
      pos1Opts fld ttl = ['VL.PName' fld, 'VL.PmType' 'VL.Quantitative', 'VL.PTitle' ttl]
      x1Opts = pos1Opts \"days\" \"Days since January 1, 2020\"
      y1Opts = pos1Opts \"magnitude\" \"Magnitude\" ++ ['VL.PSort' ['VL.Descending'], y1Range]
      y1Range = 'VL.PScale' ['VL.SDomain' ('VL.DNumbers' [-1, 3])]

      -- The filter name is used as a facet, but also to define the
      -- color and shape of the points.
      --
      filtOpts = ['VL.MName' \"filterName\"]
      filtEnc = 'VL.color' ('VL.MLegend' ['VL.LTitle' \"Filter\", 'VL.LTitleFontSize' 16, 'VL.LLabelFontSize' 14] : filtOpts)
                . 'VL.shape' filtOpts

      -- In an attempt to make the V filter results visible, I have chosen
      -- to use open symbols. It doesn't really work out well.
      --
      circle = 'VL.mark' 'VL.Point' ['VL.MOpacity' 0.5, 'VL.MFilled' False]

      -- What is plotted in the "overview" plot?
      --
      encOverview = 'VL.encoding'
                    . 'VL.position' 'VL.X' x1Opts
                    . 'VL.position' 'VL.Y' y1Opts
                    . filtEnc

      -- Select roughly the last year's observations (roughly the length of
      -- time that Betelgeuse is visible)
      --
      xlim = ('VL.Number' (-220), 'VL.Number' 100)
      ylim = ('VL.Number' (-0.5), 'VL.Number' 2.5)
      overview = 'VL.asSpec' [ w
                        , h
                        , encOverview []
                        , 'VL.selection'
                          . 'VL.select' selName 'VL.Interval' ['VL.Encodings' ['VL.ChX', 'VL.ChY']
                                                    , 'VL.SInitInterval' (Just xlim) (Just ylim)
                                                    ]
                          $ []
                        , circle
                        ]

      -- What is plotted in the "detail" plot?
      --
      selName = \"brush\"
      pos2Opts fld = [ 'VL.PName' fld, 'VL.PmType' 'VL.Quantitative', 'VL.PNoTitle'
                     , 'VL.PScale' ['VL.SDomainOpt' ('VL.DSelectionField' selName fld)] ]
      x2Opts = pos2Opts \"days\"
      y2Opts = pos2Opts \"magnitude\" ++ ['VL.PSort' ['VL.Descending']]

      encDetail = 'VL.encoding'
                  . 'VL.position' 'VL.X' x2Opts
                  . 'VL.position' 'VL.Y' y2Opts
                  . filtEnc

      detail = 'VL.asSpec' [ w
                      , h
                      , encDetail []
                      , circle
                      ]

      -- Control the labelling of the faceted plots. Here we move the
      -- label so that it appears at the top-right corner of each plot
      -- and remove the title.
      --
      headerOpts = [ 'VL.HLabelFontSize' 16
                   , 'VL.HLabelAlign' 'VL.AlignRight'
                   , 'VL.HLabelAnchor' 'VL.AEnd'
                   , 'VL.HLabelPadding' (-24)
                   , 'VL.HNoTitle'
                   , 'VL.HLabelExpr' \"'Filter: ' + datum.label\"
                   ]

      -- The "detail" plot has multiple rows, one for each filter.
      --
      details = 'VL.asSpec' [ 'VL.columns' 1
                       , 'VL.facetFlow' [ 'VL.FName' \"filterName\"
                                   , 'VL.FHeader' headerOpts
                                   ]
                       , 'VL.spacing' 10
                       , 'VL.specification' detail
                       ]

  in 'VL.toVegaLite' [ 'VL.description' desc
                , 'VL.title' titleStr ['VL.TFontSize' 18]
                , 'VL.dataFromUrl' \"https:\/\/raw.githubusercontent.com\/DougBurke\/hvega\/master\/hvega\/data\/betelgeuse-2020-03-19.json\" []
                , 'VL.transform'
                  -- concentrate on the two filters with a reasonable number of points
                  . 'VL.filter' ('VL.FExpr' \"datum.filterName[0] === \'V\'\")
                  -- remove some \"outliers\"
                  . 'VL.filter' ('VL.FExpr' \"datum.magnitude < 4\")
                  -- subtract Jan 1 2020 (start of day, hence the .0 rather than .5)
                  . 'VL.calculateAs' \"datum.jd - 2458849.0\" \"days\"
                  $ []
                , 'VL.vConcat' [overview, details]
                , 'VL.configure'
                  -- Change axis titles from bold- to normal-weight,
                  -- and increase the size of the labels
                  . 'VL.configuration' ('VL.Axis' ['VL.TitleFontWeight' 'VL.Normal', 'VL.TitleFontSize' 16, 'VL.LabelFontSize' 14])
                  $ []
                ]

@

The PNG version shows the basic features:

<<images/example-betelgeuse.png>>

However this is missing the interactive elements of the visualization, primarily
selection and zooming in the top plot changes the axis ranges of the bottom two
plots. This interactivity requires a Vega-Lite viewer such as
<https://vega.github.io/editor/#/url/vega-lite/N4IgLgTghgdgzgMwPYQLYgFwG1QIJYA2YAphJiACZRgCuqAdPkaQHJSrFYAMAugAQBeIXwDkANREgAvgBpchEmQyVqdeqigBzGHloVifADx8ALNLkgocclQCe1mSADGUAk5oFqxG6oYArCj4AWj4AJhMAVgAOKJMATnouaR5HJyQYfE1MUCgADzxrDFAwXQJiADF0sAB1YjxNAAswchgUDQIQR08AI2ICSpgwAGU8AC9vDABGE0cSsDKB4bGJyYA2KVkVMChskBoIDuUmsAAHOAwAegvoAHd6TV0Gmm6aOFI0weJB+jTUC4ARJA0TQAIX2AGtiBcGgA3YiaKAXDRwRTQuEIi5UbYXXokAiaYivYhBUJcUlBLgAZiCkwSfjg6XMIBhHxczWwoAadUa7MmES4jg0EHBuyQJygTl0tkwXHoEVmthOExAJyQeEGnRATDKFEwCFcb02NzwFDADUwqy4ApAbzKThKjKKIG6EFe5qd6t0u1ymCwJNJMkmVpSIGl2ApcpkoTlPE2XzSFHVmkKWBAPsc0pDYEVyvVihhrmkcZgCaTuzSBBQu3wfV1yiYijYHE1ZQJMDroB6fUWI3GmGms1KyvKClImrmCyqvZW602cAaUCV1bwtfIDdY7G8mx9TprBDrKns46H5H+UHsfDg6qcBgAUrAaFAILY+JMZGEyUkFUvlABHR+DLo1B4HCTJhvIq7KBo2i6DQ+ianALhlLsFBIBo6q+jSMiUrGjgMhA7KUMQiFfImMBZIO8zKgAsloOh6N437Kv+sBzMBoEbLIoBwEqTi7Fy9RNP2-KCk+IpOmKEpSjKkbgDm5CqnmmrasQdb6gQhqOMaprupa1rxkgZFZE6FZVruK77muo4QE2jEgK2pG7F2-RTss-YzOAJ7KCOzBkJRk6DNO-aznhC4-hBln1tZtlMjuEUHnYDg2khEygKh6EwMukGHkltrEPaeCOs6rrzkW-kTDAHgEEx5AsYB2wlBxGZZZFIDQfRcF2YhripZQaFQBh5nZe1sHwXhfT5Q6mXKC6bplTaKCEfoJHtmW5WYJVBDVXJP4gHVbGNVuGypEgBB0PA-aOPqN7svFVm+TFjhclA+hKJ2UC9AQACCJYNGZICOV0H19AAorkJxKCAIg+YoGCiHwADUfBYmozktsDBAAAovUZmAkh5zk9m5UyrOtGCbdtzlfQQ9TTSAECCc0x02uKkrkf2XBSCGAAkiFcho5DHGclwXOiUD3I8zz0IVFx88QGii-CUBBDTJCiyY9D0oyZPFMQuSESCxB4vChJvCIcB8CNDGW8QVj7PLXxgHA74Vp4JCBN0L5fV9YhDAA8sprl9lMUSbMtTgMycU3kAAEkgNx8AuFuG3iBJEubfAuozMDERbBYM6p77dFYql8OkyPUFAfCu14Htez7-t8AAFEL5xXDcHf0FAUAwgy9AoJoFwAJT0HwACaQKXn9HiBAaSB8JWSDgnw1CJ2ApxtxcYDGuvpA-GhOJG303RIGAK-tmvG8iw8ZpS780J4CcJyQofeIn2Ao8AJJnzcVgr1tfAzQGGgASMuMBLxIElK4G2iYq6ryAZebYBEy4IA-KSeg0ggA the Vega Editor>.

-}

module Graphics.Vega.VegaLite
       (
         -- * Creating a Vega-Lite Specification

         VL.toVegaLite
       , VL.toVegaLiteSchema
       , VL.vlSchema2, VL.vlSchema3, VL.vlSchema4, VL.vlSchema
       , VL.fromVL
       , VL.VLProperty(..)
       , VL.VLSpec
       , VL.VegaLite
       , VL.PropertySpec
       , VL.LabelledSpec
       , VL.EncodingSpec
       , VL.toEncodingSpec
       , VL.fromEncodingSpec
       , VL.TransformSpec
       , VL.toTransformSpec
       , VL.fromTransformSpec
       , VL.ResolveSpec
       , VL.toResolveSpec
       , VL.fromResolveSpec
       , VL.SelectSpec
       , VL.toSelectSpec
       , VL.fromSelectSpec
       , VL.ConfigureSpec
       , VL.toConfigureSpec
       , VL.fromConfigureSpec
       , VL.BuildEncodingSpecs
       , VL.BuildTransformSpecs
       , VL.BuildResolveSpecs
       , VL.BuildSelectSpecs
       , VL.BuildConfigureSpecs
       , VL.Angle
       , VL.Color
       , VL.DashStyle
       , VL.DashOffset
       , VL.FieldName
       , VL.GradientCoord
       , VL.GradientStops
       , VL.Opacity
       , VL.SelectionLabel
       , VL.StyleLabel
       , VL.VegaExpr
       , VL.ZIndex
       , VL.toHtml
       , VL.toHtmlFile
       , VL.toHtmlWith
       , VL.toHtmlFileWith

         -- * Creating the Data Specification
         --
         -- $dataspec

       , VL.dataFromUrl
       , VL.dataFromColumns
       , VL.dataFromRows
       , VL.dataFromJson
       , VL.dataFromSource
       , VL.dataName
       , VL.datasets
       , VL.dataColumn
       , VL.dataRow
       , VL.noData
       , VL.Data
       , VL.DataColumn
       , VL.DataRow

         -- ** Geographic Data

       , VL.geometry
       , VL.geoFeatureCollection
       , VL.geometryCollection
       , VL.Geometry(..)

       -- ** Data Generators
       --
       -- $datagen

       , VL.dataSequence
       , VL.dataSequenceAs
       , VL.sphere
       , VL.graticule
       , VL.GraticuleProperty(..)

       -- ** Formatting Input Data
       --
       -- $dataformat

       , VL.Format(..)
       , VL.DataType(..)

         -- * Creating the Transform Specification
         --
         -- $transform

       , VL.transform

         -- ** Map Projections
         --
         -- $projections

       , VL.projection
       , VL.ProjectionProperty(..)
       , VL.Projection(..)
       , VL.ClipRect(..)

         -- ** Aggregation
         --
         -- $aggregation

       , VL.aggregate
       , VL.joinAggregate
       , VL.opAs
       , VL.timeUnitAs
       , VL.Operation(..)

         -- ** Binning
         --
         -- $binning

       , VL.binAs
       , VL.BinProperty(..)

         -- ** Stacking
         --
         -- $stacking

       , VL.stack
       , VL.StackProperty(..)
       , VL.StackOffset(..)

         -- ** Data Calculation
         --
         -- $calculate

       , VL.calculateAs

         -- ** Filtering
         --
         -- $filtering

       , VL.filter
       , VL.Filter(..)
       , VL.FilterRange(..)

         -- ** Flattening
         --
         -- $flattening

       , VL.flatten
       , VL.flattenAs

         -- ** Folding and Pivoting
         --
         -- $foldpivot

       , VL.fold
       , VL.foldAs
       , VL.pivot
       , VL.PivotProperty(..)

         -- ** Relational Joining (lookup)
         --
         -- $joining

       , VL.lookup
       , VL.lookupSelection
       , VL.LookupFields(..)
       , VL.lookupAs

         -- ** Data Imputation
         --
         -- $imputation

       , VL.impute
       , VL.ImputeProperty(..)
       , VL.ImMethod(..)

         -- ** Data sampling
         --
         -- $sampling

       , VL.sample

         -- ** Density Estimation
         --
         -- $density

       , VL.density
       , VL.DensityProperty(..)

         -- ** Loess Trend Calculation
         --
         -- $loess

       , VL.loess
       , VL.LoessProperty(..)

         -- ** Regression Calculation
         --
         -- $regression

       , VL.regression
       , VL.RegressionProperty(..)
       , VL.RegressionMethod(..)

         -- ** Qualtile Calculation
         --
         -- $quantile

       , VL.quantile
       , VL.QuantileProperty(..)

         -- ** Window Transformations
         --
         -- $window

       , VL.window
       , VL.Window(..)
       , VL.WOperation(..)
       , VL.WindowProperty(..)

         -- * Creating the Mark Specification
         --
         -- $markspec

       , VL.mark
       , VL.Mark(..)

         -- ** Mark properties
         --
         -- $markproperties

       , VL.MarkProperty(..)
       , VL.StrokeCap(..)
       , VL.StrokeJoin(..)

         -- *** Used by Mark Properties

       , VL.Orientation(..)
       , VL.MarkInterpolation(..)
       , VL.Symbol(..)
       , VL.PointMarker(..)
       , VL.LineMarker(..)
       , VL.MarkErrorExtent(..)
       , VL.TooltipContent(..)
       , VL.ColorGradient(..)
       , VL.GradientProperty(..)
       , VL.TextDirection(..)
       , VL.BlendMode(..)

         -- ** Cursors
         --
         -- $cursors

       , VL.Cursor(..)

         -- * Creating the Encoding Specification
         --
         -- $encoding

       , VL.encoding
       , VL.Measurement(..)

         -- ** Position Channels
         --
         -- $position

       , VL.position
       , VL.Position(..)

         -- *** Position channel properties

       , VL.PositionChannel(..)

         -- ** Sorting properties
         --
         -- $sortprops

       , VL.SortProperty(..)
       , VL.SortField(..)

         -- ** Axis properties
         --
         -- $axisprops

       , VL.AxisProperty(..)
       , VL.ConditionalAxisProperty(..)

         -- ** Positioning Constants
         --
         -- *** Alignment

       , VL.HAlign(..)
       , VL.VAlign(..)
       , VL.BandAlign(..)

         -- *** Overlapping text

       , VL.OverlapStrategy(..)

         -- *** Legends

       , VL.Side(..)

         -- ** Mark channels
         --
         -- $markprops

       , VL.angle
       , VL.color
       , VL.fill
       , VL.fillOpacity
       , VL.opacity
       , VL.shape
       , VL.size
       , VL.stroke
       , VL.strokeDash
       , VL.strokeOpacity
       , VL.strokeWidth

         -- *** Mark Channel properties

       , VL.MarkChannel(..)

         -- *** Mark Legends
         --
         -- $marklegends

       , VL.LegendType(..)
       , VL.LegendProperty(..)
       , VL.LegendOrientation(..)
       , VL.LegendValues(..)

         -- ** Text Channels
         --
         -- $textchannels

       , VL.text
       , VL.tooltip
       , VL.tooltips
       , VL.TextChannel(..)
       , VL.FontWeight(..)

         -- ** Hyperlink Channels
         --
         -- $hyperlink

       , VL.hyperlink
       , VL.HyperlinkChannel(..)

         -- ** URL Channel
         --
         -- $urlchannel

       , VL.url

         -- ** Order Channel
         --
         -- $order

       , VL.order
       , VL.OrderChannel(..)

         -- ** Facet Channel
         --
         -- $facet

       , VL.row
       , VL.column

         -- ** Level of detail Channel
         --
         -- $detail

       , VL.detail
       , VL.DetailChannel(..)

         -- ** Aria Description Channel
         --

       , VL.ariaDescription
       , VL.AriaDescriptionChannel(..)

         -- ** Scaling
         --
         -- $scaling

       , VL.ScaleProperty(..)
       , VL.Scale(..)
       , VL.categoricalDomainMap
       , VL.domainRangeMap
       , VL.ScaleDomain(..)
       , VL.DomainLimits(..)
       , VL.ScaleRange(..)
       , VL.ScaleNice(..)
       , VL.NTimeUnit(..)

         -- *** Color scaling
         --
         -- $color

       , VL.CInterpolate(..)

         -- * Creating view compositions
         --
         -- $view

       , VL.layer
       , VL.vlConcat
       , VL.columns
       , VL.hConcat
       , VL.vConcat
       , VL.align
       , VL.alignRC
       , VL.spacing
       , VL.spacingRC
       , VL.center
       , VL.centerRC
       , VL.bounds
       , VL.Bounds(..)
       , VL.CompositionAlignment(..)

         -- ** Resolution
         --
         -- $resolution

       , VL.resolve
       , VL.resolution
       , VL.Resolve(..)
       , VL.Channel(..)
       , VL.Resolution(..)

         -- ** Faceted views
         --
         -- $facetview

       , VL.repeat
       , VL.repeatFlow
       , VL.RepeatFields(..)
       , VL.facet
       , VL.facetFlow
       , VL.FacetMapping(..)
       , VL.FacetChannel(..)
       , VL.asSpec
       , VL.specification
       , VL.Arrangement(..)

         -- *** Facet Headers
         --
         -- $facetheaders

       , VL.HeaderProperty(..)

         -- * Creating Selections for Interaction
         --
         -- $selections

       , VL.selection
       , VL.select
       , VL.Selection(..)
       , VL.SelectionProperty(..)
       , VL.Binding(..)
       , VL.BindLegendProperty(..)
       , VL.InputProperty(..)
       , VL.SelectionMarkProperty(..)

       -- ** Selection Resolution
       --
       -- $selectionresolution

       , VL.SelectionResolution(..)

         -- ** Making conditional channel encodings
         --
         -- $conditional

       , VL.BooleanOp(..)

         -- * Top-level Settings
         --
         -- $toplevel

       , VL.name
       , VL.description
       , VL.height
       , VL.heightOfContainer
       , VL.heightStep
       , VL.width
       , VL.widthOfContainer
       , VL.widthStep
       , VL.padding
       , VL.autosize
       , VL.background
       , VL.usermetadata
       , VL.Padding(..)
       , VL.Autosize(..)

         -- *** Title
         --
         -- $title

       , VL.title

         -- *** View Backgroud
         --
         -- $viewbackground

       , VL.viewBackground
       , VL.ViewBackground(..)

         -- ** Style Setting
         --
         -- $configure

       , VL.configure
       , VL.configuration
       , VL.ConfigurationProperty(..)

         -- ** Axis Configuration Options
         --
         -- $axisconfig

       , VL.AxisConfig(..)
       , VL.AxisChoice(..)

         -- ** Legend Configuration Options
         --

       , VL.LegendConfig(..)
       , VL.LegendLayout(..)
       , VL.BaseLegendLayout(..)

         -- ** Scale Configuration Options
         --
         -- $scaleconfig

       , VL.ScaleConfig(..)

         -- ** Scale Range Configuration Options
         --
         -- $scalerangeconfig

       , VL.RangeConfig(..)

         -- ** Title Configuration Options
         --
         -- $titleconfig

       , VL.TitleConfig(..)
       , VL.TitleFrame(..)

         -- ** View Configuration Options
         --
         -- $viewconfig

       , VL.ViewConfig(..)
       , VL.APosition(..)
       , VL.FieldTitleProperty(..)

         -- ** Composition Configuration Options
         --
         -- $compositionconfig

       , VL.CompositionConfig(..)

         -- * General Data types
         --
         -- $generaldatatypes

       , VL.DataValue(..)
       , VL.DataValues(..)

         -- ** Temporal data
         --
         -- $temporaldata

       , VL.DateTime(..)
       , VL.MonthName(..)
       , VL.DayName(..)
       , VL.TimeUnit(..)
       , VL.BaseTimeUnit(..)

         -- * Update notes
         --
         -- $update

         -- ** Version 0.12
         --
         -- $update01202
         -- $update01201
         -- $update01200
         
         -- ** Version 0.11
         --
         -- $update01101
         -- $update01100

         -- ** Version 0.10
         --
         -- $update01000

         -- ** Version 0.9
         --
         -- $update0910
         --
         -- $update0900

         -- ** Version 0.8
         --
         -- $update0800

         -- ** Version 0.7
         --
         -- $update0700

         -- ** Version 0.6
         --
         -- $update0600

         -- ** Version 0.5
         --
         -- $update0500

         -- ** Version 0.4
         --
         -- $update0400

        )
    where

-- VegaLite redefined several prelude functions, such as filter and
-- repeat, so hide the prelude so the documentation links work.
--
import Prelude ()

-- There has been some attempt to separate out the types based
-- on functionality, but it is somewhat hap-hazard. Most of
-- the fuctionality is in the Core and Foundation modules
-- (ie they are the ones that could perhaps be further split up).
--
import qualified Graphics.Vega.VegaLite.Configuration as VL
import qualified Graphics.Vega.VegaLite.Core as VL
import qualified Graphics.Vega.VegaLite.Data as VL
import qualified Graphics.Vega.VegaLite.Foundation as VL
import qualified Graphics.Vega.VegaLite.Geometry as VL
import qualified Graphics.Vega.VegaLite.Input as VL
import qualified Graphics.Vega.VegaLite.Legend as VL
import qualified Graphics.Vega.VegaLite.Mark as VL
import qualified Graphics.Vega.VegaLite.Output as VL
import qualified Graphics.Vega.VegaLite.Scale as VL
import qualified Graphics.Vega.VegaLite.Selection as VL
import qualified Graphics.Vega.VegaLite.Specification as VL
import qualified Graphics.Vega.VegaLite.Time as VL
import qualified Graphics.Vega.VegaLite.Transform as VL


-- Documentation

-- $dataspec
-- Functions and types for declaring the input data to the
-- visualization. See the
-- [Vega-Lite documentation](https://vega.github.io/vega-lite/docs/data.html#format).

-- $datagen
-- Functions that create new data sources.

-- $dataformat
-- See the Vega-Lite
-- [format](https://vega.github.io/vega-lite/docs/data.html#format) and
-- [JSON](https://vega.github.io/vega-lite/docs/data.html#json) documentation.

-- $transform
-- Functions and types for declaring the transformation rules that
-- are applied to data fields or geospatial coordinates before they
-- are encoded visually.
--
-- In version @0.5.0.0@ the 'VL.TransformSpec' type was introduced to
-- make it clear what functions can be used with 'VL.transform'.

-- $projections
-- See the
-- [Vega-Lite map projection documentation](https://vega.github.io/vega-lite/docs/projection.html).

-- $aggregation
-- See the
-- [Vega-Lite aggregate documentation](https://vega.github.io/vega-lite/docs/aggregate.html).

-- $binning
-- See the
-- [Vega-Lite binning documentation](https://vega.github.io/vega-lite/docs/bin.html).

-- $stacking
-- See the [Vega-Lite stack documentation](https://vega.github.io/vega-lite/docs/stack.html).

-- $calculate
-- See the
-- [Vega-Lite calculate documentation](https://vega.github.io/vega-lite/docs/calculate.html).

-- $filtering
-- See the
-- [Vega-Lite filter documentation](https://vega.github.io/vega-lite/docs/filter.html).

-- $flattening
-- See the Vega-Lite [flatten](https://vega.github.io/vega-lite/docs/flatten.html)
-- documentation.

-- $foldpivot
-- Data tidying operations that reshape the rows and columns of a dataset.
-- See the Vega-Lite [fold](https://vega.github.io/vega-lite/docs/fold.html) and
-- [pivot](https://vega.github.io/vega-lite/docs/pivot.html) documentation.

-- $joining
-- Create lookups between data tables in order to join values from
-- multiple sources. See the
-- [Vega-Lite lookup documentation](https://vega.github.io/vega-lite/docs/lookup.html).

-- $imputation
-- Impute missing data. See the
-- [Vega-Lite impute documentation](https://vega.github.io/vega-lite/docs/impute.html#transform).

-- $sampling
-- See the [Vega-Lite sample documentation](https://vega.github.io/vega-lite/docs/sample.html).

-- $density
-- See the [Vega-Lite density documentation](https://vega.github.io/vega-lite/docs/density.html).

-- $loess
-- See the [Vega-Lite loess documentation](https://vega.github.io/vega-lite/docs/loess.html).

-- $regression
-- See the [Vega-Lite regression documentation](https://vega.github.io/vega-lite/docs/regression.html).

-- $quantile
-- See the [Vega-Lite quantile documentation](https://vega.github.io/vega-lite/docs/quantile.html).

-- $window
-- See the Vega-Lite
-- [window transform field](https://vega.github.io/vega-lite/docs/window.html#field-def)
-- and
-- [window transform](https://vega.github.io/vega-lite/docs/window.html#window-transform-definition)
-- documentation.

-- $markspec
-- Types and functions for declaring the type of visual
-- marks used in the visualization.

-- $markproperties
-- See the Vega-Lite
-- [general mark](https://vega.github.io/vega-lite/docs/mark.html#general-mark-properties),
-- [area mark](https://vega.github.io/vega-lite/docs/area.html#properties),
-- [bar mark](https://vega.github.io/vega-lite/docs/bar.html#properties),
-- [boxplot](https://vega.github.io/vega-lite/docs/boxplot.html#properties),
-- [circle mark](https://vega.github.io/vega-lite/docs/circle.html#properties),
-- [error band](https://vega.github.io/vega-lite/docs/errorband.html#properties),
-- [error bar](https://vega.github.io/vega-lite/docs/errorbar.html#properties),
-- [hyperlink mark](https://vega.github.io/vega-lite/docs/mark.html#hyperlink),
-- [line mark](https://vega.github.io/vega-lite/docs/line.html#properties),
-- [point mark](https://vega.github.io/vega-lite/docs/point.html#properties),
-- [square mark](https://vega.github.io/vega-lite/docs/square.html#properties),
-- [text mark](https://vega.github.io/vega-lite/docs/text.html#properties) and
-- [tick mark](https://vega.github.io/vega-lite/docs/tick.html#properties)
-- property documentation.

-- $cursors
-- See the
-- [CSS cursor documentation](https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Keyword%20values)

-- $encoding
-- Types and functions for declaring which data fields are mapped to which
-- channels. Channels can include: position on screen (e.g. 'VL.X', 'VL.Y'); visual
-- mark properties ('VL.color', 'VL.size', 'VL.stroke', 'VL.shape'); 'VL.text'; 'VL.hyperlink';
-- ordering ('VL.order'); level of 'VL.detail'; and facets for composed
-- visualizations ('VL.facet'). All can be further customised via a series of
-- properties that determine how the encoding is implemented (such as
-- scaling, sorting, and spacing).
--
-- In version @0.5.0.0@ the 'VL.EncodingSpec' type was introduced to
-- make it clear what functions can be used with 'VL.encoding'.

-- $position
-- Control where items appear in the visualization. See the
-- [Vega-Lite position documentation](https://vega.github.io/vega-lite/docs/encoding.html#position).

-- $sortprops
-- See the
-- [Vega-Lite sort documentation](https://vega.github.io/vega-lite/docs/sort.html).

-- $axisprops
-- See the
-- Vega-Lite axis property documentation](https://vega.github.io/vega-lite/docs/axis.html#axis-properties).

-- $markprops
-- Control the appearance of the visual marks in the visualization
-- (e.g. 'VL.color' and 'VL.size').

-- $marklegends
-- See the
-- [Vega-Lite legend property documentation](https://vega.github.io/vega-lite/docs/legend.html#legend-properties).

-- $textchannels
-- Control the appearance of the text and tooltip elements in the visualization.

-- $hyperlink
-- Channels which offer a clickable URL destination. Unlike most other
-- channels, the hyperlink channel has no direct visual expression other than the
-- option of changing the cursor style when hovering, so an encoding will usually
-- pair hyperlinks with other visual channels such as marks or texts.

-- $urlchannel
-- Data-driven URL used for 'VL.Image' specification: a data field can contain
-- URL strings defining the location of image files, or the URL can be
-- given directly.

-- $order
-- Channels that relate to the order of data fields such as for sorting stacking order
-- or order of data points in a connected scatterplot. See the
-- <https://vega.github.io/vega-lite/docs/encoding.html#order Vega-Lite documentation>
-- for further details.

-- $facet
-- Channels for faceting single plots into small multiples. Can be used to create
-- trellis plots or other arrangements in rows and columns. See the
-- <https://vega.github.io/vega-lite/docs/encoding.html#facet Vega-Lite documentation>
-- for further details. See also, <#facetview faceted views> for a more flexible (but
-- more verbose) way of defining faceted views.

-- $detail
-- Used for grouping data but without changing the visual appearance of a mark. When,
-- for example, a field is encoded by color, all data items with the same value for
-- that field are given the same color. When a detail channel encodes a field, all
-- data items with the same value are placed in the same group. This allows, for example
-- a line chart with multiple lines to be created – one for each group. See the
-- <https://vega.github.io/vega-lite/docs/encoding.html#detail Vega-Lite documentation>
-- for more information.

-- NOTE: at present we don't support keyChannel, but leave in in case we
--       ever do.
--
-- ** Dynamic data
--
-- $dyndata
-- Dynamic data (adding or changing values in an existing visualization)
-- is possible with Vega Lite via the
-- <https://vega.github.io/vega/docs/api/view/#data-and-scales Vega View data streaming API>.
-- There is no direct support for this in @hvega@,
-- which only handles setting up the original visualization, but
-- the 'VL.keyChannel' enconding may be useful.

-- $scaling
-- Used to specify how the encoding of a data field should be applied. See the
-- [Vega-Lite scale documentation](https://vega.github.io/vega-lite/docs/scale.html).

-- $color
-- For color interpolation types, see the
-- [Vega-Lite continuous scale documentation](https://vega.github.io/vega-lite/docs/scale.html#continuous).

-- $view
-- Views can be combined to create more complex multiview displays. This may involve
-- layering views on top of each other (superposition) or laying them out in adjacent
-- spaces (juxtaposition using 'VL.repeat', 'VL.repeatFlow', 'VL.facet', 'VL.facetFlow',
-- 'VL.vlConcat', 'VL.hConcat', or 'VL.vConcat'). Where different views have potentially conflicting
-- channels (for example, two position scales in a layered visualization) the rules for
-- resolving them can be defined with 'VL.resolve'. For details of creating composite views see the
-- <https://vega.github.io/vega-lite/docs/composition.html Vega-Lite documentation>.

-- $resolution
-- Control the independence between composed views.
--
-- See the [Vega-Lite resolve documentation](https://vega.github.io/vega-lite/docs/resolve.html).

-- $facetview
-- #facetview#
-- These are small multiples each of which show subsets of the same dataset. The specification
-- determines which field should be used to determine subsets along with their spatial
-- arrangement (in rows or columns). For details see the
-- <https://vega.github.io/vega-lite/docs/facet.html Vega-Lite documentation>.

-- $facetheaders
-- See the
-- [Vega-Lite header documentation](https://vega.github.io/vega-lite/docs/header.html).

-- $selections
-- Selections are the way in which interactions (such as clicking or dragging) can be
-- responded to in a visualization. They transform interactions into data queries.
-- For details, see the
-- <https://vega.github.io/vega-lite/docs/selection.html Vega-Lite documentation>.

-- $selectionresolution
-- Determines how selections are made across multiple views.
-- See the [Vega-lite resolve selection documentation](https://vega.github.io/vega-lite/docs/selection.html#resolve).

-- $conditional
-- To make channel encoding conditional on the result of some interaction, use
-- 'VL.MSelectionCondition', 'VL.TSelectionCondition', or 'VL.HSelectionCondition'. Similarly
-- 'VL.MDataCondition', 'VL.TDataCondition', or 'VL.HDataCondition' will encode a mark
-- conditionally depending on some data properties such as whether a datum is null
-- or an outlier.
--
-- For interaction, once a selection has been defined and named, supplying a set of
-- encodings allow mark encodings to become dependent on that selection.
-- 'VL.MSelectionCondition' is followed firstly a (Boolean) selection and then an
-- encoding if that selection is true and another encoding to be applied if it is false.
-- The color specification below states \"whenever data marks are selected with an
-- interval mouse drag, encode the cylinder field with an ordinal color scheme,
-- otherwise make them grey\":
--
-- @
-- sel = 'VL.selection' . 'VL.select' "myBrush" 'VL.Interval' []
--
-- enc = 'VL.encoding'
--         . 'VL.position' 'VL.X' [ 'VL.PName' \"Horsepower\", 'VL.PmType' 'VL.Quantitative' ]
--         . 'VL.position' 'VL.Y' [ 'VL.PName' \"Miles_per_Gallon\", 'VL.PmType' Quantitative ]
--         . 'VL.color'
--             [ 'VL.MSelectionCondition' ('VL.SelectionName' "myBrush")
--                 [ 'VL.MName' \"Cylinders\", 'VL.MmType' 'VL.Ordinal' ]
--                 [ 'VL.MString' "grey" ]
--             ]
-- @
--
-- In a similar way, 'VL.MDataCondition' will encode a mark depending on whether any
-- predicate tests are satisfied. Unlike selections, multiple conditions and associated
-- encodings can be specified. Each test condition is evaluated in order and only on
-- failure of the test does encoding proceed to the next test. If no tests are true,
-- the encoding in the final parameter is applied in a similar way to @case of@
-- expressions:
--
-- @
-- enc = 'VL.encoding'
--         . 'VL.position' 'VL.X' [ 'VL.PName' \"value\", 'VL.PmType' 'VL.Quantitative' ]
--           . 'VL.color'
--               [ 'VL.MDataCondition'
--                    [ ( 'VL.Expr' "datum.value < 40", [ 'VL.MString' "blue" ] )
--                    , ( 'VL.Expr' "datum.value < 50", [ 'VL.MString' "red" ] )
--                    , ( 'VL.Expr' "datum.value < 60", [ 'VL.MString' "yellow" ] )
--                    ]
--                    [ 'VL.MString' "black" ]
--               ]
-- @
--
-- For more details, see the
-- <https://vega.github.io/vega-lite/docs/condition.html Vega-Lite documentation>.

-- $toplevel
-- These are in addition to the data and transform options described above,
-- and are described in the
-- [Vega-Lite top-level spec documentation](https://vega.github.io/vega-lite/docs/spec.html#top-level-specifications).

-- $title
-- Per-title settings. Use 'VL.TitleStyle' to change the appearance of all
-- titles in a multi-view specification.

-- $viewbackground
-- The background of a single view in a view composition can be styled independently
-- of other views. For more details see the
-- [Vega-Lite view background documentation](https://vega.github.io/vega-lite/docs/spec.html#view-background).

-- $configure
-- In version @0.5.0.0@ the 'VL.ConfigureSpec' type was introduced to
-- make it clear that only 'VL.configuration' should be used with
-- 'VL.configure'.

-- $axisconfig
-- See the
-- [Vega-Lite axis config documentation](https://vega.github.io/vega-lite/docs/axis.html#general-config).

-- $scaleconfig
-- See the
-- [Vega-Lite scale configuration documentation](https://vega.github.io/vega-lite/docs/scale.html#scale-config).

-- $scalerangeconfig
-- See the
-- [Vega-Lite scheme configuration documentation](https://vega.github.io/vega/docs/schemes/#scheme-properties).

-- $titleconfig
-- Unlike 'VL.title', these options apply to __all__ titles if multiple views
-- are created. See the
-- [Vega-Lite title configuration documentation](https://vega.github.io/vega-lite/docs/title.html#config).

-- $viewconfig
-- See the
-- [Vega-Lite view configuration documentation](https://vega.github.io/vega-lite/docs/spec.html#config).

-- $compositionconfig
-- See the Vega-Lite
-- [concat](https://vega.github.io/vega-lite/docs/concat.html#concat-configuration),
-- [facet](https://vega.github.io/vega-lite/docs/facet.html#facet-configuration),
-- and
-- [repeat](https://vega.github.io/vega-lite/docs/repeat.html#repeat-configuration)
-- configuration documentation pages.

-- $generaldatatypes
-- In addition to more general data types like integers and string, the following types
-- can carry data used in specifications.

-- $temporaldata
-- See the
-- [Vega-Lite dateTime documentation](https://vega.github.io/vega-lite/docs/types.html#datetime)
-- and the [Vega-Lite time unit documentation](https://vega.github.io/vega-lite/docs/timeunit.html).

-- $update
-- The following section describes how to update code that used
-- an older version of @hvega@.

-- $update01202
-- The @0.12.0.2@ release adds support for version 2.0 of the text package.
-- There are no changes to the code.

-- $update01201
-- The @0.12.0.1@ release is purely to support bytestring 0.11 when
-- running the tests. There are no changes to the code.

-- $update01200
-- The @0.12.0.0@ release allows @hvega@ to be built with version 2.0 of the
-- [aeson package](https://hackage.haskell.org/package/aeson). There are
-- no changes to the API of @hvega@, but it does mean that you may need
-- to update code that directly creates JSON, such as 'VL.dataFromJson'.
-- It is likely that 'VL.LabelledSpec' type and the related
-- @toXXXSpec/fromXXXSpec@ functions, such as 'VL.fromSelectSpec' and
-- 'VL.toSelectSpec', will be updated once we can drop
-- support for versions of @aeson@ prior to 2.0.
--

-- $update01101
-- The @0.11.0.1@ release is purely to support testing with @hashable@ 0.3.1.0.
-- There are no changes to the module.
--

-- $update01100
-- The @0.11.0.0@ release updates @hvega@ to support version 4.15 of
-- the Vega-Lite schema.
--
-- Note that @hvega@ does __not__ provide any information to help users
-- take advantage of the (new to 4.14) ability to
-- omit the type of a field when
-- [it can be inferred](https://vega.github.io/vega-lite/docs/type.html).
-- As the type is currently optional in @hvega@ users can just
-- not give a type. The example [IHaskell notebooks](https://github.com/DougBurke/hvega/tree/master/notebooks)
-- have been updated to show off the optional support.
--
-- Similarly, Vega-Lite 4.14 allows you to share the type, scale, axis,
-- and legend in a shared encoding. There is no explicit support added in
-- @0.11.0.0@ because @hvega@ already allowed you to create the specification.
--
-- __New constructors__
--
-- The 'VL.OrderChannel' type has gained 'VL.OBand', 'VL.OTitle'/'VL.ONoTitle',
-- and conditional-predicate support with 'VL.ODataCondition',
-- 'VL.OSelectionCondition', and 'VL.ONumber' constructors.
--
-- The 'VL.MarkChannel' type has gained the 'VL.MNullValue' constructor.
--
-- 
-- The 'VL.ScaleRange' type has gained 'VL.RField', 'VL.RMax', and 'VL.RMin'
-- constructors.
--
-- __Breaking Changes__
--
-- Domain settings in 'VL.ScaleProperty' and associated types have been
-- changed to better match the Vega-Lite schema: 'VL.SDomain' now takes
-- a new type ('VL.DomainLimits') which actually contains many of the
-- orignal symbols (so hopefully will require no changes), and a new
-- constructor has been added ('VL.SDomainOpt') which takes the
-- 'VL.ScaleDomain' type, which has seen new constructors - 'VL.DMax',
-- 'VL.DMaxTime', 'VL.DMid', 'VL.DMin', and 'VL.DMinTime' - as well as
-- some constructors moving to 'VL.DomainLimits'.
--
-- __Deprecated symbols__:
--
-- The 'VL.SDomainMid' constructor of 'VL.ScaleProperty' will be removed in a
-- future release as it has been replaced by the 'VL.DMid' constructor
-- in 'VL.ScaleDomain'.  
  
-- $update01000
-- The @0.10.0.0@ release updates @hvega@ to support version 4.13 of
-- the Vega-Lite schema.
--
-- __Breaking Changes__
--
-- The handling of time units (for both 'VL.TimeUnit' and 'VL.ScaleNice') has
-- changed. The contents of these types have been split into two parts:
-- a \"time unit\" and the options that get applied to it (rather than having
-- a single type that combines both functions). This does mean that setting
-- time units has now become __more verbose__, but it has stopped some
-- problem cases (and, in the case of 'VL.ScaleNice', fixed a logical error
-- on my part). The new time units are 'VL.BaseTimeUnit' and 'VL.NTimeUnit',
-- and contain the \"basic\" constructors for the time units. The
-- 'VL.TimeUnit' and 'VL.ScaleNice' constructors now reference these
-- types rather than include them in their definition, so that
-- @PTimeUnit Month@ has been changed to
-- @'VL.PTimeUnit' ('VL.TU' 'VL.Month')@
-- and
-- @SNice NMinute@ has changed to
-- @'VL.SNice' ('VL.NTU' 'VL.NMinute')@.
--
-- The 'VL.BaseTimeUnit' type has seen a number of additions: the 'VL.Week' and
-- 'VL.DayOfYear' time units added in Vega-Lite 4.13.0, along with the
-- associated composite units (such as 'VL.YearWeek'), and a number of
-- composite types that were missing (such as 'VL.MonthDateHours').
-- The 'VL.DateTime' type has added the 'VL.DTWeek' and 'VL.DTDayOfYear'
-- constructors.

-- $update0910
-- The tutorial has been expanded to add a section with pie charts.

-- $update0900
-- The @0.9.0.0@ release updates @hvega@ to support version 4.12 of
-- the Vega-Lite schema.
--
-- __New constructors__
--
-- Support for arcs has been added: the 'VL.Arc' type has been added
-- to 'VL.Mark'; 'VL.Theta', 'VL.Theta2', 'VL.R', and 'VL.R2' have
-- been added to 'VL.Position'; and 'VL.MInnerRadius',
-- 'VL.MOuterRadius', 'VL.MPadAngle', 'VL.MRadius2',
-- 'VL.MRadiusOffset', 'VL.MRadius2Offset', 'VL.MTheta2',
-- 'VL.MThetaOffset', and 'VL.MTheta2Offset' added to
-- 'VL.MarkProperty'. 'VL.ArcStyle' has been added to
-- 'VL.ConfigurationProperty'.
--
-- Support for ARIA attributes has been added to a number of features
-- (e.g. 'VL.Aria' and 'VL.AriaDescription' for 'VL.AxisConfig' and
-- 'VL.MAria', 'VL.MAriaDescription', 'VL.MAriaRole', 'VL.MAriaRoleDescription'
-- for 'VL.MarkProperty', 'VL.AriaStyle' for 'VL.ConfigurationProperty').
-- The 'VL.ariaDescrption' encoding has been added, along with the
-- 'VL.AriaDescriptionChannel'.
--
-- The 'VL.angle' encoding channel has been added for text and point marks.
--
-- The 'VL.Channel' type has gained 'VL.ChAngle', 'VL.ChTheta', 'VL.ChTheta2',
-- 'VL.ChRadius', 'VL.ChRadius2', 'VL.ChDescription', and 'VL.ChURL'.
--
-- Layers have been added to 'VL.Arrangement' ('VL.Layer') and to
-- 'VL.RepeatFields' ('VL.LayerFields').
--
-- The 'VL.MRepeatDatum' and 'VL.MDatum', 'VL.PRepeatDatum' and
-- 'VL.PDatum', and 'VL.TRepeatDatum' and 'VL.TDatum' pairs have been
-- added to 'VL.MarkChannel', 'VL.PositionChannel', and
-- 'VL.TextChannel' respectively.
--
-- The 'VL.MarkProperty' now has support for labelling the X (or X2)
-- coordinate as the \"width\" of the plot and Y (or Y2)
-- as the \"height\" of the plot. See 'VL.MXWidth', 'VL.MX2Width',
-- 'VL.MYHeight', and 'VL.MY2Height'.
--
-- Improved support for tick scales: 'VL.TickCount' and
-- 'VL.TickCountTime' have been added to 'VL.AxisConfig',
-- 'VL.AxTickCountTime' has been added to 'VL.AxisProperty',
-- 'VL.LTickCountTime' has been added to 'VL.LegendProperty', and
-- 'VL.LeTickCountTime' has been added to 'VL.LegendConfig'.
--
-- The 'VL.ScaleRange' type has now gained three new versions:
-- (experimental) 'VL.RPair' for defining the axis range,
-- and 'VL.RHeight' and 'VL.RWidth' for specifying the height 
-- or width as a signal.
--
-- 'VL.AxisProperty' has gained 'VL.AxFormatAsCustom'. 'VL.AxisConfig'
-- has gained 'VL.Format', 'VL.FormatAsNum', 'VL.FormatAsTemporal',
-- and 'VL.FormatAsCustom'. 'VL.LegendProperty' has gained
-- 'VL.LFormatAsCustom'.  'VL.HeaderProperty' has gained
-- 'VL.HFormatAsCustom'. 'VL.TextChannel' has gained
-- 'VL.TFormatAsCustom'. The 'VL.ConfigurationProperty' type has a new
-- option to configure support for custom format types
-- ('VL.CustomFormatStyle').
--
-- 'VL.AxisConfig' and 'VL.AxisProperty' have gained new cap styles:
-- 'VL.DomainCap', 'VL.GridCap', 'VL.TickCap' and
-- 'VL.AxDomainCap', 'VL.AxGridCap', 'VL.AxTickCap' respectively.
--
-- The 'VL.TZIndex' option of 'VL.TitleConfig' can now be used with
-- 'VL.TitleStyle' (prior to Vega-Lite 4.12 it was only supported
-- when used with 'VL.title'). The 'VL.LeZIndex' type has been added
-- to 'VL.LegendConfig'.
--
-- The 'VL.HyperlinkChannel' has gained a number of constructors it was
-- missing: 'VL.HyBand', 'VL.HyFormat', 'VL.HyFormatAsNum',
-- 'VL.HyFormatAsTemporal', 'VL.HyFormatAsCustom', 'VL.HyLabelExpr',
-- 'VL.HyTitle', and 'VL.HyNoTitle'. A similar update has been made
-- to 'VL.TextChannel', which has gained 'VL.TBand' and 'VL.TLabelExpr'.

-- $update0800
-- The @0.8.0.0@ release updates @hvega@ to support version 4.8 of
-- the Vega-Lite schema.
--
-- The 'VL.RepeatStyle' constructor for 'VL.ConfigurationProperty' should not
-- be used, as its functionality has been moved to 'VL.ConcatStyle' in
-- Vega-Lite 4.8. This constructor will be removed at some point in the
-- future but is still available (as support for Vega-Lite 4.8 is
-- limited).
--
-- __Breaking Changes__
--
-- The 'VL.HTitleFontWeight' constructor (a member of 'VL.HeaderProperty')
-- now takes a 'VL.FontWeight' argument rather than @Text@.
--
-- The @LeTitle@ constructor from 'VL.LegendConfig' was removed as it
-- is not supported in Vega-Lite ('VL.LeNoTitle' remains, as it is used
-- to remove legend titles from a visualization).
--
-- @ScBinLinear@ was removed from 'VL.Scale' as it is
-- not used by Vega-Lite.
--
-- __New constructors__
--
-- The 'VL.HeaderProperty' type has gained the following constructors
-- from Vega-Lite 4.8: 'VL.HLabelBaseline', 'VL.HLabelFontWeight',
-- 'VL.HLabelLineHeight', and 'VL.HOrient'.
--
-- The 'VL.AxisConfig' type has gained the 'VL.Disable' constructor from
-- Vega-Lite 4.8.
--
-- The 'VL.LegendConfig' type has gained the 'VL.LeDirection' and
-- (from Vega-Lite 4.8) 'VL.LeDisable' constructors. The 'VL.LegendProperty'
-- type has gained 'VL.LLabelExpr', 'VL.LSymbolLimit', and
-- 'VL.LTitleLineHeight' constructors.

-- $update0700
-- The @0.7.0.0@ release updates @hvega@ to support version 4.7 of
-- the Vega-Lite schema. The @0.7.0.1@ update fixes several minor
-- documentation issues.
--
-- __New functionality__
--
-- The 'VL.BlendMode' type has been added for controlling how marks blend
-- with their background. This is used with the new 'VL.MBlend' constructor
-- for marks.
--
-- __Breaking Change__
--
-- The axis style options for specific data- or mark- types
-- ('VL.AxisBand', 'VL.AxisDiscrete', 'VL.AxisPoint',
-- 'VL.AxisQuantitative', and 'VL.AxisTemporal') have been changed to
-- accept an additional argument (the new 'VL.AxisChoice' type)
-- which defines which axis (X, Y, or both) the configuration should be
-- applied to. This is to support new axis configuration options added
-- in Vega-Lite 4.7.0.
--
-- The @ChTooltip@ 'VL.Channel' constructor has been removed as support
-- for this channel type was dropped in Vega-Lite 4.
--
-- __New constructors__
--
-- The 'VL.ScaleDomain' type has gained 'VL.DSelectionField' and 'VL.DSelectionChannel'
-- constructors, which allow you to link a scale (e.g. an axis) to a selection that
-- is projected over multiple fields or encodings.
--
-- The 'VL.Operation' type has gained the 'VL.Product' specifier from Vega-Lite
-- 4.6.0.
--
-- The 'VL.TextChannel' has gained 'VL.TStrings' to support multi-line labels.
--
-- The 'VL.VAlign' type has gained 'VL.AlignLineTop' and 'VL.AlignLineBottom'
-- (Vega-Lite 4.6.0).
--
-- 'VL.LineBreakStyle' has been added to 'VL.ConfigurationProperty'.
--
-- The height of multi-line axis labels can now be set with the
-- 'VL.LabelLineHeight' and 'VL.AxLabelLineHeight' properties of the
-- 'VL.AxisConfig' and 'VL.AxisProperty' types (Vega-Lite 4.6.0).
--
-- Numeric filter ranges, specified with 'VL.FRange',
-- can now be lower- or upper-limits -
-- 'VL.NumberRangeLL' and 'VL.NumberRangeUL' respectively -
-- added to the 'VL.FilterRange' type.

-- $update0600
-- The @0.6.0.0@ release updates @hvega@ to support version 4.5 of
-- the Vega-Lite schema.
--
-- __New functionality__
--
-- New function for use with 'VL.encoding': 'VL.strokeDash'. The 'VL.ChStrokeDash'
-- constructor has been added to the 'VL.Channel' type, and 'VL.RNumberLists'
-- (Vega-Lite 4.4) to 'VL.ScaleRange'.
--
-- Named styles have been added for axes as well as marks. As mentioned below,
-- this involves deprecating the previous constructors for naming styles,
-- as there are now separate configuration options: 'VL.AxisNamedStyles'
-- and 'VL.MarkNamedStyles'. The 'VL.AStyle' and 'VL.AxStyle' options have been
-- added to 'VL.AxisConfig' and 'VL.AxisProperty' respectively.
-- The 'VL.StyleLabel' type alias has been added to help the documentation, but
-- provides no extra type safety.
--
-- __Breaking Change__
--
-- The 'VL.ConcatStyle' and 'VL.FacetStyle' constructors for
-- 'VL.ConfigurationProperty' now accept a common type,
-- 'VL.CompositionConfig', rather than having separate
-- @ConcatConfig@ and @FacetConfig@ types with the same meaning.
-- So @ConcatColumns@ and @FColumns@ have been replaced by 'VL.CompColumns',
-- and @CompSpacing@ and @FSpacing@ by 'VL.CompSpacing'.
--
-- The 'VL.ViewFill' and 'VL.ViewStroke' constructors of 'VL.ViewConfig'
-- no longer take an optional 'VL.Color' argument. The @Nothing@
-- case has been replaced by new constructors: 'VL.ViewNoFill'
-- and 'VL.ViewNoStroke'.
--
-- The 'VL.VBFill' and 'VL.VBStroke' constructors of 'VL.ViewBackground'
-- no longer take an optional 'VL.Color' argument. The @Nothing@
-- case has been replaced by new constructors: 'VL.VBNoFill'
-- and 'VL.VBNoStroke'.
--
-- __New constructors__:
--
-- 'VL.FacetChannel' has gained the following constructors:
-- 'VL.FAlign', 'VL.FCenter', and 'VL.FSpacing'. The last one
-- would have collided with the @FacetStyle@ option,
-- but this has fortuitously been renamed to 'VL.CompSpacing'.
--
-- 'VL.MSymbol' has been added to 'VL.MarkChannel' which can be
-- used to make the 'VL.shape' encoding conditional on a data
-- or selection condition.
--
-- The 'VL.TUStep' and 'VL.TUMaxBins' constructors have been added to
-- 'VL.TimeUnit' for controlling how time values are binned.
--
-- The 'VL.MarkProperty' type has gained the 'VL.MCornerRadiusEnd'
-- constructor, which is used to draw rounded histogram bars, and
-- 'VL.MTexts' for specifying multiple text values.
--
-- Error box and band properties (constructors in 'VL.MarkProperty') can now
-- be turned off with explicit @No@ variants: 'VL.MNoBorders', 'VL.MNoBox',
-- 'VL.MNoMedian', 'VL.MNoRule', and 'VL.MNoTicks'. These join the 'VL.MNoOutliers'
-- constructor.
--
-- The 'VL.ScaleProperty' type has gained 'VL.SDomainMid', useful
-- for asymmetric diverging color scales, and 'VL.SReverse' from
-- Vega-Lite v4.5. The 'VL.ScaleDomain' type has gained the
-- 'VL.DUnionWith' option from Vega-Lite v4.3. The 'VL.ScaleConfig'
-- type has gained 'VL.SCXReverse' from Vega-Lite v4.5.
--
-- Labels can now be vertically aligned to their baseline with the
-- 'VL.AlignBaseline' constructor of the 'VL.VAlign' type.
--
-- Headers ('VL.HeaderProperty') have gained the following constructors:
-- 'VL.HLabel', 'VL.HLabelExpr', 'VL.HLabelFontStyle', 'VL.HTitleFontStyle',
-- and 'VL.HTitleLineHeight'.
--
-- Conditional axis ('VL.ConditionalAxisProperty') has gained the following
-- constructors for features added in Vega-Lite v4.2 and v4.5:
-- 'VL.CAxLabelOffset', 'VL.CAxLabelPadding', and 'VL.CAxTickSize'.
--
-- Cursor handling has been enhanced (to match Vega-Lite 4.1):
-- 'VL.ViewCursor' has been added to 'VL.ViewConfig' and 'VL.SMCursor' to
-- 'VL.SelectionMarkProperty'.
--
-- The legend configuration has been updated (to match Vega-Lite 4.0)
-- with the addition of 'VL.LeSymbolLimit', 'VL.LeTickCount', 'VL.LeTitleLineHeight',
-- and 'VL.LeUnselectedOpacity' constructors.
--
-- The axis configuration and property types ('VL.AxisConfig' and 'VL.AxisProperty')
-- have gained the Vega-Lite 4.4 'VL.LabelOffset' and 'VL.AxLabelOffset' constructors.
-- Note that version 4.4.0 of the Vega-Lite specification has these fields
-- as strings but this is fixed in version 4.5.0.
--
-- 'VL.ConfigurationProperty' has added new constructors:
-- 'VL.AxisDiscrete' and 'VL.AxisPoint' from Vega-Lite 4.5,
-- 'VL.AxisQuantitative' and 'VL.AxisTemporal' from Vega-Lite 4.4,
-- 'VL.BoxplotStyle', 'VL.ErrorBandStyle', 'VL.ErrorBarStyle',
-- 'VL.FontStyle' (Vega-Lite 4.3), 'VL.HeaderColumnStyle',
-- 'VL.HeaderFacetStyle', 'VL.HeaderRowStyle',
-- 'VL.ImageStyle', and 'VL.RepeatStyle'.
--
-- __Deprecated symbols__:
--
-- 'VL.ConfigurationProperty' has seen a large number of deprecations,
-- as a number of constructors have been renamed:
--
-- * @NamedStyle@ and @NamedStyles@ have been replaced by 'VL.MarkNamedStyles';
--
-- * @Autosize@, @Background@, @CountTitle@, @FieldTitle@,
--   @Legend@, @NumberFormat@, @Padding@, @Projection@, @Range@,
--   @Scale@, @TimeFormat@, and @View@
--   constructors have been replaced by
--   'VL.AutosizeStyle', 'VL.BackgroundStyle', 'VL.CountTitleStyle',
--   'VL.FieldTitleStyle', 'VL.LegendStyle', 'VL.NumberFormatStyle',
--   'VL.PaddingStyle', 'VL.ProjectionStyle', 'VL.RangeStyle',
--   'VL.ScaleStyle', 'VL.TimeFormatStyle', and 'VL.ViewStyle' respectively.

-- $update0500
-- The @0.5.0.0@ release now creates specifications using version 4
-- of the Vega-Lite schema (version 0.4 of @hvega@ used version 3).
-- The 'VL.toVegaLiteSchema' function can be used along with the
-- 'VL.vlSchema3' to use version 3 for the output.
--
-- There is more-extensive use of type aliases, such as 'VL.Color',
-- and the introduction of several more (e.g. 'VL.DashStyle' and
-- 'VL.FieldName'). These do not add any type safety, but help the
-- documentation (as they provide a single place to explain the meaning
-- and any constraints on a particular value). There are some
-- changes that do improve type safety, discussed in the
-- \"Breaking changes\" section below.
--
-- Documentation improvements, including a new section in the
-- tutorial on choropleths contributed by Adam Conner-Sax, and
-- plots using an Aitoff projection contributed by Jo Wood.
--
-- __Changes in Vega-Lite 4__:
--
-- * The background of a visualization is now white by default whereas in
--   previous versions it was transparent. If you
--   need a transparent background then add the following configuration
--   to the visualization:
--   @'VL.configuration' ('VL.BackgroundStyle' \"rgba(0,0,0,0)\")@.
--
-- * Tooltips are now disabled by default. To enable, either use the
--   'VL.tooltip' channel or by setting @'VL.MTooltip' 'VL.TTEncoding'@.
--
-- * Title (and subtitle) strings can now be split across multiple lines:
--   use @'\n'@ to indicate where line breaks should occur.
--
-- Note that the behavior of a Vega-Lite visualization seems to depend
-- on both the version of the schema it is using, and the version of the
-- visualization software used to display it (e.g.
-- <https://vega.github.io/vega-lite/usage/embed.html Vega-Embed>).
--
-- __New functionality__:
--
-- This does not include new configuration options listed in the
-- \"new constructors\" section below.
--
-- * Colors are now cleaned of extraneous whitespace and, if empty,
--   converted to the JSON null value. This should not change the behavior
--   of any existing visualization.
--
-- * The 'VL.pivot' transform has been added, along with the 'VL.PivotProperty'
--   preferences type. This is the inverse of 'VL.fold'.
--
-- * The 'VL.density' transform has been added, along with the 'VL.DensityProperty'
--   type, to support kernel density estimation (such as generating a
--   continuous distribution from a discrete one).
--
-- * The 'VL.loess' transform has been added, along with the 'VL.LoessProperty'
--   type, to support estimating a trend (scatterplot smoothing).
--
-- * The 'VL.regression' transform has been added, along with the 'VL.RegressionProperty'
--   and 'VL.RegressionMethod' types, to support regression analysis.
--
-- * The 'VL.quantile' transform has been added, along with the
--   'VL.QuantileProperty' type, to support quantile analysis.
--
-- * The 'VL.url' encoding has been added for displaying images (via the
--   new 'VL.Image' mark type.
--
-- * The 'VL.lookupSelection' transform has been added to support
--   joining data via a selection. The 'VL.SelectionLabel' type alias
--   has been added as a guide for the documentation.
--
-- * The 'VL.heightOfContainer' and 'VL.widthOfContainer' functions
--   have been added to support responsive sizing, although I have not
--   had much success in getting them to work!
--
-- * The 'VL.tooltip' encoding will now turn off tooltips if given an
--   empty list (although note that tooltips are now off by default in
--   Vega-Lite 4).
--
-- __Breaking changes__:
--
-- * The @combineSpecs@ function has been removed.
--
-- * In an attempt to provide some type safety, the 'VL.encoding',
--   'VL.transform', 'VL.resolve', 'VL.selection', and 'VL.configure'
--   functions now take specialised types - 'VL.EncodingSpec',
--   'VL.TransformSpec', 'VL.ResolveSpec', 'VL.SelectSpec', and
--   'VL.ConfigureSpec' respectively - rather than the generic
--   'VL.LabelledSpec' type. Simple visualizations should remain
--   unchanged, but helper functions may need to have their type signatures
--   updated.
--
-- * The 'VL.lookup' function now takes the new 'VL.LookupFields'
--   type rather than a list of field names. The 'VL.lookupAs' function
--   is deprecated, as its functionality is now possible with
--   'VL.lookup'.
--
-- * The @RemoveInvalid@ constructor has been removed from
--   'VL.ConfigurationProperty'. To indicate how missing values should
--   be handled use the new 'VL.MRemoveInvalid' constructor from
--   'VL.MarkProperty' instead. This means changing
--   @'VL.configuration' (RemoveInvalid b)@ to
--   @'VL.configuration' ('VL.MarkStyle' ['VL.MRemoveInvalid' b])@.
--
-- * The @Stack@ constructor has been removed from
--   'VL.ConfigurationProperty'.
--
-- * The @SRangeStep@ constructor from 'VL.ScaleProperty' has been
--   removed. The 'VL.widthStep' and 'VL.heightStep' functions should
--   be used instead.
--
-- * The @ViewWidth@ and @ViewHeight@ constructors from 'VL.ViewConfig'
--   have been replaced by 'VL.ViewContinuousWidth', 'VL.ViewContinuousHeight',
--   'VL.ViewDiscreteWidth', and 'VL.ViewDiscreteHeight' constructors
--   (actually, they remain but are now deprecated and the
--   continuous-named versions should be used instead).
--
-- * The @SCRangeStep@ and @SCTextXRangeStep@ constructors of
--   'VL.ScaleConfig' have been removed. The new 'VL.ViewStep' constructor
--   of 'VL.ViewConfig' should be used instead. That is, users should
--   change @'VL.configuration' ('VL.Scale' [SCRangeStep (Just x)])@
--   to @'VL.configuration' ('VL.View' ['VL.ViewStep' x])@.
--
-- * The @ShortTimeLabels@, @LeShortTimeLabels@, and @MShortTImeLabels@
--   constructors have been removed from 'VL.AxisConfig', 'VL.LegendConfig',
--   and 'VL.MarkProperty' respectively.
--
-- __New constructors__:
--
-- Note that some new constructors have been described in the \"breaking
-- changes\" section above and so are not repeated here.
--
-- * 'VL.AxisProperty' has gained the 'VL.AxDataCondition' constructor for
--   marking a subset of axis properties as being conditional on their
--   position, and the 'VL.ConditionalAxisProperty' for defining which
--   properties (grid, label, and tick) can be used in this way.
--   It has also gained the 'VL.AxLabelExpr' constructor, which allows you
--   to change the content of axis labels, 'VL.AxTickBand' for positioning
--   the labels for band scales (and the associated 'VL.BandAlign' type),
--   'VL.AxTitleLineHeight' to specify the line height, and
--   'VL.AxTranslateOffset' for applying a translation offset to the
--   axis group mark.
--
-- * 'VL.AxisConfig' has gained 'VL.TickBand', 'VL.TitleLineHeight', and
--   'VL.TranslateOffset', matching the additions to 'VL.AxisProperty'.
--
-- * The 'VL.ViewBackgroundStyle' constructor has been added to 'VL.ViewConfig'.
--
-- * The 'VL.TitleConfig' type gained the following constructors:
--   'VL.TAlign', 'VL.TdX', 'VL.TdY', 'VL.TLineHeight',
--   'VL.TSubtitle', 'VL.TSubtitleColor', 'VL.TSubtitleFont',
--   'VL.TSubtitleFontSize', 'VL.TSubtitleFontStyle',
--   'VL.TSubtitleFontWeight', 'VL.TSubtitleLineHeight',
--   and 'VL.TSubtitlePadding'.
--
-- * The 'VL.AFitX' and 'VL.AFitY' constructors have been added to the
--   @Autosize@ type.
--
-- * The 'VL.SelectionProperty' type has gained the 'VL.BindLegend'
--   constructor - and associated 'VL.BindLegendProperty' type - to
--   allow selection of legend items (for categorical data only).
--
-- * The 'VL.TextChannel' type has gained 'VL.TString', which lets you specify
--   the text content as a literal.
--
-- * Two new projections - 'VL.EqualEarth' and 'VL.NaturalEarth1' - have been
--   added to the 'VL.Projection' type.
--
-- * Support for color gradients has been added for marks via the
--   'VL.MColorGradient', 'VL.MFillGradient', and
--   'VL.MStrokeGradient' constructors of
--   'VL.MarkProperty', along with the new 'VL.ColorGradient' and
--   'VL.GradientProperty' types for defining the appearance of the
--   gradient. The 'VL.GradientCoord' and 'VL.GradientStops' type aliases
--   have also been added (although they provides no type safety).
--
-- * The 'VL.Image' constructor has been added to 'VL.Mark', for use
--   with the new 'VL.url' encoding, and 'VL.MAspect' to 'VL.MarkProperty'.
--
-- * The 'VL.MCornerRadius' constructor has been added to 'VL.MarkProperty'
--   to set the corner radius of rectangular marks. If that's not enough,
--   you can change individual corners with one of:
--   'VL.MCornerRadiusTL', 'VL.MCornerRadiusTR', 'VL.MCornerRadiusBL',
--   and 'VL.MCornerRadiusBR'.
--
-- * The 'VL.MDir', 'VL.MEllipsis', and 'VL.MLimit' constructors have
--   been added to 'VL.MarkProperty' to control how text is
--   truncated. The 'VL.TextDirection' type has been added for use
--   with 'VL.MDir'.
--
-- * The 'VL.MarkProperty' type has gained 'VL.MLineBreak' and
--   'VL.MLineHeight' constructors for controlling how multi-line
--   labels are displayed. Note that @hvega@ will __always__ split on
--   the newline character (@\\n@), which will over-ride the
--   'VL.MLineBreak' setting.
--
-- * The 'VL.DTMonthNum' and 'VL.DTDayNum' constructors have been added
--   to @DateTime@.
--
-- * The 'VL.BinProperty' type has gained the 'VL.SelectionExtent'
--   constructor, which defines the bin range as an interval selection.
--
-- * The 'VL.PositionChannel' type has gained 'VL.PBand', for defining
--   the size of a mark relative to a band, and 'VL.MarkProperty' has
--   added 'VL.MTimeUnitBand' and 'VL.MTimeUnitBandPosition'.
--
-- __Bug Fixes__ in this release:
--
-- * The selection property @'VL.SInitInterval' Nothing Nothing@ is
--   now a no-op (as it does nothing), rather than generating invalid JSON.
--
-- * The following options or symbols generated incorrect JSON output:
--   'VL.ONone', 'VL.LSymbolStrokeWidth', 'VL.LeLabelOpacity'.
--

-- $update0400
-- The @0.4.0.0@ release added a large number of functions, types, and
-- constructors, including:
--
-- 'VL.toVegaLiteSchema' has been added to allow you to specify a
-- different Vega-Lite schema. 'VL.toVegaLite' uses version 3 but
-- version 4 is being worked on as I type this. The 'VL.vlSchema'
-- function has been added, along with 'VL.vlSchema4', 'VL.vlSchema3',
-- and 'VL.vlSchema2' values. The 'VL.toHtmlWith' and 'VL.toHtmlFileWith'
-- functions have been added to support more control over the
-- embedding of the Vega-Lite visualizations, and the versions of
-- the required Javascript libraries used by the @toHtmlXXX@ routines
-- has been updated.
--
-- The 'VL.VLProperty' type now exports its constructors, to support users
-- who may need to tweak or augment the JSON Vega-Lite specification
-- created by @hvega@: see [issue
-- 17](https://github.com/DougBurke/hvega/issues/17). It has also gained
-- several new constructors and associated functions, which are given in
-- brackets after the constructor: 'VL.VLAlign' ('VL.align'); 'VL.VLBounds'
-- ('VL.bounds'); 'VL.VLCenter' ('VL.center', 'VL.centerRC'); 'VL.VLColumns'
-- ('VL.columns'); 'VL.VLConcat' ('VL.vlConcat'); 'VL.VLSpacing' ('VL.alignRC',
-- 'VL.spacing', 'VL.spacingRC'); 'VL.VLUserMetadata' ('VL.usermetadata'); and
-- 'VL.VLViewBackground' ('VL.viewBackground'). It is expected that you will be
-- using the functions rather the constructors!
--
-- Four new type aliases have been added: 'VL.Angle', 'VL.Color', 'VL.Opacity',
-- and 'VL.ZIndex'. These do not provide any new functionality but do
-- document intent.
--
-- The 'VL.noData' function has been added to let compositions define the
-- source of the data (whether it is from the parent or not), and data
-- sources can be named with 'VL.dataName'. Data can be created with
-- 'VL.dataSequence', 'VL.dataSequenceAs', and 'VL.sphere'. Graticules can be
-- created with 'VL.graticule'.  The 'VL.NullValue' type has been added to
-- 'VL.DataValue' to support data sources that are missing elements, but for
-- more-complex cases it is suggested that you create your data as an
-- Aeson Value and then use 'VL.dataFromJson'. Support for data imputation
-- (creating new values based on existing data) has been added, as
-- discussed below.
--
-- The alignment, size, and composition of plots can be defined and
-- changed with 'VL.align', 'VL.alignRC', 'VL.bounds', 'VL.center', 'VL.centerRC',
-- 'VL.columns', 'VL.spacing', and 'VL.spacingRC'.
--
-- Plots can be combined and arranged with: 'VL.facet', 'VL.facetFlow',
-- 'VL.repeat', 'VL.repeatFlow', and 'VL.vlConcat'
--
-- New functions for use in a 'VL.transform': 'VL.flatten', 'VL.flattenAs',
-- 'VL.fold', 'VL.foldAs', 'VL.impute', and 'VL.stack'.
--
-- New functions for use with 'VL.encoding': 'VL.fillOpacity', 'VL.strokeOpacity',
-- 'VL.strokeWidth',
--
-- The ability to arrange specifications has added the "flow" option
-- (aka "repeat"). This is seen in the addition of the 'VL.Flow' constructor
-- to the 'VL.Arrangement' type - which is used with 'VL.ByRepeatOp',
-- 'VL.HRepeat', 'VL.MRepeat', 'VL.ORepeat', 'VL.PRepeat', and 'VL.TRepeat'.
--
-- The 'VL.Mark' type has gained 'VL.Boxplot', 'VL.ErrorBar', 'VL.ErrorBand', and
-- 'VL.Trail' constructors. The 'VL.MarkProperty' type has gained 'VL.MBorders',
-- 'VL.MBox', 'VL.MExtent', 'VL.MHeight', 'VL.MHRef', 'VL.MLine', 'VL.MMedian', 'VL.MOrder',
-- 'VL.MOutliers', 'VL.MNoOutliers', 'VL.MPoint', 'VL.MRule', 'VL.MStrokeCap', 'VL.MStrokeJoin',
-- 'VL.MStrokeMiterLimit', 'VL.MTicks', 'VL.MTooltip', 'VL.MWidth', 'VL.MX', 'VL.MX2',
-- 'VL.MXOffset', 'VL.MX2Offset', 'VL.MY', 'VL.MY2', 'VL.MYOffset', and 'VL.MY2Offset'
-- constructors.
--
-- The 'VL.Position' type has added 'VL.XError', 'VL.XError2', 'VL.YError', and
-- 'VL.YError2' constructors.
--
-- The 'VL.MarkErrorExtent' type was added.
--
-- The 'VL.BooleanOp' type has gained the 'VL.FilterOp' and 'VL.FilterOpTrans'
-- constructors which lets you use 'VL.Filter' expressions as part of a
-- boolean operation. The 'VL.Filter' type has also gained expresiveness,
-- with the 'VL.FLessThan', 'VL.FLessThanEq', 'VL.FGreaterThan', 'VL.FGreaterThanEq',
-- and 'VL.FValid'.
--
-- The 'VL.Format' type has gained the 'VL.DSV' constructor, which allow you
-- to specify the separator character for column data.
--
-- The MarkChannel type has been expanded to include: 'VL.MBinned', 'VL.MSort',
-- 'VL.MTitle', and 'VL.MNoTitle'. The PositionChannel type has added
-- 'VL.PHeight', 'VL.PWidth', 'VL.PNumber', 'VL.PBinned', 'VL.PImpute', 'VL.PTitle', and
-- 'VL.PNoTitle' constructors.
--
-- The LineMarker and PointMarker types have been added for use with
-- 'VL.MLine' and 'VL.MPoint' respectively (both from 'VL.MarkProperty').
--
-- The ability to define the binning property with
-- 'VL.binAs', 'VL.DBin', 'VL.FBin', 'VL.HBin', 'VL.MBin', 'VL.OBin', 'VL.PBin', and 'VL.TBin' has
-- been expanded by adding the 'VL.AlreadyBinned' and 'VL.BinAnchor'
-- constructors to 'VL.BinProperty', as well as changing the 'VL.Divide'
-- constructor (as described below).
--
-- The 'VL.StrokeCap' and 'VL.StrokeJoin' types has been added. These are used
-- with 'VL.MStrokeCap', 'VL.VBStrokeCap', and 'VL.ViewStrokeCap' and
-- 'VL.MStrokeJoin', 'VL.VBStrokeJoin', and 'VL.ViewStrokeJoin' respectively.
--
-- The 'VL.StackProperty' constructor has been added with the 'VL.StOffset'
-- and 'VL.StSort' constructors. As discussed below this is a breaking change
-- since the old StackProperty type has been renamed to 'VL.StackOffset'.
--
-- The 'VL.ScaleProperty' type has seen significant enhancement, by adding
-- the constructors: 'VL.SAlign', 'VL.SBase', 'VL.SBins', 'VL.SConstant' and
-- 'VL.SExponent'.  The 'VL.Scale' tye has added 'VL.ScSymLog' 'VL.ScQuantile',
-- 'VL.ScQuantize', and 'VL.ScThreshold'.
--
-- The 'VL.SortProperty' type has new constructors: 'VL.CustomSort',
-- 'VL.ByRepeatOp', 'VL.ByFieldOp', and 'VL.ByChannel'. See the breaking-changes
-- section below for the constructors that were removed.
--
-- The 'VL.AxisProperty' type has seen significant additions, including:
-- 'VL.AxBandPosition', 'VL.AxDomainColor', 'VL.AxDomainDash',
-- 'VL.AxDomainDashOffset', 'VL.AxDomainOpacity', 'VL.AxDomainWidth',
-- 'VL.AxFormatAsNum', 'VL.AxFormatAsTemporal', 'VL.AxGridColor', 'VL.AxGridDash',
-- 'VL.AxGridDashOffset', 'VL.AxGridOpacity', 'VL.AxGridWidth', 'VL.AxLabelAlign',
-- 'VL.AxLabelBaseline', 'VL.AxLabelNoBound', 'VL.AxLabelBound', 'VL.AxLabelBoundValue',
-- 'VL.AxLabelColor', 'VL.AxLabelNoFlush', 'VL.AxLabelFlush', 'VL.AxLabelFlushValue',
-- 'VL.AxLabelFlushOffset', 'VL.AxLabelFont', 'VL.AxLabelFontSize',
-- 'VL.AxLabelFontStyle', 'VL.AxLabelFontWeight', 'VL.AxLabelLimit',
-- 'VL.AxLabelOpacity', 'VL.AxLabelSeparation', 'VL.AxTickColor', 'VL.AxTickDash',
-- 'VL.AxTickDashOffset', 'VL.AxTickExtra', 'VL.AxTickMinStep', 'VL.AxTickOffset',
-- 'VL.AxTickOpacity', 'VL.AxTickRound', 'VL.AxTickWidth', 'VL.AxNoTitle',
-- 'VL.AxTitleAnchor', 'VL.AxTitleBaseline', 'VL.AxTitleColor', 'VL.AxTitleFont',
-- 'VL.AxTitleFontSize', 'VL.AxTitleFontStyle', 'VL.AxTitleFontWeight',
-- 'VL.AxTitleLimit', 'VL.AxTitleOpacity', 'VL.AxTitleX', and 'VL.AxTitleY'.
--
-- The 'VL.AxisConfig' has seen a similar enhancement, and looks similar
-- to the above apart from the constructors do not start with @Ax@.
--
-- The 'VL.LegendConfig' type has been significantly expanded and, as
-- discussed in the Breaking Changes section, changed. It has gained:
-- 'VL.LeClipHeight', 'VL.LeColumnPadding', 'VL.LeColumns', 'VL.LeGradientDirection',
-- 'VL.LeGradientHorizontalMaxLength', 'VL.LeGradientHorizontalMinLength',
-- 'VL.LeGradientLength', 'VL.LeGradientOpacity', 'VL.LeGradientThickness',
-- 'VL.LeGradientVerticalMaxLength', 'VL.LeGradientVerticalMinLength',
-- 'VL.LeGridAlign', 'VL.LeLabelFontStyle', 'VL.LeLabelFontWeight',
-- 'VL.LeLabelOpacity', 'VL.LeLabelOverlap', 'VL.LeLabelPadding',
-- 'VL.LeLabelSeparation', 'VL.LeLayout', 'VL.LeLeX', 'VL.LeLeY', 'VL.LeRowPadding',
-- 'VL.LeSymbolBaseFillColor', 'VL.LeSymbolBaseStrokeColor', 'VL.LeSymbolDash',
-- 'VL.LeSymbolDashOffset', 'VL.LeSymbolDirection', 'VL.LeSymbolFillColor',
-- 'VL.LeSymbolOffset', 'VL.LeSymbolOpacity', 'VL.LeSymbolStrokeColor', 'VL.LeTitle',
-- 'VL.LeNoTitle', 'VL.LeTitleAnchor', 'VL.LeTitleFontStyle', 'VL.LeTitleOpacity',
-- and 'VL.LeTitleOrient'.
--
-- The 'VL.LegendOrientation' type has gained 'VL.LOTop' and 'VL.LOBottom'.
--
-- The 'VL.LegendLayout' and 'VL.BaseLegendLayout' types are new, and used
-- with 'VL.LeLayout' to define the legent orient group.
--
-- The 'VL.LegendProperty' type gained: 'VL.LClipHeight', 'VL.LColumnPadding',
-- 'VL.LColumns', 'VL.LCornerRadius', 'VL.LDirection', 'VL.LFillColor',
-- 'VL.LFormatAsNum', 'VL.LFormatAsTemporal', 'VL.LGradientLength',
-- 'VL.LGradientOpacity', 'VL.LGradientStrokeColor', 'VL.LGradientStrokeWidth',
-- 'VL.LGradientThickness', 'VL.LGridAlign', 'VL.LLabelAlign', 'VL.LLabelBaseline',
-- 'VL.LLabelColor', 'VL.LLabelFont', 'VL.LLabelFontSize', 'VL.LLabelFontStyle',
-- 'VL.LLabelFontWeight', 'VL.LLabelLimit', 'VL.LLabelOffset', 'VL.LLabelOpacity',
-- 'VL.LLabelOverlap', 'VL.LLabelPadding', 'VL.LLabelSeparation', 'VL.LRowPadding',
-- 'VL.LStrokeColor', 'VL.LSymbolDash', 'VL.LSymbolDashOffset',
-- 'VL.LSymbolFillColor', 'VL.LSymbolOffset', 'VL.LSymbolOpacity', 'VL.LSymbolSize',
-- 'VL.LSymbolStrokeColor', 'VL.LSymbolStrokeWidth', 'VL.LSymbolType',
-- 'VL.LTickMinStep', 'VL.LNoTitle', 'VL.LTitleAlign', 'VL.LTitleAnchor',
-- 'VL.LTitleBaseline', 'VL.LTitleColor', 'VL.LTitleFont', 'VL.LTitleFontSize',
-- 'VL.LTitleFontStyle', 'VL.LTitleFontWeight', 'VL.LTitleLimit', 'VL.LTitleOpacity',
-- 'VL.LTitleOrient', 'VL.LTitlePadding', 'VL.LeX', and 'VL.LeY'.
--
-- 'VL.Projection' has gained the 'VL.Identity' constructor. The
-- 'VL.ProjectionProperty' type has gained 'VL.PrScale', 'VL.PrTranslate',
-- 'VL.PrReflectX', and 'VL.PrReflectY'. The 'VL.GraticuleProperty' type was
-- added to configure the appearance of graticules created with
-- 'VL.graticule'.
--
-- The 'VL.CompositionAlignment' type was added and is used with 'VL.align',
-- 'VL.alignRC', 'VL.LeGridAlign', and 'VL.LGridAlign'.
--
-- The 'VL.Bounds' type was added for use with 'VL.bounds'.
--
-- The 'VL.ImputeProperty' and 'VL.ImMethod' types were added for use with
-- 'VL.impute' and 'VL.PImpute'.
--
-- The 'VL.ScaleConfig' type has gained 'VL.SCBarBandPaddingInner',
-- 'VL.SCBarBandPaddingOuter', 'VL.SCRectBandPaddingInner', and
-- 'VL.SCRectBandPaddingOuter'.
--
-- The 'VL.SelectionProperty' type has gained 'VL.Clear', 'VL.SInit', and
-- 'VL.SInitInterval'.
--
-- The Channel type has gained: 'VL.ChLongitude', 'VL.ChLongitude2',
-- 'VL.ChLatitude', 'VL.ChLatitude2', 'VL.ChFill', 'VL.ChFillOpacity', 'VL.ChHref',
-- 'VL.ChKey', 'VL.ChStroke', 'VL.ChStrokeOpacity'.  'VL.ChStrokeWidth', 'VL.ChText',
-- and 'VL.ChTooltip'.
--
-- The 'VL.TitleConfig' type has gained: 'VL.TFontStyle', 'VL.TFrame', 'VL.TStyle',
-- and 'VL.TZIndex'.
--
-- The 'VL.TitleFrame' type is new and used with 'VL.TFrame' from 'VL.TitleConfig'.
--
-- The 'VL.ViewBackground' type is new and used with 'VL.viewBackground'.
--
-- The 'VL.ViewConfig' type has gained 'VL.ViewCornerRadius', 'VL.ViewOpacity',
-- 'VL.ViewStrokeCap', 'VL.ViewStrokeJoin', and 'VL.ViewStrokeMiterLimit'.
--
-- The 'VL.ConfigurationProperty' type, used with 'VL.configuration', has
-- gained 'VL.ConcatStyle', 'VL.FacetStyle', 'VL.GeoshapeStyle', 'VL.HeaderStyle',
-- @NamedStyles@ (renamed to 'VL.MarkNamedStyles' in 0.6.0.0),
-- and 'VL.TrailStyle' constructors.
--
-- The 'VL.ConcatConfig' type was added for use with the 'VL.ConcatStyle',
-- and the 'VL.FacetConfig' type for the 'VL.FacetStyle'
-- configuration settings.
--
-- The 'VL.HeaderProperty' type has gained: 'VL.HFormatAsNum',
-- 'VL.HFormatAsTemporal', 'VL.HNoTitle', 'VL.HLabelAlign', 'VL.HLabelAnchor',
-- 'VL.HLabelAngle', 'VL.HLabelColor', 'VL.HLabelFont', 'VL.HLabelFontSize',
-- 'VL.HLabelLimit', 'VL.HLabelOrient', 'VL.HLabelPadding', 'VL.HTitleAlign',
-- 'VL.HTitleAnchor', 'VL.HTitleAngle', 'VL.HTitleBaseline', 'VL.HTitleColor',
-- 'VL.HTitleFont', 'VL.HTitleFontSize', 'VL.HTitleFontWeight', 'VL.HTitleLimit',
-- 'VL.HTitleOrient', and 'VL.HTitlePadding'.
--
-- The 'VL.HyperlinkChannel' type has gained 'VL.HBinned'.
--
-- The 'VL.FacetChannel' type has gained 'VL.FSort', 'VL.FTitle', and 'VL.FNoTitle'.
--
-- The 'VL.TextChannel' type has gained 'VL.TBinned', 'VL.TFormatAsNum',
-- 'VL.TFormatAsTemporal', 'VL.TTitle', and 'VL.TNoTitle'.
--
-- The 'VL.TooltipContent' type was added, for use with 'VL.MTooltip'.
--
-- The 'VL.Symbol' type has gained: 'VL.SymArrow', 'VL.SymStroke',
-- 'VL.SymTriangle', 'VL.SymTriangleLeft', 'VL.SymTriangleRight', and
-- 'VL.SymWedge'.
--
-- There are a number of __breaking changes__ in this release (some
-- of which were mentioned above):
--
-- * The 'VL.title' function now takes a second argument, a list of 'VL.TitleConfig'
--   values for configuring the appearance of the title.
--
-- * The @SReverse@ constructor was removed from 'VL.ScaleProperty' as it
--   represented a Vega, rather than Vega-Lite, property. The @xSort@
--   constructors are used to change the order of an item (e.g.
--   'VL.PSort', 'VL.MSort').
--
-- * The @ScSequential@ constructor was removed from 'VL.Scale' as
--   'VL.ScLinear' should be used.
--
-- * The 'VL.SortProperty' type has had a number of changes: the @Op@,
--   @ByField@, and @ByRepeat@ constructors have been removed, and
--   'VL.ByRepeatOp', 'VL.ByFieldOp', and 'VL.ByChannel' constructors have been
--   added.
--
-- * The @AxTitleMaxLength@ and @TitleMaxLength@ constructors have been
--   removed (from 'VL.AxisProperty' and 'VL.AxisConfig' respectively) as they
--   are invalid. The 'VL.AxTitleLimit' (new in this release) and
--   'VL.TitleLimit' constructors should be used instead.
--
-- * 'VL.AxisProperty': the 'VL.AxValues' constructor has been changed from
--   accepting a list of doubles to 'VL.DataValues'. The 'VL.AxDates'
--   constructor has been deprecated and 'VL.AxValues' should be used
--   instead.
--
-- * There have been significant changes to the 'VL.LegendConfig' type: the
--   @EntryPadding@, @GradientHeight@, @GradientLabelBaseline@,
--   @GradientWidth@, and @SymbolColor@ constructors have been removed;
--   the renaming constructors have been renamed so they all begin with
--   @Le@ (e.g. @Orient@ is now 'VL.LeOrient', and 'VL.Orient' has been added
--   to 'VL.AxisConfig'); and new constructors have been added.
--
-- * The @StackProperty@ type has been renamed to 'VL.StackOffset' and its
--   constructors have changed, and a new 'VL.StackProperty'
--   type has been added (that references the 'VL.StackOffset' type).
--
-- * The @Average@ constructor of 'VL.Operation' was removed, and 'VL.Mean'
--   should be used instead.
--
-- * The @LEntryPadding@ constructor of 'VL.LegendProperty' was removed.
--
-- * The arguments to the 'VL.MDataCondition', 'VL.TDataCondition', and
--   'VL.HDataCondition' constructors - of 'VL.MarkChannel', 'VL.TextChannel',
--   and 'VL.HyperlinkChannel' respectively - have changed to support
--   accepting multiple expressions.
--
-- * The @MarkOrientation@ type has been renamed 'VL.Orientation'.
--
-- * The constructors of the 'VL.ViewConfig' type have been renamed so they
--   all begin with @View@ (to match 'VL.ViewWidth' and 'VL.ViewHeight').
--
-- * The constructors of the 'VL.ProjectionProperty' type have been renamed
--   so that they begin with @Pr@ rather than @P@ (to avoid conflicts
--   with the 'VL.PositionChannel' type).
--
-- * The 'VL.Divide' constructor of 'VL.BinProperty' now takes a list of
--   Doubles rather than two.
--
-- * The 'VL.TitleConfig' type has gained the following constructors:
--   'VL.TFontStyle', 'VL.TFrame', 'VL.TStyle', and 'VL.TZIndex'. The 'VL.TitleFrame'
--   type was added for use with 'VL.TFrame'.
--
-- * The 'VL.ArgMax' and 'VL.ArgMin' constructors of 'VL.Operation' now take an
--   optional field name, to allow them to be used as part of an encoding
--   aggregation (e.g. with 'VL.PAggregate').
--
-- * The \"z index" value has changed from an 'VL.Int' to the 'VL.ZIndex' type.
--
-- * The constructors for the 'VL.Symbol' type now all start with @Sym@, so
--   @Cross@, @Diamond@, @TriangleUp@, @TriangleDown@, and @Path@ have
--   been renamed to 'VL.SymCross', 'VL.SymDiamond', 'VL.SymTriangleUp',
--   'VL.SymTriangleDown', and 'VL.SymPath', respectively.
--
-- * The @Legend@ type has been renamed 'VL.LegendType' and its constructors
--   have been renamed 'VL.GradientLegend' and 'VL.SymbolLegend'.
