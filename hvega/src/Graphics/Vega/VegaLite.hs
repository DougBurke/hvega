{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-|
Module      : Graphics.Vega.VegaLite
Copyright   : (c) Douglas Burke, 2018-2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings, TupleSections

This is a port of the
<http://package.elm-lang.org/packages/gicentre/elm-vegalite/latest Elm
Vega Lite module>, written by Jo Wood of the giCentre at the City
University of London. It was originally based on version @2.2.1@ but
it has been updated to match later versions.  This module allows users
to create a Vega-Lite specification, targeting __version 3__ of the
<https://vega.github.io/schema/vega-lite/v3.json JSON schema>.  The
ihaskell-hvega module provides an easy way to embed Vega-Lite
visualizations in an IHaskell notebook (using
<https://vega.github.io/vega-lite/usage/embed.html Vega-Embed>).

Although this is based on the Elm module, there are differences, such
as using type constructors rather than functions for many properties -
such as @PName \"HorsePower\"@ rather than @pName \"HorsePower\"@ -
and the return value of 'toVegaLite'. The intention is to keep close
to the Elm module, but it is more a guide than an absolute
requirement!

Note that this module exports several symbols that are exported
by the Prelude, such as 'filter', 'lookup',
and 'repeat'; to avoid name clashes it's therefore advised
to either import the module qualified, for example:

@
import qualified Graphics.Vega.VegaLite as VL
@

or to hide the clashing names explicitly:

@
import Prelude hiding (filter, lookup, repeat)
@

In the following example, we'll assume the latter.

== Example

Let's say we have the following plot declaration in a module:

@
\{\-\# language OverloadedStrings \#\-\}

vl1 =
  let desc = "A very exciting bar chart"

      dat = 'dataFromRows' ['Parse' [("start", 'FoDate' "%Y-%m-%d")]]
            . 'dataRow' [("start", 'Str' "2011-03-25"), ("count", 'Number' 23)]
            . 'dataRow' [("start", 'Str' "2011-04-02"), ("count", 'Number' 45)]
            . 'dataRow' [("start", 'Str' "2011-04-12"), ("count", 'Number' 3)]

      barOpts = ['MOpacity' 0.4, 'MColor' "teal"]

      enc = 'encoding'
            . 'position' 'X' ['PName' "start", 'PmType' 'Temporal', 'PAxis' ['AxTitle' "Inception date"]]
            . 'position' 'Y' ['PName' "count", 'PmType' 'Quantitative']

  in 'toVegaLite' ['description' desc, 'background' "white"
                , dat [], 'mark' 'Bar' barOpts, enc []]
@

We can inspect how the encoded JSON looks like in an GHCi session:

@
> 'A.encode' $ 'fromVL' vl1
> "{\"mark\":{\"color\":\"teal\",\"opacity\":0.4,\"type\":\"bar\"},\"data\":{\"values\":[{\"start\":\"2011-03-25\",\"count\":23},{\"start\":\"2011-04-02\",\"count\":45},{\"start\":\"2011-04-12\",\"count\":3}],\"format\":{\"parse\":{\"start\":\"date:'%Y-%m-%d'\"}}},\"$schema\":\"https://vega.github.io/schema/vega-lite/v3.json\",\"encoding\":{\"x\":{\"field\":\"start\",\"type\":\"temporal\",\"axis\":{\"title\":\"Inception date\"}},\"y\":{\"field\":\"count\",\"type\":\"quantitative\"}},\"background\":\"white\",\"description\":\"A very exciting bar chart\"}"
@

The produced JSON can then be processed with vega-lite, which renders the following image :

<<images/example.png>>

This can be achieved in a JupyterLab session with the @vlShow@ function,
provided by @ihaskell-vega@, or 'toHtmlFile' can be used to write out a page of
HTML that includes pointer to JavaScript files which will display a Vega-Lite
specification.

-}

module Graphics.Vega.VegaLite
       (
         -- * Creating a Vega-Lite Specification

         toVegaLite
       , toVegaLiteSchema
       , vlSchema2, vlSchema3, vlSchema4, vlSchema
       , fromVL
       , VLProperty(..)
       , VLSpec
       , VegaLite
       , PropertySpec
       , LabelledSpec
       , BuildLabelledSpecs
       , Angle
       , Color
       , Opacity
       , ZIndex(..)
       , combineSpecs
       , toHtml
       , toHtmlFile

         -- * Creating the Data Specification
         --
         -- $dataspec

       , dataFromUrl
       , dataFromColumns
       , dataFromRows
       , dataFromJson
       , dataFromSource
       , dataName
       , datasets
       , dataColumn
       , dataRow
       , noData
       , Data
       , DataColumn
       , DataRow

         -- ** Geographic Data

       , geometry
       , geoFeatureCollection
       , geometryCollection
       , Geometry(..)

       -- ** Data Generators
       --
       -- $datagen

       , dataSequence
       , dataSequenceAs
       , sphere
       , graticule
       , GraticuleProperty(..)

       -- ** Formatting Input Data
       --
       -- $dataformat

       , Format(..)
       , DataType(..)

         -- * Creating the Transform Specification
         --
         -- $transform

       , transform

         -- ** Map Projections
         --
         -- $projections

       , projection
       , ProjectionProperty(..)
       , Projection(..)
       , ClipRect(..)

         -- ** Aggregation
         --
         -- $aggregation

       , aggregate
       , joinAggregate
       , opAs
       , timeUnitAs
       , Operation(..)

         -- ** Binning
         --
         -- $binning

       , binAs
       , BinProperty(..)

         -- ** Stacking
         --
         -- $stacking

       , stack
       , StackProperty(..)
       , StackOffset(..)

         -- ** Data Calculation
         --
         -- $calculate

       , calculateAs

         -- ** Filtering
         --
         -- $filtering

       , filter
       , Filter(..)
       , FilterRange(..)

         -- ** Flattening
         --
         -- $flattening

       , flatten
       , flattenAs
       , fold
       , foldAs

         -- ** Relational Joining (lookup)
         --
         -- $joining

       , lookup
       , lookupAs

         -- ** Data Imputation
         --
         -- $imputation

       , impute
       , ImputeProperty(..)
       , ImMethod(..)

         -- ** Data sampling
         --
         -- $sampling

       , sample

         -- ** Window Transformations
         --
         -- $window

       , window
       , Window(..)
       , WOperation(..)
       , WindowProperty(..)

         -- * Creating the Mark Specification
         --
         -- $markspec

       , mark
       , Mark(..)

         -- ** Mark properties
         --
         -- $markproperties

       , MarkProperty(..)
       , StrokeCap(..)
       , StrokeJoin(..)

         -- *** Used by Mark Properties

       , Orientation(..)
       , MarkInterpolation(..)
       , Symbol(..)
       , PointMarker(..)
       , LineMarker(..)
       , MarkErrorExtent(..)
       , TooltipContent(..)

         -- ** Cursors
         --
         -- $cursors

       , Cursor(..)

         -- * Creating the Encoding Specification
         --
         -- $encoding

       , encoding
       , Measurement(..)

         -- ** Position Channels
         --
         -- $position

       , position
       , Position(..)

         -- *** Position channel properties

       , PositionChannel(..)

         -- ** Sorting properties
         --
         -- $sortprops

       , SortProperty(..)
       , SortField(..)

         -- ** Axis properties
         --
         -- $axisprops

       , AxisProperty(..)

         -- ** Positioning Constants
         --
         -- *** Text Alignment

       , HAlign(..)
       , VAlign(..)

         -- *** Overlapping text

       , OverlapStrategy(..)

         -- *** Legends

       , Side(..)

         -- ** Mark channels
         --
         -- $markprops

       , size
       , color
       , fill
       , stroke
       , strokeWidth
       , opacity
       , fillOpacity
       , strokeOpacity
       , shape

         -- *** Mark Channel properties

       , MarkChannel(..)

         -- *** Mark Legends
         --
         -- $marklegends

       , Legend(..)
       , LegendProperty(..)
       , LegendOrientation(..)
       , LegendValues(..)

         -- ** Text Channels
         --
         -- $textchannels

       , text
       , tooltip
       , tooltips
       , TextChannel(..)
       , FontWeight(..)

         -- ** Hyperlink Channels
         --
         -- $hyperlink

       , hyperlink
       , HyperlinkChannel(..)

         -- ** Order Channel
         --
         -- $order

       , order
       , OrderChannel(..)

         -- ** Facet Channel
         --
         -- $facet

       , row
       , column

         -- ** Level of detail Channel
         --
         -- $detail

       , detail
       , DetailChannel(..)

         -- ** Scaling
         --
         -- $scaling

       , ScaleProperty(..)
       , Scale(..)
       , categoricalDomainMap
       , domainRangeMap
       , ScaleDomain(..)
       , ScaleRange(..)
       , ScaleNice(..)

         -- *** Color scaling
         --
         -- $color

       , CInterpolate(..)

         -- * Creating view compositions
         --
         -- $view

       , layer
       , vlConcat
       , columns
       , hConcat
       , vConcat
       , align
       , alignRC
       , spacing
       , spacingRC
       , center
       , centerRC
       , bounds
       , Bounds(..)
       , CompositionAlignment(..)

         -- ** Resolution
         --
         -- $resolution

       , resolve
       , resolution
       , Resolve(..)
       , Channel(..)
       , Resolution(..)

         -- ** Faceted views
         --
         -- $facetview

       , repeat
       , repeatFlow
       , RepeatFields(..)
       , facet
       , facetFlow
       , FacetMapping(..)
       , FacetChannel(..)
       , asSpec
       , specification
       , Arrangement(..)

         -- *** Facet Headers
         --
         -- $facetheaders

       , HeaderProperty(..)

         -- * Creating Selections for Interaction
         --
         -- $selections

       , selection
       , select
       , Selection(..)
       , SelectionProperty(..)
       , Binding(..)
       , InputProperty(..)
       , SelectionMarkProperty(..)

       -- ** Selection Resolution
       --
       -- $selectionresolution

       , SelectionResolution(..)

         -- ** Making conditional channel encodings
         --
         -- $conditional

       , BooleanOp(..)

         -- ** Top-level Settings
         --
         -- $toplevel

       , name
       , description
       , height
       , width
       , padding
       , autosize
       , background
       , usermetadata
       , Padding(..)
       , Autosize(..)

         -- *** Title
         --
         -- $title

       , title

         -- *** View Backgroud
         --
         -- $viewbackground

       , viewBackground
       , ViewBackground(..)

         -- ** Style Setting

       , configure
       , configuration
       , ConfigurationProperty(..)

         -- ** Axis Configuration Options
         --
         -- $axisconfig

       , AxisConfig(..)

         -- ** Legend Configuration Options
         --

       , LegendConfig(..)
       , LegendLayout(..)
       , BaseLegendLayout(..)

         -- ** Scale Configuration Options
         --
         -- $scaleconfig

       , ScaleConfig(..)

         -- ** Scale Range Configuration Options
         --
         -- $scalerangeconfig

       , RangeConfig(..)

         -- ** Title Configuration Options
         --
         -- $titleconfig

       , TitleConfig(..)
       , TitleFrame(..)

         -- ** View Configuration Options
         --
         -- $viewconfig

       , ViewConfig(..)
       , APosition(..)
       , FieldTitleProperty(..)

         -- ** Facet Configuration Options
         --
         -- $facetconfig

       , FacetConfig(..)

         -- ** Concatenated View Configuration Options
         --
         -- $concatconfig

       , ConcatConfig(..)

         -- * General Data types
         --
         -- $generaldatatypes

       , DataValue(..)
       , DataValues(..)

         -- ** Temporal data
         --
         -- $temporaldata

       , DateTime(..)
       , MonthName(..)
       , DayName(..)
       , TimeUnit(..)

         -- * Update notes
         --
         -- $update

         -- ** Version 0.4
         --
         -- $update0400

        )
    where

-- VegaLite uses these symbols.
import Prelude hiding (filter, lookup, repeat)

import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.Encoding as E
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as V

import Control.Arrow (first, second)

-- Aeson's Value type conflicts with the Number type
import Data.Aeson (Value, decode, encode, object, toJSON, (.=))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid ((<>))

-- added in base 4.8.0.0 / ghc 7.10.1
import Numeric.Natural (Natural)


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
-- and [fold](https://vega.github.io/vega-lite/docs/fold.html)
-- documentation.

-- $joining
-- See the
-- [Vega-Lite lookup documentation](https://vega.github.io/vega-lite/docs/lookup.html).

-- $imputation
-- Impute missing data. See the
-- [Vega-Lite impute documentation](https://vega.github.io/vega-lite/docs/impute.html#transform).

-- $sampling
-- See the [Vega-Lite sample documentation](https://vega.github.io/vega-lite/docs/sample.html)

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
-- channels. Channels can include: position on screen (e.g. 'X', 'Y'); visual
-- mark properties ('color', 'size', 'stroke', 'shape'); 'text'; 'hyperlink';
-- ordering ('order'); level of 'detail'; and facets for composed
-- visualizations ('facet'). All can be further customised via a series of
-- properties that determine how the encoding is implemented (such as
-- scaling, sorting, and spacing).

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
-- (e.g. 'color' and 'size').

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

-- $scaling
-- Used to specify how the encoding of a data field should be applied. See the
-- [Vega-Lite scale documentation](https://vega.github.io/vega-lite/docs/scale.html).

-- $color
-- For color interpolation types, see the
-- [Vega-Lite continuous scale documentation](https://vega.github.io/vega-lite/docs/scale.html#continuous).

-- $view
-- Views can be combined to create more complex multiview displays. This may involve
-- layering views on top of each other (superposition) or laying them out in adjacent
-- spaces (juxtaposition using 'repeat', 'repeatFlow', 'facet', 'facetFlow',
-- 'vlConcat', 'hConcat', or 'vConcat'). Where different views have potentially conflicting
-- channels (for example, two position scales in a layered visualization) the rules for
-- resolving them can be defined with 'resolve'. For details of creating composite views see the
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
-- 'MSelectionCondition', 'TSelectionCondition', or 'HSelectionCondition'. Similarly
-- 'MDataCondition', 'TDataCondition', or 'HDataCondition' will encode a mark
-- conditionally depending on some data properties such as whether a datum is null
-- or an outlier.
--
-- For interaction, once a selection has been defined and named, supplying a set of
-- encodings allow mark encodings to become dependent on that selection.
-- 'MSelectionCondition' is followed firstly a (Boolean) selection and then an
-- encoding if that selection is true and another encoding to be applied if it is false.
-- The color specification below states \"whenever data marks are selected with an
-- interval mouse drag, encode the cylinder field with an ordinal color scheme,
-- otherwise make them grey\":
--
-- @
-- sel = 'selection' . 'select' "myBrush" 'Interval' []
--
-- enc = 'encoding'
--         . 'position' 'X' [ 'PName' \"Horsepower\", 'PmType' 'Quantitative' ]
--         . 'position' 'Y' [ 'PName' \"Miles_per_Gallon\", 'PmType' Quantitative ]
--         . 'color'
--             [ 'MSelectionCondition' ('SelectionName' "myBrush")
--                 [ 'MName' \"Cylinders\", 'MmType' 'Ordinal' ]
--                 [ 'MString' "grey" ]
--             ]
-- @
--
-- In a similar way, 'MDataCondition' will encode a mark depending on whether any
-- predicate tests are satisfied. Unlike selections, multiple conditions and associated
-- encodings can be specified. Each test condition is evaluated in order and only on
-- failure of the test does encoding proceed to the next test. If no tests are true,
-- the encoding in the final parameter is applied in a similar way to @case of@
-- expressions:
--
-- @
-- enc = 'encoding'
--         . 'position' X [ 'PName' \"value\", 'PmType' 'Quantitative' ]
--           . 'color'
--               [ 'MDataCondition'
--                    [ ( 'Expr' "datum.value < 40", [ 'MString' "blue" ] )
--                    , ( 'Expr' "datum.value < 50", [ 'MString' "red" ] )
--                    , ( 'Expr' "datum.value < 60", [ 'MString' "yellow" ] )
--                    ]
--                    [ 'MString' "black" ]
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
-- Per-title settings. Use 'TitleStyle' to change the appearance of all
-- titles in a multi-view specification.

-- $viewbackground
-- The background of a single view in a view composition can be styled independently
-- of other views. For more details see the
-- [Vega-Lite view background documentation](https://vega.github.io/vega-lite/docs/spec.html#view-background).

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
-- Unlike 'title', these options apply to __all__ titles if multiple views
-- are created. See the
-- [Vega-Lite title configuration documentation](https://vega.github.io/vega-lite/docs/title.html#config).

-- $viewconfig
-- See the
-- [Vega-Lite view configuration documentation](https://vega.github.io/vega-lite/docs/spec.html#config).

-- $facetconfig
-- See the
-- [Vega-Lite facet configuration documentation](https://vega.github.io/vega-lite/docs/facet.html#facet-configuration).

-- $concatconfig
-- See the
-- [Vega-Lite concat configuration documentation](https://vega.github.io/vega-lite/docs/concat.html#concat-configuration).

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

-- $update0400
-- The @0.4.0.0@ release added a large number of functions, types, and
-- constructors, including:
--
-- 'toVegaLiteSchema' has been added to allow you to specify a
-- different Vega-Lite schema. 'toVegaLite' uses version 3 but
-- version 4 is being worked on as I type this. The 'vlSchema'
-- function has been added, along with 'vlSchema4', 'vlSchema3',
-- and 'vlSchema2' values.
--
-- The 'VLProperty' type now exports its constructors, to support users
-- who may need to tweak or augment the JSON Vega-Lite specification
-- created by @hvega@: see [issue
-- 17](https://github.com/DougBurke/hvega/issues/17). It has also gained
-- several new constructors and associated functions, which are given in
-- brackets after the constructor: 'VLAlign' ('align'); 'VLBounds'
-- ('bounds'); 'VLCenter' ('center', 'centerRC'); 'VLColumns'
-- ('columns'); 'VLConcat' ('vlConcat'); 'VLSpacing' ('alignRC',
-- 'spacing', 'spacingRC'); 'VLUserMetadata' ('usermetadata'); and
-- 'VLViewBackground' ('viewBackground'). It is expected that you will be
-- using the functions rather the constructors!
-- 
-- The 'ZIndex' type has been added: this provides constructors for the
-- common options - 'ZFront' and 'ZBack' - and a fall-through ('ZValue')
-- as a protection against future changes to the Vega-Lite specification.
-- 
-- Three new type aliases have been added: 'Angle', 'Color', and
-- 'Opacity'. These do not provide any new functionality but do
-- document intent.
-- 
-- The 'noData' function has been added to let compositions define the
-- source of the data (whether it is from the parent or not), and data
-- sources can be named with 'dataName'. Data can be created with
-- 'dataSequence', 'dataSequenceAs', and 'sphere'. Graticules can be
-- created with 'graticule'.  The 'NullValue' type has been added to
-- 'DataValue' to support data sources that are missing elements, but for
-- more-complex cases it is suggested that you create your data as an
-- Aeson Value and then use 'dataFromJson'. Support for data imputation
-- (creating new values based on existing data) has been added, as
-- discussed below.
-- 
-- The alignment, size, and composition of plots can be defined and
-- changed with 'align', 'alignRC', 'bounds', 'center', 'centerRC',
-- 'columns', 'spacing', and 'spacingRC'.
-- 
-- Plots can be combined and arranged with: 'facet', 'facetFlow',
-- 'repeat', 'repeatFlow', and 'vlConcat'
-- 
-- New functions for use in a 'transform': 'flatten', 'flattenAs',
-- 'fold', 'foldAs', 'impute', and 'stack'.
-- 
-- New functions for use with 'encoding': 'fillOpacity', 'strokeOpacity',
-- 'strokeWidth',
-- 
-- The ability to arrange specifications has added the "flow" option
-- (aka "repeat"). This is seen in the addition of the 'Flow' constructor
-- to the 'Arrangement' type - which is used with 'ByRepeatOp',
-- 'HRepeat', 'MRepeat', 'ORepeat', 'PRepeat', and 'TRepeat'.
-- 
-- The 'Mark' type has gained 'Boxplot', 'ErrorBar', 'ErrorBand', and
-- 'Trail' constructors. The 'MarkProperty' type has gained 'MBorders',
-- 'MBox', 'MExtent', 'MHeight', 'MHRef', 'MLine', 'MMedian', 'MOrder',
-- 'MOutliers', 'MPoint', 'MRule', 'MStrokeCap', 'MStrokeJoin',
-- 'MStrokeMiterLimit', 'MTicks', 'MTooltip', 'MWidth', 'MX', 'MX2',
-- 'MXOffset', 'MX2Offset', 'MY', 'MY2', 'MYOffset', and 'MY2Offset'
-- constructors.
-- 
-- The 'Position' type has added 'XError', 'XError2', 'YError', and
-- 'YError2' constructors.
-- 
-- The 'MarkErrorExtent' type was added.
-- 
-- The 'BooleanOp' type has gained the 'FilterOp' and 'FilterOpTrans'
-- constructors which lets you use 'Filter' expressions as part of a
-- boolean operation. The 'Filter' type has also gained expresiveness,
-- with the 'FLessThan', 'FLessThanEq', 'FGreaterThan', 'FGreaterThanEq',
-- and 'FValid'.
-- 
-- The 'Format' type has gained the 'DSV' constructor, which allow you
-- to specify the separator character for column data.
-- 
-- The MarkChannel type has been expanded to include: 'MBinned', 'MSort',
-- 'MTitle', and 'MNoTitle'. The PositionChannel type has added
-- 'PHeight', 'PWidth', 'PNumber', 'PBinned', 'PImpute', 'PTitle', and
-- 'PNoTitle' constructors.
-- 
-- The LineMarker and PointMarker types have been added for use with
-- 'MLine' and 'MPoint' respectively (both from 'MarkProperty').
-- 
-- The ability to define the binning property with 
-- 'binAs', 'DBin', 'FBin', 'HBin', 'MBin', 'OBin', 'PBin', and 'TBin' has
-- been expanded by adding the 'AlreadyBinned' and 'BinAnchor'
-- constructors to 'BinProperty', as well as changing the 'Divide'
-- constructor (as described below).
-- 
-- The 'StrokeCap' and 'StrokeJoin' types has been added. These are used
-- with 'MStrokeCap', 'VBStrokeCap', and 'ViewStrokeCap' and
-- 'MStrokeJoin', 'VBStrokeJoin', and 'ViewStrokeJoin' respectively.
-- 
-- The 'StackProperty' constructor has been added with the 'StOffset'
-- and 'StSort' constructors. As discussed below this is a breaking change
-- since the old StackProperty type has been renamed to 'StackOffset'.
-- 
-- The 'ScaleProperty' type has seen significant enhancement, by adding
-- the constructors: 'SAlign', 'SBase', 'SBins', 'SConstant' and
-- 'SExponent'.  THe 'Scale' tye has added 'ScSymLog' 'ScQuantile',
-- 'ScQuantize', and 'ScThreshold'.
-- 
-- The 'SortProperty' type has new constructors: 'CustomSort',
-- 'ByRepeatOp', 'ByFieldOp', and 'ByChannel'. See the breaking-changes
-- section below for the constructors that were removed.
-- 
-- The 'AxisProperty' type has seen significant additions, including:
-- 'AxBandPosition', 'AxDomainColor', 'AxDomainDash',
-- 'AxDomainDashOffset', 'AxDomainOpacity', 'AxDomainWidth',
-- 'AxFormatAsNum', 'AxFormatAsTemporal', 'AxGridColor', 'AxGridDash',
-- 'AxGridDashOffset', 'AxGridOpacity', 'AxGridWidth', 'AxLabelAlign',
-- 'AxLabelBaseline', 'AxLabelBound', 'AxLabelColor', 'AxLabelFlush',
-- 'AxLabelFlushOffset', 'AxLabelFont', 'AxLabelFontSize',
-- 'AxLabelFontStyle', 'AxLabelFontWeight', 'AxLabelLimit',
-- 'AxLabelOpacity', 'AxLabelSeparation', 'AxTickColor', 'AxTickDash',
-- 'AxTickDashOffset', 'AxTickExtra', 'AxTickMinStep', 'AxTickOffset',
-- 'AxTickOpacity', 'AxTickRound', 'AxTickWidth', 'AxNoTitle',
-- 'AxTitleAnchor', 'AxTitleBaseline', 'AxTitleColor', 'AxTitleFont',
-- 'AxTitleFontSize', 'AxTitleFontStyle', 'AxTitleFontWeight',
-- 'AxTitleLimit', 'AxTitleOpacity', 'AxTitleX', and 'AxTitleY'.
-- 
-- The 'AxisConfig' has seen a similar enhancement, and looks similar
-- to the above apart from the constructors do not start with @Ax@.
-- 
-- The 'LegendConfig' type has been significantly expanded and, as
-- discussed in the Breaking Changes section, changed. It has gained:
-- 'LeClipHeight', 'LeColumnPadding', 'LeColumns', 'LeGradientDirection',
-- 'LeGradientHorizontalMaxLength', 'LeGradientHorizontalMinLength',
-- 'LeGradientLength', 'LeGradientOpacity', 'LeGradientThickness',
-- 'LeGradientVerticalMaxLength', 'LeGradientVerticalMinLength',
-- 'LeGridAlign', 'LeLabelFontStyle', 'LeLabelFontWeight',
-- 'LeLabelOpacity', 'LeLabelOverlap', 'LeLabelPadding',
-- 'LeLabelSeparation', 'LeLayout', 'LeLeX', 'LeLeY', 'LeRowPadding',
-- 'LeSymbolBaseFillColor', 'LeSymbolBaseStrokeColor', 'LeSymbolDash',
-- 'LeSymbolDashOffset', 'LeSymbolDirection', 'LeSymbolFillColor',
-- 'LeSymbolOffset', 'LeSymbolOpacity', 'LeSymbolStrokeColor', 'LeTitle',
-- 'LeNoTitle', 'LeTitleAnchor', 'LeTitleFontStyle', 'LeTitleOpacity',
-- and 'LeTitleOrient'.
-- 
-- The 'LegendOrientation' type has gained 'LOTop' and 'LOBottom'.
-- 
-- The 'LegendLayout' and 'BaseLegendLayout' types are new, and used
-- with 'LeLayout' to define the legent orient group.
-- 
-- The 'LegendProperty' type gained: 'LClipHeight', 'LColumnPadding',
-- 'LColumns', 'LCornerRadius', 'LDirection', 'LFillColor',
-- 'LFormatAsNum', 'LFormatAsTemporal', 'LGradientLength',
-- 'LGradientOpacity', 'LGradientStrokeColor', 'LGradientStrokeWidth',
-- 'LGradientThickness', 'LGridAlign', 'LLabelAlign', 'LLabelBaseline',
-- 'LLabelColor', 'LLabelFont', 'LLabelFontSize', 'LLabelFontStyle',
-- 'LLabelFontWeight', 'LLabelLimit', 'LLabelOffset', 'LLabelOpacity',
-- 'LLabelOverlap', 'LLabelPadding', 'LLabelSeparation', 'LRowPadding',
-- 'LStrokeColor', 'LSymbolDash', 'LSymbolDashOffset',
-- 'LSymbolFillColor', 'LSymbolOffset', 'LSymbolOpacity', 'LSymbolSize',
-- 'LSymbolStrokeColor', 'LSymbolStrokeWidth', 'LSymbolType',
-- 'LTickMinStep', 'LNoTitle', 'LTitleAlign', 'LTitleAnchor',
-- 'LTitleBaseline', 'LTitleColor', 'LTitleFont', 'LTitleFontSize',
-- 'LTitleFontStyle', 'LTitleFontWeight', 'LTitleLimit', 'LTitleOpacity',
-- 'LTitleOrient', 'LTitlePadding', 'LeX', and 'LeY'.
-- 
-- 'Projection' has gained the 'Identity' constructor. The
-- 'ProjectionProperty' type has gained 'PrScale', 'PrTranslate',
-- 'PrReflectX', and 'PrReflectY'. The 'GraticuleProperty' type was
-- added to configure the appearance of graticules created with
-- 'graticule'.
-- 
-- The 'CompositionAlignment' type was added and is used with 'align',
-- 'alignRC', 'LeGridAlign', and 'LGridAlign'.
-- 
-- The 'Bounds' type was added for use with 'bounds'.
-- 
-- The 'ImputeProperty' and 'ImMethod' types were added for use with
-- 'impute' and 'PImpute'.
-- 
-- The 'ScaleConfig' type has gained 'SCBarBandPaddingInner',
-- 'SCBarBandPaddingOuter', 'SCRectBandPaddingInner', and
-- 'SCRectBandPaddingOuter'.
-- 
-- The 'SelectionProperty' type has gained 'Clear' and 'SInit'.
-- 
-- The Channel type has gained: 'ChLongitude', 'ChLongitude2',
-- 'ChLatitude', 'ChLatitude2', 'ChFill', 'ChFillOpacity', 'ChHref',
-- 'ChKey', 'ChStroke', 'ChStrokeOpacity'.  'ChStrokeWidth', 'ChText',
-- and 'ChTooltip'.
-- 
-- The 'TitleConfig' type has gained: 'TFontStyle', 'TFrame', 'TStyle',
-- and 'TZIndex'.
-- 
-- The 'TitleFrame' type is new and used with 'TFrame' from 'TitleConfig'.
-- 
-- The 'ViewBackground' type is new and used with 'viewBackground'.
-- 
-- The 'ViewConfig' type has gained 'ViewCornerRadius', 'ViewOpacity',
-- 'ViewStrokeCap', 'ViewStrokeJoin', and 'ViewStrokeMiterLimit'.
-- 
-- The 'ConfigurationProperty' type, used with 'configuration', has
-- gained 'ConcatStyle', 'FacetStyle', 'GeoshapeStyle', 'HeaderStyle',
-- 'NamedStyles', and 'TrailStyle' constructors.
-- 
-- The 'ConcatConfig' type was added for use with the 'ConcatStyle',
-- and the 'FacetConfig' type for the 'FacetStyle'
-- configuration settings.
-- 
-- The 'HeaderProperty' type has gained: 'HFormatAsNum',
-- 'HFormatAsTemporal', 'HNoTitle', 'HLabelAlign', 'HLabelAnchor',
-- 'HLabelAngle', 'HLabelColor', 'HLabelFont', 'HLabelFontSize',
-- 'HLabelLimit', 'HLabelOrient', 'HLabelPadding', 'HTitleAlign',
-- 'HTitleAnchor', 'HTitleAngle', 'HTitleBaseline', 'HTitleColor',
-- 'HTitleFont', 'HTitleFontSize', 'HTitleFontWeight', 'HTitleLimit',
-- 'HTitleOrient', and 'HTitlePadding'.
-- 
-- The 'HyperlinkChannel' type has gained 'HBinned'.
-- 
-- The 'FacetChannel' type has gained 'FSort', 'FTitle', and 'FNoTitle'.
-- 
-- The 'TextChannel' type has gained 'TBinned', 'TFormatAsNum',
-- 'TFormatAsTemporal', 'TTitle', and 'TNoTitle'.
-- 
-- The 'TooltipContent' type was added, for use with 'MTooltip'.
-- 
-- There are a number of __breaking changes__ in this release (some
-- of which were mentioned above):
--
-- * The 'title' function now takes a second argument, a list of 'TitleConfig'
--   values for configuring the appearance of the title.
--
-- * The @SReverse@ constructor was removed from 'ScaleProperty' as it
--   represented a Vega, rather than Vega-Lite, property. The @xSort@
--   constructors are used to change the order of an item (e.g.
--   'PSort', 'MSort').
--
-- * The @ScSequential@ constructor was removed from 'Scale' as
--   'ScLinear' should be used.
--
-- * The 'SortProperty' type has had a number of changes: the @Op@,
--   @ByField@, and @ByRepeat@ constructors have been removed, and
--   'ByRepeatOp', 'ByFieldOp', and 'ByChannel' constructors have been
--   added.
--
-- * The @AxTitleMaxLength@ and @TitleMaxLength@ constructors have been
--   removed (from 'AxisProperty' and 'AxisConfig' respectively) as they
--   are invalid. The 'AxTitleLimit' (new in this release) and
--   'TitleLimit' constructors should be used instead.
--
-- * 'AxisProperty': the 'AxValues' constructor has been changed from
--   accepting a list of doubles to 'DataValues'. The 'AxDates'
--   constructor has been deprecated and 'AxValues' should be used
--   instead.
--
-- * There have been significant changes to the 'LegendConfig' type: the
--   @EntryPadding@, @GradientHeight@, @GradientLabelBaseline@,
--   @GradientWidth@, and @SymbolColor@ constructors have been removed;
--   the renaming constructors have been renamed so they all begin with
--   @Le@ (e.g. @Orient@ is now 'LeOrient', and 'Orient' has been added
--   to 'AxisConfig'); and new constructors have been added.
--
-- * The @StackProperty@ type has been renamed to 'StackOffset' and its
--   constructors have changed, and a new 'StackProperty'
--   type has been added (that references the 'StackOffset' type).
--
-- * The @Average@ constructor of 'Operation' was removed, and 'Mean'
--   should be used instead.
--
-- * The @LEntryPadding@ constructor of 'LegendProperty' was removed.
--
-- * The arguments to the 'MDataCondition', 'TDataCondition', and
--   'HDataCondition' constructors - of 'MarkChannel', 'TextChannel',
--   and 'HyperlinkChannel' respectively - have changed to support
--   accepting multiple expressions.
--
-- * The @MarkOrientation@ type has been renamed 'Orientation'.
--
-- * The constructors of the 'ViewConfig' type have been renamed so they
--   all begin with @View@ (to match 'ViewWidth' and 'ViewHeight').
--
-- * The constructors of the 'ProjectionProperty' type have been renamed
--   so that they begin with @Pr@ rather than @P@ (to avoid conflicts
--   with the 'PositionChannel' type).
--
-- * The 'Divide' constructor of 'BinProperty' now takes a list of
--   Doubles rather than two.
--
-- * The 'TitleConfig' type has gained the following constructors:
--   'TFontStyle', 'TFrame', 'TStyle', and 'TZIndex'. The 'TitleFrame'
--   type was added for use with 'TFrame'.
--
-- * The 'ArgMax' and 'ArgMin' constructors of 'Operation' now take an
--   optional field name, to allow them to be used as part of an encoding
--   aggregation (e.g. with 'PAggregate').
--
-- * The \"z index" value has changed from an 'Int' to the 'ZIndex' type.


--- helpers

-- This could be extended to any Ord type but specialize for now to Double
clamped ::
  Double
  -- ^ minimum value allowed
  -> Double
  -- ^ maximum value allowed (must be > the minimum value)
  -> Double
  -- ^ user value
  -> Double
clamped xmin xmax x = max xmin (min xmax x)


aggregate_ :: Operation -> LabelledSpec
aggregate_ op = "aggregate" .= operationSpec op

type_ :: T.Text -> LabelledSpec
type_ t = "type" .= t

field_ :: T.Text -> LabelledSpec
field_ f = "field" .= f

-- could restrict to ascending/descending
order_ :: T.Text -> LabelledSpec
order_ o = "order" .= o

op_ :: Operation -> LabelledSpec
op_ op = "op" .= operationSpec op

repeat_ :: Arrangement -> LabelledSpec
repeat_ arr = "repeat" .= arrangementLabel arr

sort_ :: [SortProperty] -> LabelledSpec
sort_ ops = "sort" .= sortPropertySpec ops

header_ :: [HeaderProperty] -> LabelledSpec
header_ hps = "header" .= object (map headerProperty hps)

impute_ :: [ImputeProperty] -> LabelledSpec
impute_ ips = "impute" .= object (map imputeProperty ips)

mprops_ :: T.Text -> [MarkProperty] -> LabelledSpec
mprops_ f mps = f .= object (map markProperty mps)

mchan_ :: T.Text -> [MarkChannel] -> LabelledSpec
mchan_ f ms = f .= object (concatMap markChannelProperty ms)

timeUnit_ :: TimeUnit -> LabelledSpec
timeUnit_ tu = "timeUnit" .= timeUnitLabel tu

mtype_ :: Measurement -> LabelledSpec
mtype_ m = "type" .= measurementLabel m

-- The assumption at the moment is that it's always correct to
-- replace the empty list by null.
--
scaleProp_ :: [ScaleProperty] -> LabelledSpec
scaleProp_ [] = "scale" .= A.Null
scaleProp_ sps = "scale" .= object (map scaleProperty sps)

scaleConfig_ :: [ScaleConfig] -> LabelledSpec
-- scaleConfig_ [] = "scale" .= A.Null  -- not sure here
scaleConfig_ scs = "scale" .= object (map scaleConfigProperty scs)


legendProp_ :: [LegendProperty] -> LabelledSpec
legendProp_ [] = "legend" .= A.Null
legendProp_ lps = "legend" .= object (map legendProperty lps)


value_ :: T.Text -> LabelledSpec
value_ v = "value" .= v


selCond_ :: (a -> [LabelledSpec]) -> BooleanOp -> [a] -> [a] -> [LabelledSpec]
selCond_ getProps selName ifClause elseClause =
  let h = ("condition", hkey)
      toProps = concatMap getProps
      hkey = object (("selection", booleanOpSpec selName) : toProps ifClause)
      hs = toProps elseClause
  in h : hs

-- Special case the single-condition check, so that I don't get false
-- positives when comparing against the Vega-Lite specification. There
-- should be no actionable difference from this special case (i.e.
-- the output being '[object]' and 'object' have the same meaning).
--
-- There is also the simplification to replace
--      test: { selection: xxx }
-- by
--      selection: xxx
-- which happens for the Selection operator.
--
dataCond_ :: (a -> [LabelledSpec]) -> [(BooleanOp, [a])] -> [a] -> [LabelledSpec]
dataCond_ getProps tests elseClause =
  let h = ("condition", condClause)
      condClause = case conds of
                     [cond] -> cond
                     _ -> toJSON conds
      conds = map testClause tests
      testClause (Selection sel, ifClause) =
        object (("selection" .= sel) : toProps ifClause)
      testClause (predicate, ifClause) =
        object (("test", booleanOpSpec predicate) : toProps ifClause)
      toProps = concatMap getProps
      hs = toProps elseClause
  in h : hs



fromT :: T.Text -> VLSpec
fromT = toJSON

fromF :: Double -> VLSpec
fromF = toJSON

---------


-- | A Vega Lite visualization, created by
--   'toVegaLite'. The contents can be extracted with 'fromVL'.
--
newtype VegaLite =
  VL {
  fromVL :: VLSpec
  -- ^ Extract the specification for passing to a VegaLite visualizer.
  --
  --   > let vlSpec = fromVL vl
  --   > Data.ByteString.Lazy.Char8.putStrLn (Data.Aeson.Encode.Pretty.encodePretty vlSpec)
  --
  --   Note that there is __no__ validation done to ensure that the
  --   output matches the Vega Lite schema. That is, it is possible to
  --   create an invalid visualization with this module (e.g. missing a
  --   data source or referring to an undefined field).
  }

-- | The Vega-Lite specification is represented as JSON.
type VLSpec = Value

baseSchema :: T.Text
baseSchema = "https://vega.github.io/schema/vega-lite/"

-- | The latest version 2 Vega-Lite schema (equivalent to
--   @'vlSchema' 2 Nothing Nothing Nothing@).
vlSchema2 :: T.Text
vlSchema2 = vlSchema 2 Nothing Nothing Nothing

-- | The latest version 3 Vega-Lite schema (equivalent to
--   @'vlSchema' 3 Nothing Nothing Nothing@).
vlSchema3 :: T.Text
vlSchema3 = vlSchema 3 Nothing Nothing Nothing

-- | The latest version 4 Vega-Lite schema (equivalent to
--   @'vlSchema' 4 Nothing Nothing Nothing@).
vlSchema4 :: T.Text
vlSchema4 = vlSchema 4 Nothing Nothing Nothing

-- | Create the Vega-Lite schema for an arbitrary version. See
--   <https://github.com/vega/schema> for more information on naming
--   and availability.
--
--   There is no validation of the input values.
--
--   At the time of writing the latest version 4 schema - which
--   is @https://vega.github.io/schema/vega-lite/v4.0.0-beta.0.json@ -
--   can be specified as
--
--   @vlSchema 4 (Just 0) (Just 0) (Just "-beta.0")@
--
--   whereas
--
--   @vlSchema 4 Nothing Nothing Nothing@
--
--   refers to the latest version.
--
vlSchema ::
  Natural
  -- ^ The major version
  -> Maybe Natural
  -- ^ The minor version
  -> Maybe Natural
  -- ^ The \"micro\" version
  -> Maybe T.Text
  -- ^ Anything beyond "major.minor.micro" (e.g. \"-beta.0").
  -> T.Text
  -- ^ The Vega-Lite Schema
vlSchema major mminor mmicro mextra =
  let versions = [ pure (showNat major)
                 , showNat <$> mminor
                 , showNat <$> mmicro
                 ]
      version = T.intercalate "." (catMaybes versions)

  in baseSchema <> "v" <> version <> fromMaybe "" mextra <> ".json"


showNat :: Natural -> T.Text
showNat = T.pack . show


{-|

Convert a list of Vega-Lite specifications into a single JSON object
that may be passed to Vega-Lite for graphics generation. Commonly
these will include at least a data, mark, and encoding specification.

While simple properties like 'mark' may be provided directly, it is
usually clearer to label more complex ones such as encodings as
separate expressions. This becomes increasingly helpful for
visualizations that involve composition of layers, repeats and facets.

Specifications can be built up by chaining a series of functions (such
as 'dataColumn' or 'position' in the example below). Functional
composition using the '.' operator allows this to be done compactly.

@
let dat = 'dataFromColumns' []
          . 'dataColumn' "a" ('Strings' [ \"C", \"C", \"D", \"D", \"E", \"E" ])
          . 'dataColumn' "b" ('Numbers' [ 2, 7, 1, 2, 6, 8 ])

    enc = 'encoding'
          . 'position' 'X' [ 'PName' "a", 'PmType' 'Nominal' ]
          . 'position' 'Y' [ 'PName' "b", 'PmType' 'Quantitative', 'PAggregate' 'Mean' ]

in 'toVegaLite' [ dat [], 'mark' 'Bar' [], enc [] ]
@

The schema used is <https://github.com/vega/schema version 3 of Vega-Lite>,
although there are some differences, in part because of bugs in @hvega@ -
in which case please [report an issue](https://github.com/DougBurke/hvega/issues) - but also because of issues with the Vega-Lite spec (for instance there
are several minor issues I have reported against version 3.3.0 of the
Vega-Lite schema).

Use 'toVegaLiteSchema' if you need to create a Vega-Lite specification
which uses a different version of the schema.

-}

-- TODO:
--    Should the input data, or converted to VLSpec, be stored
--    without the $schema key, so it can be easily combined with
--    other visualizations?
--
--    Could we make VegaLite a Semigroup so you can easily combine
--    specifications? However, what would that mean (concatenation,
--    if so what direction, anything else?)

toVegaLite :: [PropertySpec] -> VegaLite
toVegaLite = toVegaLiteSchema vlSchema3


{-|
A version of 'toVegaLite' that allows you to change the Vega-Lite
schema version of the visualization.

@
'toVegaLiteSchema' 'vlSchema4' props
@
-}

toVegaLiteSchema ::
  T.Text
  -- ^ The schema to use (e.g. 'vlSchema4' or created with 'vlSchema').
  -> [PropertySpec]
  -- ^ The visualization.
  -> VegaLite
toVegaLiteSchema schema props =
  let kvals = ("$schema" .= schema) : map toProp props
      toProp = first vlPropertyLabel

  in VL { fromVL = object kvals }


{-|

Combines a list of labelled specifications into a single specification.
This is useful when you wish to create
a single page with multiple visulizualizations.

@
'combineSpecs'
    [ ( "vis1", myFirstVis )
    , ( "vis2", mySecondVis )
    , ( "vis3", myOtherVis )
    ]
@
-}
combineSpecs :: [LabelledSpec] -> VLSpec
combineSpecs = object

{-|

Converts VegaLite to html Text. Uses Vega-Embed.

@since 0.2.1.0
-}
toHtml :: VegaLite -> TL.Text
toHtml vl = TL.unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<head>"
  , "  <!-- Import Vega 5 & Vega-Lite 3 (does not have to be from CDN) -->"
  , "  <script src=\"https://cdn.jsdelivr.net/npm/vega@3\"></script>"
  , "  <script src=\"https://cdn.jsdelivr.net/npm/vega-lite@2\"></script>"
  , "  <!-- Import vega-embed -->"
  , "  <script src=\"https://cdn.jsdelivr.net/npm/vega-embed@3\"></script>"
  , "</head>"
  , "<body>"
  , "<div id=\"vis\"></div>"
  , "<script type=\"text/javascript\">"
  , ("  var spec = " <> (A.encodeToLazyText $ fromVL vl) <> ";")
  , "  vegaEmbed(\'#vis\', spec).then(function(result) {"
  , "  // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view"
  , "  }).catch(console.error);"
  , "</script>"
  , "</body>"
  , "</html>"
  ]

{-|

Converts VegaLite to an html file. Uses Vega-Embed.

@since 0.2.1.0
-}
toHtmlFile :: FilePath -> VegaLite -> IO ()
toHtmlFile file = TL.writeFile file . toHtml

{-|

Create a specification sufficient to define an element in a composed visualization
such as a superposed layer or juxtaposed facet. Typically a layer will contain a
full set of specifications that define a visualization with
the exception of the data specification which is usually defined outside of any one
layer. Whereas for repeated and faceted specs, the entire specification is provided.

@
spec1 = asSpec [ enc1 [], 'mark' 'Line' [] ]
@
-}
asSpec :: [PropertySpec] -> VLSpec
asSpec = object . map (first vlPropertyLabel)


{-|

Specifies a list of geo features to be used in a geoShape specification.
Each feature object in this collection can be created with the 'geometry'
function.

@
'geoFeatureCollection'
    [ 'geometry' ('GeoPolygon' [ [ ( -3, 59 ), ( -3, 52 ), ( 4, 52 ), ( -3, 59 ) ] ])
        [ ( "myRegionName", 'Str' "Northern region" ) ]
    , 'geometry' ('GeoPolygon' [ [ ( -3, 52 ), ( 4, 52 ), ( 4, 45 ), ( -3, 52 ) ] ])
        [ ( "myRegionName", 'Str' "Southern region" ) ]
    ]
@
-}
geoFeatureCollection :: [VLSpec] -> VLSpec
geoFeatureCollection geoms =
  object [ type_ "FeatureCollection"
         , "features" .=  geoms
         ]


{-|

Specifies a list of geometry objects to be used in a geoShape specification.
Each geometry object in this collection can be created with the 'geometry'
function.

@
'geometryCollection'
    [ 'geometry' ('GeoPolygon' [ [ ( -3, 59 ), ( 4, 59 ), ( 4, 52 ), ( -3, 59 ) ] ]) []
    , 'geometry' ('GeoPoint' -3.5 55.5) []
    ]
@
-}
geometryCollection :: [VLSpec] -> VLSpec
geometryCollection geoms =
  object [ type_ "GeometryCollection"
         , "geometries" .= geoms
         ]


{-|

Create a named aggregation operation on a field that can be added to a transformation.
For further details see the
<https://vega.github.io/vega-lite/docs/aggregate.html#aggregate-op-def Vega-Lite documentation>.

@
'transform'
    . 'aggregate'
        [ 'opAs' 'Min' "people" "lowerBound"
        , 'opAs' 'Max' "people" "upperBound"
        ]
        [ "age" ]
@
-}
opAs ::
  Operation
  -- ^ The aggregation operation to use.
  -> T.Text
  -- ^ The name of the field which is to be aggregated (when the operation
  --   is 'Count' leave as the empty string).
  -> T.Text
  -- ^ The name given to the transformed data.
  -> VLSpec

-- The Count case is special-cased purely to make it easier to compare
-- the hvega output against the Veg-Lite examples. There should be no
-- semantic difference here.
--
opAs Count _ label =
  object [ op_ Count, "as" .= label ]
opAs op field label =
  object [ op_ op, field_ field, "as" .= label ]


{-|

Top-level Vega-Lite properties. These are the ones that define the core of the
visualization grammar. All properties are created by functions which can be
arranged into seven broad groups:

[Data Properties] These relate to the input data to be visualized. Generated by
'dataFromColumns', 'dataFromRows', 'dataFromUrl', 'dataFromSource',
'dataFromJson', 'dataSequence', 'sphere', and 'graticule'.

[Transform Properties] These indicate that some transformation of input data should
be applied before encoding them visually. Generated by 'transform'
and 'projection' they can include data transformations such as 'filter',
'binAs' and 'calculateAs' and geo transformations of longitude, latitude coordinates
used by marks such as 'Geoshape', 'Point', and 'Line'.

[Mark Properties] These relate to the symbols used to visualize data items. They
are generated by 'mark', and include types such as 'Circle', 'Bar', and 'Line'.

[Encoding Properties] These specify which data elements are mapped to which mark characteristics
(known as /channels/). Generated by 'encoding', they include encodings
such as 'position', 'color', 'size', 'shape', 'text', 'hyperlink', and
'order'.

[Composition Properties] These allow visualization views to be combined to form more
complex visualizations. Generated by 'layer', 'repeat', 'repeatFlow', 'facet', 'facetFlow',
'vlConcat', 'columns', 'hConcat', 'vConcat', 'asSpec', and 'resolve'.

[Interaction Properties] These allow interactions such as clicking, dragging and others
generated via a GUI or data stream to influence the visualization. Generated by
'selection'.

[Supplementary and Configuration Properties] These provide a means to add metadata and
styling to one or more visualizations. Generated by 'name', 'title', 'description',
'background', 'height', 'width', 'padding', 'autosize', 'viewBackground',
and 'configure'.

Prior to @0.4.0.0@ this was an opaque data type, as the constructors
were not exported. It is suggested that you do not import the
constructors to @VLProperty@ unless you need to transform the
Vega-Lite code in some manner (e.g. because @hvega@ is missing needed
functionality or is buggy).

Note that there is only a very-limited attempt to enforce the Vega-Lite
Schema (e.g. to ensure the required components are provided).

-}

-- based on schema 3.3.0 #/definitions/TopLevelSpec
-- which accepts one of
--    #/definitions/TopLevelUnitSpec
--    #/definitions/TopLevelFacetSpec
--    #/definitions/TopLevelLayerSpec
--    #/definitions/TopLevelRepeatSpec
--    #/definitions/TopLevelConcatSpec
--    #/definitions/TopLevelVConcatSpec
--    #/definitions/TopLevelHConcatSpec

data VLProperty
    = VLAlign
      -- ^ See 'align'.
      --
      --   @since 0.4.0.0
    | VLAutosize
      -- ^ See 'autosize'.
    | VLBackground
      -- ^ See 'background'.
    | VLBounds
      -- ^ See 'bounds'.
      --
      --   @since 0.4.0.0
    | VLCenter
      -- ^ See 'center' and 'centerRC'.
      --
      --   @since 0.4.0.0
    | VLColumns
      -- ^ See 'columns'.
      --
      --   @since 0.4.0.0
    | VLConcat
      -- ^ See 'vlConcat'.
      --
      --   @since 0.4.0.0
    | VLConfig
      -- ^ See 'configure'.
    | VLData
      -- ^ See 'dataFromColumns', 'dataFromJson', 'dataFromRows',
      --   'dataFromSource', 'dataFromUrl', 'dataName', 'dataSequence',
      --   'dataSequenceAs', 'graticule', 'noData', and 'sphere'.
    | VLDatasets
      -- ^ See 'datasets'.
    | VLDescription
      -- ^ See 'description'.
    | VLEncoding
      -- ^ See 'encoding'.
    | VLFacet
      -- ^ See 'facet' and 'facetFlow'.
    | VLHConcat
      -- ^ See 'hConcat'.
    | VLHeight
      -- ^ See 'height'.
    | VLLayer
      -- ^ See 'layer'.
    | VLMark
      -- ^ See 'mark'.
    | VLName
      -- ^ See 'name'.
    | VLPadding
      -- ^ See 'padding'.
    | VLProjection
      -- ^ See 'projection'.
    | VLRepeat
      -- ^ See 'repeat' and 'repeatFlow'.
    | VLResolve
      -- ^ See 'resolve'.
    | VLSelection
      -- ^ See 'selection'.
    | VLSpacing
      -- ^ See 'alignRC', 'spacing', and 'spacingRC'.
      --
      --   @since 0.4.0.0
    | VLSpecification
      -- ^ See 'specification'.
    | VLTitle
      -- ^ See 'title'.
    | VLTransform
      -- ^ See 'transform'.
    | VLUserMetadata
      -- ^ see 'usermetadata'.
      --
      --   @since 0.4.0.0
    | VLVConcat
      -- ^ See 'vConcat'.
    | VLViewBackground
      -- ^ See 'viewBackground'.
      --
      --   @since 0.4.0.0
    | VLWidth
      -- ^ See 'width'.


vlPropertyLabel :: VLProperty -> T.Text
vlPropertyLabel VLAlign = "align"
vlPropertyLabel VLAutosize = "autosize"
vlPropertyLabel VLBackground = "background"
vlPropertyLabel VLBounds = "bounds"
vlPropertyLabel VLCenter = "center"
vlPropertyLabel VLColumns = "columns"
vlPropertyLabel VLConcat = "concat"
vlPropertyLabel VLConfig = "config"
vlPropertyLabel VLData = "data"
vlPropertyLabel VLDatasets = "datasets"
vlPropertyLabel VLDescription = "description"
vlPropertyLabel VLEncoding = "encoding"
vlPropertyLabel VLFacet = "facet"
vlPropertyLabel VLHConcat = "hconcat"
vlPropertyLabel VLHeight = "height"
vlPropertyLabel VLLayer = "layer"
vlPropertyLabel VLMark = "mark"
vlPropertyLabel VLName = "name"
vlPropertyLabel VLPadding = "padding"
vlPropertyLabel VLProjection = "projection"
vlPropertyLabel VLRepeat = "repeat"
vlPropertyLabel VLResolve = "resolve"
vlPropertyLabel VLSelection = "selection"
vlPropertyLabel VLSpacing = "spacing"
vlPropertyLabel VLSpecification = "spec"
vlPropertyLabel VLTitle = "title"
vlPropertyLabel VLTransform = "transform"
vlPropertyLabel VLUserMetadata = "usermeta"
vlPropertyLabel VLVConcat = "vconcat"
vlPropertyLabel VLViewBackground = "view"
vlPropertyLabel VLWidth = "width"


{-|

Indicates the type of data to be parsed when reading input data. For @FoDate@
and @FoUtc@, the formatting specification can be specified using
<https://vega.github.io/vega-lite/docs/data.html#format D3's formatting specifiers>
or left as an empty string if default date formatting is to be applied. Care should
be taken when assuming default parsing of dates because different browsers can
parse dates differently. Being explicit about the date format is usually safer.

-}
data DataType
    = FoNumber
      -- ^ Indicate numeric data type to be parsed when reading input data.
    | FoBoolean
      -- ^ Indicate Boolean data type to be parsed when reading input data.
    | FoDate T.Text
      -- ^ Date format for parsing input data using
      --   [D3's formatting specifiers](https://vega.github.io/vega-lite/docs/data.html#format)
      --   or left as an empty string for default formatting.
    | FoUtc T.Text
      -- ^ Similar to 'FoDate' but for UTC format dates.


{-|

Specifies the type of format a data source uses. If the format is indicated by
the file name extension (@".tsv"@, @".csv"@, @".json"@) there is no need to indicate the
format explicitly. However this can be useful if the filename extension does not
indicate type (e.g. @".txt"@) or you wish to customise the parsing of a file. For
example, when specifying the @JSON@ format, its parameter indicates the name of
property field containing the attribute data to extract. For details see the
<https://vega.github.io/vega-lite/docs/data.html#format Vega-Lite documentation>.
-}
data Format
    = JSON T.Text
      -- ^ Property to be extracted from some JSON when it has some surrounding structure.
      --   e.g., specifying the property @values.features@ is equivalent to retrieving
      --   @json.values.features@ from a JSON object with a custom delimiter.
    | CSV
      -- ^ Comma-separated (CSV) data file format.
    | TSV
      -- ^ Tab-separated (TSV) data file format
    | DSV Char
      -- ^ The fields are separated by the given character (which must be a
      --   single 16-bit code unit).
      --
      --   @since 0.4.0.0
      {- This isn't in the current vega-lite v3 schema as far as I can see
    | Arrow
      -- ^ <https://observablehq.com/@theneuralbit/introduction-to-apache-arrow Apache Arrow> format.
      --
      -- @since 0.4.0.0
      -}
    | TopojsonFeature T.Text
      -- ^ A topoJSON feature format containing an object with the given name. For example:
      --
      -- @
      -- 'dataFromUrl' \"londonBoroughs.json\" ['TopojsonFeature' \"boroughs\"]
      -- @
    | TopojsonMesh T.Text
      -- ^ A topoJSON mesh format containing an object with the given name. Unlike
      --   'TopojsonFeature', the corresponding geo data are returned as a single unified mesh,
      --   not as individual GeoJSON features.
    | Parse [(T.Text, DataType)]
      -- ^ Parsing rules when processing some data text, specified as
      --   a list of tuples in the form @(fieldname,
      --   datatype)@. Useful when automatic type inference needs to
      --   be overridden, for example when converting integers representing
      --   years into dates and strings into numbers:
      --
      -- @
      -- 'dataFromUrl' \"myDataFile.csv\"
      --    [ 'Parse' [ ( \"year\", 'FoDate' \"%Y\" ), ( "\y\", 'FoNumber' ) ] ]
      -- @


{-|

A convenience type-annotation label. It is the same as 'Data'.

@since 0.4.0.0
-}
type PropertySpec = (VLProperty, VLSpec)

{-|

Represents a named Vega-Lite specification, usually generated by a
function in this module. You shouldn't need to create @LabelledSpec@
tuples directly, but they can be useful for type annotations.
-}
type LabelledSpec = (T.Text, VLSpec)

{-|

Represent those functions which can be chained together using function
composition to append new specifications onto an existing list.
-}
type BuildLabelledSpecs = [LabelledSpec] -> [LabelledSpec]

{-|

Represents a single column of data. Used when generating inline data with
'dataColumn' and 'dataFromColumns'.
-}
type DataColumn = [LabelledSpec]

{-|

Represents a single row of data. Used when generating inline data with
'dataRow' and 'dataFromRows'.
-}
type DataRow = VLSpec

{-|

Convenience type-annotation label for use with data generation functions.

@
myRegion : ['DataColumn'] -> 'Data'
myRegion =
    'dataFromColumns' []
        . 'dataColumn' "easting" ('Numbers' [ -3, 4, 4, -3, -3 ])
        . 'dataColumn' "northing" ('Numbers' [ 52, 52, 45, 45, 52 ])
@

It is the same as 'PropertySpec' (which was added in @0.4.0.0@),
but kept separate to help better-document code.

-}
type Data = (VLProperty, VLSpec)


{-|

Convenience type-annotation label to indicate a color value.
There is __no attempt__ to validate that the user-supplied input
is a valid color.

Any supported HTML color specification can be used, such as:

@
\"#eee\"
\"#734FD8\"
\"crimson\"
\"rgb(255,204,210)\"
\"hsl(180, 50%, 50%)\"
@

@since 0.4.0.0
-}

type Color = T.Text


{-|

Convenience type-annotation label to indicate an opacity value, which
lies in the range 0 to 1 inclusive. There is __no attempt__ to validate
that the user-supplied value falls in this range.

A value of 0 indicates fully transparent (see through), and 1 is
fully opaque (does not show anything it is on top of).

@since 0.4.0.0
-}

type Opacity = Double


{-|

Convenience type-annotation label to indicate an angle, which is measured
in degrees from the horizontal (so anti-clockwise).

The value should be in the range 0 to 360, inclusive, but __no attempt__ is
made to enforce this.

@since 0.4.0.0
-}

type Angle = Double


{-|

At what "depth" (z index) is the item to be drawn (a relative depth
for items in the visualization).

@since 0.4.0.0
-}

data ZIndex =
  ZBack
  -- ^ The item is drawn behind (this corresponds to the Vega Lite
  --   @0@ value).
  | ZFront
  -- ^ The item is drawn in front (this corresponds to the Vega Lite
  --   @1@ value).
  | ZValue Natural
  -- ^ This is provided in case there is any need to use a z index value
  --   other than @0@ or @1@.


-- Avoids the need to create a sepatate function, and may be useful
-- for users who are tweaking the hvega output.
--
instance A.ToJSON ZIndex where
  toJSON ZBack = A.Number 0
  toJSON ZFront = A.Number 1
  toJSON (ZValue z) = A.Number (fromIntegral z)
  {-# INLINE toJSON #-}

  toEncoding ZBack = E.int 0
  toEncoding ZFront = E.int 1
  toEncoding (ZValue z) = E.int (fromIntegral z)
  {-# INLINE toEncoding #-}


formatProperty :: Format -> [LabelledSpec]
formatProperty (JSON js) =
  let ps = [("type", "json")]
           <> if T.null (T.strip js) then [] else [("property", js)]
  in map (second toJSON) ps

formatProperty CSV = [("type", "csv")]
formatProperty TSV = [("type", "tsv")]
formatProperty (DSV delim) = [("type", "dsv"), "delimiter" .= delim]
-- formatProperty Arrow = [("type", "arrow")]
formatProperty (TopojsonFeature os) = [("type", "topojson")
                                      , "feature" .= os
                                      ]
formatProperty (TopojsonMesh os) = [("type", "topojson")
                                   , "mesh" .= os
                                   ]
formatProperty (Parse fmts) =
  let pObj = object (map (second dataTypeSpec) fmts)
  in [("parse", pObj)]


dataTypeSpec :: DataType -> VLSpec
dataTypeSpec dType =
  let s = case dType of
        FoNumber -> "number"
        FoBoolean -> "boolean"
        FoDate fmt | T.null fmt -> "date"
                   | otherwise -> "date:'" <> fmt <> "'"
        FoUtc fmt | T.null fmt -> "utc"
                  | otherwise -> "utc:'" <> fmt <> "'"
  in toJSON s


{-|

Create a row of data. A row comprises a list of (columnName, value) pairs.
The final parameter is the list of any other rows to which this is added.

This is expected to be used with 'dataFromRows'.

@
'dataRow' [(\"Animal\", 'Str' \"Fish\"), (\"Age\", 'Number' 28), (\"Year\", 'Str' "2010")] []
@
-}
dataRow :: [(T.Text, DataValue)] -> [DataRow] -> [DataRow]
dataRow rw = (object (map (second dataValueSpec) rw) :)


{-|

Create a dataset comprising a collection of named 'Data' items. Each data item
can be created with normal data generating functions such as 'dataFromRows' or
'dataFromJson'. These can be later referred to using 'dataFromSource'.

@
let toJS = Data.Aeson.toJSON
    obj = Data.Aeson.object

    dvals = 'dataFromRows' []
            . 'dataRow' [ ( "cat", 'Str' "a" ), ( "val", 'Number' 10 ) ]
            . 'dataRow' [ ( "cat", 'Str' "b" ), ( "val", 'Number' 18 ) ]
    json = toJS
            [ obj [ ( "cat", toJS "a" ), ( "val", toJS 120 ) ]
            , obj [ ( "cat", toJS "b" ), ( "val", toJS 180 ) ]
            ]

    enc = ...

in 'toVegaLite'
    [ 'datasets' [ ( \"myData\", dvals [] ),  ( \"myJson\", 'dataFromJson' json [] ) ]
    , 'dataFromSource' \"myData\" []
    , 'mark' 'Bar' []
    , enc []
    ]
@
-}
datasets :: [(T.Text, Data)] -> Data
datasets namedData =
  -- Follow Elm in parsing the structure to get what we want, but the code is
  -- written rather differently.
  --
  -- The input is expected to be a singleton list containing a pair.
  let converted = extract . snd
      specs = map (second converted) namedData

      convert :: Value -> Maybe [(T.Text, Value)]
      convert v = HM.toList <$> decode (encode v)

      extract din =
        let extract' [(_, v)] = Just v
            extract' _ = Nothing

        in fromMaybe din (convert din >>= extract')

  in (VLDatasets, object specs)


-- | This is for composed specifications, and it tells the visualization to
--   ignore the data from the parent.
--
--   @since 0.4.0.0
noData :: Data
noData = (VLData, A.Null)


{-|

Name to give a data source. Useful when a specification needs to reference a
data source, such as one generated via an API call.

@
dvals = 'dataName' \"myName\" ('dataFromUrl' \"myData.json\" [])
@

@since 0.4.0.0

-}

-- TODO: can we restrict this to only those with a VLProperty of VLData?

dataName ::
  T.Text
  -- ^ The name to give the data source
  -> Data
  -- ^ The data source to be named.
  -> Data
  -- ^ If the input @Data@ argument is not a data source then
  --   this is just the input value.
dataName s odata@(_, dataSpec) =
  -- follow Elm in parsing the structure to get what we want, but the code is
  -- written rather differently. This is based on the code used in datasets.
  --
  -- The input is expected to be a singleton list containing a pair.
  --
  let converted = convert >>= extract

      -- Aeson's objects are wrappers around a hash map, so this should be
      -- a relatively easy conversion. The type annotation isn't needed
      -- but left in for reference.
      --
      convert :: Maybe [(T.Text, Value)]
      convert = HM.toList <$> decode (encode dataSpec)

      extract [v] = Just v
      extract _ = Nothing

  in case converted of
       Just v -> (VLData, object [ "name" .= s, v ])
       _ -> odata


{-|

Generate a sequence of numbers as a data source. The resulting
sequence will have the name @\"data\"@. To give it an alternative name use
'dataSequenceAs'.

@
myData = 'dataSequence' 0 6.28 0.1
@

@since 0.4.0.0

-}
dataSequence ::
  Double     -- ^ start of the sequence (inclusive)
  -> Double  -- ^ end of the sequence (exclusive)
  -> Double  -- ^ step size
  -> Data
dataSequence start stop step =
  let vals = [("sequence", object svals)]
      svals = [ "start" .= start
              , "stop" .= stop
              , "step" .= step
              ]

  in (VLData, object vals)


{-|

Generate a sequence of numbers as a named data source. This extends
'dataSequence' by allowing you to name the data source.

@
myTheta = 'dataSequenceAs' 0 6.28 0.1 \"theta\"
@

@since 0.4.0.0

-}
dataSequenceAs ::
  Double     -- ^ start of the sequence (inclusive)
  -> Double  -- ^ end of the sequence (exclusive)
  -> Double  -- ^ step size
  -> T.Text  -- ^ The name of the data source
  -> Data
dataSequenceAs start stop step outName =
  let vals = [("sequence", object svals)]
      svals = [ "start" .= start
              , "stop" .= stop
              , "step" .= step
              , "as" .= outName
              ]

  in (VLData, object vals)


{-|

Declare a data source from a list of column values. Each column has a
specific type (e.g. 'Number' or 'String'), but different columns can have
different types.

Note that the columns are truncated to match the length of the shortest column.

@
'dataFromColumns' [ 'Parse' [ ( \"Year\", 'FoDate' "%Y" ) ] ]
  . 'dataColumn' \"Animal\" ('Strings' [ \"Fish\", \"Dog\", \"Cat\" ])
  . 'dataColumn' \"Age\" ('Numbers' [ 28, 12, 6 ])
  . 'dataColumn' \"Year\" ('Strings' [ "2010", "2014", "2015" ])
@
-}
dataFromColumns ::
  [Format]
  -- ^ An optional list of formatting instructions for the columns.
  --
  --   Simple numbers and strings do not normally need formatting, but it is
  --   good practice to explicitly declare date-time formats as handling of
  --   these values can vary between different viewers (e.g. browsers).
  --
  --   See the
  --   <https://vega.github.io/vega-lite/docs/data.html#format Vega-Lite documentation>
  --   for more details.
  -> [DataColumn]
  -- ^ The columns to add. This is expected to be created with one or more
  --   calls to 'dataColumn'.
  -> Data
dataFromColumns fmts cols =
  let dataArray = map object (transpose cols)

      vals = [("values", toJSON dataArray)]
             <> if null fmts
                then []
                else [("format", toJSON fmtObject)]

      fmtObject = object (concatMap formatProperty fmts)

  in (VLData, object vals)


transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs) : xss) =
  let heads = filterMap elmHead xss --
      tails = filterMap elmTail xss

      elmHead (h:_) = Just h
      elmHead [] = Nothing

      elmTail [] = Nothing
      elmTail (_:ts) = Just ts

      filterMap = mapMaybe

  in (x : heads) : transpose (xs : tails)


{-|

Declare a data source from a provided json specification. The most likely use-case
for specifying json inline is when creating <http://geojson.org geojson> objects,
when 'geometry', 'geometryCollection', and 'geoFeatureCollection' functions
may be used. For more general cases of json creation, consider 'Data.Aeson.encode'.

@
let geojson =
        'geometry' ('GeoPolygon' [ [ ( -3, 59 ), ( 4, 59 ), ( 4, 52 ), ( -3, 59 ) ] ]) []
in 'toVegaLite'
    [ 'width' 200
    , 'height' 200
    , 'dataFromJson' geojson []
    , 'projection' [ 'PrType' 'Orthographic' ]
    , 'mark' 'Geoshape' []
    ]
@
-}
dataFromJson :: VLSpec -> [Format] -> Data
dataFromJson vlspec fmts =
  let js = if null fmts
           then object [("values", vlspec)]
           else object [ ("values", vlspec)
                       , ("format",
                          object (concatMap formatProperty fmts)) ]
  in (VLData, js)


{-|

A single data value. This is used when a function or constructor
can accept values of different types (e.g. either a number or a string),
such as:
'dataRow', 'geometry', many constructors of the 'Filter' type,
'ImNewValue', and 'SInit'.

-}
data DataValue
    = Boolean Bool
    | DateTime [DateTime]
    | Number Double
    | Str T.Text
    | NullValue
      -- ^ Create a JavaScript @null@ value. This can be useful when
      --   explictly recoding a value as undefined, such as in the following
      --   example:
      --
      --   @
      --   'dataFromRows' []
      --     . 'dataRow' [("x", 'Number' 1), ("y", 'String' "good")]
      --     . 'dataRow' [("x", 'Number' 2), ("y", 'NullValue')]
      --     . 'dataRow' [("x", 'Number' 3), ("y", 'String' "bad")]
      --   @
      --
      --   For more-complex data sources - such as lists of defined
      --   and un-specified values, it is suggested that 'dataFromJson'
      --   be used rather than 'dataFromRows' or 'dataFromColumns'.
      --
      --   @since 0.4.0.0

dataValueSpec :: DataValue -> VLSpec
dataValueSpec (Boolean b) = toJSON b
dataValueSpec (DateTime dt) = object (map dateTimeProperty dt)
dataValueSpec (Number x) = toJSON x
dataValueSpec (Str t) = toJSON t
dataValueSpec NullValue = A.Null


{-|

A list of data values. This is used when a function or constructor
can accept lists of different types (e.g. either a list of numbers
or a list of strings), such as:
'dataColumn', 'CustomSort', 'FOneOf', or 'ImKeyVals'.

If your data contains undefined values then it is suggested that
you convert it to JSON (e.g. 'Value') and then use 'dataFromJson'.

-}
data DataValues
    = Booleans [Bool]
    | DateTimes [[DateTime]]
    | Numbers [Double]
    | Strings [T.Text]


dataValuesSpecs :: DataValues -> [VLSpec]
dataValuesSpecs (Booleans bs) = map toJSON bs
dataValuesSpecs (DateTimes dtss) = map (object . map dateTimeProperty) dtss
dataValuesSpecs (Numbers xs) = map toJSON xs
dataValuesSpecs (Strings ss) = map toJSON ss

{-|

Create a column of data. A column has a name and a list of values. The final
parameter is the list of any other columns to which this is added.

This is expected to be used with 'dataFromColumns'.

@
'dataColumn' \"Animal\" ('Strings' [ \"Cat\", \"Dog\", \"Mouse\"]) []
@
-}
dataColumn :: T.Text -> DataValues -> [DataColumn] -> [DataColumn]
dataColumn colName dVals xs =
  let col = case dVals of
        Booleans cs -> map toJSON cs
        DateTimes cs -> map dtToJSON cs
        Numbers cs -> map toJSON cs
        Strings cs -> map toJSON cs

      dtToJSON = object . map dateTimeProperty
      x = map (colName,) col

  in x : xs


{-|

Declare a data source from a provided list of row values. Each row
contains a list of tuples where the first value is a string
representing the column name, and the second the column value for that
row. Each column can have a value of a different type but
__you must ensure__
that when subsequent rows are added, they match the types of
previous values with shared column names.

Note though that generally if you are creating data inline (as opposed
to reading from a file), adding data by column is more efficient and
less error-prone.

@
dataFromRows [ 'Parse' [ ( \"Year\", 'FoDate' "%Y" ) ] ]
  . 'dataRow' [ ( \"Animal\", 'Str' \"Fish\" ), ( \"Age\", 'Number' 28 ), ( \"Year\", 'Str' "2010" ) ]
  . 'dataRow' [ ( \"Animal\", 'Str' \"Dog\" ), ( \"Age\", 'Number' 12 ), ( \"Year\", 'Str' "2014" ) ]
  . 'dataRow' [ ( \"Animal\", 'Str' \"Cat\" ), ( \"Age\", 'Number' 6 ), ( \"Year\", 'Str' "2015" ) ]
@
-}
dataFromRows ::
  [Format]
  -- ^ An optional list of formatting instructions for the rows.
  --
  --   Simple numbers and strings do not normally need formatting, but it is
  --   good practice to explicitly declare date-time formats as handling of
  --   these values can vary between different viewers (e.g. browsers).
  --
  --   See the
  --   <https://vega.github.io/vega-lite/docs/data.html#format Vega-Lite documentation>
  --   for more details.
  -> [DataRow]
  -- ^ The rows to add. This is expected to be created with one or more
  --   calls to 'dataRow'.
  -> Data
dataFromRows fmts rows =
  let kvs = ("values", toJSON rows)
            : if null fmts
              then []
              else [("format", object (concatMap formatProperty fmts))]
  in (VLData, object kvs)


{-|

Declare data from a named source. The source may be from named 'datasets' within
a specification or a named data source created via the
<https://vega.github.io/vega/docs/api/view/#data Vega View API>.
An optional list of field formatting instructions can be provided as the second
parameter or an empty list to use the default formatting. See the
<https://vega.github.io/vega-lite/docs/data.html#named Vega-Lite documentation>
for details.

@
'toVegaLite'
    [ 'datasets' [ ( "myData", dvals [] ),  ( "myJson", 'dataFromJson' json [] ) ]
    , 'dataFromSource' "myData" []
    , 'mark' 'Bar' []
    , ...
    ]
@
-}
dataFromSource :: T.Text -> [Format] -> Data
dataFromSource sourceName fmts =
  let kvs = ("name" .= sourceName)
            : if null fmts
              then []
              else [("format", object (concatMap formatProperty fmts))]
  in (VLData, object kvs)


{-|

Declare data source from a url. The url can be a local path on a web server
or an external http(s) url. Used to create a data ( property, specification ) pair.
An optional list of field formatting instructions can be provided as the second
parameter or an empty list to use the default formatting. See the
<https://vega.github.io/vega-lite/docs/data.html#format Vega-Lite documentation>
for details.

@
'dataFromUrl' "data/weather.csv" [ 'Parse' [ ( "date", 'FoDate' "%Y-%m-%d %H:%M" ) ] ]
@
-}
-- TODO: should this use a URL type?
dataFromUrl :: T.Text -> [Format] -> Data
dataFromUrl url fmts =
  let kvs = ("url" .= url)
            : if null fmts
              then []
              else [("format", object (concatMap formatProperty fmts))]
  in (VLData, object kvs)


-- | Type of visual mark used to represent data in the visualization.
--
--   The properties of the mark can be changed with the 'MarkProperty'
--   constructors - such as 'MHeight' and 'MWidth' -  although not all
--   properties apply to all marks.
--
data Mark
    = Area
      -- ^ An [area mark](https://vega.github.io/vega-lite/docs/area.html)
      --   for representing a series of data elements, such as in a stacked
      --   area chart or streamgraph.
    | Bar
      -- ^ [Bar mark](https://vega.github.io/vega-lite/docs/bar.html)
      --   for histograms, bar charts etc.
    | Boxplot
      -- ^ [Boxplot composite mark](https://vega.github.io/vega-lite/docs/boxplot.html)
      --   for showing summaries of statistical distributions.
      --
      --   By default, just box and whiskers are shown, but ticks and outliers
      --   can be specified explicitly. For example:
      --
      --   @
      --   'mark' Boxplot
      --       [ 'MTicks' [ 'MColor' \"black\", 'MSize' 8 ]
      --       , 'MBox' [ 'MFill' \"grey\" ]
      --       , 'MOutliers' [ 'MColor' \"firebrick\" ]
      --   ]
      --   @
      --
      --   @since 0.4.0.0
    | Circle
      -- ^ [Circle mark](https://vega.github.io/vega-lite/docs/circle.html)
      --   for representing points.
    | ErrorBar
      -- ^ [Errorbar composite mark](https://vega.github.io/vega-lite/docs/errorbar.html)
      --   for showing summaries of variation along a signal. By default
      --   no ticks are drawn. To add ticks with default properties use
      --   @`MTicks` []@, otherwise supply a list of configuration options:
      --
      --   @
      --   'mark' ErrorBar [ 'MTicks' [ 'MColor' \"black\", 'MSize' 8 ] ]
      --   @
      --
      --   @since 0.4.0.0
    | ErrorBand
      -- ^ [Errorband composite mark](https://vega.github.io/vega-lite/docs/errorband.html)
      --   for showing summaries of variation along a signal. By default
      --   no border is drawn. To add a border with default properties use
      --   @'MBorders' []@, otherwise supply a list of configuration options:
      --
      --   @
      --   'mark' ErrorBand [ 'MBorders' [ 'MColor' \"black\", 'MStrokeWidth' 0.5 ] ]
      --   @
      --
      --   @since 0.4.0.0
    | Geoshape
      -- ^ [Geoshape](https://vega.github.io/vega-lite/docs/geoshape.html)
      --   determined by geographically referenced coordinates.
    | Line
      -- ^ [Line mark](https://vega.github.io/vega-lite/docs/line.html)
      --   for symbolising a sequence of values.
    | Point
      -- ^ [Point mark](https://vega.github.io/vega-lite/docs/point.html)
      --   for symbolising a data point with a symbol.
    | Rect
      -- ^ [Rectangle mark](https://vega.github.io/vega-lite/docs/rect.html).
    | Rule
      -- ^ [Rule line](https://vega.github.io/vega-lite/docs/rule.html)
      --   connecting two vertices.
    | Square
      -- ^ [Square mark](https://vega.github.io/vega-lite/docs/square.html)
      --   for symbolising points.
    | Text
      -- ^ [Text mark](https://vega.github.io/vega-lite/docs/text.html)
      --   to be displayed at some point location.
    | Tick
      -- ^ Short line - [tick](https://vega.github.io/vega-lite/docs/tick.html) -
      --   mark for symbolising point locations.
    | Trail
      -- ^ [Trail mark](https://vega.github.io/vega-lite/docs/trail.html)
      --   (line with variable width along its length).
      --
      --   @since 0.4.0.0


markLabel :: Mark -> T.Text
markLabel Area = "area"
markLabel Bar = "bar"
markLabel Boxplot = "boxplot"
markLabel Circle = "circle"
markLabel ErrorBar = "errorbar"
markLabel ErrorBand = "errorband"
markLabel Line = "line"
markLabel Geoshape = "geoshape"
markLabel Point = "point"
markLabel Rect = "rect"
markLabel Rule = "rule"
markLabel Square = "square"
markLabel Text = "text"
markLabel Tick = "tick"
markLabel Trail = "trail"


{-|

Create a mark specification. All marks must have a type (first parameter) and
can optionally be customised with a list of mark properties such as interpolation
style for lines. To keep the default style for the mark, just provide an empty list
for the second parameter.

@
'mark' 'Circle' []
'mark' 'Line' ['MInterpolate' 'StepAfter']
@

@
let dvals = 'dataFromUrl' \"city.json\" ['TopojsonFeature' \"boroughs\"] []
    markOpts = 'mark' 'Geoshape' ['MFill' \"lightgrey\", 'MStroke' \"white\"]
in 'toVegaLite' [dvals, markOpts]
@
-}
mark :: Mark -> [MarkProperty] -> PropertySpec
mark mrk props =
  let jsName = toJSON (markLabel mrk)
      vals = if null props
             then jsName
             else object (("type" .= jsName) : map markProperty props)

  in (VLMark, vals)


{-|

Mark channel properties used for creating a mark channel encoding.
-}

-- https://vega.github.io/vega-lite/docs/encoding.html#mark-prop

data MarkChannel
    = MName T.Text
      -- ^ Field used for encoding with a mark property channel.
    | MRepeat Arrangement
      -- ^ Reference in a mark channel to a field name generated by 'repeatFlow'
      --   or 'repeat'. The parameter identifies whether reference is being made to
      --   fields that are to be arranged in columns, in rows, or a with a flow layout.
    | MmType Measurement
      -- ^ Level of measurement when encoding with a mark property channel.
    | MScale [ScaleProperty]
      -- ^ Scaling applied to a field when encoding with a mark property channel.
      --   The scale will transform a field's value into a color, shape, size etc.
      --
      --   Use an empty list to remove the scale.
    | MBin [BinProperty]
      -- ^ Discretize numeric values into bins when encoding with a mark property channel.
    | MBinned
      -- ^ Indicate that data encoding with a mark are already binned.
      --
      --   @since 0.4.0.0
    | MSort [SortProperty]
      -- ^ Sort order.
      --
      --   @since 0.4.0.0
    | MTimeUnit TimeUnit
      -- ^ Time unit aggregation of field values when encoding with a mark property channel.
    | MTitle T.Text
      -- ^ Title of a field when encoding with a mark property channel.
      --
      --   @since 0.4.0.0
    | MNoTitle
      -- ^ Draw no title.
      --
      --   @since 0.4.0.0
    | MAggregate Operation
      -- ^ Compute aggregate summary statistics for a field to be encoded with a
      --   mark property channel.
    | MLegend [LegendProperty]
      -- ^ Properties of a legend that describes a mark's encoding.
      --
      --   For no legend, provide an empty list.
    | MSelectionCondition BooleanOp [MarkChannel] [MarkChannel]
      -- ^ Make a mark channel conditional on interactive selection. The first parameter
      --   is a selection condition to evaluate; the second the encoding to apply if that
      --   selection is true; the third parameter is the encoding if the selection is false.
      --
      -- @
      -- 'color'
      --   [ MSelectionCondition ('SelectionName' \"myBrush\")
      --      [ 'MName' \"myField\", 'MmType' 'Ordinal' ]
      --      [ 'MString' \"grey\" ]
      --   ]
      -- @
    | MDataCondition [(BooleanOp, [MarkChannel])] [MarkChannel]
      -- ^ Make a text channel conditional on one or more predicate expressions. The first
      --   parameter is a list of tuples each pairing an expression to evaluate with the encoding
      --   if that expression is @True@. The second is the encoding if none of the expressions
      --   evaluate as @True@.
      --
      -- @
      -- 'color'
      --   [ MDataCondition [ ( 'Expr' \"datum.myField === null\", [ 'MString' \"grey\" ] ) ]
      --      [ MString \"black\" ]
      --   ]
      -- @
      --
      --   The arguments to this constructor have changed in @0.4.0.0@
      --   to support multiple expressions.
    | MPath T.Text
      -- ^ SVG path string used when encoding with a mark property channel. Useful
      --   for providing custom shapes.
    | MNumber Double
      -- ^ Literal numeric value when encoding with a mark property channel.
    | MString T.Text
      -- ^ Literal string value when encoding with a mark property channel.
    | MBoolean Bool
      -- ^ Boolean value when encoding with a mark property channel.

markChannelProperty :: MarkChannel -> [LabelledSpec]
markChannelProperty (MName s) = [field_ s]
markChannelProperty (MRepeat arr) = ["field" .= object [repeat_ arr]]
markChannelProperty (MmType t) = [mtype_ t]
markChannelProperty (MScale sps) = [scaleProp_ sps]
markChannelProperty (MLegend lps) = [legendProp_ lps]
markChannelProperty (MBin bps) = [bin bps]
markChannelProperty MBinned = [binned_]
markChannelProperty (MSort ops) = [sort_ ops]
markChannelProperty (MSelectionCondition selName ifClause elseClause) =
  selCond_ markChannelProperty selName ifClause elseClause
markChannelProperty (MDataCondition tests elseClause) =
  dataCond_ markChannelProperty tests elseClause
markChannelProperty (MTimeUnit tu) = [timeUnit_ tu]
markChannelProperty (MAggregate op) = [aggregate_ op]
markChannelProperty (MPath s) = ["value" .= s]
markChannelProperty (MNumber x) = ["value" .= x]
markChannelProperty (MString s) = ["value" .= s]
markChannelProperty (MBoolean b) = ["value" .= b]
markChannelProperty (MTitle s) = ["title" .= s]
markChannelProperty MNoTitle = ["title" .= A.Null]


{-|

Appearance of a line marker that is overlaid on an area mark.
For use with 'MLine'.

@since 0.4.0.0

-}

data LineMarker
  = LMNone
    -- ^ No line marker.
  | LMMarker [MarkProperty]
    -- ^ The properties of a line marker overlain on an area mark.
    --
    --   Use an empty list to use a filled point with default properties.


-- An empty object has the same meaning as true, so there is no real need to
-- treat 'LMMarker []' specially, but I don't think it complicates things
-- here.
--
lineMarkerSpec :: LineMarker -> VLSpec
lineMarkerSpec LMNone = toJSON False
lineMarkerSpec (LMMarker []) = toJSON True
lineMarkerSpec (LMMarker mps) = object (map markProperty mps)


{-|

Properties for customising the appearance of a mark. For details see the
<https://vega.github.io/vega-lite/docs/mark.html#config Vega-Lite documentation>.

Not all properties are valid for each mark type.

The Vega-Lite specification supports setting those properties that take
@['MarkProperty']@ also to a boolean value. This is currently not
supported in @hvega@.

-}

-- based on schema 3.3.0 #/definitions/MarkConfig
--
-- but it also contains a number of other properties

data MarkProperty
    = MAlign HAlign
      -- ^ Horizontal alignment of a text mark.
    | MAngle Angle
      -- ^ Rotation angle of a text mark.
    | MBandSize Double
      -- ^ Band size of a bar mark.
    | MBaseline VAlign
      -- ^ Vertical alignment of a text mark.
    | MBinSpacing Double
      -- ^ Offset between bars for a binned field using a bar mark.
      --
      --   The ideal value for this is either @0@ (preferred by statisticians)
      --   or @1@ (the Vega-Lite default value, D3 example style).
    | MBorders [MarkProperty]
      -- ^ Border properties for an errorband mark.
      --
      --   @since 0.4.0.0
    | MBox [MarkProperty]
      -- ^ Box-symbol properties for the boxplot mark.
      --
      --   @since 0.4.0.0
    | MClip Bool
      -- ^ Should a mark be clipped to the enclosing group's dimensions.
    | MColor Color
      -- ^ Default color of a mark. Note that 'MFill' and 'MStroke' have higher
      --   precedence and will override this if specified.
    | MCursor Cursor
      -- ^ Cursor to be associated with a hyperlink mark.
    | MContinuousBandSize Double
      -- ^ Continuous band size of a bar mark.
    | MDiscreteBandSize Double
      -- ^ Discrete band size of a bar mark.
    | MdX Double
      -- ^ Horizontal offset between a text mark and its anchor.
    | MdY Double
      -- ^ Vertical offset between a text mark and its anchor.
    | MExtent MarkErrorExtent
      -- ^ Extent of whiskers used in a boxplot, error bars, or error bands.
      --
      --   @since 0.4.0.0
    | MFill T.Text
      -- ^ Default fill color of a mark.
    | MFilled Bool
      -- ^ Should a mark's color should be used as the fill color instead of
      --   stroke color.
    | MFillOpacity Opacity
      -- ^ Fill opacity of a mark.
    | MFont T.Text
      -- ^ Font of a text mark. Can be any font name made accessible via
      -- a css file (or a generic font like \"serif\", \"monospace\" etc.).
    | MFontSize Double
      -- ^ Font size, in pixels, used by a text mark.
    | MFontStyle T.Text
      -- ^ Font style (e.g. \"italic\") used by a text mark.
    | MFontWeight FontWeight
      -- ^ Font weight used by a text mark.
    | MHeight Double
      -- ^ Explicitly set the height of a mark. See also 'MWidth'.
      --
      --   @since 0.4.0.0
    | MHRef T.Text
      -- ^ Hyperlink to be associated with a mark making it a clickable
      --   hyperlink.
      --
      --   @since 0.4.0.0
    | MInterpolate MarkInterpolation
      -- ^ Interpolation method used by line and area marks.
    | MLine LineMarker
      -- ^ How should the vertices of an area mark be joined?
      --
      --   @since 0.4.0.0
    | MMedian [MarkProperty]
      -- ^ Median-line properties for the boxplot mark.
      --
      --   @since 0.4.0.0
    | MOpacity Opacity
      -- ^ Overall opacity of a mark in the range 0 to 1.
    | MOrder Bool
      -- ^ Ordering of vertices in a line or area mark. If @True@ (the default),
      --   the order is determined by measurement type or order channel. If
      --   @False@, the original data order is used.
      --
      --   @since 0.4.0.0
    | MOrient Orientation
      -- ^ Orientation of a non-stacked bar, tick, area or line mark.
    | MOutliers [MarkProperty]
      -- ^ Outlier symbol properties for the boxplot mark.
      --
      --   @since 0.4.0.0
    | MPoint PointMarker
      -- ^ Appearance of a point marker joining the vertices of a line or area mark.
      --
      --   @since 0.4.0.0
    | MRadius Double
      -- ^ Polar coordinate radial offset of a text mark from its origin.
    | MRule [MarkProperty]
      -- ^ Rule (main line) properties for the errorbar and boxplot marks.
      --
      --   @since 0.4.0.0
    | MShape Symbol
      -- ^ Shape of a point mark.
    | MShortTimeLabels Bool
      -- ^ Aremonth and weekday names are abbreviated in a text mark?
    | MSize Double
      -- ^ Size of a mark.
    | MStroke T.Text
      -- ^ Default stroke color of a mark.
    | MStrokeCap StrokeCap
      -- ^ Cap style of a mark's stroke.
      --
      --   @since 0.4.0.0
    | MStrokeDash [Double]
      -- ^ The stroke dash style used by a mark, defined by an alternating 'on-off'
      --   sequence of line lengths, in pixels.
    | MStrokeDashOffset Double
      -- ^ The number of pixels before the first line dash is drawn.
    | MStrokeJoin StrokeJoin
      -- ^ Line segment join style of a mark's stroke.
      --
      --   @since 0.4.0.0
    | MStrokeMiterLimit Double
      -- ^ Mitre limit at which to bevel a join between line segments of a
      --   mark's stroke.
      --
      --   @since 0.4.0.0
    | MStrokeOpacity Opacity
      -- ^ Stroke opacity of a mark in the range 0 to 1.
    | MStrokeWidth Double
      -- ^ Stroke width of a mark in pixels.
    | MStyle [T.Text]
      -- ^ Names of custom styles to apply to a mark. Each should refer to a named style
      --   defined in a separate style configuration.
    | MTension Double
      -- ^ Interpolation tension used when interpolating line and area marks.
    | MText T.Text
      -- ^ Placeholder text for a text mark for when a text channel is not specified.
    | MTheta Double
      -- ^ Polar coordinate angle (clockwise from north in radians)
      --   of a text mark from the origin (determined by its
      --   x and y properties).
    | MThickness Double
      -- ^ Thickness of a tick mark.
    | MTicks [MarkProperty]
      -- ^ Tick properties for the errorbar or boxplot mark.
      --
      --   @since 0.4.0.0
    | MTooltip TooltipContent
      -- ^ The tooltip content for a mark.
      --
      --   @since 0.4.0.0
    | MWidth Double
      -- ^ Explicitly set the width of a mark (e.g. the bar width). See also
      --   'MHeight'.
      --
      --   @since 0.4.0.0
    | MX Double
      -- ^ X position of a mark.
      --
      --   @since 0.4.0.0
    | MX2 Double
      -- ^ X2 position of a mark. This is the secondary position for
      --   lines and area marks).
      --
      --   @since 0.4.0.0
    | MXOffset Double
      -- ^ X position offset of a mark.
      --
      --   @since 0.4.0.0
    | MX2Offset Double
      -- ^ X2 position offset of a mark.
      --
      --   @since 0.4.0.0
    | MY Double
      -- ^ Y position of a mark.
      --
      --   @since 0.4.0.0
    | MY2 Double
      -- ^ Y2 position of a mark. This is the secondary position for
      --   lines and area marks).
      --
      --   @since 0.4.0.0
    | MYOffset Double
      -- ^ Y position offset of a mark.
      --
      --   @since 0.4.0.0
    | MY2Offset Double
      -- ^ Y2 position offset of a mark.
      --
      --   @since 0.4.0.0



markProperty :: MarkProperty -> LabelledSpec
markProperty (MFilled b) = "filled" .= b
markProperty (MBorders mps) = mprops_ "borders" mps
markProperty (MBox mps) = mprops_ "box" mps
markProperty (MClip b) = "clip" .= b
markProperty (MColor col) = "color" .= col
markProperty (MCursor cur) = "cursor" .= cursorLabel cur
markProperty (MFill col) = "fill" .= col
markProperty (MHeight x) = "height" .= x
markProperty (MStroke t) = "stroke" .= t
markProperty (MStrokeCap sc) = "strokeCap" .= strokeCapLabel sc
markProperty (MStrokeOpacity x) = "strokeOpacity" .= x
markProperty (MStrokeWidth w) = "strokeWidth" .= w
markProperty (MStrokeDash xs) = "strokeDash" .= xs
markProperty (MStrokeDashOffset x) = "strokeDashOffset" .= x
markProperty (MStrokeJoin sj) = "strokeJoin" .= strokeJoinLabel sj
markProperty (MStrokeMiterLimit x) = "strokeMiterLimit" .= x
markProperty (MMedian mps) = mprops_ "median" mps
markProperty (MOpacity x) = "opacity" .= x
markProperty (MFillOpacity x) = "fillOpacity" .= x
markProperty (MStyle [style]) = "style" .= style  -- special case singleton
markProperty (MStyle styles) = "style" .= styles
markProperty (MInterpolate interp) = "interpolate" .= markInterpolationLabel interp
markProperty (MLine lm) = "line" .= lineMarkerSpec lm
markProperty (MTension x) = "tension" .= x
markProperty (MOrder b) = "order" .= b
markProperty (MOrient orient) = "orient" .= orientationSpec orient
markProperty (MOutliers mps) = mprops_ "outliers" mps
markProperty (MPoint pm) = "point" .= pointMarkerSpec pm
markProperty (MShape sym) = "shape" .= symbolLabel sym
markProperty (MSize x) = "size" .= x
markProperty (MAngle x) = "angle" .= x
markProperty (MAlign algn) = "align" .= hAlignLabel algn
markProperty (MBaseline va) = "baseline" .= vAlignLabel va
markProperty (MdX dx) = "dx" .= dx
markProperty (MdY dy) = "dy" .= dy
markProperty (MExtent mee) = markErrorExtentLSpec mee
markProperty (MFont fnt) = "font" .= fnt
markProperty (MFontSize x) = "fontSize" .= x
markProperty (MFontStyle fSty) = "fontStyle" .= fSty
markProperty (MFontWeight w) = "fontWeight" .= fontWeightSpec w
markProperty (MHRef s) = "href" .= s
markProperty (MRadius x) = "radius" .= x
markProperty (MRule mps) = mprops_ "rule" mps
markProperty (MText txt) = "text" .= txt
markProperty (MTheta x) = "theta" .= x
markProperty (MTicks mps) = mprops_ "ticks" mps
markProperty (MBinSpacing x) = "binSpacing" .= x
markProperty (MContinuousBandSize x) = "continuousBandSize" .= x
markProperty (MDiscreteBandSize x) = "discreteBandSize" .= x
markProperty (MShortTimeLabels b) = "shortTimeLabels" .= b
markProperty (MBandSize x) = "bandSize" .= x
markProperty (MThickness x) = "thickness" .= x
markProperty (MTooltip TTNone) = "tooltip" .= A.Null
markProperty (MTooltip tc) = "tooltip" .= object ["content" .= ttContentLabel tc]
markProperty (MWidth x) = "width" .= x
markProperty (MX x) = "x" .= x
markProperty (MY x) = "y" .= x
markProperty (MX2 x) = "x2" .= x
markProperty (MY2 x) = "y2" .= x
markProperty (MXOffset x) = "xOffset" .= x
markProperty (MYOffset x) = "yOffset" .= x
markProperty (MX2Offset x) = "x2Offset" .= x
markProperty (MY2Offset x) = "y2Offset" .= x


-- | How are strokes capped? This is used with 'MStrokeCap', 'VBStrokeCap',
--   and `ViewStrokeCap'.
--
--   @since 0.4.0.0

data StrokeCap
    = CButt
      -- ^ Butt stroke cap.
    | CRound
      -- ^ Rounded stroke cap.
    | CSquare
      -- ^ Square stroke cap.


strokeCapLabel :: StrokeCap -> T.Text
strokeCapLabel CButt = "butt"
strokeCapLabel CRound = "round"
strokeCapLabel CSquare = "square"


-- | How are strokes joined? This is used with 'MStrokeJoin', 'VBStrokeJoin',
--   and `ViewStrokeJoin'.
--
--
--   @since 0.4.0.0

data StrokeJoin
    = JMiter
      -- ^ Mitred stroke join.
    | JRound
      -- ^ Rounded stroke join.
    | JBevel
      -- ^ Bevelled stroke join.


strokeJoinLabel :: StrokeJoin -> T.Text
strokeJoinLabel JMiter = "miter"
strokeJoinLabel JRound = "round"
strokeJoinLabel JBevel = "bevel"


{-|

Create an encoding specification from a list of channel encodings,
such as 'position', 'color', 'size', 'shape'.

@
enc = 'encoding'
        . 'position' 'X' [ 'PName' \"Animal\", 'PmType' 'Ordinal' ]
        . 'position' 'Y' [ PName \"Age\", 'PmType' 'Quantitative' ]
        . 'shape' [ 'MName' \"Species\", 'MmType' 'Nominal' ]
        . 'size' [ 'MName' \"Population\", 'MmType' 'Quantitative' ]
@

The type of @enc@ in this example is @[LabelledSpec] -> PropertySpec@,
so it can either be used to add further encoding specifications or as
@enc []@ to create a specification.

-}
encoding :: [LabelledSpec] -> PropertySpec
encoding channels = (VLEncoding, object channels)

-- TODO:
--
--  encoding of X2/... shouldn't include the PmType in the output, apparently
--  so we could try and filter that out, or just rely on the user to not
--  add the PmType fields in this case.

{-|

Type of position channel, @X@ and @Y@ represent horizontal and vertical axis
dimensions on a plane and @X2@ and @Y2@ represent secondary axis dimensions where
two scales are overlaid in the same space. Geographic positions represented by
longitude and latiutude values are identified with @Longitude@, @Latitude@ and
their respective secondary equivalents. Such geographic position channels are
subject to a map projection (set using 'projection') before being placed graphically.

-}
data Position
    = X
    | Y
    | X2
    | Y2
    | XError
      -- ^ @since 0.4.0.0
    | XError2
      -- ^ @since 0.4.0.0
    | YError
      -- ^ @since 0.4.0.0
    | YError2
      -- ^ @since 0.4.0.0
    | Longitude
    | Latitude
    | Longitude2
    | Latitude2


{-|

Type of measurement to be associated with some channel.

-}

data Measurement
    = Nominal
      -- ^ Data are categories identified by name alone and which have no intrinsic order.
    | Ordinal
      -- ^ Data are also categories, but ones which have some natural order.
    | Quantitative
      -- ^ Data are numeric measurements typically on a continuous scale.
    | Temporal
      -- ^ Data represents time in some manner.
    | GeoFeature
      -- ^ Geospatial position encoding ('Longitude' and 'Latitude') should specify the 'PmType'
      -- as @Quantitative@. Geographically referenced features encoded as 'shape' marks
      -- should specify 'MmType' as @GeoFeature@ (Vega-Lite currently refers to this type
      -- as @<https://vega.github.io/vega-lite/docs/encoding.html geojson>@.


{-|

Type of binning property to customise. See the
<https://vega.github.io/vega-lite/docs/bin.html Vega-Lite documentation> for
more details.

This is used with: 'binAs', 'DBin', 'FBin', 'HBin', 'MBin', 'OBin',
'PBin', and 'TBin'.

-}

-- based on schema 3.3.0 #/definitions/BinParams

data BinProperty
    = AlreadyBinned Bool
      -- ^ Should the input data be treated as already binned?
      --
      --   @since 0.4.0.0
    | BinAnchor Double
      -- ^ A value in the binned domain at which to anchor the bins, shifting the bin
      --   boundaries if necessary to ensure that a boundary aligns with the anchor
      --   value.
      --
      --   @since 0.4.0.0
    | Base Double
      -- ^ The number base to ude for automatic bin determination.
      --
      --   Default is @10@.
    | Divide [Double]
      -- ^ Scale factors indicating allowable subdivisions.
      --
      --   Default is @[5, 2]@.
      --
      --   Prior to @0.4.0.0@ the @Divide@ constructor took two numbers.
    | Extent Double Double
      -- ^ The range (minimum, maximum) of the desired bin values.
    | MaxBins Int
      -- ^ The maxium number of bins.
      --
      --   Default is @6@ for 'row', 'column', and 'shape' channels,
      --   @10@ otherwise.
    | MinStep Double
      -- ^ A minimum allowable step size.
    | Nice Bool
      -- ^ If @True@, the bin boundaries are adjusted to use human-friendly values,
      --   such as multiples of ten.
      --
      --   Default is @True@.
    | Step Double
      -- ^ The step size to use between bins.
      --
      --   If specified, 'MaxBins' and other related options are ignored.
    | Steps [Double]
      -- ^ Pick the step size from this list.


binProperty :: BinProperty -> LabelledSpec
binProperty (AlreadyBinned b) = "binned" .= b
binProperty (BinAnchor x) = "anchor" .= x
binProperty (Base x) = "base" .= x
binProperty (Divide xs) = "divide" .= xs
binProperty (Extent mn mx) = "extent" .= [ mn, mx ]
binProperty (MaxBins n) = "maxbins" .= n
binProperty (MinStep x) = "minstep" .= x
binProperty (Nice b) = "nice" .= b
binProperty (Step x) = "step" .= x
binProperty (Steps xs) = "steps" .= xs


bin :: [BinProperty] -> LabelledSpec
bin [] = "bin" .= True
bin xs = "bin" .= object (map binProperty xs)

binned_ :: LabelledSpec
binned_ = "bin" .= fromT "binned"


{-|

Type of aggregation operation. See the
<https://vega.github.io/vega-lite/docs/aggregate.html#ops Vega-Lite documentation>
for more details.

The @Average@ constructor was removed in version @0.4.0.0@; use 'Mean' instead.

-}
data Operation
    = ArgMax (Maybe T.Text)
      -- ^ An input data object containing the maximum field value to be used
      --   in an aggregation operation.
      --
      --   If supplied as part of an encoding aggregation, the parameter
      --   should be 'Just' the name of the field to maximise. When used
      --   as part of a transform the parameter should be 'Nothing' as the
      --   field is specified in the 'aggregate' call.
      --
      --   Encoding example, to find the production budget for the maximum
      --   US grossing film in each genre:
      --
      --   @
      --   'encoding'
      --     . 'position' 'X'
      --                [ 'PName' \"Production_Budget\"
      --                , 'PmType' 'Quantitative'
      --                , 'PAggregate' ('ArgMax' ('Just' \"US_Gross\"))
      --                ]
      --     . 'position' 'Y' ['PName' \"Major_Genre\", 'PmType' 'Nominal']
      --   @
      --
      --   An example of its use as part of an 'aggregate' call:
      --
      --   @
      --   'transform'
      --     . 'aggregate'
      --         [ 'opAs' ('ArgMax' 'Nothing') \"US_Gross\" \"amUSGross\"]
      --         [\"Major_Genre\"]
      --   @
      --
      --   The optional field name was added in the @0.4.0.0@ release.
    | ArgMin (Maybe T.Text)
      -- ^ An input data object containing the minimum field value to be used
      --   in an aggregation operation. See 'ArgMax' for a discussion of the
      --   optional argument.
      --
      --   The optional field name was added in the @0.4.0.0@ release.
    | CI0
      -- ^ Lower 95% confidence interval to be used in an aggregation operation.
    | CI1
      -- ^ Upper 95% confidence interval to be used in an aggregation operation.
    | Count
      -- ^ Total count of data objects to be used in an aggregation operation.
    | Distinct
      -- ^ Count of distinct data objects to be used in an aggregation operation.
    | Max
      -- ^ Maximum field value to be used in an aggregation operation.
    | Mean
      -- ^ Mean field value to be used in an aggregation operation.
    | Median
      -- ^ Median field value to be used in an aggregation operation.
    | Min
      -- ^ Minimum field value to be used in an aggregation operation.
    | Missing
      -- ^ Count of @null@ or @undefined@ field value to be used in an aggregation operation.
    | Q1
      -- ^ Lower quartile boundary of field values to be used in an aggregation operation.
    | Q3
      -- ^ Upper quartile boundary of field values to be used in an aggregation operation.
    | Stderr
      -- ^ Standard error of field values to be used in an aggregate operation.
    | Stdev
      -- ^ Sample standard deviation of field values to be used in an aggregate operation.
    | StdevP
      -- ^ Population standard deviation of field values to be used in an aggregate operation.
    | Sum
      -- ^ Sum of field values to be used in an aggregate operation.
    | Valid
      -- ^ Count of values that are not @null@, @undefined@, or @NaN@ to be used in an
      -- aggregation operation.
    | Variance
      -- ^ Sample variance of field values to be used in an aggregate operation.
    | VarianceP
      -- ^ Population variance of field values to be used in an aggregate operation.


-- Unlike Elm, not checking if the string is empty for ArgMin/Max

operationSpec :: Operation -> VLSpec
operationSpec (ArgMax Nothing) = "argmax"
operationSpec (ArgMax (Just s)) = object ["argmax" .= s]
operationSpec (ArgMin Nothing) = "argmin"
operationSpec (ArgMin (Just s)) = object ["argmin" .= s]
operationSpec CI0 = "ci0"
operationSpec CI1 = "ci1"
operationSpec Count = "count"
operationSpec Distinct = "distinct"
operationSpec Max = "max"
operationSpec Mean = "mean"
operationSpec Median = "median"
operationSpec Min = "min"
operationSpec Missing = "missing"
operationSpec Q1 = "q1"
operationSpec Q3 = "q3"
operationSpec Stderr = "stderr"
operationSpec Stdev = "stdev"
operationSpec StdevP = "stdevp"
operationSpec Sum = "sum"
operationSpec Valid = "valid"
operationSpec Variance = "variance"
operationSpec VarianceP = "variancep"


-- | Identifies how repeated or faceted views are arranged.
--
--   This is used with a number of constructors: 'ByRepeatOp',
--   'HRepeat', 'MRepeat', 'ORepeat', 'PRepeat', and 'TRepeat'.

-- based on schema 3.3.0 #/definitions/RepeatRef

data Arrangement
    = Column
      -- ^ Column arrangement.
    | Row
      -- ^ Row arrangement.
    | Flow
      -- ^ Flow arrangement (aka \"repeat\").
      --
      --   @since 0.4.0.0


arrangementLabel :: Arrangement -> T.Text
arrangementLabel Column = "column"
arrangementLabel Row = "row"
arrangementLabel Flow = "repeat"  -- NOTE: not "flow"!


-- | How are stacks applied within a transform?
--
--   Prior to version @0.4.0.0@ the @StackProperty@ type was
--   what is now @StackOffset@.

data StackProperty
    = StOffset StackOffset
      -- ^ Stack offset.
      --
      --   @since 0.4.0.0
    | StSort [SortField]
      -- ^ Ordering within a stack.
      --
      --   @since 0.4.0.0


-- | Describes the type of stacking to apply to a bar chart.
--
--   In @0.4.0.0@ this was renamed from @StackProperty@ to @StackOffset@,
--   but the constructor names have not changed.
--
data StackOffset
    = StZero
      -- ^ Offset a stacked layout using a baseline at the foot of
      --   the stack.
    | StNormalize
      -- ^ Rescale a stacked layout to use a common height while
      --   preserving the relative size of stacked quantities.
    | StCenter
      -- ^ Offset a stacked layout using a central stack baseline.
    | NoStack
      -- ^ Do not stack marks, but create a layered plot.

stackOffsetSpec :: StackOffset -> VLSpec
stackOffsetSpec StZero = "zero"
stackOffsetSpec StNormalize = "normalize"
stackOffsetSpec StCenter = "center"
stackOffsetSpec NoStack = A.Null

stackOffset :: StackOffset -> LabelledSpec
stackOffset so = "stack" .= stackOffsetSpec so


stackPropertySpecOffset , stackPropertySpecSort:: StackProperty -> Maybe VLSpec
stackPropertySpecOffset (StOffset op) = Just (stackOffsetSpec op)
stackPropertySpecOffset _ = Nothing

stackPropertySpecSort (StSort sfs) = Just (toJSON (map sortFieldSpec sfs))
stackPropertySpecSort _ = Nothing


{-|

Apply a stack transform for positioning multiple values. This is an alternative
to specifying stacking directly when encoding position.

@
'transform'
    . 'aggregate' [ 'opAs' 'Count' \"\" \"count_*\" ] [ \"Origin\", \"Cylinders\" ]
    . 'stack' "count_*"
        []
        \"stack_count_Origin1\"
        \"stack_count_Origin2\"
        [ 'StOffset' 'StNormalize', 'StSort' [ 'WAscending' \"Origin\" ] ]
    . 'window'
        [ ( [ 'WAggregateOp' 'Min', 'WField' \"stack_count_Origin1\" ], \"x\" )
        , ( [ 'WAggregateOp' 'Max', 'WField' \"stack_count_Origin2\" ], \"x2\" )
        ]
        [ 'WFrame' Nothing Nothing, 'WGroupBy' [ \"Origin\" ] ]
    . 'stack' \"count_*\"
        [ \"Origin\" ]
        \"y\"
        \"y2\"
        [ 'StOffset' 'StNormalize', 'StSort' [ 'WAscending' \"Cylinders\" ] ]
@

@since 0.4.0.0

-}

stack ::
  T.Text
  -- ^ The field to be stacked.
  -> [T.Text]
  -- ^ The fields to group by.
  -> T.Text
  -- ^ The output field name (start).
  -> T.Text
  -- ^ The output field name (end).
  -> [StackProperty]
  -- ^ Offset and sort properties.
  -> BuildLabelledSpecs
stack f grp start end sProps ols =
  let ags = [ toJSON f, toJSON grp, toJSON start, toJSON end
            , toSpec (mapMaybe stackPropertySpecOffset sProps)
            , toSpec (mapMaybe stackPropertySpecSort sProps)
            ]

      toSpec [x] = x
      toSpec _ = A.Null

  in ("stack", toJSON ags) : ols


{-|

Individual scale property. These are used to customise an individual scale
transformation. To customise all scales use 'configure' and supply relevant
'ScaleConfig' values. For more details see the
<https://vega.github.io/vega-lite/docs/scale.html Vega-Lite documentation>.

There are two utility routines for constructing a list of scale
properties: 'categoricalDomainMap' and 'domainRangeMap'.

The @SReverse@ constructor was removed in version @0.4.0.0@, as it
represented a Vega, rather than Vega-Lite, property. The order of
a scale can be changed with the 'PSort' constructor.
-}

-- based on schema 3.3.0 #/definitions/Scale

data ScaleProperty
    = SType Scale
      -- ^ Type of scaling to apply.
    | SAlign Double
      -- ^ Alignemnt of the steps within the scale range. A value of
      --   @0@ shifts the bands to an axis, @1@ away from the axis,
      --   and @0.5@ is centered within the range.
      --
      --   The input is clamped so that values less than 0 are mapped
      --   to 0 and greater than 1 to 1.
      --
      --   @since 0.4.0.0
    | SBase Double
      -- ^ The base to use for log scaling ('ScLog').
      --
      --   Default is @10@.
      --
      --   @since 0.4.0.0
    | SBins [Double]
      -- ^ An array of bin boundaries over the scale domain. If give, axes and legends will use
      --   these boundaries to inform the choice of tick marks and text labels.
      --
      --   @since 0.4.0.0
    | SClamp Bool
      -- ^ Should values outside the data domain be clamped (to the minimum or
      --   maximum value)?
    | SConstant Double
      -- ^ The desired slope of the 'ScSymLog' function at zero.
      --
      --   The default is @1@.
      --
      --   @since 0.4.0.0
    | SDomain ScaleDomain
      -- ^ Custom scaling domain.
    | SExponent Double
      -- ^ The exponent to use for power scaling ('ScPow').
      --
      --   @since 0.4.0.0
    | SInterpolate CInterpolate
      -- ^ Interpolation method for scaling range values.
    | SNice ScaleNice
      -- ^ \"Nice\" minimum and maximum values in a scaling (e.g. multiples of 10).
    | SPadding Double
      -- ^ Padding in pixels to apply to a scaling.
    | SPaddingInner Double
      -- ^ Inner padding to apply to a band scaling.
    | SPaddingOuter Double
      -- ^ Outer padding to apply to a band scaling.
    | SRange ScaleRange
      -- ^ Range of a scaling. The type of range depends on the encoding channel.
    | SRangeStep (Maybe Double)
      -- ^ Distance between the starts of adjacent bands in a band scaling. If
      --   @Nothing@, the distance is determined automatically.
    | SRound Bool
      -- ^ Are numeric values in a scaling are rounded to integers?
      --
      --   The default is @False@.
    | SScheme T.Text [Double]
      -- ^  Color scheme used by a color scaling. The first parameter is the
      --    name of the scheme (e.g. \"viridis\") and the second an optional
      --    specification, which can contain 1, 2, or 3 numbers:
      --
      --      - the number of colors to use (list of one number);
      --      - the extent of the color range to use (list of two numbers between 0 and 1);
      --      - the number of colors and extent (three numbers, first is the number of colors).
      --
      --    The number of colors was broken prior to @0.4.0.0@ and the option to
      --    define both the count and extent was added in @0.4.0.0@.
    | SZero Bool
      -- ^ Should a numeric scaling be forced to include a zero value?
      --
      --   Not all scales support @SZero@ and the default depends on the type of
      --   channel.

scaleProperty :: ScaleProperty -> LabelledSpec
scaleProperty (SType sType) = "type" .= scaleLabel sType
scaleProperty (SAlign c) = "align" .= clamped 0 1 c
scaleProperty (SBase x) = "base" .= x
scaleProperty (SBins xs) = "bins" .= xs
scaleProperty (SClamp b) = "clamp" .= b
scaleProperty (SConstant x) = "constant" .= x
scaleProperty (SDomain sdType) = "domain" .= scaleDomainSpec sdType
scaleProperty (SExponent x) = "exponent" .= x
scaleProperty (SInterpolate interp) = "interpolate" .= cInterpolateSpec interp
scaleProperty (SNice ni) = "nice" .= scaleNiceSpec ni
scaleProperty (SPadding x) = "padding" .= x
scaleProperty (SPaddingInner x) = "paddingInner" .= x
scaleProperty (SPaddingOuter x) = "paddingOuter" .= x
scaleProperty (SRange (RNumbers xs)) = "range" .= xs
scaleProperty (SRange (RStrings ss)) = "range" .= ss
scaleProperty (SRange (RName s)) = "range" .= s
scaleProperty (SRangeStep numOrNull) = "rangeStep" .= maybe A.Null toJSON numOrNull
scaleProperty (SRound b) = "round" .= b
scaleProperty (SScheme nme extent) = schemeProperty nme extent
scaleProperty (SZero b) = "zero" .= b


-- TODO: there should probably be a more-structured way to specify this
--
-- based on schema 3.3.0 #/definitions/SchemeParams

schemeProperty :: T.Text -> [Double] -> LabelledSpec
schemeProperty nme [n] = "scheme" .= object ["name" .= nme, "count" .= n]
schemeProperty nme [mn, mx] = "scheme" .= object ["name" .= nme, "extent" .= [mn, mx]]
schemeProperty nme [n, mn, mx] = "scheme" .= object ["name" .= nme, "count" .= n, "extent" .= [mn, mx]]
schemeProperty nme _ = "scheme" .= nme


-- | Used to indicate the type of scale transformation to apply.
--
--   The @0.4.0.0@ release removed the @ScSequential@ constructor, as
--   'ScLinear' should be used instead.

data Scale
    = ScLinear
      -- ^ A linear scale.
    | ScPow
      -- ^ A power scale. The exponent to use for scaling is specified with
      --   'SExponent'.
    | ScSqrt
      -- ^ A square-root scale.
    | ScLog
      -- ^ A log scale. Defaults to log of base 10, but can be customised with
      --   'SBase'.
    | ScSymLog
      -- ^ A [symmetrical log (PDF link)](https://www.researchgate.net/profile/John_Webber4/publication/233967063_A_bi-symmetric_log_transformation_for_wide-range_data/links/0fcfd50d791c85082e000000.pdf)
      --   scale. Similar to a log scale but supports zero and negative values. The slope
      --   of the function at zero can be set with 'SConstant'.
      --
      --   @since 0.4.0.0
    | ScTime
      -- ^ A temporal scale.
    | ScUtc
      -- ^ A temporal scale, in UTC.
    | ScOrdinal
      -- ^ An ordinal scale.
    | ScBand
      -- ^ A band scale.
    | ScPoint
      -- ^ A point scale.
    | ScBinLinear
      -- ^ A linear band scale.
    | ScBinOrdinal
      -- ^ An ordinal band scale.
    | ScQuantile
      -- ^ A quantile scale.
      --
      --   @since 0.4.0.0
    | ScQuantize
      -- ^ A quantizing scale.
      --
      --   @since 0.4.0.0
    | ScThreshold
      -- ^ A threshold scale.
      --
      --   @since 0.4.0.0


scaleLabel :: Scale -> T.Text
scaleLabel ScLinear = "linear"
scaleLabel ScPow = "pow"
scaleLabel ScSqrt = "sqrt"
scaleLabel ScLog = "log"
scaleLabel ScSymLog = "symlog"
scaleLabel ScTime = "time"
scaleLabel ScUtc = "utc"
scaleLabel ScOrdinal = "ordinal"
scaleLabel ScBand = "band"
scaleLabel ScPoint = "point"
scaleLabel ScBinLinear = "bin-linear"
scaleLabel ScBinOrdinal = "bin-ordinal"
scaleLabel ScQuantile = "quantile"
scaleLabel ScQuantize = "quantize"
scaleLabel ScThreshold = "threshold"


{-|

Describes the scale domain (type of data in scale). For full details see the
<https://vega.github.io/vega-lite/docs/scale.html#domain Vega-Lite documentation>.
-}

data ScaleDomain
    = DNumbers [Double]
      -- ^ Numeric values that define a scale domain.
    | DStrings [T.Text]
      -- ^ String values that define a scale domain.
    | DDateTimes [[DateTime]]
      -- ^ Date-time values that define a scale domain.
    | DSelection T.Text
      -- ^ Scale domain based on a named interactive selection.
    | Unaggregated
    -- ^ Specify an unaggregated scale domain (type of data in scale).

scaleDomainSpec :: ScaleDomain -> VLSpec
scaleDomainSpec (DNumbers nums) = toJSON (map toJSON nums)
scaleDomainSpec (DDateTimes dts) = toJSON (map (object . map dateTimeProperty) dts)
scaleDomainSpec (DStrings cats) = toJSON (map toJSON cats)
scaleDomainSpec (DSelection selName) = object ["selection" .= selName]
scaleDomainSpec Unaggregated = "unaggregated"


{-|

Describes the way a scale can be rounded to \"nice\" numbers. For full details see the
<https://vega.github.io/vega-lite/docs/scale.html#continuous Vega-Lite documentation>.
-}
data ScaleNice
    = NMillisecond
      -- ^ Nice time intervals that try to align with rounded milliseconds.
    | NSecond
      -- ^ Nice time intervals that try to align with whole or rounded seconds.
    | NMinute
      -- ^ Nice time intervals that try to align with whole or rounded minutes.
    | NHour
      -- ^ Nice time intervals that try to align with whole or rounded hours.
    | NDay
      -- ^ Nice time intervals that try to align with whole or rounded days.
    | NWeek
    -- ^ Nice time intervals that try to align with whole or rounded weeks.
    | NMonth
      -- ^ Nice time intervals that try to align with whole or rounded months.
    | NYear
      -- ^ Nice time intervals that try to align with whole or rounded years.
    | NInterval TimeUnit Int
      -- ^ \"Nice\" temporal interval values when scaling.
    | IsNice Bool
      -- ^ Enable or disable nice scaling.
    | NTickCount Int
      -- ^ Desired number of tick marks in a \"nice\" scaling.


scaleNiceSpec :: ScaleNice -> VLSpec
scaleNiceSpec NMillisecond = fromT "millisecond"
scaleNiceSpec NSecond = fromT "second"
scaleNiceSpec NMinute = fromT "minute"
scaleNiceSpec NHour = fromT "hour"
scaleNiceSpec NDay = fromT "day"
scaleNiceSpec NWeek = fromT "week"
scaleNiceSpec NMonth = fromT "month"
scaleNiceSpec NYear = fromT "year"
scaleNiceSpec (NInterval tu step) =
  object ["interval" .= timeUnitLabel tu, "step" .= step]
scaleNiceSpec (IsNice b) = toJSON b
scaleNiceSpec (NTickCount n) = toJSON n


{-|

Describes a scale range of scale output values. For full details see the
<https://vega.github.io/vega-lite/docs/scale.html#range Vega-Lite documentation>.

For color scales you can also specify a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme)
instead of range.

Any directly specified range for @x@ and @y@ channels will be ignored. Range can be customized
via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html)
('width' and 'height') or via range steps and paddings properties (e.g. 'SCRangeStep')
for band and point scales.

-}

data ScaleRange
    = RNumbers [Double]
      -- ^ For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous),
      --   a two-element array indicating minimum and maximum values, or an array with more than
      --   two entries for specifying a
      --   [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
    | RStrings [T.Text]
      -- ^ Text scale range for discrete scales.
    | RName T.Text
      -- ^ Name of a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config)
      --   (e.g. \"symbol\" or \"diverging\").

{-|

Indicates the type of color interpolation to apply, when mapping a data field
onto a color scale.

For details see the
<https://vega.github.io/vega-lite/docs/scale.html#continuous Vega-Lite documentation>.

-}
data CInterpolate
    = CubeHelix Double
      -- ^ Cube helix color interpolation for continuous color scales using the given
      --   gamma value (anchored at 1).
    | CubeHelixLong Double
      -- ^ Long-path cube helix color interpolation for continuous color scales using
      --   the given gamma value (anchored at 1).
    | Hcl
      -- ^ HCL color interpolation for continuous color scales.
    | HclLong
      -- ^ HCL color interpolation in polar coordinate space for continuous color scales.
    | Hsl
      -- ^ HSL color interpolation for continuous color scales.
    | HslLong
      -- ^ HSL color interpolation in polar coordinate space for continuous color scales.
    | Lab
      -- ^ Lab color interpolation for continuous color scales.
    | Rgb Double
      -- ^ RGB color interpolation for continuous color scales using the given gamma
      --   value (anchored at 1).


-- Need to tie down some types as things are too polymorphic,
-- particularly in the presence of OverloadedStrings.
--
pairT :: T.Text -> T.Text -> (T.Text, Value)
pairT a b = a .= b


cInterpolateSpec :: CInterpolate -> VLSpec
cInterpolateSpec (Rgb gamma) = object [pairT "type" "rgb", "gamma" .= gamma]
cInterpolateSpec Hsl = object [pairT "type" "hsl"]
cInterpolateSpec HslLong = object [pairT "type" "hsl-long"]
cInterpolateSpec Lab = object [pairT "type" "lab"]
cInterpolateSpec Hcl = object [pairT "type" "hcl"]
cInterpolateSpec HclLong = object [pairT "type" "hcl-long"]
cInterpolateSpec (CubeHelix gamma) = object [pairT "type" "cubehelix", "gamma" .= gamma]
cInterpolateSpec (CubeHelixLong gamma) = object [pairT "type" "cubehelix-long", "gamma" .= gamma]


{-|

Allow type of sorting to be customised. For details see the
<https://vega.github.io/vega-lite/docs/sort.html Vega-Lite documentation>.

The constructors have been changed in version @0.4.0.0@, with
@Op@, @ByField@, and @ByRepeat@ removed, and their functionality
replaced with 'ByRepeatOp', 'ByFieldOp', and 'ByChannel'.

-}
data SortProperty
    = Ascending
      -- ^ Sorting is from low to high.
    | Descending
      -- ^ Sorting is from high to low.
    | CustomSort DataValues
      -- ^ Custom sort order listing data values explicitly.
      --
      --   @since 0.4.0.0
    | ByRepeatOp Arrangement Operation
      -- ^ Sort by the aggregated summaries of the given fields
      --   (referenced by a repeat iterator) using an aggregation
      --   operation.
      --
      --   @since 0.4.0.0
    | ByFieldOp T.Text Operation
      -- ^ Sort by the aggregated summary of a field using an aggregation
      --   operation. The following example sorts the categorical data field
      --   @variety@ by the mean age of the data in each variety category:
      --
      -- @
      -- 'position' 'Y'
      --   [ 'PName' "variety"
      --   , 'PmType' 'Ordinal'
      --   , 'PSort' [ ByFieldOp "age" 'Mean', 'Descending' ]
      --   ]
      -- @
      --
      --   @since 0.4.0.0
    | ByChannel Channel
      -- ^ Sort by another channel.
      --
      -- @
      -- 'position' 'Y'
      --  [ 'PName' "age"
      --  , 'PmType' 'Ordinal'
      --  , 'PSort' [ ByChannel 'ChX' ]
      --  ]
      -- @
      --
      --   @since 0.4.0.0


sortProperty :: SortProperty -> [LabelledSpec]
sortProperty Ascending = [order_ "ascending"]
sortProperty Descending = [order_ "descending"]
sortProperty (ByChannel ch) = ["encoding" .= channelLabel ch]
sortProperty (ByFieldOp field op) = [field_ field, op_ op]
sortProperty (ByRepeatOp arr op) = ["field" .= object [repeat_ arr], op_ op]
sortProperty (CustomSort _) = []


sortPropertySpec :: [SortProperty] -> VLSpec
sortPropertySpec [] = A.Null
sortPropertySpec [Ascending] = fromT "ascending"
sortPropertySpec [Descending] = fromT "descending"
sortPropertySpec [CustomSort dvs] = toJSON (dataValuesSpecs dvs)
sortPropertySpec sps = object (concatMap sortProperty sps)


-- | Position channel properties used for creating a position channel encoding.

data PositionChannel
    = PName T.Text
      -- ^ Name of the field used for encoding with a position channel.
    | PHeight
      -- ^ Set the position to the height of the enclosing data space. Useful for placing
      --   a mark relative to the bottom edge of a view.
      --
      --   @since 0.4.0.0
    | PWidth
      -- ^ Set the position to the width of the enclosing data space. Useful for justifying
      --   a mark to the right hand edge of a view. e.g. to position a mark at the right of
      --   the data rectangle:
      --
      -- @
      -- enc =
      --   'encoding'
      --      . 'position' 'X' [ PWidth ]
      -- @
      --
      --   @since 0.4.0.0
    | PNumber Double
      -- ^ Set a position to an arbitrary value. Useful for placing items at the top of
      --   a plot area (@PNumber 0@) or a fixed number of pixels from the top.
      --   See also 'PHeight' and 'PWidth'.
      --
      --   @since 0.4.0.0
    | PRepeat Arrangement
      -- ^ Reference in a position channel to a field name generated by 'repeatFlow'
      --   or 'repeat'. The parameter identifies whether reference is being made to
      --   fields that are to be arranged in columns, in rows, or a with a flow layout.
      --
      --   For example:
      --
      -- @
      -- enc =
      --   'encoding'
      --      . 'position' 'X' [ PRepeat 'Flow', 'PmType' 'Quantitative' ]
      --
      -- spec =
      --    'asSpec' [ dataVals [], 'mark' 'Tick' [], enc [] ]
      --
      -- 'toVegaLite'
      --    [ 'repeatFlow' [ \"Horsepower\", \"Miles_per_Gallon\", \"Acceleration\"]
      --    , 'specification' spec
      --    ]
      -- @
    | PmType Measurement
      -- ^ Level of measurement when encoding with a position channel.
    | PBin [BinProperty]
      -- ^ Discretize numeric values into bins when encoding with a
      --   position channel.
      --
      --   For example, to encode a frequency histogram with bins every 5 units:
      --
      --   @
      --   enc = 'encoding'
      --           . 'position' 'X' [ 'PName' \"x\"
      --                        , 'PmType' 'Ordinal'
      --                        , 'PBin' ['Step' 5]
      --                        ]
      --           . 'position' 'Y' [ 'PmType' 'Quantitative'
      --                        , 'PAggregate' 'Count'
      --                        ]
      --   @
    | PBinned
      -- ^ Indicate that the data encoded with position is already binned.
      --
      --   @since 0.4.0.0
    | PTimeUnit TimeUnit
      -- ^ Form of time unit aggregation of field values when encoding with a position channel.
    | PTitle T.Text
      -- ^ Title of a field when encoding with a position channel.
      --
      --   @since 0.4.0.0
    | PNoTitle
      -- ^ Draw no title.
      --
      -- @since 0.4.0.0
    | PAggregate Operation
      -- ^ Compute some aggregate summary statistics for a field to be encoded
      --   with a position channel.
      --
      --   @
      --   enc = 'encoding'
      --           . 'position' 'X' [ 'PName' \"role\", 'PmType' 'Ordinal' ]
      --           . 'position' 'Y' [ 'PName' \"salary\"
      --                        , 'PmType' 'Quantitative'
      --                        , 'PAggregate' 'Mean'
      --                        ]
      --   @
    | PScale [ScaleProperty]
      -- ^ Scaling applied to a field when encoding with a position channel.
      --   The scale will transform a field's value into a position along
      --   one axis.
      --
      --   For example, the following will scale the bars positioned along
      --   a horizontal axis to have an inner spacing of 50% (0.5) of the
      --   total space allocated to each bar:
      --
      --   @
      --   enc = 'encoding'
      --           . 'position' 'X' [ 'PName' \"ageGroup\"
      --                        , 'PmType' 'Nominal'
      --                        , 'PScale' ['SPaddingInner' 0.5]
      --                        ]
      --   @
    | PAxis [AxisProperty]
      -- ^ Axis properties used when encoding with a position channel. For no axis,
      --   provide an empty list.
    | PSort [SortProperty]
      -- ^ Sort order for field when encoding with a position channel.
    | PStack StackOffset
      -- ^ Type of stacking offset for the field when encoding with a
      --   position channel.
      --
      --   For example, stacking areas away from a centreline can be used
      --   to create a
      --   [streamgraph](https://vega.github.io/vega-lite/examples/stacked_area_stream.html):
      --
      --   @
      --   enc = 'encoding'
      --           . 'position' 'X' ['PName' \"week\", 'PmType' 'Ordinal']
      --           . 'position' 'Y' [ 'PName' \"takings\"
      --                        , 'PmType' 'Quantitative'
      --                        , 'PStack' 'StCenter'
      --                        ]
      --           . 'color' ['MName' \"shop\", 'MmType' 'Nominal']
      --   @
      --
      --   Changed from @StackProperty@ in version @0.4.0.0@.
    | PImpute [ImputeProperty]
      -- ^ Set the imputation rules for a position channel. See the
      --   [Vega-Lite impute documentation](https://vega.github.io/vega-lite/docs/impute.html).
      --
      --   @since 0.4.0.0


positionChannelProperty :: PositionChannel -> LabelledSpec
positionChannelProperty (PName s) = field_ s
positionChannelProperty (PmType m) = mtype_ m
positionChannelProperty (PBin b) = bin b
positionChannelProperty PBinned = binned_
positionChannelProperty (PAggregate op) = aggregate_ op
positionChannelProperty (PTimeUnit tu) = timeUnit_ tu
positionChannelProperty (PTitle s) = "title" .= s
positionChannelProperty PNoTitle = "title" .= A.Null
positionChannelProperty (PSort ops) = sort_ ops
positionChannelProperty (PScale sps) = scaleProp_ sps
positionChannelProperty (PAxis aps) =
  let js = if null aps
           then A.Null
           else object (map axisProperty aps)
  in "axis" .= js
positionChannelProperty (PStack so) = stackOffset so
positionChannelProperty (PRepeat arr) = "field" .= object [repeat_ arr]
positionChannelProperty PHeight = value_ "height"
positionChannelProperty PWidth = value_ "width"
positionChannelProperty (PNumber x) = "value" .= x
positionChannelProperty (PImpute ips) = impute_ ips


measurementLabel :: Measurement -> T.Text
measurementLabel Nominal = "nominal"
measurementLabel Ordinal = "ordinal"
measurementLabel Quantitative = "quantitative"
measurementLabel Temporal = "temporal"
measurementLabel GeoFeature = "geojson"


positionLabel :: Position -> T.Text
positionLabel X = "x"
positionLabel Y = "y"
positionLabel X2 = "x2"
positionLabel Y2 = "y2"
positionLabel XError     = "xError"
positionLabel YError     = "yError"
positionLabel XError2    = "xError2"
positionLabel YError2    = "yError2"
positionLabel Longitude = "longitude"
positionLabel Latitude = "latitude"
positionLabel Longitude2 = "longitude2"
positionLabel Latitude2 = "latitude2"


{-|

Set the background color of the visualization. Should be specified with a CSS
string such as @\"#ffe\"@ or @\"rgb(200,20,150)\"@. If not specified the background will
be transparent.

@
'toVegaLite'
    [ 'background' "rgb(251,247,238)"
    , 'dataFromUrl' "data/population.json" []
    , 'mark' 'Bar' []
    , enc []
    ]
@
-}
background :: T.Text -> PropertySpec
background colour = (VLBackground, toJSON colour)


{-|

Provides an optional description to be associated with the visualization.

@
'toVegaLite'
    [ 'description' "Population change of key regions since 1900"
    , 'dataFromUrl' "data/population.json" []
    , 'mark' 'Bar' []
    , enc []
    ]
@
-}
description :: T.Text -> PropertySpec
description s = (VLDescription, toJSON s)


-- | Optional metadata.
--
--   @since 0.4.0.0

usermetadata ::
  A.Object
  -- ^ The metadata is passed around but ignored by VegaLite.
  -> PropertySpec
usermetadata o = (VLUserMetadata, A.Object o)


{-|

Provide an optional title to be displayed in the visualization.

@
'toVegaLite'
    [ 'title' "Population Growth" ['TColor' \"orange\"]
    , 'dataFromUrl' \"data/population.json\" []
    , 'mark' 'Bar' []
    , 'encoding' ...
    ]
@

Prior to @0.4.0.0@ there was no way to set the title options
(other than using 'configuration' with 'TitleStyle').

-}
title ::
  T.Text
  -> [TitleConfig]
  -- ^ Configure the appearance of the title.
  --
  --   @since 0.4.0.0
  -> PropertySpec
title s [] = (VLTitle, toJSON s)
title s topts = (VLTitle,
                 object ("text" .= s : map titleConfigSpec topts))


{-|

Axis customisation properties. These are used for customising individual axes.
To configure all axes, use 'AxisConfig' with a 'configuration' instead. See the
<https://vega.github.io/vega-lite/docs/axis.html#axis-properties Vega-Lite documentation>
for more details.

The @AxTitleMaxLength@ constructor was removed in release @0.4.0.0@. The
'AxTitleLimit' constructor should be used instead.

-}
{-# DEPRECATED AxDates "Please change AxDates to AxValues" #-}
data AxisProperty
    = AxBandPosition Double
      -- ^ An interpolation fraction indicating where, for @band@ scales, axis ticks should
      --   be position. A value of @0@ places ticks at the left-edge of the band, @0.5@ in
      --   the middle, and @1@ at the right edge.
      --
      --   @since 0.4.0.0
    | AxDomain Bool
      -- ^ Should the axis domain (the baseline) be displayed?
    | AxDomainColor Color
      -- ^ The axis domain color.
      --
      --   @since 0.4.0.0
    | AxDomainDash [Double]
      -- ^ The dash style of the domain (alternating stroke, space lengths
      --   in pixels).
      --
      --   @since 0.4.0.0
    | AxDomainDashOffset Double
      -- ^ The pixel offset at which to start drawing the domain dash array.
      --
      --   @since 0.4.0.0
    | AxDomainOpacity Opacity
      -- ^ The axis domain opacity.
      --
      --   @since 0.4.0.0
    | AxDomainWidth Double
      -- ^ The width of the axis domain.
      --
      --   @since 0.4.0.0
    | AxFormat T.Text
      -- ^ [Formatting pattern](https://vega.github.io/vega-lite/docs/format.html) for
      --   axis values. To distinguish between formatting as numeric values
      --   and data/time values, additionally use 'AxFormatAsNum' or 'AxFormatAsTemporal'.
    | AxFormatAsNum
      -- ^ Facet headers should be formatted as numbers. Use a
      --   [d3 numeric format string](https://github.com/d3/d3-format#locale_format)
      --   with 'AxFormat'.
      --
      -- @since 0.4.0.0
    | AxFormatAsTemporal
      -- ^ Facet headers should be formatted as dates or times. Use a
      --   [d3 date/time format string](https://github.com/d3/d3-time-format#locale_format)
      --   with 'AxFormat'.
      --
      -- @since 0.4.0.0
    | AxGrid Bool
      -- ^ Should an axis grid be displayed?
    | AxGridColor Color
      -- ^ The color for the grid.
      --
      --   @since 0.4.0.0
    | AxGridDash [Double]
      -- ^ The dash style of the grid (alternating stroke, space lengths
      --   in pixels).
      --
      --   @since 0.4.0.0
    | AxGridDashOffset Double
      -- ^ The pixel offset at which to start drawing the grid dash array.
      --
      --   @since 0.4.0.0
    | AxGridOpacity Opacity
      -- ^ The opacity of the grid.
      --
      --   @since 0.4.0.0
    | AxGridWidth Double
      -- ^ The width of the grid lines.
      --
      --   @since 0.4.0.0
    | AxLabels Bool
      -- ^ Should labels be added to an axis?
    | AxLabelAlign HAlign
      -- ^ The horizontal alignment for labels.
      --
      --   @since 0.4.0.0
    | AxLabelAngle Angle
      -- ^ The angle at which to draw labels.
    | AxLabelBaseline VAlign
      -- ^ The vertical alignment for labels.
      --
      --   @since 0.4.0.0
    | AxLabelBound (Maybe Double)  -- XXXXX don't like Maybe Double here
      -- ^ Should labels be hidden if they exceed the axis range? If @Nothing@
      --   then no check is made, otherwise it gives the maximum number of
      --   pixels by which the label bounding box can extend beyond the axis.
      --
      --   @since 0.4.0.0
    | AxLabelColor Color
      -- ^ The label color.
      --
      --   @since 0.4.0.0
    | AxLabelFlush (Maybe Double)   -- XXXXX as with labelbound
      -- ^ The label alignment at the start or end of the axis. If
      --   @Nothing@ then no adjustment is made. A value of @Just 1@ means that the
      --   labels will be left- and right- aligned for the first and last
      --   label (horizontal axis), or bottom and top text baselines are
      --   aligned for a vertical axis. Other numeric values indicate additonal
      --   space added, in pixels, which can someties help the labels better visually
      --   group with the corresponding tick marks.
      --
      --   @since 0.4.0.0
    | AxLabelFlushOffset Double
      -- ^ The number of pixels to offset flush-adjusted labels.
      --
      --   @since 0.4.0.0
    | AxLabelFont T.Text
      -- ^ The font for the label.
      --
      --   @since 0.4.0.0
    | AxLabelFontSize Double
      -- ^ The font size of the label.
      --
      --   @since 0.4.0.0
    | AxLabelFontStyle T.Text
      -- ^ The font style of the label.
      --
      --   @since 0.4.0.0
    | AxLabelFontWeight FontWeight
      -- ^ The font weight of the label.
      --
      --   @since 0.4.0.0
    | AxLabelLimit Double
      -- ^ The maximum width of a label, in pixels.
      --
      --   @since 0.4.0.0
    | AxLabelOpacity Opacity
      -- ^ The opacity of the label.
      --
      --   @since 0.4.0.0
    | AxLabelOverlap OverlapStrategy
      -- ^ How should overlapping labels be displayed?
    | AxLabelPadding Double
      -- ^ The padding, in pixels, between the label and the axis.
    | AxLabelSeparation Double
      -- ^ The minimum separation, in pixels, between label bounding boxes
      --   for them to be considered non-overlapping. This is ignored if
      --   the 'AxLabelOverlap' strategy is 'ONone'.
      --
      --   @since 0.4.0.0
    | AxMaxExtent Double
      -- ^ The maximum extent, in pixels, that axis ticks and labels should use.
      --   This determines a maxmium offset value for axis titles.
    | AxMinExtent Double
      -- ^ The minimum extent, in pixels, that axis ticks and labels should use.
      --   This determines a minmium offset value for axis titles.
    | AxOffset Double
      -- ^ The offset, in pixels, between the axis and the edge of the
      --   enclosing group or data rectangle.
    | AxOrient Side
      -- ^ The orientation of the axis.
    | AxPosition Double
      -- ^ The anchor position of the axis in pixels.
    | AxTicks Bool
      -- ^ Should tick marks be drawn on an axis?
    | AxTickColor Color
      -- ^ The color of the ticks.
      --
      --   @since 0.4.0.0
    | AxTickCount Int
      -- ^ The desired number of ticks for axes visualizing quantitative scales.
      --   This is a hint to the system, and the actual number used will be
      --   adjusted to be \"nice\" (multiples of 2, 5, or 10) and lie within the
      --   underlying scale's range.
    | AxTickDash [Double]
      -- ^ The dash style of the ticks (alternating stroke, space lengths
      --   in pixels).
      --
      --   @since 0.4.0.0
    | AxTickDashOffset Double
      -- ^ The pixel offset at which to start drawing the tick dash array.
      --
      --   @since 0.4.0.0
    | AxTickExtra Bool
      -- ^ Should an extra axis tick mark be added for the initial position of
      --   the axis?
      --
      --   @since 0.4.0.0
    | AxTickMinStep Double
      -- ^ The minimum desired step between axis ticks, in terms of the scale
      --   domain values.
      --
      --   @since 0.4.0.0
    | AxTickOffset Double
      -- ^ The position offset, in pixels, to apply to ticks, labels, and grid lines.
      --
      --   @since 0.4.0.0
    | AxTickOpacity Opacity
      -- ^ The opacity of the ticks.
      --
      --   @since 0.4.0.0
    | AxTickRound Bool
      -- ^ Should pixel position values be rounded to the nearest integer?
      --
      --   @since 0.4.0.0
    | AxTickSize Double
      -- ^ The size of the tick marks in pixels.
    | AxTickWidth Double
      -- ^ The width of the tick marks in pixels.
      --
      --   @since 0.4.0.0
    | AxTitle T.Text
      -- ^ The axis title.
    | AxNoTitle
      -- ^ Draw no title for the axis.
      --
      --   @since 0.4.0.0
    | AxTitleAlign HAlign
      -- ^ The horizontal alignment of the axis title.
    | AxTitleAnchor APosition
      -- ^ The text anchor position for placing axis titles.
      --
      --   @since 0.4.0.0
    | AxTitleAngle Angle
      -- ^ The angle of the axis title.
    | AxTitleBaseline VAlign
      -- ^ The vertical alignment of the axis title.
      --
      --   @since 0.4.0.0
    | AxTitleColor Color
      -- ^ The color of the axis title.
      --
      --   @since 0.4.0.0
    | AxTitleFont T.Text
      -- ^ The font for the axis title.
      --
      --   @since 0.4.0.0
    | AxTitleFontSize Double
      -- ^ The font size of the axis title.
      --
      --   @since 0.4.0.0
    | AxTitleFontStyle T.Text
      -- ^ The font style of the axis title.
      --
      --   @since 0.4.0.0
    | AxTitleFontWeight FontWeight
      -- ^ The font weight of the axis title.
      --
      --   @since 0.4.0.0
    | AxTitleLimit Double
      -- ^ The maximum allowed width of the axis title, in pixels.
      --
      --   @since 0.4.0.0
    | AxTitleOpacity Opacity
      -- ^ The opacity of the axis title.
      --
      --   @since 0.4.0.0
    | AxTitlePadding Double
      -- ^ The padding, in pixels, between title and axis.
    | AxTitleX Double
      -- ^ The X coordinate of the axis title, relative to the axis group.
      --
      --   @since 0.4.0.0
    | AxTitleY Double
      -- ^ The Y coordinate of the axis title, relative to the axis group.
      --
      --   @since 0.4.0.0
    | AxValues DataValues
      -- ^ Set the explicit tick, grid, and label values along an axis.
      --
      --   The following three examples are for an axis displaying a
      --   quantitative, categorical, and temporal field respectively.
      --
      --   @
      --   'PAxis' ['AxValues' ('Numbers' [2, 3, 5, 7, 11, 13, 17])]
      --   'PAxis' ['AxValues' ('Strings' ["cats", "dogs", "elephants"])]
      --   'PAxis' ['AxValues' ('DateTimes' [ ['DTYear' 2019, 'DTMonth' 'Mar', 'DTDate' 31]
      --                              , ['DTYear' 2019, 'DTMonth' 'Jun', 'DTDate' 30]
      --                              , ['DTYear' 2019, 'DTMonth' 'Sep', 'DTDate' 30]
      --                              ])]
      --   @
      --
      --   Changed in @0.4.0.0@ to take 'DataValues' rather than @[Double]@.
    | AxDates [[DateTime]]
      -- ^ The dates or times to appear along the axis.
      --
      --   As of version @0.4.0.0@, this is deprecated. The 'AxValues'
      --   constructor should be used instead.
    | AxZIndex ZIndex
      -- ^ The z-index of the axis, relative to the chart marks.


axisProperty :: AxisProperty -> LabelledSpec
axisProperty (AxBandPosition x) = "bandPosition" .= x
axisProperty (AxDomain b) = "domain" .= b
axisProperty (AxDomainColor s) = "domainColor" .= s
axisProperty (AxDomainDash ds) = "domainDash" .= ds
axisProperty (AxDomainDashOffset x) = "domainDashOffset" .= x
axisProperty (AxDomainOpacity x) = "domainOpacity" .= x
axisProperty (AxDomainWidth x) = "domainWidth" .= x
axisProperty (AxFormat fmt) = "format" .= fmt
axisProperty AxFormatAsNum = "formatType" .= fromT "number"
axisProperty AxFormatAsTemporal = "formatType" .= fromT "time"
axisProperty (AxGrid b) = "grid" .= b
axisProperty (AxGridColor s) = "gridColor" .= s
axisProperty (AxGridDash ds) = "gridDash" .= ds
axisProperty (AxGridDashOffset x) = "gridDashOffset" .= x
axisProperty (AxGridOpacity x) = "gridOpacity" .= x
axisProperty (AxGridWidth x) = "gridWidth" .= x
axisProperty (AxLabels b) = "labels" .= b
axisProperty (AxLabelAlign ha) = "labelAlign" .= hAlignLabel ha
axisProperty (AxLabelAngle a) = "labelAngle" .= a
axisProperty (AxLabelBaseline va) = "labelBaseline" .= vAlignLabel va
axisProperty (AxLabelBound mx) = "labelBound" .= mxToValue mx
axisProperty (AxLabelColor s) = "labelColor" .= s
axisProperty (AxLabelFlush mx) = "labelFlush" .= mxToValue mx
axisProperty (AxLabelFlushOffset x) = "labelFlushOffset" .= x
axisProperty (AxLabelFont s) = "labelFont" .= s
axisProperty (AxLabelFontSize x) = "labelFontSize" .= x
axisProperty (AxLabelFontStyle s) = "labelFontStyle" .= s
axisProperty (AxLabelFontWeight fw) = "labelFontWeight" .= fontWeightSpec fw
axisProperty (AxLabelLimit x) = "labelLimit" .= x
axisProperty (AxLabelOpacity x) = "labelOpacity" .= x
axisProperty (AxLabelOverlap s) = "labelOverlap" .= overlapStrategyLabel s
axisProperty (AxLabelPadding x) = "labelPadding" .= x
axisProperty (AxLabelSeparation x) = "labelSeparation" .= x
axisProperty (AxMaxExtent n) = "maxExtent" .= n
axisProperty (AxMinExtent n) = "minExtent" .= n
axisProperty (AxOffset n) = "offset" .= n
axisProperty (AxOrient side) = "orient" .= sideLabel side
axisProperty (AxPosition n) = "position" .= n
axisProperty (AxTicks b) = "ticks" .= b
axisProperty (AxTickColor s) = "tickColor" .= s
axisProperty (AxTickCount n) = "tickCount" .= n
axisProperty (AxTickDash ds) = "tickDash" .= ds
axisProperty (AxTickDashOffset x) = "tickDashOffset" .= x
axisProperty (AxTickExtra b) = "tickExtra" .= b
axisProperty (AxTickMinStep x) = "tickMinStep" .= x
axisProperty (AxTickOffset x) = "tickOffset" .= x
axisProperty (AxTickOpacity x) = "tickOpacity" .= x
axisProperty (AxTickRound b) = "tickRound" .= b
axisProperty (AxTickSize x) = "tickSize" .= x
axisProperty (AxTickWidth x) = "tickWidth" .= x
axisProperty (AxTitle ttl) = "title" .= ttl
axisProperty AxNoTitle = "title" .= A.Null
axisProperty (AxTitleAlign ha) = "titleAlign" .= hAlignLabel ha
axisProperty (AxTitleAnchor a) = "titleAnchor" .= anchorLabel a
axisProperty (AxTitleAngle x) = "titleAngle" .= x
axisProperty (AxTitleBaseline va) = "titleBaseline" .= vAlignLabel va
axisProperty (AxTitleColor s) = "titleColor" .= s
axisProperty (AxTitleFont s) = "titleFont" .= s
axisProperty (AxTitleFontSize x) = "titleFontSize" .= x
axisProperty (AxTitleFontStyle s) = "titleFontStyle" .= s
axisProperty (AxTitleFontWeight fw) = "titleFontWeight" .= fontWeightSpec fw
axisProperty (AxTitleLimit x) = "titleLimit" .= x
axisProperty (AxTitleOpacity x) = "titleOpacity" .= x
axisProperty (AxTitlePadding pad) = "titlePadding" .= pad
axisProperty (AxTitleX x) = "titleX" .= x
axisProperty (AxTitleY x) = "titleY" .= x
axisProperty (AxValues vals) = "values" .= dataValuesSpecs vals
axisProperty (AxDates dtss) = "values" .= map (object . map dateTimeProperty) dtss
axisProperty (AxZIndex z) = "zindex" .= z


-- | Indicates the horizontal alignment of text such as on an axis or legend.

data HAlign
    = AlignCenter
    | AlignLeft
    | AlignRight


-- | Indicates the vertical alignment of text that may be attached to a mark.

data VAlign
    = AlignTop
    | AlignMiddle
    | AlignBottom


hAlignLabel :: HAlign -> T.Text
hAlignLabel AlignLeft = "left"
hAlignLabel AlignCenter = "center"
hAlignLabel AlignRight = "right"


vAlignLabel :: VAlign -> T.Text
vAlignLabel AlignTop = "top"
vAlignLabel AlignMiddle = "middle"
vAlignLabel AlignBottom = "bottom"


-- | Represents one side of a rectangular space.

data Side
    = STop
    | SBottom
    | SLeft
    | SRight


sideLabel :: Side -> T.Text
sideLabel STop = "top"
sideLabel SBottom = "bottom"
sideLabel SLeft = "left"
sideLabel SRight = "right"


{-|

Type of overlap strategy to be applied when there is not space to show all items
on an axis. See the
<https://vega.github.io/vega-lite/docs/axis.html#labels Vega-Lite documentation>
for more details.
-}

data OverlapStrategy
    = ONone
      -- ^ No overlap strategy to be applied when there is not space to show all items
      --   on an axis.
    | OParity
      -- ^ Give all items equal weight in overlap strategy to be applied when there is
      --   not space to show them all on an axis.
    | OGreedy
      -- ^ Greedy overlap strategy to be applied when there is not space to show all
      --   items on an axis.

overlapStrategyLabel :: OverlapStrategy -> T.Text
overlapStrategyLabel ONone = "false"
overlapStrategyLabel OParity = "parity"
overlapStrategyLabel OGreedy = "greedy"


{-|

Allows a date or time to be represented. This is typically part of a list of
@DateTime@ items to provide a specific point in time. For details see the
<https://vega.github.io/vega-lite/docs/types.html#datetime Vega-Lite documentation>.
-}

data DateTime
    = DTYear Int
    | DTQuarter Int
    | DTMonth MonthName
    | DTDate Int
    | DTDay DayName
    | DTHours Int
    | DTMinutes Int
    | DTSeconds Int
    | DTMilliseconds Int


-- | Identifies the day of the week.

data DayName
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun


-- | Identifies a month of the year.

data MonthName
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


{-|

Describes a unit of time. Useful for encoding and transformations. See the
<https://vega.github.io/vega-lite/docs/timeunit.html Vega-Lite documentation>
for further details.

@
'encoding'
    . 'position' 'X' [ 'PName' "date", 'PmType' 'Temporal', 'PTimeUnit' ('Utc' 'YearMonthDateHours') ]
@
-}

data TimeUnit
    = Year
    | YearQuarter
    | YearQuarterMonth
    | YearMonth
    | YearMonthDate
    | YearMonthDateHours
    | YearMonthDateHoursMinutes
    | YearMonthDateHoursMinutesSeconds
    | Quarter
    | QuarterMonth
    | Month
    | MonthDate
    | Date
    | Day
    | Hours
    | HoursMinutes
    | HoursMinutesSeconds
    | Minutes
    | MinutesSeconds
    | Seconds
    | SecondsMilliseconds
    | Milliseconds
    | Utc TimeUnit
      -- ^ Encode a time as UTC (coordinated universal time, independent of local time
      --   zones or daylight saving).


dateTimeProperty :: DateTime -> LabelledSpec
dateTimeProperty (DTYear y) = "year" .= y
dateTimeProperty (DTQuarter q) = "quarter" .= q
dateTimeProperty (DTMonth mon) = "month" .= monthNameLabel mon
dateTimeProperty (DTDate dt) = "date" .= dt
dateTimeProperty (DTDay day) = "day" .= dayLabel day
dateTimeProperty (DTHours h) = "hours" .= h
dateTimeProperty (DTMinutes m) = "minutes" .= m
dateTimeProperty (DTSeconds s) = "seconds" .= s
dateTimeProperty (DTMilliseconds ms) = "milliseconds" .= ms


dayLabel :: DayName -> T.Text
dayLabel Mon = "Mon"
dayLabel Tue = "Tue"
dayLabel Wed = "Wed"
dayLabel Thu = "Thu"
dayLabel Fri = "Fri"
dayLabel Sat = "Sat"
dayLabel Sun = "Sun"


monthNameLabel :: MonthName -> T.Text
monthNameLabel Jan = "Jan"
monthNameLabel Feb = "Feb"
monthNameLabel Mar = "Mar"
monthNameLabel Apr = "Apr"
monthNameLabel May = "May"
monthNameLabel Jun = "Jun"
monthNameLabel Jul = "Jul"
monthNameLabel Aug = "Aug"
monthNameLabel Sep = "Sep"
monthNameLabel Oct = "Oct"
monthNameLabel Nov = "Nov"
monthNameLabel Dec = "Dec"


timeUnitLabel :: TimeUnit -> T.Text
timeUnitLabel Year = "year"
timeUnitLabel YearQuarter = "yearquarter"
timeUnitLabel YearQuarterMonth = "yearquartermonth"
timeUnitLabel YearMonth = "yearmonth"
timeUnitLabel YearMonthDate = "yearmonthdate"
timeUnitLabel YearMonthDateHours = "yearmonthdatehours"
timeUnitLabel YearMonthDateHoursMinutes = "yearmonthdatehoursminutes"
timeUnitLabel YearMonthDateHoursMinutesSeconds = "yearmonthdatehoursminutesseconds"
timeUnitLabel Quarter = "quarter"
timeUnitLabel QuarterMonth = "quartermonth"
timeUnitLabel Month = "month"
timeUnitLabel MonthDate = "monthdate"
timeUnitLabel Date = "date"
timeUnitLabel Day = "day"
timeUnitLabel Hours = "hours"
timeUnitLabel HoursMinutes = "hoursminutes"
timeUnitLabel HoursMinutesSeconds = "hoursminutesseconds"
timeUnitLabel Minutes = "minutes"
timeUnitLabel MinutesSeconds = "minutesseconds"
timeUnitLabel Seconds = "seconds"
timeUnitLabel SecondsMilliseconds = "secondsmilliseconds"
timeUnitLabel Milliseconds = "milliseconds"
timeUnitLabel (Utc tu) = "utc" <> timeUnitLabel tu


{-|

Represents the type of cursor to display. For an explanation of each type,
see the
<https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Keyword%20values CSS documentation>.

-}
data Cursor
    = CAuto
    | CDefault
    | CNone
    | CContextMenu
    | CHelp
    | CPointer
    | CProgress
    | CWait
    | CCell
    | CCrosshair
    | CText
    | CVerticalText
    | CAlias
    | CCopy
    | CMove
    | CNoDrop
    | CNotAllowed
    | CAllScroll
    | CColResize
    | CRowResize
    | CNResize
    | CEResize
    | CSResize
    | CWResize
    | CNEResize
    | CNWResize
    | CSEResize
    | CSWResize
    | CEWResize
    | CNSResize
    | CNESWResize
    | CNWSEResize
    | CZoomIn
    | CZoomOut
    | CGrab
    | CGrabbing


cursorLabel :: Cursor -> T.Text
cursorLabel CAuto = "auto"
cursorLabel CDefault = "default"
cursorLabel CNone = "none"
cursorLabel CContextMenu = "context-menu"
cursorLabel CHelp = "help"
cursorLabel CPointer = "pointer"
cursorLabel CProgress = "progress"
cursorLabel CWait = "wait"
cursorLabel CCell = "cell"
cursorLabel CCrosshair = "crosshair"
cursorLabel CText = "text"
cursorLabel CVerticalText = "vertical-text"
cursorLabel CAlias = "alias"
cursorLabel CCopy = "copy"
cursorLabel CMove = "move"
cursorLabel CNoDrop = "no-drop"
cursorLabel CNotAllowed = "not-allowed"
cursorLabel CAllScroll = "all-scroll"
cursorLabel CColResize = "col-resize"
cursorLabel CRowResize = "row-resize"
cursorLabel CNResize = "n-resize"
cursorLabel CEResize = "e-resize"
cursorLabel CSResize = "s-resize"
cursorLabel CWResize = "w-resize"
cursorLabel CNEResize = "ne-resize"
cursorLabel CNWResize = "nw-resize"
cursorLabel CSEResize = "se-resize"
cursorLabel CSWResize = "sw-resize"
cursorLabel CEWResize = "ew-resize"
cursorLabel CNSResize = "ns-resize"
cursorLabel CNESWResize = "nesw-resize"
cursorLabel CNWSEResize = "nwse-resize"
cursorLabel CZoomIn = "zoom-in"
cursorLabel CZoomOut = "zoom-out"
cursorLabel CGrab = "grab"
cursorLabel CGrabbing = "grabbing"


-- | Indicates the weight options for a font.

data FontWeight
    = Bold
    | Bolder
    | Lighter
    | Normal
    | W100
    | W200
    | W300
    | W400
    | W500
    | W600
    | W700
    | W800
    | W900


fontWeightSpec :: FontWeight -> VLSpec
fontWeightSpec Bold = fromT "bold"
fontWeightSpec Bolder = fromT "bolder"
fontWeightSpec Lighter = fromT "lighter"
fontWeightSpec Normal = fromT "normal"
fontWeightSpec W100 = fromF 100
fontWeightSpec W200 = fromF 200
fontWeightSpec W300 = fromF 300
fontWeightSpec W400 = fromF 400
fontWeightSpec W500 = fromF 500
fontWeightSpec W600 = fromF 600
fontWeightSpec W700 = fromF 700
fontWeightSpec W800 = fromF 800
fontWeightSpec W900 = fromF 900


{-|

Indicates mark interpolation style. See the
<https://vega.github.io/vega-lite/docs/mark.html#mark-def Vega-Lite documentation>
for details.
-}
data MarkInterpolation
    = Basis
    | BasisClosed
    | BasisOpen
    | Bundle
    | Cardinal
    | CardinalClosed
    | CardinalOpen
    | Linear
    | LinearClosed
    | Monotone
    | StepAfter
    | StepBefore
    | Stepwise


markInterpolationLabel :: MarkInterpolation -> T.Text
markInterpolationLabel Linear = "linear"
markInterpolationLabel LinearClosed = "linear-closed"
markInterpolationLabel Stepwise = "step"
markInterpolationLabel StepBefore = "step-before"
markInterpolationLabel StepAfter = "step-after"
markInterpolationLabel Basis = "basis"
markInterpolationLabel BasisOpen = "basis-open"
markInterpolationLabel BasisClosed = "basis-closed"
markInterpolationLabel Cardinal = "cardinal"
markInterpolationLabel CardinalOpen = "cardinal-open"
markInterpolationLabel CardinalClosed = "cardinal-closed"
markInterpolationLabel Bundle = "bundle"
markInterpolationLabel Monotone = "monotone"


{-|

The orientation of an item. This is used with:
'BLeLDirection', 'LDirection',
'LeGradientDirection', 'LeLDirection', 'LeSymbolDirection',
and 'MOrient'.

In @0.4.0.0@ this was renamed from @MarkOrientation@ to 'Orientation'.

-}

-- based on schema 3.3.0 #/definitions/Orientation

data Orientation
    = Horizontal
      -- ^ Display horizontally.
    | Vertical
      -- ^ Display vertically.


orientationSpec :: Orientation -> VLSpec
orientationSpec Horizontal = "horizontal"
orientationSpec Vertical = "vertical"


{-|

Indicates the extent of the rule used for the error bar.  See
<https://vega.github.io/vega-lite/docs/errorbar.html#properties Vega-Lite documentation>
for details.

Note that not all options are valid for all mark types.

This is called @SummaryExtent@ in Elm and the constructors also have
different names.

@since 0.4.0.0
-}

-- based on schema 3.3.0 #/definitions/ErrorBarExtent
--          (ConfidenceInterval to Iqr)
-- and combined with the box/band "min-max" and IQR scaling values
--

data MarkErrorExtent
  = ConfidenceInterval
    -- ^ Band extent between the 95% confidence intervals of a distribution.
  | StdErr
    -- ^ Band extent as the standard error about the mean of a distribution.
  | StdDev
    -- ^ Band extent as the standard deviation of a distribution.
  | Iqr
    -- ^ Band extent between the lower and upper quartiles of a distribution
    --   (the inter-quartile range, q1 to q3).
  | ExRange
    -- ^ Band extent between the minimum and maximum values in a distribution.
  | IqrScale Double
    -- ^ A scaling of the interquartile range to be used as whiskers in a
    --   boxplot. For example @IqrScale 1.5@  would extend whiskers to
    --   ±1.5x the IQR from the mean.

-- This is a little different from the other calls since I wanted to
-- make sure the scale factor was encoded as a number not a string.
--
extent_ :: T.Text -> LabelledSpec
extent_ v = "extent" .= v

markErrorExtentLSpec :: MarkErrorExtent -> LabelledSpec
markErrorExtentLSpec ConfidenceInterval = extent_ "ci"
markErrorExtentLSpec StdErr             = extent_ "stderr"
markErrorExtentLSpec StdDev             = extent_ "stdev"
markErrorExtentLSpec Iqr                = extent_ "iqr"
markErrorExtentLSpec ExRange            = extent_ "min-max"
markErrorExtentLSpec (IqrScale sc)      = "extent" .= sc


-- | Identifies the type of symbol.

data Symbol
    = SymCircle
      -- ^ Specify a circular symbol for a shape mark.
    | SymSquare
      -- ^ Specify a square symbol for a shape mark.
    | Cross
      -- ^ Specify a cross symbol for a shape mark.
    | Diamond
      -- ^ Specify a diamond symbol for a shape mark.
    | TriangleUp
      -- ^ Specify an upward-triangular symbol for a shape mark.
    | TriangleDown
      -- ^ Specify a downward-triangular symbol for a shape mark.
    | Path T.Text
      -- ^ A custom symbol shape as an
      --   [SVG path description](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths).


symbolLabel :: Symbol -> T.Text
symbolLabel SymCircle = "circle"
symbolLabel SymSquare = "square"
symbolLabel Cross = "cross"
symbolLabel Diamond = "diamond"
symbolLabel TriangleUp = "triangle-up"
symbolLabel TriangleDown = "triangle-down"
symbolLabel (Path svgPath) = svgPath


{-|

Indicates the auto-sizing characteristics of the visualization such as amount
of padding, whether it should fill the parent container etc. For more details see the
<https://vega.github.io/vega-lite/docs/size.html#autosize Vega-Lite documentation>.

-}

data Autosize
    = AContent
      -- ^ Interpret visualization dimensions to be for the data rectangle (external
      --   padding added to this size).
    | AFit
      -- ^ Interpret visualization dimensions to be for the entire visualization (data
      --   rectangle is shrunk to accommodate external decorations padding).
    | ANone
      -- ^ No autosizing is applied.
    | APad
      -- ^ Automatically expand size of visualization from the given dimensions in order
      --   to fit in all supplementary decorations (legends etc.).
    | APadding
      -- ^ Interpret visualization width to be for the entire visualization (data
      -- rectangle is shrunk to accommodate external padding).
    | AResize
      -- ^ Recalculate autosizing on every view update.


autosizeProperty :: Autosize -> LabelledSpec
autosizeProperty APad = ("type", fromT "pad")
autosizeProperty AFit = ("type", fromT "fit")
autosizeProperty ANone = ("type", fromT "none")
autosizeProperty AResize = "resize" .= True
autosizeProperty AContent = ("contains", fromT "content")
autosizeProperty APadding = ("contains", fromT "padding")


{-|

Declare the way the view is sized. See the
<https://vega.github.io/vega-lite/docs/size.html#autosize Vega-Lite documentation>
for details.

@
'toVegaLite'
    [ 'width' 250
    , 'height' 300
    , 'autosize' [ 'AFit', 'APadding', 'AResize' ]
    , 'dataFromUrl' "data/population.json" []
    , 'mark' 'Bar' []
    , enc []
    ]
@
-}
autosize :: [Autosize] -> PropertySpec
autosize aus = (VLAutosize, object (map autosizeProperty aus))


-- | Indicates the style in which field names are displayed.

data FieldTitleProperty
    = Verbal
      -- ^ Creates \"Sum of field\", \"Year of date\", \"field (binned)\", etc.
    | Function
      -- ^ Creates \"SUM(field)\", \"YEAR(date)\", \"BIN(field)\", etc.
    | Plain
      -- ^ Just use the field name without any extra text.


fieldTitleLabel :: FieldTitleProperty -> T.Text
fieldTitleLabel Verbal = "verbal"
fieldTitleLabel Function = "functional"
fieldTitleLabel Plain = "plain"


-- | Indicates the type of legend to create.

data Legend
    = Gradient
      -- ^ Typically used for continuous quantitative data.
    | Symbol
      -- ^ Typically used for categorical data.


legendLabel :: Legend -> T.Text
legendLabel Gradient = "gradient"
legendLabel Symbol = "symbol"


{-|

Legend configuration options. For more detail see the
<https://vega.github.io/vega-lite/docs/legend.html#config Vega-Lite documentation>.

This data type has seen significant changes in the @0.4.0.0@ release:

- the @EntryPadding@, @GradientHeight@, @GradientLabelBaseline@, @GradientWidth@
  and @SymbolColor@ constructors were removed;

- the constructors were removed;

- the remaining constructors that did not begin with @Le@ were renamed (for
  example @Orient@ was changed to 'LeOrient');

- and new constructors were added.

-}

-- based on schema 3.3.0 #/definitions/LegendConfig

data LegendConfig
    = LeClipHeight Double
      -- ^ The height in pixels at which to clip symbol legend entries.
      --
      --   @since 0.4.0.0
    | LeColumnPadding Double
      -- ^ The horizontal padding, in pixels, between symbol legend entries.
      --
      --   @since 0.4.0.0
    | LeColumns Int
      -- ^ The number of columns in which to arrange symbol legend entries. A value
      --   of @0@ or lower indicates a single row with one column per entry.
      --
      --   @since 0.4.0.0
    | LeCornerRadius Double
      -- ^ The corner radius for the full legend.
    | LeFillColor Color
      -- ^ The background fill color for the full legend.
    | LeGradientDirection Orientation
      -- ^ The default direction for gradient legends.
      --
      --   @since 0.4.0.0
    | LeGradientHorizontalMaxLength Double
      -- ^ The maximum legend length for a horizontal gradient.
      --
      --   @since 0.4.0.0
    | LeGradientHorizontalMinLength Double
      -- ^ The minimum legend length for a horizontal gradient.
      --
      --   @since 0.4.0.0
    | LeGradientLabelLimit Double
      -- ^ The maximum allowed length, in pixels, of color-ramp gradient labels.
    | LeGradientLabelOffset Double
      -- ^ The vertical offset in pixels for color-ramp gradient labels.
    | LeGradientLength Double
      -- ^ The length in pixels of the primary axis of a color gradient.
      --   See also 'LeGradientThickness'.
      --
      --   @since 0.4.0.0
    | LeGradientOpacity Opacity
      -- ^ The opacity of the color gradient.
      --
      --   @since 0.4.0.0
    | LeGradientStrokeColor Color
      -- ^ The color of the gradient stroke.
    | LeGradientStrokeWidth Double
      -- ^ The width of the gradient stroke, in pixels.
    | LeGradientThickness Double
      -- ^ The thickness in pixels of the color gradient. See also 'LeGradientLength'.
      --
      --   @since 0.4.0.0
    | LeGradientVerticalMaxLength Double
      -- ^ The maximum legend length for a vertical gradient.
      --
      --   @since 0.4.0.0
    | LeGradientVerticalMinLength Double
      -- ^ The minimum legend length for a vertical gradient.
      --
      --   @since 0.4.0.0
    | LeGridAlign CompositionAlignment
      -- ^ The alignment to apply to symbol legends rows and columns.
      --
      --    @since 0.4.0.0
    | LeLabelAlign HAlign
      -- ^ The alignment of the legend label.
    | LeLabelBaseline VAlign
      -- ^ The position of the baseline of the legend label.
    | LeLabelColor Color
      -- ^ The color of the legend label.
    | LeLabelFont T.Text
      -- ^ The font of the legend label.
    | LeLabelFontSize Double
      -- ^ The font of the legend label.
    | LeLabelFontStyle T.Text
      -- ^ The font style of the legend label.
      --
      --   @since 0.4.0.0
    | LeLabelFontWeight FontWeight
      -- ^ The font weight of the legend label.
      --
      --   @since 0.4.0.0
    | LeLabelLimit Double
      -- ^ The maxumum allowed pixel width of the legend label.
    | LeLabelOffset Double
      -- ^ The offset of the legend label.
    | LeLabelOpacity Opacity
      -- ^ The opacity of the legend label.
      --
      --   @since 0.4.0.0
    | LeLabelOverlap OverlapStrategy
      -- ^ How to resolve overlap of labels in gradient legends.
      --
      --   @since 0.4.0.0
    | LeLabelPadding Double
      -- ^ The passing in pixels between the legend and legend labels.
      --
      --   @since 0.4.0.0
    | LeLabelSeparation Double
      -- ^ The minimum separation between label bounding boxes for them
      --   to be considered non-overlapping (ignored if 'ONone' is the
      --   chosen overlap strategy).
      --
      --   @since 0.4.0.0
    | LeLayout [LegendLayout]
      -- ^ Layout parameters for the legend orient group.
      --
      --   @since 0.4.0.0
     | LeLeX Double
      -- ^ Custom x position for a legend with orientation 'LONone'.
      --
      --   @since 0.4.0.0
     | LeLeY Double
      -- ^ Custom y position for a legend with orientation 'LONone'.
      --
      --   @since 0.4.0.0
    | LeOffset Double
      -- ^ The offset in pixels between the legend and the data rectangle
      --   and axes.
    | LeOrient LegendOrientation
      -- ^ The orientation of the legend.
    | LePadding Double
      -- ^ The padding between the border and content of the legend group.
    | LeRowPadding Double
      -- ^ The vertical padding in pixels between symbol legend entries.
      --
      --   @since 0.4.0.0
    | LeShortTimeLabels Bool
      -- ^ Should month and weekday names be abbreviated?
    | LeStrokeColor Color
      -- ^ The border stoke color for the full legend.
    | LeStrokeDash [Double]
      -- ^ The border stroke dash pattern for the full legend (alternating
      --   stroke, space lengths in pixels).
    | LeStrokeWidth Double
      -- ^ The border stroke width for the full legend.
    | LeSymbolBaseFillColor Color
      -- ^ The fill color for legend symbols. This is only applied if
      --   there is no \"fill\" scale color encoding for the legend.
      --
      --   @since 0.4.0.0
    | LeSymbolBaseStrokeColor Color
      -- ^ The stroke color for legend symbols. This is only applied if
      --   there is no \"fill\" scale color encoding for the legend.
      --
      --   @since 0.4.0.0
    | LeSymbolDash [Double]
      -- ^ The pattern for dashed symbol strokes (alternating
      --   stroke, space lengths in pixels).
      --
      --   @since 0.4.0.0
    | LeSymbolDashOffset Double
      -- ^ The offset at which to start deawing the symbol dash pattern,
      --   in pixels.
      --
      --   @since 0.4.0.0
    | LeSymbolDirection Orientation
      -- ^ The default direction for symbol legends.
      --
      --   @since 0.4.0.0
    | LeSymbolFillColor Color
      -- ^ The color of the legend symbol.
      --
      --   @since 0.4.0.0
    | LeSymbolOffset Double
      -- ^ The horizontal pixel offset for legend symbols.
      --
      --   @since 0.4.0.0
    | LeSymbolOpacity Opacity
      -- ^ The opacity of the legend symbols.
      --
      --   @since 0.4.0.0
    | LeSymbolSize Double
      -- ^ The size of the legend symbol, in pixels.
    | LeSymbolStrokeColor Color
      -- ^ The stroke color for legend symbols.
      --
      --   @since 0.4.0.0
    | LeSymbolStrokeWidth Double
      -- ^ The width of the symbol's stroke.
    | LeSymbolType Symbol
      -- ^ The default shape type for legend symbols.
    | LeTitle T.Text
      -- ^ The legend title.
      --
      --   @since 0.4.0.0
    | LeNoTitle
      -- ^ Draw no title for the legend.
      --
      --   @since 0.4.0.0
    | LeTitleAlign HAlign
      -- ^ The horizontal text alignment for legend titles.
    | LeTitleAnchor APosition
      -- ^ The text anchor position for legend titles.
      --
      --   @since 0.4.0.0
    | LeTitleBaseline VAlign
      -- ^ The vertical text alignment for legend titles.
    | LeTitleColor Color
      -- ^ The color of the legend title.
    | LeTitleFont T.Text
      -- ^ The font of the legend title.
    | LeTitleFontSize Double
      -- ^ The font size of the legend title.
    | LeTitleFontStyle T.Text
      -- ^ The font style for the legend title.
      --
      --   @since 0.4.0.0
    | LeTitleFontWeight FontWeight
      -- ^ The font weight of the legend title.
    | LeTitleLimit Double
      -- ^ The maxmimum pixel width of the legend title.
    | LeTitleOpacity Opacity
      -- ^ The opacity of the legend title.
      --
      --   @since 0.4.0.0
    | LeTitleOrient Side
      -- ^ The orientation of the legend title.
      --
      --   @since 0.4.0.0
    | LeTitlePadding Double
      -- ^ The padding, in pixels, between title and legend.


legendConfigProperty :: LegendConfig -> LabelledSpec
legendConfigProperty (LeClipHeight x) = "clipHeight" .= x
legendConfigProperty (LeColumnPadding x) = "columnPadding" .= x
legendConfigProperty (LeColumns n) = "columns" .= n
legendConfigProperty (LeCornerRadius x) = "cornerRadius" .= x
legendConfigProperty (LeFillColor s) = "fillColor" .= s
legendConfigProperty (LeGradientDirection o) = "gradientDirection" .= orientationSpec o
legendConfigProperty (LeGradientHorizontalMaxLength x) = "gradientHorizontalMaxLength" .= x
legendConfigProperty (LeGradientHorizontalMinLength x) = "gradientHorizontalMinLength" .= x
legendConfigProperty (LeGradientLabelLimit x) = "gradientLabelLimit" .= x
legendConfigProperty (LeGradientLabelOffset x) = "gradientLabelOffset" .= x
legendConfigProperty (LeGradientLength x) = "gradientLength" .= x
legendConfigProperty (LeGradientOpacity x) = "gradientOpacity" .= x
legendConfigProperty (LeGradientStrokeColor s) = "gradientStrokeColor" .= s
legendConfigProperty (LeGradientStrokeWidth x) = "gradientStrokeWidth" .= x
legendConfigProperty (LeGradientThickness x) = "gradientThickness" .= x
legendConfigProperty (LeGradientVerticalMaxLength x) = "gradientVerticalMaxLength" .= x
legendConfigProperty (LeGradientVerticalMinLength x) = "gradientVerticalMinLength" .= x
legendConfigProperty (LeGridAlign ga) = "gridAlign" .= compositionAlignmentSpec ga
legendConfigProperty (LeLabelAlign ha) = "labelAlign" .= hAlignLabel ha
legendConfigProperty (LeLabelBaseline va) = "labelBaseline" .= vAlignLabel va
legendConfigProperty (LeLabelColor s) = "labelColor" .= s
legendConfigProperty (LeLabelFont s) = "labelFont" .= s
legendConfigProperty (LeLabelFontSize x) = "labelFontSize" .= x
legendConfigProperty (LeLabelFontStyle s) = "labelFontStyle" .= s
legendConfigProperty (LeLabelFontWeight fw) = "labelFontWeight" .= fontWeightSpec fw
legendConfigProperty (LeLabelLimit x) = "labelLimit" .= x
legendConfigProperty (LeLabelOffset x) = "labelOffset" .= x
legendConfigProperty (LeLabelOpacity x) = "labelOapcity" .= x
legendConfigProperty (LeLabelOverlap olap) = "labelOverlap" .= overlapStrategyLabel olap
legendConfigProperty (LeLabelPadding x) = "labelPadding" .= x
legendConfigProperty (LeLabelSeparation x) = "labelSeparation" .= x
legendConfigProperty (LeLayout ll) = "layout" .= object (map legendLayoutSpec ll)
legendConfigProperty (LeLeX x) = "legendX" .= x
legendConfigProperty (LeLeY x) = "legendY" .= x
legendConfigProperty (LeOffset x) = "offset" .= x
legendConfigProperty (LeOrient orl) = "orient" .= legendOrientLabel orl
legendConfigProperty (LePadding x) = "padding" .= x
legendConfigProperty (LeRowPadding x) = "rowPadding" .= x
legendConfigProperty (LeShortTimeLabels b) = "shortTimeLabels" .= b
legendConfigProperty (LeStrokeColor s) = "strokeColor" .= s
legendConfigProperty (LeStrokeDash xs) = "strokeDash" .= xs
legendConfigProperty (LeStrokeWidth x) = "strokeWidth" .= x
legendConfigProperty (LeSymbolBaseFillColor s) = "symbolBaseFillColor" .= s
legendConfigProperty (LeSymbolBaseStrokeColor s) = "symbolBaseStrokeColor" .= s
legendConfigProperty (LeSymbolDash xs) = "symbolDash" .= xs
legendConfigProperty (LeSymbolDashOffset x) = "symbolDashOffset" .= x
legendConfigProperty (LeSymbolDirection o) = "symbolDirection" .= orientationSpec o
legendConfigProperty (LeSymbolFillColor s) = "symbolFillColor" .= s
legendConfigProperty (LeSymbolOffset x) = "symbolOffset" .= x
legendConfigProperty (LeSymbolOpacity x) = "symbolOpacity" .= x
legendConfigProperty (LeSymbolSize x) = "symbolSize" .= x
legendConfigProperty (LeSymbolStrokeColor s) = "symbolStrokeColor" .= s
legendConfigProperty (LeSymbolStrokeWidth x) = "symbolStrokeWidth" .= x
legendConfigProperty (LeSymbolType s) = "symbolType" .= symbolLabel s
legendConfigProperty (LeTitle s) = "title" .= s
legendConfigProperty LeNoTitle = "title" .= A.Null
legendConfigProperty (LeTitleAlign ha) = "titleAlign" .= hAlignLabel ha
legendConfigProperty (LeTitleAnchor anc) = "titleAnchor" .= anchorLabel anc
legendConfigProperty (LeTitleBaseline va) = "titleBaseline" .= vAlignLabel va
legendConfigProperty (LeTitleColor s) = "titleColor" .= s
legendConfigProperty (LeTitleFont s) = "titleFont" .= s
legendConfigProperty (LeTitleFontSize x) = "titleFontSize" .= x
legendConfigProperty (LeTitleFontStyle s) = "titleFontStyle" .= s
legendConfigProperty (LeTitleFontWeight fw) = "titleFontWeight" .= fontWeightSpec fw
legendConfigProperty (LeTitleLimit x) = "titleLimit" .= x
legendConfigProperty (LeTitleOpacity x) = "titleOpacity" .= x
legendConfigProperty (LeTitleOrient orient) = "titleOrient" .= sideLabel orient
legendConfigProperty (LeTitlePadding x) = "titlePadding" .= x


{-|

Indicates the legend orientation. See the
<https://vega.github.io/vega-lite/docs/legend.html#config Vega-Lite documentation>
for more details.

-}

-- based on schema 3.3.0 #/definitions/LegendOrient

data LegendOrientation
  = LONone
  | LOLeft
  | LORight
  | LOTop
  -- ^ @since 0.4.0.0
  | LOBottom
  -- ^ @since 0.4.0.0
  | LOTopLeft
  | LOTopRight
  | LOBottomLeft
  | LOBottomRight


legendOrientLabel :: LegendOrientation -> T.Text
legendOrientLabel LONone = "none"
legendOrientLabel LOLeft = "left"
legendOrientLabel LORight = "right"
legendOrientLabel LOTop = "top"
legendOrientLabel LOBottom = "bottom"
legendOrientLabel LOTopLeft = "top-left"
legendOrientLabel LOTopRight = "top-right"
legendOrientLabel LOBottomLeft = "bottom-left"
legendOrientLabel LOBottomRight = "bottom-right"


{- |

/Highly experimental/ and used with 'LeLayout'.

@since 0.4.0.0

-}

-- based on schema 3.3.0 #/definitions/LegendLayout

-- TODO: support SignalRef?

data LegendLayout
  = LeLAnchor APosition
    -- ^ The anchor point for legend orient group layout.
  | LeLBottom [BaseLegendLayout]
  | LeLBottomLeft [BaseLegendLayout]
  | LeLBottomRight [BaseLegendLayout]
  | LeLBounds Bounds
    -- ^ The bounds calculation to ude for legend orient group layout.
  | LeLCenter Bool
    -- ^ A flag to center legends within a shared orient group.
  | LeLDirection Orientation
    -- ^ The layout firection for legend orient group layout.
  | LeLLeft [BaseLegendLayout]
  | LeLMargin Double
    -- ^ The margin, in pixels, between legends within an orient group.
  | LeLOffset Double
    -- ^ The offset, in pixels, from the chart body for a legend orient group.
  | LeLRight [BaseLegendLayout]
  | LeLTop [BaseLegendLayout]
  | LeLTopLeft [BaseLegendLayout]
  | LeLTopRight [BaseLegendLayout]


legendLayoutSpec :: LegendLayout -> LabelledSpec
legendLayoutSpec (LeLAnchor anc) = "anchor" .= anchorLabel anc
legendLayoutSpec (LeLBottom bl) = "bottom" .= toBLSpec bl
legendLayoutSpec (LeLBottomLeft bl) = "bottom-left" .= toBLSpec bl
legendLayoutSpec (LeLBottomRight bl) = "bottom-right" .= toBLSpec bl
legendLayoutSpec (LeLBounds bnds) = "bounds" .= boundsSpec bnds
legendLayoutSpec (LeLCenter b) = "center" .= b
legendLayoutSpec (LeLDirection o) = "direction" .= orientationSpec o
legendLayoutSpec (LeLLeft bl) = "left" .= toBLSpec bl
legendLayoutSpec (LeLMargin x) = "margin" .= x
legendLayoutSpec (LeLOffset x) = "offset" .= x
legendLayoutSpec (LeLRight bl) = "right" .= toBLSpec bl
legendLayoutSpec (LeLTop bl) = "top" .= toBLSpec bl
legendLayoutSpec (LeLTopLeft bl) = "top-left" .= toBLSpec bl
legendLayoutSpec (LeLTopRight bl) = "top-right" .= toBLSpec bl


toBLSpec :: [BaseLegendLayout] -> VLSpec
toBLSpec = object . map baseLegendLayoutSpec

{- |

/Highly experimental/ and used with constructors from 'LegendLayout'.

@since 0.4.0.0

-}

-- based on schema 3.3.0 #/definitions/BaseLegendLayout

data BaseLegendLayout
  = BLeLAnchor APosition
    -- ^ The anchor point for legend orient group layout.
  | BLeLBounds Bounds
    -- ^ The bounds calculation to use for legend orient group layout.
  | BLeLCenter Bool
    -- ^ A flag to center legends within a shared orient group.
  | BLeLDirection Orientation
    -- ^ The layout direction for legend orient group layout.
  | BLeLMargin Double
    -- ^ The margin, in pixels, between legends within an orient group.
  | BLeLOffset Double
    -- ^ The offset, in pixels, from the chart body for a legend orient group.


baseLegendLayoutSpec :: BaseLegendLayout -> LabelledSpec
baseLegendLayoutSpec (BLeLAnchor anc) = "anchor" .= anchorLabel anc
baseLegendLayoutSpec (BLeLBounds bnds) = "bounds" .= boundsSpec bnds
baseLegendLayoutSpec (BLeLCenter b) = "center" .= b
baseLegendLayoutSpec (BLeLDirection o) = "direction" .= orientationSpec o
baseLegendLayoutSpec (BLeLMargin x) = "margin" .= x
baseLegendLayoutSpec (BLeLOffset x) = "offset" .= x


{-|

Legend properties. For more detail see the
<https://vega.github.io/vega-lite/docs/legend.html#legend-properties Vega-Lite documentation>.

The @LEntryPadding@ constructor was removed in @0.4.0.0@.

-}

data LegendProperty
    = LClipHeight Double
      -- ^ The height, in pixels, to clip symbol legend entries.
      --
      --   @since 0.4.0.0
    | LColumnPadding Double
      -- ^ The horizontal padding, in pixels, between symbol legend entries.
      --
      --   @since 0.4.0.0
    | LColumns Int
      -- ^ The number of columns in which to arrange symbol legend entries.
      --   A value of @0@ or lower indicates a single row with one column per entry.
      --
      --   @since 0.4.0.0
    | LCornerRadius Double
      -- ^ The corner radius for the full legend.
      --
      --   @since 0.4.0.0
    | LDirection Orientation
      -- ^ The direction of the legend.
      --
      --   @since 0.4.0.0
    | LFillColor Color
      -- ^ The background fill color for the full legend.
      --
      --   @since 0.4.0.0
    | LFormat T.Text
      -- ^ [Formatting pattern](https://vega.github.io/vega-lite/docs/format.html) for
      --   legend values. To distinguish between formatting as numeric values
      --   and data/time values, additionally use 'LFormatAsNum' or 'LFormatAsTemporal'.
    | LFormatAsNum
      -- ^ Legends should be formatted as numbers. Use a
      --   [d3 numeric format string](https://github.com/d3/d3-format#locale_format)
      --   with 'LFormat'.
      --
      -- @since 0.4.0.0
    | LFormatAsTemporal
      -- ^ Legends should be formatted as dates or times. Use a
      --   [d3 date/time format string](https://github.com/d3/d3-time-format#locale_format)
      --   with 'LFormat'.
      --
      -- @since 0.4.0.0
    | LGradientLength Double
      -- ^ The length in pixels of the primary axis of the color gradient.
      --
      --   @since 0.4.0.0
    | LGradientOpacity Opacity
      -- ^ The opacity of the color gradient.
      --
      --   @since 0.4.0.0
    | LGradientStrokeColor Color
      -- ^ The color of the gradient stroke.
      --
      --   @since 0.4.0.0
    | LGradientStrokeWidth Double
      -- ^ The width, in pixels, of the gradient stroke.
      --
      --   @since 0.4.0.0
    | LGradientThickness Double
      -- ^ The thickness, in pixels, of the color gradient.
      --
      --   @since 0.4.0.0
    | LGridAlign CompositionAlignment
      -- ^ The [grid layout](https://vega.github.io/vega/docs/layout) for
      --   the symbol legends.
      --
      --   @since 0.4.0.0
    | LLabelAlign HAlign
      -- ^ @since 0.4.0.0
    | LLabelBaseline VAlign
      -- ^ @since 0.4.0.0
    | LLabelColor Color
      -- ^ @since 0.4.0.0
    | LLabelFont T.Text
      -- ^ @since 0.4.0.0
    | LLabelFontSize Double
      -- ^ @since 0.4.0.0
    | LLabelFontStyle T.Text
      -- ^ @since 0.4.0.0
    | LLabelFontWeight FontWeight
      -- ^ @since 0.4.0.0
    | LLabelLimit Double
      -- ^ @since 0.4.0.0
    | LLabelOffset Double
      -- ^ @since 0.4.0.0
    | LLabelOpacity Opacity
      -- ^ @since 0.4.0.0
    | LLabelOverlap OverlapStrategy
      -- ^ @since 0.4.0.0
    | LLabelPadding Double
      -- ^ @since 0.4.0.0
    | LLabelSeparation Double
      -- ^ @since 0.4.0.0
    | LOffset Double
      -- ^ The offset in pixels by which to displace the legend from
      --   the data rectangle and axes.
    | LOrient LegendOrientation
      -- ^ The legend orientation.
    | LPadding Double
      -- ^ The padding, in pixels, between the border and content of
      --   the legend group.
    | LRowPadding Double
      -- ^ The vertical padding, in pixels, between symbol legend entries.
      --
      --   @since 0.4.0.0
    | LStrokeColor Color
      -- ^ The border stroke color for the full legend.
      --
      --   @since 0.4.0.0
    | LSymbolDash [Double]
      -- ^ The dash style for symbols (alternating stroke, space lengths
      --   in pixels).
      --
      --   @since 0.4.0.0
    | LSymbolDashOffset Double
      -- ^ The pixel offset at which to start drawing the symbol dash array.
      --
      --   @since 0.4.0.0
    | LSymbolFillColor Color
      -- ^ The fill color of the legend symbol.
      --
      --   @since 0.4.0.0
    | LSymbolOffset Double
      -- ^ The horizontal pixel offset for legend symbols.
      --
      --   @since 0.4.0.0
    | LSymbolOpacity Opacity
      -- ^ The opacity of the legend symbols.
      --
      --   @since 0.4.0.0
    | LSymbolSize Double
      -- ^ The size of the legend symbol, in pixels.
      --
      --   @since 0.4.0.0
    | LSymbolStrokeColor Color
      -- ^ The edge color of the legend symbol.
      --
      --   @since 0.4.0.0
    | LSymbolStrokeWidth Double
      -- ^ The width of the sumbol's stroke.
      --
      --   @since 0.4.0.0
    | LSymbolType Symbol
      -- ^ @since 0.4.0.0
    | LTickCount Double
      -- ^ The desired number of tick values for quantitative legends.
    | LTickMinStep Double
      -- ^ The minimum desired step between legend ticks, in terms of the scale
      --   domain values.
      --
      --   @since 0.4.0.0
    | LTitle T.Text
    | LNoTitle
      -- ^ Draw no title.
      --
      -- @since 0.4.0.0
    | LTitleAlign HAlign
      -- ^ @since 0.4.0.0
    | LTitleAnchor APosition
      -- ^ @since 0.4.0.0
    | LTitleBaseline VAlign
      -- ^ @since 0.4.0.0
    | LTitleColor Color
      -- ^ @since 0.4.0.0
    | LTitleFont T.Text
      -- ^ @since 0.4.0.0
    | LTitleFontSize Double
      -- ^ @since 0.4.0.0
    | LTitleFontStyle T.Text
      -- ^ @since 0.4.0.0
    | LTitleFontWeight FontWeight
      -- ^ @since 0.4.0.0
    | LTitleLimit Double
      -- ^ The maximum allowed pixel width of the legend title.
      --
      --   @since 0.4.0.0
    | LTitleOpacity Opacity
      -- ^ Opacity of the legend title.
      --
      --   @since 0.4.0.0
    | LTitleOrient Side
      -- ^ Orientation of the legend title.
      --
      --   @since 0.4.0.0
    | LTitlePadding Double
      -- ^ The padding, in pixels, between title and legend.
      --
      --   @since 0.4.0.0
    | LType Legend
      -- ^ The type of the legend.
    | LValues LegendValues
      -- ^ Explicitly set the visible legend values.
    | LeX Double
      -- ^ Custom x position, in pixels, for the legend when 'LOrient' is set to 'LONone'.
      --
      --   @since 0.4.0.0
    | LeY Double
      -- ^ Custom y position, in pixels, for the legend when 'LOrient' is set to 'LONone'.
      --
      --   @since 0.4.0.0
    | LZIndex ZIndex
      -- ^ The z-index at which to draw the legend.

legendProperty :: LegendProperty -> LabelledSpec
legendProperty (LClipHeight x) = "clipHeight" .= x
legendProperty (LColumnPadding x) = "columnPadding" .= x
legendProperty (LColumns n) = "columns" .= n
legendProperty (LCornerRadius x) = "cornerRadius" .= x
legendProperty (LDirection o) = "direction" .= orientationSpec o
legendProperty (LFillColor s) = "fillColor" .= s
legendProperty (LFormat s) = "format" .= s
legendProperty LFormatAsNum = "formatType" .= fromT "number"
legendProperty LFormatAsTemporal = "formatType" .= fromT "time"
legendProperty (LGradientLength x) = "gradientLength" .= x
legendProperty (LGradientOpacity x) = "gradientOpacity" .= x
legendProperty (LGradientStrokeColor s) = "gradientStrokeColor" .= s
legendProperty (LGradientStrokeWidth x) = "gradientStrokeWidth" .= x
legendProperty (LGradientThickness x) = "gradientThickness" .= x
legendProperty (LGridAlign ga) = "gridAlign" .= compositionAlignmentSpec ga
legendProperty (LLabelAlign ha) = "labelAlign" .= hAlignLabel ha
legendProperty (LLabelBaseline va) = "labelBaseline" .= vAlignLabel va
legendProperty (LLabelColor s) = "labelColor" .= s
legendProperty (LLabelFont s) = "labelFont" .= s
legendProperty (LLabelFontSize x) = "labelFontSize" .= x
legendProperty (LLabelFontStyle s) = "labelFontStyle" .= s
legendProperty (LLabelFontWeight fw) = "labelFontWeight" .= fontWeightSpec fw
legendProperty (LLabelLimit x) = "labelLimit" .= x
legendProperty (LLabelOffset x) = "labelOffset" .= x
legendProperty (LLabelOpacity x) = "labelOpacity" .= x
legendProperty (LLabelOverlap strat) = "labelOverlap" .= overlapStrategyLabel strat
legendProperty (LLabelPadding x) = "labelPadding" .= x
legendProperty (LLabelSeparation x) = "labelSeparation" .= x
legendProperty (LOffset x) = "offset" .= x
legendProperty (LOrient orl) = "orient" .= legendOrientLabel orl
legendProperty (LPadding x) = "padding" .= x
legendProperty (LRowPadding x) = "rowPadding" .= x
legendProperty (LStrokeColor s) = "strokeColor" .= s

legendProperty (LSymbolDash ds) = "symbolDash" .= ds
legendProperty (LSymbolDashOffset x) = "symbolDashOffset" .= x
legendProperty (LSymbolFillColor s) = "symbolFillColor" .= s
legendProperty (LSymbolOffset x) = "symbolOffset" .= x
legendProperty (LSymbolOpacity x) = "symbolOpacity" .= x
legendProperty (LSymbolSize x) = "symbolSize" .= x
legendProperty (LSymbolStrokeColor s) = "symbolStrokeColor" .= s
legendProperty (LSymbolStrokeWidth x) = "symbolStrikeWidth" .= x
legendProperty (LSymbolType sym) = "symbolType" .= symbolLabel sym
legendProperty (LTickCount x) = "tickCount" .= x
legendProperty (LTickMinStep x) = "tickMinStep" .= x
legendProperty (LTitle s) = "title" .= s
legendProperty LNoTitle = "title" .= A.Null
legendProperty (LTitleAlign ha) = "titleAlign" .= hAlignLabel ha
legendProperty (LTitleAnchor anc) = "titleAnchor" .= anchorLabel anc
legendProperty (LTitleBaseline va) = "titleBaseline" .= vAlignLabel va
legendProperty (LTitleColor s) = "titleColor" .= s
legendProperty (LTitleFont s) = "titleFont" .= s
legendProperty (LTitleFontSize x) = "titleFontSize" .= x
legendProperty (LTitleFontStyle s) = "titleFontStyle" .= s
legendProperty (LTitleFontWeight fw) = "titleFontWeight" .= fontWeightSpec fw
legendProperty (LTitleLimit x) = "titleLimit" .= x
legendProperty (LTitleOpacity x) = "titleOpacity" .= x
legendProperty (LTitleOrient orient) = "titleOrient" .= sideLabel orient
legendProperty (LTitlePadding x) = "titlePadding" .= x
legendProperty (LType lType) = "type" .= legendLabel lType
legendProperty (LValues vals) =
  let ls = case vals of
        LNumbers xs    -> map toJSON xs
        LDateTimes dts -> map (object . map dateTimeProperty) dts
        LStrings ss    -> map toJSON ss
  in "values" .= ls
legendProperty (LeX x) = "legendX" .= x
legendProperty (LeY x) = "legendY" .= x
legendProperty (LZIndex z) = "zindex" .= z


-- | A list of data values suitable for setting legend values.

data LegendValues
    = LDateTimes [[DateTime]]
    | LNumbers [Double]
    | LStrings [T.Text]


-- | Specify the padding dimensions in pixel units.

data Padding
    = PSize Double
      -- ^ Use the same padding on all four edges of the container.
    | PEdges Double Double Double Double
      -- ^ Specify the padding for the left, top, right, and bottom edges.


paddingSpec :: Padding -> VLSpec
paddingSpec (PSize p) = toJSON p
paddingSpec (PEdges l t r b) =
  object [ "left" .= l
         , "top" .= t
         , "right" .= r
         , "bottom" .= b
         ]


-- | The properties of a point marker on a line or area mark.
--   For use with 'MPoint'.
--
--   @since 0.4.0.0

data PointMarker
    = PMTransparent
    -- ^ A transparent marker is used, which can be useful for
    --   interactive selections.
    | PMNone
    -- ^ No marker to be shown.
    | PMMarker [MarkProperty]
    -- ^ The properties of the marks to be shown at the points.
    --
    --   Use an empty list to use a filled point with default properties.

-- An empty object has the same meaning as true, so there is no real need to
-- treat 'PMMarker []' specially, but I don't think it complicates things
-- here.
--
pointMarkerSpec :: PointMarker -> VLSpec
pointMarkerSpec PMTransparent = "transparent"
pointMarkerSpec PMNone = toJSON False
pointMarkerSpec (PMMarker []) = toJSON True
pointMarkerSpec (PMMarker mps) = object (map markProperty mps)


{-|

Types of geographic map projection. These are based on a subset of those provided
by the <https://github.com/d3/d3-geo d3-geo library>. For details of available
projections see the
<https://vega.github.io/vega-lite/docs/projection.html#projection-types Vega-Lite documentation>.
-}

-- based on schema 3.3.0 #/definitions/ProjectionType

data Projection
    = Albers
      -- ^ An Albers equal-area conic map projection.
    | AlbersUsa
      -- ^ An Albers USA map projection that combines continental USA with
      --   Alaska and Hawaii. Unlike other projection types, this remains
      --   unaffected by 'PrRotate'.
    | AzimuthalEqualArea
      -- ^ An azimuthal equal area map projection.
    | AzimuthalEquidistant
      -- ^ An azimuthal equidistant map projection.
    | ConicConformal
      -- ^ A conformal conic map projection.
    | ConicEqualArea
      -- ^ An equal area conic map projection.
    | ConicEquidistant
      -- ^ An equidistant conic map projection.
    | Custom T.Text
      -- ^ Specify the name of the custom D3 prohection to use. See the
      --   <https://vega.github.io/vega/docs/projections/#register Vega API>
      --   for more information.
      --
      --   An example: @Custom "winkle3"@
    | Equirectangular
      -- ^ An equirectangular map projection that maps longitude to x and latitude to y.
      --   While showing less area distortion towards the poles than the default 'Mercator'
      --   projection, it is neither equal-area nor conformal.
    | Gnomonic
      -- ^ A gnomonic map projection.
    | Identity
      -- ^ The identiy projection. This can be combined with 'PrReflectX' and
      --   'PrReflectY' in the list of projection properties.
      --
      --   @since 0.4.0.0
    | Mercator
      -- ^ A Mercator map projection. This is the default projection of longitude, latitude
      --   values if no projection is set explicitly. It preserves shape (local angle) and
      --   lines of equal angular bearing remain parallel straight lines. The area is
      --   /significantly/ enlarged towards the poles.
    | Orthographic
      -- ^ An orthographic map projection.
    | Stereographic
      -- ^ A stereographic map projection.
    | TransverseMercator
      -- ^ A transverse Mercator map projection.


projectionLabel :: Projection -> T.Text
projectionLabel Albers = "albers"
projectionLabel AlbersUsa = "albersUsa"
projectionLabel AzimuthalEqualArea = "azimuthalEqualArea"
projectionLabel AzimuthalEquidistant = "azimuthalEquidistant"
projectionLabel ConicConformal = "conicConformal"
projectionLabel ConicEqualArea = "conicEqualarea"
projectionLabel ConicEquidistant = "conicEquidistant"
projectionLabel (Custom pName) = pName
projectionLabel Equirectangular = "equirectangular"
projectionLabel Gnomonic = "gnomonic"
projectionLabel Identity = "identity"
projectionLabel Mercator = "mercator"
projectionLabel Orthographic = "orthographic"
projectionLabel Stereographic = "stereographic"
projectionLabel TransverseMercator = "transverseMercator"


-- | Specifies a clipping rectangle for defining
--   the clip extent of a map projection.

data ClipRect
    = NoClip
      -- ^ No clipping it to be applied.
    | LTRB Double Double Double Double
      -- ^ The left, top, right, and bottom extents, in pixels,
      --   of a rectangular clip.


-- | Specifies the alignment of compositions. It is used with:
--   'align', 'alignRC', 'LeGridAlign', and 'LGridAlign'.
--
--   @since 0.4.0.0

data CompositionAlignment
    = CANone
    -- ^ Flow layout is used, where adjacent subviews are placed one after
    --   another.
    | CAEach
    -- ^ Each row and column may be of a variable size.
    | CAAll
    -- ^ All the rows and columns are of the same size (this is based on the
    --   maximum subview size).


compositionAlignmentSpec :: CompositionAlignment -> VLSpec
compositionAlignmentSpec CANone = "none"
compositionAlignmentSpec CAEach = "each"
compositionAlignmentSpec CAAll = "all"


{-|

Properties for customising a geospatial projection that converts longitude,latitude
pairs into planar @(x,y)@ coordinate pairs for rendering and query. For details see the
<https://vega.github.io/vega-lite/docs/projection.html Vega-Lite documentation>.

This type has been changed in the @0.4.0.0@ release so that all constructors
start with @Pr@ rather than @P@ (and so provide some differentiation to the
'PositionChannel' constructors).

-}

-- based on schema 3.3.0 #/definitions/Projection

data ProjectionProperty
    = PrType Projection
      -- ^ The type of the map projection.
    | PrClipAngle (Maybe Double)
      -- ^ The clipping circle angle in degrees. A value of @Nothing@ will switch to
      --   antimeridian cutting rather than small-circle clipping.
    | PrClipExtent ClipRect
      -- ^ Projection’s viewport clip extent to the specified bounds in pixels.
    | PrCenter Double Double
      -- ^ Projection’s center as longitude and latitude in degrees.
    | PrScale Double
      -- ^ The projection's zoom scale, which if set, overrides automatic scaling of a
      --   geo feature to fit within the viewing area.
      --
      --   @since 0.4.0.0
    | PrTranslate Double Double
      -- ^ A projection’s panning translation, which if set, overrides automatic positioning
      --   of a geo feature to fit within the viewing area
      --
      --   Note that the prefix is @Pr@ and not @P@, to match the Elm API.
      --
      --   @since 0.4.0.0
    | PrRotate Double Double Double
      -- ^ A projection’s three-axis rotation angle. The order is @lambda@ @phi@ @gamma@,
      --   and specifies the rotation angles in degrees about each spherical axis.
    | PrPrecision Double
      -- ^ Threshold for the projection’s adaptive resampling in pixels, and corresponds to the
      --   Douglas–Peucker distance. If precision is not specified, the projection’s current
      --   resampling precision of 0.707 is used.
      --
      --   Version 3.3.0 of the Vega-Lite spec claims this should be output as a string,
      --   but it is written out as a number since the
      --   [spec is in error](https://github.com/vega/vega-lite/issues/5190).
    | PrReflectX Bool
      -- ^ Reflect the x-coordinates after performing an identity projection. This
      --   creates a left-right mirror image of the geoshape marks when subject to an
      --   identity projection with 'Identity'.
      --
      -- @since 0.4.0.0
    | PrReflectY Bool
      -- ^ Reflect the y-coordinates after performing an identity projection. This
      --   creates a left-right mirror image of the geoshape marks when subject to an
      --   identity projection with 'Identity'.
      --
      -- @since 0.4.0.0
    | PrCoefficient Double
      -- ^ The @Hammer@ map projection coefficient.
    | PrDistance Double
      -- ^ The @Satellite@ map projection distance.
    | PrFraction Double
      -- ^ The @Bottomley@ map projection fraction.
    | PrLobes Int
      -- ^ Number of lobes in lobed map projections such as the @Berghaus star@.
    | PrParallel Double
      -- ^ Parallel for map projections such as the @Armadillo@.
    | PrRadius Double
      -- ^ Radius value for map projections such as the @Gingery@.
    | PrRatio Double
      -- ^ Ratio value for map projections such as the @Hill@.
    | PrSpacing Double
      -- ^ Spacing value for map projections such as the @Lagrange@.
    | PrTilt Double
      -- ^ @Satellite@ map projection tilt.


projectionProperty :: ProjectionProperty -> LabelledSpec
projectionProperty (PrType proj) = "type" .= projectionLabel proj
projectionProperty (PrClipAngle numOrNull) = "clipAngle" .= maybe A.Null toJSON numOrNull
projectionProperty (PrClipExtent rClip) =
  ("clipExtent", case rClip of
    NoClip -> A.Null
    LTRB l t r b -> toJSON (map toJSON [l, t, r, b])
  )
projectionProperty (PrCenter lon lat) = "center" .= [lon, lat]
projectionProperty (PrScale sc) = "scale" .= sc
projectionProperty (PrTranslate tx ty) = "translate" .= [tx, ty]
projectionProperty (PrRotate lambda phi gamma) = "rotate" .= [lambda, phi, gamma]
projectionProperty (PrPrecision pr) = "precision" .= pr  -- the 3.3.0 spec says this is a string, but that's wrong,  See https://github.com/vega/vega-lite/issues/5190
projectionProperty (PrReflectX b) = "reflectX" .= b
projectionProperty (PrReflectY b) = "reflectY" .= b
projectionProperty (PrCoefficient x) = "coefficient" .= x
projectionProperty (PrDistance x) = "distance" .= x
projectionProperty (PrFraction x) = "fraction" .= x
projectionProperty (PrLobes n) = "lobes" .= n
projectionProperty (PrParallel x) = "parallel" .= x
projectionProperty (PrRadius x) = "radius" .= x
projectionProperty (PrRatio x) = "ratio" .= x
projectionProperty (PrSpacing x) = "spacing" .= x
projectionProperty (PrTilt x) = "tilt" .= x


{-|

Sets the cartographic projection used for geospatial coordinates. A projection
defines the mapping from @(longitude,latitude)@ to an @(x,y)@ plane used for rendering.
This is useful when using the 'Geoshape' mark. For further details see the
<https://vega.github.io/vega-lite/docs/projection.html Vega-Lite documentation>.

@
'projection' [ 'PrType' 'Orthographic', 'PrRotate' (-40) 0 0 ]
@
-}
projection :: [ProjectionProperty] -> PropertySpec
projection pProps = (VLProjection, object (map projectionProperty pProps))


{-|

Properties for customising the colors of a range. The parameter should be a
named color scheme such as @\"accent\"@ or @\"purpleorange-11\"@. For details see the
<https://vega.github.io/vega/docs/schemes/#scheme-properties Vega-Lite documentation>.
-}
data RangeConfig
    = RCategory T.Text
    | RDiverging T.Text
    | RHeatmap T.Text
    | ROrdinal T.Text
    | RRamp T.Text
    | RSymbol T.Text


rangeConfigProperty :: RangeConfig -> LabelledSpec
rangeConfigProperty rangeCfg =
  let (l, n) = case rangeCfg of
        RCategory nme -> ("category", nme)
        RDiverging nme -> ("diverging", nme)
        RHeatmap nme -> ("heatmap", nme)
        ROrdinal nme -> ("ordinal", nme)
        RRamp nme -> ("ramp", nme)
        RSymbol nme -> ("symbol", nme)

  in l .= object [schemeProperty n []]


{-|

Scale configuration property. These are used to configure all scales.
For more details see the
<https://vega.github.io/vega-lite/docs/scale.html#scale-config Vega-Lite documentation>.

-}

data ScaleConfig
    = SCBandPaddingInner Double
      -- ^ Default inner padding for x and y band-ordinal scales.
    | SCBandPaddingOuter Double
      -- ^ Default outer padding for x and y band-ordinal scales.
    | SCBarBandPaddingInner Double
      -- ^ Default inner padding for x and y band-ordinal scales of 'Bar' marks.
      --
      --   @since 0.4.0.0
    | SCBarBandPaddingOuter Double
      -- ^ Default outer padding for x and y band-ordinal scales of 'Bar' marks.
      --
      --   @since 0.4.0.0
    | SCRectBandPaddingInner Double
      -- ^ Default inner padding for x and y band-ordinal scales of 'Rect' marks.
      --
      --   @since 0.4.0.0
    | SCRectBandPaddingOuter Double
      -- ^ Default outer padding for x and y band-ordinal scales of 'Rect' marks.
      --
      --   @since 0.4.0.0
    | SCClamp Bool
      -- ^ Whether or not by default values that exceed the data domain are clamped to
      --   the min/max range value.
    | SCMaxBandSize Double
      -- ^ Default maximum value for mapping quantitative fields to a bar's
      --   size/bandSize.
    | SCMinBandSize Double
      -- ^ Default minimum value for mapping quantitative fields to a bar's
      --   size/bandSize.
    | SCMaxFontSize Double
      -- ^ Default maximum value for mapping a quantitative field to a text
      --   mark's size.
    | SCMinFontSize Double
      -- ^ Default minimum value for mapping a quantitative field to a text
      --   mark's size.
    | SCMaxOpacity Opacity
      -- ^ Default maximum opacity for mapping a field to opacity.
    | SCMinOpacity Opacity
      -- ^ Default minimum opacity for mapping a field to opacity.
    | SCMaxSize Double
      -- ^ Default maximum size for point-based scales.
    | SCMinSize Double
      -- ^ Default minimum size for point-based scales.
    | SCMaxStrokeWidth Double
      -- ^ Default maximum stroke width for rule, line and trail marks.
    | SCMinStrokeWidth Double
      -- ^ Default minimum stroke width for rule, line and trail marks.
    | SCPointPadding Double
      -- ^ Default padding for point-ordinal scales.
    | SCRangeStep (Maybe Double)
      -- ^ Default range step for band and point scales when the mark is not text.
    | SCRound Bool
      -- ^ Are numeric values are rounded to integers when scaling? Useful
      --   for snapping to the pixel grid.
    | SCTextXRangeStep Double
      -- ^ Default range step for x band and point scales of text marks.
    | SCUseUnaggregatedDomain Bool
      -- ^ Whether or not to use the source data range before aggregation.


scaleConfigProperty :: ScaleConfig -> LabelledSpec
scaleConfigProperty (SCBandPaddingInner x) = "bandPaddingInner" .= x
scaleConfigProperty (SCBandPaddingOuter x) = "bandPaddingOuter" .= x
scaleConfigProperty (SCBarBandPaddingInner x) = "barBandPaddingInner" .= x
scaleConfigProperty (SCBarBandPaddingOuter x) = "barBandPaddingOuter" .= x
scaleConfigProperty (SCRectBandPaddingInner x) = "rectBandPaddingInner" .= x
scaleConfigProperty (SCRectBandPaddingOuter x) = "rectBandPaddingOuter" .= x
scaleConfigProperty (SCClamp b) = "clamp" .= b
scaleConfigProperty (SCMaxBandSize x) = "maxBandSize" .= x
scaleConfigProperty (SCMinBandSize x) = "minBandSize" .= x
scaleConfigProperty (SCMaxFontSize x) = "maxFontSize" .= x
scaleConfigProperty (SCMinFontSize x) = "minFontSize" .= x
scaleConfigProperty (SCMaxOpacity x) = "maxOpacity" .= x
scaleConfigProperty (SCMinOpacity x) = "minOpacity" .= x
scaleConfigProperty (SCMaxSize x) = "maxSize" .= x
scaleConfigProperty (SCMinSize x) = "minSize" .= x
scaleConfigProperty (SCMaxStrokeWidth x) = "maxStrokeWidth" .= x
scaleConfigProperty (SCMinStrokeWidth x) = "minStrokeWidth" .= x
scaleConfigProperty (SCPointPadding x) = "pointPadding" .= x
scaleConfigProperty (SCRangeStep numOrNull) = "rangeStep" .= maybe A.Null toJSON numOrNull
scaleConfigProperty (SCRound b) = "round" .= b
scaleConfigProperty (SCTextXRangeStep x) = "textXRangeStep" .= x
scaleConfigProperty (SCUseUnaggregatedDomain b) = "useUnaggregatedDomain" .= b


-- | Indicates the type of selection to be generated by the user.

data Selection
    = Single
      -- ^ Allows one mark at a time to be selected.
    | Multi
      -- ^ Allows multiple items to be selected (e.g. with
      --   shift-click).
    | Interval
      -- ^ Allows a bounding rectangle to be dragged by the user,
      --   selecting all items which intersect it.


selectionLabel :: Selection -> T.Text
selectionLabel Single = "single"
selectionLabel Multi = "multi"
selectionLabel Interval = "interval"


{-|

Properties for customising the nature of the selection. See the
<https://vega.github.io/vega-lite/docs/selection.html#selection-properties Vega-Lite documentation>
for details.

For use with 'select' and 'SelectionStyle'.
-}
data SelectionProperty
    = Empty
      -- ^ Make a selection empty by default when nothing selected.
    | BindScales
      -- ^ Enable two-way binding between a selection and the scales used
      --   in the same view. This is commonly used for zooming and panning
      --   by binding selection to position scaling:
      --
      --   @sel = 'selection' . 'select' \"mySelection\" 'Interval' ['BindScales']@
    | On T.Text
      -- ^ [Vega event stream selector](https://vega.github.io/vega/docs/event-streams/#selector)
      --   that triggers a selection, or the empty string (which sets the property to @false@).
    | Clear T.Text
      -- ^ [Vega event stream selector](https://vega.github.io/vega/docs/event-streams/#selector)
      --   that can clear a selection. For example, to allow a zoomed/panned view to be reset
      --   on shift-click:
      --
      -- @
      -- 'selection'
      --     . 'select' \"myZoomPan\"
      --         'Interval'
      --         ['BindScales', 'Clear' \"click[event.shiftKey]\"]
      -- @
      --
      --   To remove the default clearing behaviour of a selection, provide an empty string
      --   rather than an event stream selector.
      --
      --   @since 0.4.0.0
    | Translate T.Text
      -- ^ Translation selection transformation used for panning a view. See the
      --   [Vega-Lite translate documentation](https://vega.github.io/vega-lite/docs/translate.html).
    | Zoom T.Text
      -- ^ Zooming selection transformation used for zooming a view. See the
      --   [Vega-Lite zoom documentation](https://vega.github.io/vega-lite/docs/zoom.html).
    | Fields [T.Text]
      -- ^ Field names for projecting a selection.
    | Encodings [Channel]
      -- ^ Encoding channels that form a named selection.
      --
      --   For example, to __project__ a selection across all items that
      --   share the same value in the color channel:
      --
      --   @sel = 'selection' . 'select' \"mySelection\" 'Multi' ['Encodings' ['ChColor']]@
    | SInit [(T.Text, DataValue)]
      -- ^ Initialise one or more selections with values from bound fields. For example,
      --
      -- @
      -- 'selection'
      --     . 'select' \"CylYr\"
      --         'Single'
      --         [ 'Fields' [\"Cylinders\", \"Year\"]
      --         , 'SInit'
      --             [ (\"Cylinders\", 'Number' 4)
      --             , (\"Year\", 'Number' 1977)
      --             ]
      --         , 'Bind'
      --             [ 'IRange' \"Cylinders\" ['InMin' 3, 'InMax' 8, 'InStep' 1]
      --             , 'IRange' \"Year\" ['InMin' 1969, 'InMax' 1981, 'InStep' 1]
      --             ]
      --         ]
      -- @
      --
      --   @since 0.4.0.0
    | ResolveSelections SelectionResolution
      -- ^ Strategy that determines how selections' data queries are resolved when applied
      --   in a filter transform, conditional encoding rule, or scale domain.
    | SelectionMark [SelectionMarkProperty]
      -- ^ Appearance of an interval selection mark (dragged rectangle).
    | Bind [Binding]
      -- ^ Binding to some input elements as part of a named selection.
      --
      --   The followig example allows a selection to be based on a
      --   drop-down list of options:
      --
      --   @
      --   sel = 'selection'
      --           . 'select' \"mySelection\"
      --               'Single'
      --               ['Fields' [\"crimeType\"]
      --               , 'Bind' ['ISelect' \"crimeType\"
      --                         ['InOptions'
      --                            [ \"Anti-social behaviour\"
      --                            , \"Criminal damage and arson\"
      --                            , \"Drugs\"
      --                            , \"Robbery\"
      --                            , \"Vehicle crime\"
      --                            ]
      --                         ]
      --                      ]
      --               ]
      --   @
    | Nearest Bool
      -- ^ Whether or not a selection should capture nearest marks to a pointer
      --   rather than an exact position match.
    | Toggle T.Text
      -- ^ Predicate expression that determines a toggled selection. See the
      --   [Vega-Lite toggle documentation](https://vega.github.io/vega-lite/docs/toggle.html).


selectionProperty :: SelectionProperty -> LabelledSpec
selectionProperty (Fields fNames) = "fields" .= map toJSON fNames
selectionProperty (Encodings channels) = "encodings" .= map (toJSON . channelLabel) channels
selectionProperty (SInit iVals) = "init" .= object (map (second dataValueSpec) iVals)
selectionProperty (On e) = "on" .= e
selectionProperty (Clear e) = "clear" .= if T.null e then toJSON False else toJSON e
selectionProperty Empty = "empty" .= fromT "none"
selectionProperty (ResolveSelections res) = "resolve" .= selectionResolutionLabel res
selectionProperty (SelectionMark markProps) = "mark" .= object (map selectionMarkProperty markProps)
selectionProperty BindScales = "bind" .= fromT "scales"
selectionProperty (Bind binds) = "bind" .= object (map bindingSpec binds)
selectionProperty (Nearest b) = "nearest" .= b
selectionProperty (Toggle expr) = "toggle" .= expr
selectionProperty (Translate e) = "translate" .= if T.null e then toJSON False else toJSON e
selectionProperty (Zoom e) = "zoom" .= if T.null e then toJSON False else toJSON e


-- | Indicates a channel type to be used in a resolution specification.

-- assuming this is based on schema 3.3.0 #/definitions/SingleDefUnitChannel

data Channel
    = ChX
    | ChY
    | ChX2
    | ChY2
    | ChLongitude
      -- ^ @since 0.4.0.0
    | ChLongitude2
      -- ^ @since 0.4.0.0
    | ChLatitude
      -- ^ @since 0.4.0.0
    | ChLatitude2
      -- ^ @since 0.4.0.0
    | ChColor
    | ChFill
      -- ^ @since 0.3.0.0
    | ChFillOpacity
      -- ^ @since 0.4.0.0
    | ChHref
      -- ^ @since 0.4.0.0
    | ChKey
      -- ^ @since 0.4.0.0
    | ChStroke
      -- ^ @since 0.3.0.0
    | ChStrokeOpacity
      -- ^ @since 0.4.0.0
    | ChStrokeWidth
      -- ^ @since 0.4.0.0
    | ChOpacity
    | ChShape
    | ChSize
    | ChText
      -- ^ @since 0.4.0.0
    | ChTooltip
      -- ^ @since 0.4.0.0


channelLabel :: Channel -> T.Text
channelLabel ChX = "x"
channelLabel ChY = "y"
channelLabel ChX2 = "x2"
channelLabel ChY2 = "y2"
channelLabel ChLongitude = "longitude"
channelLabel ChLatitude = "latitude"
channelLabel ChLongitude2 = "longitude2"
channelLabel ChLatitude2 = "latitude2"
channelLabel ChColor = "color"
channelLabel ChFill = "fill"
channelLabel ChStroke = "stroke"
channelLabel ChStrokeWidth = "strokeWidth"
channelLabel ChShape = "shape"
channelLabel ChSize = "size"
channelLabel ChFillOpacity = "fillOpacity"
channelLabel ChStrokeOpacity = "strokeOpacity"
channelLabel ChOpacity = "opacity"
channelLabel ChText = "text"
channelLabel ChTooltip = "tooltip"
channelLabel ChHref = "href"
channelLabel ChKey = "key"


{-|

Determines how selections in faceted or repeated views are resolved. See the
<https://vega.github.io/vega-lite/docs/selection.html#resolve Vega-Lite documentation>
for details.

For use with 'ResolveSelections'.

-}
data SelectionResolution
    = Global
      -- ^ One selection available across all subviews (default).
    | Union
      -- ^ Each subview contains its own brush and marks are selected if they lie
      --   within /any/ of these individual selections.
    | Intersection
      -- ^  Each subview contains its own brush and marks are selected if they lie
      --    within /all/ of these individual selections.


selectionResolutionLabel :: SelectionResolution -> T.Text
selectionResolutionLabel Global = "global"
selectionResolutionLabel Union = "union"
selectionResolutionLabel Intersection = "intersect"


{-|

Properties for customising the appearance of an interval selection mark (dragged
rectangle). For details see the
<https://vega.github.io/vega-lite/docs/selection.html#interval-mark Vega-Lite documentation>.

-}
data SelectionMarkProperty
    = SMFill T.Text
    | SMFillOpacity Opacity
    | SMStroke T.Text
    | SMStrokeOpacity Opacity
    | SMStrokeWidth Double
    | SMStrokeDash [Double]
    | SMStrokeDashOffset Double


selectionMarkProperty :: SelectionMarkProperty -> LabelledSpec
selectionMarkProperty (SMFill colour) = "fill" .= colour
selectionMarkProperty (SMFillOpacity x) = "fillOpacity" .= x
selectionMarkProperty (SMStroke colour) = "stroke" .= colour
selectionMarkProperty (SMStrokeOpacity x) = "strokeOpacity" .= x
selectionMarkProperty (SMStrokeWidth x) = "strokeWidth" .= x
selectionMarkProperty (SMStrokeDash xs) = "strokeDash" .= xs
selectionMarkProperty (SMStrokeDashOffset x) = "strokeDashOffset" .= x


{-|

GUI Input properties. The type of relevant property will depend on the type of
input element selected. For example an @InRange@ (slider) can have numeric min,
max and step values; @InSelect@ (selector) has a list of selection label options.
For details see the
<https://vega.github.io/vega/docs/signals/#bind Vega input element binding documentation>.

The @debounce@ property, available for all input types allows a delay in input event
handling to be added in order to avoid unnecessary event broadcasting. The @Element@
property is an optional CSS selector indicating the parent element to which the
input element should be added. This allows the option of the input element to be
outside the visualization container.
-}

-- based on schema 3.3.0 #/definitions/BindRange
--       or              #/definitions/InputBinding

-- placeholder is in InputBinding
-- debounce is in BindCheckbox / BindRadioSelect / BindRange / InputBinding
-- element is in BindCheckbox / BindRadioSelect / BindRange / InputBinding

-- but InputBinding doesn't have min/max/others

data InputProperty
    = Debounce Double
    | Element T.Text
    | InOptions [T.Text]
    | InMin Double
    | InMax Double
    | InName T.Text
    | InStep Double
    | InPlaceholder T.Text


inputProperty :: InputProperty -> LabelledSpec
inputProperty (Debounce x) = "debounce" .= x
inputProperty (Element el) = "element" .= el -- #/definitions/Element
inputProperty (InOptions opts) = "options" .= map toJSON opts
inputProperty (InMin x) = "min" .= x
inputProperty (InMax x) = "max" .= x
inputProperty (InName s) = "name" .= s
inputProperty (InStep x) = "step" .= x
inputProperty (InPlaceholder el) = "placeholder" .= toJSON el


{-|

Describes the binding property of a selection based on some HTML input element
such as a checkbox or radio button. For details see the
<https://vega.github.io/vega-lite/docs/bind.html#scale-binding Vega-Lite documentation>
and the
<https://vega.github.io/vega/docs/signals/#bind Vega input binding documentation>.
-}
data Binding
    = IRange T.Text [InputProperty]
      -- ^ Range slider input element that can bound to a named field value.
    | ICheckbox T.Text [InputProperty]
      -- ^ Checkbox input element that can bound to a named field value.
    | IRadio T.Text [InputProperty]
      -- ^ Radio box input element that can bound to a named field value.
    | ISelect T.Text [InputProperty]
      -- ^ Select input element that can bound to a named field value.
    | IText T.Text [InputProperty]
      -- ^ Text input element that can bound to a named field value.
    | INumber T.Text [InputProperty]
      -- ^ Number input element that can bound to a named field value.
    | IDate T.Text [InputProperty]
      -- ^ Date input element that can bound to a named field value.
    | ITime T.Text [InputProperty]
      -- ^ Time input element that can bound to a named field value.
    | IMonth T.Text [InputProperty]
      -- ^ Month input element that can bound to a named field value.
    | IWeek T.Text [InputProperty]
      -- ^ Week input element that can bound to a named field value.
    | IDateTimeLocal T.Text [InputProperty]
      -- ^ Local time input element that can bound to a named field value.
    | ITel T.Text [InputProperty]
      -- ^ Telephone number input element that can bound to a named field value.
    | IColor T.Text [InputProperty]
      -- ^ Color input element that can bound to a named field value.


bindingSpec :: Binding -> LabelledSpec
bindingSpec bnd =
  let (lbl, input, ps) = case bnd of
        IRange label props -> (label, fromT "range", props)
        ICheckbox label props -> (label, "checkbox", props)
        IRadio label props -> (label, "radio", props)
        ISelect label props -> (label, "select", props)
        IText label props -> (label, "text", props)
        INumber label props -> (label, "number", props)
        IDate label props -> (label, "date", props)
        ITime label props -> (label, "time", props)
        IMonth label props -> (label, "month", props)
        IWeek label props -> (label, "week", props)
        IDateTimeLocal label props -> (label, "datetimelocal", props)
        ITel label props -> (label, "tel", props)
        IColor label props -> (label, "color", props)

  in (lbl, object (("input" .= input) : map inputProperty ps))


-- | Indicates the anchor position for text.

data APosition
    = AStart
      -- ^ The start of the text.
    | AMiddle
      -- ^ The middle of the text.
    | AEnd
      -- ^ The end of the text.


anchorLabel :: APosition -> T.Text
anchorLabel AStart = "start"
anchorLabel AMiddle = "middle"
anchorLabel AEnd = "end"


{-|

Title configuration properties. These are used to configure the default style
of all titles within a visualization.
For further details see the
<https://vega.github.io/vega-lite/docs/title.html#config Vega-Lite documentation>.
-}
data TitleConfig
    = TAnchor APosition
      -- ^ Default anchor position when placing titles.
    | TAngle Angle
      -- ^ Default angle when orientating titles.
    | TBaseline VAlign
      -- ^ Default vertical alignment when placing titles.
    | TColor Color
      -- ^ Default color when showing titles.
    | TFont T.Text
      -- ^ Default font when showing titles.
    | TFontSize Double
      -- ^ Default font size when showing titles.
    | TFontStyle T.Text
      -- ^ Defaylt font style when showing titles.
      --
      --   @since 0.4.0.0
    | TFontWeight FontWeight
      -- ^ Default font weight when showing titles.
    | TFrame TitleFrame
      -- ^ Default title position anchor.
      --
      --   @since 0.4.0.0
    | TLimit Double
      -- ^ Default maximum length, in pixels, of titles.
    | TOffset Double
      -- ^ Default offset, in pixels, of titles relative to the chart body.
    | TOrient Side
      -- ^ Default placement of titles relative to the chart body.
    | TStyle [T.Text]
      -- ^ A list of named styles to apply. A named style can be specified
      --   via 'NamedStyle' or 'NamedStyles'. Later styles in the list will
      --   override earlier ones if there is a conflict in any of the
      --   properties.
      --
      --   @since 0.4.0.0
    | TZIndex ZIndex
      -- ^ Drawing order of a title relative to the other chart elements.
      --
      --   @since 0.4.0.0

titleConfigSpec :: TitleConfig -> LabelledSpec
titleConfigSpec (TAnchor an) = "anchor" .= anchorLabel an
titleConfigSpec (TAngle x) = "angle" .= x
titleConfigSpec (TBaseline va) = "baseline" .= vAlignLabel va
titleConfigSpec (TColor clr) = "color" .= clr
titleConfigSpec (TFont fnt) = "font" .= fnt
titleConfigSpec (TFontSize x) = "fontSize" .= x
titleConfigSpec (TFontStyle s) = "fontStyle" .= s
titleConfigSpec (TFontWeight w) = "fontWeight" .= fontWeightSpec w
titleConfigSpec (TFrame tf) = "frame" .= titleFrameSpec tf
titleConfigSpec (TLimit x) = "limit" .= x
titleConfigSpec (TOffset x) = "offset" .= x
titleConfigSpec (TOrient sd) = "orient" .= sideLabel sd
titleConfigSpec (TStyle [style]) = "style" .= style  -- not really needed
titleConfigSpec (TStyle styles) = "style" .= styles
titleConfigSpec (TZIndex z) = "zindex" .= z

-- | Specifies how the title anchor is positioned relative to the frame.
--
--   @since 0.4.0.0
data TitleFrame
    = FrBounds
      -- ^ The position is relative to the full bounding box.
    | FrGroup
      -- ^ The pistion is relative to the group width / height.

titleFrameSpec :: TitleFrame -> VLSpec
titleFrameSpec FrBounds = "bounds"
titleFrameSpec FrGroup = "group"


-- | The properties for a single view or layer background.
--
--   @since 0.4.0.0

data ViewBackground
    = VBStyle [T.Text]
    -- ^ A list of named styles to apply. A named style can be specified
    --   via 'NamedStyle' or 'NamedStyles'. Later styles in the list will
    --   override earlier ones if there is a conflict in any of the mark
    --   properties.
    | VBCornerRadius Double
    -- ^ The radius in pixels of rounded corners.
    | VBFill (Maybe T.Text)
    -- ^ Fill color.
    | VBFillOpacity Opacity
    -- ^ Fill opacity.
    | VBOpacity Opacity
    -- ^ Overall opacity.
    | VBStroke (Maybe T.Text)
    -- ^ The stroke color for a line around the background. If @Nothing@ then
    --   no line is drawn.
    | VBStrokeOpacity Opacity
    -- ^ The opacity of the line around the background, if drawn.
    | VBStrokeWidth Double
    -- ^ The width of the line around the background, if drawn.
    | VBStrokeCap StrokeCap
    -- ^ The cap line-ending for the line around the background, if drawn.
    | VBStrokeDash [Double]
    -- ^ The dash style of the line around the background, if drawn.
    | VBStrokeDashOffset Double
    -- ^ The dash offset of the line around the background, if drawn.
    | VBStrokeJoin StrokeJoin
    -- ^ The line-joining style of the line around the background, if drawn.
    | VBStrokeMiterLimit Double
    -- ^ The mitre limit at which to bevel the line around the background, if drawn.


viewBackgroundSpec :: ViewBackground -> LabelledSpec
viewBackgroundSpec (VBStyle [style]) = "style" .= style  -- special case singleton
viewBackgroundSpec (VBStyle styles) = "style" .= styles
viewBackgroundSpec (VBCornerRadius r) = "cornerRadius" .= r
viewBackgroundSpec (VBFill (Just s)) = "fill" .= s
viewBackgroundSpec (VBFill Nothing) = "fill" .= A.Null
viewBackgroundSpec (VBFillOpacity x) = "fillOpacity" .= x
viewBackgroundSpec (VBOpacity x) = "opacity" .= x
viewBackgroundSpec (VBStroke (Just s)) = "stroke" .= s
viewBackgroundSpec (VBStroke Nothing) = "stroke" .= A.Null
viewBackgroundSpec (VBStrokeOpacity x) = "strokeOpacity" .= x
viewBackgroundSpec (VBStrokeCap cap) = "strokeCap" .= strokeCapLabel cap
viewBackgroundSpec (VBStrokeJoin jn) = "strokeJoin" .= strokeJoinLabel jn
viewBackgroundSpec (VBStrokeWidth x) = "strokeWidth" .= x
viewBackgroundSpec (VBStrokeDash xs) = "strokeDash" .= xs
viewBackgroundSpec (VBStrokeDashOffset x) = "strokeDashOffset" .= x
viewBackgroundSpec (VBStrokeMiterLimit x) = "strokeMiterLimit" .= x


-- | The background style of a single view or layer in a view composition.
--
--   @since 0.4.0.0

viewBackground :: [ViewBackground] -> PropertySpec
viewBackground vbs = (VLViewBackground, object (map viewBackgroundSpec vbs))


{-|

View configuration property. These are used to configure the style of a single
view within a visualization such as its size and default fill and stroke colors.
For further details see the
<https://vega.github.io/vega-lite/docs/spec.html#config Vega-Lite documentation>.

This type has been changed in the @0.4.0.0@ release to use a consistent
naming scheme for the constructors (everything starts with @View@). Prior to
this release only @ViewWidth@ and @ViewHeight@ were named this way. There
are also five new constructors.

-}

-- based on schema 3.3.0 #/definitions/ViewConfig

data ViewConfig
    = ViewWidth Double
      -- ^ The default width of the single plot or each plot in a trellis plot when the
      --   visualization has a continuous (non-ordinal) scale or when the
      --   'SRangeStep'/'ScRangeStep' is @Nothing@ for an ordinal scale (x axis).
    | ViewHeight Double
      -- ^ The default height of the single plot or each plot in a trellis plot when the
      --   visualization has a continuous (non-ordinal) scale or when the
      --   'SRangeStep'/'ScRangeStep' is @Nothing@ for an ordinal scale (y axis).
    | ViewClip Bool
      -- ^ Should the view be clipped?
    | ViewCornerRadius Double
      -- ^ The radius, in pixels, of rounded rectangle corners.
      --
      --   The default is @0@.
      --
      --   @since 0.4.0.0
    | ViewFill (Maybe T.Text)
      -- ^ The fill color.
    | ViewFillOpacity Opacity
      -- ^ The fill opacity.
    | ViewOpacity Opacity
      -- ^ The overall opacity.
      --
      --   The default is @0.7@ for non-aggregate plots with 'Point', 'Tick',
      --   'Circle', or 'Square' marks or layered 'Bar' charts, and @1@
      --   otherwise.
      --
      --   @since 0.4.0.0
    | ViewStroke (Maybe T.Text)
      -- ^ The stroke color.
    | ViewStrokeCap StrokeCap
      -- ^ The stroke cap for line-ending style.
      --
      --   @since 0.4.0.0
    | ViewStrokeDash [Double]
      -- ^ The stroke dash style. It is defined by an alternating 'on-off'
      --   sequence of line lengths, in pixels.
    | ViewStrokeDashOffset Double
      -- ^ Number of pixels before the first line dash is drawn.
    | ViewStrokeJoin StrokeJoin
      -- ^ The stroke line-join method.
      --
      --   @since 0.4.0.0
    | ViewStrokeMiterLimit Double
      -- ^ The miter limit at which to bevel a line join.
      --
      --   @since 0.4.0.0
    | ViewStrokeOpacity Opacity
      -- ^ The stroke opacity.
    | ViewStrokeWidth Double
      -- ^ The stroke width, in pixels.


viewConfigProperty :: ViewConfig -> LabelledSpec
viewConfigProperty (ViewWidth x) = "width" .= x
viewConfigProperty (ViewHeight x) = "height" .= x
viewConfigProperty (ViewClip b) = "clip" .= b
viewConfigProperty (ViewCornerRadius x) = "cornerRadius" .= x
viewConfigProperty (ViewFill ms) = "fill" .= maybe A.Null toJSON ms
viewConfigProperty (ViewFillOpacity x) = "fillOpacity" .= x
viewConfigProperty (ViewOpacity x) = "opacity" .= x
viewConfigProperty (ViewStroke ms) = "stroke" .= maybe A.Null toJSON ms
viewConfigProperty (ViewStrokeCap sc) = "strokeCap" .= strokeCapLabel sc
viewConfigProperty (ViewStrokeDash xs) = "strokeDash" .= xs
viewConfigProperty (ViewStrokeDashOffset x) = "strokeDashOffset" .= x
viewConfigProperty (ViewStrokeJoin sj) = "strokeJoin" .= strokeJoinLabel sj
viewConfigProperty (ViewStrokeMiterLimit x) = "strokeMiterLimit" .= x
viewConfigProperty (ViewStrokeOpacity x) = "strokeOpacity" .= x
viewConfigProperty (ViewStrokeWidth x) = "strokeWidth" .= x


{-|

Type of configuration property to customise. See the
<https://vega.github.io/vega-lite/docs/config.html Vega-Lite documentation>
for details.

-}

-- based on schema 3.3.0 #/definitions/Config
--
-- TODO:
--   Bar - change to BarConfig rather than MarkProperty?
--     BoxplotStyle BoxPlotConfig
--     Concat CompositionConfig
--     ErrorBand ErrorBandCOnfig
--     ErrorBar ErrorBarCOnfig
--   Facet takes CompositionConfig not FacetConfig
--     HeaderColumn takes HeaderConfig, just as HeaderStyle does
--     HeaderFacet ditto
--     HeaderRow ditto
--   LineStyle takes LineConfig not MarkConfig
--   TextStyle takes TextConfig not MarkConfig
--   TickStyle takes TickConfig not MarkConfig
--   TrailStyle takes LineConfig not MarkConfig
--

data ConfigurationProperty
    = AreaStyle [MarkProperty]
      -- ^ The default appearance of area marks.
    | Autosize [Autosize]
      -- ^ The default sizing of visualizations.
    | Axis [AxisConfig]
      -- ^ The default appearance of axes.
    | AxisBand [AxisConfig]
      -- ^ The default appearance of axes with band scaling.
    | AxisBottom [AxisConfig]
      -- ^ The default appearance of the bottom-side axes.
    | AxisLeft [AxisConfig]
      -- ^ The default appearance of the left-side axes.
    | AxisRight [AxisConfig]
      -- ^ The default appearance of the right-side axes.
    | AxisTop [AxisConfig]
      -- ^ The default appearance of the top-side axes.
    | AxisX [AxisConfig]
      -- ^ The default appearance of the X axes.
    | AxisY [AxisConfig]
      -- ^ The default appearance of the Y axes.
    | Background T.Text
      -- ^ The default background color of visualizations.
    | BarStyle [MarkProperty]
      -- ^ The default appearance of bar marks.
    | CircleStyle [MarkProperty]
      -- ^ The default appearance of circle marks.
    | ConcatStyle [ConcatConfig]
      -- ^ The default appearance of concatenated layouts.
      --
      --   @since 0.4.0.0
    | CountTitle T.Text
      -- ^ The default title style for count fields.
    | FacetStyle [FacetConfig]
      -- ^ The default appearance of facet layouts.
      --
      --   @since 0.4.0.0
    | FieldTitle FieldTitleProperty
      -- ^ The default title-generation style for fields.
    | GeoshapeStyle [MarkProperty]
      -- ^ The default appearance of geoshape marks.
      --
      --   @since 0.4.0.0
    | HeaderStyle [HeaderProperty]
      -- ^ The default appearance of facet headers.
      --
      --   @since 0.4.0.0
    | Legend [LegendConfig]
      -- ^ The default appearance of legends.
    | LineStyle [MarkProperty]
      -- ^ The default appearance of line marks.
    | MarkStyle [MarkProperty]
      -- ^ The default mark appearance.
    | NamedStyle T.Text [MarkProperty]
      -- ^ The default appearance of a single named style.
    | NamedStyles [(T.Text, [MarkProperty])]
      -- ^ The default appearance of a list of named styles.
      --
      --   @since 0.4.0.0
    | NumberFormat T.Text
      -- ^ The default number formatting for axis and text labels.
    | Padding Padding
      -- ^ The default padding in pixels from the edge of the of visualization
      --   to the data rectangle.
    | PointStyle [MarkProperty]
      -- ^ The default appearance of point marks.
    | Projection [ProjectionProperty]
      -- ^ The default style of map projections.
    | Range [RangeConfig]
      -- ^ The default range properties used when scaling.
    | RectStyle [MarkProperty]
      -- ^ The default appearance of rectangle marks.
    | RemoveInvalid Bool
      -- ^ The default handling of invalid (@null@ and @NaN@) values. If @True@,
      --   invalid values are skipped or filtered out when represented as marks.
    | RuleStyle [MarkProperty]
      -- ^ The default appearance of rule marks.
    | Scale [ScaleConfig]   -- TODO: rename ScaleStyle
      -- ^ The default properties used when scaling.
    | SelectionStyle [(Selection, [SelectionProperty])]
      -- ^ The default appearance of selection marks.
    | SquareStyle [MarkProperty]
      -- ^  the default appearance of square marks
    | Stack StackOffset
      -- ^ The default stack offset style for stackable marks.
      --
      --   Changed from @StackProperty@ in version @0.4.0.0@.
    | TextStyle [MarkProperty]
      -- ^ The default appearance of text marks.
    | TickStyle [MarkProperty]
      -- ^ The default appearance of tick marks.
    | TimeFormat T.Text
      -- ^ The default time format for axis and legend labels.
    | TitleStyle [TitleConfig]
      -- ^ The default appearance of visualization titles.
    | TrailStyle [MarkProperty]
      -- ^ The default style of trail marks.
      --
      --   @since 0.4.0.0
    | View [ViewConfig]
      -- ^ The default single view style.

configProperty :: ConfigurationProperty -> LabelledSpec
configProperty (Autosize aus) = "autosize" .= object (map autosizeProperty aus)
configProperty (Background bg) = "background" .= bg
configProperty (CountTitle ttl) = "countTitle" .= ttl
configProperty (ConcatStyle cps) = "concat" .= object (map concatConfigProperty cps)
configProperty (FieldTitle ftp) = "fieldTitle" .= fieldTitleLabel ftp
configProperty (RemoveInvalid b) = "invalidValues" .= if b then "filter" else A.Null
configProperty (NumberFormat fmt) = "numberFormat" .= fmt
configProperty (Padding pad) = "padding" .= paddingSpec pad
configProperty (TimeFormat fmt) = "timeFormat" .= fmt
configProperty (Axis acs) = "axis" .= object (map axisConfigProperty acs)
configProperty (AxisX acs) = "axisX" .= object (map axisConfigProperty acs)
configProperty (AxisY acs) = "axisY" .= object (map axisConfigProperty acs)
configProperty (AxisLeft acs) = "axisLeft" .= object (map axisConfigProperty acs)
configProperty (AxisRight acs) = "axisRight" .= object (map axisConfigProperty acs)
configProperty (AxisTop acs) = "axisTop" .= object (map axisConfigProperty acs)
configProperty (AxisBottom acs) = "axisBottom" .= object (map axisConfigProperty acs)
configProperty (AxisBand acs) = "axisBand" .= object (map axisConfigProperty acs)
configProperty (Legend lcs) = "legend" .= object (map legendConfigProperty lcs)
configProperty (MarkStyle mps) = mprops_ "mark" mps
configProperty (Projection pps) = "projection" .= object (map projectionProperty pps)
configProperty (AreaStyle mps) = mprops_ "area" mps
configProperty (BarStyle mps) = mprops_ "bar" mps
configProperty (CircleStyle mps) = mprops_ "circle" mps
configProperty (FacetStyle fps) = "facet" .= object (map facetConfigProperty fps)
configProperty (GeoshapeStyle mps) = mprops_ "geoshape" mps
configProperty (HeaderStyle hps) = header_ hps
configProperty (LineStyle mps) = mprops_ "line" mps
configProperty (PointStyle mps) = mprops_ "point" mps
configProperty (RectStyle mps) = mprops_ "rect" mps
configProperty (RuleStyle mps) = mprops_ "rule" mps
configProperty (SquareStyle mps) = mprops_ "square" mps
configProperty (TextStyle mps) = mprops_ "text" mps
configProperty (TickStyle mps) = mprops_ "tick" mps
configProperty (TitleStyle tcs) = "title" .= object (map titleConfigSpec tcs)
configProperty (NamedStyle nme mps) = "style" .= object [mprops_ nme mps]
configProperty (NamedStyles styles) =
  let toStyle = uncurry mprops_
  in "style" .= object (map toStyle styles)
configProperty (Scale scs) = scaleConfig_ scs
configProperty (Stack so) = stackOffset so
configProperty (Range rcs) = "range" .= object (map rangeConfigProperty rcs)
configProperty (SelectionStyle selConfig) =
  let selProp (sel, sps) = selectionLabel sel .= object (map selectionProperty sps)
  in "selection" .= object (map selProp selConfig)
configProperty (TrailStyle mps) = mprops_ "trail" mps
configProperty (View vcs) = "view" .= object (map viewConfigProperty vcs)


{-|

Axis configuration options for customising all axes. See the
<https://vega.github.io/vega-lite/docs/axis.html#general-config Vega-Lite documentation>
for more details.

The @TitleMaxLength@ constructor was removed in release @0.4.0.0@. The
@TitleLimit@ constructor should be used instead.

-}
data AxisConfig
    = BandPosition Double
      -- ^ The default axis band position.
    | Domain Bool
      -- ^ Should the axis domain be displayed?
    | DomainColor Color
      -- ^ The axis domain color.
    | DomainDash [Double]
      -- ^ The dash style of the domain (alternating stroke, space lengths
      --   in pixels).
      --
      --   @since 0.4.0.0
    | DomainDashOffset Double
      -- ^ The pixel offset at which to start drawing the domain dash array.
      --
      --   @since 0.4.0.0
    | DomainOpacity Opacity
      -- ^ The axis domain opacity.
      --
      --   @since 0.4.0.0
    | DomainWidth Double
      -- ^ The width of the axis domain.
    | Grid Bool
      -- ^ Should an axis grid be displayed?
    | GridColor Color
      -- ^ The color for the grid.
    | GridDash [Double]
      -- ^ The dash style of the grid (alternating stroke, space lengths
      --   in pixels).
    | GridDashOffset Double
      -- ^ The pixel offset at which to start drawing the grid dash array.
      --
      --   @since 0.4.0.0
    | GridOpacity Opacity
      -- ^ The opacity of the grid.
    | GridWidth Double
      -- ^ The width of the grid lines.
    | Labels Bool
      -- ^ Should labels be added to an axis?
    | LabelAlign HAlign
      -- ^ The horizontal alignment for labels.
      --
      --   @since 0.4.0.0
    | LabelAngle Angle
      -- ^ The angle at which to draw labels.
    | LabelBaseline VAlign
      -- ^ The vertical alignment for labels.
      --
      --   @since 0.4.0.0
    | LabelBound (Maybe Double)  -- XXXXX don't like Maybe Double here
      -- ^ Should labels be hidden if they exceed the axis range? If @Nothing@
      --   then no check is made, otherwise it gives the maximum number of
      --   pixels by which the label bounding box can extend beyond the axis.
      --
      --   @since 0.4.0.0
    | LabelColor Color
      -- ^ The label color.
    | LabelFlush (Maybe Double)   -- XXXXX as with labelbound
      -- ^ The label alignment at the start or end of the axis. If
      --   @Nothing@ then no adjustment is made. A value of @Just 1@ means that the
      --   labels will be left- and right- aligned for the first and last
      --   label (horizontal axis), or bottom and top text baselines are
      --   aligned for a vertical axis. Other numeric values indicate additonal
      --   space added, in pixels, which can someties help the labels better visually
      --   group with the corresponding tick marks.
      --
      --   @since 0.4.0.0
    | LabelFlushOffset Double
      -- ^ The number of pixels to offset flush-adjusted labels.
      --
      --   @since 0.4.0.0
    | LabelFont T.Text
      -- ^ The font for the label.
    | LabelFontSize Double
      -- ^ The font size of the label.
    | LabelFontStyle T.Text
      -- ^ The font style of the label.
      --
      --   @since 0.4.0.0
    | LabelFontWeight FontWeight
      -- ^ The font weight of the label.
      --
      --   @since 0.4.0.0
    | LabelLimit Double
      -- ^ The maximum width of a label, in pixels.
    | LabelOpacity Opacity
      -- ^ The opacity of the label.
      --
      --   @since 0.4.0.0
    | LabelOverlap OverlapStrategy
      -- ^ How should overlapping labels be displayed?
    | LabelPadding Double
      -- ^ The padding, in pixels, between the label and the axis.
    | LabelSeparation Double
      -- ^ The minimum separation, in pixels, between label bounding boxes
      --   for them to be considered non-overlapping. This is ignored if
      --   the 'LabelOverlap' strategy is 'ONone'.
      --
      --   @since 0.4.0.0
    | MaxExtent Double
      -- ^ The maximum extent, in pixels, that axis ticks and labels should use.
      --   This determines a maxmium offset value for axis titles.
    | MinExtent Double
      -- ^ The minimum extent, in pixels, that axis ticks and labels should use.
      --   This determines a minmium offset value for axis titles.
    | NoTitle
      -- ^ Do not draw a title for this axis.
      --
      --   @since 0.4.0.0
    | Orient Side
      -- ^ The orientation of the axis.
      --
      --   @since 0.4.0.0
    | ShortTimeLabels Bool
      -- ^ Should an axis use short time labels (abbreviated month and week-day names)?
    | Ticks Bool
      -- ^ Should tick marks be drawn on an axis?
    | TickColor Color
      -- ^ The color of the ticks.
    | TickDash [Double]
      -- ^ The dash style of the ticks (alternating stroke, space lengths
      --   in pixels).
    | TickDashOffset Double
      -- ^ The pixel offset at which to start drawing the tick dash array.
      --
      --   @since 0.4.0.0
    | TickExtra Bool
      -- ^ Should an extra axis tick mark be added for the initial position of
      --   the axis?
      --
      --   @since 0.4.0.0
    | TickOffset Double
      -- ^ The position offset, in pixels, to apply to ticks, labels, and grid lines.
      --
      --   @since 0.4.0.0
    | TickOpacity Opacity
      -- ^ The opacity of the ticks.
      --
      --   @since 0.4.0.0
    | TickRound Bool
      -- ^ Should pixel position values be rounded to the nearest integer?
    | TickSize Double
      -- ^ The size of the tick marks in pixels.
    | TickWidth Double
      -- ^ The width of the tick marks in pixels.
    | TitleAlign HAlign
      -- ^ The horizontal alignment of the axis title.
    | TitleAnchor APosition
      -- ^ The text anchor position for placing axis titles.
      --
      --   @since 0.4.0.0
    | TitleAngle Angle
      -- ^ The angle of the axis title.
    | TitleBaseline VAlign
      -- ^ The vertical alignment of the axis title.
    | TitleColor Color
      -- ^ The color of the axis title.
    | TitleFont T.Text
      -- ^ The font for the axis title.
    | TitleFontSize Double
      -- ^ The font size of the axis title.
    | TitleFontStyle T.Text
      -- ^ The font style of the axis title.
      --
      --   @since 0.4.0.0
    | TitleFontWeight FontWeight
      -- ^ The font weight of the axis title.
    | TitleLimit Double
      -- ^ The maximum allowed width of the axis title, in pixels.
    | TitleOpacity Opacity
      -- ^ The opacity of the axis title.
      --
      --   @since 0.4.0.0
    | TitlePadding Double
      -- ^ The padding, in pixels, between title and axis.
    | TitleX Double
      -- ^ The X coordinate of the axis title, relative to the axis group.
    | TitleY Double
      -- ^ The Y coordinate of the axis title, relative to the axis group.


-- Using an equality test here isn't ideal, but I am just following the
-- Elm code for now.
--
mxToValue :: Maybe Double -> Value
mxToValue (Just x) | x == 1 = toJSON True
                   | otherwise = toJSON x
mxToValue Nothing = toJSON False

axisConfigProperty :: AxisConfig -> LabelledSpec
axisConfigProperty (BandPosition x) = "bandPosition" .= x
axisConfigProperty (Domain b) = "domain" .= b
axisConfigProperty (DomainColor c) = "domainColor" .= c
axisConfigProperty (DomainDash ds) = "domainDash" .= ds
axisConfigProperty (DomainDashOffset x) = "domainDashOffset" .= x
axisConfigProperty (DomainOpacity x) = "domainOpacity" .= x
axisConfigProperty (DomainWidth w) = "domainWidth" .= w
axisConfigProperty (Grid b) = "grid" .= b
axisConfigProperty (GridColor c) = "gridColor" .= c
axisConfigProperty (GridDash ds) = "gridDash" .= ds
axisConfigProperty (GridDashOffset x) = "gridDashOffset" .= x
axisConfigProperty (GridOpacity o) = "gridOpacity" .= o
axisConfigProperty (GridWidth x) = "gridWidth" .= x
axisConfigProperty (Labels b) = "labels" .= b
axisConfigProperty (LabelAlign ha) = "labelAlign" .= hAlignLabel ha
axisConfigProperty (LabelAngle angle) = "labelAngle" .= angle
axisConfigProperty (LabelBaseline va) = "labelBaseline" .= vAlignLabel va
axisConfigProperty (LabelBound mx) = "labelBound" .= mxToValue mx
axisConfigProperty (LabelFlush mx) = "labelFlush" .= mxToValue mx
axisConfigProperty (LabelFlushOffset x) = "labelFlushOffset" .= x
axisConfigProperty (LabelColor c) = "labelColor" .= c
axisConfigProperty (LabelFont f) = "labelFont" .= f
axisConfigProperty (LabelFontSize x) = "labelFontSize" .= x
axisConfigProperty (LabelFontStyle s) = "labelFontStyle" .= s
axisConfigProperty (LabelFontWeight fw) = "labelFontWeight" .= fontWeightSpec fw
axisConfigProperty (LabelLimit x) = "labelLimit" .= x
axisConfigProperty (LabelOpacity x) = "labelOpacity" .= x
axisConfigProperty (LabelOverlap strat) = "labelOverlap" .= overlapStrategyLabel strat
axisConfigProperty (LabelPadding pad) = "labelPadding" .= pad
axisConfigProperty (LabelSeparation x) = "labelSeparation" .= x
axisConfigProperty (MaxExtent n) = "maxExtent" .= n
axisConfigProperty (MinExtent n) = "minExtent" .= n
axisConfigProperty NoTitle = "title" .= A.Null
axisConfigProperty (Orient orient) = "orient" .= sideLabel orient
axisConfigProperty (ShortTimeLabels b) = "shortTimeLabels" .= b
axisConfigProperty (Ticks b) = "ticks" .= b
axisConfigProperty (TickColor c) = "tickColor" .= c
axisConfigProperty (TickDash ds) = "tickDash" .= ds
axisConfigProperty (TickDashOffset x) = "tickDashOffset" .= x
axisConfigProperty (TickExtra b) = "tickExtra" .= b
axisConfigProperty (TickOffset x) = "tickOffset" .= x
axisConfigProperty (TickOpacity x) = "tickOpacity" .= x
axisConfigProperty (TickRound b) = "tickRound" .= b
axisConfigProperty (TickSize x) = "tickSize" .= x
axisConfigProperty (TickWidth x) = "tickWidth" .= x
axisConfigProperty (TitleAlign algn) = "titleAlign" .= hAlignLabel algn
axisConfigProperty (TitleAnchor a) = "titleAnchor" .= anchorLabel a
axisConfigProperty (TitleAngle x) = "titleAngle" .= x
axisConfigProperty (TitleBaseline va) = "titleBaseline" .= vAlignLabel va
axisConfigProperty (TitleColor c) = "titleColor" .= c
axisConfigProperty (TitleFont f) = "titleFont" .= f
axisConfigProperty (TitleFontSize x) = "titleFontSize" .= x
axisConfigProperty (TitleFontStyle s) = "titleFontStyle" .= s
axisConfigProperty (TitleFontWeight w) = "titleFontWeight" .= fontWeightSpec w
axisConfigProperty (TitleLimit x) = "titleLimit" .= x
axisConfigProperty (TitleOpacity x) = "titleOpacity" .= x
axisConfigProperty (TitlePadding x) = "titlePadding" .= x
axisConfigProperty (TitleX x) = "titleX" .= x
axisConfigProperty (TitleY y) = "titleY" .= y


{-|

Used for creating logical compositions. For example

@
'color'
    [ 'MSelectionCondition' (Or ('SelectionName' "alex") (SelectionName "morgan"))
        ['MAggregate' 'Count', 'MName' "*", 'MmType' 'Quantitative']
        ['MString' "gray"]
    ]
@

Logical compositions can be nested to any level as shown in this example

@
'Not' ('And' ('Expr' "datum.IMDB_Rating === null") ('Expr' "datum.Rotten_Tomatoes_Rating === null") )
@
-}
data BooleanOp
    = Expr T.Text
      -- ^ Expression that should evaluate to either true or false.
      --   Can use any valid
      --   [Vega expression](https://vega.github.io/vega/docs/expressions/).
    | FilterOp Filter
      -- ^ Convert a 'Filter' into a 'BooleanOp' so that it may be used as
      --   part of a more complex expression.
      --
      --   For example (using 'Data.Function.&' to apply 'FilterOp' to a filter):
      --
      --   @
      --   trans = 'transform'
      --             . 'filter' ('FCompose'
      --                        ('And'
      --                          ('FValid' "IMDB_Rating" & 'FilterOp')
      --                          ('FValid' "Rotten_Tomatoes_Rating" & 'FilterOp')
      --                        )
      --                      )
      --   @
      --
      --   @since 0.4.0.0
    | FilterOpTrans MarkChannel Filter
      -- ^ Combine a data-transformation operation with a filter before
      --   converting into a boolean operation. This can be useful when
      --   working with dates, such as the following exampe, which aggregates
      --   a set of dates into years, and filters only those years between
      --   2010 and 2017 (inclusive). The final expression is converted
      --   back into a 'BooleanOp' with 'FCompose' (combined using
      --   'Data.Function.&').
      --
      --   @
      --   'filter' ('FRange' "date" ('DateRange' ['DTYear' 2010] ['DTYear' 2017])
      --           & 'FilterOpTrans' ('MTimeUnit' 'Year')
      --           & 'FCompose'
      --           )
      --   @
      --
      --   @since 0.4.0.0
    | Selection T.Text  -- TODO: rename Selected?
      -- ^ Interactive selection that will be true or false as part of
      --   a logical composition.  For example: to filter a dataset so
      --   that only items selected interactively and that have a
      --   weight of more than 30:
      --
      -- @
      -- 'transform'
      --    . 'filter' ('FCompose' ('And' ('Selection' "brush") ('Expr' "datum.weight > 30")))
      -- @
    | SelectionName T.Text
    -- ^  Name a selection that is used as part of a conditional encoding.
    --
    -- @
    -- 'color'
    --    [ 'MSelectionCondition' ('SelectionName' \"myBrush\")
    --        ['MName' \"myField\", 'MmType' 'Nominal']
    --        ['MString' \"grey\"]
    --    ]
    -- @
    | And BooleanOp BooleanOp
      -- ^ Apply an \'and\' Boolean operation as part of a logical composition.
      --
      -- @
      -- 'And' ('Expr' "datum.IMDB_Rating === null") ('Expr' "datum.Rotten_Tomatoes_Rating === null")
      -- @
    | Or BooleanOp BooleanOp
      -- ^ Apply an \'or\' Boolean operation as part of a logical composition.
    | Not BooleanOp
      -- ^ Negate the given expression.
      --
      -- @
      -- 'Not' ('And' ('Expr' "datum.IMDB_Rating === null") ('Expr' "datum.Rotten_Tomatoes_Rating === null"))
      -- @

booleanOpSpec :: BooleanOp -> VLSpec
booleanOpSpec (Expr expr) = toJSON expr
booleanOpSpec (FilterOp f) = filterSpec f
booleanOpSpec (FilterOpTrans tr f) = trFilterSpec tr f
booleanOpSpec (SelectionName selName) = toJSON selName
booleanOpSpec (Selection sel) = object ["selection" .= sel]
booleanOpSpec (And operand1 operand2) = object ["and" .= [booleanOpSpec operand1, booleanOpSpec operand2]]
booleanOpSpec (Or operand1 operand2) = object ["or" .= [booleanOpSpec operand1, booleanOpSpec operand2]]
booleanOpSpec (Not operand) = object ["not" .= booleanOpSpec operand]


{-|

Type of filtering operation. See the
<https://vega.github.io/vega-lite/docs/filter.html Vega-Lite documentation>
for details.

These can also be included into a 'BooleanOp' expression using 'FilterOp'
and 'FilterOpTrans'
(as of version @0.4.0.0@).

-}
data Filter
    = FEqual T.Text DataValue
      -- ^ Filter a data stream so that only data in a given field equal to
      --   the given value are used.
    | FLessThan T.Text DataValue
      -- ^ Filter a data stream so that only data in a given field less than the given
      --   value are used.
      --
      --   @since 0.4.0.0
    | FLessThanEq T.Text DataValue
      -- ^ Filter a data stream so that only data in a given field less than,
      --   or equal to, the given value are used.
      --
      --   @since 0.4.0.0
    | FGreaterThan T.Text DataValue
      -- ^ Filter a data stream so that only data in a given field greater than the given
      --   value are used.
      --
      --   @since 0.4.0.0
    | FGreaterThanEq T.Text DataValue
      -- ^ Filter a data stream so that only data in a given field greater than,
      --   or equal to, the given value are used.
      --
      --   @since 0.4.0.0
    | FExpr T.Text
      -- ^ Filter a data stream so that only data that satisfy the given predicate
      --   expression are used.
    | FCompose BooleanOp
      -- ^ Build up a filtering predicate through logical composition such
      --   as 'And' and 'Or'.
      --
      --   The following fgragment will apply a filter to identify only
      --   those items selected interactively and that represent ages
      --   over 65:
      --
      --   @
      --   trans = 'transform'
      --             . 'filter'
      --                 ('FCompose'
      --                   ('And' ('Selection' "brush") ('Expr' "datum.age > 65"))
      --                 )
      --   @
    | FSelection T.Text
      -- ^ Filter a data stream so that only data in a given field that are
      --   within the given interactive selection are used.
      --
      --   @
      --   sel = 'selection' . 'select' \"myBrush\" 'Interval' ['Encodings' ['ChX']]
      --   trans = 'transform' . 'filter' ('FSelection' \"myBrush\")
      --   @
    | FOneOf T.Text DataValues
      -- ^ Filter a data stream so that only data in a given field contained in the given
      --   list of values are used.
    | FRange T.Text FilterRange
      -- ^ Filter a data stream so that only data in a given field
      --   that are within the given range are used.
      --
      --   For example:
      --
      --   @
      --   'filter' ('FRange' "date" ('DateRange' ['DTYear' 2006] ['DTYear' 2016])
      --   @
      --
      --   See 'FilterOpTrans' for more use cases.
    | FValid T.Text
      -- ^ Filter a data stream so that only valid data (i.e. not null or NaN) in a given
      --   field are used.
      --
      --   @since 0.4.0.0


fop_ :: T.Text -> T.Text -> DataValue -> [LabelledSpec]
fop_ field label val = [field_ field, label .= dataValueSpec val]

filterProperty :: Filter -> [LabelledSpec]

filterProperty (FEqual field val) = fop_ field "equal" val
filterProperty (FLessThan field val) = fop_ field "lt" val
filterProperty (FLessThanEq field val) = fop_ field "lte" val
filterProperty (FGreaterThan field val) = fop_ field "gt" val
filterProperty (FGreaterThanEq field val) = fop_ field "gte" val

filterProperty (FSelection selName) = ["selection" .= selName]

filterProperty (FRange field vals) =
  let ans = case vals of
              NumberRange mn mx -> map toJSON [mn, mx]
              DateRange dMin dMax -> [process dMin, process dMax]

      process [] = A.Null
      process dts = object (map dateTimeProperty dts)

  in [field_ field, "range" .= ans]

filterProperty (FOneOf field vals) =
  let ans = case vals of
              Numbers xs -> map toJSON xs
              DateTimes dts -> map (object . map dateTimeProperty) dts
              Strings ss -> map toJSON ss
              Booleans bs -> map toJSON bs

  in [field_ field, "oneOf" .= ans]

filterProperty (FValid field) = [field_ field, "valid" .= True]
filterProperty _ = []  -- ignore FExpr and FCompose


filterSpec :: Filter -> VLSpec
filterSpec (FExpr expr) = toJSON expr
filterSpec (FCompose boolExpr) = booleanOpSpec boolExpr
filterSpec f = object (filterProperty f)

trFilterSpec :: MarkChannel -> Filter -> VLSpec
trFilterSpec _ (FExpr expr) = toJSON expr
trFilterSpec _ (FCompose boolExpr) = booleanOpSpec boolExpr
trFilterSpec mchan fi = object (markChannelProperty mchan <> filterProperty fi)


{-|

A pair of filter range data values. The first argument is the inclusive minimum
vale to accept and the second the inclusive maximum.
-}
data FilterRange
    = NumberRange Double Double
    | DateRange [DateTime] [DateTime]


{-|

Specifies the type and content of geometry specifications for programatically
creating GeoShapes. These can be mapped to the
<https://tools.ietf.org/html/rfc7946#section-3.1 GeoJson geometry object types>
where the pluralised type names refer to their @Multi@ prefixed equivalent in the
GeoJSON specification.
-}
data Geometry
    = GeoPoint Double Double
    -- ^ The GeoJson geometry @point@ type.
    | GeoPoints [(Double, Double)]
    -- ^ The GeoJson geometry @multi-point@ type.
    | GeoLine [(Double, Double)]
    -- ^ The GeoJson geometry @line@ type.
    | GeoLines [[(Double, Double)]]
    -- ^ The GeoJson geometry @multi-line@ type.
    | GeoPolygon [[(Double, Double)]]
    -- ^ The GeoJson geometry @polygon@ type.
    | GeoPolygons [[[(Double, Double)]]]
    -- ^ The GeoJson geometry @multi-polygon@ type.


{-|

Specifies a geometric object to be used in a geoShape specification. The first
parameter is the geometric type, the second an optional list of properties to be
associated with the object.

@
'geometry' ('GeoPolygon' [ [ ( -3, 59 ), ( 4, 59 ), ( 4, 52 ), ( -3, 59 ) ] ]) []
@
-}
geometry :: Geometry -> [(T.Text, DataValue)] -> VLSpec
geometry gType properties =
  object ([ ("type", fromT "Feature")
          , ("geometry", geometryTypeSpec gType) ]
          <> if null properties
             then []
             else [("properties",
                    object (map (second dataValueSpec) properties))]
         )


geometryTypeSpec :: Geometry -> VLSpec
geometryTypeSpec gType =
  let toCoords :: [(Double, Double)] -> VLSpec
      toCoords = toJSON -- rely on Aeson converting a pair to a 2-element list

      toCoordList :: [[(Double, Double)]] -> VLSpec
      toCoordList = toJSON . map toCoords  -- this is just toJSON isn't it?

      -- can we replace this infinite tower of toJSON calls with one toJSON call?
      (ptype, cs) = case gType of
        GeoPoint x y -> ("Point", toJSON [x, y])
        GeoPoints coords -> ("MultiPoint", toCoords coords)
        GeoLine coords -> ("LineString", toCoords coords)
        GeoLines coords -> ("MultiLineString", toCoordList coords)
        GeoPolygon coords -> ("Polygon", toCoordList coords)
        GeoPolygons ccoords -> ("MultiPolygon", toJSON (map toCoordList ccoords))

  in object [("type", ptype), ("coordinates", cs)]


{-|

Generate a data source that is a sphere for bounding global geographic data.
The sphere will be subject to whatever projection is specified for the view.

@
'toVegaLite'
    [ 'sphere'
    , 'projection' [ 'PrType' 'Orthographic' ]
    , 'mark' 'Geoshape' [ 'MFill' "aliceblue" ]
    ]
@

@since 0.4.0.0
-}
sphere :: Data
sphere = (VLData, object ["sphere" .= True])


{-|

Generate a grid of lines of longitude (meridians) and latitude
(parallels).

@
let proj = 'projection' [ 'PrType' 'Orthographic' ]
    sphereSpec = 'asSpec' [ 'sphere'
                        , 'mark' 'Geoshape' [ 'MFill' "aliceblue" ] ]
    gratSpec =
        'asSpec'
            [ 'graticule' [ 'GrStep' (5, 5) ]
            , 'mark' 'Geoshape' [ 'MFilled' False, 'MStrokeWidth' 0.3 ]
            ]
in 'toVegaLite' [ proj, 'layer' [ sphereSpec, gratSpec ] ]
@

@since 0.4.0.0

-}
graticule ::
  [GraticuleProperty] -- ^ An empty list uses the default parameters
  -> Data
graticule [] = (VLData, object ["graticule" .= True])
graticule grProps =
  (VLData, object ["graticule" .= object (map graticuleProperty grProps)])


{-|

Determine the properties of graticules. See the
<https://vega.github.io/vega-lite/docs/data.html#graticule Vega-Lite documentation> for details.

@since 0.4.0.0

-}
data GraticuleProperty
    = GrExtent (Double, Double) (Double, Double)
    -- ^ Define the extent of both the major and minor graticules.
    --   The range is given as longitude, latitude pairs of the
    --   minimum and then maximum extent. The units are degrees.
    | GrExtentMajor (Double, Double) (Double, Double)
    -- ^ As @GrExtent@ but for the major graticule lines only.
    | GrExtentMinor (Double, Double) (Double, Double)
    -- ^ As @GrExtent@ but for the minor graticule lines only.
    | GrStep (Double, Double)
    -- ^ The step angles for the graticule lines, given as a longitude,
    --   latitude pair defining the EW and NS intervals respectively.
    --   The units are degrees.
    --
    --   By default major graticule lines extend to both poles but the
    --   minor lines stop at ±80 degrees latitude.
    | GrStepMajor (Double, Double)
    -- ^ As @GrStep@ but for the major graticule lines only.
    --
    --   The default is @(90, 360)@.
    | GrStepMinor (Double, Double)
    -- ^ As @GrStep@ but for the minor graticule lines only.
    --
    --   The default is @(10, 10)@.
    | GrPrecision Double
    -- ^ The precision of the graticule. The units are degrees.
    --   A smaller value reduces visual artifacts (steps) but takes longer
    --   to render.
    --
    --   The default is @2.5@.


graticuleProperty :: GraticuleProperty -> LabelledSpec
graticuleProperty (GrExtent (lng1, lat1) (lng2, lat2)) =
  "extent" .= [[lng1, lat1], [lng2, lat2]]
graticuleProperty (GrExtentMajor (lng1, lat1) (lng2, lat2)) =
  "extentMajor" .= [[lng1, lat1], [lng2, lat2]]
graticuleProperty (GrExtentMinor (lng1, lat1) (lng2, lat2)) =
  "extentMinor" .= [[lng1, lat1], [lng2, lat2]]
graticuleProperty (GrStep (lng, lat)) = "step" .= [lng, lat]
graticuleProperty (GrStepMajor (lng, lat)) = "stepMajor" .= [lng, lat]
graticuleProperty (GrStepMinor (lng, lat)) = "stepMinor" .= [lng, lat]
graticuleProperty (GrPrecision x) = "precision" .= x


{-|

Indicates whether or not a scale domain should be independent of others in a
composite visualization. See the
<https://vega.github.io/vega-lite/docs/resolve.html Vega-Lite documentation> for
details.

For use with 'Resolve'.

-}
data Resolution
    = Shared
    | Independent


resolutionLabel :: Resolution -> T.Text
resolutionLabel Shared = "shared"
resolutionLabel Independent = "independent"


{-|

Used to determine how a channel's axis, scale or legend domains should be resolved
if defined in more than one view in a composite visualization. See the
<https://vega.github.io/vega-lite/docs/resolve.html Vega-Lite documentation>
for details.
-}
data Resolve
    = RAxis [(Channel, Resolution)]
    | RLegend [(Channel, Resolution)]
    | RScale [(Channel, Resolution)]


resolveProperty :: Resolve -> LabelledSpec
resolveProperty res =
  let (nme, rls) = case res of
        RAxis chRules -> ("axis", chRules)
        RLegend chRules -> ("legend", chRules)
        RScale chRules -> ("scale", chRules)

      ans = map (\(ch, rule) -> (channelLabel ch .= resolutionLabel rule)) rls
  in (nme, object ans)


{-|

Represents a facet header property. For details, see the
<https://vega.github.io/vega-lite/docs/facet.html#header Vega-Lite documentation>.

Labels refer to the title of each sub-plot in a faceted view and
title is the overall title of the collection.

-}

-- TODO: should there be a HLabelBaseline, HTitleFontStyle, ...?
--       However, the following covers the vega-lite 3.3.0 schema

data HeaderProperty
    = HFormat T.Text
      -- ^ [Formatting pattern](https://vega.github.io/vega-lite/docs/format.html) for
      --   facet header (title) values. To distinguish between formatting as numeric values
      --   and data/time values, additionally use 'HFormatAsNum' or 'HFormatAsTemporal'.
    | HFormatAsNum
      -- ^ Facet headers should be formatted as numbers. Use a
      --   [d3 numeric format string](https://github.com/d3/d3-format#locale_format)
      --   with 'HFormat'.
      --
      -- @since 0.4.0.0
    | HFormatAsTemporal
      -- ^ Facet headers should be formatted as dates or times. Use a
      --   [d3 date/time format string](https://github.com/d3/d3-time-format#locale_format)
      --   with 'HFormat'.
      --
      -- @since 0.4.0.0
    | HTitle T.Text
      -- ^ The title for the facets.
    | HNoTitle
      -- ^ Draw no title for the facets.
      --
      -- @since 0.4.0.0
    | HLabelAlign HAlign
      -- ^ The horizontal alignment of the labels.
      --
      -- @since 0.4.0.0
    | HLabelAnchor APosition
      -- ^ The anchor position for the labels.
      --
      -- @since 0.4.0.0
    | HLabelAngle Angle
      -- ^ The angle to draw the labels.
      --
      -- @since 0.4.0.0
    | HLabelColor Color
      -- ^ The color of the labels.
      --
      -- @since 0.4.0.0
    | HLabelFont T.Text
      -- ^ The font for the labels.
      --
      -- @since 0.4.0.0
    | HLabelFontSize Double
      -- ^ The font size for the labels.
      --
      -- @since 0.4.0.0
    | HLabelLimit Double
      -- ^ The maximum length of each label.
      --
      -- @since 0.4.0.0
    | HLabelOrient Side
      -- ^ The position of the label relative to its sub-plot.
      --
      -- @since 0.4.0.0
    | HLabelPadding Double
      -- ^ The spacing in pixels between the label and its sub-plot.
      --
      -- @since 0.4.0.0
    | HTitleAlign HAlign
      -- ^ The horizontal alignment of the title.
      --
      -- @since 0.4.0.0
    | HTitleAnchor APosition
      -- ^ The anchor position for the title.
      --
      -- @since 0.4.0.0
    | HTitleAngle Angle
      -- ^ The angle to draw the title.
      --
      -- @since 0.4.0.0
    | HTitleBaseline VAlign
      -- ^ The vertical alignment of the title.
      --
      -- @since 0.4.0.0
    | HTitleColor Color
      -- ^ The color of the title.
      --
      -- @since 0.4.0.0
    | HTitleFont T.Text
      -- ^ The font for the title.
      --
      -- @since 0.4.0.0
    | HTitleFontSize Double
      -- ^ The font size for the title.
      --
      -- @since 0.4.0.0
    | HTitleFontWeight T.Text
      -- ^ The font weight for the title.
      --
      -- @since 0.4.0.0
    | HTitleLimit Double
      -- ^ The maximum length of the title.
      --
      -- @since 0.4.0.0
    | HTitleOrient Side
      -- ^ The position of the title relative to the sub-plots.
      --
      -- @since 0.4.0.0
    | HTitlePadding Double
      -- ^ The spacing in pixels between the title and the labels.
      --
      -- @since 0.4.0.0


headerProperty :: HeaderProperty -> LabelledSpec
headerProperty (HFormat fmt) = "format" .= fmt
headerProperty HFormatAsNum = "formatType" .= fromT "number"
headerProperty HFormatAsTemporal = "formatType" .= fromT "time"
headerProperty (HTitle ttl) = "title" .= ttl
headerProperty HNoTitle = "title" .= A.Null
headerProperty (HLabelAlign ha) = "labelAlign" .= hAlignLabel ha
headerProperty (HLabelAnchor a) = "labelAnchor" .= anchorLabel a
headerProperty (HLabelAngle x) = "labelAngle" .= x
headerProperty (HLabelColor s) = "labelColor" .= s
headerProperty (HLabelFont s) = "labelFont" .= s
headerProperty (HLabelFontSize x) = "labelFontSize" .= x
headerProperty (HLabelLimit x) = "labelLimit" .= x
headerProperty (HLabelOrient orient) = "labelOrient" .= sideLabel orient
headerProperty (HLabelPadding x) = "labelPadding" .= x
headerProperty (HTitleAlign ha) = "titleAlign" .= hAlignLabel ha
headerProperty (HTitleAnchor a) = "titleAnchor" .= anchorLabel a
headerProperty (HTitleAngle x) = "titleAngle" .= x
headerProperty (HTitleBaseline va) = "titleBaseline" .= vAlignLabel va
headerProperty (HTitleColor s) = "titleColor" .= s
headerProperty (HTitleFont s) = "titleFont" .= s
headerProperty (HTitleFontWeight s) = "titleFontWeight" .= s
headerProperty (HTitleFontSize x) = "titleFontSize" .= x
headerProperty (HTitleLimit x) = "titleLimit" .= x
headerProperty (HTitleOrient orient) = "titleOrient" .= sideLabel orient
headerProperty (HTitlePadding x) = "titlePadding" .= x


-- | Types of hyperlink channel property used for linking marks or text to URLs.

data HyperlinkChannel
    = HName T.Text
      -- ^ Field used for encoding with a hyperlink channel.
    | HRepeat Arrangement
      -- ^ Reference in a hyperlink channel to a field name generated by 'repeatFlow'
      --  or 'repeat'. The parameter identifies whether reference is being made to
      -- fields that are to be arranged in columns, in rows, or a with a flow layout.
    | HmType Measurement
      -- ^ Level of measurement when encoding with a hyperlink channel.
    | HBin [BinProperty]
      -- ^ Discretize numeric values into bins when encoding with a hyperlink channel.
    | HBinned
      -- ^ Indicate that data encoded with a hyperlink channel are already binned.
      --
      --   @since 0.4.0.0
    | HAggregate Operation
      -- ^ Compute aggregate summary statistics for a field to be encoded with a
      -- hyperlink channel.
    | HTimeUnit TimeUnit
    | HSelectionCondition BooleanOp [HyperlinkChannel] [HyperlinkChannel]
      -- ^ Make a hyperlink channel conditional on interactive selection. The first parameter
      --   provides the selection to evaluate, the second the encoding to apply if the hyperlink
      --   has been selected, the third the encoding if it is not selected.
    | HDataCondition [(BooleanOp, [HyperlinkChannel])] [HyperlinkChannel]
      -- ^ Make a hyperlink channel conditional on one or more predicate expressions. The first
      --   parameter is a list of tuples each pairing an expression to evaluate with the encoding
      --   if that expression is @True@. The second is the encoding if none of the expressions
      --   evaluate as @True@.
      --
      --   The arguments to this constructor have changed in @0.4.0.0@
      --   to support multiple expressions.
    | HString T.Text
      -- ^ Literal string value when encoding with a hyperlink channel.

hyperlinkChannelProperty :: HyperlinkChannel -> [LabelledSpec]
hyperlinkChannelProperty (HName s) = [field_ s]
hyperlinkChannelProperty (HRepeat arr) = ["field" .= object [repeat_ arr]]
hyperlinkChannelProperty (HmType t) = [mtype_ t]
hyperlinkChannelProperty (HBin bps) = [bin bps]
hyperlinkChannelProperty HBinned = [binned_]
hyperlinkChannelProperty (HSelectionCondition selName ifClause elseClause) =
  selCond_ hyperlinkChannelProperty selName ifClause elseClause
hyperlinkChannelProperty (HDataCondition tests elseClause) =
  dataCond_ hyperlinkChannelProperty tests elseClause
hyperlinkChannelProperty (HTimeUnit tu) = [timeUnit_ tu]
hyperlinkChannelProperty (HAggregate op) = [aggregate_ op]
hyperlinkChannelProperty (HString s) = [value_ s]


----

{-|

Create a pair of continuous domain to color mappings suitable for customising
ordered scales. The first parameter is a tuple representing the mapping of the lowest
numeric value in the domain to its equivalent color; the second tuple the mapping
of the highest numeric value to color. If the domain contains any values between
these lower and upper bounds they are interpolated according to the scale's interpolation
function. This is a convenience function equivalent to specifying separate 'SDomain'
and 'SRange' lists and is safer as it guarantees a one-to-one correspondence between
domain and range values.

@
'color'
    [ 'MName' "year"
    , 'MmType' 'Ordinal'
    , 'MScale' (domainRangeMap (1955, \"rgb(230,149,156)\") (2000, \"rgb(145,26,36)\"))
    ]
@
-}

domainRangeMap :: (Double, T.Text) -> (Double, T.Text) -> [ScaleProperty]
domainRangeMap lowerMap upperMap =
  let (domain, range) = unzip [lowerMap, upperMap]
  in [SDomain (DNumbers domain), SRange (RStrings range)]


{-|

Create a set of discrete domain to color mappings suitable for customising categorical
scales. The first item in each tuple should be a domain value and the second the
color value with which it should be associated. It is a convenience function equivalent
to specifying separate 'SDomain' and 'SRange' lists and is safer as it guarantees
a one-to-one correspondence between domain and range values.

@
'color'
    [ 'MName' "weather"
    , 'MmType' Nominal
    , 'MScale' (
        categoricalDomainMap
            [ ( "sun", "yellow" )
            , ( "rain", "blue" )
            , ( "fog", "grey" )
            ]
        )
    ]
@
-}

categoricalDomainMap :: [(T.Text, T.Text)] -> [ScaleProperty]
categoricalDomainMap scaleDomainPairs =
  let (domain, range) = unzip scaleDomainPairs
  in [SDomain (DStrings domain), SRange (RStrings range)]


{-|

Create a list of fields to use in set of repeated small multiples. The list of
fields named here can be referenced in an encoding with @'PRepeat' 'Column'@
or @'PRepeat' 'Row'@.

-}
data RepeatFields
    = RowFields [T.Text]
    | ColumnFields [T.Text]


repeatFieldsProperty :: RepeatFields -> LabelledSpec
repeatFieldsProperty rfs =
  let (nme, vs) = case rfs of
        RowFields fields -> ("row", fields)
        ColumnFields fields -> ("column", fields)

  in nme .= map toJSON vs


{-|

Types of facet channel property used for creating a composed facet view of small
multiples.

-}

-- based on schema 3.3.0 #/definitions/FacetFieldDef

data FacetChannel
    = FName T.Text
      -- ^ The name of the field from which to pull a data value.
    | FmType Measurement
      -- ^ The encoded field's type of measurement.
    | FAggregate Operation
      -- ^ Aggregation function for the field.
    | FBin [BinProperty]
      -- ^ Describe how to bin quantitative fields, or whether the
      --   channels are already binned.
    | FHeader [HeaderProperty]
      -- ^ The properties of a facet's header.
    | FSort [SortProperty]
      -- ^ Sort order for the encoded field.
      --
      --   @since 0.4.0.0
    | FTimeUnit TimeUnit
      -- ^ The time-unit for a temporal field.
    | FTitle T.Text
      -- ^ The title for the field.
      --
      --   @since 0.4.0.0
    | FNoTitle
      -- ^ Draw no title.
      --
      -- @since 0.4.0.0

facetChannelProperty :: FacetChannel -> LabelledSpec
facetChannelProperty (FName s) = field_ s
facetChannelProperty (FmType measure) = mtype_ measure
facetChannelProperty (FAggregate op) = aggregate_ op
facetChannelProperty (FBin bps) = bin bps
facetChannelProperty (FHeader hps) = header_ hps
facetChannelProperty (FSort sps) = sort_ sps
facetChannelProperty (FTitle s) = "title" .= s
facetChannelProperty FNoTitle = "title" .= A.Null
facetChannelProperty (FTimeUnit tu) = timeUnit_ tu


{-|

Configuration options for faceted views, used with 'FacetStyle'.

See the
<https://vega.github.io/vega-lite/docs/facet.html#facet-configuration Vega-Lite facet config documentation>.

@since 0.4.0.0

-}
data FacetConfig
    = FColumns Int
    -- ^ The maximum number of columns to use in a faceted-flow layout.
    | FSpacing Double
    -- ^ The spacing in pixels between sub-views in a faceted composition.


facetConfigProperty :: FacetConfig -> LabelledSpec
facetConfigProperty (FColumns n) = "columns" .= n
facetConfigProperty (FSpacing x) = "spacing" .= x


-- | Types of text channel property used for displaying text as part of the visualization.

-- Basing the following partly on vega-lite-3.3.0.json / TextFieldDef
-- but that doesn't seem to be sufficient.

data TextChannel
    = TName T.Text
      -- ^ Name of the field used for encoding with a text channel.
    | TAggregate Operation
      -- ^ Compute some aggregate summary statistics for a field to be encoded with a
      --   text channel. The type of aggregation is determined by the given operation
      --   parameter.
    | TBin [BinProperty]
      -- ^ Discretize numeric values into bins when encoding with a text channel.
    | TBinned
      -- ^ Indicate that data encoded with a text channel are already binned.
      --
      --   @since 0.4.0.0
    | TDataCondition [(BooleanOp, [TextChannel])] [TextChannel]
      -- ^ Make a text channel conditional on one or more predicate expressions. The first
      --   parameter is a list of tuples each pairing an expression to evaluate with the encoding
      --   if that expression is @True@. The second is the encoding if none of the expressions
      --   evaluate as @True@.
      --
      --   The arguments to this constructor have changed in @0.4.0.0@
      --   to support multiple expressions.
    | TFormat T.Text
      -- ^ [Formatting pattern](https://vega.github.io/vega-lite/docs/format.html)
      --   for text marks. To distinguish between formatting as numeric values and data/time
      --   values, additionally use 'TFormatAsNum' or 'TFormatAsTemporal'.
    | TFormatAsNum
      -- ^ The text marks should be formatted as numbers. Use a
      --   [d3 numeric format string](https://github.com/d3/d3-format#locale_format)
      --   with 'TFormat'.
      --
      -- @since 0.4.0.0
    | TFormatAsTemporal
      -- ^ The text marks should be formatted as dates or times. Use a
      --   [d3 date/time format string](https://github.com/d3/d3-time-format#locale_format)
      --   with 'TFormat'.
      --
      -- @since 0.4.0.0
    | TmType Measurement
      -- ^ Level of measurement when encoding with a text channel.
    | TRepeat Arrangement
      -- ^ Reference in a text channel to a field name generated by 'repeatFlow'
      --   or 'repeat'. The parameter identifies whether reference is being made to
      --   fields that are to be arranged in columns, in rows, or a with a flow layout.
    | TSelectionCondition BooleanOp [TextChannel] [TextChannel]
      -- ^ Make a text channel conditional on interactive selection. The first parameter
      --   is a selection condition to evaluate; the second the encoding to apply if that
      --   selection is true; the third parameter is the encoding if the selection is false.
    | TTitle T.Text
      -- ^ Title of a field when encoding with a text or tooltip channel.
      --
      --   @since 0.4.0.0
    | TNoTitle
      -- ^ Display no title.
      --
      --   @since 0.4.0.0
    | TTimeUnit TimeUnit
      -- ^ Time unit aggregation of field values when encoding with a text channel.


textChannelProperty :: TextChannel -> [LabelledSpec]
textChannelProperty (TName s) = [field_  s]
textChannelProperty (TAggregate op) = [aggregate_ op]
textChannelProperty (TBin bps) = [bin bps]
textChannelProperty TBinned = [binned_]
textChannelProperty (TFormat fmt) = ["format" .= fmt]
textChannelProperty TFormatAsNum = ["formatType" .= fromT "number"]
textChannelProperty TFormatAsTemporal = ["formatType" .= fromT "time"]
textChannelProperty (TmType measure) = [mtype_ measure]
textChannelProperty (TRepeat arr) = ["field" .= object [repeat_ arr]]
textChannelProperty (TTitle s) = ["title" .= s]
textChannelProperty TNoTitle = ["title" .= A.Null]
textChannelProperty (TTimeUnit tu) = [timeUnit_ tu]
textChannelProperty (TDataCondition tests elseClause) =
  dataCond_ textChannelProperty tests elseClause
textChannelProperty (TSelectionCondition selName ifClause elseClause) =
  selCond_ textChannelProperty selName ifClause elseClause


-- | Properties of an ordering channel used for sorting data fields.

data OrderChannel
    = OName T.Text
    | ORepeat Arrangement
      -- ^ Reference in an order channel to a field name generated by 'repeatFlow'
      -- or 'repeat'. The parameter identifies whether reference is being made to
      -- fields that are to be arranged in columns, in rows, or a with a flow layout.
    | OmType Measurement
    | OBin [BinProperty]
    | OAggregate Operation
    | OTimeUnit TimeUnit
    | OSort [SortProperty]


orderChannelProperty :: OrderChannel -> LabelledSpec
orderChannelProperty (OName s) = field_ s
orderChannelProperty (ORepeat arr) = "field" .= object [repeat_ arr]
orderChannelProperty (OmType measure) = mtype_ measure
orderChannelProperty (OBin bps) = bin bps
orderChannelProperty (OAggregate op) = aggregate_ op
orderChannelProperty (OTimeUnit tu) = timeUnit_ tu
orderChannelProperty (OSort ops) = sort_ ops


-- | Level of detail channel properties used for creating a grouped channel encoding.

data DetailChannel
    = DName T.Text
    | DmType Measurement
    | DBin [BinProperty]
    | DTimeUnit TimeUnit
    | DAggregate Operation


detailChannelProperty :: DetailChannel -> LabelledSpec
detailChannelProperty (DName s) = field_ s
detailChannelProperty (DmType t) = mtype_ t
detailChannelProperty (DBin bps) = bin bps
detailChannelProperty (DTimeUnit tu) = timeUnit_ tu
detailChannelProperty (DAggregate op) = aggregate_ op


{-|

Provides details of the mapping between a row or column and its field
definitions in a set of faceted small multiples. For details see the
<https://vega.github.io/vega-lite/docs/facet.html#mapping Vega-Lite documentation>.
-}
data FacetMapping
    = ColumnBy [FacetChannel]
    | RowBy [FacetChannel]


facetMappingProperty :: FacetMapping -> LabelledSpec
facetMappingProperty (RowBy fFields) =
  "row" .= object (map facetChannelProperty fFields)
facetMappingProperty (ColumnBy fFields) =
  "column" .= object (map facetChannelProperty fFields)


{-|

Create a single global configuration from a list of configuration specifications.
Configurations are applied to all relevant items in the specification. See the
<https://vega.github.io/vega-lite/docs/config.html Vega-Lite documentation> for
more details.

The following example would make axis lines (domain) 2 pixels wide,
remove the border rectangle and require interactive selection of items
to use a double-click:

@
config =
    'configure'
        . 'configuration' ('Axis' [ 'DomainWidth' 1 ])
        . 'configuration' ('View' [ 'ViewStroke' (Just "transparent") ])
        . 'configuration' ('SelectionStyle' [ ( 'Single', [ 'On' \"dblclick\" ] ) ])
@
-}
configure :: [LabelledSpec] -> PropertySpec
configure configs = (VLConfig, object configs)


-- | Alignment to apply to grid rows and columns generated by a composition
--   operator. This version sets the same alignment for rows and columns.
--
--   See also 'alignRC'.
--
--   @since 0.4.0.0

align :: CompositionAlignment -> PropertySpec
align algn = (VLAlign, compositionAlignmentSpec algn)


-- | Similar to 'align' but with independent alignments for rows and columns.
--
--   See also 'align'.
--
--   @since 0.4.0.0

alignRC ::
  CompositionAlignment     -- ^ Row alignment
  -> CompositionAlignment  -- ^ Column alignment
  -> PropertySpec
alignRC alRow alCol =
  (VLSpacing, object [ "row" .= compositionAlignmentSpec alRow
                     , "col" .= compositionAlignmentSpec alCol
                     ])


-- | Spacing between sub-views in a composition operator.
--
--   See also 'spacingRC'.
--
--   @since 0.4.0.0

spacing ::
  Double   -- ^ Spacing in pixels.
  -> PropertySpec
spacing sp = (VLSpacing, toJSON sp)


-- | Set the spacing between the rows and columns.
--
--   See also 'spacing'.
--
--   @since 0.4.0.0

spacingRC ::
  Double      -- ^ Spacing between rows (in pixels).
  -> Double   -- ^ Spacing between columns (in pixels).
  -> PropertySpec
spacingRC spRow spCol = (VLSpacing, object ["row" .= spRow, "column" .= spCol])


-- | Are sub-views in a composition operator centred relative to their respective
--   rows and columns?
--
--   See also 'centerRC'.
--
--   @since 0.4.0.0

center :: Bool -> PropertySpec
center c = (VLCenter, toJSON c)


-- | Are sub-views in a composition operator centred relative to their respective
--   rows and columns?
--
--   See also 'center'.
--
--   @since 0.4.0.0

centerRC ::
  Bool     -- ^ Are rows to be centered?
  -> Bool  -- ^ Are columns to be centered?
  -> PropertySpec
centerRC cRow cCol = (VLCenter, object ["row" .= cRow, "col" .= cCol])


-- | This is used with 'bounds' to define the extent of a sub plot.
--
--   @since 0.4.0.0

data Bounds
  = Full
    -- ^ Bounds calculation should use the entire plot area (including axes, title,
    --   and legend).
  | Flush
    -- ^ Bounds calculation should take only the specified width and height values for
    --   a sub-view. Useful when attempting to place sub-plots without axes or legends into
    --   a uniform grid structure.


boundsSpec :: Bounds -> VLSpec
boundsSpec Full = "full"
boundsSpec Flush = "flush"


{-|

Bounds calculation method to use for determining the extent of a sub-plot in
a composed view.

@since 0.4.0.0
-}
bounds :: Bounds -> PropertySpec
bounds bnds = (VLBounds, boundsSpec bnds)


{-|

The list of specifications to be juxtaposed horizontally in a flow
layout of views.

The number of columns in the flow layout can be set with 'columns'
and, if not specified, will default to a single row of unlimited columns.

@
let dvals = 'dataSequenceAs' 0 6.28 0.1 \"x\"
    trans = 'transform'
              . 'calculateAs' \"sin(datum.x)\" \"sinX\"
              . 'calculateAs' \"cos(datum.x)\" \"cosX\"
    enc = 'encoding'
            . 'position' 'X' ['PName' \"x\", 'PmType' 'Quantitative']
    encCos = enc . 'position' 'Y' ['PName' \"cosX\", 'PmType' 'Quantitative']
    encSin = enc . 'position' 'Y' ['PName' \"sinX\", 'PmType' 'Quantitative']

in toVegaLite [ dvals
              , trans []
              , 'vlConcat' [ 'asSpec' [encCos [], 'mark' 'Line' []]
                         , 'asSpec' [encSin [], 'mark' 'Line' []]
                         ]
              ]
@

This is named @concat@ in Elm VegaLite but has been renamed here
to avoid conflicting with the Prelude.

@since 0.4.0.0

-}
vlConcat :: [VLSpec] -> PropertySpec
vlConcat specs = (VLConcat, toJSON specs)


{-|

Defines the fields that will be used to facet a view in rows or columns to create
a set of small multiples. This is used where the encoding of the visualization in small
multiples is identical, but data for each is grouped by the given fields. When
creating a faceted view in this way you also need to define a full specification
to apply to each of those facets using 'asSpec'.

See the
<https://vega.github.io/vega-lite/docs/facet.html Vega-Lite documentation>
for further details.

@
'toVegaLite'
    [ facet [ 'RowBy' [ 'FName' \"Month\", 'FmType' 'Ordinal' ]
            , 'ColumnBy' [ 'FName' \"Week\", 'FmType' 'Ordinal' ]
            ]
    , 'specification' spec
    ]
@

See also 'facetFlow'.

-}

facet :: [FacetMapping] -> PropertySpec
facet fMaps = (VLFacet, object (map facetMappingProperty fMaps))


{-|

Facet a view to create small multiples in a flow layout. Used when the encoding
of the visualization in small multiples is identical, but data for each is grouped
by the given fields. When creating a faceted view in this way you also need to
define a full specification to apply to each of those facets using 'asSpec'.

Small multiples will be laid out from left to right, moving on to new rows only
if the number of plots exceeds an optional column limit (specified via 'columns').

@
'toVegaLite'
    [ facetFlow [ 'FName' \"Origin\", 'FmType' 'Nominal' ]
    , 'specification' spec
    ]
@

See also 'facet'.

@since 0.4.0.0
-}
facetFlow :: [FacetChannel] -> PropertySpec
facetFlow fFields = (VLFacet, object (map facetChannelProperty fFields))


{-|

Overrides the default height of the visualization. If not specified the height
will be calculated based on the content of the visualization. See
'autosize' for customization of the content sizing relative to this
setting.

@
'toVegaLite'
    [ 'height' 300
    , 'dataFromUrl' "data/population.json" []
    , 'mark' 'Bar' []
    , enc []
    ]
@
-}
height :: Double -> PropertySpec
height h = (VLHeight, toJSON h)


{-|

Configuration options for concatenated views, used with 'ConcatStyle'.

@since 0.4.0.0

-}
data ConcatConfig
    = ConcatColumns Int
      -- ^ The maximum number of columns to use in a concatenated flow layout.
    | ConcatSpacing Double
      -- ^ The spacing in pixels between sub-views in a concatenated view.


concatConfigProperty :: ConcatConfig -> LabelledSpec
concatConfigProperty (ConcatColumns n) = "columns" .= n
concatConfigProperty (ConcatSpacing x) = "spacing" .= x


{-|

Assigns a list of specifications to be juxtaposed horizontally in a visualization.

@
'toVegaLite'
    [ 'dataFromUrl' "data/driving.json" []
    , hConcat [ spec1, spec2 ]
    ]
@
-}
hConcat :: [VLSpec] -> PropertySpec
hConcat specs = (VLHConcat, toJSON specs)


{-|

Assigns a list of specifications to superposed layers in a visualization.

@
'toVegaLite' ['dataFromUrl' "data/driving.json" [], layer [spec1, spec2]]
@

A complete example showing @layer@ in use:

@
let dvals = 'dataFromColumns' []
              . 'dataColumn' \"x\" ('Numbers' [1, 2, 3, 4, 5])
              . 'dataColumn' \"a\" ('Numbers' [28, 91, 43, 55, 81])
    enc = 'encoding'
             . 'position' 'X' ['PName' \"x\", 'PmType' 'Ordinal']
             . 'position' 'Y' ['PName' \"a\", 'PmType' 'Quantitative']
             . 'text' ['TName' \"a\", 'TmType' 'Nominal']

    in 'toVegaLite' [ dvals []
                  , enc []
                  , 'layer' [ 'asSpec' ['mark' 'Bar' []]
                          , 'asSpec' ['mark' 'Text' ['MdY' (-8)]]
                          ]
                  ]
@

-}
layer :: [VLSpec] -> PropertySpec
layer specs = (VLLayer, toJSON specs)


{-|

Provides an optional name to be associated with the visualization.

@
'toVegaLite'
    [ 'name' \"PopGrowth\"
    , 'dataFromUrl' \"data/population.json\" []
    , 'mark' 'Bar' []
    , enc []
    ]
@
-}
name :: T.Text -> PropertySpec
name s = (VLName, toJSON s)


{-|

Set the padding around the visualization in pixel units. The way padding is
interpreted will depend on the 'autosize' properties. See the
<https://vega.github.io/vega-lite/docs/spec.html#top-level-specifications Vega-Lite documentation>
for details.

@
'toVegaLite'
    [ 'width' 500
    , 'padding' ('PEdges' 20 10 5 15)
    , 'dataFromUrl' "data/population.json" []
    , 'mark' 'Bar' []
    , enc []
    ]
@
-}
padding :: Padding -> PropertySpec
padding pad = (VLPadding, paddingSpec pad)


{-|

Define the fields that will be used to compose rows and columns of a
set of small multiples. This is used where the encoding of the
visualization in small multiples is largely identical, but the data
field used in each might vary. When a list of fields is identified
with @repeat@ you also need to define a full specification to apply to
each of those fields using 'asSpec'.

Unlike __faceting__, which creates multiple charts based on different values of a
single field, __repeating__ uses a different field for each chart.

See the
<https://vega.github.io/vega-lite/docs/repeat.html Vega-Lite documentation>
for further details.

@
'toVegaLite'
    [ 'repeat' ['ColumnFields' [\"Cat\", \"Dog\", \"Fish\"]]
    , 'specification' ('asSpec' spec)
    ]
@

See also 'repeatFlow'.

-}

repeat :: [RepeatFields] -> PropertySpec
repeat fields = (VLRepeat, object (map repeatFieldsProperty fields))


{-|

Define the fields that will be used to compose a flow layout of a set of
small multiples. Used when the encoding is largely identical, but the data field
used in each might vary. When a list of fields is identified with @repeatFlow@ you also
need to define a full specification to apply to each of those fields using 'asSpec'.

Small multiples will be laid out from left to right, moving on to new rows only
if the number of plots exceeds an optional column limit (specified via 'columns').

@
'toVegaLite'
    [ 'repeatFlow' [ \"Cat\", \"Dog\", \"Fish\" ]
    , 'specification' ('asSpec' spec)
    ]
@

See also 'repeat'.

@since 0.4.0.0

-}
repeatFlow :: [T.Text] -> PropertySpec
repeatFlow fields = (VLRepeat, toJSON fields)


{-|

Determine whether scales, axes or legends in composite views should
share channel encodings. This allows, for example, two different color
encodings to be created in a layered view, which otherwise by default
would share color channels between layers. Each resolution rule should
be in a tuple pairing the channel to which it applies and the rule
type.

@
let res = 'resolve'
            . 'resolution' ('RLegend' [('ChColor', 'Independent')])

in 'toVegaLite'
    [ 'dataFromUrl' \"data/movies.json\" []
    , 'vConcat' [heatSpec, barSpec]
    , res []
    ]
@

For more information see the
<https://vega.github.io/vega-lite/docs/resolve.html Vega-Lite documentation>.

@
let dvals = 'dataFromColumns' []
              . 'dataColumn' "x" ('Numbers' [1, 2, 3, 4, 5])
              . 'dataColumn' "a" ('Numbers' [28, 91, 43, 55, 81])
              . 'dataColumn' "b" ('Numbers' [17, 22, 28, 30, 40])
    encBar = 'encoding'
               . 'position' 'X' ['PName' \"x\", 'PmType' 'Quantitative']
               . 'position' 'Y' ['PName' \"a\", 'PmType' 'Quantitative']
    specBar = 'asSpec' ['mark' 'Bar' [], encBar []]
    encLine = 'encoding'
                . 'position' 'X' ['PName' \"x\", 'PmType' 'Quantitative']
                . 'position' 'Y' ['PName' \"b\", 'PmType' 'Quantitative']
    specLine = 'asSpec' ['mark' 'Line' ['MColor' \"firebrick\"], encLine []]
    res = 'resolve'
            . 'resolution' ('RScale' [('ChY', 'Independent')])

in 'toVegaLite' [dvals [], res [], 'layer' [specBar, specLine]]
@

-}
resolve :: [LabelledSpec] -> PropertySpec
resolve res = (VLResolve, object res)


{-|

Create a full selection specification from a list of selections. For details
see the
<https://vega.github.io/vega-lite/docs/selection.html Vega-Lite documentation>.

@
sel =
   'selection'
       . 'select' \"view\" 'Interval' ['BindScales'] []
       . 'select' \"myBrush\" 'Interval' []
       . 'select' \"myPaintbrush\" 'Multi' ['On' \"mouseover\", 'Nearest' True]
@

-}

selection :: [LabelledSpec] -> PropertySpec
selection sels = (VLSelection, object sels)


{-|

Defines a specification object for use with faceted and repeated small multiples.

@
'toVegaLite'
    [ 'facet' [ 'RowBy' [ 'FName' \"Origin\", 'FmType' 'Nominal' ] ]
    , 'specification' spec
    ]
@
-}
specification :: VLSpec -> PropertySpec
specification spec = (VLSpecification, spec)


{-|

Create a single transform from a list of transformation
specifications. Note that the order of transformations can be
important, especially if labels created with 'calculateAs',
'timeUnitAs', and 'binAs' are used in other transformations.  Using
the functional composition pipeline idiom (as example below) allows
you to provide the transformations in the order intended in a clear
manner.

@
'transform'
    . 'filter' ('FExpr' "datum.year == 2010")
    . 'calculateAs' "datum.sex == 2 ? \'Female\' : \'Male\'" "gender"
@

The supported transformations include: 'aggregate', 'binAs',
'calculateAs', 'impute', 'joinAggregate', 'lookup', 'lookupAs',
'flattenAs', 'foldAs', 'stack', 'timeUnitAs', and 'window'.

-}

transform :: [LabelledSpec] -> PropertySpec
transform transforms =
  let js = if null transforms then A.Null else toJSON (map assemble transforms)

      -- use the same approach as Elm of encoding the spec, then decoding it,
      -- rather than inspecting the structure of the JSON
      --
      assemble :: LabelledSpec -> VLSpec
      assemble (str, val) =

        let dval = decode (encode val) :: Maybe A.Value
        in case str of
          "aggregate" ->
            case dval of
              Just (A.Array vs) | V.length vs == 2 -> object [ ("aggregate", vs V.! 0)
                                                             , ("groupby", vs V.! 1) ]
              _ -> A.Null

          "bin" ->
            case dval of
              Just (A.Array vs) | V.length vs == 3 -> object [ ("bin", vs V.! 0)
                                                             , ("field", vs V.! 1)
                                                             , ("as", vs V.! 2) ]
              _ -> A.Null

          "calculate" ->
            case dval of
              Just (A.Array vs) | V.length vs == 2 -> object [ ("calculate", vs V.! 0)
                                                             , ("as", vs V.! 1) ]
              _ -> A.Null


          "impute" ->
            case dval of
              Just (A.Array vs) | V.length vs == 8 ->
                                    let [imp, key, frameObj, keyValsObj, keyValsSequenceObj, methodObj, groupbyObj, valueObj] = V.toList vs

                                        addField _ A.Null = []
                                        addField f v = [(f, v)]

                                        ols = [ ("impute", imp)
                                              , ("key", key) ]
                                              <> addField "frame" frameObj
                                              <> addField "keyvals" keyValsObj
                                              <> addField "keyvals" keyValsSequenceObj
                                              <> addField "method" methodObj
                                              <> addField "groupby" groupbyObj
                                              <> addField "value" valueObj

                                    in object ols
              _ -> A.Null

          "lookup" ->
            case dval of
              Just (A.Array vs) | V.length vs == 4 -> object [ ("lookup", vs V.! 0)
                                                             , ("from",
                                                                object [ ("data", vs V.! 1)
                                                                       , ("key", vs V.! 2)
                                                                       , ("fields", vs V.! 3) ] )
                                                             ]
              _ -> A.Null

          "lookupAs" ->
            case dval of
              Just (A.Array vs) | V.length vs == 4 -> object [ ("lookup", vs V.! 0)
                                                             , ("from",
                                                                object [ ("data", vs V.! 1)
                                                                       , ("key", vs V.! 2) ] )
                                                             , ("as", vs V.! 3) ]
              _ -> A.Null

          "flattenAs" ->
            case dval of
              Just (A.Array vs) | V.length vs == 2 -> object [ ("flatten", vs V.! 0)
                                                             , ("as", vs V.! 1) ]
              _ -> A.Null

          "foldAs" ->
            case dval of
              Just (A.Array vs) | V.length vs == 3 -> object [ ("fold", vs V.! 0)
                                                             , ("as", toJSON [vs V.! 1, vs V.! 2]) ]
              _ -> A.Null

          "stack" ->
            case dval of
              Just (A.Array vs) | V.length vs == 6 ->
                                    let [field, grp, start, end, offsetObj, sortObj] = V.toList vs

                                        addField _ A.Null = []
                                        addField f v = [(f, v)]

                                        ols = [ ("stack", field)
                                              , ("groupby", grp)
                                              , ("as", toJSON [start, end]) ]
                                              <> addField "offset" offsetObj
                                              <> addField "sort" sortObj

                                    in object ols
              _ -> A.Null

          "timeUnit" ->
            case dval of
              Just (A.Array vs) | V.length vs == 3 -> object [ ("timeUnit", vs V.! 0)
                                                             , ("field", vs V.! 1)
                                                             , ("as", vs V.! 2) ]
              _ -> A.Null

          "window" ->
            case dval of
              Just (A.Array vs) | V.length vs == 5 ->
                                    let [winObj, frameObj, peersObj, groupbyObj, sortObj] = V.toList vs

                                        addField _ A.Null = []
                                        addField f v = [(f, v)]

                                        ols = [("window", winObj)]
                                              <> addField "frame" frameObj
                                              <> addField "ignorePeers" peersObj
                                              <> addField "groupby" groupbyObj
                                              <> addField "sort" sortObj

                                    in object ols
              _ -> A.Null

          "joinaggregate" ->
            case dval of
              Just (A.Array vs) | V.length vs == 5 ->
                                    let [joinObjs, frameObj, peersObj, groupbyObj, sortObj] = V.toList vs

                                        addField _ A.Null = []
                                        addField f v = [(f, v)]

                                        ols = [("joinaggregate", joinObjs)]
                                              <> addField "frame" frameObj
                                              <> addField "ignorePeers" peersObj
                                              <> addField "groupby" groupbyObj
                                              <> addField "sort" sortObj

                                    in object ols
              _ -> A.Null

          _ -> object [(str, val)]

    in (VLTransform, js)


{-|

Assigns a list of specifications to be juxtaposed vertically in a visualization.

@
'toVegaLite'
    [ 'dataFromUrl' "data/driving.json" []
    , 'vConcat' [ spec1, spec2 ]
    ]
@
-}
vConcat :: [VLSpec] -> PropertySpec
vConcat specs = (VLVConcat, toJSON specs)


{-|

Override the default width of the visualization. If not specified the width
will be calculated based on the content of the visualization. See
'autosize' for customization of the content sizing relative to this
setting.

@
'toVegaLite'
    [ 'width' 500
    , 'dataFromUrl' "data/population.json" []
    , 'mark' 'Bar' []
    , enc []
    ]
@
-}
width :: Double -> PropertySpec
width w = (VLWidth, toJSON w)


-- | Properties for a window transform.
--
--   @since 0.4.0.0

data WindowProperty
    = WFrame (Maybe Int) (Maybe Int)
      -- ^ Moving window for use by a window transform. When a number is
      --   given, via @Just@, then it indicates the offset from the current
      --   data object. A @Nothing@ indicates an un-bounded number of rows
      --   preceding or following the current data object.
    | WIgnorePeers Bool
      -- ^ Should the sliding window in a window transform ignore peer
      --   values (those considered identical by the sort criteria).
    | WGroupBy [T.Text]
      -- ^ The fields for partitioning data objects in a window transform
      --   into separate windows. If not specified, all points will be in a
      --   single group.
    | WSort [SortField]
      -- ^ Comparator for sorting data objects within a window transform.


-- This is different to how Elm's VegaLite handles this (as of version 1.12.0)
-- Helpers for windowPropertySpec

-- allowNull :: A.ToJSON a => Maybe a -> VLSpec
allowNull :: Maybe Int -> VLSpec
allowNull (Just a) = toJSON a
allowNull Nothing = A.Null

wpFrame , wpIgnorePeers, wpGroupBy, wpSort :: WindowProperty -> Maybe VLSpec
wpFrame (WFrame m1 m2) = Just (toJSON [allowNull m1, allowNull m2])
wpFrame _ = Nothing

wpIgnorePeers (WIgnorePeers b) = Just (toJSON b)
wpIgnorePeers _ = Nothing

wpGroupBy (WGroupBy fs) = Just (toJSON fs)
wpGroupBy _ = Nothing

wpSort (WSort sfs) = Just (toJSON (map sortFieldSpec sfs))
wpSort _ = Nothing

windowPropertySpec :: [WindowProperty] -> [VLSpec]
windowPropertySpec wps =
  let frms = mapMaybe wpFrame wps
      ips = mapMaybe wpIgnorePeers wps
      gps = mapMaybe wpGroupBy wps
      sts = mapMaybe wpSort wps

      fromSpecs [spec] = spec
      fromSpecs _ = A.Null

  in map fromSpecs [frms, ips, gps, sts]


-- | How should the field be sorted when performing a window transform.
--
--   @since 0.4.00

data SortField
    = WAscending T.Text
    -- ^ Sort the field into ascending order.
    | WDescending T.Text
    -- ^ Sort the field into descending order.


sortFieldSpec :: SortField -> VLSpec
sortFieldSpec (WAscending f) = object [field_ f, order_ "ascending"]
sortFieldSpec (WDescending f) = object [field_ f, order_ "descending"]


-- | Window transformations.
--
--   @since 0.4.0.0

data Window
    = WAggregateOp Operation
      -- ^ An aggregrate operation to be used in a window transformation.
    | WOp WOperation
      -- ^ Window-specific operation to be used in a window transformation.
    | WParam Int
      -- ^ Numeric parameter for window-only operations that can be parameterised
      --   ('Ntile', 'Lag', 'Lead' and 'NthValue').
    | WField T.Text
      -- ^ Field for which to compute a window operation. Not needed for operations
      --   that do not apply to fields such as 'Count', 'Rank', and 'DenseRank'.


windowFieldProperty :: Window -> LabelledSpec
windowFieldProperty (WAggregateOp op) = "op" .= operationSpec op
windowFieldProperty (WOp op) = "op" .= wOperationLabel op
windowFieldProperty (WParam n) = "param" .= n
windowFieldProperty (WField f) = field_ f


-- | Window-specific operation for transformations (for use with 'WOp').
--
--   @since 0.4.0.0

data WOperation
    = RowNumber
      -- ^ Assign consecutive row number to values in a data object to be applied in a window transform.
    | Rank
      -- ^ Rank function to be applied in a window transform.
    | DenseRank
      -- ^ Dense rank function to be applied in a window transform.
    | PercentRank
      -- ^ Percentile of values in a sliding window to be applied in a window transform.
    | CumeDist
      -- ^ Cumulative distribution function to be applied in a window transform.
    | Ntile
      -- ^ Value preceding the current object in a sliding window to be applied in a window transform.
    | Lag
      -- ^ Value preceding the current object in a sliding window to be applied in a window transform.
    | Lead
      -- ^ Value following the current object in a sliding window to be applied in a window transform.
    | FirstValue
      -- ^ First value in a sliding window to be applied in a window transform.
    | LastValue
      -- ^ Last value in a sliding window to be applied in a window transform.
    | NthValue
      -- ^ Nth value in a sliding window to be applied in a window transform.


wOperationLabel :: WOperation -> T.Text
wOperationLabel RowNumber = "row_number"
wOperationLabel Rank = "rank"
wOperationLabel DenseRank = "dense_rank"
wOperationLabel PercentRank = "percent_rank"
wOperationLabel CumeDist = "cume_dist"
wOperationLabel Ntile = "ntile"
wOperationLabel Lag = "lag"
wOperationLabel Lead = "lead"
wOperationLabel FirstValue = "first_value"
wOperationLabel LastValue = "last_value"
wOperationLabel NthValue = "nth_value"


{-|

Defines a set of named aggregation transformations to be used when encoding
channels. This is useful when, for example, you wish to apply the same transformation
to a number of channels but do not want to define it each time. For further details
see the
<https://vega.github.io/vega-lite/docs/aggregate.html#aggregate-op-def Vega-Lite documentation>.

@
'transform'
    . 'aggregate'
        [ 'opAs' 'Min' "people" "lowerBound", 'opAs' 'Max' "people" "upperBound" ]
        [ "age" ]
@

See also 'joinAggregate'.

-}
aggregate ::
  [VLSpec]
  -- ^ The named aggregation operations to apply.
  -> [T.Text]
  -- ^ The \"group by\" fields.
  -> BuildLabelledSpecs
aggregate ops groups ols =
  let ags = toJSON [toJSON ops, toJSON (map toJSON groups)]
  in ("aggregate", ags) : ols


{-|

Aggregation transformations to be used when encoding channels. Unlike
'aggregate', this transformation joins the results to the input data.
Can be helpful for creating derived values that combine raw data with some aggregate
measure, such as percentages of group totals. The first parameter is a list
of the named aggregation operations to apply. The second is a list of possible
window aggregate field properties, such as a field to group by when aggregating.
The third parameter is a list of transformations to which this is added.

@
'transform'
    . 'joinAggregate'
        [ 'opAs' 'Mean' "rating" "avYearRating" ]
        [ 'WGroupBy' [ "year" ] ]
    . 'filter' ('FExpr' "(datum.rating - datum.avYearRating) > 3"))
@

For details, see the
[Vega-Lite join aggregate documentation](https://vega.github.io/vega-lite/docs/joinaggregate.html)

See also 'aggregate'.

@since 0.4.0.0
-}

joinAggregate ::
  [VLSpec]
  -> [WindowProperty]
  -> BuildLabelledSpecs
joinAggregate ops wProps ols =
  let ags = toJSON ops : windowPropertySpec wProps
  in ("joinaggregate", toJSON ags) : ols


{-|

Window transform for performing calculations over sorted groups of
data objects such as ranking, lead/lag analysis, running sums and averages.

The first parameter is a list of tuples each comprising a window transform field
definition and an output name. The second is the window transform definition.

@
'transform'
    . 'window' [ ( [ 'WAggregateOp' 'Sum', 'WField' "Time" ], "TotalTime" ) ]
             [ 'WFrame' Nothing Nothing ]
@

@since 0.4.0.0

-}
window ::
  [([Window], T.Text)]
  -- ^ The window-transform definition and associated output name.
  -> [WindowProperty]
  -- ^ The window transform.
  -> BuildLabelledSpecs
window wss wProps ols =
  let args = toJSON wargs : windowPropertySpec wProps
      wargs = map winFieldDef wss
      winFieldDef (ws, out) = object ("as" .= out : map windowFieldProperty ws)
  in ("window" .= toJSON args) : ols


{-|

Randomly sample rows from a data source up to a given maximum.

For example, the following randomly samples 50 values from a sine curve:

@
 dvals = 'dataSequenceAs' 0 13 0.001 \"x\"
 trans = 'transform'
           . 'calculateAs' \"sin(datum.x)\" \"y\"
           . 'sample' 50
@

@since 0.4.0.0

-}

sample :: Int -> BuildLabelledSpecs
sample maxSize ols = ("sample" .= maxSize) : ols


{-|

Create a named binning transformation that may be referenced in other Transformations
or encodings. See the
<https://vega.github.io/vega-lite/docs/bin.html Vega-Lite documentation> for
more details. Note that usually, direct binning within an encoding is preferred
over this form of bin transformation.

@
'transform'
    . 'binAs' [ 'MaxBins' 3 ] \"IMDB_Rating\" \"ratingGroup\"
@
-}
binAs ::
  [BinProperty]
  -- ^ An empty list means that the default binning is used (that is, the
  --   @bin@ field will be set to @true@ in the Vega-Lite specification).
  -> T.Text
  -- ^ The field to bin.
  -> T.Text
  -- ^ The label for the binned data.
  -> BuildLabelledSpecs
binAs bProps field label ols =
  let js = if null bProps
           then [toJSON True, toJSON field, toJSON label]
           else [object (map binProperty bProps), toJSON field, toJSON label]
 in ("bin" .= js) : ols


{-|

Creates a new data field based on calculations from existing fields and values.

See the <https://vega.github.io/vega-lite/docs/calculate.html Vega-Lite documentation>
for further details.

@
'transform' . 'calculateAs' "datum.sex == 2 ? \'F\' : \'M\'" "gender"
@
-}
calculateAs ::
  T.Text
  -- ^ The calculation to perform, supporting the
  --   [Vega-Lite expression syntax](https://vega.github.io/vega/docs/expressions/).
  -> T.Text
  -- ^ The field to assign the new values.
  -> BuildLabelledSpecs
calculateAs expr label ols = ("calculate" .= [expr, label]) : ols


{-|

Encode a color channel.

@
'color' [ 'MName' \"Species\", 'MmType' 'Nominal' ] []
@

Encoding a color channel will generate a legend by default. To stop the legend
appearing, just supply an empty list of legend properties to 'MLegend':

@
'color' [ 'MName' \"Species\", 'MmType' 'Nominal', 'MLegend' [] ] []
@
-}
color ::
  [MarkChannel]
  -- ^ The color-encoding options.
  -> BuildLabelledSpecs
color markProps ols = mchan_ "color" markProps : ols


{-|

Encodes a new facet to be arranged in columns. See the
<https://vega.github.io/vega-lite/docs/facet.html#facet-row-and-column-encoding-channels Vega-Lite column documentation>.

Note that when faceting, dimensions specified with 'width' and 'height'
refer to the individual faceted plots, not the overall visualization.

@
let dvals = 'dataFromUrl' \"crimeData.csv\"
    enc = 'encoding'
            . 'position' 'X' ['PName' \"month\", 'PmType' 'Temporal']
            . 'position' 'Y' ['PName' \"reportedCrimes\", 'PmType' 'Quantitative'
                         , 'PAggregate' 'Sum']
            . 'column' ['FName' \"crimeType\", 'FmType' 'Nominal']

    in 'toVegaLite' ['width' 100, dvals [], 'mark' 'Bar' [], enc [] ]
@
-}
column ::
  [FacetChannel]
  -- ^ The list of properties that define the faceting channel. At a minimum
  --   this should include the data field ('FName') and its measurement type
  --   ('FmType').
  -> BuildLabelledSpecs
column fFields ols =
  ("column" .= object (map facetChannelProperty fFields)) : ols


{-|

The maximum number of columns to include in a view composition flow
layout. If the number of faceted small multiples exceeds this number,
flow moves to the next row.  Only applies to flow layouts generated by
'vlConcat', 'facetFlow', and 'repeatFlow'.

@since 0.4.0.0

-}

columns ::
  Natural
  -- ^ A value of 0 means that a single row will be used (which is also
  --   the default behavior).
  -> PropertySpec
columns cols = (VLColumns, toJSON cols)


{-|

Defines a single configuration option to be applied globally across the visualization.
The first parameter identifies the type of configuration, the second a list of previous
configurations to which this one may be added.

@
'configuration' ('Axis' [ 'DomainWidth' 4 ]) []
@
-}
configuration :: ConfigurationProperty -> BuildLabelledSpecs
configuration cfg ols = configProperty cfg : ols


{-|

Encode a \"level of detail\" channel. This provides a way of grouping by a field
but unlike, say 'color', all groups have the same visual properties.

See the
<https://vega.github.io/vega-lite/docs/encoding.html#detail Vega-Lite documentation>
for details.

@
'detail' ['DName' \"Species\", 'DmType' 'Nominal'] []
@
-}
detail ::
  [DetailChannel]
  -- ^ The field to group.
  -> BuildLabelledSpecs
detail detailProps ols =
    ("detail" .= object (map detailChannelProperty detailProps)) : ols


{-|

Encode a fill channel. This acts in a similar way to encoding by 'color' but
only affects the interior of closed shapes. The first parameter is a list of mark
channel properties that characterise the way a data field is encoded by fill.
The second parameter is a list of any previous channels to which this fill channel
should be added.

@
'fill' [ 'MName' \"Species\", 'MmType' 'Nominal' ] []
@

Note that if both @fill@ and 'color' encodings are specified, @fill@ takes precedence.

-}

fill :: [MarkChannel] -> BuildLabelledSpecs
fill markProps ols = mchan_ "fill" markProps : ols


{-|

Encode a fill opacity channel. This acts in a similar way to encoding by 'opacity'
but only affects the interior of closed shapes. If both @fillOpacity@ and 'opacity'
encodings are specified, @fillOpacity@ takes precedence.

See also 'fill'.

@since 0.4.0.0
-}

fillOpacity :: [MarkChannel] -> BuildLabelledSpecs
fillOpacity markProps ols = mchan_ "fillOpacity" markProps : ols


{-|

Adds the given filter operation a list of transformations that may be applied
to a channel or field.

@
'transform'
    . 'filter' ('FEqual' \"Animal\" ('Str' \"Cat\"))
@

Filter operations can combine selections and data predicates with 'BooleanOp'
expressions (and as of @0.4.0.0@, 'FilterOp' and 'FilterOpTrans'
can be used to lift the 'Filter' type into boolean expressions):

@
'transform'
    . 'filter' ('FCompose' ('And' ('Expr' "datum.Weight_in_lbs > 3000") ('Selection' "brush")))
@

The [Vega-Lite expression documentation](https://vega.github.io/vega/docs/expressions/)
describes the supported format (e.g. the requirement to precede column names
with @"datum."@).

-}
filter :: Filter -> BuildLabelledSpecs
filter f ols = ("filter" .= filterSpec f) : ols



{-|

Map array-valued fields to a set of individual data objects, one per array entry.

See also 'flattenAs'.

@since 0.4.0.0

-}

flatten :: [T.Text] -> BuildLabelledSpecs
flatten fields ols = ("flatten" .= fields) : ols


{-|

Similar to 'flatten' but allows the new output fields to be named.

@since 0.4.0.0

-}

flattenAs ::
  [T.Text]
  -> [T.Text]
  -- ^ The names of the output fields.
  -> BuildLabelledSpecs
flattenAs fields names ols = ("flattenAs" .= [fields, names]) : ols


{-|

Perform a /gather/ operation to /tidy/ a table. Collapse multiple data fields
into two new data fields: @key@ containing the original data field names and @value@
containing the corresponding data values. This performs the same function as the
<https://tidyr.tidyverse.org/dev/articles/pivot.html pivot_longer> and
<https://tidyr.tidyverse.org/reference/gather.html gather>
operations in the R tidyverse.

See also 'foldAs'.

@
dvals =
    'dataFromColumns' []
        . 'dataColumn' \"city\" ('Strings' [ \"Bristol\", \"Sheffield\", \"Glasgow\" ])
        . 'dataColumn' \"temp2017\" ('Numbers' [ 12, 11, 7 ])
        . 'dataColumn' \"temp2018\" ('Numbers' [ 14, 13, 10 ])

trans =
    'transform'
        . 'fold' [ \"temp2017\", \"temp2018\" ]

enc =
    'encoding'
        . 'position' 'X' [ 'PName' \"key\", 'PmType' 'Nominal' ]
        . 'position' 'Y' [ 'PName' \"city\", 'PmType' 'Nominal' ]
        . 'size' [ 'MName' \"value\", 'MmType' 'Quantitative' ]
@

@since 0.4.0.0
-}

fold :: [T.Text] -> BuildLabelledSpecs
fold fields ols = ("fold" .= fields) : ols


{-|

A 'fold' where the @key@ and @value@ fields can be renamed.

@since 0.4.0.0

-}

foldAs ::
  [T.Text]
  -> T.Text
  -- ^ The name for the @key@ field.
  -> T.Text
  -- ^ The name for the @value@ field.
  -> BuildLabelledSpecs
foldAs fields keyName valName ols =
  ("foldAs" .= [toJSON fields, fromT keyName, fromT valName]) : ols


{-|

Encode a hyperlink channel.

@
'encoding'
  . 'hyperlink' [ 'HName' \"Species\", 'HmType' 'Nominal' ]
@

@
'encoding'
  . 'hyperlink' [ 'HString' \"http://www.imdb.com\" ]
@

For further details see the
<https://vega.github.io/vega-lite/docs/encoding.html#href Vega-Lite documentation>.

-}
hyperlink ::
  [HyperlinkChannel]
  -- ^ The properties for the hyperlink channel.
  -> BuildLabelledSpecs
hyperlink hyperProps ols =
  ("href" .= object (concatMap hyperlinkChannelProperty hyperProps)) : ols


{-|

Perform a lookup of named fields between two data sources. This allows
you to find values in one data source based on the values in another
(like a relational join).

Unlike 'lookupAs', this function will only return the specific fields
named in the fourth parameter. If you wish to return the entire set of
fields in the secondary data source as a single object, use
'lookupAs'.

See the <https://vega.github.io/vega-lite/docs/lookup.html Vega-Lite documentation>
for further details.

The following would return the values in the @age@ and @height@ fields from
@lookup_people.csv@ for all rows where the value in the @name@ column in that
file matches the value of @person@ in the primary data source.

@
trans = 'transform'
          . 'lookup' \"person\" ('dataFromUrl' \"data/lookup_people.csv\" []) \"name\" [\"age\", \"height\"]
@
-}
lookup ::
  T.Text
  -- ^ The field in the primary data structure acting as the key.
  -> Data
  -- ^ The secondary data source (e.g. the return from the data-generating
  --   functions such as 'dataFromUrl').
  -> T.Text
  -- ^ The name of the field in the secondary data source to match against
  --   the primary key.
  -> [T.Text]
  -- ^ The list of fields to store when the keys match.
  -> BuildLabelledSpecs
lookup key1 (_, spec) key2 fields ols =
  let js = [toJSON key1, spec, toJSON key2, toJSON (map toJSON fields)]
  in ("lookup" .= js) : ols


{-|

Perform an object lookup between two data sources. This allows you to
find values in one data source based on the values in another (like a
relational join).  Unlike 'lookup', this function returns the entire
set of field values from the secondary data source when keys
match. Those fields are stored as an object with the name provided in
the fourth parameter.

In the following example, @personDetails@ would reference all the
field values in @lookup_people.csv@ for each row where the value in
the @name@ column in that file matches the value of @person@ in the
primary data source.

@
'transform'
    . 'lookupAs' "person" ('dataFromUrl' "data/lookup_people.csv" []) "name" "personDetails"
@

If the data contained columns called @age@ and @height@ then they would
then be accessed as @personDetails.age@ and @personDetails.height@ - for
example:

@
'encoding'
  . 'position' X ['PName' "personDetails.age", 'PmType' 'Temporal', 'PTimeUnit' 'Year', 'PTitle' \"Age\"]
  . 'position' Y ['PName' "personDetails.height", 'PmType' 'Quantitative', 'PTitle' \"Height\"]
@

See the
<https://vega.github.io/vega-lite/docs/lookup.html Vega-Lite documentation>
for further details.

-}
lookupAs ::
  T.Text
  -- ^ The field in the primary data structure acting as the key.
  -> Data
  -- ^ The secondary data source (e.g. the return from the data-generating
  --   functions such as 'dataFromUrl').
  -> T.Text
  -- ^ The name of the field in the secondary data source to match against
  --   the primary key.
  -> T.Text
  -- ^ The field name for the new data.
  -> BuildLabelledSpecs
lookupAs key1 (_, spec) key2 asName ols =
  ("lookupAs" .= [toJSON key1, spec, toJSON key2, toJSON asName]) : ols


-- | This is used with `impute` and `PImpute`.
--
--   @since 0.4.0.0

data ImputeProperty
    = ImFrame (Maybe Int) (Maybe Int)
      -- ^ 1d window over which data imputation values are generated. The two
      --   parameters should either be @Just@ a number indicating the offset from the current
      --   data object, or @Nothing@ to indicate unbounded rows preceding or following the
      --   current data object.
    | ImKeyVals DataValues
      -- ^ Key values to be considered for imputation.
    | ImKeyValSequence Double Double Double
      -- ^ Key values to be considered for imputation as a sequence of numbers between
      --   a start (first parameter), to less than an end (second parameter) in steps of
      --   the third parameter.
    | ImMethod ImMethod
      -- ^ How is the imputed value constructed.
      --
      --   When using @ImMethod 'ImValue'@, the replacement value is
      --   set with 'ImNewValue'.
    | ImGroupBy [T.Text]
      -- ^ Allow imputing of missing values on a per-group basis. For use with the impute
      --   transform only and not a channel encoding.
    | ImNewValue DataValue
      -- ^ The replacement value (when using @ImMethod 'ImValue'@).


imputeProperty :: ImputeProperty -> LabelledSpec
imputeProperty (ImFrame m1 m2) = "frame" .= map allowNull [m1, m2]
imputeProperty (ImKeyVals dVals) = "keyvals" .= dataValuesSpecs dVals
imputeProperty (ImKeyValSequence start stop step) =
  "keyvals" .= object ["start" .= start, "stop" .= stop, "step" .= step]
imputeProperty (ImMethod method) = "method" .= imMethodLabel method
imputeProperty (ImNewValue dVal) = "value" .= dataValueSpec dVal
imputeProperty (ImGroupBy _) = "groupby" .= A.Null


imputePropertySpecFrame, imputePropertySpecKeyVals,
  imputePropertySpecKeyValSequence, imputePropertySpecGroupBy,
  imputePropertySpecMethod, imputePropertySpecValue :: ImputeProperty -> Maybe VLSpec

imputePropertySpecFrame (ImFrame m1 m2) = Just (toJSON (map allowNull [m1, m2]))
imputePropertySpecFrame _ = Nothing

imputePropertySpecKeyVals (ImKeyVals dVals) = Just (toJSON (dataValuesSpecs dVals))
imputePropertySpecKeyVals _ = Nothing

imputePropertySpecKeyValSequence (ImKeyValSequence start stop step) =
  let obj = ["start" .= start, "stop" .= stop, "step" .= step]
  in Just (object obj)
imputePropertySpecKeyValSequence _ = Nothing

imputePropertySpecGroupBy (ImGroupBy fields) = Just (toJSON fields)
imputePropertySpecGroupBy _ = Nothing

imputePropertySpecMethod (ImMethod method) = Just (toJSON (imMethodLabel method))
imputePropertySpecMethod _ = Nothing

imputePropertySpecValue (ImNewValue dVal) = Just (dataValueSpec dVal)
imputePropertySpecValue _ = Nothing



-- | Imputation method to use when replacing values.
--
--   @since 0.4.0.0

data ImMethod
  = ImMin
    -- ^ Use the minimum value.
  | ImMax
    -- ^ Use the maximum value.
  | ImMean
    -- ^ Use the mean value.
  | ImMedian
    -- ^ Use the median value.
  | ImValue
    -- ^ Use a replacement value (set with @ImNewValue@).


imMethodLabel :: ImMethod -> T.Text
imMethodLabel ImMin = "min"
imMethodLabel ImMax = "max"
imMethodLabel ImMean = "mean"
imMethodLabel ImMedian = "median"
imMethodLabel ImValue = "value"

{-|

Impute missing data values.

The following example creates a value for @b@, set to the
mean of existing @b@ values with @c=1@, for the \"missing\" coordinate
of (@a=30@, @c=1@):

@
let dvals = 'dataFromColumns' []
              . 'dataColumn' "a" ('Numbers' [0, 0, 10, 10, 20, 20, 30])
              . 'dataColumn' "b" ('Numbers' [28, 91, 43, 55, 81, 53, 19])
              . 'dataColumn' "c" ('Numbers' [0, 1, 0, 1, 0, 1, 0])

    trans = 'transform'
              . 'impute' "b" "a" ['ImMethod' 'ImMean', 'ImGroupBy' ["c"]]

    enc = 'encoding'
            . 'position' 'X' ['PName' \"a\", 'PmType' 'Quantitative']
            . 'position' 'Y' ['PName' \"b\", 'PmType' 'Quantitative']
            . 'color' ['MName' \"c\", 'MmType' 'Nominal']

    in 'toVegaLite' [dvals [], trans [], enc [], 'mark' 'Line' []]
@

@since 0.4.0.0
-}

impute ::
  T.Text
  -- ^ The data field to process.
  -> T.Text
  -- ^ The key field to uniquely identify data objects within a group.
  -> [ImputeProperty]
  -- ^ Define how the imputation works.
  -> BuildLabelledSpecs
impute fields key imProps ols =
  let ags = [ fromT fields, fromT key
            , toSpec (mapMaybe imputePropertySpecFrame imProps)
            , toSpec (mapMaybe imputePropertySpecKeyVals imProps)
            , toSpec (mapMaybe imputePropertySpecKeyValSequence imProps)
            , toSpec (mapMaybe imputePropertySpecMethod imProps)
            , toSpec (mapMaybe imputePropertySpecGroupBy imProps)
            , toSpec (mapMaybe imputePropertySpecValue imProps) ]

      toSpec [x] = x
      toSpec _ = A.Null

  in ("impute", toJSON ags) : ols


{-|

Encode an opacity channel. The first parameter is a list of mark
channel properties that characterise the way a data field is encoded
by opacity. The second parameter is a list of any previous channels to
which this opacity channel should be added.

@
'opacity' [ 'MName' \"Age\", 'MmType' 'Quantitative' ] []
@

See also 'fillOpacity'.

-}

opacity :: [MarkChannel] -> BuildLabelledSpecs
opacity markProps ols = mchan_ "opacity" markProps : ols


{-|

Encode an order channel.

@
enc =
    'encoding'
        . 'position' 'X' [ 'PName' "miles", 'PmType' 'Quantitative' ]
        . 'position' 'Y' [ 'PName' "gas", 'PmType' 'Quantitative' ]
        . 'order' [ 'OName' "year", 'OmType' 'Temporal', 'OSort' ['Descending'] ]
@
-}
order ::
  [OrderChannel]
  -- ^ The order-encoding options.
  -> BuildLabelledSpecs
order oDefs ols =
  ("order" .= object (map orderChannelProperty oDefs)) : ols


{-|

Encode a position channel.

@
enc =
    'encoding'
      . 'position' 'X' [ 'PName' \"Animal\", 'PmType' 'Ordinal' ]
@

Encoding by position will generate an axis by default. To prevent the axis from
appearing, simply provide an empty list of axis properties to 'PAxis':

@
enc =
    'encoding'
      . 'position' 'X' [ 'PName' \"Animal\", 'PmType' 'Ordinal', 'PAxis' [] ]
@
-}
position ::
  Position
  -- ^ The channel to encode.
  -> [PositionChannel]
  -- ^ The options for the channel; this will usually include the name ('PName')
  --    and measurement type ('PmType'), but can be a reference to a row or
  --    column repeat field.
  -> BuildLabelledSpecs
position pos pDefs ols =
  let defs = object (map positionChannelProperty pDefs)
  in (positionLabel pos, defs) : ols


{-|

Define a single resolution option to be applied when scales, axes or legends
in composite views share channel encodings. This allows, for example, two different
color encodings to be created in a layered view, which otherwise by default would
share color channels between layers. Each resolution rule should be in a tuple
pairing the channel to which it applies and the rule type.

@
'resolve'
    . resolution ('RScale' [ ( 'ChY', 'Independent' ) ])
@
-}
resolution :: Resolve -> BuildLabelledSpecs
resolution res ols = resolveProperty res : ols


{-|

Encode a new facet to be arranged in rows.

See the
<https://vega.github.io/vega-lite/docs/facet.html#facet-row-and-column-encoding-channels Vega-Lite row documentation>.

Note that when faceting, dimensions specified with 'width' and 'height'
refer to the individual faceted plots, not the whole visualization.

@
let dvals = 'dataFromUrl' \"crimeData.csv\"
    enc = 'encoding'
            . 'position' 'X' ['PName' \"month\", 'PmType' 'Temporal']
            . 'position' 'Y' ['PName' \"reportedCrimes\"
                         , 'PmType' 'Quantitative'
                         , 'PAggregate' 'Sum'
                         , 'PAxis' ['AxNoTitle']
                         ]
            . 'row' ['FName' \"crimeType\", 'FmType' 'Nominal']

in 'toVegaLite' ['height' 80, dvals [], 'mark' 'Bar' [], enc []]
@

-}
row ::
  [FacetChannel]
  -- ^ The facet properties for the channel; this should include the name of
  --   the field ('FName') and its measurement type ('FmType').
  -> BuildLabelledSpecs
row fFields ols = ("row" .= object (map facetChannelProperty fFields)) : ols


{-|

Create a single named selection that may be applied to a data query or transformation.

@
sel =
    'selection'
        . 'select' "view" 'Interval' [ 'BindScales' ] []
        . 'select' "myBrush" 'Interval' []
        . 'select' "myPaintbrush" 'Multi' [ 'On' "mouseover", 'Nearest' True ]
@

-}
select ::
  T.Text
  -- ^ The name given to the selection.
  -> Selection
  -- ^ The type of the selection.
  -> [SelectionProperty]
  -- ^ What options are applied to the selection.
  -> BuildLabelledSpecs
select nme sType options ols =
    let selProps = ("type" .= selectionLabel sType) : map selectionProperty options
    in (nme .= object selProps) : ols


{-|

Encode a shape channel.

@
'shape' [ 'MName' \"Species\", 'MmType' 'Nominal' ] []
@
-}
shape ::
  [MarkChannel]
  -- ^ What data values are used to control the shape parameters of the mark.
  -> BuildLabelledSpecs
shape markProps ols = mchan_ "shape" markProps : ols


{-|

Encode a size channel.

@
'size' [ 'MName' \"Age\", 'MmType' 'Quantitative' ] []
@
-}
size ::
  [MarkChannel]
  -- ^ What data values are used to control the size parameters of the mark.
  -> BuildLabelledSpecs
size markProps ols = mchan_ "size" markProps : ols


{-|

Encode a stroke channel. This acts in a similar way to encoding by 'color' but
only affects the exterior boundary of marks.

@
'stroke' [ 'MName' \"Species\", 'MmType' 'Nominal' ] []
@

Note that if both @stroke@ and 'color' encodings are specified, @stroke@ takes
precedence.

-}
stroke ::
  [MarkChannel]
  -- ^ What data values are used to control the stoke parameters of the mark.
  -> BuildLabelledSpecs
stroke markProps ols = mchan_ "stroke" markProps : ols


{-|

Encode a stroke opacity channel. This acts in a similar way to encoding by
'opacity' but only affects the exterior boundary of marks. If both 'opacity' and
@strokeOpacity@ are specified, @strokeOpacity@ takes precedence for stroke encoding.

@since 0.4.0.0

-}

strokeOpacity ::
  [MarkChannel]
  -- ^ What data values are used to control the stoke opacity parameters of the mark.
  -> BuildLabelledSpecs
strokeOpacity markProps ols = mchan_ "strokeOpacity" markProps : ols


{-|

Encode a stroke width channel.

@since 0.4.0.0

-}

strokeWidth ::
  [MarkChannel]
  -- ^ What data values are used to control the stoke width parameters of the mark.
  -> BuildLabelledSpecs
strokeWidth markProps ols = mchan_ "strokeWidth" markProps : ols


{-|

Encode a text channel. See the
<https://vega.github.io/vega-lite/docs/encoding.html#text Vega-Lite documentation>
for further details on the text and tooltip channels and
<https://vega.github.io/vega-lite/docs/format.html Vega-Lite formatting documentation>
for formatting the appearance of the text.

@
'encoding'
    . 'position' 'X' [ 'PName' "miles", 'PmType' 'Quantitative' ]
    . 'position' 'Y' [ 'PName' "gas", 'PmType' 'Quantitative' ]
    . 'text' [ 'TName' "miles", 'TmType' 'Quantitative' ]
@
-}
text ::
  [TextChannel]
  -- ^ What data values are used to control the text parameters.
  -> BuildLabelledSpecs
text tDefs ols =
  ("text" .= object (concatMap textChannelProperty tDefs)) : ols


{-|

Creates a new data field based on the given temporal binning. Unlike the
direct encoding binning, this transformation is named and so can be referred
to in multiple encodings. Note though that usually it is easer to apply the temporal
binning directly as part of the encoding as this will automatically format the
temporal axis. See the
<https://vega.github.io/vega-lite/docs/timeunit.html#transform Vega-Lite documentation>
for further details.

The following example takes a temporal dataset and encodes daily totals from it
grouping by month:

@
trans = 'transform' . 'timeUnitAs' 'Month' \"date\" \"monthly\"

enc = 'encoding'
        . 'position' 'X' [ 'PName' \"date\", 'PmType' 'Temporal', 'PTimeUnit' 'Day' ]
        . 'position' 'Y' [ 'PAggregate' 'Sum', 'PmType' 'Quantitative' ]
        . 'detail' [ 'DName' \"monthly\", 'DmType' 'Temporal' ]
@
-}
timeUnitAs ::
  TimeUnit
  -- ^ The width of each bin.
  -> T.Text
  -- ^ The field to bin.
  -> T.Text
  -- ^ The name of the binned data created by this routine.
  -> BuildLabelledSpecs
timeUnitAs tu field label ols =
  ("timeUnit" .= [timeUnitLabel tu, field, label]) : ols


{-|

Encode a tooltip channel. See the
<https://vega.github.io/vega-lite/docs/encoding.html#text Vega-Lite documentation>
for further details on the text and tooltip channels and
<https://vega.github.io/vega-lite/docs/format.html Vega-Lite formatting documentation>
for formatting the appearance of the text.

@
enc = 'encoding'
        . 'position' 'X' [ 'PName' \"Horsepower\", 'PmType' 'Quantitative' ]
        . 'position' 'Y' [ 'PName' \"Miles_per_Gallon\", 'PmType' 'Quantitative' ]
        . 'tooltip' [ 'TName' \"Year\", 'TmType' 'Temporal', 'TFormat' "%Y" ]
@

To encode multiple tooltip values with a mark, use 'tooltips'.

-}
tooltip ::
  [TextChannel]
  -- ^ The properties for the channel.
  -> BuildLabelledSpecs
tooltip tDefs ols =
  ("tooltip" .= object (concatMap textChannelProperty tDefs)) : ols

{-|

Encode a tooltip channel using multiple data fields.

@since 0.3.0.0

@
'encoding'
    . 'position' 'X' [ 'PName' \"Horsepower\", 'PmType' 'Quantitative' ]
    . 'position' 'Y' [ 'PName' \"Miles_per_Gallon\", 'PmType' 'Quantitative' ]
    . 'tooltips' [ [ 'TName' \"Year\",  'TmType' 'Temporal', 'TFormat' "%Y" ]
               , [ 'TName' \"Month\", 'TmType' 'Temporal', 'TFormat' "%Y" ] ]
@
-}
tooltips ::
  [[TextChannel]]
  -- ^ A separate list of properties for each channel.
  -> BuildLabelledSpecs
tooltips tDefs ols =
  ("tooltip" .= toJSON (map (object . (concatMap textChannelProperty)) tDefs)) : ols


-- | This is used with 'MTooltip'.
--
--   @since 0.4.0.0

data TooltipContent
  = TTEncoding
    -- ^ Tooltips are generated by the encoding (this is the default).
    --
    --   For example:
    --
    --   @'mark' 'Circle' ['MTooltip' 'TTEncoding']@
  | TTData
    -- ^ Tooltips are generated by all fields in the underlying data.
    --
    --   For example:
    --
    --   @'mark' 'Circle' ['MTooltip' 'TTData']@
  | TTNone
    -- ^ Disable tooltips.
    --
    --   For example:
    --
    --   @'mark' 'Circle' ['MTooltip' 'TTNone']@


-- Note that TTNone is special cased by markProperty
ttContentLabel :: TooltipContent -> T.Text
ttContentLabel TTEncoding = "encoding"
ttContentLabel TTData = "data"
ttContentLabel TTNone = "null"
