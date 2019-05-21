{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Graphics.Vega.VegaLite
Copyright   : (c) Douglas Burke, 2018-2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings, TupleSections

This is essentially a straight port of the
<http://package.elm-lang.org/packages/gicentre/elm-vega/2.2.1/VegaLite Elm Vega Lite module>
(version 2.2.1).
It allows users to create a Vega-Lite specification, targeting
version 2 of the <https://vega.github.io/schema/vega-lite/v2.json JSON schema>.
The ihaskell-hvega module provides an easy way to embed Vega-Lite
visualizations in an IHaskell notebook (using
<https://vega.github.io/vega-lite/usage/embed.html Vega-Embed>).

The major change to version 2.2.1 of the Elm version is that the type representing
the full Vega-Lite specification - that is, the return value of
'toVegaLite' - is not a 'VLSpec' (an alias for 'Data.Aeson.Value')
but is instead a newtype around this ('VegaLite'). There are also some minor
changes to the exported types and symbols (e.g. 'Utc' is exported rather than
@utc@ and @bin@ is not exported).

Note that this module exports several symbols that are exported
by the Prelude, namely 'filter', 'lookup',
and 'repeat'; to avoid name clashes it's therefore advised
to either import the module qualified, for example:

@
import qualified Graphics.Vega.VegaLite as VL
@

or to hide the clashing names explicitly:

@
import Prelude hiding (filter, lookup)
@

In the following example, we'll assume the latter.

== Example

Let's say we have the following plot declaration in a module:

@
\{\-\# language OverloadedStrings \#\-\}

vl1 = 'toVegaLite' ['description' desc, 'background' "white", 'dat' [], 'mark' 'Bar' 'barOpts', 'enc' []] where
    desc = "A very exciting bar chart"

    dat = 'dataFromRows' ['Parse' [("start", 'FoDate' "%Y-%m-%d")]]
          . 'dataRow' [("start", 'Str' "2011-03-25"), ("count", 'Number' 23)]
          . dataRow [("start", Str "2011-04-02"), ("count", Number 45)]
          . dataRow [("start", Str "2011-04-12"), ("count", Number 3)]

    barOpts = ['MOpacity' 0.4, 'MColor' "teal"]

    enc = 'encoding'
          . 'position' 'X' ['PName' "start", 'PmType' 'Temporal', 'PAxis' ['AxTitle' "Inception date"]]
          . position Y [PName "count", PmType Quantitative]
@

We can inspect how the encoded JSON looks like in an GHCi session:

@
> 'A.encode' $ 'fromVL' vl1
> "{\"mark\":{\"color\":\"teal\",\"opacity\":0.4,\"type\":\"bar\"},\"data\":{\"values\":[{\"start\":\"2011-03-25\",\"count\":23},{\"start\":\"2011-04-02\",\"count\":45},{\"start\":\"2011-04-12\",\"count\":3}],\"format\":{\"parse\":{\"start\":\"date:'%Y-%m-%d'\"}}},\"$schema\":\"https://vega.github.io/schema/vega-lite/v2.json\",\"encoding\":{\"x\":{\"field\":\"start\",\"type\":\"temporal\",\"axis\":{\"title\":\"Inception date\"}},\"y\":{\"field\":\"count\",\"type\":\"quantitative\"}},\"background\":\"white\",\"description\":\"A very exciting bar chart\"}"
@

The produced JSON can then be processed with vega-lite, which renders the following image :

<<images/example.png>>

-}

module Graphics.Vega.VegaLite
       (
         -- * Creating a Vega-Lite Specification

         toVegaLite
       , fromVL
       , VLProperty
       , VLSpec
       , VegaLite
       , LabelledSpec
       , BuildLabelledSpecs
       , combineSpecs

         -- * Creating the Data Specification
         --
         -- Functions and types for declaring the input data to the
         -- visualization.

       , dataFromUrl
       , dataFromColumns
       , dataFromRows
       , dataFromJson
       , dataFromSource
       , datasets
       , dataColumn
       , dataRow
       , geometry
       , geoFeatureCollection
       , geometryCollection
       , Data
       , DataColumn
       , DataRow
       , Format(..)
       , Geometry(..)
       , DataType(..)

         -- * Creating the Transform Specification
         --
         -- Functions and types for declaring the transformation rules that
         -- are applied to data fields or geospatial coordinates before they
         -- are encoded visually.

       , transform
       , projection
       , ProjectionProperty(..)
       , Projection(..)
       , ClipRect(..)

         -- ** Aggregation

       , aggregate
       , Operation(..)
       , opAs
       , timeUnitAs

         -- ** Binning

       , binAs
       , BinProperty(..)

         -- ** Data Calculation

       , calculateAs

         -- ** Filtering

       , filter
       , Filter(..)
       , FilterRange(..)

         -- ** Relational Joining (lookup)

       , lookup
       , lookupAs

         -- * Creating the Mark Specification
         --
         -- Types and functions for declaring the type of visual
         -- marks used in the visualization.

       , mark
       , Mark(..)
       , MarkProperty(..)
       , MarkOrientation(..)
       , MarkInterpolation(..)
       , Symbol(..)
       , Cursor(..)

         -- * Creating the Encoding Specification
         --
         -- $encoding

       , encoding
       , Measurement(..)

         -- ** Position Channels
         --
         -- Control where items appear in the visualization.

       , position
       , PositionChannel(..)
       , Position(..)
       , SortProperty(..)
       , StackProperty(..)
       , AxisProperty(..)
       , OverlapStrategy(..)
       , Side(..)
       , HAlign(..)
       , VAlign(..)
       , FontWeight(..)
       , TimeUnit(..)

         -- ** Mark channels
         --
         -- Control the appearance of the visual marks in the visualization
         -- (e.g. 'color' and 'size').

       , size
       , color
       , fill
       , stroke
       , opacity
       , shape
       , MarkChannel(..)
       , LegendProperty(..)
       , Legend(..)
       , LegendOrientation(..)
       , LegendValues(..)

         -- ** Text Channels
         --
         -- Control the appearance of the text and tooltip elements in the visualization.

       , text
       , tooltip
       , TextChannel(..)

         -- ** Hyperlink Channels
         --
         -- $hyperlink

       , hyperlink
       , HyperlinkChannel(..)

         -- ** Order Channels
         --
         -- $order

       , order
       , OrderChannel(..)

         -- ** Facet Channels
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
         -- How the encoding of a data field is applied.

       , ScaleProperty(..)
       , Scale(..)
       , categoricalDomainMap
       , domainRangeMap
       , ScaleDomain(..)
       , ScaleRange(..)
       , ScaleNice(..)
       , CInterpolate(..)

         -- * Creating view compositions
         --
         -- $view

       , layer
       , hConcat
       , vConcat
       , resolve
       , resolution
       , Resolve(..)
       , Channel(..)
       , Resolution(..)

         -- ** Faceted views
         -- #facetview#
         --
         -- $facetview

       , repeat
       , RepeatFields(..)
       , facet
       , FacetMapping(..)
       , FacetChannel(..)
       , asSpec
       , specification
       , Arrangement(..)
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
       , SelectionResolution(..)
       , SelectionMarkProperty(..)

         -- ** Making conditional channel encodings
         --
         -- $conditional

       , BooleanOp(..)

         -- * Global Configuration
         --
         -- Configuration options that affect the entire visualization. These are in addition
         -- to the data and transform options described above.

       , name
       , title
       , description
       , height
       , width
       , padding
       , autosize
       , background
       , configure
       , configuration
       , ConfigurationProperty(..)
       , Autosize(..)
       , Padding(..)
       , AxisConfig(..)
       , LegendConfig(..)
       , ScaleConfig(..)
       , TitleConfig(..)
       , APosition(..)
       , ViewConfig(..)
       , RangeConfig(..)
       , FieldTitleProperty(..)

         -- * General Data types
         --
         -- In addition to more general data types like integers and string, the following types
         -- can carry data used in specifications.

       , DataValue(..)
       , DataValues(..)
       , DateTime(..)
       , MonthName(..)
       , DayName(..)

        )
    where

-- VegaLite uses these symbols.
import Prelude hiding (filter, lookup, repeat)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V

import Control.Arrow (first, second)

-- Aeson's Value type conflicts with the Number type
import Data.Aeson ((.=), Value, decode, encode, object, toJSON)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))

-- Documentation

-- $encoding
-- Types and functions for declaring which data fields are mapped to which
-- channels. Channels can include: position on screen (e.g. 'X', 'Y'); visual
-- mark properties ('color', 'size', 'stroke', 'shape'); 'text'; 'hyperlink';
-- ordering ('order'); level of 'detail'; and facets for composed
-- visualizations ('facet'). All can be further customised via a series of
-- properties that determine how the encoding is implemented (such as
-- scaling, sorting, and spacing).

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

-- NOTE: the facetview link above doesn't seem to work. Argh.

-- $detail
-- Used for grouping data but without changing the visual appearance of a mark. When,
-- for example, a field is encoded by color, all data items with the same value for
-- that field are given the same color. When a detail channel encodes a field, all
-- data items with the same value are placed in the same group. This allows, for example
-- a line chart with multiple lines to be created – one for each group. See the
-- <https://vega.github.io/vega-lite/docs/encoding.html#detail Vega-Lite documentation>
-- for more information.

-- $view
-- Views can be combined to create more complex multiview displays. This may involve
-- layering views on top of each other (superposition) or laying them out in adjacent
-- spaces (juxtaposition using 'repeat', 'facet', 'hConcat', or 'vConcat'). Where different
-- views have potentially conflicting channels (for example, two position scales in
-- a layered visualization) the rules for resolving them can be defined with 'resolve'.
-- For details of creating composite views see the
-- <https://vega.github.io/vega-lite/docs/composition.html Vega-Lite documentation>.

-- $facetview
-- These are small multiples each of which show subsets of the same dataset. The specification
-- determines which field should be used to determine subsets along with their spatial
-- arrangement (in rows or columns). For details see the
-- <https://vega.github.io/vega-lite/docs/facet.html Vega-Lite documentation>.

-- $selections
-- Selections are the way in which interactions (such as clicking or dragging) can be
-- responded to in a visualization. They transform interactions into data queries.
-- For details, see the
-- <https://vega.github.io/vega-lite/docs/selection.html Vega-Lite documentation>.

-- $conditional
-- Sometimes it is useful to make channel encoding conditional on something. For example,
-- on the result of some interaction such as clicking or dragging or some data property
-- such whether null or an outlier. 'MSelectionCondition' and 'TSelectionCondition' will
-- encode a mark or text dependent on an interactive selection. 'MDataCondition'
-- and 'TDataCondition' will encode it dependening on some data property.
--
-- For interaction, once a selection has been defined and named, supplying a set of
-- 'MSelectionCondition' encodings allow mark encodings to become dependent on that selection.
-- 'MSelectionCondition' is followed firstly by a Boolean expression relating to the
-- selection upon which it is dependent, then an \"if\" and an \"else\" clause. Each clause
-- is a list of mark field encodings that should be applied when the selection is true
-- (the \"if clause\") and when it is false (the \"else clause\"). The color encoding below
-- is saying \"whenever data marks are selected with an interval mouse drag, encode
-- the cylinder field with an ordinal color scheme, else make them grey\".
--
-- @
-- sel = selection . select "myBrush" Interval []
--
-- enc = encoding
--         . position X [ PName \"Horsepower\", PmType Quantitative ]
--         . position Y [ PName \"Miles_per_Gallon\", PmType Quantitative ]
--         . color
--             [ MSelectionCondition (SelectionName "myBrush")
--                 [ MName \"Cylinders\", MmType Ordinal ]
--                 [ MString "grey" ]
--             ]
-- @
--
-- In a similar way, 'MDataCondition' will encocode a mark in one of two ways depending
-- on whether a predicate test is satisfied.
--
-- @
-- enc = encoding
--         . position X [ PName \"IMDB_Rating\", PmType Quantitative ]
--         . position Y [ PName \"Rotten_Tomatoes_Rating\", PmType Quantitative ]
--           . color
--               [ MDataCondition
--                   (Or (Expr "datum.IMDB_Rating === null")
--                       (Expr "datum.Rotten_Tomatoes_Rating === null")
--                   )
--                   [ MString "#ddd" ]
--                   [ MString "#0099ee" ]
--               ]
-- @
--
-- For details, see the
-- <https://vega.github.io/vega-lite/docs/condition.html Vega-Lite documentation>.

--- helpers not in VegaLite.elm

aggregate_ :: Operation -> LabelledSpec
aggregate_ op = "aggregate" .= operationLabel op

field_ :: T.Text -> LabelledSpec
field_ f = "field" .= f

op_ :: Operation -> LabelledSpec
op_ op = "op" .= operationLabel op

repeat_ :: Arrangement -> LabelledSpec
repeat_ arr = "repeat" .= arrangementLabel arr

sort_ :: [SortProperty] -> LabelledSpec
sort_ ops = "sort" .= sortPropertySpec ops

timeUnit_ :: TimeUnit -> LabelledSpec
timeUnit_ tu = "timeUnit" .= timeUnitLabel tu

type_ :: Measurement -> LabelledSpec
type_ m = "type" .= measurementLabel m

value_ :: T.Text -> LabelledSpec
value_ v = "value" .= v

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
  -- Note that there is __no__ validation done to ensure that the output matches
  -- the Vega Lite schema. That is, it is possible to create an invalid visualization
  -- with this module (e.g. missing a data source or referring to an undefined
  -- field).
  }

-- | The specification is represented as JSON.
type VLSpec = Value

vlSchemaName :: T.Text
vlSchemaName = "https://vega.github.io/schema/vega-lite/v2.json"

{-|

Convert a list of Vega-Lite specifications into a single JSON object that may be
passed to Vega-Lite for graphics generation. Commonly these will include at least
a data, mark, and encoding specification.

While simple properties like 'mark' may be provided directly, it is usually clearer
to label more complex ones such as encodings as separate expressions. This becomes
increasingly helpful for visualizations that involve composition of layers, repeats
and facets.

Specifications can be built up by chaining a series of functions (such as 'dataColumn'
or 'position' in the example below). Functional composition using the '.' operator
allows this to be done compactly.

@
let dat = dataFromColumns []
          . dataColumn "a" (Strings [ \"C", \"C", \"D", \"D", \"E", \"E" ])
          . dataColumn "b" (Numbers [ 2, 7, 1, 2, 6, 8 ])

    enc = encoding
          . position X [ PName "a", PmType Nominal ]
          . position Y [ PName "b", PmType Quantitative, PAggregate Mean ]

in toVegaLite [ dat [], mark Bar [], enc [] ]
@
-}
toVegaLite :: [(VLProperty, VLSpec)] -> VegaLite
toVegaLite vals =
  let kvals = ("$schema" .= vlSchemaName)
              : map toProp vals
      toProp = first vlPropertyLabel

  in VL { fromVL = object kvals }


{-|

Combines a list of labelled specifications into a single specification.
This is useful when you wish to create
a single page with multiple visulizualizations.

@
combineSpecs
    [ ( "vis1", myFirstVis )
    , ( "vis2", mySecondVis )
    , ( "vis3", myOtherVis )
    ]
@
-}
combineSpecs :: [LabelledSpec] -> VLSpec
combineSpecs = object

{-|

Create a specification sufficient to define an element in a composed visualization
such as a superposed layer or juxtaposed facet. Typically a layer will contain a
full set of specifications that define a visualization with
the exception of the data specification which is usually defined outside of any one
layer. Whereas for repeated and faceted specs, the entire specification is provided.

@
spec1 = asSpec [ enc1 [], mark Line [] ]
@
-}
asSpec :: [(VLProperty, VLSpec)] -> VLSpec
asSpec = object . map (first vlPropertyLabel)


{-|

Specifies a list of geo features to be used in a geoShape specification.
Each feature object in this collection can be created with the 'geometry'
function.

@
geojson =
    geoFeatureCollection
        [ geometry (GeoPolygon [ [ ( -3, 59 ), ( -3, 52 ), ( 4, 52 ), ( -3, 59 ) ] ])
            [ ( "myRegionName", Str "Northern region" ) ]
        , geometry (GeoPolygon [ [ ( -3, 52 ), ( 4, 52 ), ( 4, 45 ), ( -3, 52 ) ] ])
            [ ( "myRegionName", Str "Southern region" ) ]
        ]
@
-}
geoFeatureCollection :: [VLSpec] -> VLSpec
geoFeatureCollection geoms =
  object [ "type" .= ("FeatureCollection" :: T.Text)
         , "features" .=  geoms
         ]


{-|

Specifies a list of geometry objects to be used in a geoShape specification.
Each geometry object in this collection can be created with the 'geometry'
function.

@
geojson =
    geometryCollection
        [ geometry (GeoPolygon [ [ ( -3, 59 ), ( 4, 59 ), ( 4, 52 ), ( -3, 59 ) ] ]) []
        , geometry (GeoPoint -3.5 55.5) []
        ]
@
-}
geometryCollection :: [VLSpec] -> VLSpec
geometryCollection geoms =
  object [ "type" .= ("GeometryCollection" :: T.Text)
         , "geometries" .= geoms
         ]


{-|

Create a named aggregation operation on a field that can be added to a transformation.
For further details see the
<https://vega.github.io/vega-lite/docs/aggregate.html#aggregate-op-def Vega-Lite documentation>.

@
trans =
    transform
        . aggregate
            [ opAs Min "people" "lowerBound"
            , opAs Max "people" "upperBound"
            ]
            [ "age" ]
@
-}
opAs ::
  Operation
  -- ^ The aggregation operation to use.
  -> T.Text
  -- ^ The name of the field which is to be aggregated.
  -> T.Text
  -- ^ The name given to the transformed data.
  -> VLSpec
opAs op field label =
  object [ op_ op
         , field_ field
         , "as" .= label
         ]


{-|

Top-level Vega-Lite properties. These are the ones that define the core of the
visualization grammar. All properties are created by functions which can be
arranged into seven broad groups:

[Data Properties] These relate to the input data to be visualized. Generated by
'dataFromColumns', 'dataFromRows', 'dataFromUrl', 'dataFromSource' and
'dataFromJson'.

[Transform Properties] These indicate that some transformation of input data should
be applied before encoding them visually. Generated by 'transform'
and 'projection' they can include data transformations such as 'filter',
'binAs' and 'calculateAs' and geo transformations of longitude, latitude coordinates
used by marks such as 'Geoshape', 'Point', and 'Line'.

[Mark Properties] These relate to the symbols used to visualize data items. They
are generated by 'mark', and include types such as 'Circle', 'Bar', and 'Line'.

[Encoding Properties] These specify which data elements are mapped to which mark characteristics
(known as /channels/). Generated by 'encoding', they include encodings
such as 'position', 'color', 'size', 'shape', 'text' and 'hyperlink'.

[Composition Properties] These allow visualization views to be combined to form more
complex visualizations. Generated by 'layer', 'repeat', 'facet',
'hConcat', 'vConcat', 'spec', and 'resolve'.

[Interaction Properties] These allow interactions such as clicking, dragging and others
generated via a GUI or data stream to influence the visualization. Generated by
'selection'.

[Supplementary and Configuration Properties] These provide a means to add metadata and
styling to one or more visualizations. Generated by 'name', 'title', 'description',
'background', 'height', 'width', 'padding', 'autosize', and 'configure'.
-}
data VLProperty
    = VLName
    | VLDescription
    | VLTitle
    | VLWidth
    | VLHeight
    | VLAutosize
    | VLPadding
    | VLBackground
    | VLData
    | VLDatasets
    | VLMark
    | VLTransform
    | VLProjection
    | VLEncoding
    | VLLayer
    | VLHConcat
    | VLVConcat
    | VLRepeat
    | VLFacet
    | VLSpec
    | VLResolve
    | VLConfig
    | VLSelection


vlPropertyLabel :: VLProperty -> T.Text
vlPropertyLabel VLName = "name"
vlPropertyLabel VLDescription = "description"
vlPropertyLabel VLTitle = "title"
vlPropertyLabel VLWidth = "width"
vlPropertyLabel VLHeight = "height"
vlPropertyLabel VLPadding = "padding"
vlPropertyLabel VLAutosize = "autosize"
vlPropertyLabel VLBackground = "background"
vlPropertyLabel VLData = "data"
vlPropertyLabel VLDatasets = "datasets"
vlPropertyLabel VLProjection = "projection"
vlPropertyLabel VLMark = "mark"
vlPropertyLabel VLTransform = "transform"
vlPropertyLabel VLEncoding = "encoding"
vlPropertyLabel VLConfig = "config"
vlPropertyLabel VLSelection = "selection"
vlPropertyLabel VLHConcat = "hconcat"
vlPropertyLabel VLVConcat = "vconcat"
vlPropertyLabel VLLayer = "layer"
vlPropertyLabel VLRepeat = "repeat"
vlPropertyLabel VLFacet = "facet"
vlPropertyLabel VLSpec = "spec"
vlPropertyLabel VLResolve = "resolve"


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
    | FoBoolean
    | FoDate T.Text
    | FoUtc T.Text


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
    | CSV
    | TSV
    | TopojsonFeature T.Text
    | TopojsonMesh T.Text
    | Parse [(T.Text, DataType)]


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
'dataColumn'.
-}
type DataColumn = [LabelledSpec]

{-|

Represents a single row of data. Used when generating inline data with
'dataRow'.
-}
type DataRow = VLSpec

{-|

Convenience type annotation label for use with data generation functions.

@
myRegion : [DataColumn] -> Data
myRegion =
    dataFromColumns []
        . dataColumn "easting" (Numbers [ -3, 4, 4, -3, -3 ])
        . dataColumn "northing" (Numbers [ 52, 52, 45, 45, 52 ])
@
-}
type Data = (VLProperty, VLSpec)


formatProperty :: Format -> [LabelledSpec]
formatProperty (JSON js) =
  let ps = [("type", "json")]
           <> if T.null (T.strip js) then [] else [("property", js)]
  in map (second toJSON) ps

formatProperty CSV = [("type", "csv")]
formatProperty TSV = [("type", "tsv")]
formatProperty (TopojsonFeature os) = [ ("type", "topojson")
                                      , ("feature", toJSON os) ]
formatProperty (TopojsonMesh os) = [ ("type", "topojson")
                                   , ("mesh", toJSON os) ]
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

@
dataRow [(\"Animal\", Str \"Fish\"), (\"Age\",Number 28), (\"Year\", Str "2010")] []
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

    data = dataFromRows []
            . dataRow [ ( "cat", Str "a" ), ( "val", Number 10 ) ]
            . dataRow [ ( "cat", Str "b" ), ( "val", Number 18 ) ]
    json = toJS
            [ obj [ ( "cat", toJS "a" ), ( "val", toJS 120 ) ]
            , obj [ ( "cat", toJS "b" ), ( "val", toJS 180 ) ]
            ]

    enc = ...

in toVegaLite
    [ datasets [ ( "myData", data [] ),  ( "myJson", dataFromJson json [] ) ]
    , dataFromSource "myData" []
    , mark Bar []
    , enc []
    ]
@
-}
datasets :: [(T.Text, Data)] -> Data
datasets namedData =
  -- follow Elm in parsing the structure to get what we want, but the code is
  -- written rather differently
  --
  -- The input is expected to be a singleton list containing a pair.
  let convert = extract . snd
      specs = map (second convert) namedData

      extract din =
        let extract' :: [(T.Text, Value)] -> Value
            extract' [(_, v)] = v
            extract' _ = din

        in maybe din extract' (decode (encode din))

  in (VLDatasets, object specs)


{-|

Declare a data source from a provided list of column values. Each column contains
values of the same type, but columns each with a different type are permitted.
Columns should all contain the same number of items; if not the dataset will be
truncated to the length of the shortest column. An optional list of field formatting
instructions can be provided as the first parameter or an empty list to use the
default formatting. See the
<https://vega.github.io/vega-lite/docs/data.html#format Vega-Lite documentation>
for details. The columns themselves are most easily generated with 'dataColumn'

@
data =
    dataFromColumns [ Parse [ ( \"Year\", FoDate "%Y" ) ] ]
        . dataColumn \"Animal\" (Strings [ \"Fish\", \"Dog\", \"Cat\" ])
        . dataColumn \"Age\" (Numbers [ 28, 12, 6 ])
        . dataColumn \"Year\" (Strings [ "2010", "2014", "2015" ])
@
-}
dataFromColumns :: [Format] -> [DataColumn] -> Data
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
        geometry (GeoPolygon [ [ ( -3, 59 ), ( 4, 59 ), ( 4, 52 ), ( -3, 59 ) ] ]) []
in toVegaLite
    [ width 200
    , height 200
    , dataFromJson geojson []
    , projection [ PType Orthographic ]
    , mark Geoshape []
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

A single data value. This is used when a function can accept values of different
types (e.g. either a number or a string).
-}
data DataValue
    = Boolean Bool
    | DateTime [DateTime]
    | Number Double
    | Str T.Text


dataValueSpec :: DataValue -> VLSpec
dataValueSpec (Boolean b) = toJSON b
dataValueSpec (DateTime dt) = object (map dateTimeProperty dt)
dataValueSpec (Number x) = toJSON x
dataValueSpec (Str t) = toJSON t


{-|

A list of data values. This is used when a function can accept lists of
different types (e.g. either a list of numbers or a list of strings).
-}
data DataValues
    = Booleans [Bool]
    | DateTimes [[DateTime]]
    | Numbers [Double]
    | Strings [T.Text]


{-|

Create a column of data. A column has a name and a list of values. The final
parameter is the list of any other columns to which this is added.

@
dataColumn \"Animal\" (Strings [ \"Cat\", \"Dog\", \"Mouse\"]) []
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

Declare a data source from a provided list of row values. Each row contains
a list of tuples where the first value is a string representing the column name, and the
second the column value for that row. Each column can have a value of a different type
but you must ensure that when subsequent rows are added, they match the types of previous
values with shared column names. An optional list of field formatting instructions can
be provided as the first parameter or an empty list to use the default formatting. See the
<https://vega.github.io/vega-lite/docs/data.html#format Vega-Lite documentation>
for details.

The rows themselves are most easily generated with 'dataRow'. Note though that generally
if you are creating data inline (as opposed to reading from a file), adding data by column
is more efficient and less error-prone.

@
data = dataFromRows [ Parse [ ( \"Year\", FoDate "%Y" ) ] ]
        . dataRow [ ( \"Animal\", Str \"Fish\" ), ( \"Age\", Number 28 ), ( \"Year\", Str "2010" ) ]
        . dataRow [ ( \"Animal\", Str \"Dog\" ), ( \"Age\", Number 12 ), ( \"Year\", Str "2014" ) ]
        . dataRow [ ( \"Animal\", Str \"Cat\" ), ( \"Age\", Number 6 ), ( \"Year\", Str "2015" ) ]
@
-}
dataFromRows :: [Format] -> [DataRow] -> Data
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
toVegaLite
    [ datasets [ ( "myData", data [] ),  ( "myJson", dataFromJson json [] ) ]
    , dataFromSource "myData" []
    , mark Bar []
    , enc []
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
toVegaLite
    [ dataFromUrl "data/weather.csv" [ Parse [ ( "date", FoDate "%Y-%m-%d %H:%M" ) ] ]
    , mark Line []
    , enc []
    ]
@
-}
-- TODO: should use a URL type
dataFromUrl :: T.Text -> [Format] -> Data
dataFromUrl url fmts =
  let kvs = ("url" .= url)
            : if null fmts
              then []
              else [("format", object (concatMap formatProperty fmts))]
  in (VLData, object kvs)


-- | Type of visual mark used to represent data in the visualization.
data Mark
    = Area
    | Bar
    | Circle
    | Geoshape
    | Line
    | Point
    | Rect
    | Rule
    | Square
    | Text
    | Tick


markLabel :: Mark -> T.Text
markLabel Area = "area"
markLabel Bar = "bar"
markLabel Circle = "circle"
markLabel Line = "line"
markLabel Geoshape = "geoshape"
markLabel Point = "point"
markLabel Rect = "rect"
markLabel Rule = "rule"
markLabel Square = "square"
markLabel Text = "text"
markLabel Tick = "tick"


{-|

Create a mark specification. All marks must have a type (first parameter) and
can optionally be customised with a list of mark properties such as interpolation
style for lines. To keep the default style for the mark, just provide an empty list
for the second parameter.

@
mark Circle []
mark Line [ MInterpolate StepAfter ]
@
-}
mark :: Mark -> [MarkProperty] -> (VLProperty, VLSpec)
mark mrk props =
  let jsName = toJSON (markLabel mrk)
      vals = if null props
             then jsName
             else object (("type" .= jsName) : map markProperty props)

  in (VLMark, vals)


{-|

Mark channel properties used for creating a mark channel encoding.
-}
data MarkChannel
    = MName T.Text
    | MRepeat Arrangement
    | MmType Measurement
    | MScale [ScaleProperty]
      -- ^ Use an empty list to remove the scale.
    | MBin [BinProperty]
    | MTimeUnit TimeUnit
    | MAggregate Operation
    | MLegend [LegendProperty]
      -- ^ Use an empty list to remove the legend.
    | MSelectionCondition BooleanOp [MarkChannel] [MarkChannel]
    | MDataCondition BooleanOp [MarkChannel] [MarkChannel]
    | MPath T.Text
    | MNumber Double
    | MString T.Text
    | MBoolean Bool


markChannelProperty :: MarkChannel -> [LabelledSpec]
markChannelProperty (MName s) = [field_ s]
markChannelProperty (MRepeat arr) = ["field" .= object [repeat_ arr]]
markChannelProperty (MmType t) = [type_ t]
markChannelProperty (MScale sps) =
  [("scale", if null sps then A.Null else object (map scaleProperty sps))]
markChannelProperty (MLegend lps) =
  [("legend", if null lps then A.Null else object (map legendProperty lps))]
markChannelProperty (MBin bps) = [bin bps]
markChannelProperty (MSelectionCondition selName ifClause elseClause) =
  let h = ("condition", hkey)
      toProps = concatMap markChannelProperty
      hkey = object (("selection", booleanOpSpec selName) : toProps ifClause)
      hs = toProps elseClause
  in h : hs
markChannelProperty (MDataCondition predicate ifClause elseClause) =
  let h = ("condition", hkey)
      toProps = concatMap markChannelProperty
      hkey = object (("test", booleanOpSpec predicate) : toProps ifClause)
      hs = toProps elseClause
  in h : hs
markChannelProperty (MTimeUnit tu) = [timeUnit_ tu]
markChannelProperty (MAggregate op) = [aggregate_ op]
markChannelProperty (MPath s) = ["value" .= s]
markChannelProperty (MNumber x) = ["value" .= x]
markChannelProperty (MString s) = ["value" .= s]
markChannelProperty (MBoolean b) = ["value" .= b]


{-|

Properties for customising the appearance of a mark. For details see the
<https://vega.github.io/vega-lite/docs/mark.html#config Vega-Lite documentation>.

Not all properties are valid for each mark type.
-}
data MarkProperty
    = MAlign HAlign
    | MAngle Double
    | MBandSize Double
    | MBaseline VAlign
    | MBinSpacing Double
    | MClip Bool
    | MColor T.Text
    | MCursor Cursor
    | MContinuousBandSize Double
    | MDiscreteBandSize Double
    | MdX Double
    | MdY Double
    | MFill T.Text
    | MFilled Bool
    | MFillOpacity Double
    | MFont T.Text
    | MFontSize Double
    | MFontStyle T.Text
    | MFontWeight FontWeight
    | MInterpolate MarkInterpolation
    | MOpacity Double
    | MOrient MarkOrientation
    | MRadius Double
    | MShape Symbol
    | MShortTimeLabels Bool
    | MSize Double
    | MStroke T.Text
    | MStrokeDash [Double]
    | MStrokeDashOffset Double
    | MStrokeOpacity Double
    | MStrokeWidth Double
    | MStyle [T.Text]
    | MTension Double
    | MText T.Text
    | MTheta Double
    | MThickness Double


markProperty :: MarkProperty -> LabelledSpec
markProperty (MFilled b) = ("filled", toJSON b)
markProperty (MClip b) = ("clip", toJSON b)
markProperty (MColor col) = ("color", toJSON col)
markProperty (MCursor cur) = ("cursor", toJSON (cursorLabel cur))
markProperty (MFill col) = ("fill", toJSON col)
markProperty (MStroke t) = ("stroke", toJSON t)
markProperty (MStrokeOpacity x) = ("strokeOpacity", toJSON x)
markProperty (MStrokeWidth w) = ("strokeWidth", toJSON w)
markProperty (MStrokeDash xs) = ("strokeDash", toJSON (map toJSON xs))
markProperty (MStrokeDashOffset x) = ("strokeDashOffset", toJSON x)
markProperty (MOpacity x) = ("opacity", toJSON x)
markProperty (MFillOpacity x) = ("fillOpacity", toJSON x)
markProperty (MStyle styles) = ("style", toJSON (map toJSON styles))
markProperty (MInterpolate interp) = ("interpolate", toJSON (markInterpolationLabel interp))
markProperty (MTension x) = ("tension", toJSON x)
markProperty (MOrient orient) = ("orient", toJSON (markOrientationLabel orient))
markProperty (MShape sym) = ("shape", toJSON (symbolLabel sym))
markProperty (MSize x) = ("size", toJSON x)
markProperty (MAngle x) = ("angle", toJSON x)
markProperty (MAlign align) = ("align", toJSON (hAlignLabel align))
markProperty (MBaseline va) = ("baseline", toJSON (vAlignLabel va))
markProperty (MdX dx) = ("dx", toJSON dx)
markProperty (MdY dy) = ("dy", toJSON dy)
markProperty (MFont fnt) = ("font", toJSON fnt)
markProperty (MFontSize x) = ("fontSize", toJSON x)
markProperty (MFontStyle fSty) = ("fontStyle", toJSON fSty)
markProperty (MFontWeight w) = ("fontWeight", fontWeightSpec w)
markProperty (MRadius x) = ("radius", toJSON x)
markProperty (MText txt) = ("text", toJSON txt)
markProperty (MTheta x) = ("theta", toJSON x)
markProperty (MBinSpacing x) = ("binSpacing", toJSON x)
markProperty (MContinuousBandSize x) = ("continuousBandSize", toJSON x)
markProperty (MDiscreteBandSize x) = ("discreteBandSize", toJSON x)
markProperty (MShortTimeLabels b) = ("shortTimeLabels", toJSON b)
markProperty (MBandSize x) = ("bandSize", toJSON x)
markProperty (MThickness x) = ("thickness", toJSON x)


{-|

Create an encoding specification from a list of channel encodings.

@
enc = encoding
        . position X [ PName \"Animal\", PmType Ordinal ]
        . position Y [ PName \"Age\", PmType Quantitative ]
        . shape [ MName \"Species\", MmType Nominal ]
        . size [ MName \"Population\", MmType Quantitative ]
@
-}
encoding :: [LabelledSpec] -> (VLProperty, VLSpec)
encoding channels = (VLEncoding, object channels)


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
-}

data BinProperty
    = Base Double
    | Divide Double Double
    | Extent Double Double
    | MaxBins Int
    | MinStep Double
    | Nice Bool
    | Step Double
    | Steps [Double]


binProperty :: BinProperty -> LabelledSpec
binProperty (MaxBins n) = ("maxbins", toJSON n)
binProperty (Base x) = ("base", toJSON x)
binProperty (Step x) = ("step", toJSON x)
binProperty (Steps xs) = ("steps", toJSON (map toJSON xs))
binProperty (MinStep x) = ("minstep", toJSON x)
binProperty (Divide x y) = ("divide", toJSON [ toJSON x, toJSON y ])
binProperty (Extent mn mx) = ("extent", toJSON [ toJSON mn, toJSON mx ])
binProperty (Nice b) = ("nice", toJSON b)


bin :: [BinProperty] -> LabelledSpec
bin bProps =
  let ans = if null bProps
            then toJSON True
            else object (map binProperty bProps)
  in ("bin", ans)


{-|

Type of aggregation operation. See the
<https://vega.github.io/vega-lite/docs/aggregate.html#ops Vega-Lite documentation>
for more details.
-}
data Operation
    = ArgMax
    | ArgMin
    | Average
    | CI0
    | CI1
    | Count
    | Distinct
    | Max
    | Mean
    | Median
    | Min
    | Missing
    | Q1
    | Q3
    | Stderr
    | Stdev
    | StdevP
    | Sum
    | Valid
    | Variance
    | VarianceP


operationLabel :: Operation -> T.Text
operationLabel ArgMax = "argmax"
operationLabel ArgMin = "argmin"
operationLabel Average = "average"
operationLabel CI0 = "ci0"
operationLabel CI1 = "ci1"
operationLabel Count = "count"
operationLabel Distinct = "distinct"
operationLabel Max = "max"
operationLabel Mean = "mean"
operationLabel Median = "median"
operationLabel Min = "min"
operationLabel Missing = "missing"
operationLabel Q1 = "q1"
operationLabel Q3 = "q3"
operationLabel Stderr = "stderr"
operationLabel Stdev = "stdev"
operationLabel StdevP = "stdevp"
operationLabel Sum = "sum"
operationLabel Valid = "valid"
operationLabel Variance = "variance"
operationLabel VarianceP = "variancep"


-- | Identifies whether a repeated or faceted view is arranged in rows or columns.

data Arrangement
    = Column
    | Row

arrangementLabel :: Arrangement -> T.Text
arrangementLabel Column = "column"
arrangementLabel Row = "row"


-- | Describes the type of stacking to apply to a bar chart.

data StackProperty
    = StZero
    | StNormalize
    | StCenter
    | NoStack


stackProperty :: StackProperty -> LabelledSpec
stackProperty StZero = ("stack", "zero")
stackProperty StNormalize = ("stack", "normalize")
stackProperty StCenter = ("stack", "center")
stackProperty NoStack = ("stack", A.Null)


{-|

Individual scale property. These are used to customise an individual scale
transformation. To customise all scales use 'config' and supply relevant
'ScaleConfig' values. For more details see the
<https://vega.github.io/vega-lite/docs/scale.html Vega-Lite documentation>.

-}

data ScaleProperty
    = SType Scale
    | SDomain ScaleDomain
    | SRange ScaleRange
    | SScheme T.Text [Double]
    | SPadding Double
    | SPaddingInner Double
    | SPaddingOuter Double
    | SRangeStep (Maybe Double)
    | SRound Bool
    | SClamp Bool
      -- TODO:  Need to restrict set of valid scale types that work with color interpolation.
    | SInterpolate CInterpolate
    | SNice ScaleNice
    | SZero Bool
      -- TODO: Check: This is a Vega, not Vega-Lite property so can generate a warning if validated against the Vega-Lite spec.
    | SReverse Bool


scaleProperty :: ScaleProperty -> LabelledSpec
scaleProperty (SType sType) = ("type", toJSON (scaleLabel sType))
scaleProperty (SDomain sdType) = ("domain", scaleDomainSpec sdType)
scaleProperty (SRange range) =
  let js = case range of
        RNumbers xs -> toJSON (map toJSON xs)
        RStrings ss -> toJSON (map toJSON ss)
        RName s -> toJSON s
  in ("range", js)
scaleProperty (SScheme nme extent) = schemeProperty nme extent
scaleProperty (SPadding x) = ("padding", toJSON x)
scaleProperty (SPaddingInner x) = ("paddingInner", toJSON x)
scaleProperty (SPaddingOuter x) = ("paddingOuter", toJSON x)
scaleProperty (SRangeStep numOrNull) = ("rangeStep", maybe A.Null toJSON numOrNull)
scaleProperty (SRound b) = ("round", toJSON b)
scaleProperty (SClamp b) = ("clamp", toJSON b)
scaleProperty (SInterpolate interp) = ("interpolate", cInterpolateSpec interp)
scaleProperty (SNice ni) = ("nice", scaleNiceSpec ni)
scaleProperty (SZero b) = ("zero", toJSON b)
scaleProperty (SReverse b) = ("reverse", toJSON b)


schemeProperty :: T.Text -> [Double] -> LabelledSpec
schemeProperty nme extent =
  let js = case extent of
        [mn, mx] -> object ["name" .= nme, "extent" .= [mn, mx]]
        _ -> toJSON nme

  in ("scheme", js)


-- | Used to indicate the type of scale transformation to apply.

data Scale
    = ScLinear
    | ScPow
    | ScSqrt
    | ScLog
    | ScTime
    | ScUtc
    | ScSequential
    | ScOrdinal
    | ScBand
    | ScPoint
    | ScBinLinear
    | ScBinOrdinal


scaleLabel :: Scale -> T.Text
scaleLabel ScLinear = "linear"
scaleLabel ScPow = "pow"
scaleLabel ScSqrt = "sqrt"
scaleLabel ScLog = "log"
scaleLabel ScTime = "time"
scaleLabel ScUtc = "utc"
scaleLabel ScSequential = "sequential"
scaleLabel ScOrdinal = "ordinal"
scaleLabel ScBand = "band"
scaleLabel ScPoint = "point"
scaleLabel ScBinLinear = "bin-linear"
scaleLabel ScBinOrdinal = "bin-ordinal"


{-|

Describes the scale domain (type of data in scale). For full details see the
<https://vega.github.io/vega-lite/docs/scale.html#domain Vega-Lite documentation>.
-}

data ScaleDomain
    = DNumbers [Double]
    | DStrings [T.Text]
    | DDateTimes [[DateTime]]
    | DSelection T.Text
    | Unaggregated


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
    | NSecond
    | NMinute
    | NHour
    | NDay
    | NWeek
    | NMonth
    | NYear
    | NInterval TimeUnit Int
    | IsNice Bool
    | NTickCount Int


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
-}

data ScaleRange
    = RNumbers [Double]
    | RStrings [T.Text]
    | RName T.Text


{-|

Indicates the type of color interpolation to apply, when mapping a data field
onto a color scale. Note that color interpolation cannot be applied with the default
\"sequential\" color scale ('ScSequential'), so additionally, you should set the
'SType' to another continuous scale such as 'ScLinear' and 'ScPow'.

For details see the
<https://vega.github.io/vega-lite/docs/scale.html#continuous Vega-Lite documentation>.
-}
data CInterpolate
    = CubeHelix Double
      -- ^ The numeric value is the gamma value for the scheme (the recommended
      --   value is 1).
    | CubeHelixLong Double
      -- ^ The numeric value is the gamma value for the scheme (the recommended
      --   value is 1).
    | Hcl
    | HclLong
    | Hsl
    | HslLong
    | Lab
    | Rgb Double
      -- ^ The numeric value is the gamma value for the scheme (the recommended
      --   value is 1).


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
-}
data SortProperty
    = Ascending
    | Descending
    | Op Operation
    | ByField T.Text
    | ByRepeat Arrangement


sortProperty :: SortProperty -> LabelledSpec
sortProperty Ascending = "order" .= fromT "ascending"
sortProperty Descending = "order" .= fromT "descending"
sortProperty (ByField field) = field_ field
sortProperty (Op op) = op_ op
sortProperty (ByRepeat arr) = ("field", object [repeat_ arr])


sortPropertySpec :: [SortProperty] -> VLSpec
sortPropertySpec [] = A.Null
sortPropertySpec [Ascending] = fromT "ascending"
sortPropertySpec [Descending] = fromT "descending"
sortPropertySpec ops = object (map sortProperty ops)


-- | Position channel properties used for creating a position channel encoding.

data PositionChannel
    = PName T.Text
    | PRepeat Arrangement
    | PmType Measurement
    | PBin [BinProperty]
    | PTimeUnit TimeUnit
    | PAggregate Operation
    | PScale [ScaleProperty]
    | PAxis [AxisProperty]
    | PSort [SortProperty]
    | PStack StackProperty


positionChannelProperty :: PositionChannel -> LabelledSpec
positionChannelProperty (PName s) = field_ s
positionChannelProperty (PRepeat arr) = "field" .= object [repeat_ arr]
positionChannelProperty (PmType m) = type_ m
positionChannelProperty (PBin b) = bin b
positionChannelProperty (PTimeUnit tu) = timeUnit_ tu
positionChannelProperty (PAggregate op) = aggregate_ op
positionChannelProperty (PScale sps) =
  let js = if null sps
           then A.Null
           else object (map scaleProperty sps)
  in "scale" .=  js
positionChannelProperty (PAxis aps) =
  let js = if null aps
           then A.Null
           else object (map axisProperty aps)
  in "axis" .= js
positionChannelProperty (PSort ops) = sort_ ops
positionChannelProperty (PStack sp) = stackProperty sp


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
positionLabel Longitude = "longitude"
positionLabel Latitude = "latitude"
positionLabel Longitude2 = "longitude2"
positionLabel Latitude2 = "latitude2"


{-|

Set the background color of the visualization. Should be specified with a CSS
string such as \"#ffe\" or \"rgb(200,20,150)\". If not specified the background will
be transparent.

@
toVegaLite
    [ background "rgb(251,247,238)"
    , dataFromUrl "data/population.json" []
    , mark Bar []
    , enc []
    ]
@
-}
background :: T.Text -> (VLProperty, VLSpec)
background colour = (VLBackground, toJSON colour)


{-|

Provides an optional description to be associated with the visualization.

@
toVegaLite
    [ description "Population change of key regions since 1900"
    , dataFromUrl "data/population.json" []
    , mark Bar []
    , enc []
    ]
@
-}
description :: T.Text -> (VLProperty, VLSpec)
description s = (VLDescription, toJSON s)


{-|

Provide an optional title to be displayed in the visualization.

@
toVegaLite
    [ title "Population Growth"
    , dataFromUrl "data/population.json" []
    , mark Bar []
    , enc []
    ]
@
-}
title :: T.Text -> (VLProperty, VLSpec)
title s = (VLTitle, toJSON s)


{-|

Axis customisation properties. These are used for customising individual axes.
To configure all axes, use 'AxisConfig' with a 'configuration' instead. See the
<https://vega.github.io/vega-lite/docs/axis.html#axis-properties Vega-Lite documentation>
for more details.
-}
data AxisProperty
    = AxDomain Bool
    | AxFormat T.Text
    | AxGrid Bool
    | AxLabelAngle Double
    | AxLabelOverlap OverlapStrategy
    | AxLabelPadding Double
    | AxLabels Bool
    | AxMaxExtent Double
    | AxMinExtent Double
    | AxOffset Double
    | AxOrient Side
    | AxPosition Double
    | AxTicks Bool
    | AxTickCount Int
    | AxTickSize Double
    | AxTitle T.Text
    | AxTitleAlign HAlign
    | AxTitleAngle Double
    | AxTitleMaxLength Double
    | AxTitlePadding Double
    | AxValues [Double]
    | AxDates [[DateTime]]
    | AxZIndex Int


axisProperty :: AxisProperty -> LabelledSpec
axisProperty (AxFormat fmt) = "format" .= fmt
axisProperty (AxLabels b) = "labels" .= b
axisProperty (AxLabelAngle a) = "labelAngle" .= a
axisProperty (AxLabelOverlap s) = "labelOverlap" .= overlapStrategyLabel s
axisProperty (AxLabelPadding pad) = "labelPadding" .= pad
axisProperty (AxDomain b) = "domain" .= b
axisProperty (AxGrid b) = "grid" .= b
axisProperty (AxMaxExtent n) = "maxExtent" .= n
axisProperty (AxMinExtent n) = "minExtent" .= n
axisProperty (AxOrient side) = "orient" .= sideLabel side
axisProperty (AxOffset n) = "offset" .= n
axisProperty (AxPosition n) = "position" .= n
axisProperty (AxZIndex n) = "zindex" .= n
axisProperty (AxTicks b) = "ticks" .= b
axisProperty (AxTickCount n) = "tickCount" .= n
axisProperty (AxTickSize sz) = "tickSize" .= sz
axisProperty (AxValues vals) = "values" .= map toJSON vals
axisProperty (AxDates dtss) = "values" .= map (object . map dateTimeProperty) dtss
axisProperty (AxTitle ttl) = "title" .= ttl
axisProperty (AxTitleAlign align) = "titleAlign" .= hAlignLabel align
axisProperty (AxTitleAngle angle) = "titleAngle" .= angle
axisProperty (AxTitleMaxLength len) = "titleMaxLength" .= len
axisProperty (AxTitlePadding pad) = "titlePadding" .= pad


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
    | OParity
    | OGreedy


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
encoding
    . position X [ PName "date", PmType Temporal, PTimeUnit (Utc YearMonthDateHours) ]
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

Indicates desired orientation of a mark (e.g. horizontally or vertically
oriented bars).
-}
data MarkOrientation
    = Horizontal
    | Vertical


markOrientationLabel :: MarkOrientation -> T.Text
markOrientationLabel Horizontal = "horizontal"
markOrientationLabel Vertical = "vertical"


-- | Identifies the type of symbol.

data Symbol
    = SymCircle
    | SymSquare
    | Cross
    | Diamond
    | TriangleUp
    | TriangleDown
    | Path T.Text
      -- ^ Define a custom shape with a SVG path description.


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
    | AFit
    | ANone
    | APad
    | APadding
    | AResize


autosizeProperty :: Autosize -> LabelledSpec
autosizeProperty APad = ("type", fromT "pad")
autosizeProperty AFit = ("type", fromT "fit")
autosizeProperty ANone = ("type", fromT "none")
autosizeProperty AResize = "resize".= True
autosizeProperty AContent = ("contains", fromT "content")
autosizeProperty APadding = ("contains", fromT "padding")


{-|

Declare the way the view is sized. See the
<https://vega.github.io/vega-lite/docs/size.html#autosize Vega-Lite documentation>
for details.

@
toVegaLite
    [ width 250
    , height 300
    , autosize [ AFit, APadding, AResize ]
    , dataFromUrl "data/population.json" []
    , mark Bar []
    , enc []
    ]
@
-}
autosize :: [Autosize] -> (VLProperty, VLSpec)
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
fieldTitleLabel Function = "function"
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
-}

data LegendConfig
    = CornerRadius Double
    | FillColor T.Text
    | Orient LegendOrientation
    | Offset Double
    | StrokeColor T.Text
    | LeStrokeDash [Double]
    | LeStrokeWidth Double
    | LePadding Double
    | GradientLabelBaseline VAlign
    | GradientLabelLimit Double
    | GradientLabelOffset Double
    | GradientStrokeColor T.Text
    | GradientStrokeWidth Double
    | GradientHeight Double
    | GradientWidth Double
    | LeLabelAlign HAlign
    | LeLabelBaseline VAlign
    | LeLabelColor T.Text
    | LeLabelFont T.Text
    | LeLabelFontSize Double
    | LeLabelLimit Double
    | LeLabelOffset Double
    | LeShortTimeLabels Bool
    | EntryPadding Double
    | SymbolColor T.Text
    | SymbolType Symbol
    | SymbolSize Double
    | SymbolStrokeWidth Double
    | LeTitleAlign HAlign
    | LeTitleBaseline VAlign
    | LeTitleColor T.Text
    | LeTitleFont T.Text
    | LeTitleFontSize Double
    | LeTitleFontWeight FontWeight
    | LeTitleLimit Double
    | LeTitlePadding Double


legendConfigProperty :: LegendConfig -> LabelledSpec
legendConfigProperty (CornerRadius r) = "cornerRadius" .= r
legendConfigProperty (FillColor s) = "fillColor" .= s
legendConfigProperty (Orient orl) = "orient" .= legendOrientLabel orl
legendConfigProperty (Offset x) = "offset" .= x
legendConfigProperty (StrokeColor s) = "strokeColor" .= s
legendConfigProperty (LeStrokeDash xs) = "strokeDash" .= map toJSON xs
legendConfigProperty (LeStrokeWidth x) = "strokeWidth" .= x
legendConfigProperty (LePadding x) = "padding" .= x
legendConfigProperty (GradientLabelBaseline va) = "gradientLabelBaseline" .= vAlignLabel va
legendConfigProperty (GradientLabelLimit x) = "gradientLabelLimit" .= x
legendConfigProperty (GradientLabelOffset x) = "gradientLabelOffset" .= x
legendConfigProperty (GradientStrokeColor s) = "gradientStrokeColor" .= s
legendConfigProperty (GradientStrokeWidth x) = "gradientStrokeWidth" .= x
legendConfigProperty (GradientHeight x) = "gradientHeight" .= x
legendConfigProperty (GradientWidth x) = "gradientWidth" .= x
legendConfigProperty (LeLabelAlign ha) = "labelAlign" .= hAlignLabel ha
legendConfigProperty (LeLabelBaseline va) = "labelBaseline" .= vAlignLabel va
legendConfigProperty (LeLabelColor s) = "labelColor" .= s
legendConfigProperty (LeLabelFont s) = "labelFont" .= s
legendConfigProperty (LeLabelFontSize x) = "labelFontSize" .= x
legendConfigProperty (LeLabelLimit x) = "labelLimit" .= x
legendConfigProperty (LeLabelOffset x) = "labelOffset" .= x
legendConfigProperty (LeShortTimeLabels b) = "shortTimeLabels" .= b
legendConfigProperty (EntryPadding x) = "entryPadding" .= x
legendConfigProperty (SymbolColor s) = "symbolColor" .= s
legendConfigProperty (SymbolType s) = "symbolType" .= symbolLabel s
legendConfigProperty (SymbolSize x) = "symbolSize" .= x
legendConfigProperty (SymbolStrokeWidth x) = "symbolStrokeWidth" .= x
legendConfigProperty (LeTitleAlign ha) = "titleAlign" .= hAlignLabel ha
legendConfigProperty (LeTitleBaseline va) = "titleBaseline" .= vAlignLabel va
legendConfigProperty (LeTitleColor s) = "titleColor" .= s
legendConfigProperty (LeTitleFont s) = "titleFont" .= s
legendConfigProperty (LeTitleFontSize x) = "titleFontSize" .= x
legendConfigProperty (LeTitleFontWeight fw) = "titleFontWeight" .= fontWeightSpec fw
legendConfigProperty (LeTitleLimit x) = "titleLimit" .= x
legendConfigProperty (LeTitlePadding x) = "titlePadding" .= x


{-|

Indicates the legend orientation. See the
<https://vega.github.io/vega-lite/docs/legend.html#config Vega-Lite documentation>
for more details.
-}

data LegendOrientation
    = LOBottomLeft
    | LOBottomRight
    | LOLeft
    | LONone
    | LORight
    | LOTopLeft
    | LOTopRight


legendOrientLabel :: LegendOrientation -> T.Text
legendOrientLabel LOLeft = "left"
legendOrientLabel LOBottomLeft = "bottom-left"
legendOrientLabel LOBottomRight = "bottom-right"
legendOrientLabel LORight = "right"
legendOrientLabel LOTopLeft = "top-left"
legendOrientLabel LOTopRight = "top-right"
legendOrientLabel LONone = "none"


{-|

Legend properties. For more detail see the
<https://vega.github.io/vega-lite/docs/legend.html#legend-properties Vega-Lite documentation>.
-}
data LegendProperty
    = LEntryPadding Double
    | LFormat T.Text
    | LOffset Double
    | LOrient LegendOrientation
    | LPadding Double
    | LTickCount Double
    | LTitle T.Text
    | LType Legend
    | LValues LegendValues
    | LZIndex Int


legendProperty :: LegendProperty -> LabelledSpec
legendProperty (LType lType) = "type" .= legendLabel lType
legendProperty (LEntryPadding x) = "entryPadding" .= x
legendProperty (LFormat s) = "format" .= s
legendProperty (LOffset x) = "offset" .= x
legendProperty (LOrient orl) = "orient" .= legendOrientLabel orl
legendProperty (LPadding x) = "padding" .= x
legendProperty (LTickCount x) = "tickCount" .= x
legendProperty (LTitle ttl) = "title" .= if T.null ttl then A.Null else fromT ttl
legendProperty (LValues vals) =
  let ls = case vals of
        LNumbers xs    -> map toJSON xs
        LDateTimes dts -> map (object . map dateTimeProperty) dts
        LStrings ss    -> map toJSON ss
  in "values" .= ls
legendProperty (LZIndex n) = "zindex" .= n


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

{-|

Types of geographic map projection. These are based on a subset of those provided
by the <https://github.com/d3/d3-geo d3-geo library>. For details of available
projections see the
<https://vega.github.io/vega-lite/docs/projection.html#projection-types Vega-Lite documentation>.
-}
data Projection
    = Albers
    | AlbersUsa
    | AzimuthalEqualArea
    | AzimuthalEquidistant
    | ConicConformal
    | ConicEqualArea
    | ConicEquidistant
    | Custom T.Text
      -- ^ Specify the name of the custom D3 prohection to use. See the
      --   <https://vega.github.io/vega/docs/projections/#register Vega API>
      --   for more information.
    | Equirectangular
    | Gnomonic
    | Mercator
    | Orthographic
    | Stereographic
    | TransverseMercator


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
projectionLabel Mercator = "mercator"
projectionLabel Orthographic = "orthographic"
projectionLabel Stereographic = "stereographic"
projectionLabel TransverseMercator = "transverseMercator"


-- | Specifies a clipping rectangle in pixel units for defining
--   the clip extent of a map projection.

data ClipRect
    = NoClip
    | LTRB Double Double Double Double
      -- ^ The left, top, right, and bottom extents.


{-|

Properties for customising a geospatial projection that converts longitude,latitude
pairs into planar (x,y) coordinate pairs for rendering and query. For details see the
<https://vega.github.io/vega-lite/docs/projection.html Vega-Lite documentation>.
-}
data ProjectionProperty
    = PType Projection
    | PClipAngle (Maybe Double)
    | PClipExtent ClipRect
    | PCenter Double Double
    | PRotate Double Double Double
    | PPrecision Double
    | PCoefficient Double
    | PDistance Double
    | PFraction Double
    | PLobes Int
    | PParallel Double
    | PRadius Double
    | PRatio Double
    | PSpacing Double
    | PTilt Double


projectionProperty :: ProjectionProperty -> LabelledSpec
projectionProperty (PType proj) = "type" .= projectionLabel proj
projectionProperty (PClipAngle numOrNull) = "clipAngle" .= maybe A.Null toJSON numOrNull
projectionProperty (PClipExtent rClip) =
  ("clipExtent", case rClip of
    NoClip -> A.Null
    LTRB l t r b -> toJSON (map toJSON [l, t, r, b])
  )
projectionProperty (PCenter lon lat) = "center" .= map toJSON [lon, lat]
projectionProperty (PRotate lambda phi gamma) = "rotate" .= map toJSON [lambda, phi, gamma]
projectionProperty (PPrecision pr) = "precision" .= pr
projectionProperty (PCoefficient x) = "coefficient" .= x
projectionProperty (PDistance x) = "distance" .= x
projectionProperty (PFraction x) = "fraction" .= x
projectionProperty (PLobes n) = "lobes" .= n
projectionProperty (PParallel x) = "parallel" .= x
projectionProperty (PRadius x) = "radius" .= x
projectionProperty (PRatio x) = "ratio" .= x
projectionProperty (PSpacing x) = "spacing" .= x
projectionProperty (PTilt x) = "tilt" .= x


{-|

Sets the cartographic projection used for geospatial coordinates. A projection
defines the mapping from @(longitude,latitude)@ to an @(x,y)@ plane used for rendering.
This is useful when using the 'Geoshape' mark. For further details see the
<https://vega.github.io/vega-lite/docs/projection.html Vega-Lite documentation>.

@
proj = projection [ PType Orthographic, PRotate (-40) 0 0 ]
@
-}
projection :: [ProjectionProperty] -> (VLProperty, VLSpec)
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
    | SCBandPaddingOuter Double
    | SCClamp Bool
    | SCMaxBandSize Double
    | SCMinBandSize Double
    | SCMaxFontSize Double
    | SCMinFontSize Double
    | SCMaxOpacity Double
    | SCMinOpacity Double
    | SCMaxSize Double
    | SCMinSize Double
    | SCMaxStrokeWidth Double
    | SCMinStrokeWidth Double
    | SCPointPadding Double
    | SCRangeStep (Maybe Double)
    | SCRound Bool
    | SCTextXRangeStep Double
    | SCUseUnaggregatedDomain Bool


scaleConfigProperty :: ScaleConfig -> LabelledSpec
scaleConfigProperty (SCBandPaddingInner x) = "bandPaddingInner" .= x
scaleConfigProperty (SCBandPaddingOuter x) = "bandPaddingOuter" .= x
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
-}
data SelectionProperty
    = On T.Text
      -- ^ A <https://vega.github.io/vega/docs/event-streams Vega event stream>
      --   or the empty string (which sets the property to @false@).
    | Translate T.Text
      -- ^ A <https://vega.github.io/vega/docs/event-streams Vega event stream>
      --   or the empty string (which sets the property to @false@).
    | Zoom T.Text
      -- ^ A <https://vega.github.io/vega/docs/event-streams Vega event stream>
      --   or the empty string (which sets the property to @false@).
    | Fields [T.Text]
    | Encodings [Channel]
    | Empty
    | ResolveSelections SelectionResolution
    | SelectionMark [SelectionMarkProperty]
    | BindScales
    | Bind [Binding]
    | Nearest Bool
    | Toggle T.Text
      -- ^ A <https://vega.github.io/vega/docs/expressions Vega expression> that evaluates
      --   to @true@ or @false@.


selectionProperty :: SelectionProperty -> LabelledSpec
selectionProperty (Fields fNames) = "fields" .= map toJSON fNames
selectionProperty (Encodings channels) = "encodings" .= map (toJSON . channelLabel) channels
selectionProperty (On e) = "on" .= e
selectionProperty Empty = "empty" .= ("none" :: T.Text)
selectionProperty (ResolveSelections res) = "resolve" .= selectionResolutionLabel res
selectionProperty (SelectionMark markProps) = "mark" .= object (map selectionMarkProperty markProps)
selectionProperty BindScales = "bind" .= ("scales" :: T.Text)
selectionProperty (Bind binds) = "bind" .= object (map bindingSpec binds)
selectionProperty (Nearest b) = "nearest" .= b
selectionProperty (Toggle expr) = "toggle" .= expr
selectionProperty (Translate e) = "translate" .= if T.null e then toJSON False else toJSON e
selectionProperty (Zoom e) = "zoom" .= if T.null e then toJSON False else toJSON e


-- | Indicates a channel type to be used in a resolution specification.

data Channel
    = ChX
    | ChY
    | ChX2
    | ChY2
    | ChColor
    | ChFill
    | ChStroke
    | ChOpacity
    | ChShape
    | ChSize


channelLabel :: Channel -> T.Text
channelLabel ChX = "x"
channelLabel ChY = "y"
channelLabel ChX2 = "x2"
channelLabel ChY2 = "y2"
channelLabel ChColor = "color"
channelLabel ChFill = "fill"
channelLabel ChStroke = "stroke"
channelLabel ChOpacity = "opacity"
channelLabel ChShape = "shape"
channelLabel ChSize = "size"


{-|

Determines how selections in faceted or repeated views are resolved. See the
<https://vega.github.io/vega-lite/docs/selection.html#resolve Vega-Lite documentation>
for details
-}
data SelectionResolution
    = Global
    | Union
    | Intersection


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
    | SMFillOpacity Double
    | SMStroke T.Text
    | SMStrokeOpacity Double
    | SMStrokeWidth Double
    | SMStrokeDash [Double]
    | SMStrokeDashOffset Double


selectionMarkProperty :: SelectionMarkProperty -> LabelledSpec
selectionMarkProperty (SMFill colour) = "fill" .= colour
selectionMarkProperty (SMFillOpacity x) = "fillOpacity" .= x
selectionMarkProperty (SMStroke colour) = "stroke" .= colour
selectionMarkProperty (SMStrokeOpacity x) = "strokeOpacity" .= x
selectionMarkProperty (SMStrokeWidth x) = "strokeWidth" .= x
selectionMarkProperty (SMStrokeDash xs) = "strokeDash" .= map toJSON xs
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
inputProperty (InMin x) = "min".= x
inputProperty (InMax x) = "max".= x
inputProperty (InStep x) = "step".= x
inputProperty (Debounce x) = "debounce".= x
inputProperty (InName s) = "name" .= s
inputProperty (InOptions opts) = "options" .= map toJSON opts
inputProperty (InPlaceholder el) = "placeholder" .= toJSON el
inputProperty (Element el) = "element" .= toJSON el


{-|

Describes the binding property of a selection based on some HTML input element
such as a checkbox or radio button. For details see the
<https://vega.github.io/vega-lite/docs/bind.html#scale-binding Vega-Lite documentation>
and the
<https://vega.github.io/vega/docs/signals/#bind Vega input binding documentation>.
-}
data Binding
    = IRange T.Text [InputProperty]
    | ICheckbox T.Text [InputProperty]
    | IRadio T.Text [InputProperty]
    | ISelect T.Text [InputProperty]
      -- TODO: Check validity: The following input types can generate a warning if options are included even if options appear to have an effect (e.g. placeholder)
    | IText T.Text [InputProperty]
    | INumber T.Text [InputProperty]
    | IDate T.Text [InputProperty]
    | ITime T.Text [InputProperty]
    | IMonth T.Text [InputProperty]
    | IWeek T.Text [InputProperty]
    | IDateTimeLocal T.Text [InputProperty]
    | ITel T.Text [InputProperty]
    | IColor T.Text [InputProperty]


bindingSpec :: Binding -> LabelledSpec
bindingSpec bnd =
  let (lbl, input, ps) = case bnd of
        IRange label props -> (label, "range" :: T.Text, props)
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
    | AMiddle
    | AEnd


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
    | TAngle Double
    | TBaseline VAlign
    | TColor T.Text
    | TFont T.Text
    | TFontSize Double
    | TFontWeight FontWeight
    | TLimit Double
    | TOffset Double
    | TOrient Side


titleConfigSpec :: TitleConfig -> LabelledSpec
titleConfigSpec (TAnchor an) = "anchor" .= anchorLabel an
titleConfigSpec (TAngle x) = "angle" .= x
titleConfigSpec (TBaseline va) = "baseline" .= vAlignLabel va
titleConfigSpec (TColor clr) = "color" .= clr
titleConfigSpec (TFont fnt) = "font" .= fnt
titleConfigSpec (TFontSize x) = "fontSize" .= x
titleConfigSpec (TFontWeight w) = "fontWeight" .= fontWeightSpec w
titleConfigSpec (TLimit x) = "limit" .= x
titleConfigSpec (TOffset x) = "offset" .= x
titleConfigSpec (TOrient sd) = "orient" .= sideLabel sd


{-|

View configuration property. These are used to configure the style of a single
view within a visualization such as its size and default fill and stroke colors.
For further details see the
<https://vega.github.io/vega-lite/docs/spec.html#config Vega-Lite documentation>.
-}
data ViewConfig
    = ViewWidth Double
    | ViewHeight Double
    | Clip Bool
    | Fill (Maybe T.Text)
    | FillOpacity Double
    | Stroke (Maybe T.Text)
    | StrokeOpacity Double
    | StrokeWidth Double
    | StrokeDash [Double]
    | StrokeDashOffset Double


viewConfigProperty :: ViewConfig -> LabelledSpec
viewConfigProperty (ViewWidth x) = "width" .= x
viewConfigProperty (ViewHeight x) = "height" .= x
viewConfigProperty (Clip b) = "clip" .= b
viewConfigProperty (Fill ms) = "fill" .= fromMaybe "" ms
viewConfigProperty (FillOpacity x) = "fillOpacity" .= x
viewConfigProperty (Stroke ms) = "stroke" .= fromMaybe "" ms
viewConfigProperty (StrokeOpacity x) = "strokeOpacity" .= x
viewConfigProperty (StrokeWidth x) = "strokeWidth" .= x
viewConfigProperty (StrokeDash xs) = "strokeDash" .= map toJSON xs
viewConfigProperty (StrokeDashOffset x) = "strokeDashOffset" .= x


{-|
Type of configuration property to customise. See the
<https://vega.github.io/vega-lite/docs/config.html Vega-Lite documentation>
for details.
-}
data ConfigurationProperty
    = AreaStyle [MarkProperty]
    | Autosize [Autosize]
    | Axis [AxisConfig]
    | AxisX [AxisConfig]
    | AxisY [AxisConfig]
    | AxisLeft [AxisConfig]
    | AxisRight [AxisConfig]
    | AxisTop [AxisConfig]
    | AxisBottom [AxisConfig]
    | AxisBand [AxisConfig]
    | Background T.Text
    | BarStyle [MarkProperty]
    | CircleStyle [MarkProperty]
    | CountTitle T.Text
    | FieldTitle FieldTitleProperty
    | Legend [LegendConfig]
    | LineStyle [MarkProperty]
    | MarkStyle [MarkProperty]
    | NamedStyle T.Text [MarkProperty]
    | NumberFormat T.Text
    | Padding Padding
    | PointStyle [MarkProperty]
    | Projection [ProjectionProperty]
    | Range [RangeConfig]
    | RectStyle [MarkProperty]
    | RemoveInvalid Bool
    | RuleStyle [MarkProperty]
    | Scale [ScaleConfig]
    | SelectionStyle [(Selection, [SelectionProperty])]
    | SquareStyle [MarkProperty]
    | Stack StackProperty
    | TextStyle [MarkProperty]
    | TickStyle [MarkProperty]
    | TitleStyle [TitleConfig]
    | TimeFormat T.Text
    | View [ViewConfig]


configProperty :: ConfigurationProperty -> LabelledSpec
configProperty (Autosize aus) = "autosize" .= object (map autosizeProperty aus)
configProperty (Background bg) = "background" .= bg
configProperty (CountTitle ttl) = "countTitle" .= ttl
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
configProperty (MarkStyle mps) = "mark" .= object (map markProperty mps)
configProperty (Projection pps) = "projection" .= object (map projectionProperty pps)
configProperty (AreaStyle mps) = "area" .= object (map markProperty mps)
configProperty (BarStyle mps) = "bar" .= object (map markProperty mps)
configProperty (CircleStyle mps) = "circle" .= object (map markProperty mps)
configProperty (LineStyle mps) = "line" .= object (map markProperty mps)
configProperty (PointStyle mps) = "point" .= object (map markProperty mps)
configProperty (RectStyle mps) = "rect" .= object (map markProperty mps)
configProperty (RuleStyle mps) = "rule" .= object (map markProperty mps)
configProperty (SquareStyle mps) = "square" .= object (map markProperty mps)
configProperty (TextStyle mps) = "text" .= object (map markProperty mps)
configProperty (TickStyle mps) = "tick" .= object (map markProperty mps)
configProperty (TitleStyle tcs) = "title" .= object (map titleConfigSpec tcs)
configProperty (NamedStyle nme mps) = "style" .= object [nme .= object (map markProperty mps)]
configProperty (Scale scs) = "scale" .= object (map scaleConfigProperty scs)
configProperty (Stack sp) = stackProperty sp
configProperty (Range rcs) = "range" .= object (map rangeConfigProperty rcs)
configProperty (SelectionStyle selConfig) =
  let selProp (sel, sps) = selectionLabel sel .= object (map selectionProperty sps)
  in "selection" .= object (map selProp selConfig)
configProperty (View vcs) = "view" .= object (map viewConfigProperty vcs)


{-|

Axis configuration options for customising all axes. See the
<https://vega.github.io/vega-lite/docs/axis.html#general-config Vega-Lite documentation>
for more details.
-}
data AxisConfig
    = BandPosition Double
    | Domain Bool
    | DomainColor T.Text
    | DomainWidth Double
    | MaxExtent Double
    | MinExtent Double
    | Grid Bool
    | GridColor T.Text
    | GridDash [Double]
    | GridOpacity Double
    | GridWidth Double
    | Labels Bool
    | LabelAngle Double
    | LabelColor T.Text
    | LabelFont T.Text
    | LabelFontSize Double
    | LabelLimit Double
    | LabelOverlap OverlapStrategy
    | LabelPadding Double
    | ShortTimeLabels Bool
    | Ticks Bool
    | TickColor T.Text
    | TickRound Bool
    | TickSize Double
    | TickWidth Double
    | TitleAlign HAlign
    | TitleAngle Double
    | TitleBaseline VAlign
    | TitleColor T.Text
    | TitleFont T.Text
    | TitleFontWeight FontWeight
    | TitleFontSize Double
    | TitleLimit Double
    | TitleMaxLength Double
    | TitlePadding Double
    | TitleX Double
    | TitleY Double


axisConfigProperty :: AxisConfig -> LabelledSpec
axisConfigProperty (BandPosition x) = ("bandPosition", toJSON x)
axisConfigProperty (Domain b) = ("domain", toJSON b)
axisConfigProperty (DomainColor c) = ("domainColor", fromT c)
axisConfigProperty (DomainWidth w) = ("domainWidth", toJSON w)
axisConfigProperty (MaxExtent n) = ("maxExtent", toJSON n)
axisConfigProperty (MinExtent n) = ("minExtent", toJSON n)
axisConfigProperty (Grid b) = ("grid", toJSON b)
axisConfigProperty (GridColor c) = ("gridColor", fromT c)
axisConfigProperty (GridDash ds) = ("gridDash", toJSON (map toJSON ds))
axisConfigProperty (GridOpacity o) = ("gridOpacity", toJSON o)
axisConfigProperty (GridWidth x) = ("gridWidth", toJSON x)
axisConfigProperty (Labels b) = ("labels", toJSON b)
axisConfigProperty (LabelAngle angle) = ("labelAngle", toJSON angle)
axisConfigProperty (LabelColor c) = ("labelColor", fromT c)
axisConfigProperty (LabelFont f) = ("labelFont", fromT f)
axisConfigProperty (LabelFontSize x) = ("labelFontSize", toJSON x)
axisConfigProperty (LabelLimit x) = ("labelLimit", toJSON x)
axisConfigProperty (LabelOverlap strat) = ("labelOverlap", fromT (overlapStrategyLabel strat))
axisConfigProperty (LabelPadding pad) = ("labelPadding", toJSON pad)
axisConfigProperty (ShortTimeLabels b) = ("shortTimeLabels", toJSON b)
axisConfigProperty (Ticks b) = ("ticks", toJSON b)
axisConfigProperty (TickColor c) = ("tickColor", fromT c)
axisConfigProperty (TickRound b) = ("tickRound", toJSON b)
axisConfigProperty (TickSize x) = ("tickSize", toJSON x)
axisConfigProperty (TickWidth x) = ("tickWidth", toJSON x)
axisConfigProperty (TitleAlign align) = ("titleAlign", fromT (hAlignLabel align))
axisConfigProperty (TitleAngle angle) = ("titleAngle", toJSON angle)
axisConfigProperty (TitleBaseline va) = ("titleBaseline", fromT (vAlignLabel va))
axisConfigProperty (TitleColor c) = ("titleColor", fromT c)
axisConfigProperty (TitleFont f) = ("titleFont", fromT f)
axisConfigProperty (TitleFontWeight w) = ("titleFontWeight", fontWeightSpec w)
axisConfigProperty (TitleFontSize x) = ("titleFontSize", toJSON x)
axisConfigProperty (TitleLimit x) = ("titleLimit", toJSON x)
axisConfigProperty (TitleMaxLength x) = ("titleMaxLength", toJSON x)
axisConfigProperty (TitlePadding x) = ("titlePadding", toJSON x)
axisConfigProperty (TitleX x) = ("titleX", toJSON x)
axisConfigProperty (TitleY y) = ("titleY", toJSON y)


{-|

Used for creating logical compositions. For example

@
color
    [ MSelectionCondition (Or (SelectionName "alex") (SelectionName "morgan"))
        [ MAggregate Count, MName "*", MmType Quantitative ]
        [ MString "gray" ]
    ]
@

Logical compositions can be nested to any level as shown in this example

@
Not (And (Expr "datum.IMDB_Rating === null") (Expr "datum.Rotten_Tomatoes_Rating === null") )
@
-}
data BooleanOp
    = Expr T.Text
    | Selection T.Text
    | SelectionName T.Text
    | And BooleanOp BooleanOp
    | Or BooleanOp BooleanOp
    | Not BooleanOp


booleanOpSpec :: BooleanOp -> VLSpec
booleanOpSpec (Expr expr) = toJSON expr
booleanOpSpec (SelectionName selName) = toJSON selName
booleanOpSpec (Selection sel) = object ["selection" .= sel]
booleanOpSpec (And operand1 operand2) = object ["and" .= [booleanOpSpec operand1, booleanOpSpec operand2]]
booleanOpSpec (Or operand1 operand2) = object ["or" .= [booleanOpSpec operand1, booleanOpSpec operand2]]
booleanOpSpec (Not operand) = object ["not" .= booleanOpSpec operand]


{-|

Type of filtering operation. See the
<https://vega.github.io/vega-lite/docs/filter.html Vega-Lite documentation>
for details.
-}
data Filter
    = FEqual T.Text DataValue
    | FExpr T.Text
    | FCompose BooleanOp
    | FSelection T.Text
    | FOneOf T.Text DataValues
    | FRange T.Text FilterRange


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
    | GeoPoints [(Double, Double)]
    | GeoLine [(Double, Double)]
    | GeoLines [[(Double, Double)]]
    | GeoPolygon [[(Double, Double)]]
    | GeoPolygons [[[(Double, Double)]]]


{-|

Specifies a geometric object to be used in a geoShape specification. The first
parameter is the geometric type, the second an optional list of properties to be
associated with the object.

@
geojson =
    geometry (GeoPolygon [ [ ( -3, 59 ), ( 4, 59 ), ( 4, 52 ), ( -3, 59 ) ] ]) []
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

Indicates whether or not a scale domain should be independent of others in a
composite visualization. See the
<https://vega.github.io/vega-lite/docs/resolve.html Vega-Lite documentation> for
details.
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
-}
data HeaderProperty
    = HFormat T.Text
    | HTitle T.Text


headerProperty :: HeaderProperty -> LabelledSpec
headerProperty (HFormat fmt) = "format" .= fmt
headerProperty (HTitle ttl) = "title" .= ttl


-- | Types of hyperlink channel property used for linking marks or text to URLs.

data HyperlinkChannel
    = HName T.Text
    | HRepeat Arrangement
    | HmType Measurement
    | HBin [BinProperty]
    | HAggregate Operation
    | HTimeUnit TimeUnit
    | HSelectionCondition BooleanOp [HyperlinkChannel] [HyperlinkChannel]
    | HDataCondition BooleanOp [HyperlinkChannel] [HyperlinkChannel]
    | HString T.Text


hyperlinkChannelProperty :: HyperlinkChannel -> [LabelledSpec]
hyperlinkChannelProperty (HName s) = [field_ s]
hyperlinkChannelProperty (HRepeat arr) = ["field" .= object [repeat_ arr]]
hyperlinkChannelProperty (HmType t) = [type_ t]
hyperlinkChannelProperty (HBin bps) = [bin bps]
hyperlinkChannelProperty (HSelectionCondition selName ifClause elseClause) =
  let h = ("condition", hkey)
      toProps = concatMap hyperlinkChannelProperty
      hkey = object (("selection", booleanOpSpec selName) : toProps ifClause)
      hs = toProps elseClause
  in h : hs
hyperlinkChannelProperty (HDataCondition predicate ifClause elseClause) =
  let h = ("condition", hkey)
      toProps = concatMap hyperlinkChannelProperty
      hkey = object (("test", booleanOpSpec predicate) : toProps ifClause)
      hs = toProps elseClause
  in h : hs
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
color
    [ MName "year"
    , MmType Ordinal
    , MScale (domainRangeMap ( 1955, "#e6959c" ) ( 2000, "#911a24" ))
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
color
    [ MName "weather"
    , MmType Nominal
    , MScale (
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
fields named here can be referenced in an encoding with @PRepeat Column@
or @PRepeat Row@.
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
data FacetChannel
    = FName T.Text
    | FmType Measurement
    | FBin [BinProperty]
    | FAggregate Operation
    | FTimeUnit TimeUnit
    | FHeader [HeaderProperty]


facetChannelProperty :: FacetChannel -> LabelledSpec
facetChannelProperty (FName s) = field_ s
facetChannelProperty (FmType measure) = type_ measure
facetChannelProperty (FBin bps) = bin bps
facetChannelProperty (FAggregate op) = aggregate_ op
facetChannelProperty (FTimeUnit tu) = timeUnit_ tu
facetChannelProperty (FHeader hProps) = "header" .= object (map headerProperty hProps)


-- | Types of text channel property used for displaying text as part of the visualization.

data TextChannel
    = TName T.Text
    | TRepeat Arrangement
    | TmType Measurement
    | TBin [BinProperty]
    | TAggregate Operation
    | TTimeUnit TimeUnit
    | TSelectionCondition BooleanOp [TextChannel] [TextChannel]
    | TDataCondition BooleanOp [TextChannel] [TextChannel]
    | TFormat T.Text


textChannelProperty :: TextChannel -> [LabelledSpec]
textChannelProperty (TName s) = [field_  s]
textChannelProperty (TRepeat arr) = ["field" .= object [repeat_ arr]]
textChannelProperty (TmType measure) = [type_ measure]
textChannelProperty (TBin bps) = [bin bps]
textChannelProperty (TAggregate op) = [aggregate_ op]
textChannelProperty (TTimeUnit tu) = [timeUnit_ tu]
textChannelProperty (TFormat fmt) = ["format" .= fmt]
textChannelProperty (TSelectionCondition selName ifClause elseClause) =
  let h = ("condition", hkey)
      toProps = concatMap textChannelProperty
      hkey = object (("selection", booleanOpSpec selName) : toProps ifClause)
      hs = toProps elseClause
  in h : hs
textChannelProperty (TDataCondition predicate ifClause elseClause) =
  let h = ("condition", hkey)
      toProps = concatMap textChannelProperty
      hkey = object (("test", booleanOpSpec predicate) : toProps ifClause)
      hs = toProps elseClause
  in h : hs


-- | Properties of an ordering channel used for sorting data fields.

data OrderChannel
    = OName T.Text
    | ORepeat Arrangement
    | OmType Measurement
    | OBin [BinProperty]
    | OAggregate Operation
    | OTimeUnit TimeUnit
    | OSort [SortProperty]


orderChannelProperty :: OrderChannel -> LabelledSpec
orderChannelProperty (OName s) = field_ s
orderChannelProperty (ORepeat arr) = "field" .= object [repeat_ arr]
orderChannelProperty (OmType measure) = type_ measure
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
detailChannelProperty (DmType t) = type_ t
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

@
config =
    configure
        . configuration (Axis [ DomainWidth 1 ])
        . configuration (View [ Stroke (Just "transparent") ])
        . configuration (SelectionStyle [ ( Single, [ On "dblclick" ] ) ])
@
-}
configure :: [LabelledSpec] -> (VLProperty, VLSpec)
configure configs = (VLConfig, object configs)


{-|

Defines the fields that will be used to facet a view in rows or columns to create
a set of small multiples. This is used where the encoding of the visualization in small
multiples is identical, but data for each is grouped by the given fields. When
creating a faceted view in this way you also need to define a full specification
to apply to each of those facets using 'asSpec'.

@
toVegaLite
    [ facet [ RowBy [ FName \"Origin\", FmType Nominal ] ]
    , specifcation spec
    ]
@

See the
<https://vega.github.io/vega-lite/docs/facet.html Vega-Lite documentation>
for further details.

-}

facet :: [FacetMapping] -> (VLProperty, VLSpec)
facet fMaps = (VLFacet, object (map facetMappingProperty fMaps))


{-|

Overrides the default height of the visualization. If not specified the height
will be calculated based on the content of the visualization.

@
toVegaLite
    [ height 300
    , dataFromUrl "data/population.json" []
    , mark Bar []
    , enc []
    ]
@
-}
height :: Double -> (VLProperty, VLSpec)
height h = (VLHeight, toJSON h)


{-|

Assigns a list of specifications to be juxtaposed horizontally in a visualization.

@
toVegaLite
    [ dataFromUrl "data/driving.json" []
    , hConcat [ spec1, spec2 ]
    ]
@
-}
hConcat :: [VLSpec] -> (VLProperty, VLSpec)
hConcat specs = (VLHConcat, toJSON specs)


{-|

Assigns a list of specifications to superposed layers in a visualization.

@
toVegaLite
    [ dataFromUrl "data/driving.json" []
    , layer [ spec1, spec2 ]
    ]
@
-}
layer :: [VLSpec] -> (VLProperty, VLSpec)
layer specs = (VLLayer, toJSON specs)


{-|

Provides an optional name to be associated with the visualization.

@
toVegaLite
    [ name \"PopGrowth\"
    , dataFromUrl "data/population.json" []
    , mark Bar []
    , enc []
    ]
@
-}
name :: T.Text -> (VLProperty, VLSpec)
name s = (VLName, toJSON s)


{-|

Set the padding around the visualization in pixel units. The way padding is
interpreted will depend on the 'autosize' properties. See the
<https://vega.github.io/vega-lite/docs/spec.html#top-level-specifications Vega-Lite documentation>
for details.

@
toVegaLite
    [ width 500
    , padding (PEdges 20 10 5 15)
    , dataFromUrl "data/population.json" []
    , mark Bar []
    , enc []
    ]
@
-}
padding :: Padding -> (VLProperty, VLSpec)
padding pad = (VLPadding, paddingSpec pad)


{-|

Define the fields that will be used to compose rows and columns of a set of
small multiples. This is used where the encoding of the visualization in small
multiples is largely identical, but the data field used in each might vary. When
a list of fields is identified with @repeat@ you also need to define a full specification
to apply to each of those fields using 'asSpec'.

@
toVegaLite
    [ repeat [ ColumnFields [ \"Cat\", \"Dog\", \"Fish\" ] ]
    , specification (asSpec spec)
    ]
@

See the
<https://vega.github.io/vega-lite/docs/repeat.html Vega-Lite documentation>
for further details.

-}
repeat :: [RepeatFields] -> (VLProperty, VLSpec)
repeat fields = (VLRepeat, object (map repeatFieldsProperty fields))


{-|

Determine whether scales, axes or legends in composite views should share channel
encodings. This allows, for example, two different color encodings to be created
in a layered view, which otherwise by default would share color channels between
layers. Each resolution rule should be in a tuple pairing the channel to which it
applies and the rule type.

@
let res = resolve
            . resolution (RLegend [ ( ChColor, Independent ) ])
in toVegaLite
    [ dataFromUrl "data/movies.json" []
    , vConcat [ heatSpec, barSpec ]
    , res []
    ]
@

For more information see the
<https://vega.github.io/vega-lite/docs/resolve.html Vega-Lite documentation>.

-}
resolve :: [LabelledSpec] -> (VLProperty, VLSpec)
resolve res = (VLResolve, object res)


{-|

Create a full selection specification from a list of selections. For details
see the
<https://vega.github.io/vega-lite/docs/selection.html Vega-Lite documentation>.

@
selection = selection . select "view" Interval [ BindScales ]
@
-}
selection :: [LabelledSpec] -> (VLProperty, VLSpec)
selection sels = (VLSelection, object sels)


{-|

Defines a specification object for use with faceted and repeated small multiples.

@
toVegaLite
    [ facet [ RowBy [ FName \"Origin\", FmType Nominal ] ]
    , specifcation spec
    ]
@
-}
specification :: VLSpec -> (VLProperty, VLSpec)
specification spec = (VLSpec, spec)


{-|

Create a single transform from a list of transformation specifications. Note
that the order of transformations can be important, especially if labels created
with 'calculateAs', 'timeUnitAs', and 'binAs' are used in other transformations.
Using the functional composition pipeline idiom (as example below) allows you to
provide the transformations in the order intended in a clear manner.

@
trans = transform
        . filter (FExpr "datum.year == 2010")
        . calculateAs "datum.sex == 2 ? 'Female' : 'Male'" "gender"
@
-}

transform :: [LabelledSpec] -> (VLProperty, VLSpec)
transform transforms =
  let js = if null transforms then A.Null else toJSON (map assemble transforms)

      -- use the same approach as Elm of encoding the spec, then decoding it,
      -- rather than inspecting the structure of the JSON
      --
      assemble :: LabelledSpec -> VLSpec
      assemble (str, val) =

        let dval = decode (encode val)
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

          "timeUnit" ->
            case dval of
              Just (A.Array vs) | V.length vs == 3 -> object [ ("timeUnit", vs V.! 0)
                                                             , ("field", vs V.! 1)
                                                             , ("as", vs V.! 2) ]
              _ -> A.Null

          _ -> object [(str, val)]

    in (VLTransform, js)


{-|

Assigns a list of specifications to be juxtaposed vertically in a visualization.

@
toVegaLite
    [ dataFromUrl "data/driving.json" []
    , vConcat [ spec1, spec2 ]
    ]
@
-}
vConcat :: [VLSpec] -> (VLProperty, VLSpec)
vConcat specs = (VLVConcat, toJSON specs)


{-|

Override the default width of the visualization. If not specified the width
will be calculated based on the content of the visualization.

@
toVegaLite
    [ width 500
    , dataFromUrl "data/population.json" []
    , mark Bar []
    , enc []
    ]
@
-}
width :: Double -> (VLProperty, VLSpec)
width w = (VLWidth, toJSON w)


{-|

Defines a set of named aggregation transformations to be used when encoding
channels. This is useful when, for example, you wish to apply the same transformation
to a number of channels but do not want to define it each time. For further details
see the
<https://vega.github.io/vega-lite/docs/aggregate.html#aggregate-op-def Vega-Lite documentation>.

@
trans =
    transform
        . aggregate
            [ opAs Min "people" "lowerBound", opAs Max "people" "upperBound" ]
            [ "age" ]
@
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

Create a named binning transformation that may be referenced in other Transformations
or encodings. See the
<https://vega.github.io/vega-lite/docs/bin.html Vega-Lite documentation> for
more details. Note that usually, direct binning within an encoding is preferred
over this form of bin transformation.

@
trans =
    transform
        . binAs [ MaxBins 3 ] \"IMDB_Rating\" "ratingGroup"
@
-}
binAs ::
  [BinProperty]
  -- ^ An empty list means that the default binning is used.
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

Creates a new data field based on calculations from existing fields.
See the <https://vega.github.io/vega-lite/docs/calculate.html Vega-Lite documentation>
for further details.

@
trans =
    transform . calculateAs "datum.sex == 2 ? 'F' : 'M'" "gender"
@
-}
calculateAs ::
  T.Text
  -- ^ The calculation to perform.
  -> T.Text
  -- ^ The field to assign the new values.
  -> BuildLabelledSpecs
calculateAs expr label ols = ("calculate" .= [expr, label]) : ols


{-|

Encode a color channel.

@
color [ MName \"Species\", MmType Nominal ] []
@

Encoding a color channel will generate a legend by default. To stop the legend
appearing, just supply an empty list of legend properties to 'MLegend':

@
color [ MName \"Species\", MmType Nominal, MLegend [] ] []
@
-}
color ::
  [MarkChannel]
  -- ^ Control how the data field is encoded by color.
  -> BuildLabelledSpecs
color markProps ols =
  let cs = object (concatMap markChannelProperty markProps)
  in ("color", cs) : ols


{-|

Encodes a new facet to be arranged in columns.

@
enc =
    encoding
        . position X [ PName "people", PmType Quantitative ]
        . position Y [ PName "gender", PmType Nominal ]
        . column [ FName "age", FmType Ordinal ]
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

Defines a single configuration option to be applied globally across the visualization.
The first parameter identifies the type of configuration, the second a list of previous
configurations to which this one may be added.

@
configuration (Axis [ DomainWidth 4 ]) []
@
-}
configuration :: ConfigurationProperty -> BuildLabelledSpecs
configuration cfg ols = configProperty cfg : ols


{-|

Encode a \"level of detail\" channel. This provides a way of grouping by a field
but unlike, say 'color', all groups have the same visual properties. The first
parameter is a list of the field characteristics to be grouped. The second parameter
is a list of any previous channels to which this detail channel should be added. See the
<https://vega.github.io/vega-lite/docs/encoding.html#detail Vega-Lite documentation>
for details.

@
detail [ DName \"Species\", DmType Nominal ] []
@
-}
detail :: [DetailChannel] -> BuildLabelledSpecs
detail detailProps ols =
    ("detail" .= object (map detailChannelProperty detailProps)) : ols


{-|

Encode a fill channel. This acts in a similar way to encoding by 'color' but
only affects the interior of closed shapes. The first parameter is a list of mark
channel properties that characterise the way a data field is encoded by fill.
The second parameter is a list of any previous channels to which this fill channel
should be added.

@
fill [ MName \"Species\", MmType Nominal ] []
@

Note that if both @fill@ and 'color' encodings are specified, @fill@ takes precedence.

-}
fill :: [MarkChannel] -> BuildLabelledSpecs
fill markProps ols =
  ("fill" .= object (concatMap markChannelProperty markProps)) : ols


{-|

Adds the given filter operation a list of transformations that may be applied
to a channel or field. The first parameter is the filter operation and the second,
often implicit, parameter is the list of other filter operations to which this
should be added in sequence.

@
trans =
    transform
        . filter (FEqual \"Animal\" (Str \"Cat\"))
@

Filter operations can combine selections and data predicates with 'BooleanOp'
expressions:

@
trans =
    transform
        . filter (FCompose (And (Expr "datum.Weight_in_lbs > 3000") (Selection "brush")))
@
-}
filter :: Filter -> BuildLabelledSpecs
filter f ols =
  let js = case f of
        FExpr expr -> toJSON expr
        FCompose boolExpr -> booleanOpSpec boolExpr

        FEqual field val -> object [field_ field, "equal" .= dataValueSpec val]

        FSelection selName -> object ["selection" .= selName]

        FRange field vals ->
            let ans = case vals of
                        NumberRange mn mx -> map toJSON [mn, mx]
                        DateRange dMin dMax ->
                          [ object (map dateTimeProperty dMin)
                          , object (map dateTimeProperty dMax)
                          ]
            in object [field_ field, "range" .= ans]

        FOneOf field vals ->
            let ans = case vals of
                        Numbers xs -> map toJSON xs
                        DateTimes dts -> map (object . map dateTimeProperty) dts
                        Strings ss -> map toJSON ss
                        Booleans bs -> map toJSON bs

            in object [field_ field, "oneOf" .= ans]

  in ("filter", js) : ols


{-|

Encode a hyperlink channel. The first parameter is a list of hyperlink channel
properties that characterise the hyperlinking such as the destination url and cursor
type. The second parameter is a list of any previous encoding channels to which
this hyperlink channel should be added.

@
hyperlink [ HName \"Species\", HmType Nominal ] []
@

For further details see the
<https://vega.github.io/vega-lite/docs/encoding.html#href Vega-Lite documentation>.

-}
hyperlink :: [HyperlinkChannel] -> BuildLabelledSpecs
hyperlink hyperProps ols =
  ("href" .= object (concatMap hyperlinkChannelProperty hyperProps)) : ols


{-|

Perform a lookup of named fields between two data sources. This allows you to
find values in one data source based on the values in another (like a relational
join).

Unlike 'lookupAs', this function will only return the specific fields named in the
fourth parameter. If you wish to return the entire set of fields in the secondary
data source as a single object, use 'lookupAs'.

See the <https://vega.github.io/vega-lite/docs/lookup.html Vega-Lite documentation>
for further details.

The following would return the values in the @age@ and @height@ fields from
@lookup_people.csv@ for all rows where the value in the @name@ column in that
file matches the value of @person@ in the primary data source.

@
trans =
    transform
        . lookup "person" (dataFromUrl "data/lookup_people.csv" []) "name" [ "age", "height" ]
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

Perform an object lookup between two data sources. This allows you to find
values in one data source based on the values in another (like a relational
join).

Unlike 'lookup', this function returns the entire set of field values from the
secondary data source when keys match. Those fields are stored as an object with
the name provided in the fourth parameter.

See the
<https://vega.github.io/vega-lite/docs/lookup.html Vega-Lite documentation>
for further details.

In the following example, @personDetails@ would reference all the field values in
@lookup_people.csv@ for each row where the value in the @name@ column in that
file matches the value of @person@ in the primary data source.

@
trans = transform
        . lookupAs "person" (dataFromUrl "data/lookup_people.csv" []) "name" "personDetails"
@
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


{-|

Encode an opacity channel. The first parameter is a list of mark channel properties
that characterise the way a data field is encoded by opacity. The second parameter
is a list of any previous channels to which this opacity channel should be added.

@
opacity [ MName \"Age\", MmType Quantitative ] []
@
-}
opacity :: [MarkChannel] -> BuildLabelledSpecs
opacity markProps ols =
  ("opacity" .= object (concatMap markChannelProperty markProps)) : ols


{-|

Encode an order channel. The first parameter is a list of order field definitions
to define the channel. The second parameter is a list of any previous channels to
which this order channel is to be added.

@
enc =
    encoding
        . position X [ PName "miles", PmType Quantitative ]
        . position Y [ PName "gas", PmType Quantitative ]
        . order [ OName "year", OmType Temporal ]
@
-}
order :: [OrderChannel] -> BuildLabelledSpecs
order oDefs ols =
  ("order" .= object (map orderChannelProperty oDefs)) : ols


{-|

Encode a position channel.

@
enc =
    encoding
      . position X [ PName \"Animal\", PmType Ordinal ]
@

Encoding by position will generate an axis by default. To prevent the axis from
appearing, simply provide an empty list of axis properties to 'PAxis':

@
enc =
    encoding
      . position X [ PName \"Animal\", PmType Ordinal, PAxis [] ]
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
resolve
    . resolution (RScale [ ( ChY, Independent ) ])
@
-}
resolution :: Resolve -> BuildLabelledSpecs
resolution res ols = resolveProperty res : ols


{-|

Encode a new facet to be arranged in rows.

@
enc =
    encoding
        . position X [ PName "people", PmType Quantitative ]
        . position Y [ PName "gender", PmType Nominal ]
        . row [ FName "age", FmType Ordinal ]
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
    selection
        . select "view" Interval [ BindScales ] []
        . select "myBrush" Interval []
        . select "myPaintbrush" Multi [ On "mouseover", Nearest True ]
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
shape [ MName \"Species\", MmType Nominal ] []
@
-}
shape ::
  [MarkChannel]
  -- ^ What data values are used to control the shape parameters of the mark.
  -> BuildLabelledSpecs
shape markProps ols = ("shape" .= object (concatMap markChannelProperty markProps)) : ols


{-|

Encode a size channel.

@
size [ MName \"Age\", MmType Quantitative ] []
@
-}
size ::
  [MarkChannel]
  -- ^ What data values are used to control the size parameters of the mark.
  -> BuildLabelledSpecs
size markProps ols = ("size" .= object (concatMap markChannelProperty markProps)) : ols


{-|

Encode a stroke channel. This acts in a similar way to encoding by 'color' but
only affects the exterior boundary of marks.

@
stroke [ MName \"Species\", MmType Nominal ] []
@

Note that if both @stroke@ and 'color' encodings are specified, @stroke@ takes
precedence.

-}
stroke ::
  [MarkChannel]
  -- ^ What data values are used to control the stoke parameters of the mark.
  -> BuildLabelledSpecs
stroke markProps ols =
  ("stroke" .= object (concatMap markChannelProperty markProps)) : ols


{-|

Encode a text channel. See the
<https://vega.github.io/vega-lite/docs/encoding.html#text Vega-Lite documentation>
for further details on the text and tooltip channels and
<https://vega.github.io/vega-lite/docs/format.html Vega-Lite formatting documentation>
for formatting the appearance of the text.

@
enc =
    encoding
        . position X [ PName "miles", PmType Quantitative ]
        . position Y [ PName "gas", PmType Quantitative ]
        . text [ TName "miles", TmType Quantitative ]
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
trans = transform . timeUnitAs Month "date" "monthly"

enc = encoding
        . position X [ PName "date", PmType Temporal, PTimeUnit Day ]
        . position Y [ PAggregate Sum, PmType Quantitative ]
        . detail [ DName "monthly", DmType Temporal ]
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
enc = encoding
        . position X [ PName \"Horsepower\", PmType Quantitative ]
        . position Y [ PName \"Miles_per_Gallon\", PmType Quantitative ]
        . tooltip [ TName \"Year\", TmType Temporal, TFormat "%Y" ]
@
-}
tooltip ::
  [TextChannel]
  -- ^ The properties for the channel.
  -> BuildLabelledSpecs
tooltip tDefs ols =
  ("tooltip" .= object (concatMap textChannelProperty tDefs)) : ols
