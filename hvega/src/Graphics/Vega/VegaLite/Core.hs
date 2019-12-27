{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-|
Module      : Graphics.Vega.VegaLite.Core
Copyright   : (c) Douglas Burke, 2018-2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

This provides the functionality of the VegaLite module but is
not directly exported to the user.

-}

module Graphics.Vega.VegaLite.Core
       ( transform

       , aggregate
       , joinAggregate
       , opAs
       , timeUnitAs
       , Operation(..)

       , binAs
       , BinProperty(..)

       , stack
       , StackProperty(..)
       , StackOffset(..)

       , calculateAs

       , filter
       , Filter(..)
       , FilterRange(..)

       , flatten
       , flattenAs
       , fold
       , foldAs

       , lookup
       , lookupAs

       , impute
       , ImputeProperty(..)
       , ImMethod(..)

       , sample

       , window
       , Window(..)
       , WOperation(..)
       , WindowProperty(..)

       , mark
       , Mark(..)

       , MarkProperty(..)
       , StrokeCap(..)
       , StrokeJoin(..)

       , Orientation(..)
       , MarkInterpolation(..)
       , Symbol(..)
       , PointMarker(..)
       , LineMarker(..)
       , MarkErrorExtent(..)
       , TooltipContent(..)

       , Cursor(..)

       , encoding
       , Measurement(..)

       , position
       , Position(..)

       , PositionChannel(..)

       , SortProperty(..)
       , SortField(..)

       , AxisProperty(..)

       , HAlign(..)
       , VAlign(..)

       , OverlapStrategy(..)

       , Side(..)

       , size
       , color
       , fill
       , stroke
       , strokeWidth
       , opacity
       , fillOpacity
       , strokeOpacity
       , shape

       , MarkChannel(..)

       , LegendType(..)
       , LegendProperty(..)
       , LegendOrientation(..)
       , LegendValues(..)

       , text
       , tooltip
       , tooltips
       , TextChannel(..)
       , FontWeight(..)

       , hyperlink
       , HyperlinkChannel(..)

       , order
       , OrderChannel(..)

       , row
       , column

       , detail
       , DetailChannel(..)

       , ScaleProperty(..)
       , Scale(..)
       , categoricalDomainMap
       , domainRangeMap
       , ScaleDomain(..)
       , ScaleRange(..)
       , ScaleNice(..)

       , CInterpolate(..)

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

       , resolve
       , resolution
       , Resolve(..)
       , Channel(..)
       , Resolution(..)

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

       , HeaderProperty(..)

       , BooleanOp(..)

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

       , title

       , viewBackground
       , ViewBackground(..)

       , configure

       , AxisConfig(..)

       , LegendConfig(..)
       , LegendLayout(..)
       , BaseLegendLayout(..)

       , ScaleConfig(..)

       , RangeConfig(..)

       , TitleConfig(..)
       , TitleFrame(..)

       , ViewConfig(..)
       , APosition(..)
       , FieldTitleProperty(..)

       , FacetConfig(..)

       , ConcatConfig(..)

       -- not for external export
       , fromT
       , channelLabel
       , anchorLabel
       , sideLabel
       , hAlignLabel
       , vAlignLabel
       , strokeCapLabel
       , strokeJoinLabel
       , overlapStrategyLabel
       , fontWeightSpec
       , schemeProperty
       , orientationSpec
       , boundsSpec
       , symbolLabel
       , legendOrientLabel
       , compositionAlignmentSpec
       , stackOffset
       , titleConfigSpec
       , autosizeProperty
       , paddingSpec
       , header_
       , mprops_

       )
    where

-- VegaLite uses these symbols.
import Prelude hiding (filter, lookup, repeat)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V

-- Aeson's Value type conflicts with the Number type
import Data.Aeson (Value, decode, encode, object, toJSON, (.=))
import Data.Maybe (mapMaybe)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

-- added in base 4.8.0.0 / ghc 7.10.1
import Numeric.Natural (Natural)

import Graphics.Vega.VegaLite.Data
  ( DataValue(..)
  , DataValues(..)
  , dataValueSpec
  , dataValuesSpecs
  )
import Graphics.Vega.VegaLite.Input
  ( Data
  )
import Graphics.Vega.VegaLite.Specification
  ( VLProperty(..)
  , VLSpec
  , PropertySpec
  , LabelledSpec
  , BuildLabelledSpecs
  , Angle
  , Color
  , Opacity
  , ZIndex
  , asSpec
  , specification
  )
import Graphics.Vega.VegaLite.Time
  ( DateTime
  , TimeUnit
  , dateTimeProperty
  , timeUnitLabel
  )

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

-- TODO: should this turn an empty list into true?
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
      --   Tick marks can be added using 'MTicks' and outliers turned
      --   off with 'MNoOutliers' or configured with 'MOutliers'.
      --   For example:
      --
      --   @
      --   'mark' Boxplot
      --       [ 'MTicks' [ 'MColor' \"black\", 'MSize' 8 ]
      --       , 'MBox' [ 'MFill' \"grey\" ]
      --       , 'MOutliers' [ 'MColor' \"firebrick\" ]
      --   ]
      --   @
      --
      --   The range of the box plot is controlled with 'MExtent' with
      --   the 'IqrScale' or 'ExRange' options (the default is
      --   @IqrScale 1.5@).
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
let dvals = 'Graphics.Vega.VegaLite.dataFromUrl' \"city.json\" ['Graphics.Vega.VegaLite.TopojsonFeature' \"boroughs\"] []
    markOpts = 'mark' 'Geoshape' ['MFill' \"lightgrey\", 'MStroke' \"white\"]
in 'Graphics.Vega.VegaLite.toVegaLite' [dvals, markOpts]
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
      -- ^ Border properties for an 'ErrorBand' mark.
      --
      --   @since 0.4.0.0
    | MBox [MarkProperty]
      -- ^ Box-symbol properties for a 'Boxplot' mark.
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
      -- ^ Extent of whiskers used with 'Boxplot', 'ErrorBar', and
      --   'ErrorBand' marks.
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
      -- ^ Median-line properties for the 'Boxplot' mark.
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
      -- ^ Outlier symbol properties for the 'Boxplot' mark.
      --
      --   @since 0.4.0.0
    | MNoOutliers
      -- ^ Do not draw outliers with the 'Boxplot' mark.
      --
      --   @since 0.4.0.0
    | MPoint PointMarker
      -- ^ Appearance of a point marker joining the vertices of a line or area mark.
      --
      --   @since 0.4.0.0
    | MRadius Double
      -- ^ Polar coordinate radial offset of a text mark from its origin.
    | MRule [MarkProperty]
      -- ^ Rule (main line) properties for the 'ErrorBar' and 'Boxplot' marks.
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
      -- ^ Tick properties for the 'ErrorBar' or 'Boxplot' mark.
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
markProperty (MOutliers []) = "outliers" .= True  -- TODO: should mprops_ do this?
markProperty (MOutliers mps) = mprops_ "outliers" mps
markProperty MNoOutliers = "outliers" .= False
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
subject to a map projection (set using 'Graphics.Vega.VegaLite.projection') before being placed graphically.

-}
data Position
    = X
    | Y
    | X2
    -- ^ The secondary coordinate for ranged 'Area', 'Bar', 'Rect', and 'Rule'
    --    marks.
    | Y2
    -- ^ The secondary coordinate for ranged 'Area', 'Bar', 'Rect', and 'Rule'
    --    marks.
    | XError
      -- ^ Indicates that the 'X' channel represents the mid-point and
      --   the 'XError' channel gives the offset. If 'XError2' is not
      --   defined then this channel value is applied symmetrically.
      --
      --   @since 0.4.0.0
    | XError2
      -- ^ Used to support asymmetric error ranges defined as 'XError'
      --   and 'XError2'. One of 'XError' or 'XError2' channels must
      --   contain positive values and the other negative values.
      --
      --   @since 0.4.0.0
    | YError
      -- ^ Indicates that the 'Y' channel represents the mid-point and
      --   the 'YError' channel gives the offset. If 'YError2' is not
      --   defined then this channel value is applied symmetrically.
      --
      --   @since 0.4.0.0
    | YError2
      -- ^ Used to support asymmetric error ranges defined as 'YError'
      --   and 'YError2'. One of 'YError' or 'YError2' channels must
      --   contain positive values and the other negative values.
      --
      --   @since 0.4.0.0
    | Longitude
      -- ^ The longitude value for projections.
    | Latitude
      -- ^ The latitude value for projections.
    | Longitude2
      -- ^ A second longitude coordinate.
    | Latitude2
      -- ^ A second longitude coordinate.


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
      -- ^ The number base to use for automatic bin determination.
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
      -- ^ Alignment of the steps within the scale range. A value of
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
      -- 'Graphics.Vega.VegaLite.toVegaLite'
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
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'background' "rgb(251,247,238)"
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
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
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'description' "Population change of key regions since 1900"
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
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
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'title' "Population Growth" ['TColor' \"orange\"]
    , 'Graphics.Vega.VegaLite.dataFromUrl' \"data/population.json\" []
    , 'mark' 'Bar' []
    , 'encoding' ...
    ]
@

Prior to @0.4.0.0@ there was no way to set the title options
(other than using 'Graphics.Vega.VegaLite.configuration' with 'Graphics.Vega.VegaLite.TitleStyle').

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
To configure all axes, use 'AxisConfig' with a 'Graphics.Vega.VegaLite.configuration' instead. See the
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
    | AxLabelNoBound
      -- ^ No boundary overlap check is applied to labels. This is the
      --   default behavior.
      --
      --   See also 'AxLabelBound' and 'AxLabelBoundValue'.
      --
      --   @since 0.4.0.0
    | AxLabelBound
      -- ^ Labels are hidden if they exceed the axis range by more than 1
      --   pixel.
      --
      --   See also 'AxLabelNoBound' and 'AxLabelBoundValue'.
      --
      --   @since 0.4.0.0
    | AxLabelBoundValue Double
      -- ^ Labels are hidden if they exceed the axis range by more than
      --   the given number of pixels.
      --
      --   See also 'AxLabelNoBound' and 'AxLabelBound'.
      --
      --   @since 0.4.0.0
    | AxLabelColor Color
      -- ^ The label color.
      --
      --   @since 0.4.0.0
    | AxLabelNoFlush
      -- ^ The labels are not aligned flush to the scale. This is the
      --   default for non-continuous X scales.
      --
      --   See also 'AxLabelFlush' and 'AxLabelFlushValue'.
      --
      --   @since 0.4.0.0
    | AxLabelFlush
      -- ^ The first and last axis labels are aligned flush to the scale
      --   range.
      --
      --   See also 'AxLabelNoFlush' and 'AxLabelFlushValue'.
      --
      --   @since 0.4.0.0
    | AxLabelFlushValue Double
      -- ^ The labels are aligned flush, and the parameter determines
      --   the extra offset, in pixels, to apply to the first and last
      --   labels. This can help the labels better group (visually) with
      --   the corresponding axis ticks.
      --
      --   See also 'AxLabelNoFlush' and 'AxLabelFlush'.
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
      --   'PAxis' ['AxValues' ('DateTimes' [ ['Graphics.Vega.VegaLite.DTYear' 2019, 'Graphics.Vega.VegaLite.DTMonth' 'Graphics.Vega.VegaLite.Mar', 'Graphics.Vega.VegaLite.DTDate' 31]
      --                              , ['Graphics.Vega.VegaLite.DTYear' 2019, 'Graphics.Vega.VegaLite.DTMonth' 'Graphics.Vega.VegaLite.Jun', 'Graphics.Vega.VegaLite.DTDate' 30]
      --                              , ['Graphics.Vega.VegaLite.DTYear' 2019, 'Graphics.Vega.VegaLite.DTMonth' 'Graphics.Vega.VegaLite.Sep', 'Graphics.Vega.VegaLite.DTDate' 30]
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
axisProperty AxLabelNoBound = "labelBound" .= False
axisProperty AxLabelBound = "labelBound" .= True
axisProperty (AxLabelBoundValue x) = "labelBound" .= x
axisProperty (AxLabelColor s) = "labelColor" .= s
axisProperty AxLabelNoFlush = "labelFlush" .= False
axisProperty AxLabelFlush = "labelFlush" .= True
axisProperty (AxLabelFlushValue x) = "labelFlush" .= x
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
    --   'Boxplot'. For example @IqrScale 1.5@  would extend whiskers to
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


-- | Identifies the type of symbol used with the 'Point' mark type.
--   It is used with 'MShape', 'LeSymbolType', and 'LSymbolType'.
--
--   In version @0.4.0.0@ all constructors were changed to start
--   with @Sym@.
--
data Symbol
    = SymCircle
      -- ^ Specify a circular symbol for a shape mark.
    | SymSquare
      -- ^ Specify a square symbol for a shape mark.
    | SymCross
      -- ^ Specify a cross symbol for a shape mark.
    | SymDiamond
      -- ^ Specify a diamond symbol for a shape mark.
    | SymTriangleUp
      -- ^ Specify an upward-triangular symbol for a shape mark.
    | SymTriangleDown
      -- ^ Specify a downward-triangular symbol for a shape mark.
    | SymTriangleRight
      -- ^ Specify an right-facing triangular symbol for a shape mark.
      --
      --   @since 0.4.0.0
    | SymTriangleLeft
      -- ^ Specify an left-facing triangular symbol for a shape mark.
      --
      --   @since 0.4.0.0
    | SymStroke
      -- ^ The line symbol.
      --
      --  @since 0.4.0.0
    | SymArrow
      -- ^ Centered directional shape.
      --
      --  @since 0.4.0.0
    | SymTriangle
      -- ^ Centered directional shape. It is not clear what difference
      --   this is to 'SymTriangleUp'.
      --
      --  @since 0.4.0.0
    | SymWedge
      -- ^ Centered directional shape.
      --
      --  @since 0.4.0.0
    | SymPath T.Text
      -- ^ A custom symbol shape as an
      --   [SVG path description](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths).
      --
      --   For correct sizing, the path should be defined within a square
      --   bounding box, defined on an axis of -1 to 1 for both dimensions.


symbolLabel :: Symbol -> T.Text
symbolLabel SymCircle = "circle"
symbolLabel SymSquare = "square"
symbolLabel SymCross = "cross"
symbolLabel SymDiamond = "diamond"
symbolLabel SymTriangleUp = "triangle-up"
symbolLabel SymTriangleDown = "triangle-down"
symbolLabel SymTriangleRight = "triangle-right"
symbolLabel SymTriangleLeft = "triangle-left"
symbolLabel SymStroke = "stroke"
symbolLabel SymArrow = "arrow"
symbolLabel SymTriangle = "triangle"
symbolLabel SymWedge = "wedge"
symbolLabel (SymPath svgPath) = svgPath


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
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'width' 250
    , 'height' 300
    , 'autosize' [ 'AFit', 'APadding', 'AResize' ]
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
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


-- | Indicates the type of legend to create. It is used with 'LType'.
--
--   Prior to version @0.4.0.0.0@ this was called @Legend@ and the
--   constructors did not end in @Legend@.
--
data LegendType
    = GradientLegend
      -- ^ Typically used for continuous quantitative data.
    | SymbolLegend
      -- ^ Typically used for categorical data.


legendLabel :: LegendType -> T.Text
legendLabel GradientLegend = "gradient"
legendLabel SymbolLegend = "symbol"


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
    -- ^ The bounds calculation to use for legend orient group layout.
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


{-|

Legend properties, set with 'MLegend'. For more detail see the
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
    | LType LegendType
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


-- | A list of data values suitable for setting legend values, used with
--   'LValues'.


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
      --   via 'Graphics.Vega.VegaLite.NamedStyle' or 'Graphics.Vega.VegaLite.NamedStyles'. Later styles in the list will
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
    --   via 'Graphics.Vega.VegaLite.NamedStyle' or 'Graphics.Vega.VegaLite.NamedStyles'. Later styles in the list will
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
    | LabelNoBound
      -- ^ No boundary overlap check is applied to labels. This is the
      --   default behavior.
      --
      --   See also 'LabelBound' and 'LabelBoundValue'.
      --
      --   @since 0.4.0.0
    | LabelBound
      -- ^ Labels are hidden if they exceed the axis range by more than 1
      --   pixel.
      --
      --   See also 'LabelNoBound' and 'LabelBoundValue'.
      --
      --   @since 0.4.0.0
    | LabelBoundValue Double
      -- ^ Labels are hidden if they exceed the axis range by more than
      --   the given number of pixels.
      --
      --   See also 'LabelNoBound' and 'LabelBound'.
      --
      --   @since 0.4.0.0
    | LabelColor Color
      -- ^ The label color.
    | LabelNoFlush
      -- ^ The labels are not aligned flush to the scale. This is the
      --   default for non-continuous X scales.
      --
      --   See also 'LabelFlush' and 'LabelFlushValue'.
      --
      --   @since 0.4.0.0
    | LabelFlush
      -- ^ The first and last axis labels are aligned flush to the scale
      --   range.
      --
      --   See also 'LabelNoFlush' and 'LabelFlushValue'.
      --
      --   @since 0.4.0.0
    | LabelFlushValue Double
      -- ^ The labels are aligned flush, and the parameter determines
      --   the extra offset, in pixels, to apply to the first and last
      --   labels. This can help the labels better group (visually) with
      --   the corresponding axis ticks.
      --
      --   See also 'LabelNoFlush' and 'LabelFlush'.
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
      --   'filter' ('FRange' "date" ('DateRange' ['Graphics.Vega.VegaLite.DTYear' 2010] ['Graphics.Vega.VegaLite.DTYear' 2017])
      --           & 'FilterOpTrans' ('MTimeUnit' 'Graphics.Vega.VegaLite.Year')
      --           & 'FCompose'
      --           )
      --   @
      --
      --   @since 0.4.0.0
    | Selection T.Text  -- TODO: rename Selected since collides with Selection type
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
      --   sel = 'Graphics.Vega.VegaLite.selection' . 'Graphics.Vega.VegaLite.select' \"myBrush\" 'Graphics.Vega.VegaLite.Interval' ['Graphics.Vega.VegaLite.Encodings' ['ChX']]
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
      --   'filter' ('FRange' "date" ('DateRange' ['Graphics.Vega.VegaLite.DTYear' 2006] ['Graphics.Vega.VegaLite.DTYear' 2016])
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

      ans = map (\(ch, rule) -> channelLabel ch .= resolutionLabel rule) rls
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

Configuration options for faceted views, used with 'Graphics.Vega.VegaLite.FacetStyle'.

See the
<https://vega.github.io/vega-lite/docs/facet.html#facet-configuration Vega-Lite facet config documentation>.

@since 0.4.0.0

-}
data FacetConfig
    = FColumns Int
    -- ^ The maximum number of columns to use in a faceted-flow layout.
    | FSpacing Double
    -- ^ The spacing in pixels between sub-views in a faceted composition.



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
        . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.Axis' [ 'DomainWidth' 1 ])
        . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.View' [ 'ViewStroke' (Just "transparent") ])
        . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.SelectionStyle' [ ( 'Graphics.Vega.VegaLite.Single', [ 'Graphics.Vega.VegaLite.On' \"dblclick\" ] ) ])
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
let dvals = 'Graphics.Vega.VegaLite.dataSequenceAs' 0 6.28 0.1 \"x\"
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
'Graphics.Vega.VegaLite.toVegaLite'
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
'Graphics.Vega.VegaLite.toVegaLite'
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
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'height' 300
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
    , 'mark' 'Bar' []
    , enc []
    ]
@
-}
height :: Double -> PropertySpec
height h = (VLHeight, toJSON h)


{-|

Configuration options for concatenated views, used with 'Graphics.Vega.VegaLite.ConcatStyle'.

@since 0.4.0.0

-}
data ConcatConfig
    = ConcatColumns Int
      -- ^ The maximum number of columns to use in a concatenated flow layout.
    | ConcatSpacing Double
      -- ^ The spacing in pixels between sub-views in a concatenated view.


{-|

Assigns a list of specifications to be juxtaposed horizontally in a visualization.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'Graphics.Vega.VegaLite.dataFromUrl' "data/driving.json" []
    , hConcat [ spec1, spec2 ]
    ]
@
-}
hConcat :: [VLSpec] -> PropertySpec
hConcat specs = (VLHConcat, toJSON specs)


{-|

Assigns a list of specifications to superposed layers in a visualization.

@
'Graphics.Vega.VegaLite.toVegaLite' ['Graphics.Vega.VegaLite.dataFromUrl' "data/driving.json" [], layer [spec1, spec2]]
@

A complete example showing @layer@ in use:

@
let dvals = 'Graphics.Vega.VegaLite.dataFromColumns' []
              . 'Graphics.Vega.VegaLite.dataColumn' \"x\" ('Numbers' [1, 2, 3, 4, 5])
              . 'Graphics.Vega.VegaLite.dataColumn' \"a\" ('Numbers' [28, 91, 43, 55, 81])
    enc = 'encoding'
             . 'position' 'X' ['PName' \"x\", 'PmType' 'Ordinal']
             . 'position' 'Y' ['PName' \"a\", 'PmType' 'Quantitative']
             . 'text' ['TName' \"a\", 'TmType' 'Nominal']

    in 'Graphics.Vega.VegaLite.toVegaLite' [ dvals []
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
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'name' \"PopGrowth\"
    , 'Graphics.Vega.VegaLite.dataFromUrl' \"data/population.json\" []
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
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'width' 500
    , 'padding' ('PEdges' 20 10 5 15)
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
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
'Graphics.Vega.VegaLite.toVegaLite'
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
'Graphics.Vega.VegaLite.toVegaLite'
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

in 'Graphics.Vega.VegaLite.toVegaLite'
    [ 'Graphics.Vega.VegaLite.dataFromUrl' \"data/movies.json\" []
    , 'vConcat' [heatSpec, barSpec]
    , res []
    ]
@

For more information see the
<https://vega.github.io/vega-lite/docs/resolve.html Vega-Lite documentation>.

@
let dvals = 'Graphics.Vega.VegaLite.dataFromColumns' []
              . 'Graphics.Vega.VegaLite.dataColumn' "x" ('Numbers' [1, 2, 3, 4, 5])
              . 'Graphics.Vega.VegaLite.dataColumn' "a" ('Numbers' [28, 91, 43, 55, 81])
              . 'Graphics.Vega.VegaLite.dataColumn' "b" ('Numbers' [17, 22, 28, 30, 40])
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

in 'Graphics.Vega.VegaLite.toVegaLite' [dvals [], res [], 'layer' [specBar, specLine]]
@

-}
resolve :: [LabelledSpec] -> PropertySpec
resolve res = (VLResolve, object res)


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
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'Graphics.Vega.VegaLite.dataFromUrl' "data/driving.json" []
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
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'width' 500
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
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
<https://vega.github.io/vega-lite/docs/joinaggregate.html Vega-Lite join aggregate documentation>.

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
 dvals = 'Graphics.Vega.VegaLite.dataSequenceAs' 0 13 0.001 \"x\"
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
let dvals = 'Graphics.Vega.VegaLite.dataFromUrl' \"crimeData.csv\"
    enc = 'encoding'
            . 'position' 'X' ['PName' \"month\", 'PmType' 'Temporal']
            . 'position' 'Y' ['PName' \"reportedCrimes\", 'PmType' 'Quantitative'
                         , 'PAggregate' 'Sum']
            . 'column' ['FName' \"crimeType\", 'FmType' 'Nominal']

    in 'Graphics.Vega.VegaLite.toVegaLite' ['width' 100, dvals [], 'mark' 'Bar' [], enc [] ]
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
    'Graphics.Vega.VegaLite.dataFromColumns' []
        . 'Graphics.Vega.VegaLite.dataColumn' \"city\" ('Strings' [ \"Bristol\", \"Sheffield\", \"Glasgow\" ])
        . 'Graphics.Vega.VegaLite.dataColumn' \"temp2017\" ('Numbers' [ 12, 11, 7 ])
        . 'Graphics.Vega.VegaLite.dataColumn' \"temp2018\" ('Numbers' [ 14, 13, 10 ])

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
          . 'lookup' \"person\" ('Graphics.Vega.VegaLite.dataFromUrl' \"data/lookup_people.csv\" []) \"name\" [\"age\", \"height\"]
@
-}
lookup ::
  T.Text
  -- ^ The field in the primary data structure acting as the key.
  -> Data
  -- ^ The secondary data source (e.g. the return from the data-generating
  --   functions such as 'Graphics.Vega.VegaLite.dataFromUrl').
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
    . 'lookupAs' "person" ('Graphics.Vega.VegaLite.dataFromUrl' "data/lookup_people.csv" []) "name" "personDetails"
@

If the data contained columns called @age@ and @height@ then they would
then be accessed as @personDetails.age@ and @personDetails.height@ - for
example:

@
'encoding'
  . 'position' X ['PName' "personDetails.age", 'PmType' 'Temporal', 'PTimeUnit' 'Graphics.Vega.VegaLite.Year', 'PTitle' \"Age\"]
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
  --   functions such as 'Graphics.Vega.VegaLite.dataFromUrl').
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
let dvals = 'Graphics.Vega.VegaLite.dataFromColumns' []
              . 'Graphics.Vega.VegaLite.dataColumn' "a" ('Numbers' [0, 0, 10, 10, 20, 20, 30])
              . 'Graphics.Vega.VegaLite.dataColumn' "b" ('Numbers' [28, 91, 43, 55, 81, 53, 19])
              . 'Graphics.Vega.VegaLite.dataColumn' "c" ('Numbers' [0, 1, 0, 1, 0, 1, 0])

    trans = 'transform'
              . 'impute' "b" "a" ['ImMethod' 'ImMean', 'ImGroupBy' ["c"]]

    enc = 'encoding'
            . 'position' 'X' ['PName' \"a\", 'PmType' 'Quantitative']
            . 'position' 'Y' ['PName' \"b\", 'PmType' 'Quantitative']
            . 'color' ['MName' \"c\", 'MmType' 'Nominal']

    in 'Graphics.Vega.VegaLite.toVegaLite' [dvals [], trans [], enc [], 'mark' 'Line' []]
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
let dvals = 'Graphics.Vega.VegaLite.dataFromUrl' \"crimeData.csv\"
    enc = 'encoding'
            . 'position' 'X' ['PName' \"month\", 'PmType' 'Temporal']
            . 'position' 'Y' ['PName' \"reportedCrimes\"
                         , 'PmType' 'Quantitative'
                         , 'PAggregate' 'Sum'
                         , 'PAxis' ['AxNoTitle']
                         ]
            . 'row' ['FName' \"crimeType\", 'FmType' 'Nominal']

in 'Graphics.Vega.VegaLite.toVegaLite' ['height' 80, dvals [], 'mark' 'Bar' [], enc []]
@

-}
row ::
  [FacetChannel]
  -- ^ The facet properties for the channel; this should include the name of
  --   the field ('FName') and its measurement type ('FmType').
  -> BuildLabelledSpecs
row fFields ols = ("row" .= object (map facetChannelProperty fFields)) : ols


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
trans = 'transform' . 'timeUnitAs' 'Graphics.Vega.VegaLite.Month' \"date\" \"monthly\"

enc = 'encoding'
        . 'position' 'X' [ 'PName' \"date\", 'PmType' 'Temporal', 'PTimeUnit' 'Graphics.Vega.VegaLite.Day' ]
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
  ("tooltip" .= toJSON (map (object . concatMap textChannelProperty) tDefs)) : ols


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
