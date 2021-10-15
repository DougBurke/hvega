{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-|
Module      : Graphics.Vega.VegaLite.Core
Copyright   : (c) Douglas Burke, 2018-2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

The main fuctionality of VegaLite is provided by
the Foundation and Core modules, but there are types
(and functions) scattered around the place. There is
some logic into what goes where, but it's not perfect.

-}

module Graphics.Vega.VegaLite.Core
       ( transform

       , aggregate
       , joinAggregate
       , opAs
       , timeUnitAs

       , binAs

       , stack

       , calculateAs

       , filter
       , Filter(..)
       , FilterRange(..)

       , flatten
       , flattenAs
       , fold
       , foldAs
       , pivot
       , PivotProperty(..)

       , lookup
       , lookupSelection
       , LookupFields(..)
       , lookupAs

       , impute

       , sample

       , density
       , DensityProperty(..)

       , loess
       , LoessProperty(..)

       , regression
       , RegressionProperty(..)
       , RegressionMethod(..)

       , quantile
       , QuantileProperty(..)

       , window

       , mark

       , encoding

       , position

       , PositionChannel(..)

       , SortProperty(..)

       , AxisProperty(..)
       , ConditionalAxisProperty(..)

       , angle
       , color
       , fill
       , fillOpacity
       , opacity
       , shape
       , size
       , stroke
       , strokeDash
       , strokeOpacity
       , strokeWidth

       , MarkChannel(..)

       , text
       , tooltip
       , tooltips
       , TextChannel(..)

       , hyperlink
       , url
       , HyperlinkChannel(..)

       , order
       , OrderChannel(..)

       , row
       , column

       , detail
       , DetailChannel(..)

       , ariaDescription
       , AriaDescriptionChannel(..)

       , ScaleProperty(..)
       , categoricalDomainMap
       , domainRangeMap

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

       , resolve
       , resolution

       , repeat
       , repeatFlow
       , facet
       , facetFlow
       , FacetMapping(..)
       , FacetChannel(..)

       , BooleanOp(..)

       , name
       , description
       , height
       , heightOfContainer
       , heightStep
       , width
       , widthOfContainer
       , widthStep
       , padding
       , autosize
       , background
       , usermetadata

       , viewBackground

       , configure

       -- not for external export
       , autosizeProperty
       , axisProperty
       , paddingSpec
       , schemeProperty

       )
    where

-- VegaLite uses these symbols.
import Prelude hiding (filter, lookup, repeat)

import qualified Data.Aeson as A

#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.Key as Key
#endif

import qualified Data.Text as T

import Data.Aeson (object, toJSON, (.=))
import Data.Aeson.Types (Pair)
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
import Graphics.Vega.VegaLite.Foundation
  ( Angle
  , Color
  , DashStyle
  , DashOffset
  , FieldName
  , Opacity
  , StyleLabel
  , VegaExpr
  , ZIndex
  , FontWeight
  , Measurement
  , Arrangement
  , APosition
  , Position
  , HAlign
  , VAlign
  , BandAlign
  , Scale
  , OverlapStrategy
  , Side
  , StackProperty
  , StackOffset
  , StrokeCap
  , Channel
  , Resolve
  , Bounds
  , CompositionAlignment
  , Padding
  , Autosize
  , RepeatFields
  , CInterpolate
  , ViewBackground
  , HeaderProperty
  , Symbol
  , fromT
  , fromColor
  , fromDS
  , splitOnNewline
  , field_
  , header_
  , order_
  , fontWeightSpec
  , measurementLabel
  , arrangementLabel
  , anchorLabel
  , hAlignLabel
  , vAlignLabel
  , bandAlignLabel
  , scaleLabel
  , strokeCapLabel
  , positionLabel
  , overlapStrategyLabel
  , sideLabel
  , stackPropertySpecSort
  , stackPropertySpecOffset
  , stackOffset
  , channelLabel
  , resolveProperty
  , boundsSpec
  , compositionAlignmentSpec
  , paddingSpec
  , autosizeProperty
  , repeatFieldsProperty
  , cInterpolateSpec
  , viewBackgroundSpec
  , symbolLabel
  , (.=~), toObject, toKey
  )
import Graphics.Vega.VegaLite.Input
  ( Data
  )
import Graphics.Vega.VegaLite.Legend
  ( LegendProperty
  , legendProp_
  )
import Graphics.Vega.VegaLite.Mark
  ( Mark
  , MarkProperty
  , markLabel
  , markProperty
  )
import Graphics.Vega.VegaLite.Scale
  ( ScaleDomain(..)
  , DomainLimits(..)
  , ScaleRange(..)
  , ScaleNice
  , scaleDomainProperty
  , domainLimitsSpec
  , scaleNiceSpec
  )
import Graphics.Vega.VegaLite.Specification
  ( VLProperty(..)
  , VLSpec
  , PropertySpec
  , EncodingSpec(..)
  , BuildEncodingSpecs
  , TransformSpec(..)
  , BuildTransformSpecs
  , ConfigureSpec(..)
  , ResolveSpec(..)
  , BuildResolveSpecs
  , SelectionLabel
  )
import Graphics.Vega.VegaLite.Time
  ( DateTime
  , TimeUnit
  , dateTimeSpec
  , timeUnitSpec
  )
import Graphics.Vega.VegaLite.Transform
  ( Operation(Count)
  , Window
  , BinProperty
  , WindowProperty
  , ImputeProperty
  , aggregate_
  , op_
  , binned_
  , impute_
  , bin
  , binProperty
  , operationSpec
  , windowTS
  , joinAggregateTS
  , imputeTS
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


repeat_ :: Arrangement -> Pair
repeat_ arr = "repeat" .= arrangementLabel arr

sort_ :: [SortProperty] -> Pair
sort_ ops = "sort" .= sortPropertySpec ops

mchan_ :: T.Text -> [MarkChannel] -> EncodingSpec
mchan_ f ms = ES (f .=~ object (concatMap markChannelProperty ms))

mtype_ :: Measurement -> Pair
mtype_ m = "type" .= measurementLabel m

timeUnit_ :: TimeUnit -> Pair
timeUnit_ tu = "timeUnit" .= timeUnitSpec tu

-- The assumption at the moment is that it's always correct to
-- replace the empty list by null.
--
scaleProp_ :: [ScaleProperty] -> Pair
scaleProp_ [] = "scale" .= A.Null
scaleProp_ sps = "scale" .= object (map scaleProperty sps)


value_ :: T.Text -> Pair
value_ v = "value" .= v


selCond_ :: (a -> [Pair]) -> BooleanOp -> [a] -> [a] -> [Pair]
selCond_ getProps selName ifClause elseClause =
  let h = ("condition", hkey)
      toProps = concatMap getProps
      hkey = object (toKey ("selection", booleanOpSpec selName) : toProps ifClause)
      hs = toProps elseClause
  in (h : hs)

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
dataCond_ :: (a -> [Pair]) -> [(BooleanOp, [a])] -> [a] -> [Pair]
dataCond_ getProps tests elseClause =
  let h = ("condition", condClause)
      condClause = case conds of
                     [cond] -> cond
                     _ -> toJSON conds
      conds = map testClause tests
      testClause (Selection sel, ifClause) =
        object (("selection" .= sel) : toProps ifClause)
      testClause (predicate, ifClause) =
        object (toKey ("test", booleanOpSpec predicate) : toProps ifClause)
      toProps = concatMap getProps
      hs = toProps elseClause
  in (h : hs)



{-|

Create a named aggregation operation on a field that can be added to a transformation.
For further details see the
<https://vega.github.io/vega-lite/docs/aggregate.html#aggregate-op-def Vega-Lite documentation>.

@
'transform'
    . 'aggregate'
        [ 'opAs' 'Graphics.Vega.VegaLite.Min' "people" "lowerBound"
        , 'opAs' 'Graphics.Vega.VegaLite.Max' "people" "upperBound"
        ]
        [ "age" ]
@
-}
opAs ::
  Operation
  -- ^ The aggregation operation to use.
  -> FieldName
  -- ^ The name of the field which is to be aggregated (when the operation
  --   is 'Count' leave as the empty string).
  -> FieldName
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

Create a mark specification. All marks must have a type (first parameter) and
can optionally be customised with a list of mark properties such as interpolation
style for lines. To keep the default style for the mark, just provide an empty list
for the second parameter.

@
'mark' 'Graphics.Vega.VegaLite.Circle' []
'mark' 'Graphics.Vega.VegaLite.Line' ['Graphics.Vega.VegaLite.MInterpolate' 'Graphics.Vega.VegaLite.StepAfter']
@

@
let dvals = 'Graphics.Vega.VegaLite.dataFromUrl' \"city.json\" ['Graphics.Vega.VegaLite.TopojsonFeature' \"boroughs\"] []
    markOpts = 'mark' 'Graphics.Vega.VegaLite.Geoshape' ['Graphics.Vega.VegaLite.MFill' \"lightgrey\", 'Graphics.Vega.VegaLite.MStroke' \"white\"]
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
    = MName FieldName
      -- ^ Field used for encoding with a mark property channel.
    | MRepeat Arrangement
      -- ^ Reference in a mark channel to a field name generated by 'repeatFlow'
      --   or 'repeat'. The parameter identifies whether reference is being made to
      --   fields that are to be arranged in columns, in rows, or a with a flow layout.
    | MRepeatDatum Arrangement
      -- ^ Reference in a mark channel to a datum value generated by 'repeatFlow'
      --   or 'repeat'. The parameter identifies whether reference is being made to
      --   a datum that is to be encoded in layers, or in columns or rows in a
      --   flow layout.
      --
      --   @since 0.9.0.0
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
      --      [ 'MName' \"myField\", 'MmType' 'Graphics.Vega.VegaLite.Ordinal' ]
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
    | MDatum DataValue
      -- ^ Name of a literal data item used for encoding with a mark property channel.
      --   Unlike 'MNumber', 'MString', and 'MBoolean', datum literals represent values in
      --   data space.
      --
      --   @since 0.9.0.0
    | MNumber Double
      -- ^ Literal numeric value when encoding with a mark property channel.
    | MString T.Text
      -- ^ Literal string value when encoding with a mark property channel.
    | MBoolean Bool
      -- ^ Boolean value when encoding with a mark property channel.
    | MNullValue
      -- ^ A null value.
      --
      --   @since 0.11.0.0
    | MSymbol Symbol
      -- ^ A symbol literal. This can be useful when making a symbol dependent on some data or
      --   selection condition (e.g. 'MDataCondition' or 'MSelectionCondition').
      --
      --   For example:
      --
      --   @
      --   'encoding'
      --     . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' "to", 'PmType' 'Graphics.Vega.VegaLite.Quantitative', 'PAxis' [] ]
      --     . 'shape' ['MDataCondition'
      --               [('Expr' "datum.to > 100", [MSymbol 'Graphics.Vega.VegaLite.SymTriangleRight'])]
      --               [MSymbol 'Graphics.Vega.VegaLite.SymTriangleLeft']
      --   @
      --
      --   @since 0.6.0.0

markChannelProperty :: MarkChannel -> [Pair]
markChannelProperty (MName s) = ["field" .= s]
markChannelProperty (MRepeat arr) = ["field" .= object [repeat_ arr]]
markChannelProperty (MRepeatDatum arr) = ["datum" .= object [repeat_ arr]]
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
markChannelProperty (MDatum d) = ["datum" .= dataValueSpec d]
markChannelProperty (MNumber x) = ["value" .= x]
markChannelProperty (MString s) = ["value" .= s]
markChannelProperty (MBoolean b) = ["value" .= b]
markChannelProperty (MSymbol s) = ["value" .= symbolLabel s]
markChannelProperty MNullValue = ["value" .= A.Null]
markChannelProperty (MTitle s) = ["title" .= splitOnNewline s]
markChannelProperty MNoTitle = ["title" .= A.Null]


{-|

Create an encoding specification from a list of channel encodings.

@
enc = 'encoding'
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"Animal\", 'PmType' 'Graphics.Vega.VegaLite.Ordinal' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"Age\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'shape' [ 'MName' \"Species\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ]
        . 'size' [ 'MName' \"Population\", 'MmType' 'Graphics.Vega.VegaLite.Quantitative' ]
@

The type of @enc@ in this example is @[EncodingSpec] -> PropertySpec@,
so it can either be used to add further encoding specifications or as
@enc []@ to create a specification.

The supported encodings are:
'ariaDescription', 'angle', 'color', 'column', 'detail', 'fill', 'fillOpacity',
'hyperlink', 'opacity', 'order', 'position', 'row', 'shape', 'size',
'stroke', 'strokeDash', 'strokeOpacity', 'strokeWidth', 'text', 'tooltip',
'tooltips', and 'url'.

There is currently no support for encoding by
<https://vega.github.io/vega-lite/docs/encoding.html#key key>.

-}
encoding ::
  [EncodingSpec]
  -- ^ The channel encodings (the order does not matter).
  --
  --   Prior to @0.5.0.0@ this argument was @['LabelledSpec']@.
  -> PropertySpec
encoding channels = (VLEncoding, toObject (map unES channels))


{-|

Encode an Aria description.

@since 0.9.0.0
-}
ariaDescription ::
  [AriaDescriptionChannel]
  -- ^ The properties for the channel.
  -> BuildEncodingSpecs
ariaDescription ads ols =
  ES ("description", object (concatMap ariaDescriptionChannelProperty ads)) : ols


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
        [ 'Graphics.Vega.VegaLite.StOffset' 'Graphics.Vega.VegaLite.StNormalize', 'Graphics.Vega.VegaLite.StSort' [ 'Graphics.Vega.VegaLite.WAscending' \"Origin\" ] ]
    . 'window'
        [ ( [ 'Graphics.Vega.VegaLite.WAggregateOp' 'Graphics.Vega.VegaLite.Min', 'Graphics.Vega.VegaLite.WField' \"stack_count_Origin1\" ], \"x\" )
        , ( [ 'Graphics.Vega.VegaLite.WAggregateOp' 'Graphics.Vega.VegaLite.Max', 'Graphics.Vega.VegaLite.WField' \"stack_count_Origin2\" ], \"x2\" )
        ]
        [ 'Graphics.Vega.VegaLite.WFrame' Nothing Nothing, 'Graphics.Vega.VegaLite.WGroupBy' [ \"Origin\" ] ]
    . 'stack' \"count_*\"
        [ \"Origin\" ]
        \"y\"
        \"y2\"
        [ 'Graphics.Vega.VegaLite.StOffset' 'Graphics.Vega.VegaLite.StNormalize', 'Graphics.Vega.VegaLite.StSort' [ 'Graphics.Vega.VegaLite.WAscending' \"Cylinders\" ] ]
@

@since 0.4.0.0

-}

stack ::
  FieldName
  -- ^ The field to be stacked.
  -> [FieldName]
  -- ^ The fields to group by.
  -> FieldName
  -- ^ The output field name (start).
  -> FieldName
  -- ^ The output field name (end).
  -> [StackProperty]
  -- ^ Offset and sort properties.
  -> BuildTransformSpecs
stack f grp start end sProps ols =
  let addField n [x] = [n .= x]
      addField _ _ = []

      mOffset = mapMaybe stackPropertySpecOffset sProps
      mSort = mapMaybe stackPropertySpecSort sProps

      fields = [ "stack" .= f
               , "groupby" .= grp
               , "as" .= [ start, end ] ]
               <> addField "offset" mOffset
               <> addField "sort" mSort

  in TS (object fields) : ols


{-|

Individual scale property. These are used to customise an individual scale
transformation. To customise all scales use 'configure' and supply relevant
'Graphics.Vega.VegaLite.ScaleConfig' values. For more details see the
<https://vega.github.io/vega-lite/docs/scale.html Vega-Lite documentation>.

There are two utility routines for constructing a list of scale
properties: 'categoricalDomainMap' and 'domainRangeMap'.

The @SRangeStep@ constructor was removed in version @0.5.0.0@. Users
should use the 'heightStep' and 'widthStep' functions instead.

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
      -- ^ The base to use for log scaling ('Graphics.Vega.VegaLite.ScLog').
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
      -- ^ The desired slope of the 'Graphics.Vega.VegaLite.ScSymLog' function at zero.
      --
      --   The default is @1@.
      --
      --   @since 0.4.0.0
    | SDomain DomainLimits
      -- ^ Custom scaling domain. See also 'SDomainOpt'.
      --
      --   In verson @0.11.0.0@ some functionality was moved to 'SDomainOpt'.
    | SDomainMid Double
      -- ^ Set the mid-point of a continuous diverging domain.
      --
      --   This is deprecated as of 0.11.0.0 and @'SDomainOpt' ('DMid' x)@ should be used
      --   instead.
      --
      --   @since 0.6.0.0
    | SDomainOpt ScaleDomain
      -- ^ Custom scaling domain. See also 'SDomain'.
      --
      --   @since 0.11.0.0
    | SExponent Double
      -- ^ The exponent to use for power scaling ('Graphics.Vega.VegaLite.ScPow').
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
    | SReverse Bool
      -- ^ Should the order of the scale range be reversed?
      --
      --   @since 0.6.0.0
    | SRound Bool
      -- ^ Are numeric values in a scaling rounded to integers?
      --
      --   The default is @False@.
    | SScheme T.Text [Double]   -- TODO: review this; what is #/definitions/SchemeParams?
      -- ^  Color scheme used by a color scaling. The first parameter is the
      --    name of the scheme (e.g. \"viridis\") and the second an optional
      --    specification, which can contain 1, 2, or 3 numbers:
      --
      --      - the number of colors to use (list of one number);
      --      - the extent of the color range to use (list of two numbers between 0 and 1);
      --      - the number of colors and extent (three numbers, first is the number of colors).
      --
      --    For the full list of supported schemes, please refer to the
      --    <https://vega.github.io/vega/docs/schemes/#reference Vega Scheme>
      --    reference.
      --
      --    The number of colors was broken prior to @0.4.0.0@ and the option to
      --    define both the count and extent was added in @0.4.0.0@.
    | SZero Bool
      -- ^ Should a numeric scaling be forced to include a zero value?
      --
      --   Not all scales support @SZero@ and the default depends on the type of
      --   channel.


scaleProperty :: ScaleProperty -> Pair
scaleProperty (SType sType) = "type" .= scaleLabel sType
scaleProperty (SAlign c) = "align" .= clamped 0 1 c
scaleProperty (SBase x) = "base" .= x
scaleProperty (SBins xs) = "bins" .= xs
scaleProperty (SClamp b) = "clamp" .= b
scaleProperty (SConstant x) = "constant" .= x
scaleProperty (SDomain dl) = "domain" .= domainLimitsSpec dl
scaleProperty (SDomainMid x) = "domainMid" .= x
scaleProperty (SDomainOpt sd) = scaleDomainProperty sd
scaleProperty (SExponent x) = "exponent" .= x
scaleProperty (SInterpolate interp) = "interpolate" .= cInterpolateSpec interp
scaleProperty (SNice ni) = "nice" .= scaleNiceSpec ni
scaleProperty (SPadding x) = "padding" .= x
scaleProperty (SPaddingInner x) = "paddingInner" .= x
scaleProperty (SPaddingOuter x) = "paddingOuter" .= x
scaleProperty (SRange (RField f)) = "range" .= object ["field" .= f]
scaleProperty (SRange (RMax x)) = "rangeMax" .= x
scaleProperty (SRange (RMin x)) = "rangeMin" .= x
scaleProperty (SRange (RPair lo hi)) = "range" .= [lo, hi]
scaleProperty (SRange (RHeight w)) = "range" .= [fromT "height", toJSON w]
scaleProperty (SRange (RWidth h)) = "range" .= [toJSON h, fromT "width"]
scaleProperty (SRange (RNumbers xs)) = "range" .= xs
scaleProperty (SRange (RNumberLists xss)) = "range" .= xss
scaleProperty (SRange (RStrings ss)) = "range" .= ss
scaleProperty (SRange (RName s)) = "range" .= s
scaleProperty (SReverse b) = "reverse" .= b
scaleProperty (SRound b) = "round" .= b
scaleProperty (SScheme nme extent) = schemeProperty nme extent
scaleProperty (SZero b) = "zero" .= b


-- TODO: there should probably be a more-structured way to specify this
--
-- based on schema 3.3.0 #/definitions/SchemeParams

schemeProperty :: T.Text -> [Double] -> Pair
schemeProperty nme [n] = "scheme" .= object ["name" .= nme, "count" .= n]
schemeProperty nme [mn, mx] = "scheme" .= object ["name" .= nme, "extent" .= [mn, mx]]
schemeProperty nme [n, mn, mx] = "scheme" .= object ["name" .= nme, "count" .= n, "extent" .= [mn, mx]]
schemeProperty nme _ = "scheme" .= nme


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
    | ByFieldOp FieldName Operation
      -- ^ Sort by the aggregated summary of a field using an aggregation
      --   operation. The following example sorts the categorical data field
      --   @variety@ by the mean age of the data in each variety category:
      --
      -- @
      -- 'position' 'Graphics.Vega.VegaLite.Y'
      --   [ 'PName' "variety"
      --   , 'PmType' 'Graphics.Vega.VegaLite.Ordinal'
      --   , 'PSort' [ ByFieldOp "age" 'Graphics.Vega.VegaLite.Mean', 'Descending' ]
      --   ]
      -- @
      --
      --   @since 0.4.0.0
    | ByChannel Channel
      -- ^ Sort by another channel.
      --
      -- @
      -- 'position' 'Graphics.Vega.VegaLite.Y'
      --  [ 'PName' "age"
      --  , 'PmType' 'Graphics.Vega.VegaLite.Ordinal'
      --  , 'PSort' [ ByChannel 'Graphics.Vega.VegaLite.ChX' ]
      --  ]
      -- @
      --
      --   @since 0.4.0.0


sortProperty :: SortProperty -> [Pair]
sortProperty Ascending = [order_ "ascending"]
sortProperty Descending = [order_ "descending"]
sortProperty (ByChannel ch) = ["encoding" .= channelLabel ch]
sortProperty (ByFieldOp field op) = ["field" .= field, op_ op]
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
    = PName FieldName
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
      --      . 'position' 'Graphics.Vega.VegaLite.X' [ PWidth ]
      -- @
      --
      --   @since 0.4.0.0
    | PDatum DataValue
      -- ^ Set a position to an arbitrary data value. Useful for placing items at a
      --   specific point in the data space. To place in data screen space use
      --   'PNumber'.
      --
      --   @since 0.9.0.0
    | PNumber Double
      -- ^ Set a position to an arbitrary value. Useful for placing items at the top of
      --   a plot area (@PNumber 0@) or a fixed number of pixels from the top.
      --   See also 'PHeight' and 'PWidth'.
      --
      --   Use 'PDatum' to place an item using a data coordinate.
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
      --      . 'position' 'Graphics.Vega.VegaLite.X' [ PRepeat 'Graphics.Vega.VegaLite.Flow', 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
      --
      -- spec =
      --    'Graphics.Vega.VegaLite.asSpec' [ dataVals [], 'mark' 'Graphics.Vega.VegaLite.Tick' [], enc [] ]
      --
      -- 'Graphics.Vega.VegaLite.toVegaLite'
      --    [ 'repeatFlow' [ \"Horsepower\", \"Miles_per_Gallon\", \"Acceleration\"]
      --    , 'Graphics.Vega.VegaLite.specification' spec
      --    ]
      -- @
    | PRepeatDatum Arrangement
      -- ^ Reference in a position channel to a datum value generated by 'repeatFlow'
      --   or 'repeat'. The parameter identifies whether reference is being made to
      --   a datum that is to be encoded in layers, or in columns or rows in a
      --   flow layout.
      --
      --   @since 0.9.0.0
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
      --           . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"x\"
      --                        , 'PmType' 'Graphics.Vega.VegaLite.Ordinal'
      --                        , 'PBin' ['Graphics.Vega.VegaLite.Step' 5]
      --                        ]
      --           . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PmType' 'Graphics.Vega.VegaLite.Quantitative'
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
      --           . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"role\", 'PmType' 'Graphics.Vega.VegaLite.Ordinal' ]
      --           . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"salary\"
      --                        , 'PmType' 'Graphics.Vega.VegaLite.Quantitative'
      --                        , 'PAggregate' 'Graphics.Vega.VegaLite.Mean'
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
      --           . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"ageGroup\"
      --                        , 'PmType' 'Graphics.Vega.VegaLite.Nominal'
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
      --           . 'position' 'Graphics.Vega.VegaLite.X' ['PName' \"week\", 'PmType' 'Graphics.Vega.VegaLite.Ordinal']
      --           . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"takings\"
      --                        , 'PmType' 'Graphics.Vega.VegaLite.Quantitative'
      --                        , 'PStack' 'Graphics.Vega.VegaLite.StCenter'
      --                        ]
      --           . 'color' ['MName' \"shop\", 'MmType' 'Graphics.Vega.VegaLite.Nominal']
      --   @
      --
      --   Changed from @StackProperty@ in version @0.4.0.0@.
    | PImpute [ImputeProperty]
      -- ^ Set the imputation rules for a position channel. See the
      --   [Vega-Lite impute documentation](https://vega.github.io/vega-lite/docs/impute.html).
      --
      --   @since 0.4.0.0
    | PBand Double
      -- ^ Specify the mark position or size relative to the band size.
      --   The value is in the range 0 to 1, inclusive.
      --
      --   For rectangular-based marks ('Graphics.Vega.VegaLite.Rect', 'Graphics.Vega.VegaLite.Bar', and 'Graphics.Vega.VegaLite.Image'),
      --   the value is the scale factor relative to the band width
      --   (or height), or the time unit interval.
      --
      --   For non-rectangular marks, the relative position on a band of a
      --   stacked, binned, time unit, or band scale is used. A value of
      --   0 positions the band at the beginning of the band, and 1
      --   at the end.
      --
      --   @since 0.5.0.0

positionChannelProperty :: PositionChannel -> Pair
positionChannelProperty (PName s) = "field" .= s
positionChannelProperty (PmType m) = mtype_ m
positionChannelProperty (PBin b) = bin b
positionChannelProperty PBinned = binned_
positionChannelProperty (PAggregate op) = aggregate_ op
positionChannelProperty (PTimeUnit tu) = timeUnit_ tu
positionChannelProperty (PTitle s) = "title" .= splitOnNewline s
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
positionChannelProperty (PRepeatDatum arr) = "datum" .= object [repeat_ arr]
positionChannelProperty PHeight = value_ "height"
positionChannelProperty PWidth = value_ "width"
positionChannelProperty (PDatum d) = "datum" .= dataValueSpec d
positionChannelProperty (PNumber x) = "value" .= x
positionChannelProperty (PImpute ips) = impute_ ips
positionChannelProperty (PBand x) = "band" .= x


{-|

Set the background color of the visualization. If not specified the background
will be white.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'background' "rgb(251,247,238)"
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
    , 'mark' 'Graphics.Vega.VegaLite.Bar' []
    , enc []
    ]
@
-}
background ::
  Color
  -- ^ The background color. For example, @\"rgba(0,0,0,0)\"@ is
  --   transparent.
  --
  --   This was changed to use the @Color@ type alias in version @0.5.0.0@.
  -> PropertySpec
background colour = (VLBackground, fromColor colour)


{-|

Provides an optional description to be associated with the visualization.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'description' "Population change of key regions since 1900"
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
    , 'mark' 'Graphics.Vega.VegaLite.Bar' []
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

Axis customisation properties. These are used for customising individual axes.
To configure all axes, use 'Graphics.Vega.VegaLite.AxisConfig' with a 'Graphics.Vega.VegaLite.configuration' instead. See the
<https://vega.github.io/vega-lite/docs/axis.html#axis-properties Vega-Lite documentation>
for more details.

The @AxTitleMaxLength@ constructor was removed in release @0.4.0.0@. The
'AxTitleLimit' constructor should be used instead.

-}
{-# DEPRECATED AxDates "Please change AxDates to AxValues" #-}
data AxisProperty
    = AxAria Bool
      -- ^ A boolean flag indicating if
      --   [ARIA attributes](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)
      --   should be included (SVG output only).
      --
      --   If False, the \"aria-hidden\" attribute will be set on the output SVG group, removing
      --   the axis from the ARIA accessibility tree.
      --
      --   __Default value:__ True
      --
      --   @since 0.9.0.0
    | AxAriaDescription T.Text
      -- ^ A text description of this axis for
      --   [ARIA accessibility](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)
      --   (SVG output only).
      --
      --   If the 'AxAria' property is True, for SVG output the
      --   [\"aria-label\" attribute](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_aria-label_attribute)
      --   will be set to this description.
      --
      --   If the description is unspecified it will be automatically generated.
      --
      --   @since 0.9.0.0
    | AxBandPosition Double
      -- ^ An interpolation fraction indicating where, for @band@ scales, axis ticks should
      --   be position. A value of @0@ places ticks at the left-edge of the band, @0.5@ in
      --   the middle, and @1@ at the right edge.
      --
      --   @since 0.4.0.0
    | AxDataCondition BooleanOp ConditionalAxisProperty
      -- ^ Set conditions on an axis property. The first argument is the
      --   test to apply, and the second is the pair of properties
      --   to set if the condition holds or not.
      --
      --   The test parameter has access to the axis @value@ and @label@
      --   properties: that is
      --
      --   @
      --   'PAxis' [ 'AxDataCondition'
      --             ('Expr' "datum.value <= 2")
      --             ('CAxTickColor' "red" "blue")
      --         , 'AxDataCondition'
      --             ('Expr' "datum.label == '4.0'")
      --             ('CAxTickWidth' 5 2)
      --         ]
      --   @
      --
      --   Inline aggregation can be performed (before the test)
      --   using 'FilterOpTrans', which can be particularly useful for
      --   filtering temporal data. The following example will use solid
      --   grid lines for the first day in January, and dashes for
      --   all other dates (using 'Data.Function.&'):
      --
      --   @
      --   'PAxis' [ 'AxDataCondition'
      --             ('FEqual' "value" ('Graphics.Vega.VegaLite.DateTime' ['Grahics.Vega.VegaLite.DTMonth' 'Graphics.Vega.VegaLite.Jan', 'Graphics.Vega.VegaLite.DTDate' 1])
      --             & 'FilterOpTrans' ('MTimeUnit' ('Graphics.Vega.VegaLite.TU' 'Graphics.Vega.VegaLite.MonthDate')))
      --             ('CAxGridDash' [] [2, 2])
      --         ]
      --   @
      --
      --   @since 0.5.0.0
    | AxDomain Bool
      -- ^ Should the axis domain (the baseline) be displayed?
    | AxDomainCap StrokeCap
      -- ^ The stroke cap for the domain lines' ending style.
      --
      --   @since 0.9.0.0
    | AxDomainColor Color
      -- ^ The axis domain color.
      --
      --   @since 0.4.0.0
    | AxDomainDash DashStyle
      -- ^ The dash pattern of the domain.
      --
      --   @since 0.4.0.0
    | AxDomainDashOffset DashOffset
      -- ^ The offset for the dash pattern.
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
      --   and data/time values, additionally use 'AxFormatAsNum', 'AxFormatAsTemporal',
      --   or 'AxFormatAsCustom'.
      --
      --   When used with a [custom formatType](https://vega.github.io/vega-lite/docs/config.html#custom-format-type),
      --   this value will be passed as \"format\" alongside \"datum.value\" to the
      --   registered function.
    | AxFormatAsNum
      -- ^ Facet headers should be formatted as numbers. Use a
      --   [d3 numeric format string](https://github.com/d3/d3-format#locale_format)
      --   with 'AxFormat'.
      --
      --   @since 0.4.0.0
    | AxFormatAsTemporal
      -- ^ Facet headers should be formatted as dates or times. Use a
      --   [d3 date/time format string](https://github.com/d3/d3-time-format#locale_format)
      --   with 'AxFormat'.
      --
      --   @since 0.4.0.0
    | AxFormatAsCustom T.Text
      -- ^ The [custom format type](https://vega.github.io/vega-lite/docs/config.html#custom-format-type)
      --   for use with with 'AxFormat'.
      --
      --   @since 0.9.0.0
    | AxGrid Bool
      -- ^ Should an axis grid be displayed?
    | AxGridCap StrokeCap
      -- ^ The stroke cap for the grid lines' ending style.
      --
      --   @since 0.9.0.0
    | AxGridColor Color
      -- ^ The color for the grid.
      --
      --   @since 0.4.0.0
    | AxGridDash DashStyle
      -- ^ The dash pattern of the grid.
      --
      --   @since 0.4.0.0
    | AxGridDashOffset DashOffset
      -- ^ The offset for the dash pattern.
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
    | AxLabelExpr VegaExpr
      -- ^ Provide the expression used to generate axis labels.
      --
      --   The expression can use @datum.value@ and @datum.label@ to access
      --   the data value and default label text respectively.
      --
      --   The following example uses four digit years for decades and
      --   two-digit years for other years:
      --
      --   @
      --   AxLabelExpr "if(year(datum.value) % 10 == 0, utcFormat(datum.value,'%Y'), utcFormat(datum.value,'%y'))"
      --   @
      --
      --   @since 0.5.0.0
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
    | AxLabelLineHeight Double
      -- ^ The line height, in pixels, for multi-line label text.
      --
      --   Added in Vega-Lite 4.6.0.
      --
      --   @since 0.7.0.0
    | AxLabelOffset Double
      -- ^ The pixel offset for labels, in addition to 'AxTickOffset'.
      --
      --   @since 0.6.0.0
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
      --   the 'AxLabelOverlap' strategy is 'Graphics.Vega.VegaLite.ONone'.
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
    | AxStyle [StyleLabel]
      -- ^ The named styles - generated with 'Graphics.Vega.VegaLite.AxisNamedStyles' -
      --   to apply to the axis.
      --
      --   @since 0.6.0.0
    | AxTicks Bool
      -- ^ Should tick marks be drawn on an axis?
    | AxTickBand BandAlign
      -- ^ For band scales, indicates if ticks and grid lines should be
      --   placed at the center of a band (the default) or at the band
      --   extents to indicate intervals.
      --
      --   @since 0.5.0.0
    | AxTickCap StrokeCap
      -- ^ The stroke cap for the grid lines' ending style.
      --
      --   @since 0.9.0.0
    | AxTickColor Color
      -- ^ The color of the ticks.
      --
      --   @since 0.4.0.0
    | AxTickCount Int
      -- ^ The desired number of ticks for axes visualizing quantitative scales.
      --   This is a hint to the system, and the actual number used will be
      --   adjusted to be \"nice\" (multiples of 2, 5, or 10) and lie within the
      --   underlying scale's range.
      --
      --   The 'AxTickCountTime' option can instead be used for \"time\" or
      --   \"utc\" scales.
    | AxTickCountTime ScaleNice
      -- ^ A specialised version of 'AxTickCount' for \"time\" and \"utc\"
      --   time scales.
      --
      --   The 'Graphics.Vega.VegaLite.IsNice' and 'Graphics.Vega.VegaLte.NTickCount'
      --   options should not be used as they generate invalid VegaLite.
      --
      --   @since 0.9.0.0
    | AxTickDash DashStyle
      -- ^ The dash pattern of the ticks.
      --
      --   @since 0.4.0.0
    | AxTickDashOffset DashOffset
      -- ^ The offset for the dash pattern.
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
      --   See also 'AxLabelOffset'.
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
    | AxTitleLineHeight Double
      -- ^ Line height, in pixels, for multi-line title text.
      --
      --   @since 0.5.0.0
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
    | AxTranslateOffset Double
      -- ^ The translation offset in pixels applied to the axis group
      --   mark x and y. If specified it overrides the default value
      --   of a 0.5 offset to pixel-align stroked lines.
      --
      --   @since 0.5.0.0
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


axisProperty :: AxisProperty -> Pair
axisProperty (AxStyle [s]) = "style" .= s
axisProperty (AxStyle s) = "style" .= s

axisProperty (AxAria b) = "aria" .= b
axisProperty (AxAriaDescription t) = "description" .= t

axisProperty (AxBandPosition x) = "bandPosition" .= x
axisProperty (AxDataCondition predicate cap) =
  let (ifAxProp, elseAxProp) = conditionalAxisProperty cap
      (axKey, ifProp) = axisProperty ifAxProp
      (_, elseProp) = axisProperty elseAxProp
  in axKey .= object [ "condition" .= object [ "test" .= booleanOpSpec predicate
                                             , "value" .= ifProp
                                             ]
                     , "value" .= elseProp]
axisProperty (AxDomain b) = "domain" .= b
axisProperty (AxDomainCap c) = "domainCap" .= strokeCapLabel c
axisProperty (AxDomainColor s) = "domainColor" .= fromColor s
axisProperty (AxDomainDash ds) = "domainDash" .= fromDS ds
axisProperty (AxDomainDashOffset x) = "domainDashOffset" .= x
axisProperty (AxDomainOpacity x) = "domainOpacity" .= x
axisProperty (AxDomainWidth x) = "domainWidth" .= x
axisProperty (AxFormat fmt) = "format" .= fmt
axisProperty AxFormatAsNum = "formatType" .= fromT "number"
axisProperty AxFormatAsTemporal = "formatType" .= fromT "time"
axisProperty (AxFormatAsCustom c) = "formatType" .= c
axisProperty (AxGrid b) = "grid" .= b
axisProperty (AxGridCap c) = "gridCap" .= strokeCapLabel c
axisProperty (AxGridColor s) = "gridColor" .= fromColor s
axisProperty (AxGridDash ds) = "gridDash" .= fromDS ds
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
axisProperty (AxLabelColor s) = "labelColor" .= fromColor s
axisProperty (AxLabelExpr e) = "labelExpr" .= e
axisProperty AxLabelNoFlush = "labelFlush" .= False
axisProperty AxLabelFlush = "labelFlush" .= True
axisProperty (AxLabelFlushValue x) = "labelFlush" .= x
axisProperty (AxLabelFlushOffset x) = "labelFlushOffset" .= x
axisProperty (AxLabelFont s) = "labelFont" .= s
axisProperty (AxLabelFontSize x) = "labelFontSize" .= x
axisProperty (AxLabelFontStyle s) = "labelFontStyle" .= s
axisProperty (AxLabelFontWeight fw) = "labelFontWeight" .= fontWeightSpec fw
axisProperty (AxLabelLimit x) = "labelLimit" .= x
axisProperty (AxLabelLineHeight x) = "labelLineHeight" .= x
axisProperty (AxLabelOffset x) = "labelOffset" .= x
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
axisProperty (AxTickBand bnd) = "tickBand" .= bandAlignLabel bnd
axisProperty (AxTickCap c) = "tickCap" .= strokeCapLabel c
axisProperty (AxTickColor s) = "tickColor" .= fromColor s
axisProperty (AxTickCount n) = "tickCount" .= n
axisProperty (AxTickCountTime sn) = "tickCount" .= scaleNiceSpec sn
axisProperty (AxTickDash ds) = "tickDash" .= fromDS ds
axisProperty (AxTickDashOffset x) = "tickDashOffset" .= x
axisProperty (AxTickExtra b) = "tickExtra" .= b
axisProperty (AxTickMinStep x) = "tickMinStep" .= x
axisProperty (AxTickOffset x) = "tickOffset" .= x
axisProperty (AxTickOpacity x) = "tickOpacity" .= x
axisProperty (AxTickRound b) = "tickRound" .= b
axisProperty (AxTickSize x) = "tickSize" .= x
axisProperty (AxTickWidth x) = "tickWidth" .= x
axisProperty (AxTitle ttl) = "title" .= splitOnNewline ttl
axisProperty AxNoTitle = "title" .= A.Null
axisProperty (AxTitleAlign ha) = "titleAlign" .= hAlignLabel ha
axisProperty (AxTitleAnchor a) = "titleAnchor" .= anchorLabel a
axisProperty (AxTitleAngle x) = "titleAngle" .= x
axisProperty (AxTitleBaseline va) = "titleBaseline" .= vAlignLabel va
axisProperty (AxTitleColor s) = "titleColor" .= fromColor s
axisProperty (AxTitleFont s) = "titleFont" .= s
axisProperty (AxTitleFontSize x) = "titleFontSize" .= x
axisProperty (AxTitleFontStyle s) = "titleFontStyle" .= s
axisProperty (AxTitleFontWeight fw) = "titleFontWeight" .= fontWeightSpec fw
axisProperty (AxTitleLimit x) = "titleLimit" .= x
axisProperty (AxTitleLineHeight x) = "titleLineHeight" .= x
axisProperty (AxTitleOpacity x) = "titleOpacity" .= x
axisProperty (AxTitlePadding pad) = "titlePadding" .= pad
axisProperty (AxTitleX x) = "titleX" .= x
axisProperty (AxTitleY x) = "titleY" .= x
axisProperty (AxTranslateOffset x) = "translate" .= x
axisProperty (AxValues vals) = "values" .= dataValuesSpecs vals
axisProperty (AxDates dtss) = "values" .= map dateTimeSpec dtss
axisProperty (AxZIndex z) = "zindex" .= z


{-|

For use with 'AxDataCondition', and defines those axis properties
which can be conditioned on their position (or label).

The constuctor determines the axis property (a label, tick, or
grid element), and the two arguments are the value to set if the condition
is 'True' (first), and for when it is 'False' (second).

@since 0.5.0.0
-}

data ConditionalAxisProperty
  = CAxGridColor Color Color
    -- ^ The color for the axis grid.
  | CAxGridDash DashStyle DashStyle
    -- ^ The dash pattern for the axis grid.
  | CAxGridDashOffset DashOffset DashOffset
    -- ^ The offset for the dash pattern.
  | CAxGridOpacity Opacity Opacity
    -- ^ The opacity of the axis grid.
  | CAxGridWidth Double Double
    -- ^ The width of the axis grid.
  | CAxLabelAlign HAlign HAlign
    -- ^ Axis label horizontal alignment.
  | CAxLabelBaseline VAlign VAlign
    -- ^ Axis label vertical alignment.
  | CAxLabelColor Color Color
    -- ^ Axis label color.
  | CAxLabelFont T.Text T.Text
    -- ^ Axis label font.
  | CAxLabelFontSize Double Double
    -- ^ Axis label font.
  | CAxLabelFontStyle T.Text T.Text
    -- ^ Axis label font style.
  | CAxLabelFontWeight FontWeight FontWeight
    -- ^ Axis label font weight.
  | CAxLabelOffset Double Double
    -- ^ Axis label offset.
    --
    --  @since 0.6.0.0
  | CAxLabelOpacity Opacity Opacity
    -- ^ Axis label opacity.
  | CAxLabelPadding Double Double
    -- ^ Axis label padding.
    --
    --   @since 0.6.0.0
  | CAxTickColor T.Text T.Text
    -- ^ Tick color for the axis.
  | CAxTickDash DashStyle DashStyle
    -- ^ The dash pattern for the axis ticks.
  | CAxTickDashOffset DashOffset DashOffset
    -- ^ The offset for the dash pattern.
  | CAxTickOpacity Opacity Opacity
    -- ^ Opacity of the axis tick marks.
  | CAxTickSize Double Double
    -- ^ Size, in pixels, of the axis tick marks.
    --
    --   @since 0.6.0.0
  | CAxTickWidth Double Double
    -- ^ Width, in pixels, of the axis tick marks.


conditionalAxisProperty :: ConditionalAxisProperty -> (AxisProperty, AxisProperty)
conditionalAxisProperty (CAxGridColor t f) = (AxGridColor t, AxGridColor f)
conditionalAxisProperty (CAxGridDash t f) = (AxGridDash t, AxGridDash f)
conditionalAxisProperty (CAxGridDashOffset t f) = (AxGridDashOffset t, AxGridDashOffset f)
conditionalAxisProperty (CAxGridOpacity t f) = (AxGridOpacity t, AxGridOpacity f)
conditionalAxisProperty (CAxGridWidth t f) = (AxGridWidth t, AxGridWidth f)
conditionalAxisProperty (CAxLabelAlign t f) = (AxLabelAlign t, AxLabelAlign f)
conditionalAxisProperty (CAxLabelBaseline t f) = (AxLabelBaseline t, AxLabelBaseline f)
conditionalAxisProperty (CAxLabelColor t f) = (AxLabelColor t, AxLabelColor f)
conditionalAxisProperty (CAxLabelFont t f) = (AxLabelFont t, AxLabelFont f)
conditionalAxisProperty (CAxLabelFontSize t f) = (AxLabelFontSize t, AxLabelFontSize f)
conditionalAxisProperty (CAxLabelFontStyle t f) = (AxLabelFontStyle t, AxLabelFontStyle f)
conditionalAxisProperty (CAxLabelFontWeight t f) = (AxLabelFontWeight t, AxLabelFontWeight f)
conditionalAxisProperty (CAxLabelOffset t f) = (AxLabelOffset t, AxLabelOffset f)
conditionalAxisProperty (CAxLabelOpacity t f) = (AxLabelOpacity t, AxLabelOpacity f)
conditionalAxisProperty (CAxLabelPadding t f) = (AxLabelPadding t, AxLabelPadding f)
conditionalAxisProperty (CAxTickColor t f) = (AxTickColor t, AxTickColor f)
conditionalAxisProperty (CAxTickDash t f) = (AxTickDash t, AxTickDash f)
conditionalAxisProperty (CAxTickDashOffset t f) = (AxTickDashOffset t, AxTickDashOffset f)
conditionalAxisProperty (CAxTickOpacity t f) = (AxTickOpacity t, AxTickOpacity f)
conditionalAxisProperty (CAxTickSize t f) = (AxTickSize t, AxTickSize f)
conditionalAxisProperty (CAxTickWidth t f) = (AxTickWidth t, AxTickWidth f)


{-|

Declare the way the view is sized. See the
<https://vega.github.io/vega-lite/docs/size.html#autosize Vega-Lite documentation>
for details.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'width' 250
    , 'height' 300
    , 'autosize' [ 'Graphics.Vega.VegaLite.AFit', 'Graphics.Vega.VegaLite.APadding', 'Graphics.Vega.VegaLite.AResize' ]
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
    , 'mark' 'Graphics.Vega.VegaLite.Bar' []
    , enc []
    ]
@
-}
autosize :: [Autosize] -> PropertySpec
autosize aus = (VLAutosize, object (map autosizeProperty aus))


-- | The background style of a single view or layer in a view composition.
--
--   @since 0.4.0.0

viewBackground :: [ViewBackground] -> PropertySpec
viewBackground vbs = (VLViewBackground, object (map viewBackgroundSpec vbs))


{-|

Used for creating logical compositions. For example

@
'color'
    [ 'MSelectionCondition' (Or ('SelectionName' "alex") (SelectionName "morgan"))
        ['MAggregate' 'Count', 'MName' "*", 'MmType' 'Graphics.Vega.VegaLite.Quantitative']
        ['MString' "gray"]
    ]
@

Logical compositions can be nested to any level as shown in this example

@
'Not' ('And' ('Expr' "datum.IMDB_Rating === null") ('Expr' "datum.Rotten_Tomatoes_Rating === null") )
@
-}
data BooleanOp
    = Expr VegaExpr
      -- ^ Expression that should evaluate to either true or false.
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
      --   'filter' ('FRange' "date" ('NumberRange' 2010 2017)
      --           & 'FilterOpTrans' ('MTimeUnit' ('Graphics.Vega.VegaLite.TU' 'Graphics.Vega.VegaLite.Year'))
      --           & 'FCompose'
      --           )
      --   @
      --
      --   @since 0.4.0.0
    | Selection SelectionLabel  -- TODO: rename Selected since collides with Selection type
      -- ^ Interactive selection that will be true or false as part of
      --   a logical composition.  For example: to filter a dataset so
      --   that only items selected interactively and that have a
      --   weight of more than 30:
      --
      -- @
      -- 'transform'
      --    . 'filter' ('FCompose' ('And' ('Selection' "brush") ('Expr' "datum.weight > 30")))
      -- @
    | SelectionName SelectionLabel
    -- ^  Name a selection that is used as part of a conditional encoding.
    --
    -- @
    -- 'color'
    --    [ 'MSelectionCondition' ('SelectionName' \"myBrush\")
    --        ['MName' \"myField\", 'MmType' 'Graphics.Vega.VegaLite.Nominal']
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
    = FEqual FieldName DataValue
      -- ^ Filter a data stream so that only data in a given field equal to
      --   the given value are used.
    | FLessThan FieldName DataValue
      -- ^ Filter a data stream so that only data in a given field less than the given
      --   value are used.
      --
      --   @since 0.4.0.0
    | FLessThanEq FieldName DataValue
      -- ^ Filter a data stream so that only data in a given field less than,
      --   or equal to, the given value are used.
      --
      --   @since 0.4.0.0
    | FGreaterThan FieldName DataValue
      -- ^ Filter a data stream so that only data in a given field greater than the given
      --   value are used.
      --
      --   @since 0.4.0.0
    | FGreaterThanEq FieldName DataValue
      -- ^ Filter a data stream so that only data in a given field greater than,
      --   or equal to, the given value are used.
      --
      --   @since 0.4.0.0
    | FExpr VegaExpr
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
    | FSelection SelectionLabel
      -- ^ Filter a data stream so that only data in a given field that are
      --   within the given interactive selection are used.
      --
      --   @
      --   sel = 'Graphics.Vega.VegaLite.selection' . 'Graphics.Vega.VegaLite.select' \"myBrush\" 'Graphics.Vega.VegaLite.Interval' ['Graphics.Vega.VegaLite.Encodings' ['Graphics.Vega.VegaLite.ChX']]
      --   trans = 'transform' . 'filter' ('FSelection' \"myBrush\")
      --   @
    | FOneOf FieldName DataValues
      -- ^ Filter a data stream so that only data in a given field contained in the given
      --   list of values are used.
    | FRange FieldName FilterRange
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
    | FValid FieldName
      -- ^ Filter a data stream so that only valid data (i.e. not null or NaN) in a given
      --   field are used.
      --
      --   @since 0.4.0.0


#if MIN_VERSION_aeson(2, 0, 0)
fop_ :: FieldName -> Key.Key -> DataValue -> [Pair]
#else
fop_ :: FieldName -> T.Text -> DataValue -> [Pair]
#endif
fop_ field label val = ["field" .= field,
                        label .= dataValueSpec val]

filterProperty :: Filter -> [Pair]

filterProperty (FEqual field val) = fop_ field "equal" val
filterProperty (FLessThan field val) = fop_ field "lt" val
filterProperty (FLessThanEq field val) = fop_ field "lte" val
filterProperty (FGreaterThan field val) = fop_ field "gt" val
filterProperty (FGreaterThanEq field val) = fop_ field "gte" val

filterProperty (FSelection selName) = ["selection" .= selName]

filterProperty (FRange field vals) =
  let ans = case vals of
              NumberRange mn mx -> map toJSON [mn, mx]
              NumberRangeLL mn -> [toJSON mn, A.Null]
              NumberRangeUL mx -> [A.Null, toJSON mx]
              DateRange dMin dMax -> [process dMin, process dMax]

      process [] = A.Null
      process dts = dateTimeSpec dts

  in ["field" .= field, "range" .= ans]

filterProperty (FOneOf field vals) =
  let ans = case vals of
              Numbers xs -> map toJSON xs
              DateTimes dts -> map dateTimeSpec dts
              Strings ss -> map toJSON ss
              Booleans bs -> map toJSON bs

  in ["field" .= field, "oneOf" .= ans]

filterProperty (FValid field) = ["field" .= field, "valid" .= True]
filterProperty _ = []  -- ignore FExpr and FCompose


filterSpec :: Filter -> VLSpec
filterSpec (FExpr expr) = toJSON expr
filterSpec (FCompose boolExpr) = booleanOpSpec boolExpr
filterSpec f = object (filterProperty f)

trFilterSpec :: MarkChannel -> Filter -> VLSpec
trFilterSpec _ (FExpr expr) = toJSON expr
trFilterSpec _ (FCompose boolExpr) = booleanOpSpec boolExpr
trFilterSpec mchan fi = object (markChannelProperty mchan <> filterProperty fi)


-- | A pair of filter range data values, used with 'FRange'.

data FilterRange
    = NumberRange Double Double
      -- ^ Select between these two values (both limits are inclusive).
    | NumberRangeLL Double
      -- ^ A lower limit (inclusive).
      --
      --   @since 0.7.0.0
    | NumberRangeUL Double
      -- ^ An upper limit (inclusive).
      --
      --   @since 0.7.0.0
    | DateRange [DateTime] [DateTime]
      -- ^ Select between these two dates (both limits are inclusive).
      --
      --   If a limit is the empty list then the filter is treated as
      --   a limit only on the other value, so
      --   @DateRange [] ['Graphics.Vega.VegaLite.DTYear' 2019]@
      --   acts as an upper-limit on the date range. One of the two
      --   limits __should__ be defined, but there is no enforcement
      --   of this.


-- | Types of hyperlink channel property used for linking marks or text to URLs.
--
--   Unfortunately there is a split between @H@ and @Hy@ as the prefix.
data HyperlinkChannel
    = HName FieldName
      -- ^ Field used for encoding with a hyperlink channel.
    | HRepeat Arrangement
      -- ^ Reference in a hyperlink channel to a field name generated by 'repeatFlow'
      --   or 'repeat'. The parameter identifies whether reference is being made to
      --   fields that are to be arranged in columns, in rows, or a with a flow layout.
    | HmType Measurement
      -- ^ Level of measurement when encoding with a hyperlink channel.
    | HAggregate Operation
      -- ^ Compute aggregate summary statistics for a field to be encoded with a
      --   hyperlink channel.
    | HyBand Double
      -- ^ Specify the mark position or size relative to the band size.
      --   The value is in the range 0 to 1, inclusive.
      --
      --   @since 0.9.0.0
    | HBin [BinProperty]
      -- ^ Discretize numeric values into bins when encoding with a hyperlink channel.
    | HBinned
      -- ^ Indicate that data encoded with a hyperlink channel are already binned.
      --
      --   @since 0.4.0.0
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
    | HyFormat T.Text
      -- ^ [Formatting pattern](https://vega.github.io/vega-lite/docs/format.html)
      --   for hyperlink properties. To distinguish between formatting as numeric values and data/time
      --   values, additionally use 'HyFormatAsNum', 'HyFormatAsTemporal', and
      --   'HyFormatAsCustom'.
      --
      --   @since 0.9.0.0
    | HyFormatAsNum
      -- ^ The marks should be formatted as numbers. Use a
      --   [d3 numeric format string](https://github.com/d3/d3-format#locale_format)
      --   with 'HyFormat'.
      --
      --   @since 0.9.0.0
    | HyFormatAsTemporal
      -- ^ The marks should be formatted as dates or times. Use a
      --   [d3 date/time format string](https://github.com/d3/d3-time-format#locale_format)
      --   with 'HyFormat'.
      --
      --   @since 0.9.0.0
    | HyFormatAsCustom T.Text
      -- ^ The [custom format type](https://vega.github.io/vega-lite/docs/config.html#custom-format-type)
      --   for use with with 'HyFormat'.
      --
      --   @since 0.9.0.0
    | HyLabelExpr VegaExpr
      -- ^ Provide the expression used to generate labels.
      --
      --   @since 0.9.0.0
    | HString T.Text
      -- ^ Literal string value when encoding with a hyperlink channel.
    | HTimeUnit TimeUnit
      -- ^ Time unit aggregation of field values when encoding with a
      --   hyperlink channel.
    | HyTitle T.Text
      -- ^ Title of a field when encoding with a hyperlink channel.
      --
      --   @since 0.9.0.0
    | HyNoTitle
      -- ^ Display no title.
      --
      --   @since 0.9.0.0

hyperlinkChannelProperty :: HyperlinkChannel -> [Pair]
hyperlinkChannelProperty (HName s) = ["field" .= s]
hyperlinkChannelProperty (HRepeat arr) = ["field" .= object [repeat_ arr]]
hyperlinkChannelProperty (HmType t) = [mtype_ t]
hyperlinkChannelProperty (HAggregate op) = [aggregate_ op]
hyperlinkChannelProperty (HyBand x) = ["band" .= x]
hyperlinkChannelProperty (HBin bps) = [bin bps]
hyperlinkChannelProperty HBinned = [binned_]
hyperlinkChannelProperty (HSelectionCondition selName ifClause elseClause) =
  selCond_ hyperlinkChannelProperty selName ifClause elseClause
hyperlinkChannelProperty (HDataCondition tests elseClause) =
  dataCond_ hyperlinkChannelProperty tests elseClause
hyperlinkChannelProperty (HyFormat fmt) = ["format" .= fmt]
hyperlinkChannelProperty HyFormatAsNum = ["formatType" .= fromT "number"]
hyperlinkChannelProperty HyFormatAsTemporal = ["formatType" .= fromT "time"]
hyperlinkChannelProperty (HyFormatAsCustom c) = ["formatType" .= c]
hyperlinkChannelProperty (HyLabelExpr lbl) = ["labelExpr" .= lbl]
hyperlinkChannelProperty (HString s) = [value_ s]
hyperlinkChannelProperty (HTimeUnit tu) = [timeUnit_ tu]
hyperlinkChannelProperty (HyTitle t) = ["title" .= t]
hyperlinkChannelProperty HyNoTitle = ["title" .= A.Null]


-- | A text description of this mark for ARIA accessibility.
--
--   @since 0.9.0.0
data AriaDescriptionChannel
    = ADName FieldName
      -- ^ Field used for encoding with an Aria description.
    | ADRepeat Arrangement
      -- ^ Reference in an Aria description channel to a field name generated by 'repeatFlow'
      --   or 'repeat'. The parameter identifies whether reference is being made to
      --   fields that are to be arranged in columns, in rows, or a with a flow layout.
    | ADmType Measurement
      -- ^ Level of measurement.
    | ADAggregate Operation
      -- ^ Compute aggregate summary statistics for a field to be encoded.
    | ADBand Double
      -- ^ Specify the mark position or size relative to the band size.
      --   The value is in the range 0 to 1, inclusive.
    | ADBin [BinProperty]
      -- ^ Discretize numeric values into bins.
    | ADBinned
      -- ^ Indicate that data encoded are already binned.
    | ADSelectionCondition BooleanOp [AriaDescriptionChannel] [AriaDescriptionChannel]
      -- ^ Make the channel conditional on interactive selection. The first parameter
      --   provides the selection to evaluate, the second the encoding to apply if the description
      --   has been selected, the third the encoding if it is not selected.
    | ADDataCondition [(BooleanOp, [AriaDescriptionChannel])] [AriaDescriptionChannel]
      -- ^ Make the channel conditional on one or more predicate expressions. The first
      --   parameter is a list of tuples each pairing an expression to evaluate with the encoding
      --   if that expression is @True@. The second is the encoding if none of the expressions
      --   evaluate as @True@.
    | ADFormat T.Text
      -- ^ [Formatting pattern](https://vega.github.io/vega-lite/docs/format.html)
      --   for descriptions. To distinguish between formatting as numeric values and data/time
      --   values, additionally use 'ADFormatAsNum', 'ADFormatAsTemporal', and
      --   'ADFormatAsCustom'.
    | ADFormatAsNum
      -- ^ The marks should be formatted as numbers. Use a
      --   [d3 numeric format string](https://github.com/d3/d3-format#locale_format)
      --   with 'ADFormat'.
    | ADFormatAsTemporal
      -- ^ The marks should be formatted as dates or times. Use a
      --   [d3 date/time format string](https://github.com/d3/d3-time-format#locale_format)
      --   with 'ADFormat'.
    | ADFormatAsCustom T.Text
      -- ^ The [custom format type](https://vega.github.io/vega-lite/docs/config.html#custom-format-type)
      --   for use with with 'ADFormat'.
    | ADLabelExpr VegaExpr
      -- ^ Provide the expression used to generate labels.
    | ADString T.Text
      -- ^ Literal string value.
    | ADTimeUnit TimeUnit
      -- ^ Time unit aggregation of field values when encoding with an Aria
      --   description channel.
    | ADTitle T.Text
      -- ^ Title of a field when encoding with an Aria description channel.
    | ADNoTitle
      -- ^ Display no title.


ariaDescriptionChannelProperty :: AriaDescriptionChannel -> [Pair]
ariaDescriptionChannelProperty (ADName s) = ["field" .= s]
ariaDescriptionChannelProperty (ADRepeat arr) = ["field" .= object [repeat_ arr]]
ariaDescriptionChannelProperty (ADmType t) = [mtype_ t]
ariaDescriptionChannelProperty (ADAggregate op) = [aggregate_ op]
ariaDescriptionChannelProperty (ADBand x) = ["band" .= x]
ariaDescriptionChannelProperty (ADBin bps) = [bin bps]
ariaDescriptionChannelProperty ADBinned = [binned_]
ariaDescriptionChannelProperty (ADSelectionCondition selName ifClause elseClause) =
  selCond_ ariaDescriptionChannelProperty selName ifClause elseClause
ariaDescriptionChannelProperty (ADDataCondition tests elseClause) =
  dataCond_ ariaDescriptionChannelProperty tests elseClause
ariaDescriptionChannelProperty (ADFormat fmt) = ["format" .= fmt]
ariaDescriptionChannelProperty ADFormatAsNum = ["formatType" .= fromT "number"]
ariaDescriptionChannelProperty ADFormatAsTemporal = ["formatType" .= fromT "time"]
ariaDescriptionChannelProperty (ADFormatAsCustom c) = ["formatType" .= c]
ariaDescriptionChannelProperty (ADLabelExpr lbl) = ["labelExpr" .= lbl]
ariaDescriptionChannelProperty (ADString s) = [value_ s]
ariaDescriptionChannelProperty (ADTimeUnit tu) = [timeUnit_ tu]
ariaDescriptionChannelProperty (ADTitle t) = ["title" .= t]
ariaDescriptionChannelProperty ADNoTitle = ["title" .= A.Null]


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
    , 'MmType' 'Graphics.Vega.VegaLite.Ordinal'
    , 'MScale' (domainRangeMap (1955, \"rgb(230,149,156)\") (2000, \"rgb(145,26,36)\"))
    ]
@
-}

domainRangeMap :: (Double, Color) -> (Double, Color) -> [ScaleProperty]
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

categoricalDomainMap :: [(T.Text, Color)] -> [ScaleProperty]
categoricalDomainMap scaleDomainPairs =
  let (domain, range) = unzip scaleDomainPairs
  in [SDomain (DStrings domain), SRange (RStrings range)]


{-|

Types of facet channel property used for creating a composed facet view of small
multiples.

-}

-- based on schema 3.3.0 #/definitions/FacetFieldDef
-- although it's a bit different now (maybe RowColumnEncodingFieldDef in 4.2.0)


data FacetChannel
    = FName FieldName
      -- ^ The name of the field from which to pull a data value.
    | FmType Measurement
      -- ^ The encoded field's type of measurement.
    | FAggregate Operation
      -- ^ Aggregation function for the field.
    | FAlign CompositionAlignment
      -- ^ The alignment to apply to the row- or column- facet's subplot.
      --
      --   @since 0.6.0.0
    | FBin [BinProperty]
      -- ^ Describe how to bin quantitative fields, or whether the
      --   channels are already binned.
    | FCenter Bool
      -- ^ Should sub-views be centered relative to their respective rows or
      --   columns.
      --
      --   @since 0.6.0.0
    | FHeader [HeaderProperty]
      -- ^ The properties of a facet's header.
    | FSort [SortProperty]
      -- ^ Sort order for the encoded field.
      --
      --   @since 0.4.0.0
    | FSpacing Double
      -- ^ The pixel spacing between sub-views.
      --
      --   If you have code from a version of @hvega@ before @0.6.0.0@ that
      --   uses @FSpacing@ (with 'Graphics.Vega.VegaLite.FacetStyle'), please
      --   use 'Graphics.Vega.VegaLite.CompSpacing' as a replacement.
      --
      --   @since 0.6.0.0
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

facetChannelProperty :: FacetChannel -> Pair
facetChannelProperty (FName s) = "field" .= s
facetChannelProperty (FmType measure) = mtype_ measure
facetChannelProperty (FAlign algn) = "align" .= compositionAlignmentSpec algn
facetChannelProperty (FAggregate op) = aggregate_ op
facetChannelProperty (FBin bps) = bin bps
facetChannelProperty (FCenter b) = "center" .= b
facetChannelProperty (FHeader hps) = toKey (header_ "" hps)
facetChannelProperty (FSort sps) = sort_ sps
facetChannelProperty (FSpacing x) = "spacing" .= x
facetChannelProperty (FTitle s) = "title" .= s
facetChannelProperty FNoTitle = "title" .= A.Null
facetChannelProperty (FTimeUnit tu) = timeUnit_ tu


-- | Types of text channel property used for displaying text as part of the visualization.

-- Basing the following partly on vega-lite-3.3.0.json / TextFieldDef
-- but that doesn't seem to be sufficient.

data TextChannel
    = TName FieldName
      -- ^ Name of the field used for encoding with a text channel.
    | TRepeat Arrangement
      -- ^ Reference in a text channel to a field name generated by 'repeatFlow'
      --   or 'repeat'. The parameter identifies whether reference is being made to
      --   fields that are to be arranged in columns, in rows, or a with a flow layout.
    | TRepeatDatum Arrangement
      -- ^ Reference in a text channel to a datum value generated by 'repeatFlow'
      --   or 'repeat'. The parameter identifies whether reference is being made to
      --   a datum that is to be encoded in layers, or in columns or rows in a
      --   flow layout.
      --
      --   @since 0.9.0.0
    | TmType Measurement
      -- ^ Level of measurement when encoding with a text channel.
    | TAggregate Operation
      -- ^ Compute some aggregate summary statistics for a field to be encoded with a
      --   text channel. The type of aggregation is determined by the given operation
      --   parameter.
    | TBand Double
      -- ^ Specify the mark position or size relative to the band size.
      --   The value is in the range 0 to 1, inclusive.
      --
      --   @since 0.9.0.0
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
    | TSelectionCondition BooleanOp [TextChannel] [TextChannel]
      -- ^ Make a text channel conditional on interactive selection. The first parameter
      --   is a selection condition to evaluate; the second the encoding to apply if that
      --   selection is true; the third parameter is the encoding if the selection is false.
    | TDatum DataValue
      -- ^ A constant value in the data domain.
      --
      --   @since 0.9.0.0
    | TFormat T.Text
      -- ^ [Formatting pattern](https://vega.github.io/vega-lite/docs/format.html)
      --   for text marks. To distinguish between formatting as numeric values and data/time
      --   values, additionally use 'TFormatAsNum', 'TFormatAsTemporal', and
      --   'TFormatAsCustom'.
    | TFormatAsNum
      -- ^ The text marks should be formatted as numbers. Use a
      --   [d3 numeric format string](https://github.com/d3/d3-format#locale_format)
      --   with 'TFormat'.
      --
      --   @since 0.4.0.0
    | TFormatAsTemporal
      -- ^ The text marks should be formatted as dates or times. Use a
      --   [d3 date/time format string](https://github.com/d3/d3-time-format#locale_format)
      --   with 'TFormat'.
      --
      --   @since 0.4.0.0
    | TFormatAsCustom T.Text
      -- ^ The [custom format type](https://vega.github.io/vega-lite/docs/config.html#custom-format-type)
      --   for use with with 'TFormat'.
      --
      --   @since 0.9.0.0
    | TLabelExpr VegaExpr
      -- ^ Provide the expression used to generate labels.
      --
      --   @since 0.9.0.0
    | TString T.Text
      -- ^ A literal value for encoding a text property channel. See also 'TStrings'.
      --
      --   This can be useful for a text annotation, such as:
      --
      --   @
      --   'encoding'
      --      . 'position' 'Graphics.Vega.VegaLite.X' [ 'PNumber' 300 ]
      --      . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PNumber' 1234 ]
      --      . 'text' [ 'TString' \"Upper limit\" ]
      --   @
      --
      --   @since 0.5.0.0
    | TStrings [T.Text]
      -- ^ A multi-line value. See also 'TString'.
      --
      --   @since 0.7.0.0
    | TTimeUnit TimeUnit
      -- ^ Time unit aggregation of field values when encoding with a text channel.
    | TTitle T.Text
      -- ^ Title of a field when encoding with a text or tooltip channel.
      --
      --   @since 0.4.0.0
    | TNoTitle
      -- ^ Display no title.
      --
      --   @since 0.4.0.0

textChannelProperty :: TextChannel -> [Pair]
textChannelProperty (TName s) = ["field" .= s]
textChannelProperty (TRepeat arr) = ["field" .= object [repeat_ arr]]
textChannelProperty (TRepeatDatum arr) = ["datum" .= object [repeat_ arr]]
textChannelProperty (TmType measure) = [mtype_ measure]
textChannelProperty (TAggregate op) = [aggregate_ op]
textChannelProperty (TBand x) = ["band" .= x]
textChannelProperty (TBin bps) = [bin bps]
textChannelProperty TBinned = [binned_]
textChannelProperty (TDataCondition tests elseClause) =
  dataCond_ textChannelProperty tests elseClause
textChannelProperty (TSelectionCondition selName ifClause elseClause) =
  selCond_ textChannelProperty selName ifClause elseClause
textChannelProperty (TDatum dv) = ["datum" .= dataValueSpec dv]
textChannelProperty (TFormat fmt) = ["format" .= fmt]
textChannelProperty TFormatAsNum = ["formatType" .= fromT "number"]
textChannelProperty TFormatAsTemporal = ["formatType" .= fromT "time"]
textChannelProperty (TFormatAsCustom c) = ["formatType" .= c]
textChannelProperty (TLabelExpr e) = ["labelExpr" .= e]
textChannelProperty (TString s) = ["value" .= s]
textChannelProperty (TStrings xs) = ["value" .= xs]
textChannelProperty (TTimeUnit tu) = [timeUnit_ tu]
textChannelProperty (TTitle s) = ["title" .= splitOnNewline s]
textChannelProperty TNoTitle = ["title" .= A.Null]


-- | Properties of an ordering channel used for sorting data fields.

-- maps to OrderFieldDef

data OrderChannel
    = OName FieldName
      -- ^ The name of the field used for encoding with an order channel.
    | ORepeat Arrangement
      -- ^ Reference in an order channel to a field name generated by 'repeatFlow'
      -- or 'repeat'. The parameter identifies whether reference is being made to
      -- fields that are to be arranged in columns, in rows, or a with a flow layout.
    | OAggregate Operation
      -- ^ Compute some aggregate summary statistics for a field to be encoded
      --   with an order channel.
    | OBand Double
      -- ^ For rect-based marks, define the mark size relative to the bandwidth of
      --   band scales, bins, or time units: a value of 1 uses the range and 0.5
      --   half the range. For other marks it defines the relative position in a
      --   band of a stacked, binned, time unit, or band scale: if 0 the marks
      --   will be positioned at the beginning of the band and 0.5 gives the
      --   middle of the band.
      --
      --   The argument must be in the range 0 to 1, inclusive, but there is no
      --   check on this.
      --
      --   @since 0.11.0.0
    | OBin [BinProperty]
      -- ^ Discretize numeric values into bins when encoding with an
      --   order channel.
    | OSort [SortProperty]
      -- ^ Sort order for field when encoding with an order channel.
    | OTimeUnit TimeUnit
      -- ^ Form of time unit aggregation of field values when encoding with
      --   an order channel.
    | OTitle T.Text
      -- ^ The title for the field.
      --
      --   Note that if both the field and axis, header, or legend titles are
      --   defined than the latter (axis, header, or legend) will be used.
      --
      --   @since 0.11.0.0
    | ONoTitle
      -- ^ Remove the title.
      --
      --   @since 0.11.0.0
    | OmType Measurement
      -- ^ The level of measurement when encoding with an order channel.
    | ODataCondition [(BooleanOp, [OrderChannel])] [OrderChannel]
      -- ^ Make an order channel conditional on one or more predicate expressions. The first
      --   parameter is a list of tuples each pairing an expression to evaluate with the encoding
      --   if that expression is @True@. The second is the encoding if none of the expressions
      --   evaluate as @True@.
      --
      --   @since 0.11.0.0
    | OSelectionCondition BooleanOp [OrderChannel] [OrderChannel]
      -- ^ Make an order channel conditional on interactive selection. The first parameter
      --   is a selection condition to evaluate; the second the encoding to apply if that
      --   selection is true; the third parameter is the encoding if the selection is false.
      --
      --   An example:
      --
      --   @'order' ['OSelectionCondition' ('SelectionName' "highlight")
      --           ['ONumber' 1] ['ONumber' 0]]
      --   @
      --
      --   @since 0.11.0.0
    | ONumber Double
      -- ^ Create a value with this number. For use with 'OSelectionCondition'
      --   and 'ODataCondition'.
      --
      --   @since 0.11.0.0

orderChannelProperty :: OrderChannel -> [Pair]
orderChannelProperty (OAggregate op) = [aggregate_ op]
orderChannelProperty (OBand x) = ["band" .= x]
orderChannelProperty (OBin bps) = [bin bps]
orderChannelProperty (OName s) = ["field" .= s]
orderChannelProperty (ORepeat arr) = ["field" .= object [repeat_ arr]]
orderChannelProperty (OSort ops) = [sort_ ops]
orderChannelProperty (OTimeUnit tu) = [timeUnit_ tu]
orderChannelProperty (OTitle s) = ["title" .= s]
orderChannelProperty ONoTitle = ["title" .= A.Null]
orderChannelProperty (OmType measure) = [mtype_ measure]
orderChannelProperty (ODataCondition tests elseClause) =
  dataCond_ orderChannelProperty tests elseClause
orderChannelProperty (OSelectionCondition selName ifClause elseClause) =
  selCond_ orderChannelProperty selName ifClause elseClause
orderChannelProperty (ONumber n) = ["value" .= n]

-- | Level of detail channel properties used for creating a grouped channel
--   encoding.

data DetailChannel
    = DName FieldName
      -- ^ The name of the field.
    | DmType Measurement
      -- ^ The measurement type of the field.
    | DBin [BinProperty]
      -- ^ How to convert discrete numeric values into bins.
    | DTimeUnit TimeUnit
      -- ^ The form of time unit aggregation.
    | DAggregate Operation
      -- ^ How should the detail field be aggregated?


detailChannelProperty :: DetailChannel -> Pair
detailChannelProperty (DName s) = "field" .= s  -- field_ s
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


facetMappingProperty :: FacetMapping -> Pair
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
        . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.Axis' [ 'Graphics.Vega.VegaLite.DomainWidth' 1 ])
        . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.ViewStyle' [ 'Graphics.Vega.VegaLite.ViewStroke' "transparent" ])
        . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.SelectionStyle' [ ( 'Graphics.Vega.VegaLite.Single', [ 'Graphics.Vega.VegaLite.On' \"dblclick\" ] ) ])
@
-}

configure ::
  [ConfigureSpec]
  -- ^ The configuration options, created with 'Graphics.Vega.VegaLite.configuration'.
  --
  --    Prior to version @0.5.0.0@ this was @['LabelledSpec']@.
  -> PropertySpec
configure configs = (VLConfig, toObject (map unCS configs))


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
See also 'hConcat' and 'vConcat'.

The number of columns in the flow layout can be set with 'columns'
and, if not specified, will default to a single row of unlimited columns.

@
let dvals = 'Graphics.Vega.VegaLite.dataSequenceAs' 0 6.28 0.1 \"x\"
    trans = 'transform'
              . 'calculateAs' \"sin(datum.x)\" \"sinX\"
              . 'calculateAs' \"cos(datum.x)\" \"cosX\"
    enc = 'encoding'
            . 'position' 'Graphics.Vega.VegaLite.X' ['PName' \"x\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative']
    encCos = enc . 'position' 'Graphics.Vega.VegaLite.Y' ['PName' \"cosX\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative']
    encSin = enc . 'position' 'Graphics.Vega.VegaLite.Y' ['PName' \"sinX\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative']

in toVegaLite [ dvals
              , trans []
              , 'vlConcat' [ 'Graphics.Vega.VegaLite.asSpec' [encCos [], 'mark' 'Graphics.Vega.VegaLite.Line' []]
                         , 'Graphics.Vega.VegaLite.asSpec' [encSin [], 'mark' 'Graphics.Vega.VegaLite.Line' []]
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
to apply to each of those facets using 'Graphics.Vega.VegaLite.asSpec'.

See the
<https://vega.github.io/vega-lite/docs/facet.html Vega-Lite documentation>
for further details.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ facet [ 'RowBy' [ 'FName' \"Month\", 'FmType' 'Graphics.Vega.VegaLite.Ordinal' ]
            , 'ColumnBy' [ 'FName' \"Week\", 'FmType' 'Graphics.Vega.VegaLite.Ordinal' ]
            ]
    , 'Graphics.Vega.VegaLite.specification' spec
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
define a full specification to apply to each of those facets using 'Graphics.Vega.VegaLite.asSpec'.

Small multiples will be laid out from left to right, moving on to new rows only
if the number of plots exceeds an optional column limit (specified via 'columns').

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ facetFlow [ 'FName' \"Origin\", 'FmType' 'Graphics.Vega.VegaLite.Nominal' ]
    , 'Graphics.Vega.VegaLite.specification' spec
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
setting, 'heightOfContainer' for setting the height to that of
the surrounding container,
and 'heightStep' for setting the height of discrete fields.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'height' 300
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
    , 'mark' 'Graphics.Vega.VegaLite.Bar' []
    , enc []
    ]
@
-}
height :: Double -> PropertySpec
height h = (VLHeight, toJSON h)


{-|
Set the height of the view to that of the surrounding container,
to allow for responsive sizing.

Please see the [Vega Lite responsive sizing](https://vega.github.io/vega-lite/docs/size.html#specifying-responsive-width-and-height)
documentation for caveats and limitations.

@since 0.5.0.0
-}
heightOfContainer :: PropertySpec
heightOfContainer = (VLHeight, fromT "container")


{-|

Set the height of the discrete y-field (e.g. individual bars in a
horizontal bar chart).
The total height is then calculated based on the number of discrete fields
(e.g. bars).

@
'Graphics.Vega.VegaLite.toVegaLite'
  [ 'heightStep' 17
  , data []
  , enc []
  , 'mark' 'Graphcs.Vega.VegaLite.Bar' []
  ]
@

This replaces the use of @SRangeStep@ from 'ScaleProperty'.

@since 0.5.0.0
-}

-- Note that unlike ELm, we do not create a separate property here
-- (ie no VLHeightStep)
--
heightStep :: Double -> PropertySpec
heightStep s = (VLHeight, object [ "step" .= s ])


{-|

Assigns a list of specifications to be juxtaposed horizontally in a visualization.
See also 'vConcat' and 'vlConcat'.

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
             . 'position' 'Graphics.Vega.VegaLite.X' ['PName' \"x\", 'PmType' 'Graphics.Vega.VegaLite.Ordinal']
             . 'position' 'Graphics.Vega.VegaLite.Y' ['PName' \"a\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative']
             . 'text' ['TName' \"a\", 'TmType' 'Graphics.Vega.VegaLite.Nominal']

    in 'Graphics.Vega.VegaLite.toVegaLite' [ dvals []
                  , enc []
                  , 'layer' [ 'Graphics.Vega.VegaLite.asSpec' ['mark' 'Graphics.Vega.VegaLite.Bar' []]
                          , 'Graphics.Vega.VegaLite.asSpec' ['mark' 'Graphics.Vega.VegaLite.Text' ['Graphics.Vega.VegaLite.MdY' (-8)]]
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
    , 'mark' 'Graphics.Vega.VegaLite.Bar' []
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
    , 'padding' ('Graphics.Vega.VegaLite.PEdges' 20 10 5 15)
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
    , 'mark' 'Graphics.Vega.VegaLite.Bar' []
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
each of those fields using 'Graphics.Vega.VegaLite.asSpec'.

Unlike __faceting__, which creates multiple charts based on different values of a
single field, __repeating__ uses a different field for each chart.

See the
<https://vega.github.io/vega-lite/docs/repeat.html Vega-Lite documentation>
for further details.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'repeat' ['Graphics.Vega.VegaLite.ColumnFields' [\"Cat\", \"Dog\", \"Fish\"]]
    , 'Graphics.Vega.VegaLite.specification' ('Graphics.Vega.VegaLite.asSpec' spec)
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
need to define a full specification to apply to each of those fields using 'Graphics.Vega.VegaLite.asSpec'.

Small multiples will be laid out from left to right, moving on to new rows only
if the number of plots exceeds an optional column limit (specified via 'columns').

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'repeatFlow' [ \"Cat\", \"Dog\", \"Fish\" ]
    , 'Graphics.Vega.VegaLite.specification' ('Graphics.Vega.VegaLite.asSpec' spec)
    ]
@

See also 'repeat'.

@since 0.4.0.0

-}
repeatFlow ::
  [FieldName]
  -> PropertySpec
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
            . 'resolution' ('Graphics.Vega.VegaLite.RLegend' [('Graphics.Vega.VegaLite.ChColor', 'Graphics.Vega.VegaLite.Independent')])

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
               . 'position' 'Graphics.Vega.VegaLite.X' ['PName' \"x\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative']
               . 'position' 'Graphics.Vega.VegaLite.Y' ['PName' \"a\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative']
    specBar = 'Graphics.Vega.VegaLite.asSpec' ['mark' 'Graphics.Vega.VegaLite.Bar' [], encBar []]
    encLine = 'encoding'
                . 'position' 'Graphics.Vega.VegaLite.X' ['PName' \"x\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative']
                . 'position' 'Graphics.Vega.VegaLite.Y' ['PName' \"b\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative']
    specLine = 'Graphics.Vega.VegaLite.asSpec' ['mark' 'Graphics.Vega.VegaLite.Line' ['Graphics.Vega.VegaLite.MColor' \"firebrick\"], encLine []]
    res = 'resolve'
            . 'resolution' ('Graphics.Vega.VegaLite.RScale' [('Graphics.Vega.VegaLite.ChY', 'Graphics.Vega.VegaLite.Independent')])

in 'Graphics.Vega.VegaLite.toVegaLite' [dvals [], res [], 'layer' [specBar, specLine]]
@

-}
resolve ::
  [ResolveSpec]
  -- ^ The arguments created by 'Graphics.Vega.VegaLite.resolution'.
  --
  --   Prior to @0.5.0.0@ this argument was @['LabelledSpec']@.
  -> PropertySpec
resolve res = (VLResolve, toObject (map unRS res))


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

The supported transformations are:
'aggregate', 'binAs', 'calculateAs', 'density', 'filter', 'flatten',
'flattenAs', 'fold', 'foldAs', 'impute', 'joinAggregate', 'loess',
'lookup', 'lookupAs', 'lookupSelection', 'pivot', 'quantile',
'regression', 'sample', 'stack', 'timeUnitAs', and 'window'.

-}

transform ::
  [TransformSpec]
  -- ^ The transformations to apply. The order does matter.
  --
  --   Prior to @0.5.0.0@ this argument was @['LabelledSpec']@.
  -> PropertySpec
transform transforms =
  let js = if null transforms then A.Null else toJSON (map unTS transforms)
  in (VLTransform, js)


{-|

Assigns a list of specifications to be juxtaposed vertically in a visualization.
See also 'hConcat' and 'vlConcat'.

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
setting, 'widthOfContainer' for setting the width to that of
the surrounding container,
and 'widthStep' for setting the width of discrete fields.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'width' 500
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
    , 'mark' 'Graphics.Vega.VegaLite.Bar' []
    , enc []
    ]
@
-}
width :: Double -> PropertySpec
width w = (VLWidth, toJSON w)


{-|
Set the width of the view to that of the surrounding container,
to allow for responsive sizing.

Please see the [Vega Lite responsive sizing](https://vega.github.io/vega-lite/docs/size.html#specifying-responsive-width-and-height)
documentation for caveats and limitations.

@since 0.5.0.0
-}
widthOfContainer :: PropertySpec
widthOfContainer = (VLWidth, fromT "container")


{-|

Set the width of the discrete x-field (e.g. individual bars in a bar chart).
The total width is then calculated based on the number of discrete fields
(e.g. bars).

@
'Graphics.Vega.VegaLite.toVegaLite'
  [ 'widthStep' 17
  , data []
  , enc []
  , 'mark' 'Graphcs.Vega.VegaLite.Bar' []
  ]
@

This replaces the use of @SRangeStep@ from 'ScaleProperty'.

@since 0.5.0.0
-}

-- Note that unlike ELm, we do not create a separate property here
-- (ie no VLWidthStep)
--
widthStep :: Double -> PropertySpec
widthStep s = (VLWidth, object [ "step" .= s ])


{-|

Defines a set of named aggregation transformations to be used when encoding
channels. This is useful when, for example, you wish to apply the same transformation
to a number of channels but do not want to define it each time. For further details
see the
<https://vega.github.io/vega-lite/docs/aggregate.html#aggregate-op-def Vega-Lite documentation>.

@
'transform'
    . 'aggregate'
        [ 'opAs' 'Graphics.Vega.VegaLite.Min' "people" "lowerBound"
        , 'opAs' 'Graphics.Vega.VegaLite.Max' "people" "upperBound" ]
        [ "age" ]
@

See also 'joinAggregate'.

-}
aggregate ::
  [VLSpec]
  -- ^ The named aggregation operations to apply.
  -> [FieldName]
  -- ^ The \"group by\" fields.
  -> BuildTransformSpecs
aggregate ops groups ols =
  let fields = [ "aggregate" .= ops
               , "groupby" .= groups ]
  in TS (object fields) : ols


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
        [ 'opAs' 'Graphics.Vega.VegaLite.Mean' "rating" "avYearRating" ]
        [ 'Graphics.Vega.VegaLite.WGroupBy' [ "year" ] ]
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
  -> BuildTransformSpecs
joinAggregate ops wProps ols = joinAggregateTS ops wProps : ols


{-|

Window transform for performing calculations over sorted groups of
data objects such as ranking, lead/lag analysis, running sums and averages.

@
'transform'
    . 'window' [ ( [ 'Graphics.Vega.VegaLite.WAggregateOp' 'Graphics.Vega.VegaLite.Sum', 'Graphics.Vega.VegaLite.WField' \"Time\" ], \"TotalTime\" ) ]
             [ 'Graphics.Vega.VegaLite.WFrame' Nothing Nothing ]
@

@since 0.4.0.0

-}
window ::
  [([Window], FieldName)]
  -- ^ The window-transform definition and associated output name.
  -> [WindowProperty]
  -- ^ The window transform.
  -> BuildTransformSpecs
window wss wProps ols = windowTS wss wProps : ols


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

sample :: Int -> BuildTransformSpecs
sample maxSize ols = TS (object [ "sample" .= maxSize ]) : ols


{-|

Configure the kernel density estimation process. Used by 'density'.

@since 0.5.0.0
-}

data DensityProperty
  = DnAs FieldName FieldName
    -- ^ Name the outputs of a density transform. The first argument is the
    --   name of the field containing the samples and the second the name
    --   for the field containing the density estimates.
    --
    --   The defaults are @\"value\"@ and @\"density\"@ respectively.
  | DnBandwidth Double
    -- ^ The bandwidth (standard deviation) of the Gaussian kernel to be
    --   used in the KDE. If not given, or set to 0, then
    --   [Scott's method](https://stats.stackexchange.com/questions/90656/kernel-bandwidth-scotts-vs-silvermans-rules)
    --   is used.
  | DnCounts Bool
    -- ^ If @'True'@ then the KDE generates counts, if @'False'@ it
    --   generates probabilities.
    --
    --   The default is probabilities.
  | DnCumulative Bool
    -- ^ Should the density estimates be cumulative?
    --
    --   The default is @'False'@.
  | DnExtent Double Double
    -- ^ The domain (minimum to maximum) from which to sample a distribution
    --   for the density estimation.
    --
    --   The default is to use the full extent of the input values.
  | DnGroupBy [FieldName]
    -- ^ The data fields to group by.
    --
    --   The default is to use a single group containing all the data objects.
  | DnMaxSteps Natural
    -- ^ The maximum number of samples to take from the extent domain.
    --
    --   The default is 200.
  | DnMinSteps Natural
    -- ^ The minimum number of samples to take from the extent domain.
    --
    --   The default is 25.
  | DnSteps Natural
    -- ^ This overrides the 'DnMinSteps' and 'DnMaxSteps' options and
    --   specified an exact number of steps to take from the extent
    --   domain.
    --
    --   It can be used with 'DnExtent' to ensure a consistent
    --   set of sample points for stacked densities.


data DensityPropertyLabel =
  DPLGroupby | DPLCumulative | DPLCounts | DPLBandwidth | DPLExtent |
  DPLMinsteps | DPLMaxsteps | DPLSteps | DPLAs


densityPropertySpec :: DensityPropertyLabel -> [DensityProperty] -> VLSpec
densityPropertySpec DPLGroupby ps =
  let wanted (DnGroupBy xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

densityPropertySpec DPLCumulative ps =
  let wanted (DnCumulative xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

densityPropertySpec DPLCounts ps =
  let wanted (DnCounts xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

densityPropertySpec DPLBandwidth ps =
  let wanted (DnBandwidth xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

densityPropertySpec DPLExtent ps =
  let wanted (DnExtent xs ys) = Just [xs, ys]
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

densityPropertySpec DPLMinsteps ps =
  let wanted (DnMinSteps xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

densityPropertySpec DPLMaxsteps ps =
  let wanted (DnMaxSteps xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

densityPropertySpec DPLSteps ps =
  let wanted (DnSteps xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

densityPropertySpec DPLAs ps =
  let wanted (DnAs xs ys) = Just [xs, ys]
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null



{-|

Apply /Kernel Density Estimation/ to a data stream to generate a new stream
of samples of the estimated density. This is useful for representing
probability distributions and generating continuous distributions from
discrete samples.

The following example creates a faceted display of the smoothed
length and width distributions from the iris dataset.

@
dvals = 'Graphics.Vega.VegaLite.dataFromUrl' \"https:\/\/vega.github.io\/vega-lite\/data\/iris.json" []

colNames = [ \"petalWidth\", \"petalLength\", \"sepalWidth\", \"sepalLength\" ]
trans = 'transform'
        . 'foldAs' colNames \"measurement\" \"value\"
        . 'density' \"value\" [ 'DnGroupBy' [ \"measurement\" ] ]

enc = 'encoding'
      . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"value\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
      . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"density\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
      . 'row' [ 'FName' \"measurement\", 'FmType' 'Graphics.Vega.VegaLite.Nominal' ]

layer = 'Graphics.Vega.VegaLite.asSpec' [ trans [], enc [], 'mark' 'Graphics.Vega.VegaLite.Area' [ 'Graphics.Vega.VegaLite.MOpacity' 0.7 ] ]
@

@since 0.5.0.0
-}

density ::
  FieldName
  -- ^ The field used for the KDE.
  -> [DensityProperty]
  -- ^ Configure the calculation.
  -> BuildTransformSpecs
density field dps ols =
  let addField n p = case densityPropertySpec p dps of
                       A.Null -> []
                       x -> [ n .= x ]

      ofields = [ "density" .= field ]
                <> addField "groupby" DPLGroupby
                <> addField "cumulative" DPLCumulative
                <> addField "counts" DPLCounts
                <> addField "bandwidth" DPLBandwidth
                <> addField "extent" DPLExtent
                <> addField "minsteps" DPLMinsteps
                <> addField "maxsteps" DPLMaxsteps
                <> addField "steps" DPLSteps
                <> addField "as" DPLAs

  in TS (object ofields) : ols


{-|

Configure the trend fitting used by the 'loess' encoding.

@since 0.5.0.0
-}
data LoessProperty
  = LsAs FieldName FieldName
    -- ^ Name the outputs of a loess transform. The first argument is the
    --   name of the field containing the smoothed independent variable
    --   and the second the name for the field containing the smoothed
    --   dependent variable.
    --
    --   If not specified the original field names will be used.
  | LsBandwidth Double
    -- ^ The amount of smoothing. The value should be in the range 0 to 1,
    --   inclusive.
    --
    --   The default is 0.3.
  | LsGroupBy [FieldName]
    -- ^ The data fields to group by.
    --
    --   The default is to use a single group containing all the data objects.


data LoessPropertyLabel = LLAs | LLBandwidth | LLGroupBy

loessPropertySpec :: LoessPropertyLabel -> [LoessProperty] -> VLSpec
loessPropertySpec LLAs ps =
  let wanted (LsAs xs ys) = Just [xs, ys]
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

loessPropertySpec LLBandwidth ps =
  let wanted (LsBandwidth xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

loessPropertySpec LLGroupBy ps =
  let wanted (LsGroupBy xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null


{-|

Generate a /loess/ (locally-estimated scatterplot smoothing) trendline
through a pair of data fields.

See also 'regression'.

The following example overlays the trendline generated by 'loess'
(the \"xsm\", \"ysm\" points) on the raw points (assuming the data
source has fields called \"xraw\" and \"yraw\" for the independent
and dependent fields, respectively).

@
transLS = 'transform'
          . 'loess' \"yraw\" \"xraw\" [ 'LsAs' \"xsm\" \"ysm\" ]

encRaw = 'encoding'
         . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"xraw\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
         . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"yraw\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]

encLS = 'encoding'
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"xsm\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"ysm\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]

layers = 'layer' [ 'Graphics.Vega.VegaLite.asSpec' [ encRaw [], 'mark' 'Graphics.Vega.VegaLite.Point' [ 'Graphics.Vega.VegaLite.MOpacity' 0.5 ] ]
               , 'Graphics.Vega.VegaLite.asSpec' [ transLS [], encLS [], 'mark' 'Graphics.Vega.VegaLite.Line' [ 'Graphics.Vega.VegaLite.MColor' \"firebrick\" ] ]
               ]
@

@since 0.5.0.0
-}

loess ::
  FieldName
  -- ^ The field representing the dependent variable (often displayed on
  --   the y axis).
  -> FieldName
  -- ^ The field representing the independent variable (often the x axis).
  -> [LoessProperty]
  -- ^ Customize the trend fitting.
  -> BuildTransformSpecs
loess depField indField lsp ols =
  let addField n p = case loessPropertySpec p lsp of
                       A.Null -> []
                       x -> [ n .= x ]

      ofields = [ "loess" .= depField
                , "on" .= indField ]
                <> addField "groupby" LLGroupBy
                <> addField "bandwidth" LLBandwidth
                <> addField "as" LLAs

  in TS (object ofields) : ols


{-|

The functional form of the regression analysis. Used by 'RgMethod'.

@since 0.5.0.0
-}
data RegressionMethod
  = RgLinear
    -- ^ Linear regression.
  | RgLog
    -- ^ Logarithmic regression.
  | RgExp
    -- ^ Exponential regression.
  | RgPow
    -- ^ Power regression.
  | RgQuad
    -- ^ Quadratic regression.
  | RgPoly
    -- ^ A polynomial. The order to use is given by the 'RgOrder'
    --   constructor, and defaults to 3.

regressionMethodSpec :: RegressionMethod -> VLSpec
regressionMethodSpec RgLinear = fromT "linear"
regressionMethodSpec RgLog = fromT "log"
regressionMethodSpec RgExp = fromT "exp"
regressionMethodSpec RgPow = fromT "pow"
regressionMethodSpec RgQuad = fromT "quad"
regressionMethodSpec RgPoly = fromT "poly"


{-|

Configure the regression process (used by 'regression').

@since 0.5.0.0
-}

data RegressionProperty
  = RgAs FieldName FieldName
    -- ^ Name the outputs of the regression analysis. The first argument is the
    --   name of the field containing the independent variable, the second
    --   the dependent variable.
    --
    --   If not specified the original field names will be used.
  | RgExtent Double Double
    -- ^ The domain (minimum to maximum) over which to estimate the dependent
    --   variable in the regression.
    --
    --   The default is to use the full extent of the input values.
  | RgGroupBy [FieldName]
    -- ^ The data fields to group by.
    --
    --   The default is to use a single group containing all the data objects.
  | RgMethod RegressionMethod
    -- ^ The type of regression model to use.
  | RgOrder Natural
    -- ^ The order of the polynomial model.
    --
    --   This is only used if @'RgMethod' 'RgPoly'@ is set.
  | RgParams Bool
    -- ^ Should the transform return the regression model parameters, one object
    --   per group, rather than the trend line points.
    --
    --   If set, the returned objects include a @\"coef\"@ array of fitted
    --   coefficient values, starting with the intercept term and then including
    --   terms of increasing order, and a @\"rSquared\"@ value, indicating
    --   the total variance explained by the model.
    --
    --   The default is @'False'@.


data RegressionPropertyLabel =
  RPLAs | RPLExtent | RPLGroupBy | RPLMethod | RPLOrder | RPLParams


regressionPropertySpec :: RegressionPropertyLabel -> [RegressionProperty] -> VLSpec
regressionPropertySpec RPLAs ps =
  let wanted (RgAs xs ys) = Just [xs, ys]
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

regressionPropertySpec RPLExtent ps =
  let wanted (RgExtent xs ys) = Just [xs, ys]
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

regressionPropertySpec RPLGroupBy ps =
  let wanted (RgGroupBy xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

regressionPropertySpec RPLMethod ps =
  let wanted (RgMethod xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> regressionMethodSpec x
    _ -> A.Null

regressionPropertySpec RPLOrder ps =
  let wanted (RgOrder xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

regressionPropertySpec RPLParams ps =
  let wanted (RgParams xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null


{-|

Generate a 2d regression model for smoothing and predicting data.

See also 'loess'.

The following example overlays the points generated by 'regression'
(the \"xrg\", \"yrg\" points) on the raw points (assuming the data
source has fields called \"xraw\" and \"yraw\" for the independent
and dependent fields, respectively).

@
transLS = 'transform'
          . 'regression' \"yraw\" \"xraw\" [ 'RgAs' \"xrg\" \"yrg\" ]

encRaw = 'encoding'
         . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"xraw\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
         . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"yraw\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]

encLS = 'encoding'
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"xrg\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"yrg\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]

layers = 'layer' [ 'Graphics.Vega.VegaLite.asSpec' [ encRaw [], 'mark' 'Graphics.Vega.VegaLite.Point' [ 'Graphics.Vega.VegaLite.MOpacity' 0.5 ] ]
               , 'Graphics.Vega.VegaLite.asSpec' [ transLS [], encLS [], 'mark' 'Graphics.Vega.VegaLite.Line' [ 'Graphics.Vega.VegaLite.MColor' \"firebrick\" ] ]
               ]
@

@since 0.5.0.0
-}

regression ::
  FieldName
  -- ^ The field representing the dependent variable (often displayed on
  --   the y axis).
  -> FieldName
  -- ^ The field representing the independent variable (often the x axis).
  -> [RegressionProperty]
  -- ^ Customize the regression.
  -> BuildTransformSpecs
regression depField indField rps ols =
  let addField n p = case regressionPropertySpec p rps of
                       A.Null -> []
                       x -> [ n .= x ]

      ofields = [ "regression" .= depField
                , "on" .= indField ]
                <> addField "groupby" RPLGroupBy
                <> addField "method" RPLMethod
                <> addField "order" RPLOrder
                <> addField "extent" RPLExtent
                <> addField "params" RPLParams
                <> addField "as" RPLAs

  in TS (object ofields) : ols


{-|
Configure the quantile analysis performed by 'quantile'.

@since 0.5.0.0
-}
data QuantileProperty
  = QtAs FieldName FieldName
    -- ^ Name the fields used to store the calculated probability and
    --   associated quantile values.
    --
    --   The defaults are @\"prob\"@ and @\"value\"@.
  | QtGroupBy [FieldName]
    -- ^ The data fields to group by.
    --
    -- The default is to use a single group containing all the data objects.
  | QtProbs [Double]
    -- ^ The probabilites (measured in the range 0-1) for which to
    --   compute quantile values.
    --
    --   The default is to use a step size of 0.01, or the
    --   'QtStep' value if given.
  | QtStep Double
    -- ^ The interval between probabilities when performing a quantile
    --   transformation.
    --
    --   All value from half the given step size to 1 will be sampled,
    --   and is only used if 'QtProbs' is not set.


data QuantilePropertyLabel =
  QPLAs | QPLGroupBy | QPLProbs | QPLStep


quantilePropertySpec :: QuantilePropertyLabel -> [QuantileProperty] -> VLSpec
quantilePropertySpec QPLAs ps =
  let wanted (QtAs xs ys) = Just [xs, ys]
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

quantilePropertySpec QPLGroupBy ps =
  let wanted (QtGroupBy xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

quantilePropertySpec QPLProbs ps =
  let wanted (QtProbs xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

quantilePropertySpec QPLStep ps =
  let wanted (QtStep xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null


{-|
Calculate quantile values from an input data stream. This can be useful
for examining distributional properties of a data stream, and for
creating
<https://en.wikipedia.org/wiki/Q–Q_plot Q-Q plots>.

As an example:

@
let dvals = 'Graphics.Vega.VegaLite.dataFromUrl' \"data/normal-2d.json\" []

    trans = 'transform'
            . 'quantile' \"u\" [ 'QtStep' 0.01, 'QtAs' \"p\" \"v\" ]
            . 'calculateAs' \"quantileUniform(datum.p)\" \"unif\"
            . 'calculateAs' \"quantileNormal(datum.p)\" \"norm\"

    enc x y = 'encoding'
              . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' x, 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
              . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' y, 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]

    leftSpec = 'Graphics.Vega.VegaLite.asSpec' [ 'mark' 'Graphics.Vega.VegaLite.Point' [], enc \"unif\" \"v\" [] ]
    rightSpec = 'Graphics.Vega.VegaLite.asSpec' [ 'mark' 'Graphics.Vega.VegaLite.Point' [], enc \"norm\" \"v\" [] ]

in 'Graphics.Vega.VegaLite.toVegaLite' [ dvals, trans [], 'hConcat' [ leftSpec, rightSpec ] ]
@

@since 0.5.0.0
-}
quantile ::
  FieldName
  -- ^ The field to analyse.
  -> [QuantileProperty]
  -- ^ Configure the quantile analysis
  -> BuildTransformSpecs
quantile field qps ols =
  let addField n p = case quantilePropertySpec p qps of
                       A.Null -> []
                       x -> [ n .= x ]

      ofields = [ "quantile" .= field ]
                <> addField "groupby" QPLGroupBy
                <> addField "probs" QPLProbs
                <> addField "step" QPLStep
                <> addField "as" QPLAs

  in TS (object ofields) : ols


{-|

Create a named binning transformation that may be referenced in other Transformations
or encodings. See the
<https://vega.github.io/vega-lite/docs/bin.html Vega-Lite documentation> for
more details. Note that usually, direct binning within an encoding is preferred
over this form of bin transformation.

@
'transform'
    . 'binAs' [ 'Graphics.Vega.VegaLite.MaxBins' 3 ] \"IMDB_Rating\" \"ratingGroup\"
@
-}
binAs ::
  [BinProperty]
  -- ^ An empty list means that the default binning is used (that is, the
  --   @bin@ field will be set to @true@ in the Vega-Lite specification).
  -> FieldName
  -- ^ The field to bin.
  -> FieldName
  -- ^ The label for the binned data.
  -> BuildTransformSpecs
binAs bProps field label ols =
  let fields = [ "bin" .= if null bProps then toJSON True else binObj
               , "field" .= field
               , "as" .= label ]

      binObj = object (map binProperty bProps)

 in TS (object fields) : ols


{-|

Creates a new data field based on calculations from existing fields and values.

See the <https://vega.github.io/vega-lite/docs/calculate.html Vega-Lite documentation>
for further details.

@
'transform' . 'calculateAs' "datum.sex == 2 ? \'F\' : \'M\'" "gender"
@
-}
calculateAs ::
  VegaExpr
  -- ^ The calculation to perform.
  -> FieldName
  -- ^ The field to assign the new values.
  -> BuildTransformSpecs
calculateAs expr label ols =
  let fields = [ "calculate" .= expr, "as" .= label ]
  in TS (object fields) : ols


{-|

Encode an angle (orientation) channel, which allows for data-driven
rotation of text, point, and square marks.

@since 0.9.0.0
-}
angle ::
  [MarkChannel]
  -- ^ The color-encoding options.
  -> BuildEncodingSpecs
angle markProps ols = mchan_ "angle" markProps : ols


{-|

Encode a color channel.

@
'color' [ 'MName' \"Species\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ] []
@

Encoding a color channel will generate a legend by default. To stop the legend
appearing, just supply an empty list of legend properties to 'MLegend':

@
'color' [ 'MName' \"Species\", 'MmType' 'Graphics.Vega.VegaLite.Nominal', 'MLegend' [] ] []
@
-}
color ::
  [MarkChannel]
  -- ^ The color-encoding options.
  -> BuildEncodingSpecs
color markProps ols = mchan_ "color" markProps : ols


{-|

Encodes a new facet to be arranged in columns. See the
<https://vega.github.io/vega-lite/docs/facet.html#facet-row-and-column-encoding-channels Vega-Lite column documentation>.

Note that when faceting, dimensions specified with 'width' and 'height'
refer to the individual faceted plots, not the overall visualization.

@
let dvals = 'Graphics.Vega.VegaLite.dataFromUrl' \"crimeData.csv\"
    enc = 'encoding'
            . 'position' 'Graphics.Vega.VegaLite.X' ['PName' \"month\", 'PmType' 'Graphics.Vega.VegaLite.Temporal']
            . 'position' 'Graphics.Vega.VegaLite.Y' ['PName' \"reportedCrimes\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative'
                         , 'PAggregate' 'Graphics.Vega.VegaLite.Sum']
            . 'column' ['FName' \"crimeType\", 'FmType' 'Graphics.Vega.VegaLite.Nominal']

    in 'Graphics.Vega.VegaLite.toVegaLite' ['width' 100, dvals [], 'mark' 'Graphics.Vega.VegaLite.Bar' [], enc [] ]
@
-}
column ::
  [FacetChannel]
  -- ^ The list of properties that define the faceting channel. At a minimum
  --   this should include the data field ('FName') and its measurement type
  --   ('FmType').
  -> BuildEncodingSpecs
column fFields ols =
  ES ("column", object (map facetChannelProperty fFields)) : ols


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
'detail' ['DName' \"Species\", 'DmType' 'Graphics.Vega.VegaLite.Nominal'] []
@
-}
detail ::
  [DetailChannel]
  -- ^ The field to group.
  -> BuildEncodingSpecs
detail detailProps ols =
    ES ("detail", object (map detailChannelProperty detailProps)) : ols


{-

Elm added this in version 2.0, but I think it needs more structure
than just a field name, so am leaving out for now.

Encode a key channel, to support dynamic data via the
<https://vega.github.io/vega/docs/api/view/ Vega View API>.

See the <https://vega.github.io/vega-lite/docs/encoding.html#key Vega-Lite documentation>
for more information.

@
'encoding' . 'keyChannel' \"Species\"
@

@since 0.5.0.0
keyChannel ::
  FieldName
  -- ^ The field to use as the unique key for data binding.
  -> BuildLabelledSpecs
keyChannel f ols =
    ("key" .= object ["field" .= f]) : ols
    -- ("key" .= f) : ols

-}

{-|

Encode a fill channel. This acts in a similar way to encoding by 'color' but
only affects the interior of closed shapes.

@
'fill' [ 'MName' \"Species\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ] []
@

Note that if both @fill@ and 'color' encodings are specified, @fill@ takes precedence.

-}

fill ::
  [MarkChannel]
  -- ^ Configure the fill.
  -> BuildEncodingSpecs
fill markProps ols = mchan_ "fill" markProps : ols


{-|

Encode a fill opacity channel. This acts in a similar way to encoding by 'opacity'
but only affects the interior of closed shapes. If both @fillOpacity@ and 'opacity'
encodings are specified, @fillOpacity@ takes precedence.

See also 'fill'.

@since 0.4.0.0
-}

fillOpacity :: [MarkChannel] -> BuildEncodingSpecs
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

The [Vega expression documentation](https://vega.github.io/vega/docs/expressions/)
describes the supported format (e.g. the requirement to precede column names
with @"datum."@).

-}
filter :: Filter -> BuildTransformSpecs
filter f ols = TS (object [ "filter" .= filterSpec f ]) : ols



{-|

Map array-valued fields to a set of individual data objects, one per array entry.

See also 'flattenAs'.

@since 0.4.0.0

-}

flatten :: [FieldName] -> BuildTransformSpecs
flatten fields ols = TS (object [ "flatten" .= fields ]) : ols


{-|

Similar to 'flatten' but allows the new output fields to be named.

@since 0.4.0.0

-}

flattenAs ::
  [FieldName]
  -> [FieldName]
  -- ^ The names of the output fields.
  -> BuildTransformSpecs
flattenAs fields names ols =
  let ofields = [ "flatten" .= fields, "as" .= names ]
  in TS (object ofields) : ols


{-|

Perform a /gather/ operation to /tidy/ a table. Collapse multiple data fields
into two new data fields: @key@ containing the original data field names and
@value@ containing the corresponding data values.

It is the inverse of 'pivot'. See also 'foldAs'.

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
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"key\", 'PmType' 'Graphics.Vega.VegaLite.Nominal' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"city\", 'PmType' 'Graphics.Vega.VegaLite.Nominal' ]
        . 'size' [ 'MName' \"value\", 'MmType' 'Graphics.Vega.VegaLite.Quantitative' ]
@

@since 0.4.0.0
-}

fold ::
  [FieldName]
  -- ^ The data fields to fold.
  -> BuildTransformSpecs
fold fields ols = TS (object [ "fold" .= fields ]) : ols


{-|

A 'fold' where the @key@ and @value@ fields can be renamed.

@since 0.4.0.0

-}

foldAs ::
  [FieldName]
  -- ^ The data fields to fold.
  -> FieldName
  -- ^ The name for the @key@ field.
  -> FieldName
  -- ^ The name for the @value@ field.
  -> BuildTransformSpecs
foldAs fields keyName valName ols =
  let ofields = [ "fold" .= fields
                , "as" .= [ keyName, valName ]
                ]
  in TS (object ofields) : ols


{-|

Perform a /pivot/ operation on a table. Spreads a key-value pair of fields
across multiple fields according to the data in the /key/ field.

It is the inverse of 'fold'.

@
dvals =
    'Graphics.Vega.VegaLite.dataFromColumns' []
        . 'Graphics.Vega.VegaLite.dataColumn' \"city\" ('Strings' [ \"Bristol\", \"Bristol\", \"Sheffield\", \"Sheffield\", \"Glasgow\", \"Glasgow\" ])
        . 'Graphics.Vega.VegaLite.dataColumn' \"temperature\" ('Numbers' [ 12, 14, 11, 13, 7, 10 ])
        . 'Graphics.Vega.VegaLite.dataColumn' \"year\" ('Numbers' [ 2017, 2018, 2017, 2018, 2017, 2018 ])

trans =
    'transform'
        . 'pivot' "year" "temperature" [ 'PiGroupBy' [ \"city\" ] ]

enc =
    'encoding'
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"2017\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"city\", 'PmType' 'Graphics.Vega.VegaLite.Nominal' ]
@

@since 0.5.0.0
-}

pivot ::
  FieldName
  -- ^ The key field.
  -> FieldName
  -- ^ The value field.
  -> [PivotProperty]
  -> BuildTransformSpecs
pivot field valField pProps ols =
  let addField n p = case pivotPropertySpec p pProps of
                       A.Null -> []
                       x -> [n .= x]

      ofields = [ "pivot" .= field
                , "value" .= valField ]
                <> addField "groupby" PPLGroupBy
                <> addField "limit" PPLLimit
                <> addField "op" PPLOp

  in TS (object ofields) : ols


{-|
Configure the 'pivot' operation.

@since 0.5.0.0
-}

data PivotProperty
  = PiGroupBy [FieldName]
    -- ^ The data fields to group by when pivoting. If unspecified
    --   then a single group containing all the data objects will
    --   be used.
  | PiLimit Natural
    -- ^ The maximum number of fields to generate when pivoting. If
    --   0 or unspecified all fields are pivoted. The pivot names
    --   are sorted into ascending order before the limit is
    --   applied.
  | PiOp Operation
    -- ^ The aggregation operation to apply to grouped fields.


data PivotPropertyLabel = PPLGroupBy | PPLLimit | PPLOp

-- Multiple properties will lead to no output; in some ways
-- this makes sense (aka "you are telling me multiple things,
-- so I give up") and is used elsewhere.
--
-- TODO: this should return a Maybe VLSpec
pivotPropertySpec ::
  PivotPropertyLabel
  -> [PivotProperty]
  -> VLSpec
pivotPropertySpec PPLGroupBy ps =
  let wanted (PiGroupBy xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

pivotPropertySpec PPLLimit ps =
  let wanted (PiLimit xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> toJSON x
    _ -> A.Null

pivotPropertySpec PPLOp ps =
  let wanted (PiOp xs) = Just xs
      wanted _ = Nothing

  in case mapMaybe wanted ps of
    [x] -> operationSpec x
    _ -> A.Null


{-|

Encode a URL for use with the 'Graphics.Vega.VegaLite.Image' mark type.

The URL can be encoded directly:

@
let axVals = 'Numbers' [ 0.5, 1.5, 2.5 ]

    dvals = 'Graphics.Vega.VegaLite.dataFromColumns' []
            . 'Graphics.Vega.VegaLite.dataColumn' "x" axVals
            . 'Graphics.Vega.VegaLite.dataColumn' "y" axVals

    enc = 'encoding'
          . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' "x", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
          . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' "y", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
          . 'url' [ 'HString' \"wonderful-image.png\" ]

    imMark = 'mark' 'Graphics.Vega.VegaLite.Image' [ 'Graphics.Vega.VegaLite.MWidth' 50, 'Graphics.Vega.VegaLite.MHeight' 25 ]

in 'Graphics.Vega.VegaLite.toVegaLite' [ dvals [], enc [], imMark ]
@

or by referencing a data field containing the URL values:

@
... 'Graphics.Vega.VegaLite.dataColumn' "img" ('Strings' [ \"i1.png\", \"i2.png\", \"i4.png\" ])

... 'url' [ 'HName' \"img\", 'HmType' 'Graphics.Vega.VegaLite.Nominal' ]
@

@since 0.5.0.0
-}

url :: [HyperlinkChannel] -> BuildEncodingSpecs
url hPs ols =
  ES ("url", object (concatMap hyperlinkChannelProperty hPs)) : ols


{-|

Encode a hyperlink channel.

@
'encoding'
  . 'hyperlink' [ 'HName' \"Species\", 'HmType' 'Graphics.Vega.VegaLite.Nominal' ]
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
  -> BuildEncodingSpecs
hyperlink hyperProps ols =
  ES ("href", object (concatMap hyperlinkChannelProperty hyperProps)) : ols


{-|

Perform a lookup of named fields between two data sources. This allows
you to find values in one data source based on the values in another
(like a relational join).

Use 'lookupSelection' for linking data with interactive selections.

See the <https://vega.github.io/vega-lite/docs/lookup.html Vega-Lite documentation>
for further details.

The following would return the values in the @age@ and @height@ fields from
@lookup_people.csv@ for all rows where the value in the @name@ column in that
file matches the value of @person@ in the primary data source.

@
peopleData = 'Graphics.Vega.VegaLite.dataFromUrl' \"data/lookup_people.csv\" []
lfields = 'LuFields' [\"age\", \"height\"]
trans = 'transform'
          . 'lookup' \"person\" peopleData \"name\" lfields
@

Note that the interface has changed in version @0.5.0.0@: the
output field names argument now uses the new 'LookupFields'
type. This provides greater flexibility in naming and
default behaviour. The conversion from version 0.4 is
simple: change

@
lookup key1 dataSource key2 fields
@

to

@
lookup key1 dataSource key2 (LuFields fields)
@

-}
lookup ::
  FieldName
  -- ^ The field in the primary data structure acting as the key.
  -> Data
  -- ^ The secondary data source (e.g. the return from the data-generating
  --   functions such as 'Graphics.Vega.VegaLite.dataFromUrl').
  -> FieldName
  -- ^ The name of the field in the secondary data source to match against
  --   the primary key.
  -> LookupFields
  -- ^ The list of fields to store when the keys match.
  --
  --   This was changed from @[T.Text]@ in vesion 0.5.0.0.
  -> BuildTransformSpecs
lookup key1 (_, spec) key2 lfields ols =
  let get1 = jj . map fst
      get2 = jj . map snd

      jj :: A.ToJSON a => a -> Maybe A.Value
      jj = Just . toJSON

      res = case lfields of
             LuFields fs -> ( jj fs, Nothing, Nothing )
             LuFieldAs fas -> ( get1 fas, get2 fas, Nothing )
             LuAs s -> ( Nothing, jj s, Nothing )
             LuFieldsWithDefault fs def
               -> ( jj fs, Nothing , jj def )
             LuFieldsAsWithDefault fas def
               -> ( get1 fas, get2 fas, jj def )
             LuAsWithDefault s def -> ( Nothing, jj s, jj def )

      (mfields, mas, mdefault) = res

      addField n (Just x) = [ (n, x) ]
      addField _ _ = []

      fromFields = [ "data" .= spec
                   , "key" .= key2
                   ]
                   <> addField "fields" mfields

      ofields = [ "lookup" .= key1
                , "from" .= object fromFields
                ]
                <> addField "as" mas
                <> addField "default" mdefault

  in TS (object ofields) : ols


{-|

Attach the results of an interactive selection to a primary data source.
This is similar to 'lookup' except that the data in a selection are used
in place of the secondary data source.

See the [Vega Lite lookup selection](https://vega.github.io/vega-lite/docs/lookup.html#lookup-selection) documentation.

@
sel = 'Graphics.Vega.VegaLite.selection'
      . 'Graphics.Vega.VegaLite.select' \"mySel\" 'Graphics.Vega.VegaLite.Single' [ 'Graphics.Vega.VegaLite.On' \"mouseover\", 'Graphics.Vega.VegaLite.Encodings' [ 'Graphics.Vega.VegaLite.ChX' ] ]

trans = 'transform'
        . 'lookupSelection' \"country\" \"mySel\" \"country\"
@

@since 0.5.0.0
-}

lookupSelection ::
  FieldName
  -- ^ The field to lookup in the primary data source.
  -> SelectionLabel
  -- ^ The name of the selection (as set with 'Graphics.Vega.VegaLite.select').
  -> FieldName
  -- ^ The name of the field in the selection to link with the
  --   primary data field.
  -> BuildTransformSpecs
lookupSelection key1 selName key2 ols =
  let ofields = [ "lookup" .= key1
                , "from" .= object [ "selection" .= selName
                                   , "key" .= key2 ]
                ]

  in TS (object ofields) : ols


{-|
Configure the field selection in 'lookup'.

@since 0.5.0.0
-}
data LookupFields
  = LuFields [FieldName]
    -- ^ The name of the fields to return from the secondary data
    --   source.
  | LuFieldAs [(FieldName, FieldName)]
    -- ^ Select fields from the secondary data source (first
    --   argument) and allow them to be referred to with a
    --   new name (second argument).
  | LuAs FieldName
    -- ^ Create a single name for all the fields in the
    --   secondary data source. The individual fields use dot
    --   notation to combine the given name with the field name.
    --
    --   @
    --   dvals = 'Graphics.Vega.VegaLite.dataFromUrl' \"data/flights.airport.csv" []
    --   trans = 'transform'
    --           . 'lookup' \"origin\" dvals "iata" ('LuAs' \"o\")
    --   enc = 'encoding'
    --         . 'position' 'Graphics.Vega.VegaLite.Longitude' [ 'PName' \"o.longitude\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    --         . 'position' 'Graphics.Vega.VegaLite.Lattude' [ 'PName' \"o.latitude\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    --   @
  | LuFieldsWithDefault [FieldName] T.Text
    -- ^ The name of the fields to return from the secondary
    --   data source, along with the default value to use
    --   if the lookup fails.
  | LuFieldsAsWithDefault [(FieldName, FieldName)] T.Text
    -- ^ Allow fields to be renamed and provide a default for
    --   when the lookup fails.
  | LuAsWithDefault FieldName T.Text
    -- ^ Create a single name for all the fields in the
    --   secondary data source, but the second parameter
    --   gives the default value for when the lookup fails.


{-|

This routine is deprecated (as of version @0.5.0.0@) in favor
of 'lookup', as

@
lookupAs "key1" dataSource "key2" "matchName"
@

can be written as

@
lookup "key1" dataSource "key2" (LuAs "matchName")
@

-}
{-# DEPRECATED lookupAs "Please change 'lookupAs ... alias' to 'lookup ... (LuAs alias)'" #-}
lookupAs ::
  FieldName
  -- ^ The field in the primary data structure acting as the key.
  -> Data
  -- ^ The secondary data source (e.g. the return from the data-generating
  --   functions such as 'Graphics.Vega.VegaLite.dataFromUrl').
  -> FieldName
  -- ^ The name of the field in the secondary data source to match against
  --   the primary key.
  -> FieldName
  -- ^ The field name for the new data.
  -> BuildTransformSpecs
lookupAs key1 sData key2 asName =
  lookup key1 sData key2 (LuAs asName)


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
              . 'impute' "b" "a" ['Graphics.Vega.VegaLite.ImMethod' 'Graphics.Vega.VegaLite.ImMean', 'Graphics.Vega.VegaLite.ImGroupBy' ["c"]]

    enc = 'encoding'
            . 'position' 'Graphics.Vega.VegaLite.X' ['PName' \"a\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative']
            . 'position' 'Graphics.Vega.VegaLite.Y' ['PName' \"b\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative']
            . 'color' ['MName' \"c\", 'MmType' 'Graphics.Vega.VegaLite.Nominal']

    in 'Graphics.Vega.VegaLite.toVegaLite' [dvals [], trans [], enc [], 'mark' 'Graphics.Vega.VegaLite.Line' []]
@

@since 0.4.0.0
-}

impute ::
  FieldName
  -- ^ The data field to process.
  -> FieldName
  -- ^ The key field to uniquely identify data objects within a group.
  -> [ImputeProperty]
  -- ^ Define how the imputation works.
  -> BuildTransformSpecs
impute fields keyField imProps ols = imputeTS fields keyField imProps : ols


{-|

Encode an opacity channel. The first parameter is a list of mark
channel properties that characterise the way a data field is encoded
by opacity. The second parameter is a list of any previous channels to
which this opacity channel should be added.

@
'opacity' [ 'MName' \"Age\", 'MmType' 'Graphics.Vega.VegaLite.Quantitative' ] []
@

See also 'fillOpacity'.

-}

opacity :: [MarkChannel] -> BuildEncodingSpecs
opacity markProps ols = mchan_ "opacity" markProps : ols


{-|

Encode an order channel.

@
'encoding'
    . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' "miles", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' "gas", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    . 'order' [ 'OName' "year", 'OmType' 'Graphics.Vega.VegaLite.Temporal', 'OSort' ['Descending'] ]
@

<https://vega.github.io/vega-lite/docs/condition.html Conditional values>
can be set with 'OSelectionCondition', such as

@
'order' [ 'OSelectionCondition' ('SelectionName "highlight")
          ['ONumber' 1] ['ONumber' 0]
@
-}

order ::
  [OrderChannel]
  -- ^ The order-encoding options.
  -> BuildEncodingSpecs
order oDefs ols =
  ES ("order", object (concatMap orderChannelProperty oDefs)) : ols


{-|

Encode a position channel.

@
enc =
    'encoding'
      . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"Animal\", 'PmType' 'Graphics.Vega.VegaLite.Ordinal' ]
@

Encoding by position will generate an axis by default. To prevent the axis from
appearing, simply provide an empty list of axis properties to 'PAxis':

@
enc =
    'encoding'
      . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"Animal\", 'PmType' 'Graphics.Vega.VegaLite.Ordinal', 'PAxis' [] ]
@
-}
position ::
  Position
  -- ^ The channel to encode.
  -> [PositionChannel]
  -- ^ The options for the channel; this will usually include the name ('PName')
  --    and measurement type ('PmType'), but can be a reference to a row or
  --    column repeat field.
  -> BuildEncodingSpecs
position pos pDefs ols =
  let defs = object (map positionChannelProperty pDefs)
  in ES (positionLabel pos, defs) : ols


{-|

Define a single resolution option to be applied when scales, axes or legends
in composite views share channel encodings. This allows, for example, two different
color encodings to be created in a layered view, which otherwise by default would
share color channels between layers. Each resolution rule should be in a tuple
pairing the channel to which it applies and the rule type.

@
'resolve'
    . resolution ('Graphics.Vega.VegaLite.RScale' [ ( 'Graphics.Vega.VegaLite.ChY', 'Graphics.Vega.VegaLite.Independent' ) ])
@
-}
resolution ::
  Resolve
  -> BuildResolveSpecs
  -- ^ Prior to @0.5.0.0@ this was @BuildLabelledSpecs@.
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
            . 'position' 'Graphics.Vega.VegaLite.X' ['PName' \"month\", 'PmType' 'Graphics.Vega.VegaLite.Temporal']
            . 'position' 'Graphics.Vega.VegaLite.Y' ['PName' \"reportedCrimes\"
                         , 'PmType' 'Graphics.Vega.VegaLite.Quantitative'
                         , 'PAggregate' 'Graphics.Vega.VegaLite.Sum'
                         , 'PAxis' ['AxNoTitle']
                         ]
            . 'row' ['FName' \"crimeType\", 'FmType' 'Graphics.Vega.VegaLite.Nominal']

in 'Graphics.Vega.VegaLite.toVegaLite' ['height' 80, dvals [], 'mark' 'Graphics.Vega.VegaLite.Bar' [], enc []]
@

-}
row ::
  [FacetChannel]
  -- ^ The facet properties for the channel; this should include the name of
  --   the field ('FName') and its measurement type ('FmType').
  -> BuildEncodingSpecs
row fFields ols = ES ("row", object (map facetChannelProperty fFields)) : ols


{-|

Encode a shape channel.

@
'shape' [ 'MName' \"Species\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ] []
@
-}
shape ::
  [MarkChannel]
  -- ^ What data values are used to control the shape parameters of the mark.
  -> BuildEncodingSpecs
shape markProps ols = mchan_ "shape" markProps : ols


{-|

Encode a size channel.

@
'size' [ 'MName' \"Age\", 'MmType' 'Graphics.Vega.VegaLite.Quantitative' ] []
@
-}
size ::
  [MarkChannel]
  -- ^ What data values are used to control the size parameters of the mark.
  -> BuildEncodingSpecs
size markProps ols = mchan_ "size" markProps : ols


{-|

Encode a stroke channel. This acts in a similar way to encoding by 'color' but
only affects the exterior boundary of marks.

@
'stroke' [ 'MName' \"Species\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ] []
@

Note that if both @stroke@ and 'color' encodings are specified, @stroke@ takes
precedence.

-}
stroke ::
  [MarkChannel]
  -- ^ What data values are used to control the stoke parameters of the mark.
  -> BuildEncodingSpecs
stroke markProps ols = mchan_ "stroke" markProps : ols


{-|

Encode a stroke-dash channel.

The following will use a different dash style for each value in the
\"symbol" field (a multi-series line chart):

@
'Graphics.Vega.VegaLite.toVegaLite' [ 'Graphics.Vega.VegaLite.dataFromUrl' \"data/stocks.csv\" []
           , 'mark' 'Graphics.Vega.VegaLite.Line' []
           , 'encoding'
             . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"date\", 'PmType' 'Graphics.Vega.VegaLite.Temporal' ]
             . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"price\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
             . strokeDash [ 'MName' \"symbol\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ]
             $ []
           ]
@

It can also be used to change the line style for connected
points (e.g. to indicate where the data changes its \"predicted\"
value, noting that there are two points at @\"a\"@ equal to @\"E\"@):

@
'Graphics.Vega.VegaLite.toVegaLite' [ 'Graphics.Vega.VegaLite.dataFromColumns' []
             . 'Graphics.Vega.VegaLite.dataColumn' \"a\" ('Strings' [ \"A\", \"B\", \"D\", \"E\", \"E\", \"G\", \"H\"])
             . 'Graphics.Vega.VegaLite.dataColumn' \"b\" ('Numbers' [ 28, 55, 91, 81, 81, 19, 87 ])
             . 'Graphics.Vega.VegaLite.dataColumn' \"predicted\" ('Booleans' [False, False, False, False, True, True, True])
             $ []
           , 'mark' 'Graphics.Vega.VegaLite.Line' []
           , 'encoding'
             . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"a\", 'PmType' 'Graphics.Vega.VegaLite.Ordinal' ]
             . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"b\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
             . strokeDash [ 'MName' \"predicted\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ]
             $ []
           ]
@

@since 0.6.0.0

-}

strokeDash ::
  [MarkChannel]
  -- ^ What data values are used to control the stoke opacity parameters of the mark.
  -> BuildEncodingSpecs
strokeDash markProps ols = mchan_ "strokeDash" markProps : ols


{-|

Encode a stroke opacity channel. This acts in a similar way to encoding by
'opacity' but only affects the exterior boundary of marks. If both 'opacity' and
@strokeOpacity@ are specified, @strokeOpacity@ takes precedence for stroke encoding.

@since 0.4.0.0

-}

strokeOpacity ::
  [MarkChannel]
  -- ^ What data values are used to control the stoke opacity parameters of the mark.
  -> BuildEncodingSpecs
strokeOpacity markProps ols = mchan_ "strokeOpacity" markProps : ols


{-|

Encode a stroke width channel.

@since 0.4.0.0

-}

strokeWidth ::
  [MarkChannel]
  -- ^ What data values are used to control the stoke width parameters of the mark.
  -> BuildEncodingSpecs
strokeWidth markProps ols = mchan_ "strokeWidth" markProps : ols


{-|

Encode a text channel. See the
<https://vega.github.io/vega-lite/docs/encoding.html#text Vega-Lite documentation>
for further details on the text and tooltip channels and
<https://vega.github.io/vega-lite/docs/format.html Vega-Lite formatting documentation>
for formatting the appearance of the text.

@
'encoding'
    . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' "miles", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' "gas", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    . 'text' [ 'TName' "miles", 'TmType' 'Graphics.Vega.VegaLite.Quantitative' ]
@
-}
text ::
  [TextChannel]
  -- ^ What data values are used to control the text parameters.
  -> BuildEncodingSpecs
text tDefs ols =
  ES ("text", object (concatMap textChannelProperty tDefs)) : ols


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
trans = 'transform' . 'timeUnitAs' ('Graphics.Vega.VegaLite.TU' 'Graphics.Vega.VegaLite.Month') \"date\" \"monthly\"

enc = 'encoding'
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"date\", 'PmType' 'Graphics.Vega.VegaLite.Temporal', 'PTimeUnit' ('Graphics.Vega.VegaLite.TU' 'Graphics.Vega.VegaLite.Day') ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PAggregate' 'Graphics.Vega.VegaLite.Sum', 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'detail' [ 'DName' \"monthly\", 'DmType' 'Graphics.Vega.VegaLite.Temporal' ]
@

-}
timeUnitAs ::
  TimeUnit
  -- ^ The width of each bin.
  --
  --   Prior to @0.10.0.0@ this was sent a single time unit.
  -> FieldName
  -- ^ The field to bin.
  -> FieldName
  -- ^ The name of the binned data created by this routine.
  -> BuildTransformSpecs
timeUnitAs tu field label ols =
  let fields = [ "timeUnit" .= timeUnitSpec tu
               , "field" .= field
               , "as" .= label ]
  in TS (object fields) : ols


{-|

Encode a tooltip channel. See the
<https://vega.github.io/vega-lite/docs/encoding.html#text Vega-Lite documentation>
for further details on the text and tooltip channels and
<https://vega.github.io/vega-lite/docs/format.html Vega-Lite formatting documentation>
for formatting the appearance of the text.

@
enc = 'encoding'
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"Horsepower\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"Miles_per_Gallon\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'tooltip' [ 'TName' \"Year\", 'TmType' 'Graphics.Vega.VegaLite.Temporal', 'TFormat' "%Y" ]
@

To encode multiple tooltip values with a mark, use 'tooltips'.

-}
tooltip ::
  [TextChannel]
  -- ^ The properties for the channel.
  --
  --   If the list is empty then this turns off tooltip support for
  --   this channel. This is new to @0.5.0.0@, but is also the
  --   default behavior in Vega Lite 4.
  -> BuildEncodingSpecs
tooltip [] ols =
  ES ("tooltip", A.Null) : ols
tooltip tDefs ols =
  ES ("tooltip", object (concatMap textChannelProperty tDefs)) : ols


{-|

Encode a tooltip channel using multiple data fields.

@since 0.3.0.0

@
'encoding'
    . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"Horsepower\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"Miles_per_Gallon\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    . 'tooltips' [ [ 'TName' \"Year\",  'TmType' 'Graphics.Vega.VegaLite.Temporal', 'TFormat' "%Y" ]
               , [ 'TName' \"Month\", 'TmType' 'Graphics.Vega.VegaLite.Temporal', 'TFormat' "%Y" ] ]
@
-}
tooltips ::
  [[TextChannel]]
  -- ^ A separate list of properties for each channel.
  -> BuildEncodingSpecs
tooltips tDefs ols =
  ES ("tooltip" .=~ map (object . concatMap textChannelProperty) tDefs) : ols
