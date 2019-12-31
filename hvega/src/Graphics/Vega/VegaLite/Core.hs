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

       , lookup
       , lookupAs

       , impute

       , sample

       , window

       , mark

       , encoding

       , position

       , PositionChannel(..)

       , SortProperty(..)

       , AxisProperty(..)

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

       , text
       , tooltip
       , tooltips
       , TextChannel(..)

       , hyperlink
       , HyperlinkChannel(..)

       , order
       , OrderChannel(..)

       , row
       , column

       , detail
       , DetailChannel(..)

       , ScaleProperty(..)
       , categoricalDomainMap
       , domainRangeMap
       , ScaleDomain(..)
       , ScaleRange(..)
       , ScaleNice(..)

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
       , width
       , padding
       , autosize
       , background
       , usermetadata

       , viewBackground

       , configure

       -- not for external export
       , schemeProperty
       , autosizeProperty
       , paddingSpec

       )
    where

-- VegaLite uses these symbols.
import Prelude hiding (filter, lookup, repeat)

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Aeson (decode, encode, object, toJSON, (.=))
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
  , Opacity
  , ZIndex
  , FontWeight
  , Measurement
  , Arrangement
  , APosition
  , Position
  , HAlign
  , VAlign
  , Scale
  , OverlapStrategy
  , Side
  , StackProperty
  , StackOffset
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
  , fromT
  , field_
  , header_
  , order_
  , fontWeightSpec
  , measurementLabel
  , arrangementLabel
  , anchorLabel
  , hAlignLabel
  , vAlignLabel
  , scaleLabel
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
import Graphics.Vega.VegaLite.Specification
  ( VLProperty(..)
  , VLSpec
  , PropertySpec
  , LabelledSpec
  , BuildLabelledSpecs
  )
import Graphics.Vega.VegaLite.Time
  ( DateTime
  , TimeUnit
  , timeUnit_
  , dateTimeProperty
  , timeUnitLabel
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
  , imputeFields_
  , bin
  , binProperty
  , windowFieldProperty
  , windowPropertySpec
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


repeat_ :: Arrangement -> LabelledSpec
repeat_ arr = "repeat" .= arrangementLabel arr

sort_ :: [SortProperty] -> LabelledSpec
sort_ ops = "sort" .= sortPropertySpec ops

mchan_ :: T.Text -> [MarkChannel] -> LabelledSpec
mchan_ f ms = f .= object (concatMap markChannelProperty ms)

mtype_ :: Measurement -> LabelledSpec
mtype_ m = "type" .= measurementLabel m

-- The assumption at the moment is that it's always correct to
-- replace the empty list by null.
--
scaleProp_ :: [ScaleProperty] -> LabelledSpec
scaleProp_ [] = "scale" .= A.Null
scaleProp_ sps = "scale" .= object (map scaleProperty sps)


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

Create an encoding specification from a list of channel encodings,
such as 'position', 'color', 'size', 'shape'.

@
enc = 'encoding'
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"Animal\", 'PmType' 'Graphics.Vega.VegaLite.Ordinal' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ PName \"Age\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'shape' [ 'MName' \"Species\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ]
        . 'size' [ 'MName' \"Population\", 'MmType' 'Graphics.Vega.VegaLite.Quantitative' ]
@

The type of @enc@ in this example is @[LabelledSpec] -> PropertySpec@,
so it can either be used to add further encoding specifications or as
@enc []@ to create a specification.

-}
encoding :: [LabelledSpec] -> PropertySpec
encoding channels = (VLEncoding, object channels)

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
'Graphics.Vega.VegaLite.ScaleConfig' values. For more details see the
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
    | SDomain ScaleDomain
      -- ^ Custom scaling domain.
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
('width' and 'height') or via range steps and paddings properties (e.g. 'Graphics.Vega.VegaLite.SCRangeStep')
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
      --      . 'position' 'Graphics.Vega.VegaLite.X' [ PWidth ]
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


{-|

Set the background color of the visualization. Should be specified with a CSS
string such as @\"#ffe\"@ or @\"rgb(200,20,150)\"@. If not specified the background will
be transparent.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'background' "rgb(251,247,238)"
    , 'Graphics.Vega.VegaLite.dataFromUrl' "data/population.json" []
    , 'mark' 'Graphics.Vega.VegaLite.Bar' []
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
      --   sel = 'Graphics.Vega.VegaLite.selection' . 'Graphics.Vega.VegaLite.select' \"myBrush\" 'Graphics.Vega.VegaLite.Interval' ['Graphics.Vega.VegaLite.Encodings' ['Graphics.Vega.VegaLite.ChX']]
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
    , 'MmType' 'Graphics.Vega.VegaLite.Ordinal'
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
        . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.Axis' [ 'Graphics.Vega.VegaLite.DomainWidth' 1 ])
        . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.View' [ 'Graphics.Vega.VegaLite.ViewStroke' (Just "transparent") ])
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
setting.

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
    , 'mark' 'Graphics.Vega.VegaLite.Bar' []
    , enc []
    ]
@
-}
width :: Double -> PropertySpec
width w = (VLWidth, toJSON w)


{-|

Defines a set of named aggregation transformations to be used when encoding
channels. This is useful when, for example, you wish to apply the same transformation
to a number of channels but do not want to define it each time. For further details
see the
<https://vega.github.io/vega-lite/docs/aggregate.html#aggregate-op-def Vega-Lite documentation>.

@
'transform'
    . 'aggregate'
        [ 'opAs' 'Graphics.Vega.VegaLite.Min' "people" "lowerBound", 'opAs' 'Graphics.Vega.VegaLite.Max' "people" "upperBound" ]
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
    . 'window' [ ( [ 'Graphics.Vega.VegaLite.WAggregateOp' 'Graphics.Vega.VegaLite.Sum', 'Graphics.Vega.VegaLite.WField' "Time" ], "TotalTime" ) ]
             [ 'Graphics.Vega.VegaLite.WFrame' Nothing Nothing ]
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
    . 'binAs' [ 'Graphics.Vega.VegaLite.MaxBins' 3 ] \"IMDB_Rating\" \"ratingGroup\"
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
'detail' ['DName' \"Species\", 'DmType' 'Graphics.Vega.VegaLite.Nominal'] []
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
'fill' [ 'MName' \"Species\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ] []
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
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"key\", 'PmType' 'Graphics.Vega.VegaLite.Nominal' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"city\", 'PmType' 'Graphics.Vega.VegaLite.Nominal' ]
        . 'size' [ 'MName' \"value\", 'MmType' 'Graphics.Vega.VegaLite.Quantitative' ]
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
  . 'position' X ['PName' "personDetails.age", 'PmType' 'Graphics.Vega.VegaLite.Temporal', 'PTimeUnit' 'Graphics.Vega.VegaLite.Year', 'PTitle' \"Age\"]
  . 'position' Y ['PName' "personDetails.height", 'PmType' 'Graphics.Vega.VegaLite.Quantitative', 'PTitle' \"Height\"]
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
  T.Text
  -- ^ The data field to process.
  -> T.Text
  -- ^ The key field to uniquely identify data objects within a group.
  -> [ImputeProperty]
  -- ^ Define how the imputation works.
  -> BuildLabelledSpecs
impute fields key imProps ols =
  (imputeFields_ fields key imProps) : ols


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

opacity :: [MarkChannel] -> BuildLabelledSpecs
opacity markProps ols = mchan_ "opacity" markProps : ols


{-|

Encode an order channel.

@
enc =
    'encoding'
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' "miles", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' "gas", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'order' [ 'OName' "year", 'OmType' 'Graphics.Vega.VegaLite.Temporal', 'OSort' ['Descending'] ]
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
    . resolution ('Graphics.Vega.VegaLite.RScale' [ ( 'Graphics.Vega.VegaLite.ChY', 'Graphics.Vega.VegaLite.Independent' ) ])
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
  -> BuildLabelledSpecs
row fFields ols = ("row" .= object (map facetChannelProperty fFields)) : ols


{-|

Encode a shape channel.

@
'shape' [ 'MName' \"Species\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ] []
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
'size' [ 'MName' \"Age\", 'MmType' 'Graphics.Vega.VegaLite.Quantitative' ] []
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
'stroke' [ 'MName' \"Species\", 'MmType' 'Graphics.Vega.VegaLite.Nominal' ] []
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
    . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' "miles", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' "gas", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    . 'text' [ 'TName' "miles", 'TmType' 'Graphics.Vega.VegaLite.Quantitative' ]
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
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"date\", 'PmType' 'Graphics.Vega.VegaLite.Temporal', 'PTimeUnit' 'Graphics.Vega.VegaLite.Day' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PAggregate' 'Graphics.Vega.VegaLite.Sum', 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'detail' [ 'DName' \"monthly\", 'DmType' 'Graphics.Vega.VegaLite.Temporal' ]
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
        . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"Horsepower\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"Miles_per_Gallon\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
        . 'tooltip' [ 'TName' \"Year\", 'TmType' 'Graphics.Vega.VegaLite.Temporal', 'TFormat' "%Y" ]
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
    . 'position' 'Graphics.Vega.VegaLite.X' [ 'PName' \"Horsepower\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    . 'position' 'Graphics.Vega.VegaLite.Y' [ 'PName' \"Miles_per_Gallon\", 'PmType' 'Graphics.Vega.VegaLite.Quantitative' ]
    . 'tooltips' [ [ 'TName' \"Year\",  'TmType' 'Graphics.Vega.VegaLite.Temporal', 'TFormat' "%Y" ]
               , [ 'TName' \"Month\", 'TmType' 'Graphics.Vega.VegaLite.Temporal', 'TFormat' "%Y" ] ]
@
-}
tooltips ::
  [[TextChannel]]
  -- ^ A separate list of properties for each channel.
  -> BuildLabelledSpecs
tooltips tDefs ols =
  ("tooltip" .= toJSON (map (object . concatMap textChannelProperty) tDefs)) : ols
