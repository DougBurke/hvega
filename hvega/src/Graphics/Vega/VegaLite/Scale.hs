{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Scale
Copyright   : (c) Douglas Burke, 2018-2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings

Scale-related functionality.

-}

module Graphics.Vega.VegaLite.Scale
       ( ScaleDomain(..)
       , DomainLimits(..)
       , ScaleRange(..)
       , ScaleNice(..)
       , NTimeUnit(..)

         -- not for external export
       , scaleDomainProperty
       , domainLimitsSpec
       , scaleNiceSpec
       ) where

import qualified Data.Text as T

import Data.Aeson ((.=), object, toJSON)
import Data.Aeson.Types (Pair)


import Graphics.Vega.VegaLite.Foundation
  ( Channel
  , FieldName
  , fromT
  , channelLabel
  )
import Graphics.Vega.VegaLite.Specification
  ( VLSpec
  , SelectionLabel
  )
import Graphics.Vega.VegaLite.Time
  ( DateTime
  , dateTimeSpec
  )


{-|

Describes the scale domain (type of data in scale). For full details see the
<https://vega.github.io/vega-lite/docs/scale.html#domain Vega-Lite documentation>.

In @0.11.0.0@ the functionality has been split into 'ScaleDomain' and
'DomainLimits'.
-}

data ScaleDomain
    = DMax Double
      -- ^ Sets the maximum value in the scale domain.
      --   It is only intended for scales with a continuous domain.
      --
      --   It is supported in Vega-Lite 4.14 and later.
      --
      --   @since 0.11.0.0
    | DMaxTime [DateTime]
      -- ^ 'DMax' for dates.
      --
      --   It is supported in Vega-Lite 4.14 and later.
      --
      --   @since 0.11.0.0
    | DMid Double
      -- ^ Sets the mid-point of a continuous diverging domain.
      --
      --   It replaces 'Graphics.Vega.VegaLite.SDomainMid'.
      --
      --   @since 0.11.0.0
    | DMin Double
      -- ^ Sets the minimum value in the scale domain.
      --   It is only intended for scales with a continuous domain.
      --
      --   It is supported in Vega-Lite 4.14 and later.
      --
      --   @since 0.11.0.0
    | DMinTime [DateTime]
      -- ^ 'DMin' for dates.
      --
      --   It is supported in Vega-Lite 4.14 and later.
      --
      --   @since 0.11.0.0
    | DSelection SelectionLabel
      -- ^ Scale domain based on a named interactive selection.
      --   See also 'DSelectionField' and 'DSelectionChannel', which should
      --   be used when a selection is
      --   [projected](https://vega.github.io/vega-lite/docs/project.html)
      --   over multiple fields or encodings.
      --
      --   In @0.7.0.0@ the argument type was changed to @SelectionLabel@
      --   (which is a type synonym for @Text@).
    | DSelectionField SelectionLabel FieldName
      -- ^ Use the given selection /and/ associated field, when the selection
      --   is projected over multiple fields or encodings.
      --
      --   @since 0.7.0.0
    | DSelectionChannel SelectionLabel Channel
      -- ^ Use the given selection /and/ associated encoding, when the selection
      --   is projected over multiple fields or encodings.
      --
      --   @since 0.7.0.0
    | DUnionWith DomainLimits
      -- ^ Combine the domain of the data with the provided domain.
      --
      --   The following example will use a range of at least 0 to 100,
      --   but this will be increased if the data (either initially or
      --   via any updates to the Vege-Lite visualization) exceeds this:
      --
      --   @'Graphics.Vega.VegaLite.PScale' ['Graphics.Vega.VegaLite.SDomainOpt' (DUnionWith ('DNumbers' [0, 100]))]@
      --
      --   @since 0.6.0.0
    | Unaggregated
      -- ^ Indicate that a domain of aggregated data should be scaled to
      --   the domain of the data prior to aggregation.


-- For now we do not include Unaggregated in DomainLimits as the
-- documentaiton suggests we should ubt the schema does not: see
-- https://github.com/vega/vega-lite/issues/7022
--

{-|

Represent the range of the domain, which is used by 'VL.SDomain'
and 'DUnionWith'.

Prior to @0.11.0.0@ this was part of 'ScaleDomain'.

@since 0.11.0.0
-}
data DomainLimits
  = DNumbers [Double]
    -- ^ Numeric values that define a scale domain.
    --
    --   It is expected that this contains two values (minimum and
    --   maximum), but more can be given for
    --   [piecewise  quantitative scales](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
  | DStrings [T.Text]
    -- ^ String values that define a scale domain
  | DDateTimes [[DateTime]]
    -- ^ Date-time values that define a scale domain.


scaleDomainProperty :: ScaleDomain -> Pair

scaleDomainProperty (DMax x) = "domainMax" .= x
scaleDomainProperty (DMaxTime dts) = "domainMax" .= dateTimeSpec dts
scaleDomainProperty (DMid x) = "domainMid" .= x
scaleDomainProperty (DMin x) = "domainMin" .= x
scaleDomainProperty (DMinTime dts) = "domainMin" .= dateTimeSpec dts


scaleDomainProperty (DSelection selName) = "domain" .= object ["selection" .= selName]
scaleDomainProperty (DSelectionField selName field) = "domain" .= object [ "selection" .= selName
                                                                         , "field" .= field ]
scaleDomainProperty (DSelectionChannel selName ch) = "domain" .= object [ "selection" .= selName
                                                                        , "encoding" .= channelLabel ch ]
scaleDomainProperty (DUnionWith sd) = "domain" .= object ["unionWith" .= domainLimitsSpec sd]
scaleDomainProperty Unaggregated = "domain" .= fromT "unaggregated"


domainLimitsSpec :: DomainLimits -> VLSpec
domainLimitsSpec (DNumbers nums) = toJSON (map toJSON nums)
domainLimitsSpec (DDateTimes dts) = toJSON (map dateTimeSpec dts)
domainLimitsSpec (DStrings cats) = toJSON (map toJSON cats)


{-|

Describes the way a scale can be rounded to \"nice\" numbers. For full details see the
<https://vega.github.io/vega-lite/docs/scale.html#continuous Vega-Lite documentation>.

Prior to version @0.10.0.0@ the time units were included in the constructors
for @ScaleNice@.

-}
data ScaleNice
  = NTU NTimeUnit
    -- ^ Time range.
  | NInterval NTimeUnit Int
    -- ^ \"Nice\" temporal interval values when scaling.
  | IsNice Bool
    -- ^ Enable or disable nice scaling.
  | NTickCount Int
    -- ^ Desired number of tick marks in a \"nice\" scaling.

{-|

The time intervals that can be rounded to \"nice\" numbers.

Prior to @0.10.0.0@ these were part of 'ScaleNice'.

-}

data NTimeUnit
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


nTimeUnitSpec :: NTimeUnit -> VLSpec
nTimeUnitSpec NMillisecond = fromT "millisecond"
nTimeUnitSpec NSecond = fromT "second"
nTimeUnitSpec NMinute = fromT "minute"
nTimeUnitSpec NHour = fromT "hour"
nTimeUnitSpec NDay = fromT "day"
nTimeUnitSpec NWeek = fromT "week"
nTimeUnitSpec NMonth = fromT "month"
nTimeUnitSpec NYear = fromT "year"


scaleNiceSpec :: ScaleNice -> VLSpec
scaleNiceSpec (NTU tu) = nTimeUnitSpec tu
scaleNiceSpec (NInterval tu step) =
  object ["interval" .= nTimeUnitSpec tu, "step" .= step]
scaleNiceSpec (IsNice b) = toJSON b
scaleNiceSpec (NTickCount n) = toJSON n


{-|

Describes a scale range of scale output values. For full details see the
<https://vega.github.io/vega-lite/docs/scale.html#range Vega-Lite documentation>.

For color scales you can also specify a color [scheme](https://vega.github.io/vega-lite/docs/scale.html#scheme)
instead of range.

Any directly specified range for @x@ and @y@ channels will be ignored. Range can be customized
via the view's corresponding [size](https://vega.github.io/vega-lite/docs/size.html)
('Graphics.Vega.VegaLite.width' and 'Graphics.Vega.VegaLite.height') or via range steps and paddings properties (e.g. 'Graphics.Vega.VegaLite.SCRangeStep')
for band and point scales.

-}

data ScaleRange
    = RField FieldName
      -- ^ For [discrete](https://vega.github.io/vega-lite/docs/scale.html#discrete)
      --   and [discretizing](https://vega.github.io/vega-lite/docs/scale.html#discretizing)
      --   scales, the name if the field to use.
      --
      --   For example. if the field \"color\" contains CSS color names, we can say
      --   @RField \"color\"@.
      --
      --   It is supported in Vega-Lite 4.14 and later.
      --
      --   @since 0.11.0.0
    | RMax Double
      -- ^ Sets the maximum value in the scale range.
      --   It is only intended for scales with a continuous range.
      --
      --   It is supported in Vega-Lite 4.14 and later.
      --
      --   @since 0.11.0.0
    | RMin Double
      -- ^ Sets the minimum value in the scale range.
      --   It is only intended for scales with a continuous range.
      --
      --   It is supported in Vega-Lite 4.14 and later.
      --
      --   @since 0.11.0.0
    | RPair Double Double
      -- ^ The minimum and maximum values.
      --
      --   @since 0.9.0.0
    | RHeight Double
      -- ^ Specify the width as a number and height as the string @"height"@.
      --
      --   @since 0.9.0.0
    | RWidth Double
      -- ^ Specify the height as a number and width as the string @"width"@.
      --
      --   @since 0.9.0.0
    | RNumbers [Double]
      -- ^ For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous),
      --   a two-element array indicating minimum and maximum values, or an array with more than
      --   two entries for specifying a
      --   [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
      --
      --   Support for the two-element version may be removed (ie this left only
      --   for piecewise scales).
    | RNumberLists [[Double]]
      -- ^ A scale range comprising of numeric lists, such as custom dash styles for
      --   the 'Graphics.Vega.VegaLite.strokeDash' channel encoding.
      --
      --   @since 0.6.0.0
    | RStrings [T.Text]
      -- ^ Text scale range for discrete scales.
    | RName T.Text
      -- ^ Name of a [pre-defined named scale range](https://vega.github.io/vega-lite/docs/scale.html#range-config)
      --   (e.g. \"symbol\" or \"diverging\").

{-

TODO:

* TickCount accepts number or TimeInterval but this is a subset of ScaleNice

-}
