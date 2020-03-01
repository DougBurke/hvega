{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Scale
Copyright   : (c) Douglas Burke, 2018-2020
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings

Scale-related functionality.

-}

module Graphics.Vega.VegaLite.Scale
       ( ScaleDomain(..)
       , ScaleRange(..)
       , ScaleNice(..)

         -- not for external export
       , scaleDomainSpec
       , scaleNiceSpec
       ) where

import qualified Data.Text as T

import Data.Aeson ((.=), object, toJSON)


import Graphics.Vega.VegaLite.Foundation
  ( fromT
  )
import Graphics.Vega.VegaLite.Specification
  ( VLSpec
  )
import Graphics.Vega.VegaLite.Time
  ( DateTime
  , TimeUnit
  , dateTimeProperty
  , timeUnitSpec
  )


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
    | DUnionWith ScaleDomain
      -- ^ Combine the domain of the data with the provided domain.
      --
      --   The following example will use a range of at least 0 to 100,
      --   but this will be increased if the data (either initially or
      --   via any updates to the Vege-Lite visualization) exceeds this:
      --
      --   @'Graphics.Vega.VegaLite.PScale' ['Graphics.Vega.VegaLite.SDomain' (DUnionWith ('DNumbers' [0, 100]))]@
      --
      --   Note that 'DUnionWith' should not be nested, but this
      --   is not enforced by @hvega@.
      --
      --   @since 0.6.0.0
    | Unaggregated
    -- ^ Indicate that a domain of aggregated data should be scaled to
    --   the domain of the data prior to aggregation.

scaleDomainSpec :: ScaleDomain -> VLSpec
scaleDomainSpec (DNumbers nums) = toJSON (map toJSON nums)
scaleDomainSpec (DDateTimes dts) = toJSON (map (object . map dateTimeProperty) dts)
scaleDomainSpec (DStrings cats) = toJSON (map toJSON cats)
scaleDomainSpec (DSelection selName) = object ["selection" .= selName]
scaleDomainSpec (DUnionWith sd) = object ["unionWith" .= scaleDomainSpec sd]
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
  object ["interval" .= timeUnitSpec tu, "step" .= step]
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
    = RNumbers [Double]
      -- ^ For [continuous scales](https://vega.github.io/vega-lite/docs/scale.html#continuous),
      --   a two-element array indicating minimum and maximum values, or an array with more than
      --   two entries for specifying a
      --   [piecewise scale](https://vega.github.io/vega-lite/docs/scale.html#piecewise).
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
