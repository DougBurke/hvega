{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Time
Copyright   : (c) Douglas Burke, 2018-2020
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

Time-related types.

-}

module Graphics.Vega.VegaLite.Time
       ( DateTime(..)
       , MonthName(..)
       , DayName(..)
       , TimeUnit(..)

       -- not for external export
       , timeUnit_

       , dateTimeProperty
       -- , dayLabel
       -- , monthNameLabel
       , timeUnitLabel
       
       ) where

import qualified Data.Text as T

import Data.Aeson ((.=))

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Graphics.Vega.VegaLite.Specification (LabelledSpec)


timeUnit_ :: TimeUnit -> LabelledSpec
timeUnit_ tu = "timeUnit" .= timeUnitLabel tu


{-|

Allows a date or time to be represented. This is typically part of a list of
@DateTime@ items to provide a specific point in time. For details see the
<https://vega.github.io/vega-lite/docs/types.html#datetime Vega-Lite documentation>.
-}

data DateTime
    = DTYear Int
    | DTQuarter Int
    | DTMonth MonthName
    | DTMonthNum Int
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
'Graphics.Vega.VegaLite.encoding'
    . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.X' [ 'Graphics.Vega.VegaLite.PName' "date", 'Graphics.Vega.VegaLite.PmType' 'Graphics.Vega.VegaLite.Temporal', 'Graphics.Vega.VegaLite.PTimeUnit' ('Utc' 'YearMonthDateHours') ]
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
dateTimeProperty (DTMonthNum n) = "month" .= n
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
