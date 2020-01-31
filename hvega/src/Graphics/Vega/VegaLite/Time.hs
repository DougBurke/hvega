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

There is __no check__ that the provided @Int@ values lie within the
required bounds.

A 'DateTime' value of 'DTDay' or 'DTDayNum' should not be combined with
'DTYear', 'DTQuarter', 'DTMonth', 'DTMonthNum', or 'DTDate'.

-}

data DateTime
    = DTYear Int
    | DTQuarter Int
      -- ^ The quarter of the year (1 to 4, inclusive).
    | DTMonth MonthName
    | DTMonthNum Int
      -- ^ The month number (1 to 12, inclusive).
      --
      --    @since 0.5.0.0
    | DTDate Int
      -- ^ Day of the month (1 to 31, inclusive).
    | DTDay DayName
    | DTDayNum Int
      -- ^ The day number (1 represents Monday, 7 is Sunday).
      --
      --    @since 0.5.0.0
    | DTHours Int
      -- ^ Hour of the day, where 0 is midnight, 1 is 1am, and
      --   23 is 11pm.
    | DTMinutes Int
      -- ^ The minute of an hour (0 to 59, inclusive).
    | DTSeconds Int
      -- ^ The second of a minute (0 to 59, inclusive).
    | DTMilliseconds Int
      -- ^ The milliseconds of a second (0 to 999, inclusive).


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
      -- ^ Year.
    | YearQuarter
      -- ^ Year and quarter.
    | YearQuarterMonth
      -- ^ Year, quarter, and month.
    | YearMonth
      -- ^ Year and month.
    | YearMonthDate
      -- ^ Year, month, and day of month.
    | YearMonthDateHours
      -- ^ Year, month, day of month, and hour of day.
    | YearMonthDateHoursMinutes
      -- ^ Year, month, day of month, hour of day, and minutes.
    | YearMonthDateHoursMinutesSeconds
      -- ^ Year, month, day of month, hour of day, minutes, and seconds.
    | Quarter
      -- ^ Quarter of the year.
    | QuarterMonth
      -- ^ Quarter of the year and month.
    | Month
      -- ^ Month of the year.
    | MonthDate
      -- ^ Month of the year and day of the month.
    | Date
      -- ^ Day of the month (1 to 31).
    | Day
      -- ^ Day of the week.
    | Hours
      -- ^ Hour of the day.
    | HoursMinutes
      -- ^ Hour of the day and minutes.
    | HoursMinutesSeconds
      -- ^ Hour of the day, minutes, and seconds.
    | Minutes
      -- ^ Minutes of the hour.
    | MinutesSeconds
      -- ^ Minutes of the hour and seconds.
    | Seconds
      -- ^ Seconds of the minute.
    | SecondsMilliseconds
      -- ^ Seconds of the minute and milliseconds.
    | Milliseconds
      -- ^ Milliseconds.
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
dateTimeProperty (DTDayNum n) = "day" .= n
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
