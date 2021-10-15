{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Time
Copyright   : (c) Douglas Burke, 2018-2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings

Time-related types.

-}

module Graphics.Vega.VegaLite.Time
       ( DateTime(..)
       , MonthName(..)
       , DayName(..)
       , TimeUnit(..)
       , BaseTimeUnit(..)

       -- not for external export
       , dateTimeSpec
       , timeUnitSpec
       
       ) where

import qualified Data.Text as T

import Data.Aeson ((.=), object, toJSON)
import Data.Aeson.Types (Pair)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

-- added in base 4.8.0.0 / ghc 7.10.1
import Numeric.Natural (Natural)

import Graphics.Vega.VegaLite.Specification (VLSpec)


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
    | DTWeek Int
      -- ^ The week number. Each week begins on Sunday, which means that days
      --   before the first Sunday of the year are considered to be in week 0,
      --   as the first Sunday of the year is the start of week 1.
      --
      --   @since 0.10.0.0
    | DTDay DayName
    | DTDayNum Int
      -- ^ The day number (1 represents Monday, 7 is Sunday).
      --
      --   @since 0.5.0.0
    | DTDayOfYear Int
      -- ^ The day of the year (1 to 366).
      --
      --   @since 0.10.0.0
    | DTDate Int
      -- ^ Day of the month (1 to 31, inclusive).
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



-- Vega-Lite 4.4.0 has
--   LocalMultiTimeUnit which is yearquarter, yearquartermonth, ,secondsmilliseconds
--   LocalSingleTimeUnit year, quarter, ..., milliseconds
-- and
--   UtcMultiTimeUnit which is utc <> LocalMultiTimeUnit
--   UtcSingleTimeUnit         utc <> LocalSingleTimeUnit
--
-- TimeUnit       is either of SingleTimeUnit or MultiTimeUnit
-- SingleTimeUnut is either of LocalSingleTimeUnit or UtcSingleTimeUnit
-- MultiTimeUnit  is either of LocalMultiTieUnit or UtcMultiTimeUnit
--
-- "timeUnit" settings are TimeUnit or TimeUnitParams
--
-- TimeUnitParams is an object with fields
--   maxbins - number
--   step    - number
--   unit    - this is TimeUnit
--   utc     - boolean
--

{-|

Describes a unit of time. Useful for encoding and transformations. See the
<https://vega.github.io/vega-lite/docs/timeunit.html Vega-Lite documentation>
for further details.

@
'Graphics.Vega.VegaLite.encoding'
    . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.X' [ 'Graphics.Vega.VegaLite.PName' "date", 'Graphics.Vega.VegaLite.PmType' 'Graphics.Vega.VegaLite.Temporal', 'Graphics.Vega.VegaLite.PTimeUnit' ('Utc' 'YearMonthDateHours') ]
@

Prior to version @0.10.0.0@ the field was a combination of what is now
'BaseTimeUnit' and the \"option\" fields (e.g. encode as UTC or the
maximum nuber of bins).

-}


data TimeUnit
  = TU BaseTimeUnit
    -- ^ Encode a time as local time.
  | Utc BaseTimeUnit
    -- ^ Encode a time as UTC (coordinated universal time, independent of local time
    --   zones or daylight saving).
  | TUMaxBins Natural
    -- ^ The maximum number of bins to use when discretising time values.
    --   This can be useful as an algternative to explicitly providing the
    --   time unit to bin by, as it will be inferred from the temporal
    --   extent and the number of bins. As an example, @[TUMaxBins 366]@
    --   will bin by day when applied to a dataset of hourly readings
    --   for a full year.
    --
    --   @since 0.6.0.0
  | TUStep Double BaseTimeUnit
    -- ^ The number of steps between time-unit bins, in terms of the
    --   least-significant unit provided. So @TUStep 14 YearMonthDate@
    --   will bin temporal data into bi-weekly groups.
    --
    --   @since 0.6.0.0
  | UtcStep Double BaseTimeUnit
    -- ^ The number of steps between time-unit bins, in terms of the
    --   least-significant unit provided for UTC times.
    --   So @UtcStep 14 YearMonthDate]@
    --   will bin temporal data into bi-weekly groups.
    --
    --   @since 0.10.0.0


{-|

Define the time unit, either as a base unit - such as 'Hours' - or
as a composite type, for example 'WeeksDayHours'.

@since 0.10.0.0

-}

data BaseTimeUnit
    = Year
      -- ^ Year.
    | Quarter
      -- ^ Quarter of the year.
    | Month
      -- ^ Month of the year.
    | Week
      -- ^ Sunday-based week number. Days before the first Sunday of the year
      --   are considered to be in week 0, and the first Sunday of the year
      --   is the start of week 1,
      --
      --   @since 0.10.0.0
    | Date
      -- ^ Day of the month (1 to 31).
    | Day
      -- ^ Day of the week.
    | DayOfYear
      -- ^ Day of the year (starting at 1).
      --
      --   @since 0.10.0.0
    | Hours
      -- ^ Hour of the day.
    | Minutes
      -- ^ Minutes of the hour.
    | Seconds
      -- ^ Seconds of the minute.
    | Milliseconds
      -- ^ Milliseconds.
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
    | YearWeek
      -- ^ Year and week.
      --
      --   @since 0.10.0
    | YearWeekDay
      -- ^ Year, week, and day.
      --
      --   @since 0.10.0
    | YearWeekDayHours
      -- ^ Year, week, day, and hour of day.
      --
      --   @since 0.10.0
    | YearWeekDayHoursMinutes
      -- ^ Year, week, day, hour of day, and minutes.
      --
      --   @since 0.10.0
    | YearWeekDayHoursMinutesSeconds
      -- ^ Year, week, day, hour of day, minutes, and seconds.
      --
      --   @since 0.10.0
    | YearDayOfYear
      -- ^ Year and day of year.
      --
      --   @since 0.10.0
    | QuarterMonth
      -- ^ Quarter of the year and month.
    | MonthDate
      -- ^ Month of the year and day of the month.
    | MonthDateHours
      -- ^ Month, day of the month, and hours.
      --
      --   @since 0.10.0.0
    | MonthDateHoursMinutes
      -- ^ Month, day of the month, hours, and minutes.
      --
      --   @since 0.10.0.0
    | MonthDateHoursMinutesSeconds
      -- ^ Month, day of the month, hours, minutes, and seconds.
      --
      --   @since 0.10.0.0
    | WeekDay
      -- ^ Week and day of month.
      --
      --   @since 0.10.0.0
    | WeeksDayHours
      -- ^ Week, day of month, and hours.
      --
      --   @since 0.10.0.0
    | WeeksDayHoursMinutes
      -- ^ Week, day of month, hours, and minutes.
      --
      --   @since 0.10.0.0
    | WeeksDayHoursMinutesSeconds
      -- ^ Week, day of month, hours, minutes, and seconds.
      --
      --   @since 0.10.0.0
    | DayHours
      -- ^ Day of the week and hours.
      --
      --   @since 0.10.0.0
    | DayHoursMinutes
      -- ^ Day of the week, hours, and minutes.
      --
      --   @since 0.10.0.0
    | DayHoursMinutesSeconds
      -- ^ Day of the week, hours, minutes, and seconds.
      --
      --   @since 0.10.0.0
    | HoursMinutes
      -- ^ Hour of the day and minutes.
    | HoursMinutesSeconds
      -- ^ Hour of the day, minutes, and seconds.
    | MinutesSeconds
      -- ^ Minutes of the hour and seconds.
    | SecondsMilliseconds
      -- ^ Seconds of the minute and milliseconds.


baseTimeUnitLabel :: BaseTimeUnit -> T.Text
baseTimeUnitLabel Year = "year"
baseTimeUnitLabel Quarter = "quarter"
baseTimeUnitLabel Month = "month"
baseTimeUnitLabel Week = "week"
baseTimeUnitLabel Date = "date"
baseTimeUnitLabel Day = "day"
baseTimeUnitLabel DayOfYear = "dayofyear"
baseTimeUnitLabel Hours = "hours"
baseTimeUnitLabel Minutes = "minutes"
baseTimeUnitLabel Seconds = "seconds"
baseTimeUnitLabel Milliseconds = "milliseconds"

baseTimeUnitLabel YearQuarter = "yearquarter"
baseTimeUnitLabel YearQuarterMonth = "yearquartermonth"
baseTimeUnitLabel YearMonth = "yearmonth"
baseTimeUnitLabel YearMonthDate = "yearmonthdate"
baseTimeUnitLabel YearMonthDateHours = "yearmonthdatehours"
baseTimeUnitLabel YearMonthDateHoursMinutes = "yearmonthdatehoursminutes"
baseTimeUnitLabel YearMonthDateHoursMinutesSeconds = "yearmonthdatehoursminutesseconds"
baseTimeUnitLabel YearWeek = "yearweek"
baseTimeUnitLabel YearWeekDay = "yearweekday"
baseTimeUnitLabel YearWeekDayHours = "yearweekdayhours"
baseTimeUnitLabel YearWeekDayHoursMinutes = "yearweekdayhoursminutes"
baseTimeUnitLabel YearWeekDayHoursMinutesSeconds = "yearweekdayhoursminutesseconds"
baseTimeUnitLabel YearDayOfYear = "yeardayofyear"
baseTimeUnitLabel QuarterMonth = "quartermonth"
baseTimeUnitLabel MonthDate = "monthdate"
baseTimeUnitLabel MonthDateHours = "monthdatehours"
baseTimeUnitLabel MonthDateHoursMinutes = "monthdatehoursminutes"
baseTimeUnitLabel MonthDateHoursMinutesSeconds = "monthdatehoursminutesseconds"
baseTimeUnitLabel WeekDay = "weekday"
baseTimeUnitLabel WeeksDayHours = "weeksdayhours"
baseTimeUnitLabel WeeksDayHoursMinutes = "weeksdayhoursminutes"
baseTimeUnitLabel WeeksDayHoursMinutesSeconds = "weeksdayhoursminutesseconds"
baseTimeUnitLabel DayHours = "dayhours"
baseTimeUnitLabel DayHoursMinutes = "dayhoursminutes"
baseTimeUnitLabel DayHoursMinutesSeconds = "dayhoursminutesseconds"
baseTimeUnitLabel HoursMinutes = "hoursminutes"
baseTimeUnitLabel HoursMinutesSeconds = "hoursminutesseconds"
baseTimeUnitLabel MinutesSeconds = "minutesseconds"
baseTimeUnitLabel SecondsMilliseconds = "secondsmilliseconds"


dateTimeProperty :: DateTime -> Pair
dateTimeProperty (DTYear y) = "year" .= y
dateTimeProperty (DTQuarter q) = "quarter" .= q
dateTimeProperty (DTMonth mon) = "month" .= monthNameLabel mon
dateTimeProperty (DTMonthNum n) = "month" .= n
dateTimeProperty (DTWeek w) = "week" .= w
dateTimeProperty (DTDate dt) = "date" .= dt
dateTimeProperty (DTDay day) = "day" .= dayLabel day
dateTimeProperty (DTDayOfYear n) = "dayofyear" .= n
dateTimeProperty (DTDayNum n) = "day" .= n
dateTimeProperty (DTHours h) = "hours" .= h
dateTimeProperty (DTMinutes m) = "minutes" .= m
dateTimeProperty (DTSeconds s) = "seconds" .= s
dateTimeProperty (DTMilliseconds ms) = "milliseconds" .= ms


dateTimeSpec :: [DateTime] -> VLSpec
dateTimeSpec = object . map dateTimeProperty

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


fromT :: T.Text -> VLSpec
fromT = toJSON

timeUnitSpec :: TimeUnit -> VLSpec
timeUnitSpec (TU tu) = fromT (baseTimeUnitLabel tu)
timeUnitSpec (Utc tu) = fromT ("utc" <> baseTimeUnitLabel tu)
timeUnitSpec (TUStep x tu) = object ["step" .= x, "unit" .= baseTimeUnitLabel tu]
timeUnitSpec (UtcStep x tu) = object ["step" .= x, "unit" .= ("utc" <> baseTimeUnitLabel tu)]
timeUnitSpec (TUMaxBins n) = object ["maxbins" .= n]
