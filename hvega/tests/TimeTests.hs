{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite TimeTests.elm as of version 1.12.0
--

module TimeTests (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("timeYear", timeYear)
            , ("timeQuarter", timeQuarter)
            , ("timeQuarterMonth", timeQuarterMonth)
            , ("timeMonth", timeMonth)
            , ("timeMonthDate", timeMonthDate)
            , ("timeDate", timeDate)
            , ("timeYearMonthDateHours", timeYearMonthDateHours)
            , ("timeYearMonthDateHoursMinutes", timeYearMonthDateHoursMinutes)
            , ("timeYearMonthDateHoursMinutesSeconds", timeYearMonthDateHoursMinutesSeconds)
            , ("timeDay", timeDay)
            , ("timeHours", timeHours)
            , ("timeHoursMinutes", timeHoursMinutes)
            , ("timeHoursMinutesSeconds", timeHoursMinutesSeconds)
            , ("timeMinutes", timeMinutes)
            , ("timeMinutesSeconds", timeMinutesSeconds)
            , ("localTime", localTime)
            , ("utcTime", utcTime)
            ]


timeByUnit :: TimeUnit -> VegaLite
timeByUnit tu =
    let
        dataVals =
            dataFromUrl "https://gicentre.github.io/data/tests/timeTest.tsv" []

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PTimeUnit tu ]
                . position Y [ PName "temperature", PmType Quantitative
                             , PAggregate Mean, PScale [ SZero False ] ]
    in
    toVegaLite [ width 800, dataVals, enc []
               , mark Line [ MStrokeWidth 0.2 ] ]


timeYear :: VegaLite
timeYear = timeByUnit Year


timeQuarter :: VegaLite
timeQuarter = timeByUnit Quarter


timeQuarterMonth :: VegaLite
timeQuarterMonth = timeByUnit QuarterMonth


timeMonth :: VegaLite
timeMonth = timeByUnit Month


timeMonthDate :: VegaLite
timeMonthDate = timeByUnit MonthDate


timeDate :: VegaLite
timeDate = timeByUnit Date


timeYearMonthDateHours :: VegaLite
timeYearMonthDateHours = timeByUnit YearMonthDateHours


timeYearMonthDateHoursMinutes :: VegaLite
timeYearMonthDateHoursMinutes = timeByUnit YearMonthDateHoursMinutes


timeYearMonthDateHoursMinutesSeconds :: VegaLite
timeYearMonthDateHoursMinutesSeconds =
    timeByUnit YearMonthDateHoursMinutesSeconds


timeDay :: VegaLite
timeDay = timeByUnit Day


timeHours :: VegaLite
timeHours = timeByUnit Hours


timeHoursMinutes :: VegaLite
timeHoursMinutes = timeByUnit HoursMinutes


timeHoursMinutesSeconds :: VegaLite
timeHoursMinutesSeconds = timeByUnit HoursMinutesSeconds


timeMinutes :: VegaLite
timeMinutes = timeByUnit Minutes


timeMinutesSeconds :: VegaLite
timeMinutesSeconds = timeByUnit MinutesSeconds

-- TODO: Add milliseconds example
-- | SecondsMilliseconds
-- | Milliseconds


data Date
    = Local
    | UTC


parseTime :: Date -> VegaLite
parseTime dType =
    let
        format =
            case dType of
                Local ->
                    FoDate "%d %b %Y %H:%M"

                UTC ->
                    FoUtc "%d %b %Y %H:%M"

        tu =
            case dType of
                Local ->
                    PTimeUnit YearMonthDateHours

                UTC ->
                    PTimeUnit (Utc YearMonthDateHours)

        timeScale =
            case dType of
                Local ->
                    PScale [ SType ScTime ]

                UTC ->
                    PScale [ SType ScUtc ]

        dataVals =
            dataFromColumns [ Parse [ ( "date", format ) ] ]
                . dataColumn "date" (Strings [ "28 Oct 2017 22:00", "28 Oct 2017 23:00", "29 Oct 2017 00:00", "29 Oct 2017 01:00", "29 Oct 2017 02:00", "29 Oct 2017 03:00", "29 Oct 2017 04:00" ])
                . dataColumn "value" (Numbers [ 1, 2, 3, 4, 5, 6, 7 ])

        enc =
            encoding
                . position X [ PName "date", PmType Temporal
                             , tu, timeScale
                             , PAxis [ AxFormat "%d %b %H:%M" ] ]
                . position Y [ PName "value", PmType Quantitative ]
                . size [ MNumber 500 ]
    in
    toVegaLite [ width 800, dataVals [], enc [], mark Circle [] ]


localTime :: VegaLite
localTime = parseTime Local


utcTime :: VegaLite
utcTime = parseTime UTC
