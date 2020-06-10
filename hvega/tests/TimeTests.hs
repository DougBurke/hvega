{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Based on the Elm VegaLite TimeTests.elm as of version 1.12.0
--

module TimeTests (testSpecs) where

import Data.Aeson (Value)
import Data.Aeson.QQ.Simple (aesonQQ)

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
            , ("monthAggregate", monthAggregate)
            , ("withBar", withBar)
            , ("timeBand", timeBand)
            , ("withBarOrdinal", withBarOrdinal)
            , ("timeUnitTransform", timeUnitTransform)
            , ("parseAsUTC", parseAsUTC)
            , ("parseAsLocal", parseAsLocal)
            , ("parseAsFormat", parseAsFormat)
            , ("outputAsUTC", outputAsUTC)
            , ("outputScaledAsUTC", outputScaledAsUTC)
            , ("customizeStep", customizeStep)
            , ("nestedTime1", nestedTime1)
            , ("nestedTime2", nestedTime2)
            ]


timeByUnit :: BaseTimeUnit -> VegaLite
timeByUnit btu =
    let
        dataVals =
            dataFromUrl "https://gicentre.github.io/data/tests/timeTest.tsv" []

        enc =
            encoding
                . position X [ PName "date", PmType Temporal, PTimeUnit (TU btu) ]
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
                    PTimeUnit (TU YearMonthDateHours)

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


-- examples from the documentation
--
-- https://vega.github.io/vega-lite/docs/timeunit.html

seattleTemps :: Data
seattleTemps = dataFromUrl "https://vega.github.io/vega-lite/data/seattle-temps.csv" []

seattleWeather :: Data
seattleWeather = dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []


monthAggregate :: VegaLite
monthAggregate =
  let enc = encoding
            . position X [ PName "date"
                         , PTimeUnit (TU Month)
                         , PmType Temporal
                         ]
            . position Y [ PName "temp"
                         , PAggregate Mean
                         , PmType Quantitative ]

  in toVegaLite
        [ seattleTemps
        , mark Line [MInterpolate Monotone]
        , enc []
        ]


withBar :: VegaLite
withBar =
  let enc = encoding
            . position X [ PName "date"
                         , PTimeUnit (TU Month)
                         , PmType Temporal
                         ]
            . position Y [ PName "precipitation"
                         , PAggregate Mean
                         , PmType Quantitative ]

  in toVegaLite
        [ seattleWeather
        , mark Bar []
        , enc []
        ]


-- https://vega.github.io/vega-lite/docs/timeunit.html#time-units-band

timeBand :: VegaLite
timeBand =
  let enc = encoding
            . position X [ PName "date", PTimeUnit (TU Month)
                         , PmType Temporal, PBand 0.5 ]
            . position Y [ PName "temp", PAggregate Mean
                         , PmType Quantitative ]

  in toVegaLite
        [ width 400
        , seattleTemps
        , enc []
        , mark Line [ MPoint (PMMarker [ MFill "black" ]) ]
        ]


-- https://vega.github.io/vega-lite/docs/timeunit.html#time-unit-with-ordinal-fields

withBarOrdinal :: VegaLite
withBarOrdinal =
  let enc = encoding
            . position X [ PName "date"
                         , PTimeUnit (TU Month)
                         , PmType Ordinal
                         ]
            . position Y [ PName "precipitation"
                         , PAggregate Mean
                         , PmType Quantitative ]

  in toVegaLite
        [ seattleWeather
        , mark Bar []
        , enc []
        ]


-- https://vega.github.io/vega-lite/docs/timeunit.html#transform

timeUnitTransform :: VegaLite
timeUnitTransform =
  let enc = encoding
            . position X [ PName "month"
                         , PmType Temporal
                         , PAxis [AxFormat "%b"]
                         ]
            . position Y [ PName "temp_max"
                         , PAggregate Max
                         , PmType Quantitative ]

  in toVegaLite
        [ seattleWeather
        , mark Line []
        , transform (timeUnitAs (TU Month) "date" "month" [])
        , enc []
        ]


-- https://vega.github.io/vega-lite/docs/timeunit.html#input

parseAsUTC :: VegaLite
parseAsUTC =
  toVegaLite [ dataFromColumns []
               . dataColumn "date" (Strings ["2011-10-10", "2011-10-12"])
               $ []
             , mark Point []
             , encoding
               . position Y [ PName "date"
                            , PmType Ordinal
                            , PTimeUnit (Utc Hours)
                            , PAxis [AxTitle "time"]
                            ]
               $ []
             ]


parseAsLocal :: VegaLite
parseAsLocal =
  toVegaLite [ dataFromColumns []
               . dataColumn "date" (Strings ["10 Oct 2011 22:48:00", "11 Oct 2022 23:00:00"])
               $ []
             , mark Point []
             , encoding
               . position Y [ PName "date"
                            , PmType Ordinal
                            , PTimeUnit (TU HoursMinutes)
                            , PAxis [AxTitle "time"]
                            ]
               $ []
             ]


parseAsFormat :: VegaLite
parseAsFormat =
  toVegaLite [ dataFromColumns [Parse [("date", FoUtc "%d %b %Y %H:%M:%S")]]
               . dataColumn "date" (Strings ["10 Oct 2011 22:48:00", "11 Oct 2022 23:00:00"])
               $ []
             , mark Point []
             , encoding
               . position Y [ PName "date"
                            , PmType Ordinal
                            , PTimeUnit (TU HoursMinutes)
                            , PAxis [AxTitle "time"]
                            ]
               $ []
             ]


-- https://vega.github.io/vega-lite/docs/timeunit.html#output

exampleData :: Data
exampleData =
  dataFromColumns []
  . dataColumn "date" (Strings [ "Sun, 01 Jan 2012 23:00:00"
                               , "Sun, 02 Jan 2012 00:00:00"
                               , "Sun, 02 Jan 2012 01:00:00"
                               , "Sun, 02 Jan 2012 02:00:00"
                               , "Sun, 02 Jan 2012 03:00:00"
                               ])

  . dataColumn "price" (Numbers [150, 100, 170, 165, 200])
  $ []


outputAsUTC :: VegaLite
outputAsUTC =
  toVegaLite [ exampleData
             , mark Line []
             , encoding
               . position X [ PName "date"
                            , PmType Temporal
                            , PTimeUnit (Utc YearMonthDateHoursMinutes)
                            , PAxis [AxLabelAngle 15]
                            ]
               . position Y [ PName "price"
                            , PmType Quantitative
                            ]
               $ []
             ]


outputScaledAsUTC :: VegaLite
outputScaledAsUTC =
  toVegaLite [ exampleData
             , mark Line []
             , encoding
               . position X [ PName "date"
                            , PmType Temporal
                            , PTimeUnit (TU YearMonthDateHoursMinutes)
                            , PScale [SType ScUtc]
                            , PAxis [AxLabelAngle 15]
                            ]
               . position Y [ PName "price"
                            , PmType Quantitative
                            ]
               $ []
             ]


-- https://vega.github.io/vega-lite/docs/timeunit.html#example-customizing-step

customizeStep :: VegaLite
customizeStep =
  toVegaLite [ dataFromColumns []
               . dataColumn "date" (Strings [ "Sun, 01 Jan 2012 00:00:00"
                                            , "Sun, 01 Jan 2012 00:01:00"
                                            , "Sun, 01 Jan 2012 00:02:00"
                                            , "Sun, 01 Jan 2012 00:03:00"
                                            , "Sun, 01 Jan 2012 00:04:00"
                                            , "Sun, 01 Jan 2012 00:05:00"
                                            , "Sun, 01 Jan 2012 00:06:00"
                                            , "Sun, 01 Jan 2012 00:07:00"
                                            , "Sun, 01 Jan 2012 00:08:00"
                                            , "Sun, 01 Jan 2012 00:09:00"
                                            , "Sun, 01 Jan 2012 00:10:00"
                                            , "Sun, 01 Jan 2012 00:11:00"
                                            , "Sun, 01 Jan 2012 00:12:00"
                                            , "Sun, 01 Jan 2012 00:13:00"
                                            , "Sun, 01 Jan 2012 00:14:00"
                                            , "Sun, 01 Jan 2012 00:15:00"
                                            ])
               . dataColumn "distance" (Numbers [ 1
                                                , 1
                                                , 2
                                                , 1
                                                , 4
                                                , 2
                                                , 5
                                                , 2
                                                , 6
                                                , 4
                                                , 1
                                                , 1
                                                , 3
                                                , 0
                                                , 2
                                                , 3
                                                ])
               $ []
             , mark Bar []
             , encoding
               . position X [ PName "date"
                            , PmType Temporal
                            , PTimeUnit (TUStep 5 Minutes)
                            ]
               . position Y [ PName "distance"
                            , PmType Quantitative
                            , PAggregate Sum
                            ]
               $ []
             ]


embeddedData, flattenedData :: Value
embeddedData = [aesonQQ|
[
   {"histo": {"date": "Sun, 02 Jan 2012 00:00:00", "price": 150}},
   {"histo": {"date": "Sun, 02 Jan 2012 00:00:00", "price": 100}},
   {"histo": {"date": "Sun, 02 Jan 2012 01:00:00", "price": 170}},
   {"histo": {"date": "Sun, 02 Jan 2012 02:00:00", "price": 165}},
   {"histo": {"date": "Sun, 02 Jan 2012 03:00:00", "price": 200}}
]
|]
  
flattenedData = [aesonQQ|
[
   {"date": "Sun, 02 Jan 2012 00:00:00", "price": 150},
   {"date": "Sun, 02 Jan 2012 00:00:00", "price": 100},
   {"date": "Sun, 02 Jan 2012 01:00:00", "price": 170},
   {"date": "Sun, 02 Jan 2012 02:00:00", "price": 165},
   {"date": "Sun, 02 Jan 2012 03:00:00", "price": 200}
]
|]
  

-- https://github.com/vega/vega-lite/issues/5662

nestedTime :: Value -> FieldName -> FieldName -> VegaLite
nestedTime jData xname yname =
  let desc = "Google's stock price over time."
      dvals = dataFromJson jData []
      
  in toVegaLite [ description desc
                , dvals
                , mark Line []
                , encoding
                  . position X [ PName xname
                               , PmType Temporal
                               , PTimeUnit (TU YearMonthDateHoursMinutes)
                               , PScale [SType ScUtc]
                               ]
                  . position Y [ PName yname
                               , PmType Quantitative
                               ]
                  $ []
                ]

  
nestedTime1, nestedTime2 :: VegaLite
nestedTime1 = nestedTime embeddedData "histo.date" "histo.price"
nestedTime2 = nestedTime flattenedData "date" "price"
