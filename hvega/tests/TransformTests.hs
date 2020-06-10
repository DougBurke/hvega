{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TransformTests (testSpecs) where

import qualified Data.Text as T

import Data.Aeson (Value)
import Data.Aeson.QQ.Simple (aesonQQ)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Prelude hiding (filter, lookup)

import Graphics.Vega.VegaLite


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("checkordering", checkOrdering)
            , ("binempty", binEmpty)
            , ("binstep", binStep)
            , ("imputemean", imputeMean)
            , ("densityplot", densityPlot)
            , ("loessplot", loessPlot)
            , ("lookupplot", lookupPlot)
            , ("pivotplot", pivotPlot)
            , ("quantileplot", quantilePlot)
            , ("regressionplot", regressionPlot)
            , ("flattenplot", flattenPlot)
            , ("foldasplot", foldAsPlot)
            , ("stackplot", stackPlot)
            , ("weatherbymonth", weatherByMonth)
            , ("weatherbytwomonths", weatherByTwoMonths)
            , ("distances", distances)
            , ("aggregates", aggregates)
            , ("weathermaxbins", weatherMaxBins)
            , ("windowplot", windowPlot)
            , ("joinaggregateplot", joinAggregatePlot)
            ]
            

cars :: Data
cars = dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

movies :: [Format] -> Data
movies = dataFromUrl "https://vega.github.io/vega-lite/data/movies.json"

us :: T.Text -> Data
us feature =  dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json"
              [ TopojsonFeature feature ]


pos :: Position -> FieldName -> Measurement -> BuildEncodingSpecs
pos ax n t = position ax [ PName n, PmType t ]

checkOrdering :: VegaLite
checkOrdering =
  let trans = transform
              -- calculateAs transform first to test that order of transforms is preserved.
              . calculateAs "datum.Acceleration" "myAcceleration"
              . aggregate [ opAs Mean "myAcceleration" "mean_acceleration" ]
                          [ "Cylinders" ]

      enc = encoding
            . pos X "Cylinders" Ordinal
            . pos Y "mean_acceleration" Quantitative

  in toVegaLite [ cars, trans [], enc [], mark Bar [] ]


binTransform :: [BinProperty] -> VegaLite
binTransform bProps =
  let trans = transform
              . calculateAs "datum.IMDB_Rating" "rating"
              . filter (FExpr "datum.rating != null")
              . binAs bProps "rating" "ratingGroup"

      enc = encoding
            . pos X "ratingGroup" Ordinal
            . position Y [ PAggregate Count
                         , PmType Quantitative
                         , PTitle "Number of movies" ]

  in toVegaLite [ width 400, movies [], trans [], enc [], mark Bar [] ]

binEmpty :: VegaLite
binEmpty = binTransform []

binStep :: VegaLite
binStep = binTransform [ Step 0.25 ]

imputeMean :: VegaLite
imputeMean =
  let imputeData =
        dataFromColumns []
        . dataColumn "A" (Numbers [ 0, 0, 1, 1, 2, 2, 3 ])
        . dataColumn "B" (Numbers [ 28, 91, 43, 55, 81, 53, 19 ])
        . dataColumn "C" (Numbers [ 0, 1, 0, 1, 0, 1, 0 ])

      trans =
        transform
        . calculateAs "datum.A" "a"
        . calculateAs "datum.B" "b"
        . calculateAs "datum.C" "c"
        . impute "b" "a" [ ImMethod ImMean
                         , ImGroupBy [ "c" ]
                         , ImFrame (Just (-2)) (Just 2)
                         ]

      enc =
        encoding
        . pos X "a" Quantitative
        . pos Y "b" Quantitative
        . color [ MName "c", MmType Nominal ]

      lopts = [ MStrokeDash [ 5, 10, 5 ]
              , MStrokeOpacity 0.5
              , MStrokeWidth 2
              ]

  in toVegaLite [ imputeData [], trans [], enc [], mark Line lopts ]


densityPlot :: VegaLite
densityPlot =
  let trans = transform
              . filter (FExpr "datum.IMDB_Rating != null")
              . density "IMDB_Rating" [ DnBandwidth 0.3 ]

      enc = encoding
            . position X [ PName "value", PmType Quantitative, PTitle "IMDB Rating" ]
            . pos Y "density" Quantitative

      aopts = [ MFillOpacity 0.8
              , MFill "teal"
              , MStroke "firebrick"
              , MStrokeWidth 2
              , MStrokeOpacity 0.7 ]
      
  in toVegaLite [ width 400, height 400, movies [], trans [], enc [], mark Area aopts ]


loessPlot :: VegaLite
loessPlot =
  let trans = transform
              . calculateAs "datum.IMDB_Rating" "imdbRating"
              . calculateAs "datum.Rotten_Tomatoes_Rating" "rtRating"
              . loess "imdbRating" "rtRating" [ LsBandwidth 0.1
                                              , LsAs "tx" "ty" ]

      enc1 = encoding
             . pos X "Rotten_Tomatoes_Rating" Quantitative
             . pos Y "IMDB_Rating" Quantitative

      enc2 = encoding
             . pos X "tx" Quantitative
             . pos Y "ty" Quantitative

      pointSpec = asSpec [ enc1 [], mark Point [ MFilled True, MOpacity 0.3 ] ]
      trendSpec = asSpec [ trans [], enc2 [], mark Line [ MColor "orange" ] ]
      
  in toVegaLite [ width 300, height 300, movies [], layer [ pointSpec, trendSpec ] ]


lookupPlot :: VegaLite
lookupPlot =
  let unemployed = dataFromUrl "https://vega.github.io/vega-lite/data/unemployment.tsv" []

      trans = transform
              . calculateAs "datum.id" "countyID"
              . lookup "countyID" unemployed "id" (LuFields [ "rate" ])

      proj = projection [ PrType AlbersUsa ]

      enc = encoding
            . color [ MName "rate"
                    , MmType Quantitative
                    , MScale [ SType ScQuantize, SScheme "category10" [ 10 ] ]
                    ]

  in toVegaLite [ width 500, height 300, us "counties", proj, trans [], enc [],
                  mark Geoshape [] ]


pivotPlot :: VegaLite
pivotPlot =
  let temps = dataFromColumns []
              . dataColumn "city" (Strings [ "Bristol", "Bristol", "Sheffield", "Sheffield", "Glasgow", "Glasgow" ])
              . dataColumn "temp" (Numbers [ 12, 14, 11, 13, 7, 10 ])
              . dataColumn "year" (Numbers [ 2017, 2018, 2017, 2018, 2017, 2018 ])

      trans =
        transform
        . calculateAs "datum.year" "Year"
        . calculateAs "datum.city" "City"
        . calculateAs "datum.temp" "Temperature"
        . pivot "Year" "Temperature" [ PiGroupBy [ "City" ] ]

      -- 2017 temperatures for the Bristol, Sheffield and Glasgow
      enc =
        encoding
        . pos X "2017" Quantitative
        . pos Y "City" Nominal

  in toVegaLite [ temps [], trans [], enc [], mark Circle [] ]


quantilePlot :: VegaLite
quantilePlot =
  let norm = dataFromUrl "https://vega.github.io/vega-lite/data/normal-2d.json" []

      trans =
        transform
        . quantile "u" [ QtStep 0.01, QtAs "p" "v" ]
        . calculateAs "quantileUniform(datum.p)" "unif"
        . calculateAs "quantileNormal(datum.p)" "norm"

      enc1 = encoding . pos X "unif" Quantitative . pos Y "v" Quantitative
      enc2 = encoding . pos X "norm" Quantitative . pos Y "v" Quantitative

  in toVegaLite [ norm
                , trans []
                , hConcat [ asSpec [ enc1 [], mark Point [] ]
                          , asSpec [ enc2 [], mark Point [] ]
                          ]
                ]

regressionPlot :: VegaLite
regressionPlot =
  let trans =
        transform
        . calculateAs "datum.IMDB_Rating" "imdbRating"
        . calculateAs "datum.Rotten_Tomatoes_Rating" "rtRating"
        . regression "imdbRating" "rtRating" [ RgMethod RgPoly
                                             , RgOrder 3
                                             , RgExtent 10 90 ]

      enc1 = encoding
             . pos X "Rotten_Tomatoes_Rating" Quantitative
             . pos Y "IMDB_Rating" Quantitative

      enc2 = encoding
             . pos X "rtRating" Quantitative
             . pos Y "imdbRating" Quantitative

      pointSpec = asSpec [ enc1 [], mark Point [ MFilled True, MOpacity 0.3 ] ]
      regSpec = asSpec [ trans [], enc2 [], mark Line [ MColor "firebrick" ] ]

  in toVegaLite [ width 300, height 300, movies [], layer [ pointSpec, regSpec ] ]


dummyData :: Value
dummyData = [aesonQQ|
[ { "key": "alpha", "foo": [ 1, 2 ], "bar": [ "A", "B" ] }
, { "key": "beta", "foo": [ 3, 4, 5 ], "bar": [ "C", "D" ] }
]
|]

flattenPlot :: VegaLite
flattenPlot =
  let dvals = dataFromJson dummyData []

      trans = transform . flattenAs [ "foo", "bar" ] [ "quant", "cat" ]

      enc = encoding
            . pos X "quant" Quantitative
            . pos Y "cat" Nominal
            . color [ MName "key", MmType Nominal ]

  in toVegaLite [ dvals, trans [], mark Circle [], enc [] ]


foldAsPlot :: VegaLite
foldAsPlot =
  let dvals = dataFromColumns []
              . dataColumn "country" (Strings [ "USA", "Canada" ])
              . dataColumn "gold" (Numbers [ 10, 7 ])
              . dataColumn "silver" (Numbers [ 20, 26 ])

      trans = transform
              . calculateAs "datum.gold" "goldMedals"
              . calculateAs "datum.silver" "silverMedals"
              . foldAs [ "goldMedals", "silverMedals" ] "k" "v"
              . calculateAs "datum.k" "year"
              . calculateAs "datum.v" "numberOfMedals"

      enc = encoding
            . column [ FName "year", FmType Nominal ]
            . pos X "country" Nominal
            . pos Y "numberOfMedals" Quantitative
            . color [ MName "country", MmType Nominal, MLegend [] ]

  in toVegaLite [ dvals [], trans [], mark Bar [], enc [] ]


stackPlot :: VegaLite
stackPlot =
  let trans = transform
              . aggregate [ opAs Count "" "count_*" ] [ "Origin", "Cylinders" ]
              . stack "count_*"
                       []
                       "stack_count_Origin1"
                       "stack_count_Origin2"
                       [ StOffset StNormalize, StSort [ WAscending "Origin" ] ]
              . window
                    [ ( [ WAggregateOp Min, WField "stack_count_Origin1" ], "x" )
                    , ( [ WAggregateOp Max, WField "stack_count_Origin2" ], "x2" )
                    ]
                    [ WFrame Nothing Nothing, WGroupBy [ "Origin" ] ]
              . stack "count_*"
                    [ "Origin" ]
                    "y"
                    "y2"
                    [ StOffset StNormalize, StSort [ WAscending "Cylinders" ] ]

      enc = encoding
            . position X [ PName "x", PmType Quantitative, PAxis [] ]
            . position X2 [ PName "x2" ]
            . position Y [ PName "y", PmType Quantitative, PAxis [] ]
            . position Y2 [ PName "y2" ]
            . color [ MName "Origin", MmType Nominal ]
            . opacity [ MName "Cylinders", MmType Quantitative, MLegend [] ]
            . tooltips
                    [ [ TName "Origin", TmType Nominal ]
                    , [ TName "Cylinders", TmType Quantitative ]
                    ]


  in toVegaLite [ cars, trans [], enc [], mark Rect [] ]


weather :: TimeUnit -> FieldName -> VegaLite
weather tunit field =
  let weatherData = dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv"
                    [ Parse [ ( "date", FoDate "%Y/%m/%d" ) ] ]

      trans = transform
              . calculateAs "datum.date" "sampleDate"
              . calculateAs "datum.temp_max" "maxTemp"
              . timeUnitAs tunit "sampleDate" field

      enc = encoding
            . position X [ PName field, PmType Temporal, PAxis [ AxFormat "%b" ] ]
            . position Y [ PName "maxTemp", PmType Quantitative, PAggregate Max ]

  in toVegaLite [ width 400
                , weatherData
                , trans []
                , enc []
                , mark Line [ MPoint (PMMarker [ MFill "black" ]) ]
                ]

weatherByMonth :: VegaLite
weatherByMonth = weather (TU Month) "month"

weatherByTwoMonths :: VegaLite
weatherByTwoMonths = weather (TUStep 2 Month) "bimonth"

weatherMaxBins :: VegaLite
weatherMaxBins = weather (TUMaxBins 3) "tbin"


distances :: VegaLite
distances =
  let dateTime mnt = "Sun, 01 Jan 2012 00:0" <> T.pack (show mnt) <> ":00"
      dates = map dateTime [1 :: Int .. 15]

      dvals = dataFromColumns []
              . dataColumn "date" (Strings dates)
              . dataColumn "distance" (Numbers [ 1, 1, 2, 1, 4, 2, 5, 2, 6, 4, 1, 1, 3, 0, 2, 3 ])

      enc = encoding
            . position X [ PName "date"
                         , PmType Temporal
                         , PTimeUnit (TUMaxBins 15) ]
            . position Y [ PName "distance"
                         , PmType Quantitative
                         , PAggregate Sum ]

  in toVegaLite [ dvals [], enc [], mark Bar [] ]


aggregates :: VegaLite
aggregates =
  let dvals = dataFromColumns []
              . dataColumn "xs" (Strings ["A", "A", "B", "A", "B", "B", "C", "C"])
              . dataColumn "ys" (Numbers [1, 2, 2, 2, 3, 2, 4, 8])

      enc op = encoding (position Y [ PName "ys"
                                    , PAxis [AxTitle "Y"]
                                    , PmType Quantitative
                                    , PAggregate op ] [])

      lyr1 = asSpec [ enc Sum, mark Bar [MStroke "black"] ]
      lyr2 = asSpec [ enc Product, mark Line [MColor "brown"] ]
      lyr3 = asSpec [ enc Max, mark Point [MColor "orange"] ]
      lyr4 = asSpec [ enc Distinct, mark Square [MColor "cyan"] ]

      axOpts = PAxis [AxTitle "X", AxLabelAngle 0]

  in toVegaLite [ dvals []
                , encoding (position X [PName "xs", axOpts, PmType Ordinal] [])
                , layer [lyr1, lyr2, lyr3, lyr4]
                ]


activityData :: Data
activityData =
  dataFromColumns []
  . dataColumn "Activity" (Strings [ "Sleeping", "Eating", "TV", "Work", "Exercise" ])
  . dataColumn "Time" (Numbers [ 8, 2, 4, 8, 2 ])
  $ []

windowPlot :: VegaLite
windowPlot =
  let trans = transform
              . window [ ( [ WAggregateOp Sum, WField "Time" ], "TotalTime" ) ]
                       [ WFrame Nothing Nothing ]
              . calculateAs "datum.Time/datum.TotalTime * 100" "PercentOfTotal"

      enc = encoding
            . position X [ PName "PercentOfTotal", PmType Quantitative, PTitle "% of total time" ]
            . pos Y "Activity" Nominal

  in toVegaLite
        [ heightStep 12
        , activityData
        , trans []
        , mark Bar []
        , enc []
        ]

joinAggregatePlot :: VegaLite
joinAggregatePlot =
  let trans = transform
              . joinAggregate [ opAs Sum "Time" "TotalTime" ] []
              . calculateAs "datum.Time/datum.TotalTime * 100" "PercentOfTotal"

      enc = encoding
            . position X [ PName "PercentOfTotal", PmType Quantitative, PTitle "% of total time" ]
            . pos Y "Activity" Nominal

  in toVegaLite
        [ heightStep 12
        , activityData
        , trans []
        , mark Bar []
        , enc []
        ]
