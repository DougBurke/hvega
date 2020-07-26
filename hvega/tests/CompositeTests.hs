{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite CompositeTests.elm as of version 1.12.0
--
module CompositeTests (testSpecs) where

import qualified Data.Text as T

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Graphics.Vega.VegaLite


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("boxplot1", boxplot1)
            , ("boxplot2", boxplot2)
            , ("boxplot3", boxplot3)
            , ("boxplotnobox", boxplotNoBox)
            , ("boxplotnooutliers", boxplotNoOutliers)
            , ("boxplotnomedian", boxplotNoMedian)
            , ("boxplotnorule", boxplotNoRule)
            , ("boxplotnoticks", boxplotNoTicks)
            , ("errorband1", errorband1)
            , ("errorband1no", errorband1No)
            , ("errorband2", errorband2)
            , ("errorband2no", errorband2No)
            , ("errorbar1", errorbar1)
            , ("errorbar2", errorbar2)
            , ("errorbar3", errorbar3)
            , ("errorbar3no", errorbar3No)
            , ("errorbar4", errorbar4)
            , ("errorbar5", errorbar5)
            , ("errorbar6", errorbar6)
            , ("errorbar7", errorbar7)
            ]


-- help in converting from the Elm version
pName :: T.Text -> PositionChannel
pName = PName

pOrdinal, pQuant :: PositionChannel
pOrdinal = PmType Ordinal
pQuant = PmType Quantitative


bPlot :: MarkErrorExtent -> [MarkProperty] -> [MarkProperty] -> VegaLite
bPlot ext mops def =
  let pop = dataFromUrl "https://vega.github.io/vega-lite/data/population.json" []

      enc = encoding
            . position X [ PName "age", PmType Ordinal ]
            . position Y [ PName "people", PmType Quantitative, PAxis [ AxTitle "Population" ] ]

      -- special case the empty list so as not to change the boxplot1/2/3 output
      -- (created before this capability was added).
      --
      v = [ pop, mark Boxplot (MExtent ext : mops), enc [] ]
      vs = case def of
             [] -> v
             _ -> configure (configuration (BoxplotStyle def) []) : v

    in toVegaLite vs

boxplot1 :: VegaLite
boxplot1 = bPlot ExRange [] []

boxplot2 :: VegaLite
boxplot2 = bPlot (IqrScale 2) [] []

boxplot3 :: VegaLite
boxplot3 =
    let mopts = [ MBox [ MColor "firebrick" ]
                , MOutliers [ MColor "black", MStrokeWidth 0.3, MSize 10 ]
                , MMedian [ MSize 18, MFill "black", MStrokeWidth 0 ]
                , MRule [ MStrokeWidth 0.4 ]
                , MTicks [ MSize 8 ]
                ]

    in bPlot (IqrScale 0.5) mopts []

-- Could combine into one plot, but useful to see the individual elements turned off
--
-- We need to set the default values to turn on ticks (since they are off by
-- default).
--

defConfig :: [MarkProperty]
defConfig = [ MTicks [] ]

boxplotNoBox, boxplotNoOutliers, boxplotNoMedian, boxplotNoRule, boxplotNoTicks :: VegaLite
boxplotNoBox = bPlot (IqrScale 0.5) [ MNoBox ] defConfig
boxplotNoOutliers = bPlot (IqrScale 0.5) [ MNoOutliers ] defConfig
boxplotNoMedian = bPlot (IqrScale 0.5) [ MNoMedian ] defConfig
boxplotNoRule = bPlot (IqrScale 0.5) [ MNoRule ] defConfig
boxplotNoTicks = bPlot (IqrScale 0.5) [ MNoTicks ] defConfig

-- Note: at present only called with ci or stdev arguments

eBand :: T.Text -> Bool -> VegaLite
eBand ext hasBorders =
    let
        cars =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        label =
            case ext of
                "ci" ->
                    "(95% CI)"

                "stdev" ->
                    "(1 stdev)"

                "stderr" ->
                    "(1 std Error)"

                "range" ->
                    "(min to max)"

                _ ->
                    "(IQR)"

        summary =
            case ext of
                "ci" ->
                    ConfidenceInterval

                "stdev" ->
                    StdDev

                "stderr" ->
                    StdErr

  {-
                "range" ->
                    ExRange
  -}
  
                _ ->
                    Iqr

        enc =
            encoding
                . position X [ PName "Year", PmType Temporal, PTimeUnit (TU Year) ]
                . position Y
                    [ PName "Miles_per_Gallon"
                    , PmType Quantitative
                    , PScale [ SZero False ]
                    , PTitle ("Miles per Gallon " <> label)
                    ]

        mopts = [ MExtent summary
                , MInterpolate Monotone
                , if hasBorders then MBorders [] else MNoBorders
                ]

    in
    toVegaLite [ cars
               , mark ErrorBand mopts
               , enc [] ]


errorband1, errorband1No :: VegaLite
errorband1 = eBand "ci" True
errorband1No = eBand "ci" False


errorband2, errorband2No :: VegaLite
errorband2 = eBand "stdev" True
errorband2No = eBand "stdev" False


barley :: Data
barley = dataFromUrl "https://vega.github.io/vega-lite/data/barley.json" []


eBar :: MarkErrorExtent -> VegaLite
eBar ext =
    let enc =
            encoding
                . position X [ PName "yield", PmType Quantitative
                             , PScale [ SZero False ] ]
                . position Y
                    [ PName "variety"
                    , PmType Ordinal
                    ]

        mopts = [ MExtent ext, MTicks [ MStroke "blue" ] ]
        
    in
    toVegaLite [ barley
               , mark ErrorBar mopts
               , enc [] ]


errorbar1 :: VegaLite
errorbar1 =
    eBar ConfidenceInterval


errorbar2 :: VegaLite
errorbar2 =
    eBar StdDev


ebarsColor :: Bool -> VegaLite
ebarsColor hasTicks =
    let
        des =
            description "Error bars with color encoding"

        specErrorBars =
            asSpec [ mark ErrorBar [ if hasTicks then MTicks [] else MNoTicks ]
                   , encErrorBars [] ]

        encErrorBars =
            encoding
                . position X [ PName "yield", PmType Quantitative
                             , PScale [ SZero False ] ]
                . position Y [ PName "variety", PmType Ordinal ]
                . color [ MString "#4682b4" ]

        specPoints =
            asSpec [ mark Point [ MFilled True, MColor "black" ], encPoints [] ]

        encPoints =
            encoding
                . position X [ PName "yield", PmType Quantitative
                             , PAggregate Mean ]
                . position Y [ PName "variety", PmType Ordinal ]
    in
    toVegaLite
        [ des
        , barley
        , layer [ specErrorBars, specPoints ]
        ]


errorbar3, errorbar3No :: VegaLite
errorbar3 = ebarsColor True
errorbar3No = ebarsColor False


yieldData :: [DataColumn] -> Data
yieldData =
  dataFromColumns []
  . dataColumn "yieldError" (Numbers [ 7.55, 6.98, 3.92, 11.97 ])
  . dataColumn "yieldError2" (Numbers [ -10.55, -3.98, -0.92, -15.97 ])
  . dataColumn "yield" (Numbers [ 32.4, 30.97, 33.96, 30.45 ])
  . dataColumn "variety" (Strings [ "Glabron", "Manchuria", "No. 457", "No. 462" ])

  
errorbar4 :: VegaLite
errorbar4 =
  let des = description "Symetric error bars encoded with xError channel"

      specErrorBars = asSpec [ mark ErrorBar [ MTicks [] ], encErrorBars [] ]

      encErrorBars = encoding
                     . position X [ pName "yield", pQuant
                                  , PScale [ SZero False ] ]
                     . position Y [ pName "variety", pOrdinal ]
                     . position XError [ pName "yieldError" ]

      specPoints = asSpec [ mark Point [ MFilled True, MColor "black" ]
                          , encPoints [] ]

      encPoints = encoding
                  . position X [ pName "yield", pQuant ]
                  . position Y [ pName "variety", pOrdinal ]

  in toVegaLite [ des, yieldData []
                , layer [ specErrorBars, specPoints ]
                ]


errorbar5 :: VegaLite
errorbar5 =
  let des = description "Asymetric error bars encoded with xError and xError2 channels"
  
      specErrorBars = asSpec [ mark ErrorBar [ MTicks [] ]
                             , encErrorBars [] ]

      encErrorBars =
        encoding
        . position X [ pName "yield", pQuant, PScale [ SZero False ] ]
        . position Y [ pName "variety", pOrdinal ]
        . position XError [ pName "yieldError" ]
        . position XError2 [ pName "yieldError2" ]

      specPoints = asSpec [ mark Point [ MFilled True, MColor "black" ]
                          , encPoints [] ]

      encPoints = encoding
                  . position X [ pName "yield", pQuant ]
                  . position Y [ pName "variety", pOrdinal ]

  in toVegaLite [ des, yieldData [], layer [ specErrorBars, specPoints ] ]


errorbar6 :: VegaLite
errorbar6 =
  let des = description "Symetric error bars encoded with yError channel"

      specErrorBars = asSpec [ mark ErrorBar [ MTicks [] ], encErrorBars [] ]
      encErrorBars = encoding
                     . position Y [ pName "yield", pQuant, PScale [ SZero False ] ]
                     . position X [ pName "variety", pOrdinal ]
                     . position YError [ pName "yieldError" ]

      specPoints = asSpec [ mark Point [ MFilled True, MColor "black" ]
                          , encPoints [] ]
      encPoints = encoding
                  . position Y [ pName "yield", pQuant ]
                  . position X [ pName "variety", pOrdinal ]

  in toVegaLite [ des, yieldData [], layer [ specErrorBars, specPoints ] ]


errorbar7 :: VegaLite
errorbar7 =
  let des = description "Asymetric error bars encoded with yError and yError2 channels"

      specErrorBars = asSpec [ mark ErrorBar [ MTicks [] ], encErrorBars [] ]
      encErrorBars = encoding
                     . position Y [ pName "yield", pQuant, PScale [ SZero False ] ]
                     . position X [ pName "variety", pOrdinal ]
                     . position YError [ pName "yieldError" ]
                     . position YError2 [ pName "yieldError2" ]

      specPoints = asSpec [ mark Point [ MFilled True, MColor "black" ]
                          , encPoints [] ]
      encPoints = encoding
                  . position Y [ pName "yield", pQuant ]
                  . position X [ pName "variety", pOrdinal ]

  in toVegaLite [ des, yieldData [], layer [ specErrorBars, specPoints ] ]
