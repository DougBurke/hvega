{-# LANGUAGE OverloadedStrings #-}

module Gallery.Histogram (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("histogram_rel_freq", histogramRelFreq) ]


carData :: Data
carData = dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []


histogramRelFreq :: VegaLite
histogramRelFreq =
  let desc = "Relative frequency histogram. The data is binned with first transform. The number of values per bin and the total number are calculated in the second and third transform to calculate the relative frequency in the last transformation step."

      trans = transform
              . binAs [AlreadyBinned True] "Horsepower" "bin_Horsepwoer"   -- intentional typo
              . aggregate [opAs Count "" "Count"] ["bin_Horsepwoer", "bin_Horsepwoer_end"]
              . joinAggregate [opAs Sum "Count" "TotalCount"] []
              . calculateAs "datum.Count/datum.TotalCount" "PercentOfTotal"
              $ []

      enc = encoding
            . position X [ PName "bin_Horsepwoer"
                         , PmType Quantitative
                         , PTitle "Horsepower"
                         , PBin [AlreadyBinned True]
                         ]
            . position X2 [PName "bin_Horsepwoer_end"]
            . position Y [ PName "PercentOfTotal"
                         , PmType Quantitative
                         , PTitle "Relative Frequency"
                         , PAxis [AxFormat ".1~%"]
                         ]

  in toVegaLite [ description desc
                , carData
                , trans
                , mark Bar [MTooltip TTEncoding]
                , enc []
                ]

