{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite TextFormatTests.elm as of version 1.12.0
--

module TextFormatTests (testSpecs) where

import qualified Data.Text as T

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("textFormat1", textFormat1)
            , ("textFormat2", textFormat2)
            ]

textFormat1 :: VegaLite
textFormat1 =
    let
        xs = map (T.pack . show) [1981 .. 2001 :: Int]
        ys = map (\n -> 2011 + 1991 - n) [1991 .. 2011]
        
        dataVals =
            dataFromColumns []
                . dataColumn "a" (Strings xs)
                . dataColumn "b" (Numbers ys)

        encSym =
            encoding
                . position X [ PName "a", PmType Temporal ]
                . position Y [ PName "b", PmType Quantitative ]

        specSym =
            asSpec [ mark Circle [], encSym [] ]

        encLabel =
            encoding
                . position X [ PName "a", PmType Temporal
                             , PAxis [ AxFormatAsTemporal
                                     , AxFormat "%y" ] ]
                . position Y [ PName "b", PmType Quantitative
                             , PScale [ SZero False ]
                             , PAxis [ AxFormatAsNum
                                     , AxFormat ".2f" ] ]
                . text [ TName "a", TmType Temporal
                       , TFormatAsTemporal
                       , TFormat "%b `%y" ]
                . color [ MName "a", MmType Temporal
                        , MLegend [ LFormatAsTemporal
                                  , LFormat "%b %Y" ] ]

        specLabel =
            asSpec [ mark Text [ MdY 4, MdX 22 ], encLabel [] ]
    in
    toVegaLite [ width 600, height 400, dataVals []
               , layer [ specSym, specLabel ] ]

textFormat2 :: VegaLite
textFormat2 =
    let
        dataVals =
            dataFromUrl "https://gicentre.github.io/data/westMidlands/westMidsCrimesAggregated.tsv"

        trans =
            transform
                . filter (FExpr "datum.month >= '2016'")

        enc =
            encoding
                . position X [ PName "crimeType", PmType Nominal
                             , PAxis [ AxTitle "" ] ]
                . position Y [ PName "reportedCrimes", PmType Quantitative
                             , PAggregate Sum ]
                . color [ MName "crimeType", MmType Nominal, MLegend [] ]
                . column [ FName "month", FmType Temporal
                         , FHeader [ HFormatAsTemporal
                                   , HFormat "%b %y" ] ]
    in
    toVegaLite [ width 100, dataVals [], trans []
               , mark Bar [], enc [] ]
