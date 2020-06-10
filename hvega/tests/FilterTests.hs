{-# LANGUAGE OverloadedStrings #-}

module FilterTests (testSpecs) where

import Data.Function ((&))

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("datedate", dateDate)
            , ("datedatell", dateDateLL)
            , ("datedateul", dateDateUL)
            , ("datenumber", dateNumber)
            , ("datenumberll", dateNumberLL)
            , ("datenumberul", dateNumberUL)
            ]

stockData :: Data
stockData = dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []

goog :: [TransformSpec] -> PropertySpec
goog = transform . filter (FExpr "datum.symbol === 'GOOG'")

baseEnc :: [EncodingSpec] -> PropertySpec
baseEnc = encoding
          . position X [ PName "date", PmType Temporal, PAxis [ AxFormat "%m %Y" ] ]
          . position Y [ PName "price", PmType Quantitative ]


dateFilter :: Filter -> VegaLite
dateFilter fexpr =
  toVegaLite [ stockData
             , goog
               . filter fexpr
               $ []
             , width 400
             , mark Line []
             , baseEnc []
             , configure
               . configuration (Axis [DomainColor "#ddd", TickColor "#ddd"])
               . configuration (LineBreakStyle " ")
               $ []
             ]

  
dateFilterNumbers :: Maybe Double -> Maybe Double -> VegaLite
dateFilterNumbers mlo mhi =
  let yearRange = FRange "date" frange
                  & FilterOpTrans (MTimeUnit (TU Year))
                  & FCompose

      frange = case (mlo, mhi) of
                 (Just lo, Just hi) -> NumberRange lo hi
                 (Just lo, Nothing) -> NumberRangeLL lo
                 (Nothing, Just hi) -> NumberRangeUL hi
                 _ -> error "Internal error"

  in dateFilter yearRange


dateFilterDates :: Maybe Int -> Maybe Int -> VegaLite
dateFilterDates mlo mhi =
  let yearRange = FRange "date" frange

      frange = case (mlo, mhi) of
                 (Just lo, Just hi) -> DateRange [DTYear lo] [DTYear hi]
                 (Just lo, Nothing) -> DateRange [DTYear lo] []
                 (Nothing, Just hi) -> DateRange [] [DTYear hi]
                 _ -> error "Internal error"

  in dateFilter yearRange


dateDate, dateDateLL, dateDateUL :: VegaLite
dateDate = dateFilterDates (Just 2006) (Just 2007)
dateDateLL = dateFilterDates (Just 2006) Nothing
dateDateUL = dateFilterDates Nothing (Just 2007)


dateNumber, dateNumberLL, dateNumberUL :: VegaLite
dateNumber = dateFilterNumbers (Just 2006) (Just 2007)
dateNumberLL = dateFilterNumbers (Just 2006) Nothing
dateNumberUL = dateFilterNumbers Nothing (Just 2007)
