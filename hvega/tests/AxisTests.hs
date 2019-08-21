{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite AxisTests.elm in the 2.0 development version
--
module AxisTests (testSpecs) where

import qualified Data.Text as T

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("axis1", axis1)
            , ("axis2", axis2)
            , ("axis3", axis3)
            , ("axis4", axis4)
            -- , ("axis5", axis5)  require improved AxValues support
            , ("axis6", axis6)
            -- , ("axis7", axis7)  require AxLabelExpr support (VL 4)
            -- , ("axis8", axis8)  require AxLabelExpr support (VL 4)
            ]


-- We do not provide these in hvega, so define them here to make copying
-- the Elm tests over easier.
--
pOrdinal, pQuant, pTemporal :: PositionChannel
pOrdinal = PmType Ordinal
pQuant = PmType Quantitative
pTemporal = PmType Temporal

pName :: T.Text -> PositionChannel
pName = PName


simpleData :: [DataColumn] -> Data
simpleData =
  let xs = map fromIntegral [1::Int .. 100]
  in dataFromColumns []
       . dataColumn "x" (Numbers xs)
       . dataColumn "catX" (Strings (map (T.pack . show) xs))
       . dataColumn "y" (Numbers xs)


temporalData :: [DataColumn] -> Data
temporalData =
  let dates = [ "2019-01-01 09:00:00"
              , "2019-01-02 09:00:00"
              , "2019-01-03 09:00:00"
              , "2019-01-04 09:00:00"
              , "2019-01-05 09:00:00"
              , "2019-01-06 09:00:00"
              , "2019-01-07 09:00:00"
              , "2019-01-08 09:00:00"
              , "2019-01-09 09:00:00"
              , "2019-01-10 09:00:00"
              , "2019-01-11 09:00:00"
              , "2019-01-12 09:00:00"
              ]

      xs = map fromIntegral [1::Int .. 12]

  in dataFromColumns []
     . dataColumn "date" (Strings dates)
     . dataColumn "y" (Numbers xs)


axis1 :: VegaLite
axis1 =
  let enc = encoding
              . position X [ pName "x", pQuant ]
              . position Y [ pName "y", pQuant ]
    in
    toVegaLite [ simpleData [], enc [], mark Line [ MPoint (PMMarker []) ] ]


axis2 :: VegaLite
axis2 =
  let enc = encoding
              . position X [ pName "catX", pOrdinal ]
              . position Y [ pName "y", pQuant ]
    in
    toVegaLite [ simpleData [], enc [], mark Line [ MPoint (PMMarker []) ] ]


axis3 :: VegaLite
axis3 =
  let enc = encoding
              . position X [ pName "date", pTemporal ]
              . position Y [ pName "y", pQuant ]
    in
    toVegaLite [ simpleData [], enc [], mark Line [ MPoint (PMMarker []) ] ]


axis4 :: VegaLite
axis4 =
  let enc = encoding
              . position X [ pName "x"
                           , pQuant
                           , PAxis [AxValues [1, 25, 39, 90]]
                           ]
              . position Y [ pName "y", pQuant ]
    in
    toVegaLite [ simpleData [], enc [], mark Line [ MPoint (PMMarker []) ] ]


{-
axis5 :: VegaLite
axis5 =
  let enc = encoding
              . position X [ pName "catX"
                           , pOrdinal
                           , PAxis [AxValues ["1", "25", "39", "dummy", "90"]]
                           ]
              . position Y [ pName "y", pQuant ]
    in
    toVegaLite [ simpleData [], enc [], mark Line [ MPoint (PMMarker []) ] ]
-}


axis6 :: VegaLite
axis6 =
  let enc = encoding
              . position X [ pName "date"
                           , pTemporal
                           , PAxis [AxDates axDates]
                           ]
              . position Y [ pName "y", pQuant ]

      axDates = [ [DTYear 2019, DTMonth Jan, DTDate 4 ]
                , [DTYear 2019, DTMonth Jan, DTDate 8 ]
                , [DTYear 2019, DTMonth Jan, DTDate 20 ]
                ]
                
  in 
    toVegaLite [ temporalData [], enc [], mark Line [ MPoint (PMMarker []) ] ]


{-
axis7 :: VegaLite
axis7 =
  let enc = encoding
              . position X [ pName "x"
                           , pQuant
                           , PAxis [AxLabelExpr "datum.value / 100"]
                           ]
              . position Y [ pName "y", pQuant ]
  in
    toVegaLite [ simpleData [], enc [], mark Line [ MPoint (PMMarker []) ] ]


axis8 :: VegaLite
axis8 =
  let enc = encoding
              . position X [ pName "catX"
                           , pOrdinal
                           , PAxis [AxLabelExpr "'number' + datum.label"]
                           ]
              . position Y [ pName "y", pQuant ]
  in
    toVegaLite [ simpleData [], enc [], mark Line [ MPoint (PMMarker []) ] ]

-}
