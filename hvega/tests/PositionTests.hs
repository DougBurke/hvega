{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite PositionTests.elm as of version 1.12.0
--

module PositionTests (testSpecs) where

import qualified Data.Text as T

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("position1", position1)
            , ("position2", position2)
            , ("position3", position3)
            , ("position4", position4)
            , ("position5", position5)
            , ("position6", position6)
            , ("position7", position7)
            , ("position8", position8)
            , ("position9", position9)
            , ("position10", position10)
            , ("position11", position11)
            , ("position12", position12)
            , ("position8r", position8r)
            , ("position9r", position9r)
            , ("position10r", position10r)
            , ("position11r", position11r)
            , ("position12r", position12r)
            ]


pOrdinal, pQuant :: PositionChannel
pOrdinal = PmType Ordinal
pQuant = PmType Quantitative

pName :: T.Text -> PositionChannel
pName = PName

bar :: [MarkProperty] -> PropertySpec
bar = mark Bar


emptyData, someData :: [DataColumn] -> Data
emptyData =
    dataFromColumns []
        . dataColumn "empty" (Numbers [0])

-- make sure this is not symmetric in x
someData =
    dataFromColumns []
        . dataColumn "cat" (Numbers [1, 2, 3, 4, 5])
        . dataColumn "val" (Numbers [10, 20, 30, 15, 12])
        . dataColumn "empty" (Numbers [0])


position1 :: VegaLite
position1 =
    toVegaLite [ emptyData []
               , mark Circle [ MX 150, MY 150, MSize 200 ] ]

position2 :: VegaLite
position2 =
    toVegaLite [ emptyData []
               , mark Bar [ MX 150, MY 150 ] ]


position3 :: VegaLite
position3 =
    toVegaLite [ emptyData []
               , mark Bar [ MX 150, MY 150, MX2 200 ] ]


position4 :: VegaLite
position4 =
    toVegaLite [ emptyData []
               , mark Bar [ MX 150, MY 150, MY2 200 ] ]


position5 :: VegaLite
position5 =
    toVegaLite [ emptyData []
               , mark Bar [ MX 150, MY 150, MX2 200, MY2 200 ] ]


position6 :: VegaLite
position6 =
    let
        enc =
            encoding
                . position X [ pName "cat", pOrdinal ]
                . position Y [ pName "val", pQuant ]
    in
    toVegaLite [ width 300, someData [], enc [], bar [ MWidth 20 ] ]


position7 :: VegaLite
position7 =
    let
        enc =
            encoding
                . position X [ pName "val", pQuant ]
                . position Y [ pName "cat", pOrdinal ]
    in
    toVegaLite [ height 300, someData [], enc [], bar [ MHeight 20 ] ]


bAlign :: [ScaleProperty] -> Double -> VegaLite
bAlign sOpts x =
    let
        enc =
            encoding
                . position X
                    [ pName "cat"
                    , pOrdinal
                    , PScale ([ SAlign x, SPaddingInner 0.5 ] ++ sOpts)
                    ]
                . position Y [ pName "val", pQuant ]
    in
    toVegaLite [ width 400, someData [], enc [], bar [] ]


barAlign :: Double -> VegaLite
barAlign = bAlign []


barAlignR :: Double -> VegaLite
barAlignR = bAlign [SReverse True]


position8, position9, position10, position11, position12 :: VegaLite
position8 = barAlign (-10)  -- test clamping
position9 = barAlign 0.3
position10 = barAlign 0.5
position11 = barAlign 0.7
position12 = barAlign 10    -- test clamping

position8r, position9r, position10r, position11r, position12r :: VegaLite
position8r = barAlignR (-10)  -- test clamping
position9r = barAlignR 0.3
position10r = barAlignR 0.5
position11r = barAlignR 0.7
position12r = barAlignR 10    -- test clamping
