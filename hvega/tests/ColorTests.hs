{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ColorTests.elm as of version 1.12.0
--
-- The following tests do not meet the VegaLite spec (v3.3.0):
--    interp1          -- SInterpolate should only accept Rgb/CubeHelix[Long]
--    interp2
--    interp3
--    interp4
--    interp5
--

module ColorTests (testSpecs) where

import qualified Data.Text as T

import Graphics.Vega.VegaLite

import Prelude hiding (filter, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("defContinuous", defContinuous)
            , ("defOrdinal", defOrdinal)
            , ("defNominal", defNominal)
            , ("namedContinuous1", namedContinuous1)
            , ("namedContinuous2", namedContinuous2)
            , ("namedContinuous3", namedContinuous3)
            , ("namedContinuous4", namedContinuous4)
            , ("customContinuous", customContinuous)
            , ("customDiscrete", customDiscrete)
            , ("scale1", scale1)
            , ("scale3", scale3)
            , ("scale4", scale4)
            , ("scale5", scale5)
            , ("scale6", scale6)
            , ("scale7", scale7)
            , ("scale8", scale8)
            , ("interp1", interp1)
            , ("interp2", interp2)
            , ("interp3", interp3)
            , ("interp4", interp4)
            , ("interp5", interp5)
            , ("interp6", interp6)
            , ("interp7", interp7)
            , ("gamma1", gamma1)
            , ("gamma2", gamma2)
            , ("gamma3", gamma3)
            , ("gamma4", gamma4)
            , ("gamma5", gamma5)
            ]


chart :: T.Text -> ([a] -> [EncodingSpec]) -> VegaLite
chart desText enc =
    let des = description desText

        dataVals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json"

        trans =
            transform
                . calculateAs "(datum.Acceleration - 15.52)/2.80" "accelerationZScore"

        fullEnc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . size [ MNumber 60 ]
                . opacity [ MNumber 1 ]
                . enc
    in
    toVegaLite [ des, dataVals [], trans [], mark Circle [], fullEnc [] ]


defContinuous :: VegaLite
defContinuous =
    chart "Default continuous colour scales."
        (color [ MName "Acceleration", MmType Quantitative ])


defOrdinal :: VegaLite
defOrdinal =
    chart "Default ordinal colour scales."
        (color [ MName "Cylinders", MmType Ordinal ])


defNominal :: VegaLite
defNominal =
    chart "Default nominal colour scales."
        (color [ MName "Origin", MmType Nominal ])


namedContinuous1 :: VegaLite
namedContinuous1 =
    chart "Continuous colour scale based on named vega schame. Should use the entire plasma colour scheme."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SScheme "plasma" [] ] ])


namedContinuous2 :: VegaLite
namedContinuous2 =
    chart "Continuous colour scale based on named vega schame. Should use the upper half of the plasma colour scheme."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SScheme "plasma" [ 0.5, 1 ] ] ])


namedContinuous3 :: VegaLite
namedContinuous3 =
    chart "Continuous colour scale based on named vega schame. Should use the flipped plasma colour scheme (i.e. red to orange)."
        (color [ MName "Acceleration"
               , MmType Quantitative
               , MScale [ SScheme "plasma" [] ]
               , MSort [ Descending ]
               ])


namedContinuous4 :: VegaLite
namedContinuous4 =
    chart "Continuous colour scale based on named vega schame. Should use the first half of the flipped plasma colour scheme (i.e. red to orange)."
        (color [ MName "Acceleration"
               , MmType Quantitative
               , MScale [ SScheme "plasma" [ 0, 0.5 ] ]
               , MSort [ Descending ]
               ])


customContinuous :: VegaLite
customContinuous =
    chart "Custom continuous colour scheme (red to blue ramp)."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SRange (RStrings [ "#f33", "#33f" ]) ] ])


customDiscrete :: VegaLite
customDiscrete =
    chart "Custom discrete colours (red, green, blue)."
        (color [ MName "Origin", MmType Nominal, MScale [ SRange (RStrings [ "#e33", "#3a3", "#33d" ]) ] ])


scale1 :: VegaLite
scale1 =
    chart "Linear (default) colour scale."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


{- ScSequential has been removed from hvega, with the suggstion to use ScLinear instead,
   which is tested in scale1. This is left in mainly to explain the test numbering.
scale2 :: VegaLite
scale2 =
    chart "Sequential (deprecated) colour scale."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SType ScSequential, SRange (RStrings [ "yellow", "red" ]) ] ])
-}

scale3 :: VegaLite
scale3 =
    chart "Power colour scale with default (1) exponent."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SType ScPow, SRange (RStrings [ "yellow", "red" ]) ] ])

scale4 :: VegaLite
scale4 =
    chart "Cubic Power colour scale."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SType ScPow, SExponent 3, SRange (RStrings [ "yellow", "red" ]) ] ])

scale5 :: VegaLite
scale5 =
    chart "Square root colour scale."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SType ScSqrt, SRange (RStrings [ "yellow", "red" ]) ] ])


scale6 :: VegaLite
scale6 =
    chart "Log colour scale."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SType ScLog, SRange (RStrings [ "yellow", "red" ]) ] ])

scale7 :: VegaLite
scale7 =
    chart "SymLog colour scale with default slope constant (1)."
        (color [ MName "accelerationZScore", MmType Quantitative, MScale [ SType ScSymLog, SRange (RStrings [ "yellow", "red" ]) ] ])

scale8 :: VegaLite
scale8 =
    chart "SymLog colour scale with slope constant of 0.01."
        (color [ MName "accelerationZScore", MmType Quantitative, MScale [ SType ScSymLog, SConstant 0.01, SRange (RStrings [ "yellow", "red" ]) ] ])

interp1 :: VegaLite
interp1 =
    chart "HSL interpolation."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate Hsl, SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


interp2 :: VegaLite
interp2 =
    chart "HSL-long interpolation."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate HslLong, SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


interp3 :: VegaLite
interp3 =
    chart "Lab interpolation."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate Lab, SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


interp4 :: VegaLite
interp4 =
    chart "HCL interpolation."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate Hcl, SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


interp5 :: VegaLite
interp5 =
    chart "HCL-long interpolation."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate HclLong, SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


interp6 :: VegaLite
interp6 =
    chart "cube-helix interpolation."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate (CubeHelix 1), SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


interp7 :: VegaLite
interp7 =
    chart "cube-helix-long interpolation."
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate (CubeHelixLong 1), SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


gamma1 :: VegaLite
gamma1 =
    chart "cube-helix-long interpolation, gamma of -0.1"
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate (CubeHelixLong (-0.1)), SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


gamma2 :: VegaLite
gamma2 =
    chart "cube-helix-long interpolation, gamma of 0"
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate (CubeHelixLong 0), SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


gamma3 :: VegaLite
gamma3 =
    chart "cube-helix-long interpolation with default gamma value of 1"
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate (CubeHelixLong 1), SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


gamma4 :: VegaLite
gamma4 =
    chart "cube-helix-long interpolation, gamma of 2"
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate (CubeHelixLong 2), SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])


gamma5 :: VegaLite
gamma5 =
    chart "cube-helix-long interpolation, gamma of 10"
        (color [ MName "Acceleration", MmType Quantitative, MScale [ SInterpolate (CubeHelixLong 10), SType ScLinear, SRange (RStrings [ "yellow", "red" ]) ] ])

