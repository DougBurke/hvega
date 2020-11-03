{-# LANGUAGE OverloadedStrings #-}

{-

Limited set of encoding-related tests, as it is used in every test.

-}

module EncodingTests (testSpecs) where

import Graphics.Vega.VegaLite

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("strokedashgroup", strokeDashGroup)
            , ("strokedashline", strokeDashLine)
            , ("nullencoding", nullEncoding)
            ]
            

strokeDashGroup :: VegaLite
strokeDashGroup =
  toVegaLite [ dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv" []
             , mark Line []
             , encoding
               . position X [ PName "date", PmType Temporal ]
               . position Y [ PName "price", PmType Quantitative ]
               . strokeDash [ MName "symbol", MmType Nominal ]
               $ []
             ]

strokeDashLine :: VegaLite
strokeDashLine =
  toVegaLite [ dataFromColumns []
               . dataColumn "a" (Strings [ "A", "B", "D", "E", "E", "G", "H"])
               . dataColumn "b" (Numbers [ 28, 55, 91, 81, 81, 19, 87 ])
               . dataColumn "predicted" (Booleans [False, False, False, False, True, True, True])
               $ []
             , mark Line []
             , encoding
               . position X [ PName "a", PmType Ordinal ]
               . position Y [ PName "b", PmType Quantitative ]
               . strokeDash [ MName "predicted", MmType Nominal ]
               $ []
             ]

-- See https://github.com/vega/vega-lite/issues/6762
--
-- Note: a "null" encoding here is with no arguments, which creates {}.
-- We don't support creating null instead of {}.
--
nullEncoding :: VegaLite
nullEncoding =
  let dvals = dataFromColumns []
              . dataColumn "a" (Strings ["A", "B", "D", "E", "E", "G", "H", "I"])
              . dataColumn "b" (Numbers [28, 55, 91, 81, 81, 19, 87, 52])

      lyr1 = [mark Bar []]
      lyr2 = [mark Rule [], encoding (position X [] [])]
      
  in toVegaLite [ description "Can we null the encoding"
                , dvals []
                , encoding
                  . position X [PName "a", PmType Nominal, PAxis [AxLabelAngle 0]]
                  . position Y [PName "b", PmType Quantitative]
                  $ []
                , layer (map asSpec [lyr1, lyr2])
                ]
