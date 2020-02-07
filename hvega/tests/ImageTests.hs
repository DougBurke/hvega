{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ImageTests.elm as of version 2.0
--
module ImageTests (testSpecs) where

-- import qualified Data.Aeson as A
import qualified Data.Text as T

-- import Data.Aeson ((.=))
-- import Data.Function ((&))

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("image1", image1)
            , ("image2", image2)
            , ("image3", image3)
            , ("image4", image4)
            , ("image5", image5)
            , ("image6", image6)
            , ("image7", image7)
            , ("image8", image8)
            , ("image9", image9)
            , ("image10", image10)
            {-
            , ("image11", image11)
            , ("image12", image12)
            -}
            ]


base :: T.Text
base = "https://vega.github.io/vega-lite/data/"


imgUrls :: DataValues
imgUrls = Strings (map (base <>) [ "ffox.png", "gimp.png", "7zip.png" ])

                      
imageEnc :: ([DataColumn] -> [DataColumn]) -> (BuildEncodingSpecs) -> VegaLite
imageEnc dataSupp encSupp =
  let axVals = Numbers [ 0.5, 1.5, 2.5 ]
      dvals = dataFromColumns []
              . dataColumn "x" axVals
              . dataColumn "y" axVals
              . dataSupp

      enc = encoding
            . position X [ PName "x", PmType Quantitative ]
            . position Y [ PName "y", PmType Quantitative ]
            . encSupp

  in toVegaLite [ dvals [], enc [], mark Image [ MWidth 25, MHeight 25 ] ]


image1 :: VegaLite
image1 = imageEnc (const []) (url [ HString (base <> "ffox.png") ])


image2 :: VegaLite
image2 =
  imageEnc
    (dataColumn "img" imgUrls)
    (url [ HName "img", HmType Nominal ])


lakeImage :: [MarkProperty] -> VegaLite
lakeImage mProps =
  let col lbl = dataColumn lbl (Numbers [ 5 ])
      dvals = dataFromColumns [] . col "x" . col "y"

      pos t l = position t [ PName l
                           , PmType Quantitative
                           , PScale [ SDomain (DNumbers [0, 10]) ]
                           ]
      enc = encoding . pos X "x" . pos Y "y"
      encImage = enc . url [ HString "https://gicentre.github.io/data/images/LillyTarn.jpg" ]
      
  in toVegaLite
        [ dvals []
        , layer
            [ asSpec [ encImage [], mark Image mProps ]
            , asSpec [ enc [], mark Circle [ MColor "red", MSize 100, MOpacity 0.5 ] ]
            ]
        ]


image3 :: VegaLite
image3 = lakeImage [ MWidth 100, MAspect True ]


image4 :: VegaLite
image4 =
    lakeImage [ MWidth 100, MHeight 100, MAspect False ]


image5 :: VegaLite
image5 =
    lakeImage [ MWidth 100, MHeight 61, MAlign AlignLeft ]


image6 :: VegaLite
image6 =
    lakeImage [ MWidth 100, MHeight 61, MAlign AlignCenter ]


image7 :: VegaLite
image7 =
    lakeImage [ MWidth 100, MHeight 61, MAlign AlignRight ]


image8 :: VegaLite
image8 =
    lakeImage [ MWidth 100, MHeight 61, MBaseline AlignTop ]


image9 :: VegaLite
image9 =
    lakeImage [ MWidth 100, MHeight 61, MBaseline AlignMiddle ]


image10 :: VegaLite
image10 =
    lakeImage [ MWidth 100, MHeight 61, MBaseline AlignBottom ]


{-

These two are valid according to the 4.0.2 schema, but when viewed in
the Vega Editor a warning message is displayed about how the size
channel is incompatible with the image mark and so is dropped.

image11 :: VegaLite
image11 =
  imageEnc
    (dataColumn "img" imgUrls . dataColumn "mag" (Numbers [ 1, 1, 1 ]))
    (url [ HName "img", HmType Nominal ] . size [ MName "mag", MmType Quantitative ])


image12 :: VegaLite
image12 =
  imageEnc
    (dataColumn "img" imgUrls . dataColumn "mag" (Numbers [ 1, 2, 1 ]))
    (url [ HName "img", HmType Nominal ] . size [ MName "mag", MmType Quantitative ])

-}
