{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Foundation
Copyright   : (c) Douglas Burke, 2018-2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings

Basic types that are used throughout VegaLite.

-}

module Graphics.Vega.VegaLite.Foundation
       ( Angle
       , Color
       , Opacity
       , ZIndex

       , FontWeight(..)

       -- not for external export
       , fontWeightSpec

       )
    where

import qualified Data.Text as T

import Data.Aeson (toJSON)


-- added in base 4.8.0.0 / ghc 7.10.1
import Numeric.Natural (Natural)


import Graphics.Vega.VegaLite.Specification (VLSpec)


{-|

Convenience type-annotation label to indicate a color value.
There is __no attempt__ to validate that the user-supplied input
is a valid color.

Any supported HTML color specification can be used, such as:

@
\"#eee\"
\"#734FD8\"
\"crimson\"
\"rgb(255,204,210)\"
\"hsl(180, 50%, 50%)\"
@

@since 0.4.0.0
-}

type Color = T.Text


{-|

Convenience type-annotation label to indicate an opacity value, which
lies in the range 0 to 1 inclusive. There is __no attempt__ to validate
that the user-supplied value falls in this range.

A value of 0 indicates fully transparent (see through), and 1 is
fully opaque (does not show anything it is on top of).

@since 0.4.0.0
-}

type Opacity = Double


{-|

Convenience type-annotation label to indicate an angle, which is measured
in degrees from the horizontal (so anti-clockwise).

The value should be in the range 0 to 360, inclusive, but __no attempt__ is
made to enforce this.

@since 0.4.0.0
-}

type Angle = Double


{-|

At what "depth" (z index) is the item to be drawn (a relative depth
for items in the visualization). The standard values are @0@ for
back and @1@ for front, but other values can be used if you want
to ensure a certain layering of items.

The following example is taken from a discussion with
<https://github.com/gicentre/elm-vegalite/issues/15#issuecomment-524527125 Jo Wood>:

@
let dcols = 'Graphics.Vega.VegaLite.dataFromColumns' []
              . 'Graphics.Vega.VegaLite.dataColumn' "x" ('Graphics.Vega.VegaLite.Numbers' [ 20, 10 ])
              . 'Graphics.Vega.VegaLite.dataColumn' "y" ('Graphics.Vega.VegaLite.Numbers' [ 10, 20 ])
              . 'Graphics.Vega.VegaLite.dataColumn' "cat" ('Graphics.Vega.VegaLite.Strings' [ "a", "b" ])

    axis lbl z = [ 'Graphics.Vega.VegaLite.PName' lbl, 'Graphics.Vega.VegaLite.PmType' 'Graphics.Vega.VegaLite.Quantitative', 'Graphics.Vega.VegaLite.PAxis' [ 'Graphics.Vega.VegaLite.AxZIndex' z ] ]
    enc = 'Graphics.Vega.VegaLite.encoding'
            . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.X' (axis "x" 2)
            . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.Y' (axis "y" 1)
            . 'Graphics.Vega.VegaLite.color' [ 'Graphics.Vega.VegaLite.MName' "cat", 'Graphics.Vega.VegaLite.MmType' 'Graphics.Vega.VegaLite.Nominal', 'Graphics.Vega.VegaLite.MLegend' [] ]

    cfg = 'Graphics.Vega.VegaLite.configure'
            . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.Axis' [ 'Graphics.Vega.VegaLite.GridWidth' 8 ])
            . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.AxisX' [ 'Graphics.Vega.VegaLite.GridColor' "red" ])
            . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.AxisY' [ 'Graphics.Vega.VegaLite.GridColor' "blue" ])

in 'Graphics.Vega.VegaLite.toVegaLite' [ cfg []
              , dcols []
              , enc []
              , 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Circle' [ 'Graphics.Vega.VegaLite.MSize' 5000, 'Graphics.Vega.VegaLite.MOpacity' 1 ]
              ]
@

<<images/zindex.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABBYEQMYPYDsBmBLA5lAXGUk-QEMAPFAZwE0sdx9ao0AnFAEwGE4AbOBqqAIw4BXAKZQatAL4AacfijEyADSq5acxi3Zce2KA2HMxasNNl55JUirNr6TZgHUWAFwAWVABw2IE2afMAtgQMANbWxlCkKABeotgArAAMyTIRcAAOBDAozgCeVACMqbZ56XHQ2QwwHKJ+xRBQzATOBOG2AG4EQsJW2ADa3viqxnQwzbyt9SPmRFQATIlT0w352AWJg3j+y8PLDWPOvHxQS8tQs2uLm7arYAvXPoMAunWyUAAkpDCuwkG8rs5nOlSJgAPSg9rCNAEAB0aByrkEfBhKDgoK+PyCEKhBAAtBwcsIIQBmGEAK1IiBOb2ECHgzBQCAw2F25ng2ja0ygqGEHEMugO1L2UFK5SgCDgAUZXSFZxqaFp-LACEEHA4g22dAu1GFPL5vFmpzoot4AEdBAQEM4cs0UJDZVyFL0dXtIFBoozmMJtXMHiYNUboLdWbY9UqoPlA+YTbpzZbrS1rfao26nZzXe7Pd7Cn7fMY85BfL4gA View the visualization in the Vega Editor>

@since 0.4.0.0
-}

type ZIndex = Natural


-- | Indicates the weight options for a font.

data FontWeight
    = Bold
    | Bolder
    | Lighter
    | Normal
    | W100
    | W200
    | W300
    | W400
    | W500
    | W600
    | W700
    | W800
    | W900


fromF :: Double -> VLSpec
fromF = toJSON

fromT :: T.Text -> VLSpec
fromT = toJSON


fontWeightSpec :: FontWeight -> VLSpec
fontWeightSpec Bold = fromT "bold"
fontWeightSpec Bolder = fromT "bolder"
fontWeightSpec Lighter = fromT "lighter"
fontWeightSpec Normal = fromT "normal"
fontWeightSpec W100 = fromF 100
fontWeightSpec W200 = fromF 200
fontWeightSpec W300 = fromF 300
fontWeightSpec W400 = fromF 400
fontWeightSpec W500 = fromF 500
fontWeightSpec W600 = fromF 600
fontWeightSpec W700 = fromF 700
fontWeightSpec W800 = fromF 800
fontWeightSpec W900 = fromF 900
