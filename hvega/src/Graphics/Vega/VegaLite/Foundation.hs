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
       , Measurement(..)
       , Arrangement(..)
       , APosition(..)
       , Orientation(..)
       , Position(..)

       , HAlign(..)
       , VAlign(..)

       , StrokeCap(..)
       , StrokeJoin(..)

       , Scale(..)

       -- not for external export
       , fontWeightSpec
       , measurementLabel
       , arrangementLabel
       , anchorLabel
       , orientationSpec
       , hAlignLabel
       , vAlignLabel
       , strokeCapLabel
       , strokeJoinLabel
       , scaleLabel

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
            . 'Graphics.Vega.VegaLite.position' 'X' (axis "x" 2)
            . 'Graphics.Vega.VegaLite.position' 'Y' (axis "y" 1)
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


{-|

Type of measurement to be associated with some channel.

-}

data Measurement
    = Nominal
      -- ^ Data are categories identified by name alone and which have no intrinsic order.
    | Ordinal
      -- ^ Data are also categories, but ones which have some natural order.
    | Quantitative
      -- ^ Data are numeric measurements typically on a continuous scale.
    | Temporal
      -- ^ Data represents time in some manner.
    | GeoFeature
      -- ^ Geospatial position encoding ('Longitude' and 'Latitude') should specify the 'Graphics.Vega.VegaLite.PmType'
      -- as @Quantitative@. Geographically referenced features encoded as 'Graphics.Vega.VegaLite.shape' marks
      -- should specify 'Graphics.Vega.VegaLite.MmType' as @GeoFeature@ (Vega-Lite currently refers to this type
      -- as @<https://vega.github.io/vega-lite/docs/encoding.html geojson>@.


measurementLabel :: Measurement -> T.Text
measurementLabel Nominal = "nominal"
measurementLabel Ordinal = "ordinal"
measurementLabel Quantitative = "quantitative"
measurementLabel Temporal = "temporal"
measurementLabel GeoFeature = "geojson"


-- | Identifies how repeated or faceted views are arranged.
--
--   This is used with a number of constructors: 'Graphics.Vega.VegaLite.ByRepeatOp',
--   'Graphics.Vega.VegaLite.HRepeat', 'Graphics.Vega.VegaLite.MRepeat', 'Graphics.Vega.VegaLite.ORepeat', 'Graphics.Vega.VegaLite.PRepeat', and 'Graphics.Vega.VegaLite.TRepeat'.

-- based on schema 3.3.0 #/definitions/RepeatRef

data Arrangement
    = Column
      -- ^ Column arrangement.
    | Row
      -- ^ Row arrangement.
    | Flow
      -- ^ Flow arrangement (aka \"repeat\").
      --
      --   @since 0.4.0.0


arrangementLabel :: Arrangement -> T.Text
arrangementLabel Column = "column"
arrangementLabel Row = "row"
arrangementLabel Flow = "repeat"  -- NOTE: not "flow"!


-- | Indicates the anchor position for text.

data APosition
    = AStart
      -- ^ The start of the text.
    | AMiddle
      -- ^ The middle of the text.
    | AEnd
      -- ^ The end of the text.


anchorLabel :: APosition -> T.Text
anchorLabel AStart = "start"
anchorLabel AMiddle = "middle"
anchorLabel AEnd = "end"


{-|

The orientation of an item. This is used with:
'Graphics.Vega.VegaLite.BLeLDirection', 'Graphics.Vega.VegaLite.LDirection',
'Graphics.Vega.VegaLite.LeGradientDirection', 'Graphics.Vega.VegaLite.LeLDirection', 'Graphics.Vega.VegaLite.LeSymbolDirection',
and 'Graphics.Vega.VegaLite.MOrient'.

In @0.4.0.0@ this was renamed from @MarkOrientation@ to 'Orientation'.

-}

-- based on schema 3.3.0 #/definitions/Orientation

data Orientation
    = Horizontal
      -- ^ Display horizontally.
    | Vertical
      -- ^ Display vertically.


orientationSpec :: Orientation -> VLSpec
orientationSpec Horizontal = "horizontal"
orientationSpec Vertical = "vertical"


-- TODO:
--
--  encoding of X2/... shouldn't include the PmType in the output, apparently
--  so we could try and filter that out, or just rely on the user to not
--  add the PmType fields in this case.

{-|

Type of position channel, @X@ and @Y@ represent horizontal and vertical axis
dimensions on a plane and @X2@ and @Y2@ represent secondary axis dimensions where
two scales are overlaid in the same space. Geographic positions represented by
longitude and latiutude values are identified with @Longitude@, @Latitude@ and
their respective secondary equivalents. Such geographic position channels are
subject to a map projection (set using 'Graphics.Vega.VegaLite.projection') before being placed graphically.

-}
data Position
    = X
    | Y
    | X2
    -- ^ The secondary coordinate for ranged 'Graphics.Vega.VegaLite.Area', 'Graphics.Vega.VegaLite.Bar', 'Graphics.Vega.VegaLite.Rect', and 'Graphics.Vega.VegaLite.Rule'
    --    marks.
    | Y2
    -- ^ The secondary coordinate for ranged 'Graphics.Vega.VegaLite.Area', 'Graphics.Vega.VegaLite.Bar', 'Graphics.Vega.VegaLite.Rect', and 'Graphics.Vega.VegaLite.Rule'
    --    marks.
    | XError
      -- ^ Indicates that the 'X' channel represents the mid-point and
      --   the 'XError' channel gives the offset. If 'XError2' is not
      --   defined then this channel value is applied symmetrically.
      --
      --   @since 0.4.0.0
    | XError2
      -- ^ Used to support asymmetric error ranges defined as 'XError'
      --   and 'XError2'. One of 'XError' or 'XError2' channels must
      --   contain positive values and the other negative values.
      --
      --   @since 0.4.0.0
    | YError
      -- ^ Indicates that the 'Y' channel represents the mid-point and
      --   the 'YError' channel gives the offset. If 'YError2' is not
      --   defined then this channel value is applied symmetrically.
      --
      --   @since 0.4.0.0
    | YError2
      -- ^ Used to support asymmetric error ranges defined as 'YError'
      --   and 'YError2'. One of 'YError' or 'YError2' channels must
      --   contain positive values and the other negative values.
      --
      --   @since 0.4.0.0
    | Longitude
      -- ^ The longitude value for projections.
    | Latitude
      -- ^ The latitude value for projections.
    | Longitude2
      -- ^ A second longitude coordinate.
    | Latitude2
      -- ^ A second longitude coordinate.


-- | Indicates the horizontal alignment of text such as on an axis or legend.

data HAlign
    = AlignCenter
    | AlignLeft
    | AlignRight


-- | Indicates the vertical alignment of text that may be attached to a mark.

data VAlign
    = AlignTop
    | AlignMiddle
    | AlignBottom


hAlignLabel :: HAlign -> T.Text
hAlignLabel AlignLeft = "left"
hAlignLabel AlignCenter = "center"
hAlignLabel AlignRight = "right"


vAlignLabel :: VAlign -> T.Text
vAlignLabel AlignTop = "top"
vAlignLabel AlignMiddle = "middle"
vAlignLabel AlignBottom = "bottom"


-- | How are strokes capped? This is used with 'Graphics.Vega.VegaLite.MStrokeCap', 'Graphics.Vega.VegaLite.VBStrokeCap',
--   and 'Graphics.Vega.VegaLite.ViewStrokeCap'.
--
--   @since 0.4.0.0

data StrokeCap
    = CButt
      -- ^ Butt stroke cap.
    | CRound
      -- ^ Rounded stroke cap.
    | CSquare
      -- ^ Square stroke cap.


strokeCapLabel :: StrokeCap -> T.Text
strokeCapLabel CButt = "butt"
strokeCapLabel CRound = "round"
strokeCapLabel CSquare = "square"


-- | How are strokes joined? This is used with 'Graphics.Vega.VegaLite.MStrokeJoin', 'Graphics.Vega.VegaLite.VBStrokeJoin',
--   and 'Graphics.Vega.VegaLite.ViewStrokeJoin'.
--
--
--   @since 0.4.0.0

data StrokeJoin
    = JMiter
      -- ^ Mitred stroke join.
    | JRound
      -- ^ Rounded stroke join.
    | JBevel
      -- ^ Bevelled stroke join.


strokeJoinLabel :: StrokeJoin -> T.Text
strokeJoinLabel JMiter = "miter"
strokeJoinLabel JRound = "round"
strokeJoinLabel JBevel = "bevel"


-- | Used to indicate the type of scale transformation to apply.
--
--   The @0.4.0.0@ release removed the @ScSequential@ constructor, as
--   'ScLinear' should be used instead.

data Scale
    = ScLinear
      -- ^ A linear scale.
    | ScPow
      -- ^ A power scale. The exponent to use for scaling is specified with
      --   'Graphics.Vega.VegaLite.SExponent'.
    | ScSqrt
      -- ^ A square-root scale.
    | ScLog
      -- ^ A log scale. Defaults to log of base 10, but can be customised with
      --   'Graphics.Vega.VegaLite.SBase'.
    | ScSymLog
      -- ^ A [symmetrical log (PDF link)](https://www.researchgate.net/profile/John_Webber4/publication/233967063_A_bi-symmetric_log_transformation_for_wide-range_data/links/0fcfd50d791c85082e000000.pdf)
      --   scale. Similar to a log scale but supports zero and negative values. The slope
      --   of the function at zero can be set with 'Graphics.Vega.VegaLite.SConstant'.
      --
      --   @since 0.4.0.0
    | ScTime
      -- ^ A temporal scale.
    | ScUtc
      -- ^ A temporal scale, in UTC.
    | ScOrdinal
      -- ^ An ordinal scale.
    | ScBand
      -- ^ A band scale.
    | ScPoint
      -- ^ A point scale.
    | ScBinLinear
      -- ^ A linear band scale.
    | ScBinOrdinal
      -- ^ An ordinal band scale.
    | ScQuantile
      -- ^ A quantile scale.
      --
      --   @since 0.4.0.0
    | ScQuantize
      -- ^ A quantizing scale.
      --
      --   @since 0.4.0.0
    | ScThreshold
      -- ^ A threshold scale.
      --
      --   @since 0.4.0.0


scaleLabel :: Scale -> T.Text
scaleLabel ScLinear = "linear"
scaleLabel ScPow = "pow"
scaleLabel ScSqrt = "sqrt"
scaleLabel ScLog = "log"
scaleLabel ScSymLog = "symlog"
scaleLabel ScTime = "time"
scaleLabel ScUtc = "utc"
scaleLabel ScOrdinal = "ordinal"
scaleLabel ScBand = "band"
scaleLabel ScPoint = "point"
scaleLabel ScBinLinear = "bin-linear"
scaleLabel ScBinOrdinal = "bin-ordinal"
scaleLabel ScQuantile = "quantile"
scaleLabel ScQuantize = "quantize"
scaleLabel ScThreshold = "threshold"
