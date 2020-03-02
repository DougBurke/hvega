{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Foundation
Copyright   : (c) Douglas Burke, 2018-2020
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

Basic types that are used throughout VegaLite.
Would it make sense to break this up into
smaller modules?

-}

module Graphics.Vega.VegaLite.Foundation
       ( Angle
       , Color
       , DashStyle
       , DashOffset
       , FieldName
       , Opacity
       , StyleLabel
       , VegaExpr
       , ZIndex

       , FontWeight(..)
       , Measurement(..)
       , Arrangement(..)
       , APosition(..)
       , Orientation(..)
       , Position(..)

       , HAlign(..)
       , VAlign(..)
       , BandAlign(..)

       , StrokeCap(..)
       , StrokeJoin(..)

       , Scale(..)

       , SortField(..)

       , Cursor(..)

       , OverlapStrategy(..)
       , Side(..)

       , Symbol(..)

       , StackProperty(..)
       , StackOffset(..)

       , TooltipContent(..)

       , Channel(..)
       , Resolve(..)
       , Resolution(..)

       , Bounds(..)
       , CompositionAlignment(..)
       , Padding(..)
       , Autosize(..)
       , RepeatFields(..)
       , CInterpolate(..)

       , ViewBackground(..)

       , HeaderProperty(..)

       -- not for external export
       , fontWeightSpec
       , measurementLabel
       , arrangementLabel
       , anchorLabel
       , orientationSpec
       , hAlignLabel
       , vAlignLabel
       , bandAlignLabel
       , strokeCapLabel
       , strokeJoinLabel
       , scaleLabel
       , positionLabel
       , sortFieldSpec
       , cursorLabel
       , overlapStrategyLabel
       , sideLabel
       , symbolLabel
       , stackPropertySpecSort
       , stackPropertySpecOffset
       , stackOffset
       , ttContentLabel
       , channelLabel
       , resolveProperty
       , boundsSpec
       , compositionAlignmentSpec
       , paddingSpec
       , autosizeProperty
       , repeatFieldsProperty
       , cInterpolateSpec
       , viewBackgroundSpec

       , fromT
       , fromColor
       , fromDS
       , splitOnNewline
       , field_
       , header_
       , order_
       , allowNull
       )
    where

import qualified Data.Aeson as A
import qualified Data.Text as T

import Data.Aeson ((.=), object, toJSON)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

-- added in base 4.8.0.0 / ghc 7.10.1
import Numeric.Natural (Natural)


import Graphics.Vega.VegaLite.Specification
  ( VLSpec
  , LabelledSpec
  , ResolveSpec(..)
  )


field_ :: FieldName -> LabelledSpec
field_ f = "field" .= f

header_ :: T.Text -> [HeaderProperty] -> LabelledSpec
header_ extra hps = ("header" <> extra) .= object (map headerProperty hps)

-- could restrict to ascending/descending
order_ :: T.Text -> LabelledSpec
order_ o = "order" .= o


-- allowNull :: A.ToJSON a => Maybe a -> VLSpec
allowNull :: Maybe Int -> VLSpec
allowNull (Just a) = toJSON a
allowNull Nothing = A.Null


{-|

The field name. This can include \"dotted\" notation, such as
@\"o.latitude\"@.

There is __no attempt__ to validate this value (e.g. check it
is not empty, contains only valid characters, or
remove excess whitespace).

@since 0.5.0.0
-}
type FieldName = T.Text


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

A blank string is converted to the JSON null value (new in @0.5.0.0@).

@since 0.4.0.0
-}

type Color = T.Text


-- strip out trailing white space just to be sure
fromColor :: Color -> VLSpec
fromColor = cleanT


-- strips leading and trailing white space and, if the result
-- is empty, returns Null, otherwise the trimmed text.
--
cleanT :: T.Text -> VLSpec
cleanT t =
  let tout = T.strip t
  in if T.null tout
     then A.Null
     else toJSON tout



{-|
The dash style for a line. This is defined as a series of on and then
off lengths, in pixels. So @[10, 4, 5, 2]@ means a long line, followed
by a space, then a line half as long as the first segment, and then
a short space. This pattern is then repeated.

This is a convenience type annotation and there is __no validation__
of the input.

@since 0.5.0.0
-}
type DashStyle = [Double]


fromDS :: DashStyle -> VLSpec
-- fromDS [] = A.Null  -- what is the correct handling of this?
fromDS = toJSON


{-|
The offset at which to start drawing the line dash (given by a
'DashStyle' argument), in pixels.

This is a convenience type annotation and there is __no validation__
of the input.

@since 0.5.0.0
-}
type DashOffset = Double


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

Convenience type-annotation to indicate a name, or label, that represents
a set of mark or axis styles. The styles are generated with
'Graphics.Vega.VegaLite.AxisNamedStyles' and
'Graphics.Vega.VegaLite.MarkNamedStyles',
and used with constructs such as
'Graphics.Vega.VegaLite.AStyle',
'Graphics.Vega.VegaLite.AxStyle',
'Graphics.Vega.VegaLite.MStyle', and
'Graphics.Vega.VegaLite.TStyle'.

@since 0.6.0.0
-}

type StyleLabel = T.Text

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


{-|

Convenience type-annotation label to indicate a
<https://vega.github.io/vega/docs/expressions/ Vega Expression>.
There is __no attempt__ to validate the expression.

Examples include:

@
"datum.IMDB_Rating != null"
"datum.height / 1000"
"if(datum.index % 2 == 1, datum.label, '')"
"sampleLogNormal(2.3, 0.3)"
@

@since 0.5.0.0
-}
type VegaExpr = T.Text


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

-- If there is a new-line in the text then convert to a list.
splitOnNewline :: T.Text -> VLSpec
splitOnNewline ts =
  case T.split (== '\n') ts of
    [] -> fromT ""
    [s] -> toJSON s
    s -> toJSON s


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


positionLabel :: Position -> T.Text
positionLabel X = "x"
positionLabel Y = "y"
positionLabel X2 = "x2"
positionLabel Y2 = "y2"
positionLabel XError     = "xError"
positionLabel YError     = "yError"
positionLabel XError2    = "xError2"
positionLabel YError2    = "yError2"
positionLabel Longitude = "longitude"
positionLabel Latitude = "latitude"
positionLabel Longitude2 = "longitude2"
positionLabel Latitude2 = "latitude2"



-- | Indicates the horizontal alignment of text such as on an axis or legend.

data HAlign
    = AlignCenter
    | AlignLeft
    | AlignRight


-- | Indicates the vertical alignment of text that may be attached to a mark.

data VAlign
    = AlignTop
      -- ^ The position refers to the top of the text.
    | AlignMiddle
      -- ^ The middle of the text.
    | AlignBottom
      -- ^ The position refers to the bottom of the text, including
      --   descenders, like g.
    | AlignBaseline
      -- ^ The position refers to the baseline of the text (so it does
      --   not include descenders). This maps to the Vega-Lite
      --   @\"alphabetic\"@ value.
      --
      --   @since 0.6.0.0

hAlignLabel :: HAlign -> T.Text
hAlignLabel AlignLeft = "left"
hAlignLabel AlignCenter = "center"
hAlignLabel AlignRight = "right"

vAlignLabel :: VAlign -> T.Text
vAlignLabel AlignTop = "top"
vAlignLabel AlignMiddle = "middle"
vAlignLabel AlignBottom = "bottom"
vAlignLabel AlignBaseline = "alphabetic"

{-|

Where should tick marks and grid lines be placed. This is used with
'Graphics.Vega.VegaLite.AxTickBand' and 'Graphics.Vega.VegaLite.TickBand'.

@since 0.5.0.0
-}

data BandAlign
  = BCenter
    -- ^ Use the center of the band.
  | BExtent
    -- ^ Use the band extents.


bandAlignLabel :: BandAlign -> T.Text
bandAlignLabel BCenter = "center"
bandAlignLabel BExtent = "extent"


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


-- | How should the field be sorted when performing a window transform.
--
--   @since 0.4.00

data SortField
    = WAscending FieldName
    -- ^ Sort the field into ascending order.
    | WDescending FieldName
    -- ^ Sort the field into descending order.


sortFieldSpec :: SortField -> VLSpec
sortFieldSpec (WAscending f) = object [field_ f, order_ "ascending"]
sortFieldSpec (WDescending f) = object [field_ f, order_ "descending"]


{-|

Represents the type of cursor to display. For an explanation of each type,
see the
<https://developer.mozilla.org/en-US/docs/Web/CSS/cursor#Keyword%20values CSS documentation>.

-}
data Cursor
    = CAuto
    | CDefault
    | CNone
    | CContextMenu
    | CHelp
    | CPointer
    | CProgress
    | CWait
    | CCell
    | CCrosshair
    | CText
    | CVerticalText
    | CAlias
    | CCopy
    | CMove
    | CNoDrop
    | CNotAllowed
    | CAllScroll
    | CColResize
    | CRowResize
    | CNResize
    | CEResize
    | CSResize
    | CWResize
    | CNEResize
    | CNWResize
    | CSEResize
    | CSWResize
    | CEWResize
    | CNSResize
    | CNESWResize
    | CNWSEResize
    | CZoomIn
    | CZoomOut
    | CGrab
    | CGrabbing


cursorLabel :: Cursor -> T.Text
cursorLabel CAuto = "auto"
cursorLabel CDefault = "default"
cursorLabel CNone = "none"
cursorLabel CContextMenu = "context-menu"
cursorLabel CHelp = "help"
cursorLabel CPointer = "pointer"
cursorLabel CProgress = "progress"
cursorLabel CWait = "wait"
cursorLabel CCell = "cell"
cursorLabel CCrosshair = "crosshair"
cursorLabel CText = "text"
cursorLabel CVerticalText = "vertical-text"
cursorLabel CAlias = "alias"
cursorLabel CCopy = "copy"
cursorLabel CMove = "move"
cursorLabel CNoDrop = "no-drop"
cursorLabel CNotAllowed = "not-allowed"
cursorLabel CAllScroll = "all-scroll"
cursorLabel CColResize = "col-resize"
cursorLabel CRowResize = "row-resize"
cursorLabel CNResize = "n-resize"
cursorLabel CEResize = "e-resize"
cursorLabel CSResize = "s-resize"
cursorLabel CWResize = "w-resize"
cursorLabel CNEResize = "ne-resize"
cursorLabel CNWResize = "nw-resize"
cursorLabel CSEResize = "se-resize"
cursorLabel CSWResize = "sw-resize"
cursorLabel CEWResize = "ew-resize"
cursorLabel CNSResize = "ns-resize"
cursorLabel CNESWResize = "nesw-resize"
cursorLabel CNWSEResize = "nwse-resize"
cursorLabel CZoomIn = "zoom-in"
cursorLabel CZoomOut = "zoom-out"
cursorLabel CGrab = "grab"
cursorLabel CGrabbing = "grabbing"


{-|

Type of overlap strategy to be applied when there is not space to show all items
on an axis, and is used by
'Graphics.Vega.VegaLite.AxLabelOverlap',
'Graphics.Vega.VegaLite.LabelOverlap',
'Graphics.Vega.VegaLite.LLabelOverlap',
and 'Graphics.Vega.VegaLite.LeLabelOverlap'.
See the
<https://vega.github.io/vega-lite/docs/axis.html#labels Vega-Lite documentation>
for more details.
-}

data OverlapStrategy
    = ONone
      -- ^ No overlap strategy to be applied when there is not space to show all items
      --   on an axis.
    | OParity
      -- ^ Give all items equal weight in overlap strategy to be applied when there is
      --   not space to show them all on an axis.
    | OGreedy
      -- ^ Greedy overlap strategy to be applied when there is not space to show all
      --   items on an axis.

overlapStrategyLabel :: OverlapStrategy -> VLSpec
overlapStrategyLabel ONone = toJSON False
overlapStrategyLabel OParity = toJSON True  -- fromT "parity"
overlapStrategyLabel OGreedy = fromT "greedy"


{-|

Represents one side of a rectangular space.

Used by
'Graphics.Vega.VegaLite.AxOrient',
'Graphics.Vega.VegaLite.HLabelOrient',
'Graphics.Vega.VegaLite.HTitleOrient',
'Graphics.Vega.VegaLite.LTitleOrient',
'Graphics.Vega.VegaLite.LeTitleOrient',
'Graphics.Vega.VegaLite.Orient',
and
'Graphics.Vega.VegaLite.TOrient'.

-}

data Side
    = STop
    | SBottom
    | SLeft
    | SRight


sideLabel :: Side -> T.Text
sideLabel STop = "top"
sideLabel SBottom = "bottom"
sideLabel SLeft = "left"
sideLabel SRight = "right"


-- | Identifies the type of symbol used with the 'Graphics.Vega.VegaLite.Point' mark type.
--   It is used with 'Graphics.Vega.VegaLite.MShape', 'Graphics.Vega.VegaLite.LeSymbolType', and 'Graphics.Vega.VegaLite.LSymbolType'.
--
--   In version @0.4.0.0@ all constructors were changed to start
--   with @Sym@.
--
data Symbol
    = SymCircle
      -- ^ Specify a circular symbol for a shape mark.
    | SymSquare
      -- ^ Specify a square symbol for a shape mark.
    | SymCross
      -- ^ Specify a cross symbol for a shape mark.
    | SymDiamond
      -- ^ Specify a diamond symbol for a shape mark.
    | SymTriangleUp
      -- ^ Specify an upward-triangular symbol for a shape mark.
    | SymTriangleDown
      -- ^ Specify a downward-triangular symbol for a shape mark.
    | SymTriangleRight
      -- ^ Specify an right-facing triangular symbol for a shape mark.
      --
      --   @since 0.4.0.0
    | SymTriangleLeft
      -- ^ Specify an left-facing triangular symbol for a shape mark.
      --
      --   @since 0.4.0.0
    | SymStroke
      -- ^ The line symbol.
      --
      --  @since 0.4.0.0
    | SymArrow
      -- ^ Centered directional shape.
      --
      --  @since 0.4.0.0
    | SymTriangle
      -- ^ Centered directional shape. It is not clear what difference
      --   this is to 'SymTriangleUp'.
      --
      --  @since 0.4.0.0
    | SymWedge
      -- ^ Centered directional shape.
      --
      --  @since 0.4.0.0
    | SymPath T.Text
      -- ^ A custom symbol shape as an
      --   [SVG path description](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths).
      --
      --   For correct sizing, the path should be defined within a square
      --   bounding box, defined on an axis of -1 to 1 for both dimensions.


symbolLabel :: Symbol -> T.Text
symbolLabel SymCircle = "circle"
symbolLabel SymSquare = "square"
symbolLabel SymCross = "cross"
symbolLabel SymDiamond = "diamond"
symbolLabel SymTriangleUp = "triangle-up"
symbolLabel SymTriangleDown = "triangle-down"
symbolLabel SymTriangleRight = "triangle-right"
symbolLabel SymTriangleLeft = "triangle-left"
symbolLabel SymStroke = "stroke"
symbolLabel SymArrow = "arrow"
symbolLabel SymTriangle = "triangle"
symbolLabel SymWedge = "wedge"
symbolLabel (SymPath svgPath) = svgPath


-- | How are stacks applied within a transform?
--
--   Prior to version @0.4.0.0@ the @StackProperty@ type was
--   what is now @StackOffset@.

data StackProperty
    = StOffset StackOffset
      -- ^ Stack offset.
      --
      --   @since 0.4.0.0
    | StSort [SortField]
      -- ^ Ordering within a stack.
      --
      --   @since 0.4.0.0


-- | Describes the type of stacking to apply to a bar chart.
--
--   In @0.4.0.0@ this was renamed from @StackProperty@ to @StackOffset@,
--   but the constructor names have not changed.
--
data StackOffset
    = StZero
      -- ^ Offset a stacked layout using a baseline at the foot of
      --   the stack.
    | StNormalize
      -- ^ Rescale a stacked layout to use a common height while
      --   preserving the relative size of stacked quantities.
    | StCenter
      -- ^ Offset a stacked layout using a central stack baseline.
    | NoStack
      -- ^ Do not stack marks, but create a layered plot.

stackOffsetSpec :: StackOffset -> VLSpec
stackOffsetSpec StZero = "zero"
stackOffsetSpec StNormalize = "normalize"
stackOffsetSpec StCenter = "center"
stackOffsetSpec NoStack = A.Null

stackOffset :: StackOffset -> LabelledSpec
stackOffset so = "stack" .= stackOffsetSpec so


stackPropertySpecOffset , stackPropertySpecSort:: StackProperty -> Maybe VLSpec
stackPropertySpecOffset (StOffset op) = Just (stackOffsetSpec op)
stackPropertySpecOffset _ = Nothing

stackPropertySpecSort (StSort sfs) = Just (toJSON (map sortFieldSpec sfs))
stackPropertySpecSort _ = Nothing


-- | This is used with 'Graphics.Vega.VegaLite.MTooltip' and
--   can be used with 'Graphics.Vega.VegaLite.mark' or
--   'Graphics.Vega.VegaLite.MarkStyle'.
--
--   @since 0.4.0.0

data TooltipContent
  = TTEncoding
    -- ^ When enabled, tooltips are generated by the encoding
    --   (this is the default).
    --
    --   For example:
    --
    --   @'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Circle' ['Graphics.Vega.VegaLite.MTooltip' 'TTEncoding']@
  | TTData
    -- ^ Tooltips are generated by all fields in the underlying data.
    --
    --   For example:
    --
    --   @'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Circle' ['Graphics.Vega.VegaLite.MTooltip' 'TTData']@
  | TTNone
    -- ^ Disable tooltips. This is the default behavior in Vega-Lite 4,
    --   and can also be achieved by adding an encoding of
    --   @'Graphics.Vega.VegaLite.tooltip' []@.
    --
    --   For example:
    --
    --   @'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Circle' ['Graphics.Vega.VegaLite.MTooltip' 'TTNone']@


-- Note that TTNone is special cased by markProperty
ttContentLabel :: TooltipContent -> T.Text
ttContentLabel TTEncoding = "encoding"
ttContentLabel TTData = "data"
ttContentLabel TTNone = "null"


-- | Indicates a channel type to be used in a resolution specification.
--
--   Used with the 'Resolve' type and the
--   'Graphics.Vega.VegaLite.BLChannel', 'Graphics.Vega.VegaLite.BLChannelEvent',
--   'Graphics.Vega.VegaLite.ByChannel', and 'Graphics.Vega.VegaLite.Encodings'
--   constructors.

-- assuming this is based on schema 3.3.0 #/definitions/SingleDefUnitChannel

data Channel
    = ChX
    | ChY
    | ChX2
    | ChY2
    | ChLongitude
      -- ^ @since 0.4.0.0
    | ChLongitude2
      -- ^ @since 0.4.0.0
    | ChLatitude
      -- ^ @since 0.4.0.0
    | ChLatitude2
      -- ^ @since 0.4.0.0
    | ChColor
    | ChFill
      -- ^ @since 0.3.0.0
    | ChFillOpacity
      -- ^ @since 0.4.0.0
    | ChHref
      -- ^ @since 0.4.0.0
    | ChKey
      -- ^ @since 0.4.0.0
    | ChOpacity
    | ChShape
    | ChSize
    | ChStroke
      -- ^ @since 0.3.0.0
    | ChStrokeDash
      -- ^ @since 0.6.0.0
    | ChStrokeOpacity
      -- ^ @since 0.4.0.0
    | ChStrokeWidth
      -- ^ @since 0.4.0.0
    | ChText
      -- ^ @since 0.4.0.0
    | ChTooltip
      -- ^ @since 0.4.0.0


channelLabel :: Channel -> T.Text
channelLabel ChX = "x"
channelLabel ChY = "y"
channelLabel ChX2 = "x2"
channelLabel ChY2 = "y2"
channelLabel ChLongitude = "longitude"
channelLabel ChLatitude = "latitude"
channelLabel ChLongitude2 = "longitude2"
channelLabel ChLatitude2 = "latitude2"
channelLabel ChColor = "color"
channelLabel ChFill = "fill"
channelLabel ChFillOpacity = "fillOpacity"
channelLabel ChHref = "href"
channelLabel ChKey = "key"
channelLabel ChOpacity = "opacity"
channelLabel ChShape = "shape"
channelLabel ChSize = "size"
channelLabel ChStroke = "stroke"
channelLabel ChStrokeDash = "strokeDash"
channelLabel ChStrokeWidth = "strokeWidth"
channelLabel ChStrokeOpacity = "strokeOpacity"
channelLabel ChText = "text"
channelLabel ChTooltip = "tooltip"


{-|

Indicates whether or not a scale domain should be independent of others in a
composite visualization. See the
<https://vega.github.io/vega-lite/docs/resolve.html Vega-Lite documentation> for
details.

For use with 'Resolve'.

-}
data Resolution
    = Shared
    | Independent


resolutionLabel :: Resolution -> T.Text
resolutionLabel Shared = "shared"
resolutionLabel Independent = "independent"


{-|

Used to determine how a channel's axis, scale or legend domains should be resolved
if defined in more than one view in a composite visualization. See the
<https://vega.github.io/vega-lite/docs/resolve.html Vega-Lite documentation>
for details.
-}
data Resolve
    = RAxis [(Channel, Resolution)]
    | RLegend [(Channel, Resolution)]
    | RScale [(Channel, Resolution)]


resolveProperty :: Resolve -> ResolveSpec
resolveProperty res =
  let (nme, rls) = case res of
        RAxis chRules -> ("axis", chRules)
        RLegend chRules -> ("legend", chRules)
        RScale chRules -> ("scale", chRules)

      ans = map (\(ch, rule) -> channelLabel ch .= resolutionLabel rule) rls
  in RS (nme, object ans)


-- | This is used with 'Graphics.Vega.VegaLite.bounds' to define the extent of a sub plot.
--
--   @since 0.4.0.0

data Bounds
  = Full
    -- ^ Bounds calculation should use the entire plot area (including axes, title,
    --   and legend).
  | Flush
    -- ^ Bounds calculation should take only the specified width and height values for
    --   a sub-view. Useful when attempting to place sub-plots without axes or legends into
    --   a uniform grid structure.


boundsSpec :: Bounds -> VLSpec
boundsSpec Full = "full"
boundsSpec Flush = "flush"


-- | Specifies the alignment of compositions. It is used with:
--   'Graphics.Vega.VegaLite.align', 'Graphics.Vega.VegaLite.alignRC',
--   'Graphics.Vega.VegaLite.LeGridAlign', 'Graphics.Vega.VegaLite.LGridAlign',
--   and 'Graphics.Vega.VegaLite.FAlign'.
--
--   @since 0.4.0.0

data CompositionAlignment
    = CANone
    -- ^ Flow layout is used, where adjacent subviews are placed one after
    --   another.
    | CAEach
    -- ^ Each row and column may be of a variable size.
    | CAAll
    -- ^ All the rows and columns are of the same size (this is based on the
    --   maximum subview size).


compositionAlignmentSpec :: CompositionAlignment -> VLSpec
compositionAlignmentSpec CANone = "none"
compositionAlignmentSpec CAEach = "each"
compositionAlignmentSpec CAAll = "all"


-- | Specify the padding dimensions in pixel units.

data Padding
    = PSize Double
      -- ^ Use the same padding on all four edges of the container.
    | PEdges Double Double Double Double
      -- ^ Specify the padding for the left, top, right, and bottom edges.


paddingSpec :: Padding -> VLSpec
paddingSpec (PSize p) = toJSON p
paddingSpec (PEdges l t r b) =
  object [ "left" .= l
         , "top" .= t
         , "right" .= r
         , "bottom" .= b
         ]


{-|

Indicates the auto-sizing characteristics of the visualization such as amount
of padding, whether it should fill the parent container etc. For more details see the
<https://vega.github.io/vega-lite/docs/size.html#autosize Vega-Lite documentation>.

-}

data Autosize
    = AContent
      -- ^ Interpret visualization dimensions to be for the data rectangle (external
      --   padding added to this size).
    | AFit
      -- ^ Interpret visualization dimensions to be for the entire visualization (data
      --   rectangle is shrunk to accommodate external decorations padding).
    | AFitX
      -- ^ Interpret visualization width to be for the entire visualization width (data
      --   rectangle width is shrunk to accommodate external decorations padding).
      --
      --   @since 0.5.0.0
    | AFitY
      -- ^ Interpret visualization height to be for the entire visualization height (data
      --   rectangle height is shrunk to accommodate external decorations padding).
      --
      --   @since 0.5.0.0
    | ANone
      -- ^ No autosizing is applied.
    | APad
      -- ^ Automatically expand size of visualization from the given dimensions in order
      --   to fit in all supplementary decorations (legends etc.).
    | APadding
      -- ^ Interpret visualization width to be for the entire visualization (data
      -- rectangle is shrunk to accommodate external padding).
    | AResize
      -- ^ Recalculate autosizing on every view update.


autosizeProperty :: Autosize -> LabelledSpec
autosizeProperty APad = "type" .= fromT "pad"
autosizeProperty AFit = "type" .= fromT "fit"
autosizeProperty AFitX = "type" .= fromT "fit-x"
autosizeProperty AFitY = "type" .= fromT "fit-y"
autosizeProperty ANone = "type" .= fromT "none"
autosizeProperty AResize = "resize" .= True
autosizeProperty AContent = "contains" .= fromT "content"
autosizeProperty APadding = "contains" .= fromT "padding"


{-|

Create a list of fields to use in set of repeated small multiples. The list of
fields named here can be referenced in an encoding with @'Graphics.Vega.VegaLite.PRepeat' 'Graphics.Vega.VegaLite.Column'@
or @'Graphics.Vega.VegaLite.PRepeat' 'Graphics.Vega.VegaLite.Row'@.

-}
data RepeatFields
    = RowFields [FieldName]
    | ColumnFields [FieldName]


repeatFieldsProperty :: RepeatFields -> LabelledSpec
repeatFieldsProperty (RowFields fs) = "row" .= fs
repeatFieldsProperty (ColumnFields fs) = "column" .= fs


{-|

Indicates the type of color interpolation to apply, when mapping a data field
onto a color scale.

For details see the
<https://vega.github.io/vega-lite/docs/scale.html#continuous Vega-Lite documentation>.

-}
data CInterpolate
    = CubeHelix Double
      -- ^ Cube helix color interpolation for continuous color scales using the given
      --   gamma value (anchored at 1).
    | CubeHelixLong Double
      -- ^ Long-path cube helix color interpolation for continuous color scales using
      --   the given gamma value (anchored at 1).
    | Hcl
      -- ^ HCL color interpolation for continuous color scales.
    | HclLong
      -- ^ HCL color interpolation in polar coordinate space for continuous color scales.
    | Hsl
      -- ^ HSL color interpolation for continuous color scales.
    | HslLong
      -- ^ HSL color interpolation in polar coordinate space for continuous color scales.
    | Lab
      -- ^ Lab color interpolation for continuous color scales.
    | Rgb Double
      -- ^ RGB color interpolation for continuous color scales using the given gamma
      --   value (anchored at 1).


-- Need to tie down some types as things are too polymorphic,
-- particularly in the presence of OverloadedStrings.
--
pairT :: T.Text -> T.Text -> (T.Text, A.Value)
pairT a b = a .= b


cInterpolateSpec :: CInterpolate -> VLSpec
cInterpolateSpec (Rgb gamma) = object [pairT "type" "rgb", "gamma" .= gamma]
cInterpolateSpec Hsl = object [pairT "type" "hsl"]
cInterpolateSpec HslLong = object [pairT "type" "hsl-long"]
cInterpolateSpec Lab = object [pairT "type" "lab"]
cInterpolateSpec Hcl = object [pairT "type" "hcl"]
cInterpolateSpec HclLong = object [pairT "type" "hcl-long"]
cInterpolateSpec (CubeHelix gamma) = object [pairT "type" "cubehelix", "gamma" .= gamma]
cInterpolateSpec (CubeHelixLong gamma) = object [pairT "type" "cubehelix-long", "gamma" .= gamma]


{-| The properties for a single view or layer background.

Used with 'Graphics.Vega.VegaLite.viewBackground' and
'Graphics.Vega.VegaLite.ViewBackgroundStyle'.

In version @0.6.0.0@ the constructors that used to take an optional color,
namely 'VBFill' and 'VBStroke', were split out, so that they
now take a 'Color' argument and new constructors - 'VBNoFill' and
'VBNoStroke' - were added to replace the @Nothing@ versions.

@since 0.4.0.0

-}

data ViewBackground
    = VBStyle [StyleLabel]
    -- ^ A list of named styles to apply. A named style can be specified
    --   via 'Graphics.Vega.VegaLite.MarkNamedStyles'. Later styles in the list will
    --   override earlier ones if there is a conflict in any of the mark
    --   properties.
    | VBCornerRadius Double
    -- ^ The radius in pixels of rounded corners.
    | VBFill Color
    -- ^ Fill color. See also 'VBNoFill'.
    --
    --   This was changed to use the @Color@ type alias in version @0.5.0.0@
    --   and removed the @Maybe@ type in version @0.6.0.0@.
    | VBNoFill
    -- ^ Do not use a fill. See also 'VBFill'.
    --
    --   @since 0.6.0.0
    | VBFillOpacity Opacity
    -- ^ Fill opacity.
    | VBOpacity Opacity
    -- ^ Overall opacity.
    | VBStroke Color
    -- ^ The stroke color for a line around the background. See also 'VBNoStroke'.
    --
    --   This was changed to use the @Color@ type alias in version @0.5.0.0@
    --   and removed the @Maybe@ type in version @0.6.0.0@.
    | VBNoStroke
    -- ^ Do not use a stroke. See also 'VBStroke'.
    --
    --   @since 0.6.0.0
    | VBStrokeOpacity Opacity
    -- ^ The opacity of the line around the background, if drawn.
    | VBStrokeWidth Double
    -- ^ The width of the line around the background, if drawn.
    | VBStrokeCap StrokeCap
    -- ^ The cap line-ending for the line around the background, if drawn.
    | VBStrokeDash DashStyle
    -- ^ The dash pattern of the line around the background, if drawn.
    | VBStrokeDashOffset DashOffset
    -- ^ The offset of the dash pattern for the line around the background, if drawn.
    | VBStrokeJoin StrokeJoin
    -- ^ The line-joining style of the line around the background, if drawn.
    | VBStrokeMiterLimit Double
    -- ^ The mitre limit at which to bevel the line around the background, if drawn.


viewBackgroundSpec :: ViewBackground -> LabelledSpec
viewBackgroundSpec (VBStyle [style]) = "style" .= style  -- special case singleton
viewBackgroundSpec (VBStyle styles) = "style" .= styles
viewBackgroundSpec (VBCornerRadius r) = "cornerRadius" .= r
viewBackgroundSpec (VBFill s) = "fill" .= s
viewBackgroundSpec VBNoFill = "fill" .= A.Null
viewBackgroundSpec (VBFillOpacity x) = "fillOpacity" .= x
viewBackgroundSpec (VBOpacity x) = "opacity" .= x
viewBackgroundSpec (VBStroke s) = "stroke" .= s
viewBackgroundSpec VBNoStroke = "stroke" .= A.Null
viewBackgroundSpec (VBStrokeOpacity x) = "strokeOpacity" .= x
viewBackgroundSpec (VBStrokeCap cap) = "strokeCap" .= strokeCapLabel cap
viewBackgroundSpec (VBStrokeJoin jn) = "strokeJoin" .= strokeJoinLabel jn
viewBackgroundSpec (VBStrokeWidth x) = "strokeWidth" .= x
viewBackgroundSpec (VBStrokeDash xs) = "strokeDash" .= fromDS xs
viewBackgroundSpec (VBStrokeDashOffset x) = "strokeDashOffset" .= x
viewBackgroundSpec (VBStrokeMiterLimit x) = "strokeMiterLimit" .= x


{-|

Represents a facet header property. For details, see the
<https://vega.github.io/vega-lite/docs/facet.html#header Vega-Lite documentation>.

Labels refer to the title of each sub-plot in a faceted view and
title is the overall title of the collection.

-}

{-
In 4.2.0 this represents both

  HeaderConfig
  Header

which have the same keys.

-}

data HeaderProperty
    = HFormat T.Text
      -- ^ [Formatting pattern](https://vega.github.io/vega-lite/docs/format.html) for
      --   facet header (title) values. To distinguish between formatting as numeric values
      --   and data/time values, additionally use 'HFormatAsNum' or 'HFormatAsTemporal'.
    | HFormatAsNum
      -- ^ Facet headers should be formatted as numbers. Use a
      --   [d3 numeric format string](https://github.com/d3/d3-format#locale_format)
      --   with 'HFormat'.
      --
      -- @since 0.4.0.0
    | HFormatAsTemporal
      -- ^ Facet headers should be formatted as dates or times. Use a
      --   [d3 date/time format string](https://github.com/d3/d3-time-format#locale_format)
      --   with 'HFormat'.
      --
      -- @since 0.4.0.0
    | HLabel Bool
      -- ^ Should labels be included as part of the header. The default is @True@.
      --
      --   @since 0.6.0.0
    | HLabelAlign HAlign
      -- ^ The horizontal alignment of the labels.
      --
      -- @since 0.4.0.0
    | HLabelAnchor APosition
      -- ^ The anchor position for the labels.
      --
      -- @since 0.4.0.0
    | HLabelAngle Angle
      -- ^ The angle to draw the labels. The default is 0 for column headers
      --   and -90 for row headers.
      --
      --   @since 0.4.0.0
    | HLabelColor Color
      -- ^ The color of the labels.
      --
      -- @since 0.4.0.0
    | HLabelExpr VegaExpr
      -- ^ The expression used to generate header labels.
      --
      --   The expression can use @datum.value@ and @datum.label@ to access
      --   the data value and default label text respectively.
      --
      --   @since 0.6.0.0
    | HLabelFont T.Text
      -- ^ The font for the labels.
      --
      -- @since 0.4.0.0
    | HLabelFontSize Double
      -- ^ The font size for the labels.
      --
      -- @since 0.4.0.0
    | HLabelFontStyle T.Text
      -- ^ The font style for the labels.
      --
      --   @since 0.6.0.0
    | HLabelLimit Double
      -- ^ The maximum length of each label.
      --
      -- @since 0.4.0.0
    | HLabelOrient Side
      -- ^ The position of the label relative to its sub-plot.
      --
      -- @since 0.4.0.0
    | HLabelPadding Double
      -- ^ The spacing in pixels between the label and its sub-plot.
      --
      -- @since 0.4.0.0
    | HTitle T.Text
      -- ^ The title for the facets.
    | HNoTitle
      -- ^ Draw no title for the facets.
      --
      -- @since 0.4.0.0
    | HTitleAlign HAlign
      -- ^ The horizontal alignment of the title.
      --
      -- @since 0.4.0.0
    | HTitleAnchor APosition
      -- ^ The anchor position for the title.
      --
      -- @since 0.4.0.0
    | HTitleAngle Angle
      -- ^ The angle to draw the title.
      --
      -- @since 0.4.0.0
    | HTitleBaseline VAlign
      -- ^ The vertical alignment of the title.
      --
      -- @since 0.4.0.0
    | HTitleColor Color
      -- ^ The color of the title.
      --
      -- @since 0.4.0.0
    | HTitleFont T.Text
      -- ^ The font for the title.
      --
      -- @since 0.4.0.0
    | HTitleFontSize Double
      -- ^ The font size for the title.
      --
      -- @since 0.4.0.0
    | HTitleFontStyle T.Text
      -- ^ The font style for the title.
      --
      --   @since 0.6.0.0
    | HTitleFontWeight T.Text
      -- ^ The font weight for the title.
      --
      -- @since 0.4.0.0
    | HTitleLimit Double
      -- ^ The maximum length of the title.
      --
      -- @since 0.4.0.0
    | HTitleLineHeight Double
      -- ^ The line height, in pixels, for multi-line title text.
      --
      --   @since 0.6.0.0
    | HTitleOrient Side
      -- ^ The position of the title relative to the sub-plots.
      --
      -- @since 0.4.0.0
    | HTitlePadding Double
      -- ^ The spacing in pixels between the title and the labels.
      --
      -- @since 0.4.0.0


headerProperty :: HeaderProperty -> LabelledSpec
headerProperty (HFormat fmt) = "format" .= fmt
headerProperty HFormatAsNum = "formatType" .= fromT "number"
headerProperty HFormatAsTemporal = "formatType" .= fromT "time"
headerProperty (HTitle ttl) = "title" .= splitOnNewline ttl
headerProperty HNoTitle = "title" .= A.Null
headerProperty (HLabel b) = "labels" .= b
headerProperty (HLabelAlign ha) = "labelAlign" .= hAlignLabel ha
headerProperty (HLabelAnchor a) = "labelAnchor" .= anchorLabel a
headerProperty (HLabelAngle x) = "labelAngle" .= x
headerProperty (HLabelColor s) = "labelColor" .= fromColor s
headerProperty (HLabelExpr s) = "labelExpr" .= s
headerProperty (HLabelFont s) = "labelFont" .= s
headerProperty (HLabelFontSize x) = "labelFontSize" .= x
headerProperty (HLabelFontStyle s) = "labelFontStyle" .= s
headerProperty (HLabelLimit x) = "labelLimit" .= x
headerProperty (HLabelOrient orient) = "labelOrient" .= sideLabel orient
headerProperty (HLabelPadding x) = "labelPadding" .= x
headerProperty (HTitleAlign ha) = "titleAlign" .= hAlignLabel ha
headerProperty (HTitleAnchor a) = "titleAnchor" .= anchorLabel a
headerProperty (HTitleAngle x) = "titleAngle" .= x
headerProperty (HTitleBaseline va) = "titleBaseline" .= vAlignLabel va
headerProperty (HTitleColor s) = "titleColor" .= fromColor s
headerProperty (HTitleFont s) = "titleFont" .= s
headerProperty (HTitleFontWeight s) = "titleFontWeight" .= s
headerProperty (HTitleFontSize x) = "titleFontSize" .= x
headerProperty (HTitleFontStyle s) = "titleFontStyle" .= s
headerProperty (HTitleLimit x) = "titleLimit" .= x
headerProperty (HTitleLineHeight x) = "titleLineHeight" .= x
headerProperty (HTitleOrient orient) = "titleOrient" .= sideLabel orient
headerProperty (HTitlePadding x) = "titlePadding" .= x
