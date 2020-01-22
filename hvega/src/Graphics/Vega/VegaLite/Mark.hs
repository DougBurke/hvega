{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Mark
Copyright   : (c) Douglas Burke, 2018-2020
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings

This provides the functionality of the VegaLite module but is
not directly exported to the user.

-}

module Graphics.Vega.VegaLite.Mark
       ( Mark(..)
       , MarkProperty(..)
       , MarkInterpolation(..)
       , PointMarker(..)
       , LineMarker(..)
       , MarkErrorExtent(..)
       , GradientCoord
       , GradientStops
       , ColorGradient(..)
       , GradientProperty(..)
       , TextDirection(..)

         -- not for external export
       , mprops_

       , markLabel
       , markProperty

       ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

import Data.Aeson ((.=), object, toJSON)
import Data.List (sortOn)


import Graphics.Vega.VegaLite.Foundation
  ( Angle
  , Color
  , DashStyle
  , DashOffset
  , Cursor
  , FontWeight
  , Opacity
  , Orientation
  , StrokeCap
  , StrokeJoin
  , Symbol
  , TooltipContent(TTNone)
  , HAlign
  , VAlign
  , fromColor
  , fromDS
  , cursorLabel
  , fontWeightSpec
  , orientationSpec
  , strokeCapLabel
  , strokeJoinLabel
  , symbolLabel
  , ttContentLabel
  , hAlignLabel
  , vAlignLabel
  )
import Graphics.Vega.VegaLite.Specification
  ( VLSpec
  , LabelledSpec
  )


-- TODO: should this turn an empty list into true?
mprops_ :: T.Text -> [MarkProperty] -> LabelledSpec
mprops_ f mps = f .= object (map markProperty mps)


-- | Type of visual mark used to represent data in the visualization.
--
--   The properties of the mark can be changed with the 'MarkProperty'
--   constructors - such as 'MHeight' and 'MWidth' -  although not all
--   properties apply to all marks.
--
data Mark
    = Area
      -- ^ An [area mark](https://vega.github.io/vega-lite/docs/area.html)
      --   for representing a series of data elements, such as in a stacked
      --   area chart or streamgraph.
    | Bar
      -- ^ [Bar mark](https://vega.github.io/vega-lite/docs/bar.html)
      --   for histograms, bar charts etc.
    | Boxplot
      -- ^ [Boxplot composite mark](https://vega.github.io/vega-lite/docs/boxplot.html)
      --   for showing summaries of statistical distributions.
      --
      --   Tick marks can be added using 'MTicks' and outliers turned
      --   off with 'MNoOutliers' or configured with 'MOutliers'.
      --   For example:
      --
      --   @
      --   'Graphics.Vega.VegaLite.mark' Boxplot
      --       [ 'MTicks' [ 'MColor' \"black\", 'MSize' 8 ]
      --       , 'MBox' [ 'MFill' \"grey\" ]
      --       , 'MOutliers' [ 'MColor' \"firebrick\" ]
      --   ]
      --   @
      --
      --   The range of the box plot is controlled with 'MExtent' with
      --   the 'IqrScale' or 'ExRange' options (the default is
      --   @IqrScale 1.5@).
      --
      --   @since 0.4.0.0
    | Circle
      -- ^ [Circle mark](https://vega.github.io/vega-lite/docs/circle.html)
      --   for representing points.
    | ErrorBar
      -- ^ [Errorbar composite mark](https://vega.github.io/vega-lite/docs/errorbar.html)
      --   for showing summaries of variation along a signal. By default
      --   no ticks are drawn. To add ticks with default properties use
      --   @`MTicks` []@, otherwise supply a list of configuration options:
      --
      --   @
      --   'Graphics.Vega.VegaLite.mark' ErrorBar [ 'MTicks' [ 'MColor' \"black\", 'MSize' 8 ] ]
      --   @
      --
      --   @since 0.4.0.0
    | ErrorBand
      -- ^ [Errorband composite mark](https://vega.github.io/vega-lite/docs/errorband.html)
      --   for showing summaries of variation along a signal. By default
      --   no border is drawn. To add a border with default properties use
      --   @'MBorders' []@, otherwise supply a list of configuration options:
      --
      --   @
      --   'Graphics.Vega.VegaLite.mark' ErrorBand [ 'MBorders' [ 'MColor' \"black\", 'MStrokeWidth' 0.5 ] ]
      --   @
      --
      --   @since 0.4.0.0
    | Geoshape
      -- ^ [Geoshape](https://vega.github.io/vega-lite/docs/geoshape.html)
      --   determined by geographically referenced coordinates.
    | Image
      -- ^ [Vega Lite image mark](https://vega.github.io/vega-lite/docs/image.html),
      --   where the image to display is given via the
      --   'Graphics.Vega.VegaLite.url' channel, and the width and height
      --   defined by the 'MWidth' and 'MHeight' properties.
      --
      --   @since 0.5.0.0
    | Line
      -- ^ [Line mark](https://vega.github.io/vega-lite/docs/line.html)
      --   for symbolising a sequence of values.
    | Point
      -- ^ [Point mark](https://vega.github.io/vega-lite/docs/point.html)
      --   for symbolising a data point with a symbol.
    | Rect
      -- ^ [Rectangle mark](https://vega.github.io/vega-lite/docs/rect.html).
    | Rule
      -- ^ [Rule line](https://vega.github.io/vega-lite/docs/rule.html)
      --   connecting two vertices.
    | Square
      -- ^ [Square mark](https://vega.github.io/vega-lite/docs/square.html)
      --   for symbolising points.
    | Text
      -- ^ [Text mark](https://vega.github.io/vega-lite/docs/text.html)
      --   to be displayed at some point location.
    | Tick
      -- ^ Short line - [tick](https://vega.github.io/vega-lite/docs/tick.html) -
      --   mark for symbolising point locations.
    | Trail
      -- ^ [Trail mark](https://vega.github.io/vega-lite/docs/trail.html)
      --   (line with variable width along its length).
      --
      --   @since 0.4.0.0


markLabel :: Mark -> T.Text
markLabel Area = "area"
markLabel Bar = "bar"
markLabel Boxplot = "boxplot"
markLabel Circle = "circle"
markLabel ErrorBar = "errorbar"
markLabel ErrorBand = "errorband"
markLabel Line = "line"
markLabel Geoshape = "geoshape"
markLabel Image = "image"
markLabel Point = "point"
markLabel Rect = "rect"
markLabel Rule = "rule"
markLabel Square = "square"
markLabel Text = "text"
markLabel Tick = "tick"
markLabel Trail = "trail"


{-|

Properties for customising the appearance of a mark. For details see the
<https://vega.github.io/vega-lite/docs/mark.html#config Vega-Lite documentation>.

Not all properties are valid for each mark type.

The Vega-Lite specification supports setting those properties that take
@['MarkProperty']@ also to a boolean value. This is currently not
supported in @hvega@.

In @version 0.5.0.0@ the 'MRemoveInvalid' constructor was added, which
replaces the @RemoveInvalid@ constructor of
'Graphics.Vega.VegaLite.ConfigurationProperty', and the
@MShortTimeLabels@ constuctor was removed.

-}

-- based on schema
--     #/definitions/MarkConfig
--     #/definitions/MarkDef
--     #/definitions/OverlayMarkDef
--
--     #/definitions/TickConfig
--
-- ie it conflates meaning

data MarkProperty
    = MAlign HAlign
      -- ^ Horizontal alignment of a text mark.
    | MAngle Angle
      -- ^ Rotation angle of a text mark.
    | MAspect Bool
      -- ^ Should the aspect ratio of an 'Image' mark be preserved?
      --
      --   @since 0.5.0.0
    | MBandSize Double
      -- ^ Band size of a bar mark.
    | MBaseline VAlign
      -- ^ Vertical alignment of a text mark.
    | MBinSpacing Double
      -- ^ Offset between bars for a binned field using a bar mark.
      --
      --   The ideal value for this is either @0@ (preferred by statisticians)
      --   or @1@ (the Vega-Lite default value, D3 example style).
    | MBorders [MarkProperty]
      -- ^ Border properties for an 'ErrorBand' mark.
      --
      --   @since 0.4.0.0
    | MBox [MarkProperty]
      -- ^ Box-symbol properties for a 'Boxplot' mark.
      --
      --   @since 0.4.0.0
    | MClip Bool
      -- ^ Should a mark be clipped to the enclosing group's dimensions.
    | MColor Color
      -- ^ Default color of a mark. Note that 'MFill' and 'MStroke' have higher
      --   precedence and will override this if specified.
    | MColorGradient ColorGradient GradientStops [GradientProperty]
      -- ^ The color gradient to apply to a mark. The first argument
      --   determines its type, the second is the list of color
      --   interpolation points, and the third
      --   allows for customization.
      --
      --   @
      --   'MColorGradient'
      --       'GrRadial'
      --       [ ( 0, \"red\" ), ( 1, \"blue\" ) ]
      --       [ ]
      --   @
      --
      --   @since 0.5.0.0
    | MCornerRadius Double
      -- ^ Corner radius of all corners of a rectangular mark, in pixels.
      --
      --   The default is 0. This value is over-ridden by any of
      --   'MCornerRadiusTL', 'MCornerRadiusTR', 'MCornerRadiusBL',
      --   or 'MCornerRadiusBR'.
      --
      --   @since 0.5.0.0
    | MCornerRadiusTL Double
      -- ^ Top-left corner radius of a rectangular mark, in pixels.
      --
      --   The default is 0.
      --
      --   @since 0.5.0.0
    | MCornerRadiusTR Double
      -- ^ Top-right corner radius of a rectangular mark, in pixels.
      --
      --   The default is 0.
      --
      --   @since 0.5.0.0
    | MCornerRadiusBL Double
      -- ^ Bottom-left corner radius of a rectangular mark, in pixels.
      --
      --   The default is 0.
      --
      --   @since 0.5.0.0
    | MCornerRadiusBR Double
      -- ^ Bottom-right corner radius of a rectangular mark, in pixels.
      --
      --   The default is 0.
      --
      --   @since 0.5.0.0
    | MCursor Cursor
      -- ^ Cursor to be associated with a hyperlink mark.
    | MDir TextDirection
      -- ^ Direction of the text. This property determines which side of the
      --   label is truncated by the 'MLimit' parameter. See also 'MEllipsis'.
      --
      --   The default is 'LTR'.
      --
      --   @since 0.5.0.0
    | MContinuousBandSize Double
      -- ^ Continuous band size of a bar mark.
    | MDiscreteBandSize Double
      -- ^ Discrete band size of a bar mark.
    | MdX Double
      -- ^ Horizontal offset between a text mark and its anchor.
    | MdY Double
      -- ^ Vertical offset between a text mark and its anchor.
    | MEllipsis T.Text
      -- ^ The ellipsis string for text truncated in response to
      --   'MLimit'. See also 'MDir'.
      --
      --   The default is @\"…\"@.
      --
      --   @since 0.5.0.0
    | MExtent MarkErrorExtent
      -- ^ Extent of whiskers used with 'Boxplot', 'ErrorBar', and
      --   'ErrorBand' marks.
      --
      --   @since 0.4.0.0
    | MFill Color
      -- ^ Default fill color of a mark.
      --
      --   This was changed to use the @Color@ type alias in version @0.5.0.0@.
    | MFilled Bool
      -- ^ Should a mark's color should be used as the fill color instead of
      --   stroke color.
    | MFillGradient ColorGradient GradientStops [GradientProperty]
      -- ^ The color gradient to apply to the interior of a mark. The first argument
      --   determines its type, the second is the list of color
      --   interpolation points, and the third
      --   allows for customization.
      --
      --   @
      --   'MFillGradient'
      --       'GrLinear'
      --       [ ( 0, \"orange\" ), ( 1, \"green\" ) ]
      --       [ ]
      --   @
      --
      --   @since 0.5.0.0
    | MFillOpacity Opacity
      -- ^ Fill opacity of a mark.
    | MFont T.Text
      -- ^ Font of a text mark. Can be any font name made accessible via
      -- a css file (or a generic font like \"serif\", \"monospace\" etc.).
    | MFontSize Double
      -- ^ Font size, in pixels, used by a text mark.
    | MFontStyle T.Text
      -- ^ Font style (e.g. \"italic\") used by a text mark.
    | MFontWeight FontWeight
      -- ^ Font weight used by a text mark.
    | MHeight Double
      -- ^ Explicitly set the height of a mark. See also 'MWidth'.
      --
      --   @since 0.4.0.0
    | MHRef T.Text
      -- ^ Hyperlink to be associated with a mark making it a clickable
      --   hyperlink.
      --
      --   @since 0.4.0.0
    | MInterpolate MarkInterpolation
      -- ^ Interpolation method used by line and area marks.
    | MLimit Double
      -- ^ The maximum length of the text mark in pixels. If the text is
      --   larger then it will be truncated, with the truncation controlled
      --   by 'MEllipsis' and 'MDir'.
      --
      --   The default value is @0@, which indicates no truncation.
      --
      --   @since 0.5.0.0
    | MLine LineMarker
      -- ^ How should the vertices of an area mark be joined?
      --
      --   @since 0.4.0.0
    | MLineBreak T.Text
      -- ^ A delimeter, such as a newline character, upon which to break
      --   text strings into multiple lines.
      --
      --   Note that @hvega@ automatically breaks text on the @\\n@ character,
      --   which will over-ride this setting. Therefore setting this only
      --   makes sense if the text does not contain @\n@ characters.
      --
      --   @since 0.5.0.0
    | MLineHeight Double
      -- ^ The height, in pixels, of each line of text in a multi-line text mark.
      --
      --   @since 0.5.0.0
    | MMedian [MarkProperty]
      -- ^ Median-line properties for the 'Boxplot' mark.
      --
      --   @since 0.4.0.0
    | MOpacity Opacity
      -- ^ Overall opacity of a mark in the range 0 to 1.
    | MOrder Bool
      -- ^ Ordering of vertices in a line or area mark. If @True@ (the default),
      --   the order is determined by measurement type or order channel. If
      --   @False@, the original data order is used.
      --
      --   @since 0.4.0.0
    | MOrient Orientation
      -- ^ Orientation of a non-stacked bar, tick, area or line mark.
    | MOutliers [MarkProperty]
      -- ^ Outlier symbol properties for the 'Boxplot' mark.
      --
      --   @since 0.4.0.0
    | MNoOutliers
      -- ^ Do not draw outliers with the 'Boxplot' mark.
      --
      --   @since 0.4.0.0
    | MPoint PointMarker
      -- ^ Appearance of a point marker joining the vertices of a line or area mark.
      --
      --   @since 0.4.0.0
    | MRadius Double
      -- ^ Polar coordinate radial offset of a text mark from its origin.
    | MRemoveInvalid Bool
      -- ^ The default handling of invalid (@null@ and @NaN@) values. If @True@,
      --   invalid values are skipped or filtered out when represented as marks,
      --   otherwise they are taken to be @0@.
      --
      --   This replaces @RemoveInvalid@ from
      --   'Graphics.Vega.VegaLite.ConfigurationProperty'
      --   in version 0.4 of @hvega@.
      --
      --   @since 0.5.0.0
    | MRule [MarkProperty]
      -- ^ Rule (main line) properties for the 'ErrorBar' and 'Boxplot' marks.
      --
      --   @since 0.4.0.0
    | MShape Symbol
      -- ^ Shape of a point mark.
    | MSize Double
      -- ^ Size of a mark.
    | MStroke Color
      -- ^ Default stroke color of a mark.
      --
      --   This was changed to use the @Color@ type alias in version @0.5.0.0@.
    | MStrokeCap StrokeCap
      -- ^ Cap style of a mark's stroke.
      --
      --   @since 0.4.0.0
    | MStrokeDash DashStyle
      -- ^ The stroke dash pattern used by a mark.
    | MStrokeDashOffset DashOffset
      -- ^ The offset for the dash pattern.
    | MStrokeGradient ColorGradient GradientStops [GradientProperty]
      -- ^ The color gradient to apply to the boundary of a mark. The first argument
      --   determines its type, the second is the list of color
      --   interpolation points, and the third
      --   allows for customization.
      --
      --   @
      --   'MStrokeGradient'
      --       'GrLinear'
      --       [ ( 0, \"pink\" ), ( 1, \"violet\" ) ]
      --       [ ]
      --   @
      --
      --   @since 0.5.0.0
    | MStrokeJoin StrokeJoin
      -- ^ Line segment join style of a mark's stroke.
      --
      --   @since 0.4.0.0
    | MStrokeMiterLimit Double
      -- ^ Mitre limit at which to bevel a join between line segments of a
      --   mark's stroke.
      --
      --   @since 0.4.0.0
    | MStrokeOpacity Opacity
      -- ^ Stroke opacity of a mark in the range 0 to 1.
    | MStrokeWidth Double
      -- ^ Stroke width of a mark in pixels.
    | MStyle [T.Text]
      -- ^ Names of custom styles to apply to a mark. Each should refer to a named style
      --   defined in a separate style configuration.
    | MTension Double
      -- ^ Interpolation tension used when interpolating line and area marks.
    | MText T.Text
      -- ^ Placeholder text for a text mark for when a text channel is not specified.
    | MTheta Double
      -- ^ Polar coordinate angle (clockwise from north in radians)
      --   of a text mark from the origin (determined by its
      --   x and y properties).
    | MThickness Double
      -- ^ Thickness of a tick mark.
    | MTicks [MarkProperty]
      -- ^ Tick properties for the 'ErrorBar' or 'Boxplot' mark.
      --
      --   @since 0.4.0.0
    | MTimeUnitBand Double
      -- ^ The default relative band size for a time unit.
      --
      --   If set to 1 the bandwidth of the marks will be equal to the time unit band step,
      --   and if set to 0.5 they will be half that.
      --
      --   @since 0.5.0.0
    | MTimeUnitBandPosition Double
      -- ^ The default relative band position for a time unit.
      --
      --   If set to 0 the marks will be positioned at the start of the band,
      --   and if set to 0.5 they will be in the middle.
      --
      --   @since 0.5.0.0
    | MTooltip TooltipContent
      -- ^ The tooltip content for a mark.
      --
      --   @since 0.4.0.0
    | MType Mark
      -- ^ The mark type.
      --
      --   @since 0.5.0.0
    | MWidth Double
      -- ^ Explicitly set the width of a mark (e.g. the bar width). See also
      --   'MHeight'.
      --
      --   @since 0.4.0.0
    | MX Double
      -- ^ X position of a mark.
      --
      --   @since 0.4.0.0
    | MX2 Double
      -- ^ X2 position of a mark. This is the secondary position for
      --   lines and area marks).
      --
      --   @since 0.4.0.0
    | MXOffset Double
      -- ^ X position offset of a mark.
      --
      --   @since 0.4.0.0
    | MX2Offset Double
      -- ^ X2 position offset of a mark.
      --
      --   @since 0.4.0.0
    | MY Double
      -- ^ Y position of a mark.
      --
      --   @since 0.4.0.0
    | MY2 Double
      -- ^ Y2 position of a mark. This is the secondary position for
      --   lines and area marks).
      --
      --   @since 0.4.0.0
    | MYOffset Double
      -- ^ Y position offset of a mark.
      --
      --   @since 0.4.0.0
    | MY2Offset Double
      -- ^ Y2 position offset of a mark.
      --
      --   @since 0.4.0.0


markProperty :: MarkProperty -> LabelledSpec

-- special case the gradients
markProperty (MColorGradient dir stops opts) =
  "color" .= gradientSpec dir stops opts
markProperty (MFillGradient dir stops opts) =
  "fill" .= gradientSpec dir stops opts
markProperty (MStrokeGradient dir stops opts) =
  "stroke" .= gradientSpec dir stops opts

-- where are these defined?
markProperty (MContinuousBandSize x) = "continuousBandSize" .= x
markProperty (MDiscreteBandSize x) = "discreteBandSize" .= x

markProperty (MAlign algn) = "align" .= hAlignLabel algn
markProperty (MAngle x) = "angle" .= x
markProperty (MAspect b) = "aspect" .= b
markProperty (MBaseline va) = "baseline" .= vAlignLabel va

-- only available in TickConfig
markProperty (MBandSize x) = "bandSize" .= x

markProperty (MBinSpacing x) = "binSpacing" .= x

-- only available in ErrorBand[Config|Def], PartsMixins<ErrorBandPart>
markProperty (MBorders mps) = mprops_ "borders" mps

-- BoxPlot[Config|Deg], PartsMixins<BoxPlotPart>
markProperty (MBox mps) = mprops_ "box" mps

markProperty (MClip b) = "clip" .= b
markProperty (MColor col) = "color" .= fromColor col
markProperty (MCornerRadius x) = "cornerRadius" .= x
markProperty (MCornerRadiusTL x) = "cornerRadiusTopLeft" .= x
markProperty (MCornerRadiusTR x) = "cornerRadiusTopRight" .= x
markProperty (MCornerRadiusBL x) = "cornerRadiusBottomLeft" .= x
markProperty (MCornerRadiusBR x) = "cornerRadiusBottomRight" .= x
markProperty (MCursor cur) = "cursor" .= cursorLabel cur
markProperty (MDir td) = "dir" .= textdirLabel td
markProperty (MdX dx) = "dx" .= dx
markProperty (MdY dy) = "dy" .= dy
markProperty (MEllipsis s) = "ellipsis" .= s

-- combo of BoxPlot[Config|Def], ErrorBand[Config|Def], ErrorBar[Config|Def]
markProperty (MExtent mee) = markErrorExtentLSpec mee

markProperty (MFill col) = "fill" .= fromColor col
markProperty (MFilled b) = "filled" .= b
markProperty (MFillOpacity x) = "fillOpacity" .= x
markProperty (MFont fnt) = "font" .= fnt
markProperty (MFontSize x) = "fontSize" .= x
markProperty (MFontStyle fSty) = "fontStyle" .= fSty
markProperty (MFontWeight w) = "fontWeight" .= fontWeightSpec w
markProperty (MHeight x) = "height" .= x
markProperty (MHRef s) = "href" .= s
markProperty (MInterpolate interp) = "interpolate" .= markInterpolationLabel interp
markProperty (MRemoveInvalid b) = "invalid" .= if b then "filter" else A.Null
markProperty (MLimit x) = "limit" .= x
markProperty (MLine lm) = "line" .= lineMarkerSpec lm
markProperty (MLineBreak s) = "lineBreak" .= s
markProperty (MLineHeight x) = "lineHeight" .= x

-- BoxPlot[Config|Def] possibly others
markProperty (MMedian mps) = mprops_ "median" mps

markProperty (MOpacity x) = "opacity" .= x
markProperty (MOrder b) = "order" .= b
markProperty (MOrient orient) = "orient" .= orientationSpec orient

-- what uses this?
markProperty (MOutliers []) = "outliers" .= True  -- TODO: should mprops_ do this?
markProperty (MOutliers mps) = mprops_ "outliers" mps
markProperty MNoOutliers = "outliers" .= False

markProperty (MPoint pm) = "point" .= pointMarkerSpec pm
markProperty (MRadius x) = "radius" .= x

-- what uses this?
markProperty (MRule mps) = mprops_ "rule" mps

markProperty (MShape sym) = "shape" .= symbolLabel sym
markProperty (MSize x) = "size" .= x
markProperty (MStroke t) = "stroke" .= fromColor t
markProperty (MStrokeCap sc) = "strokeCap" .= strokeCapLabel sc
markProperty (MStrokeDash xs) = "strokeDash" .= fromDS xs
markProperty (MStrokeDashOffset x) = "strokeDashOffset" .= x
markProperty (MStrokeJoin sj) = "strokeJoin" .= strokeJoinLabel sj
markProperty (MStrokeMiterLimit x) = "strokeMiterLimit" .= x
markProperty (MStrokeOpacity x) = "strokeOpacity" .= x
markProperty (MStrokeWidth w) = "strokeWidth" .= w
markProperty (MStyle [style]) = "style" .= style  -- special case singleton
markProperty (MStyle styles) = "style" .= styles
markProperty (MTension x) = "tension" .= x
markProperty (MText txt) = "text" .= txt
markProperty (MTheta x) = "theta" .= x
markProperty (MThickness x) = "thickness" .= x

-- what uses this?
markProperty (MTicks mps) = mprops_ "ticks" mps

markProperty (MTimeUnitBand x) = "timeUnitBand" .= x
markProperty (MTimeUnitBandPosition x) = "timeUnitBandPosition" .= x
markProperty (MTooltip TTNone) = "tooltip" .= A.Null
markProperty (MTooltip tc) = "tooltip" .= object ["content" .= ttContentLabel tc]
markProperty (MType m) = "type" .= markLabel m
markProperty (MWidth x) = "width" .= x
markProperty (MX x) = "x" .= x
markProperty (MY x) = "y" .= x
markProperty (MX2 x) = "x2" .= x
markProperty (MY2 x) = "y2" .= x
markProperty (MXOffset x) = "xOffset" .= x
markProperty (MYOffset x) = "yOffset" .= x
markProperty (MX2Offset x) = "x2Offset" .= x
markProperty (MY2Offset x) = "y2Offset" .= x

-- unlike elm, need to sort the stops list since we don't have a
-- smart constructor (although it's not obvious this is actually needed,
-- as I think Vega-Lite doesn't require this).
--
gradientSpec :: ColorGradient -> GradientStops -> [GradientProperty] -> VLSpec
gradientSpec dir stops props =
  let sortedStops = sortOn fst stops
  in object ([ "gradient" .= colorGradientLabel dir
             , "stops" .= map stopSpec sortedStops ]
             ++ map gradientProperty props)


{-|

Indicates the mark interpolation style. See the
<https://vega.github.io/vega-lite/docs/mark.html#mark-def Vega-Lite documentation>
for details.
-}
data MarkInterpolation
    = Basis
      -- ^ A B-spline interpolation between points anchored at the first
      --   and last points.
    | BasisClosed
      -- ^ Closed B-spline interpolation between points forming a polygon.
    | BasisOpen
      -- ^ Open B-spline interpolation between points, which may not
      --   intersect the first and last points.
    | Bundle
      -- ^ Bundle curve interpolation between points. This is equivalent to 'Basis'
      --   except that the tension parameter is used to straighten the spline.
    | Cardinal
      -- ^ Cardinal spline interpolation between points anchored at the first
      --   and last points.
    | CardinalClosed
      -- ^ Closed Cardinal spline interpolation between points forming a polygon.
    | CardinalOpen
      -- ^ Open Cardinal spline interpolation between points, which may not
      --   intersect the first and last points.
    | Linear
      -- ^ Linear interpolation between points.
    | LinearClosed
      -- ^ Closed linear interpolaiton between points forming a polygon.
    | Monotone
      -- ^ Cubic spline interpolation that preserves monotonicity between points.
    | StepAfter
      -- ^ Piecewise (stepped) constant interpolation function after each point in a
      --   sequence.
    | StepBefore
      -- ^ Piecewise (stepped) constant interpolation function before each point in a
      --   sequence.
    | Stepwise
      -- ^ Piecewise (stepped) constant interpolation function centred on each point
      --   in a sequence.


markInterpolationLabel :: MarkInterpolation -> T.Text
markInterpolationLabel Linear = "linear"
markInterpolationLabel LinearClosed = "linear-closed"
markInterpolationLabel Stepwise = "step"
markInterpolationLabel StepBefore = "step-before"
markInterpolationLabel StepAfter = "step-after"
markInterpolationLabel Basis = "basis"
markInterpolationLabel BasisOpen = "basis-open"
markInterpolationLabel BasisClosed = "basis-closed"
markInterpolationLabel Cardinal = "cardinal"
markInterpolationLabel CardinalOpen = "cardinal-open"
markInterpolationLabel CardinalClosed = "cardinal-closed"
markInterpolationLabel Bundle = "bundle"
markInterpolationLabel Monotone = "monotone"


-- | The properties of a point marker on a line or area mark.
--   For use with 'MPoint'.
--
--   @since 0.4.0.0

data PointMarker
    = PMTransparent
    -- ^ A transparent marker is used, which can be useful for
    --   interactive selections.
    | PMNone
    -- ^ No marker to be shown.
    | PMMarker [MarkProperty]
    -- ^ The properties of the marks to be shown at the points.
    --
    --   Use an empty list to use a filled point with default properties.

-- An empty object has the same meaning as true, so there is no real need to
-- treat 'PMMarker []' specially, but I don't think it complicates things
-- here.
--
pointMarkerSpec :: PointMarker -> VLSpec
pointMarkerSpec PMTransparent = "transparent"
pointMarkerSpec PMNone = toJSON False
pointMarkerSpec (PMMarker []) = toJSON True
pointMarkerSpec (PMMarker mps) = object (map markProperty mps)


{-|

Appearance of a line marker that is overlaid on an area mark.
For use with 'MLine'.

@since 0.4.0.0

-}

data LineMarker
  = LMNone
    -- ^ No line marker.
  | LMMarker [MarkProperty]
    -- ^ The properties of a line marker overlain on an area mark.
    --
    --   Use an empty list to use a filled point with default properties.


-- An empty object has the same meaning as true, so there is no real need to
-- treat 'LMMarker []' specially, but I don't think it complicates things
-- here.
--
lineMarkerSpec :: LineMarker -> VLSpec
lineMarkerSpec LMNone = toJSON False
lineMarkerSpec (LMMarker []) = toJSON True
lineMarkerSpec (LMMarker mps) = object (map markProperty mps)


{-|

Indicates the extent of the rule used for the error bar.  See
<https://vega.github.io/vega-lite/docs/errorbar.html#properties Vega-Lite documentation>
for details.

Note that not all options are valid for all mark types.

This is called @SummaryExtent@ in Elm and the constructors also have
different names.

@since 0.4.0.0
-}

-- based on schema 3.3.0 #/definitions/ErrorBarExtent
--          (ConfidenceInterval to Iqr)
-- and combined with the box/band "min-max" and IQR scaling values
--

data MarkErrorExtent
  = ConfidenceInterval
    -- ^ Band extent between the 95% confidence intervals of a distribution.
  | StdErr
    -- ^ Band extent as the standard error about the mean of a distribution.
  | StdDev
    -- ^ Band extent as the standard deviation of a distribution.
  | Iqr
    -- ^ Band extent between the lower and upper quartiles of a distribution
    --   (the inter-quartile range, q1 to q3).
  | ExRange
    -- ^ Band extent between the minimum and maximum values in a distribution.
  | IqrScale Double
    -- ^ A scaling of the interquartile range to be used as whiskers in a
    --   'Boxplot'. For example @IqrScale 1.5@  would extend whiskers to
    --   ±1.5x the IQR from the mean.

-- This is a little different from the other calls since I wanted to
-- make sure the scale factor was encoded as a number not a string.
--
extent_ :: T.Text -> LabelledSpec
extent_ v = "extent" .= v

markErrorExtentLSpec :: MarkErrorExtent -> LabelledSpec
markErrorExtentLSpec ConfidenceInterval = extent_ "ci"
markErrorExtentLSpec StdErr             = extent_ "stderr"
markErrorExtentLSpec StdDev             = extent_ "stdev"
markErrorExtentLSpec Iqr                = extent_ "iqr"
markErrorExtentLSpec ExRange            = extent_ "min-max"
markErrorExtentLSpec (IqrScale sc)      = "extent" .= sc


{-|
Define the form of the
<https://vega.github.io/vega-lite/docs/types.html#gradient color gradient>
(for use with 'MColorGradient' and 'MFillGradient').

@since 0.5.0.0

-}

data ColorGradient
  = GrLinear
    -- ^ A linear gradient.
  | GrRadial
    -- ^ A radial gradient.


colorGradientLabel :: ColorGradient -> T.Text
colorGradientLabel GrLinear = "linear"
colorGradientLabel GrRadial = "radial"


{-|

Convenience type-annotation to label a normalized coordinate
for color gradients. The value should be in the range 0 to 1,
inclusive. There is __no attempt__ to validate that the number
lies within this range.

@since 0.5.0.0
-}
type GradientCoord = Double


{-|

Convenience type-annotation label to indicate the color interpolation
points - i.e. the colors to use at points along the
normalized range 0 to 1 (inclusive).

The list does not have to be sorted. There is no check that the
color is valid (i.e. not empty or a valid color specification).

@since 0.5.0.0
-}
type GradientStops = [(GradientCoord, Color)]


stopSpec :: (GradientCoord, Color) -> VLSpec
stopSpec (x, c) = object [ "offset" .= x, "color" .= fromColor c ]


{-|

Control the appearance of the gradient. Used by 'MColorGradient',
'MFillGradient', and 'MStrokeGradient'.

@since 0.5.0.0

-}

data GradientProperty
  = GrX1 GradientCoord
    -- ^ The start of the color gradient (X axis); for radial
    --   gradients it represents the center of the inner circle.
    --
    --   The default for linear gradients is 0, and for radial
    --   gradients it is 0.5.
  | GrY1 GradientCoord
    -- ^ The start of the color gradient (Y axis); for radial
    --   gradients it represents the center of the inner circle.
    --
    --   The default for linear gradients is 0, and for radial
    --   gradients it is 0.5.
  | GrX2 GradientCoord
    -- ^ The end of the color gradient (X axis); for radial
    --   gradients it represents the center of the outer circle.
    --
    --   The default for linear gradients is 1, and for radial
    --   gradients it is 0.5.
  | GrY2 GradientCoord
    -- ^ The end of the color gradient (Y axis); for radial
    --   gradients it represents the center of the outer circle.
    --
    --   The default for linear gradients is 1, and for radial
    --   gradients it is 0.5.
  | GrR1 GradientCoord
    -- ^ The radius of the inner circle (radial color gradients
    --   only). The default is 0.
  | GrR2 GradientCoord
    -- ^ The radius of the outer circle (radial color gradients
    --   only). The default is 0.5.


gradientProperty :: GradientProperty -> LabelledSpec
gradientProperty (GrX1 x) = "x1" .= x
gradientProperty (GrX2 x) = "x2" .= x
gradientProperty (GrY1 x) = "y1" .= x
gradientProperty (GrY2 x) = "y2" .= x
gradientProperty (GrR1 x) = "r1" .= x
gradientProperty (GrR2 x) = "r2" .= x


{-|
Determine the direction to draw the text.

Used by 'MDir'.

@since 0.5.0.0
-}
data TextDirection
  = LTR
    -- ^ Left to right.
  | RTL
    -- ^ Right to left.

textdirLabel :: TextDirection -> T.Text
textdirLabel LTR = "ltr"
textdirLabel RTL = "rtl"
