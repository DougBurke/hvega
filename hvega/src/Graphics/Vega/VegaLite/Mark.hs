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

         -- not for external export
       , mprops_

       , markLabel
       , markProperty

       ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

import Data.Aeson ((.=), object, toJSON)


import Graphics.Vega.VegaLite.Foundation
  ( Angle
  , Color
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


-- strips leading and trailing white space and, if the result
-- is empty, returns Null, otherwise the trimmed text.
--
cleanT :: T.Text -> VLSpec
cleanT t =
  let tout = T.strip t
  in if T.null tout
     then A.Null
     else toJSON tout


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
'Graphics.Vega.VegaLite.ConfigurationProperty'.

-}

-- based on schema 3.3.0 #/definitions/MarkConfig
--
-- but it also contains a number of other properties

data MarkProperty
    = MAlign HAlign
      -- ^ Horizontal alignment of a text mark.
    | MAngle Angle
      -- ^ Rotation angle of a text mark.
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
    | MCursor Cursor
      -- ^ Cursor to be associated with a hyperlink mark.
    | MContinuousBandSize Double
      -- ^ Continuous band size of a bar mark.
    | MDiscreteBandSize Double
      -- ^ Discrete band size of a bar mark.
    | MdX Double
      -- ^ Horizontal offset between a text mark and its anchor.
    | MdY Double
      -- ^ Vertical offset between a text mark and its anchor.
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
    | MLine LineMarker
      -- ^ How should the vertices of an area mark be joined?
      --
      --   @since 0.4.0.0
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
    | MShortTimeLabels Bool
      -- ^ Aremonth and weekday names are abbreviated in a text mark?
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
    | MStrokeDash [Double]
      -- ^ The stroke dash style used by a mark, defined by an alternating \"on-off\"
      --   sequence of line lengths, in pixels.
    | MStrokeDashOffset Double
      -- ^ The number of pixels before the first line dash is drawn.
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
    | MTooltip TooltipContent
      -- ^ The tooltip content for a mark.
      --
      --   @since 0.4.0.0
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
markProperty (MFilled b) = "filled" .= b
markProperty (MBorders mps) = mprops_ "borders" mps
markProperty (MBox mps) = mprops_ "box" mps
markProperty (MClip b) = "clip" .= b
markProperty (MColor col) = "color" .= col
markProperty (MCursor cur) = "cursor" .= cursorLabel cur
markProperty (MFill col) = "fill" .= cleanT col
markProperty (MHeight x) = "height" .= x
markProperty (MStroke t) = "stroke" .= cleanT t
markProperty (MStrokeCap sc) = "strokeCap" .= strokeCapLabel sc
markProperty (MStrokeOpacity x) = "strokeOpacity" .= x
markProperty (MStrokeWidth w) = "strokeWidth" .= w
markProperty (MStrokeDash xs) = "strokeDash" .= xs
markProperty (MStrokeDashOffset x) = "strokeDashOffset" .= x
markProperty (MStrokeJoin sj) = "strokeJoin" .= strokeJoinLabel sj
markProperty (MStrokeMiterLimit x) = "strokeMiterLimit" .= x
markProperty (MMedian mps) = mprops_ "median" mps
markProperty (MOpacity x) = "opacity" .= x
markProperty (MFillOpacity x) = "fillOpacity" .= x
markProperty (MStyle [style]) = "style" .= style  -- special case singleton
markProperty (MStyle styles) = "style" .= styles
markProperty (MInterpolate interp) = "interpolate" .= markInterpolationLabel interp
markProperty (MLine lm) = "line" .= lineMarkerSpec lm
markProperty (MTension x) = "tension" .= x
markProperty (MOrder b) = "order" .= b
markProperty (MOrient orient) = "orient" .= orientationSpec orient
markProperty (MOutliers []) = "outliers" .= True  -- TODO: should mprops_ do this?
markProperty (MOutliers mps) = mprops_ "outliers" mps
markProperty MNoOutliers = "outliers" .= False
markProperty (MPoint pm) = "point" .= pointMarkerSpec pm
markProperty (MShape sym) = "shape" .= symbolLabel sym
markProperty (MSize x) = "size" .= x
markProperty (MAngle x) = "angle" .= x
markProperty (MAlign algn) = "align" .= hAlignLabel algn
markProperty (MBaseline va) = "baseline" .= vAlignLabel va
markProperty (MdX dx) = "dx" .= dx
markProperty (MdY dy) = "dy" .= dy
markProperty (MExtent mee) = markErrorExtentLSpec mee
markProperty (MFont fnt) = "font" .= fnt
markProperty (MFontSize x) = "fontSize" .= x
markProperty (MFontStyle fSty) = "fontStyle" .= fSty
markProperty (MFontWeight w) = "fontWeight" .= fontWeightSpec w
markProperty (MHRef s) = "href" .= s
markProperty (MRadius x) = "radius" .= x
markProperty (MRemoveInvalid b) = "invalid" .= if b then "filter" else A.Null
markProperty (MRule mps) = mprops_ "rule" mps
markProperty (MText txt) = "text" .= txt
markProperty (MTheta x) = "theta" .= x
markProperty (MTicks mps) = mprops_ "ticks" mps
markProperty (MBinSpacing x) = "binSpacing" .= x
markProperty (MContinuousBandSize x) = "continuousBandSize" .= x
markProperty (MDiscreteBandSize x) = "discreteBandSize" .= x
markProperty (MShortTimeLabels b) = "shortTimeLabels" .= b
markProperty (MBandSize x) = "bandSize" .= x
markProperty (MThickness x) = "thickness" .= x
markProperty (MTooltip TTNone) = "tooltip" .= A.Null
markProperty (MTooltip tc) = "tooltip" .= object ["content" .= ttContentLabel tc]
markProperty (MWidth x) = "width" .= x
markProperty (MX x) = "x" .= x
markProperty (MY x) = "y" .= x
markProperty (MX2 x) = "x2" .= x
markProperty (MY2 x) = "y2" .= x
markProperty (MXOffset x) = "xOffset" .= x
markProperty (MYOffset x) = "yOffset" .= x
markProperty (MX2Offset x) = "x2Offset" .= x
markProperty (MY2Offset x) = "y2Offset" .= x


{-|

Indicates mark interpolation style. See the
<https://vega.github.io/vega-lite/docs/mark.html#mark-def Vega-Lite documentation>
for details.
-}
data MarkInterpolation
    = Basis
    | BasisClosed
    | BasisOpen
    | Bundle
    | Cardinal
    | CardinalClosed
    | CardinalOpen
    | Linear
    | LinearClosed
    | Monotone
    | StepAfter
    | StepBefore
    | Stepwise


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
