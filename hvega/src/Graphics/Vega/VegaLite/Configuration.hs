{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Configuration
Copyright   : (c) Douglas Burke, 2018-2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings

Top-level configuration options. As this can configure most
of a visualization, it needs to import most of the other
modules.

-}

module Graphics.Vega.VegaLite.Configuration
       ( ConfigurationProperty(..)
       , FieldTitleProperty(..)

       , ViewConfig(..)
       , FacetConfig(..)
       , ConcatConfig(..)
       , ScaleConfig(..)
       , RangeConfig(..)
       , AxisConfig(..)

       , configuration
       ) where


import qualified Data.Aeson as A
import qualified Data.Text as T

import Data.Aeson ((.=), object, toJSON)


import Graphics.Vega.VegaLite.Core
  ( HeaderProperty
  , LegendConfig(..)
  , TitleConfig
  , LegendLayout(..)
  , BaseLegendLayout(..)
  , schemeProperty
  , legendOrientLabel
  , titleConfigSpec
  , header_
  )
import Graphics.Vega.VegaLite.Foundation
  ( Angle
  , Color
  , APosition
  , FontWeight
  , Opacity
  , OverlapStrategy
  , Side
  , StackOffset
  , StrokeCap
  , StrokeJoin
  , HAlign
  , VAlign
  , Padding
  , Autosize
  , anchorLabel
  , fontWeightSpec
  , orientationSpec
  , hAlignLabel
  , vAlignLabel
  , strokeCapLabel
  , strokeJoinLabel
  , sideLabel
  , overlapStrategyLabel
  , symbolLabel
  , stackOffset
  , boundsSpec
  , compositionAlignmentSpec
  , paddingSpec
  , autosizeProperty
  )
import Graphics.Vega.VegaLite.Geometry
  ( ProjectionProperty
  , projectionProperty
  )
import Graphics.Vega.VegaLite.Mark
  ( MarkProperty
  , mprops_
  )

import Graphics.Vega.VegaLite.Selection
  ( Selection
  , SelectionProperty
  , selectionProperty
  , selectionLabel
  )
import Graphics.Vega.VegaLite.Specification
  ( VLSpec
  , LabelledSpec
  , BuildLabelledSpecs
  )
  

{-|

Type of configuration property to customise. See the
<https://vega.github.io/vega-lite/docs/config.html Vega-Lite documentation>
for details.

-}

-- based on schema 3.3.0 #/definitions/Config
--
-- TODO:
--   Bar - change to BarConfig rather than MarkProperty?
--     BoxplotStyle BoxPlotConfig
--     Concat CompositionConfig
--     ErrorBand ErrorBandCOnfig
--     ErrorBar ErrorBarCOnfig
--   Facet takes CompositionConfig not FacetConfig
--     HeaderColumn takes HeaderConfig, just as HeaderStyle does
--     HeaderFacet ditto
--     HeaderRow ditto
--   LineStyle takes LineConfig not MarkConfig
--   TextStyle takes TextConfig not MarkConfig
--   TickStyle takes TickConfig not MarkConfig
--   TrailStyle takes LineConfig not MarkConfig
--

data ConfigurationProperty
    = AreaStyle [MarkProperty]
      -- ^ The default appearance of area marks.
    | Autosize [Autosize]
      -- ^ The default sizing of visualizations.
    | Axis [AxisConfig]
      -- ^ The default appearance of axes.
    | AxisBand [AxisConfig]
      -- ^ The default appearance of axes with band scaling.
    | AxisBottom [AxisConfig]
      -- ^ The default appearance of the bottom-side axes.
    | AxisLeft [AxisConfig]
      -- ^ The default appearance of the left-side axes.
    | AxisRight [AxisConfig]
      -- ^ The default appearance of the right-side axes.
    | AxisTop [AxisConfig]
      -- ^ The default appearance of the top-side axes.
    | AxisX [AxisConfig]
      -- ^ The default appearance of the X axes.
    | AxisY [AxisConfig]
      -- ^ The default appearance of the Y axes.
    | Background T.Text
      -- ^ The default background color of visualizations.
    | BarStyle [MarkProperty]
      -- ^ The default appearance of bar marks.
    | CircleStyle [MarkProperty]
      -- ^ The default appearance of circle marks.
    | ConcatStyle [ConcatConfig]
      -- ^ The default appearance of concatenated layouts.
      --
      --   @since 0.4.0.0
    | CountTitle T.Text
      -- ^ The default title style for count fields.
    | FacetStyle [FacetConfig]
      -- ^ The default appearance of facet layouts.
      --
      --   @since 0.4.0.0
    | FieldTitle FieldTitleProperty
      -- ^ The default title-generation style for fields.
    | GeoshapeStyle [MarkProperty]
      -- ^ The default appearance of geoshape marks.
      --
      --   @since 0.4.0.0
    | HeaderStyle [HeaderProperty]
      -- ^ The default appearance of facet headers.
      --
      --   @since 0.4.0.0
    | Legend [LegendConfig]
      -- ^ The default appearance of legends.
    | LineStyle [MarkProperty]
      -- ^ The default appearance of line marks.
    | MarkStyle [MarkProperty]
      -- ^ The default mark appearance.
    | NamedStyle T.Text [MarkProperty]
      -- ^ The default appearance of a single named style.
    | NamedStyles [(T.Text, [MarkProperty])]
      -- ^ The default appearance of a list of named styles.
      --
      --   @since 0.4.0.0
    | NumberFormat T.Text
      -- ^ The default number formatting for axis and text labels.
    | Padding Padding
      -- ^ The default padding in pixels from the edge of the of visualization
      --   to the data rectangle.
    | PointStyle [MarkProperty]
      -- ^ The default appearance of point marks.
    | Projection [ProjectionProperty]
      -- ^ The default style of map projections.
    | Range [RangeConfig]
      -- ^ The default range properties used when scaling.
    | RectStyle [MarkProperty]
      -- ^ The default appearance of rectangle marks.
    | RemoveInvalid Bool
      -- ^ The default handling of invalid (@null@ and @NaN@) values. If @True@,
      --   invalid values are skipped or filtered out when represented as marks.
    | RuleStyle [MarkProperty]
      -- ^ The default appearance of rule marks.
    | Scale [ScaleConfig]   -- TODO: rename ScaleStyle
      -- ^ The default properties used when scaling.
    | SelectionStyle [(Selection, [SelectionProperty])]
      -- ^ The default appearance of selection marks.
    | SquareStyle [MarkProperty]
      -- ^  the default appearance of square marks
    | Stack StackOffset
      -- ^ The default stack offset style for stackable marks.
      --
      --   Changed from @StackProperty@ in version @0.4.0.0@.
    | TextStyle [MarkProperty]
      -- ^ The default appearance of text marks.
    | TickStyle [MarkProperty]
      -- ^ The default appearance of tick marks.
    | TimeFormat T.Text
      -- ^ The default time format for axis and legend labels.
    | TitleStyle [TitleConfig]
      -- ^ The default appearance of visualization titles.
    | TrailStyle [MarkProperty]
      -- ^ The default style of trail marks.
      --
      --   @since 0.4.0.0
    | View [ViewConfig]
      -- ^ The default single view style.

configProperty :: ConfigurationProperty -> LabelledSpec
configProperty (Autosize aus) = "autosize" .= object (map autosizeProperty aus)
configProperty (Background bg) = "background" .= bg
configProperty (CountTitle ttl) = "countTitle" .= ttl
configProperty (ConcatStyle cps) = "concat" .= object (map concatConfigProperty cps)
configProperty (FieldTitle ftp) = "fieldTitle" .= fieldTitleLabel ftp
configProperty (RemoveInvalid b) = "invalidValues" .= if b then "filter" else A.Null
configProperty (NumberFormat fmt) = "numberFormat" .= fmt
configProperty (Padding pad) = "padding" .= paddingSpec pad
configProperty (TimeFormat fmt) = "timeFormat" .= fmt
configProperty (Axis acs) = "axis" .= object (map axisConfigProperty acs)
configProperty (AxisX acs) = "axisX" .= object (map axisConfigProperty acs)
configProperty (AxisY acs) = "axisY" .= object (map axisConfigProperty acs)
configProperty (AxisLeft acs) = "axisLeft" .= object (map axisConfigProperty acs)
configProperty (AxisRight acs) = "axisRight" .= object (map axisConfigProperty acs)
configProperty (AxisTop acs) = "axisTop" .= object (map axisConfigProperty acs)
configProperty (AxisBottom acs) = "axisBottom" .= object (map axisConfigProperty acs)
configProperty (AxisBand acs) = "axisBand" .= object (map axisConfigProperty acs)
configProperty (Legend lcs) = "legend" .= object (map legendConfigProperty lcs)
configProperty (MarkStyle mps) = mprops_ "mark" mps
configProperty (Projection pps) = "projection" .= object (map projectionProperty pps)
configProperty (AreaStyle mps) = mprops_ "area" mps
configProperty (BarStyle mps) = mprops_ "bar" mps
configProperty (CircleStyle mps) = mprops_ "circle" mps
configProperty (FacetStyle fps) = "facet" .= object (map facetConfigProperty fps)
configProperty (GeoshapeStyle mps) = mprops_ "geoshape" mps
configProperty (HeaderStyle hps) = header_ hps
configProperty (LineStyle mps) = mprops_ "line" mps
configProperty (PointStyle mps) = mprops_ "point" mps
configProperty (RectStyle mps) = mprops_ "rect" mps
configProperty (RuleStyle mps) = mprops_ "rule" mps
configProperty (SquareStyle mps) = mprops_ "square" mps
configProperty (TextStyle mps) = mprops_ "text" mps
configProperty (TickStyle mps) = mprops_ "tick" mps
configProperty (TitleStyle tcs) = "title" .= object (map titleConfigSpec tcs)
configProperty (NamedStyle nme mps) = "style" .= object [mprops_ nme mps]
configProperty (NamedStyles styles) =
  let toStyle = uncurry mprops_
  in "style" .= object (map toStyle styles)
configProperty (Scale scs) = scaleConfig_ scs
configProperty (Stack so) = stackOffset so
configProperty (Range rcs) = "range" .= object (map rangeConfigProperty rcs)
configProperty (SelectionStyle selConfig) =
  let selProp (sel, sps) = selectionLabel sel .= object (map selectionProperty sps)
  in "selection" .= object (map selProp selConfig)
configProperty (TrailStyle mps) = mprops_ "trail" mps
configProperty (View vcs) = "view" .= object (map viewConfigProperty vcs)


{-|

Scale configuration property. These are used to configure all scales.
For more details see the
<https://vega.github.io/vega-lite/docs/scale.html#scale-config Vega-Lite documentation>.

-}

data ScaleConfig
    = SCBandPaddingInner Double
      -- ^ Default inner padding for x and y band-ordinal scales.
    | SCBandPaddingOuter Double
      -- ^ Default outer padding for x and y band-ordinal scales.
    | SCBarBandPaddingInner Double
      -- ^ Default inner padding for x and y band-ordinal scales of 'Graphics.Vega.VegaLite.Bar' marks.
      --
      --   @since 0.4.0.0
    | SCBarBandPaddingOuter Double
      -- ^ Default outer padding for x and y band-ordinal scales of 'Graphics.Vega.VegaLite.Bar' marks.
      --
      --   @since 0.4.0.0
    | SCRectBandPaddingInner Double
      -- ^ Default inner padding for x and y band-ordinal scales of 'Graphics.Vega.VegaLite.Rect' marks.
      --
      --   @since 0.4.0.0
    | SCRectBandPaddingOuter Double
      -- ^ Default outer padding for x and y band-ordinal scales of 'Graphics.Vega.VegaLite.Rect' marks.
      --
      --   @since 0.4.0.0
    | SCClamp Bool
      -- ^ Whether or not by default values that exceed the data domain are clamped to
      --   the min/max range value.
    | SCMaxBandSize Double
      -- ^ Default maximum value for mapping quantitative fields to a bar's
      --   size/bandSize.
    | SCMinBandSize Double
      -- ^ Default minimum value for mapping quantitative fields to a bar's
      --   size/bandSize.
    | SCMaxFontSize Double
      -- ^ Default maximum value for mapping a quantitative field to a text
      --   mark's size.
    | SCMinFontSize Double
      -- ^ Default minimum value for mapping a quantitative field to a text
      --   mark's size.
    | SCMaxOpacity Opacity
      -- ^ Default maximum opacity for mapping a field to opacity.
    | SCMinOpacity Opacity
      -- ^ Default minimum opacity for mapping a field to opacity.
    | SCMaxSize Double
      -- ^ Default maximum size for point-based scales.
    | SCMinSize Double
      -- ^ Default minimum size for point-based scales.
    | SCMaxStrokeWidth Double
      -- ^ Default maximum stroke width for rule, line and trail marks.
    | SCMinStrokeWidth Double
      -- ^ Default minimum stroke width for rule, line and trail marks.
    | SCPointPadding Double
      -- ^ Default padding for point-ordinal scales.
    | SCRangeStep (Maybe Double)
      -- ^ Default range step for band and point scales when the mark is not text.
    | SCRound Bool
      -- ^ Are numeric values are rounded to integers when scaling? Useful
      --   for snapping to the pixel grid.
    | SCTextXRangeStep Double
      -- ^ Default range step for x band and point scales of text marks.
    | SCUseUnaggregatedDomain Bool
      -- ^ Whether or not to use the source data range before aggregation.


scaleConfig_ :: [ScaleConfig] -> LabelledSpec
-- scaleConfig_ [] = "scale" .= A.Null  -- not sure here
scaleConfig_ scs = "scale" .= object (map scaleConfigProperty scs)


-- | Indicates the style in which field names are displayed.

data FieldTitleProperty
    = Verbal
      -- ^ Creates \"Sum of field\", \"Year of date\", \"field (binned)\", etc.
    | Function
      -- ^ Creates \"SUM(field)\", \"YEAR(date)\", \"BIN(field)\", etc.
    | Plain
      -- ^ Just use the field name without any extra text.


fieldTitleLabel :: FieldTitleProperty -> T.Text
fieldTitleLabel Verbal = "verbal"
fieldTitleLabel Function = "functional"
fieldTitleLabel Plain = "plain"


legendConfigProperty :: LegendConfig -> LabelledSpec
legendConfigProperty (LeClipHeight x) = "clipHeight" .= x
legendConfigProperty (LeColumnPadding x) = "columnPadding" .= x
legendConfigProperty (LeColumns n) = "columns" .= n
legendConfigProperty (LeCornerRadius x) = "cornerRadius" .= x
legendConfigProperty (LeFillColor s) = "fillColor" .= s
legendConfigProperty (LeGradientDirection o) = "gradientDirection" .= orientationSpec o
legendConfigProperty (LeGradientHorizontalMaxLength x) = "gradientHorizontalMaxLength" .= x
legendConfigProperty (LeGradientHorizontalMinLength x) = "gradientHorizontalMinLength" .= x
legendConfigProperty (LeGradientLabelLimit x) = "gradientLabelLimit" .= x
legendConfigProperty (LeGradientLabelOffset x) = "gradientLabelOffset" .= x
legendConfigProperty (LeGradientLength x) = "gradientLength" .= x
legendConfigProperty (LeGradientOpacity x) = "gradientOpacity" .= x
legendConfigProperty (LeGradientStrokeColor s) = "gradientStrokeColor" .= s
legendConfigProperty (LeGradientStrokeWidth x) = "gradientStrokeWidth" .= x
legendConfigProperty (LeGradientThickness x) = "gradientThickness" .= x
legendConfigProperty (LeGradientVerticalMaxLength x) = "gradientVerticalMaxLength" .= x
legendConfigProperty (LeGradientVerticalMinLength x) = "gradientVerticalMinLength" .= x
legendConfigProperty (LeGridAlign ga) = "gridAlign" .= compositionAlignmentSpec ga
legendConfigProperty (LeLabelAlign ha) = "labelAlign" .= hAlignLabel ha
legendConfigProperty (LeLabelBaseline va) = "labelBaseline" .= vAlignLabel va
legendConfigProperty (LeLabelColor s) = "labelColor" .= s
legendConfigProperty (LeLabelFont s) = "labelFont" .= s
legendConfigProperty (LeLabelFontSize x) = "labelFontSize" .= x
legendConfigProperty (LeLabelFontStyle s) = "labelFontStyle" .= s
legendConfigProperty (LeLabelFontWeight fw) = "labelFontWeight" .= fontWeightSpec fw
legendConfigProperty (LeLabelLimit x) = "labelLimit" .= x
legendConfigProperty (LeLabelOffset x) = "labelOffset" .= x
legendConfigProperty (LeLabelOpacity x) = "labelOapcity" .= x
legendConfigProperty (LeLabelOverlap olap) = "labelOverlap" .= overlapStrategyLabel olap
legendConfigProperty (LeLabelPadding x) = "labelPadding" .= x
legendConfigProperty (LeLabelSeparation x) = "labelSeparation" .= x
legendConfigProperty (LeLayout ll) = "layout" .= object (map legendLayoutSpec ll)
legendConfigProperty (LeLeX x) = "legendX" .= x
legendConfigProperty (LeLeY x) = "legendY" .= x
legendConfigProperty (LeOffset x) = "offset" .= x
legendConfigProperty (LeOrient orl) = "orient" .= legendOrientLabel orl
legendConfigProperty (LePadding x) = "padding" .= x
legendConfigProperty (LeRowPadding x) = "rowPadding" .= x
legendConfigProperty (LeShortTimeLabels b) = "shortTimeLabels" .= b
legendConfigProperty (LeStrokeColor s) = "strokeColor" .= s
legendConfigProperty (LeStrokeDash xs) = "strokeDash" .= xs
legendConfigProperty (LeStrokeWidth x) = "strokeWidth" .= x
legendConfigProperty (LeSymbolBaseFillColor s) = "symbolBaseFillColor" .= s
legendConfigProperty (LeSymbolBaseStrokeColor s) = "symbolBaseStrokeColor" .= s
legendConfigProperty (LeSymbolDash xs) = "symbolDash" .= xs
legendConfigProperty (LeSymbolDashOffset x) = "symbolDashOffset" .= x
legendConfigProperty (LeSymbolDirection o) = "symbolDirection" .= orientationSpec o
legendConfigProperty (LeSymbolFillColor s) = "symbolFillColor" .= s
legendConfigProperty (LeSymbolOffset x) = "symbolOffset" .= x
legendConfigProperty (LeSymbolOpacity x) = "symbolOpacity" .= x
legendConfigProperty (LeSymbolSize x) = "symbolSize" .= x
legendConfigProperty (LeSymbolStrokeColor s) = "symbolStrokeColor" .= s
legendConfigProperty (LeSymbolStrokeWidth x) = "symbolStrokeWidth" .= x
legendConfigProperty (LeSymbolType s) = "symbolType" .= symbolLabel s
legendConfigProperty (LeTitle s) = "title" .= s
legendConfigProperty LeNoTitle = "title" .= A.Null
legendConfigProperty (LeTitleAlign ha) = "titleAlign" .= hAlignLabel ha
legendConfigProperty (LeTitleAnchor anc) = "titleAnchor" .= anchorLabel anc
legendConfigProperty (LeTitleBaseline va) = "titleBaseline" .= vAlignLabel va
legendConfigProperty (LeTitleColor s) = "titleColor" .= s
legendConfigProperty (LeTitleFont s) = "titleFont" .= s
legendConfigProperty (LeTitleFontSize x) = "titleFontSize" .= x
legendConfigProperty (LeTitleFontStyle s) = "titleFontStyle" .= s
legendConfigProperty (LeTitleFontWeight fw) = "titleFontWeight" .= fontWeightSpec fw
legendConfigProperty (LeTitleLimit x) = "titleLimit" .= x
legendConfigProperty (LeTitleOpacity x) = "titleOpacity" .= x
legendConfigProperty (LeTitleOrient orient) = "titleOrient" .= sideLabel orient
legendConfigProperty (LeTitlePadding x) = "titlePadding" .= x


legendLayoutSpec :: LegendLayout -> LabelledSpec
legendLayoutSpec (LeLAnchor anc) = "anchor" .= anchorLabel anc
legendLayoutSpec (LeLBottom bl) = "bottom" .= toBLSpec bl
legendLayoutSpec (LeLBottomLeft bl) = "bottom-left" .= toBLSpec bl
legendLayoutSpec (LeLBottomRight bl) = "bottom-right" .= toBLSpec bl
legendLayoutSpec (LeLBounds bnds) = "bounds" .= boundsSpec bnds
legendLayoutSpec (LeLCenter b) = "center" .= b
legendLayoutSpec (LeLDirection o) = "direction" .= orientationSpec o
legendLayoutSpec (LeLLeft bl) = "left" .= toBLSpec bl
legendLayoutSpec (LeLMargin x) = "margin" .= x
legendLayoutSpec (LeLOffset x) = "offset" .= x
legendLayoutSpec (LeLRight bl) = "right" .= toBLSpec bl
legendLayoutSpec (LeLTop bl) = "top" .= toBLSpec bl
legendLayoutSpec (LeLTopLeft bl) = "top-left" .= toBLSpec bl
legendLayoutSpec (LeLTopRight bl) = "top-right" .= toBLSpec bl


toBLSpec :: [BaseLegendLayout] -> VLSpec
toBLSpec = object . map baseLegendLayoutSpec


baseLegendLayoutSpec :: BaseLegendLayout -> LabelledSpec
baseLegendLayoutSpec (BLeLAnchor anc) = "anchor" .= anchorLabel anc
baseLegendLayoutSpec (BLeLBounds bnds) = "bounds" .= boundsSpec bnds
baseLegendLayoutSpec (BLeLCenter b) = "center" .= b
baseLegendLayoutSpec (BLeLDirection o) = "direction" .= orientationSpec o
baseLegendLayoutSpec (BLeLMargin x) = "margin" .= x
baseLegendLayoutSpec (BLeLOffset x) = "offset" .= x


{-|

Properties for customising the colors of a range. The parameter should be a
named color scheme such as @\"accent\"@ or @\"purpleorange-11\"@. For details see the
<https://vega.github.io/vega/docs/schemes/#scheme-properties Vega-Lite documentation>.
-}
data RangeConfig
    = RCategory T.Text
    | RDiverging T.Text
    | RHeatmap T.Text
    | ROrdinal T.Text
    | RRamp T.Text
    | RSymbol T.Text


rangeConfigProperty :: RangeConfig -> LabelledSpec
rangeConfigProperty rangeCfg =
  let (l, n) = case rangeCfg of
        RCategory nme -> ("category", nme)
        RDiverging nme -> ("diverging", nme)
        RHeatmap nme -> ("heatmap", nme)
        ROrdinal nme -> ("ordinal", nme)
        RRamp nme -> ("ramp", nme)
        RSymbol nme -> ("symbol", nme)

  in l .= object [schemeProperty n []]


scaleConfigProperty :: ScaleConfig -> LabelledSpec
scaleConfigProperty (SCBandPaddingInner x) = "bandPaddingInner" .= x
scaleConfigProperty (SCBandPaddingOuter x) = "bandPaddingOuter" .= x
scaleConfigProperty (SCBarBandPaddingInner x) = "barBandPaddingInner" .= x
scaleConfigProperty (SCBarBandPaddingOuter x) = "barBandPaddingOuter" .= x
scaleConfigProperty (SCRectBandPaddingInner x) = "rectBandPaddingInner" .= x
scaleConfigProperty (SCRectBandPaddingOuter x) = "rectBandPaddingOuter" .= x
scaleConfigProperty (SCClamp b) = "clamp" .= b
scaleConfigProperty (SCMaxBandSize x) = "maxBandSize" .= x
scaleConfigProperty (SCMinBandSize x) = "minBandSize" .= x
scaleConfigProperty (SCMaxFontSize x) = "maxFontSize" .= x
scaleConfigProperty (SCMinFontSize x) = "minFontSize" .= x
scaleConfigProperty (SCMaxOpacity x) = "maxOpacity" .= x
scaleConfigProperty (SCMinOpacity x) = "minOpacity" .= x
scaleConfigProperty (SCMaxSize x) = "maxSize" .= x
scaleConfigProperty (SCMinSize x) = "minSize" .= x
scaleConfigProperty (SCMaxStrokeWidth x) = "maxStrokeWidth" .= x
scaleConfigProperty (SCMinStrokeWidth x) = "minStrokeWidth" .= x
scaleConfigProperty (SCPointPadding x) = "pointPadding" .= x
scaleConfigProperty (SCRangeStep numOrNull) = "rangeStep" .= maybe A.Null toJSON numOrNull
scaleConfigProperty (SCRound b) = "round" .= b
scaleConfigProperty (SCTextXRangeStep x) = "textXRangeStep" .= x
scaleConfigProperty (SCUseUnaggregatedDomain b) = "useUnaggregatedDomain" .= b


{-|

View configuration property. These are used to configure the style of a single
view within a visualization such as its size and default fill and stroke colors.
For further details see the
<https://vega.github.io/vega-lite/docs/spec.html#config Vega-Lite documentation>.

This type has been changed in the @0.4.0.0@ release to use a consistent
naming scheme for the constructors (everything starts with @View@). Prior to
this release only @ViewWidth@ and @ViewHeight@ were named this way. There
are also five new constructors.

-}

-- based on schema 3.3.0 #/definitions/ViewConfig

data ViewConfig
    = ViewWidth Double
      -- ^ The default width of the single plot or each plot in a trellis plot when the
      --   visualization has a continuous (non-ordinal) scale or when the
      --   'SRangeStep'/'ScRangeStep' is @Nothing@ for an ordinal scale (x axis).
    | ViewHeight Double
      -- ^ The default height of the single plot or each plot in a trellis plot when the
      --   visualization has a continuous (non-ordinal) scale or when the
      --   'SRangeStep'/'ScRangeStep' is @Nothing@ for an ordinal scale (y axis).
    | ViewClip Bool
      -- ^ Should the view be clipped?
    | ViewCornerRadius Double
      -- ^ The radius, in pixels, of rounded rectangle corners.
      --
      --   The default is @0@.
      --
      --   @since 0.4.0.0
    | ViewFill (Maybe T.Text)
      -- ^ The fill color.
    | ViewFillOpacity Opacity
      -- ^ The fill opacity.
    | ViewOpacity Opacity
      -- ^ The overall opacity.
      --
      --   The default is @0.7@ for non-aggregate plots with 'Graphics.Vega.VegaLite.Point', 'Graphics.Vega.VegaLite.Tick',
      --   'Graphics.Vega.VegaLite.Circle', or 'Graphics.Vega.VegaLite.Square' marks or layered 'Graphics.Vega.VegaLite.Bar' charts, and @1@
      --   otherwise.
      --
      --   @since 0.4.0.0
    | ViewStroke (Maybe T.Text)
      -- ^ The stroke color.
    | ViewStrokeCap StrokeCap
      -- ^ The stroke cap for line-ending style.
      --
      --   @since 0.4.0.0
    | ViewStrokeDash [Double]
      -- ^ The stroke dash style. It is defined by an alternating \"on-off\"
      --   sequence of line lengths, in pixels.
    | ViewStrokeDashOffset Double
      -- ^ Number of pixels before the first line dash is drawn.
    | ViewStrokeJoin StrokeJoin
      -- ^ The stroke line-join method.
      --
      --   @since 0.4.0.0
    | ViewStrokeMiterLimit Double
      -- ^ The miter limit at which to bevel a line join.
      --
      --   @since 0.4.0.0
    | ViewStrokeOpacity Opacity
      -- ^ The stroke opacity.
    | ViewStrokeWidth Double
      -- ^ The stroke width, in pixels.


viewConfigProperty :: ViewConfig -> LabelledSpec
viewConfigProperty (ViewWidth x) = "width" .= x
viewConfigProperty (ViewHeight x) = "height" .= x
viewConfigProperty (ViewClip b) = "clip" .= b
viewConfigProperty (ViewCornerRadius x) = "cornerRadius" .= x
viewConfigProperty (ViewFill ms) = "fill" .= maybe A.Null toJSON ms
viewConfigProperty (ViewFillOpacity x) = "fillOpacity" .= x
viewConfigProperty (ViewOpacity x) = "opacity" .= x
viewConfigProperty (ViewStroke ms) = "stroke" .= maybe A.Null toJSON ms
viewConfigProperty (ViewStrokeCap sc) = "strokeCap" .= strokeCapLabel sc
viewConfigProperty (ViewStrokeDash xs) = "strokeDash" .= xs
viewConfigProperty (ViewStrokeDashOffset x) = "strokeDashOffset" .= x
viewConfigProperty (ViewStrokeJoin sj) = "strokeJoin" .= strokeJoinLabel sj
viewConfigProperty (ViewStrokeMiterLimit x) = "strokeMiterLimit" .= x
viewConfigProperty (ViewStrokeOpacity x) = "strokeOpacity" .= x
viewConfigProperty (ViewStrokeWidth x) = "strokeWidth" .= x


{-|

Axis configuration options for customising all axes. See the
<https://vega.github.io/vega-lite/docs/axis.html#general-config Vega-Lite documentation>
for more details.

The @TitleMaxLength@ constructor was removed in release @0.4.0.0@. The
@TitleLimit@ constructor should be used instead.

-}
data AxisConfig
    = BandPosition Double
      -- ^ The default axis band position.
    | Domain Bool
      -- ^ Should the axis domain be displayed?
    | DomainColor Color
      -- ^ The axis domain color.
    | DomainDash [Double]
      -- ^ The dash style of the domain (alternating stroke, space lengths
      --   in pixels).
      --
      --   @since 0.4.0.0
    | DomainDashOffset Double
      -- ^ The pixel offset at which to start drawing the domain dash array.
      --
      --   @since 0.4.0.0
    | DomainOpacity Opacity
      -- ^ The axis domain opacity.
      --
      --   @since 0.4.0.0
    | DomainWidth Double
      -- ^ The width of the axis domain.
    | Grid Bool
      -- ^ Should an axis grid be displayed?
    | GridColor Color
      -- ^ The color for the grid.
    | GridDash [Double]
      -- ^ The dash style of the grid (alternating stroke, space lengths
      --   in pixels).
    | GridDashOffset Double
      -- ^ The pixel offset at which to start drawing the grid dash array.
      --
      --   @since 0.4.0.0
    | GridOpacity Opacity
      -- ^ The opacity of the grid.
    | GridWidth Double
      -- ^ The width of the grid lines.
    | Labels Bool
      -- ^ Should labels be added to an axis?
    | LabelAlign HAlign
      -- ^ The horizontal alignment for labels.
      --
      --   @since 0.4.0.0
    | LabelAngle Angle
      -- ^ The angle at which to draw labels.
    | LabelBaseline VAlign
      -- ^ The vertical alignment for labels.
      --
      --   @since 0.4.0.0
    | LabelNoBound
      -- ^ No boundary overlap check is applied to labels. This is the
      --   default behavior.
      --
      --   See also 'LabelBound' and 'LabelBoundValue'.
      --
      --   @since 0.4.0.0
    | LabelBound
      -- ^ Labels are hidden if they exceed the axis range by more than 1
      --   pixel.
      --
      --   See also 'LabelNoBound' and 'LabelBoundValue'.
      --
      --   @since 0.4.0.0
    | LabelBoundValue Double
      -- ^ Labels are hidden if they exceed the axis range by more than
      --   the given number of pixels.
      --
      --   See also 'LabelNoBound' and 'LabelBound'.
      --
      --   @since 0.4.0.0
    | LabelColor Color
      -- ^ The label color.
    | LabelNoFlush
      -- ^ The labels are not aligned flush to the scale. This is the
      --   default for non-continuous X scales.
      --
      --   See also 'LabelFlush' and 'LabelFlushValue'.
      --
      --   @since 0.4.0.0
    | LabelFlush
      -- ^ The first and last axis labels are aligned flush to the scale
      --   range.
      --
      --   See also 'LabelNoFlush' and 'LabelFlushValue'.
      --
      --   @since 0.4.0.0
    | LabelFlushValue Double
      -- ^ The labels are aligned flush, and the parameter determines
      --   the extra offset, in pixels, to apply to the first and last
      --   labels. This can help the labels better group (visually) with
      --   the corresponding axis ticks.
      --
      --   See also 'LabelNoFlush' and 'LabelFlush'.
      --
      --   @since 0.4.0.0
    | LabelFlushOffset Double
      -- ^ The number of pixels to offset flush-adjusted labels.
      --
      --   @since 0.4.0.0
    | LabelFont T.Text
      -- ^ The font for the label.
    | LabelFontSize Double
      -- ^ The font size of the label.
    | LabelFontStyle T.Text
      -- ^ The font style of the label.
      --
      --   @since 0.4.0.0
    | LabelFontWeight FontWeight
      -- ^ The font weight of the label.
      --
      --   @since 0.4.0.0
    | LabelLimit Double
      -- ^ The maximum width of a label, in pixels.
    | LabelOpacity Opacity
      -- ^ The opacity of the label.
      --
      --   @since 0.4.0.0
    | LabelOverlap OverlapStrategy
      -- ^ How should overlapping labels be displayed?
    | LabelPadding Double
      -- ^ The padding, in pixels, between the label and the axis.
    | LabelSeparation Double
      -- ^ The minimum separation, in pixels, between label bounding boxes
      --   for them to be considered non-overlapping. This is ignored if
      --   the 'LabelOverlap' strategy is 'Graphics.Vega.VegaLite.ONone'.
      --
      --   @since 0.4.0.0
    | MaxExtent Double
      -- ^ The maximum extent, in pixels, that axis ticks and labels should use.
      --   This determines a maxmium offset value for axis titles.
    | MinExtent Double
      -- ^ The minimum extent, in pixels, that axis ticks and labels should use.
      --   This determines a minmium offset value for axis titles.
    | NoTitle
      -- ^ Do not draw a title for this axis.
      --
      --   @since 0.4.0.0
    | Orient Side
      -- ^ The orientation of the axis.
      --
      --   @since 0.4.0.0
    | ShortTimeLabels Bool
      -- ^ Should an axis use short time labels (abbreviated month and week-day names)?
    | Ticks Bool
      -- ^ Should tick marks be drawn on an axis?
    | TickColor Color
      -- ^ The color of the ticks.
    | TickDash [Double]
      -- ^ The dash style of the ticks (alternating stroke, space lengths
      --   in pixels).
    | TickDashOffset Double
      -- ^ The pixel offset at which to start drawing the tick dash array.
      --
      --   @since 0.4.0.0
    | TickExtra Bool
      -- ^ Should an extra axis tick mark be added for the initial position of
      --   the axis?
      --
      --   @since 0.4.0.0
    | TickOffset Double
      -- ^ The position offset, in pixels, to apply to ticks, labels, and grid lines.
      --
      --   @since 0.4.0.0
    | TickOpacity Opacity
      -- ^ The opacity of the ticks.
      --
      --   @since 0.4.0.0
    | TickRound Bool
      -- ^ Should pixel position values be rounded to the nearest integer?
    | TickSize Double
      -- ^ The size of the tick marks in pixels.
    | TickWidth Double
      -- ^ The width of the tick marks in pixels.
    | TitleAlign HAlign
      -- ^ The horizontal alignment of the axis title.
    | TitleAnchor APosition
      -- ^ The text anchor position for placing axis titles.
      --
      --   @since 0.4.0.0
    | TitleAngle Angle
      -- ^ The angle of the axis title.
    | TitleBaseline VAlign
      -- ^ The vertical alignment of the axis title.
    | TitleColor Color
      -- ^ The color of the axis title.
    | TitleFont T.Text
      -- ^ The font for the axis title.
    | TitleFontSize Double
      -- ^ The font size of the axis title.
    | TitleFontStyle T.Text
      -- ^ The font style of the axis title.
      --
      --   @since 0.4.0.0
    | TitleFontWeight FontWeight
      -- ^ The font weight of the axis title.
    | TitleLimit Double
      -- ^ The maximum allowed width of the axis title, in pixels.
    | TitleOpacity Opacity
      -- ^ The opacity of the axis title.
      --
      --   @since 0.4.0.0
    | TitlePadding Double
      -- ^ The padding, in pixels, between title and axis.
    | TitleX Double
      -- ^ The X coordinate of the axis title, relative to the axis group.
    | TitleY Double
      -- ^ The Y coordinate of the axis title, relative to the axis group.


axisConfigProperty :: AxisConfig -> LabelledSpec
axisConfigProperty (BandPosition x) = "bandPosition" .= x
axisConfigProperty (Domain b) = "domain" .= b
axisConfigProperty (DomainColor c) = "domainColor" .= c
axisConfigProperty (DomainDash ds) = "domainDash" .= ds
axisConfigProperty (DomainDashOffset x) = "domainDashOffset" .= x
axisConfigProperty (DomainOpacity x) = "domainOpacity" .= x
axisConfigProperty (DomainWidth w) = "domainWidth" .= w
axisConfigProperty (Grid b) = "grid" .= b
axisConfigProperty (GridColor c) = "gridColor" .= c
axisConfigProperty (GridDash ds) = "gridDash" .= ds
axisConfigProperty (GridDashOffset x) = "gridDashOffset" .= x
axisConfigProperty (GridOpacity o) = "gridOpacity" .= o
axisConfigProperty (GridWidth x) = "gridWidth" .= x
axisConfigProperty (Labels b) = "labels" .= b
axisConfigProperty (LabelAlign ha) = "labelAlign" .= hAlignLabel ha
axisConfigProperty (LabelAngle angle) = "labelAngle" .= angle
axisConfigProperty (LabelBaseline va) = "labelBaseline" .= vAlignLabel va
axisConfigProperty LabelNoBound = "labelBound" .= False
axisConfigProperty LabelBound = "labelBound" .= True
axisConfigProperty (LabelBoundValue x) = "labelBound" .= x
axisConfigProperty LabelNoFlush = "labelFlush" .= False
axisConfigProperty LabelFlush = "labelFlush" .= True
axisConfigProperty (LabelFlushValue x) = "labelFlush" .= x
axisConfigProperty (LabelFlushOffset x) = "labelFlushOffset" .= x
axisConfigProperty (LabelColor c) = "labelColor" .= c
axisConfigProperty (LabelFont f) = "labelFont" .= f
axisConfigProperty (LabelFontSize x) = "labelFontSize" .= x
axisConfigProperty (LabelFontStyle s) = "labelFontStyle" .= s
axisConfigProperty (LabelFontWeight fw) = "labelFontWeight" .= fontWeightSpec fw
axisConfigProperty (LabelLimit x) = "labelLimit" .= x
axisConfigProperty (LabelOpacity x) = "labelOpacity" .= x
axisConfigProperty (LabelOverlap strat) = "labelOverlap" .= overlapStrategyLabel strat
axisConfigProperty (LabelPadding pad) = "labelPadding" .= pad
axisConfigProperty (LabelSeparation x) = "labelSeparation" .= x
axisConfigProperty (MaxExtent n) = "maxExtent" .= n
axisConfigProperty (MinExtent n) = "minExtent" .= n
axisConfigProperty NoTitle = "title" .= A.Null
axisConfigProperty (Orient orient) = "orient" .= sideLabel orient
axisConfigProperty (ShortTimeLabels b) = "shortTimeLabels" .= b
axisConfigProperty (Ticks b) = "ticks" .= b
axisConfigProperty (TickColor c) = "tickColor" .= c
axisConfigProperty (TickDash ds) = "tickDash" .= ds
axisConfigProperty (TickDashOffset x) = "tickDashOffset" .= x
axisConfigProperty (TickExtra b) = "tickExtra" .= b
axisConfigProperty (TickOffset x) = "tickOffset" .= x
axisConfigProperty (TickOpacity x) = "tickOpacity" .= x
axisConfigProperty (TickRound b) = "tickRound" .= b
axisConfigProperty (TickSize x) = "tickSize" .= x
axisConfigProperty (TickWidth x) = "tickWidth" .= x
axisConfigProperty (TitleAlign algn) = "titleAlign" .= hAlignLabel algn
axisConfigProperty (TitleAnchor a) = "titleAnchor" .= anchorLabel a
axisConfigProperty (TitleAngle x) = "titleAngle" .= x
axisConfigProperty (TitleBaseline va) = "titleBaseline" .= vAlignLabel va
axisConfigProperty (TitleColor c) = "titleColor" .= c
axisConfigProperty (TitleFont f) = "titleFont" .= f
axisConfigProperty (TitleFontSize x) = "titleFontSize" .= x
axisConfigProperty (TitleFontStyle s) = "titleFontStyle" .= s
axisConfigProperty (TitleFontWeight w) = "titleFontWeight" .= fontWeightSpec w
axisConfigProperty (TitleLimit x) = "titleLimit" .= x
axisConfigProperty (TitleOpacity x) = "titleOpacity" .= x
axisConfigProperty (TitlePadding x) = "titlePadding" .= x
axisConfigProperty (TitleX x) = "titleX" .= x
axisConfigProperty (TitleY y) = "titleY" .= y


{-|

Configuration options for faceted views, used with 'Graphics.Vega.VegaLite.FacetStyle'.

See the
<https://vega.github.io/vega-lite/docs/facet.html#facet-configuration Vega-Lite facet config documentation>.

@since 0.4.0.0

-}
data FacetConfig
    = FColumns Int
    -- ^ The maximum number of columns to use in a faceted-flow layout.
    | FSpacing Double
    -- ^ The spacing in pixels between sub-views in a faceted composition.


facetConfigProperty :: FacetConfig -> LabelledSpec
facetConfigProperty (FColumns n) = "columns" .= n
facetConfigProperty (FSpacing x) = "spacing" .= x


{-|

Configuration options for concatenated views, used with 'Graphics.Vega.VegaLite.ConcatStyle'.

@since 0.4.0.0

-}
data ConcatConfig
    = ConcatColumns Int
      -- ^ The maximum number of columns to use in a concatenated flow layout.
    | ConcatSpacing Double
      -- ^ The spacing in pixels between sub-views in a concatenated view.


concatConfigProperty :: ConcatConfig -> LabelledSpec
concatConfigProperty (ConcatColumns n) = "columns" .= n
concatConfigProperty (ConcatSpacing x) = "spacing" .= x


{-|

Defines a single configuration option to be applied globally across the visualization.
The first parameter identifies the type of configuration, the second a list of previous
configurations to which this one may be added.

@
'configuration' ('Axis' [ 'DomainWidth' 4 ]) []
@
-}
configuration :: ConfigurationProperty -> BuildLabelledSpecs
configuration cfg ols = configProperty cfg : ols
