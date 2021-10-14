{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Configuration
Copyright   : (c) Douglas Burke, 2018-2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

Top-level configuration options. As this can configure most
of a visualization, it needs to import most of the other
modules.

-}

module Graphics.Vega.VegaLite.Configuration
       ( ConfigurationProperty(..)
       , FieldTitleProperty(..)

       , ViewConfig(..)
       , CompositionConfig(..)
       , ScaleConfig(..)
       , RangeConfig(..)
       , AxisConfig(..)
       , AxisChoice(..)
       , LegendConfig(..)
       , TitleConfig(..)

       , TitleFrame(..)

       , configuration
       , title

       ) where


import qualified Data.Aeson as A

import qualified Data.Text as T

import Data.Aeson ((.=), object)
import Data.Aeson.Types (Pair)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Graphics.Vega.VegaLite.Core
  ( AxisProperty
  , axisProperty
  , schemeProperty
  )
import Graphics.Vega.VegaLite.Foundation
  ( Angle
  , Color
  , StyleLabel
  , CompositionAlignment
  , DashStyle
  , DashOffset
  , APosition
  , FontWeight
  , Opacity
  , Orientation
  , OverlapStrategy
  , Side
  , StrokeCap
  , StrokeJoin
  , Symbol
  , HAlign
  , VAlign
  , BandAlign
  , Padding
  , Autosize
  , ZIndex
  , HeaderProperty
  , ViewBackground
  , Cursor
  , fromT
  , fromColor
  , fromDS
  , splitOnNewline
  , header_
  , anchorLabel
  , fontWeightSpec
  , orientationSpec
  , hAlignLabel
  , vAlignLabel
  , bandAlignLabel
  , strokeCapLabel
  , strokeJoinLabel
  , sideLabel
  , overlapStrategyLabel
  , symbolLabel
  , compositionAlignmentSpec
  , paddingSpec
  , autosizeProperty
  , viewBackgroundSpec
  , cursorLabel
  , (.=~), toObject
  )
import Graphics.Vega.VegaLite.Geometry
  ( ProjectionProperty
  , projectionProperty
  )
import Graphics.Vega.VegaLite.Legend
  ( LegendLayout
  , LegendOrientation
  , legendOrientLabel
  , legendLayoutSpec
  )
import Graphics.Vega.VegaLite.Mark
  ( MarkProperty
  , oldMprops_
  )
import Graphics.Vega.VegaLite.Scale
  ( ScaleNice
  , scaleNiceSpec
  )
import Graphics.Vega.VegaLite.Selection
  ( Selection
  , SelectionProperty
  , selectionProperties
  , selectionLabel
  )
import Graphics.Vega.VegaLite.Specification
  ( VLSpec
  , VLProperty(VLTitle)
  , ConfigureSpec(..)
  , BuildConfigureSpecs
  , LabelledSpec
  , PropertySpec
  )


{-|

Type of configuration property to customise. See the
<https://vega.github.io/vega-lite/docs/config.html Vega-Lite documentation>
for details. There are multiple ways to configure the properties
of an axis, as discussed in the Vega-Lite
<https://vega.github.io/vega-lite/docs/axis.html#config axis configuration>
documentation.

Used by 'configuration'.

In @version 0.7.0.0@, the 'AxisBand' , 'AxisDiscrete', 'AxisPoint',
'AxisQuantitative', and 'AxisTemporal' were changed to accept an
additional argument ('AxisChoice'), to define which axis the configuration
should be applied to.

In @version 0.6.0.0@:

- the @Autosize@, @Background@, @CountTitle@, @FieldTitle@, @Legend@,
  @NumberFormat@, @Padding@, @Projection@, @Range@, @Scale@.
  @TimeFormat@, and @View@
  constructors have been deprecated, and should be replaced by
  'AutosizeStyle', 'BackgroundStyle', 'CountTitleStyle', 'FieldTitleStyle',
  'LegendStyle', 'NumberFormatStyle', 'PaddingStyle', 'ProjectionStyle',
  'RangeStyle', 'ScaleStyle', 'TimeFormatStyle', and 'ViewStyle'
  respectively. The axis configuration options have not been updated
  to this system.

- new constructors have been added: 'AxisDiscrete', 'AxisPoint',
  'AxisQuantitative', 'AxisTemporal', 'BoxplotStyle', 'ErrorBandStyle',
  'ErrorBarStyle', 'HeaderColumnStyle', 'HeaderFacetStyle', 'HeaderRowStyle',
  'ImageStyle', and 'RepeatStyle'.

- 'ConcatStyle' and 'FacetStyle' now take a common type, 'CompositionConfig',
  rather than @ConcatConfig@ and @FacetStyle@.

In @version 0.5.0.0@:

- the @RemoveInvalid@ constructor was removed, as
the new 'Graphics.Vega.VegaLite.MRemoveInvalid' constructor for the
'MarkProperty' type should be used instead
(so @'configuration' (RemoveInvalid b)@ changes to
@'configuration' ('Graphics.Vega.VegaLite.MarkStyle' ['Graphics.Vega.VegaLite.MRemoveInvalid' b])@.

- the @Stack@ constructor (which was called @StackProperty@ prior
  to version @0.4.0.0@) was removed.

-}

{-# DEPRECATED Autosize "Please change Autosize to AutosizeStyle" #-}
{-# DEPRECATED Background "Please change Background to BackgroundStyle" #-}
{-# DEPRECATED CountTitle "Please change CountTitle to CountTitleStyle" #-}
{-# DEPRECATED FieldTitle "Please change FieldTitle to FieldTitleStyle" #-}
{-# DEPRECATED Legend "Please change Legend to LegendStyle" #-}
{-# DEPRECATED NumberFormat "Please change NumberFormat to NumberFormatStyle" #-}
{-# DEPRECATED Padding "Please change Padding to PaddingStyle" #-}
{-# DEPRECATED Projection "Please change Projection to ProjectionStyle" #-}
{-# DEPRECATED Range "Please change Range to RangeStyle" #-}
{-# DEPRECATED Scale "Please change Scale to ScaleStyle" #-}
{-# DEPRECATED TimeFormat "Please change TimeFormat to TimeFormatStyle" #-}
{-# DEPRECATED View "Please change View to ViewStyle" #-}

{-# DEPRECATED NamedStyle "Please change Legend to MarkNamedStyles" #-}
{-# DEPRECATED NamedStyles "Please change Legend to MarkNamedStyles" #-}

data ConfigurationProperty
    = ArcStyle [MarkProperty]
      -- ^ The default appearance of arc marks.
      --
      --   @since 0.9.0.0
    | AreaStyle [MarkProperty]
      -- ^ The default appearance of area marks.
    | AriaStyle Bool
      -- ^ A boolean flag indicating if ARIA default attributes should be included for
      --   marks and guides (SVG output only). If False, the \"aria-hidden\"
      --   attribute will be set for all guides, removing them from the ARIA accessibility
      --   tree and Vega-Lite will not generate default descriptions for marks.
      --
      --   __Default value:__ True
      --
      --   @since 0.9.0.0
    | AutosizeStyle [Autosize]
      -- ^ The default sizing of visualizations.
      --
      --   This was renamed from @Autosize@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | Axis [AxisConfig]
      -- ^ The default appearance of axes.
    | AxisBand AxisChoice [AxisConfig]
      -- ^ The default appearance of axes with band scaling.
      --
      --   See also 'AxisDiscrete'.
    | AxisBottom [AxisConfig]
      -- ^ The default appearance of the bottom-side axes.
    | AxisDiscrete AxisChoice [AxisConfig]
      -- ^ The default appearance of axes with point or band scales.
      --
      --   See also 'AxisBand' and 'AxisPoint'.
      --
      --   @since 0.6.0.0
    | AxisLeft [AxisConfig]
      -- ^ The default appearance of the left-side axes.
    | AxisPoint AxisChoice [AxisConfig]
      -- ^ The default appearance of axes with point scales.
      --
      --   See also 'AxisDiscrete'.
      --
      --   @since 0.6.0.0
    | AxisQuantitative AxisChoice [AxisConfig]
      -- ^ The default appearance of quantitative axes.
      --
      --   @since 0.6.0.0
    | AxisRight [AxisConfig]
      -- ^ The default appearance of the right-side axes.
    | AxisTemporal AxisChoice [AxisConfig]
      -- ^ The default appearance of temporal axes.
      --
      --   @since 0.6.0.0
    | AxisTop [AxisConfig]
      -- ^ The default appearance of the top-side axes.
    | AxisX [AxisConfig]
      -- ^ The default appearance of the X axes.
    | AxisY [AxisConfig]
      -- ^ The default appearance of the Y axes.
    | AxisNamedStyles [(StyleLabel, [AxisProperty])]
      -- ^  Assign a set of axis styles to a label. These labels can then be referred
      --    to when configuring an axis with 'Graphics.Vega.VegaLite.AxStyle' and
      --    'AStyle'.
      --
      --   To customize the style for guides (axes, headers, and legends), Vega-Lite
      --   includes the following built-in style names:
      --
      --    - \"guide-label\": style for axis, legend, and header labels
      --    - \"guide-title\": style for axis, legend, and header titles
      --    - \"group-label\": styles for chart titles
      --    - \"group-subtitle\"
      --
      --   @since 0.6.0.0
    | BackgroundStyle Color
      -- ^ The default background color of visualizations.
      --
      --   This was changed to use the @Color@ type alias in version @0.5.0.0@.
      --
      --   This was renamed from @Background@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | BarStyle [MarkProperty]
      -- ^ The default appearance of bar marks.
    | BoxplotStyle [MarkProperty]
      -- ^ The default appearance for box plots.
      --
      --   @since 0.6.0.0
    | CircleStyle [MarkProperty]
      -- ^ The default appearance of circle marks.
    | ConcatStyle [CompositionConfig]
      -- ^ The default appearance for all concatenation and repeat view
      --   composition operators ('Graphics.Vega.VegaLite.vlConcat',
      --   'Graphics.Vega.VegaLite.hConcat', 'Graphics.Vega.VegaLite.vConcat',
      --   and 'Graphics.Vega.VegaLite.repeat`).
      --
      --   In @0.6.0.0@ this was changed from accepting @ConcatConfig@ to
      --   'CompositionConfig'.
      --
      --   Vega-Lite 4.8 changed this field to also control repeat-view
      --   operators (which previously had used @RepeatStyle@).
      --
      --   @since 0.4.0.0
    | CountTitleStyle T.Text
      -- ^ The default axis and legend title for count fields. The default is
      --   @"Count of Records"@.
      --
      --   This was renamed from @CountTitle@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | CustomFormatStyle Bool
      -- ^ Allow the \"formatType\" property for text marks and guides to accept a custom
      --   formatter function registered as a
      --   [Vega Expression](https://vega.github.io/vega-lite/docs/compile.html#format-type).
      --
      --   @since 0.9.0.0
    | ErrorBandStyle [MarkProperty]
      -- ^ The default appearance for error bands.
      --
      --   @since 0.6.0.0
    | ErrorBarStyle [MarkProperty]
      -- ^ The default appearance for error bars.
      --
      --   @since 0.6.0.0
    | FacetStyle [CompositionConfig]
      -- ^ The default appearance of facet layouts.
      --
      --   In @0.6.0.0@ this was changed from accepting @FacetConfig@ to
      --   'CompositionConfig'.
      --
      --   @since 0.4.0.0
    | FieldTitleStyle FieldTitleProperty
      -- ^ The default title-generation style for fields.
      --
      --   This was renamed from @FieldTitle@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | FontStyle T.Text
      -- ^ The default font for all text marks, titles, and labels.
      --
      --   The naming scheme used here is somewhat unfortunate, as this
      --   is for the name of the font (such as @\"serif\"@ or
      --   @\"Comic Sans MS\"@), not the font-style.
      --
      --   @since 0.6.0.0
    | GeoshapeStyle [MarkProperty]
      -- ^ The default appearance of geoshape marks.
      --
      --   @since 0.4.0.0
    | HeaderStyle [HeaderProperty]
      -- ^ The default appearance of all headers.
      --
      --   @since 0.4.0.0
    | HeaderColumnStyle [HeaderProperty]
      -- ^ The default appearance for column headers.
      --
      --   @since 0.6.0.0
    | HeaderFacetStyle [HeaderProperty]
      -- ^ The default appearance for non-row and non-column facet headers.
      --
      --   @since 0.6.0.0
    | HeaderRowStyle [HeaderProperty]
      -- ^ The default appearance for row headers.
      --
      --   @since 0.6.0.0
    | ImageStyle [MarkProperty]
      -- ^ The default appearance for images.
      --
      --   @since 0.6.0.0
    | LegendStyle [LegendConfig]
      -- ^ The default appearance of legends.
      --
      --   This was renamed from @Legend@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | LineStyle [MarkProperty]
      -- ^ The default appearance of line marks.
    | LineBreakStyle T.Text
      -- ^ The delimiter, such as a newline character, upon which to break text
      --   strings into multiple lines. This can be over-ridden by mark or style configuration
      --   settings.
      --
      --   Added in Vega-Lite 4.6.0.
      --
      --   @since 0.7.0.0
    | MarkStyle [MarkProperty]
      -- ^ The default mark appearance.
    | MarkNamedStyles [(StyleLabel, [MarkProperty])]
      -- ^  Assign a set of mark styles to a label. These labels can then be referred
      --    to when configuring a mark, such as with 'TStyle'.
      --
      --   @since 0.6.0.0
    | NumberFormatStyle T.Text
      -- ^ The default number formatting for axis and text labels, using
      --   [D3's number format pattern](https://github.com/d3/d3-format#locale_format).
      --
      --   As an example @NumberFormatStyle "s"@ will use SI units.
      --
      --   This was renamed from @NumberFormat@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | PaddingStyle Padding
      -- ^ The default padding in pixels from the edge of the of visualization
      --   to the data rectangle.
      --
      --   This was renamed from @Padding@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | PointStyle [MarkProperty]
      -- ^ The default appearance of point marks.
    | ProjectionStyle [ProjectionProperty]
      -- ^ The default style of map projections.
      --
      --   This was renamed from @Projection@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | RangeStyle [RangeConfig]
      -- ^ The default range properties used when scaling.
      --
      --   This was renamed from @Range@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | RectStyle [MarkProperty]
      -- ^ The default appearance of rectangle marks.
    | RepeatStyle [CompositionConfig]  -- TODO: remove
      -- ^ The default appearance for the 'Graphics.Vega.VegaLite.repeat` operator.
      --
      --   Support for this setting was removed in Vega-Lite 4.8. This
      --   constructor is currently still supported, but will be removed
      --   in a future release. The 'ConcatStyle' option should be
      --   used instead.
      --
      --   @since 0.6.0.0
    | RuleStyle [MarkProperty]
      -- ^ The default appearance of rule marks.
    | ScaleStyle [ScaleConfig]
      -- ^ The default properties used when scaling.
      --
      --   This was renamed from @Scale@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | SelectionStyle [(Selection, [SelectionProperty])]
      -- ^ The default appearance of selection marks.
    | SquareStyle [MarkProperty]
      -- ^  the default appearance of square marks
    | TextStyle [MarkProperty]
      -- ^ The default appearance of text marks.
    | TickStyle [MarkProperty]
      -- ^ The default appearance of tick marks.
    | TimeFormatStyle T.Text
      -- ^ The default time format for raw time values (without time units)
      --   in text marks, legend labels, and header labels. This does /not/
      --   control the appearance of axis labels.
      --
      --   The default is @\"%b %d, %Y\"@.
      --
      --   This was renamed from @TimeFormat@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | TitleStyle [TitleConfig]
      -- ^ The default appearance of visualization titles.
    | TrailStyle [MarkProperty]
      -- ^ The default style of trail marks.
      --
      --   @since 0.4.0.0
    | ViewStyle [ViewConfig]
      -- ^ The default properties for
      --   [single view plots](https://vega.github.io/vega-lite/docs/spec.html#single).
      --
      --   This was renamed from @View@ in @0.6.0.0@.
      --
      --   @since 0.6.0.0
    | Autosize [Autosize]
      -- ^ As of version @0.6.0.0@ this is deprecated and 'AutosizeStyle' should be used
      --   instead.
    | Background Color
      -- ^ As of version @0.6.0.0@ this is deprecated and 'BackgroundStyle' should be used
      --   instead.
    | CountTitle T.Text
      -- ^ As of version @0.6.0.0@ this is deprecated and 'CountTitleStyle' should be used
      --   instead.
    | FieldTitle FieldTitleProperty
      -- ^ As of version @0.6.0.0@ this is deprecated and 'FieldTitleStyle' should be used
      --   instead.
    | Legend [LegendConfig]
      -- ^ As of version @0.6.0.0@ this is deprecated and 'LegendStyle' should be used
      --   instead.
    | NumberFormat T.Text
      -- ^ As of version @0.6.0.0@ this is deprecated and 'NumberFormatStyle' should be used
      --   instead.
    | Padding Padding
      -- ^ As of version @0.6.0.0@ this is deprecated and 'PaddingStyle' should be used
      --   instead.
    | Projection [ProjectionProperty]
      -- ^ As of version @0.6.0.0@ this is deprecated and 'ProjectionStyle' should be used
      --   instead.
    | Range [RangeConfig]
      -- ^ As of version @0.6.0.0@ this is deprecated and 'RangeStyle' should be used
      --   instead.
    | Scale [ScaleConfig]
      -- ^ As of version @0.6.0.0@ this is deprecated and 'ScaleStyle' should be used
      --   instead.
    | TimeFormat T.Text
      -- ^ As of version @0.6.0.0@ this is deprecated and 'TimeFormatStyle' should be used
      --   instead.
    | View [ViewConfig]
      -- ^ As of version @0.6.0.0@ this is deprecated and 'ViewStyle' should be used
      --   instead.
    | NamedStyle StyleLabel [MarkProperty]
      -- ^ As of version @0.6.0.0@ this is deprecated and 'MarkNamedStyles' should be
      --   used instead.
    | NamedStyles [(StyleLabel, [MarkProperty])]
      -- ^ As of version @0.6.0.0@ this is deprecated and 'MarkNamedStyles' should be
      --   used instead.


-- | Which axis should the configuration be applied to?
--
--   Added in Vega-Lite 4.7.0.
--
--   @since 0.7.0.0
data AxisChoice
  = AxXY
    -- ^ Apply the configuration to both axes.
    --
    --   This was the default behavior prior to @0.7.0.0@.
  | AxX
    -- ^ Select the X axis.
  | AxY
    -- ^ Select the Y axis.


toAxis :: T.Text -> [AxisConfig] -> LabelledSpec
toAxis lbl acs = ("axis" <> lbl) .=~ object (map axisConfigProperty acs)

toAxisChoice :: AxisChoice -> T.Text -> [AxisConfig] -> LabelledSpec
toAxisChoice AxXY lbl = toAxis lbl
toAxisChoice AxX lbl = toAxis ("X" <> lbl)
toAxisChoice AxY lbl = toAxis ("Y" <> lbl)

aprops_ :: T.Text -> [AxisProperty] -> LabelledSpec
aprops_ f mps = f .=~ object (map axisProperty mps)

-- easier to turn into a ConfigSpec in config than here
configProperty :: ConfigurationProperty -> LabelledSpec
configProperty (ArcStyle mps) = oldMprops_ "arc" mps
configProperty (AreaStyle mps) = oldMprops_ "area" mps
configProperty (AriaStyle b) = "aria" .=~ b
configProperty (AutosizeStyle aus) = "autosize" .=~ object (map autosizeProperty aus)
configProperty (Axis acs) = toAxis "" acs
configProperty (AxisBand c acs) = toAxisChoice c "Band" acs
configProperty (AxisBottom acs) = toAxis "Bottom" acs
configProperty (AxisDiscrete c acs) = toAxisChoice c "Discrete" acs
configProperty (AxisLeft acs) = toAxis "Left" acs
configProperty (AxisPoint c acs) = toAxisChoice c "Point" acs
configProperty (AxisQuantitative c acs) = toAxisChoice c "Quantitative" acs
configProperty (AxisRight acs) = toAxis "Right" acs
configProperty (AxisTemporal c acs) = toAxisChoice c "Temporal" acs
configProperty (AxisTop acs) = toAxis "Top" acs
configProperty (AxisX acs) = toAxis "X" acs
configProperty (AxisY acs) = toAxis "Y" acs

-- configProperty (AxisNamedStyles [(nme, mps)]) = "style" .=~ object [aprops_ nme mps]
configProperty (AxisNamedStyles styles) =
  let toStyle = uncurry aprops_
  in "style" .=~ toObject (map toStyle styles)

configProperty (BackgroundStyle bg) = "background" .=~ bg
configProperty (BarStyle mps) = oldMprops_ "bar" mps
configProperty (BoxplotStyle mps) = oldMprops_ "boxplot" mps
configProperty (CircleStyle mps) = oldMprops_ "circle" mps
configProperty (ConcatStyle cps) = "concat" .=~ object (map compConfigProperty cps)
configProperty (CountTitleStyle ttl) = "countTitle" .=~ ttl
configProperty (CustomFormatStyle b) = "customFormatTypes" .=~ b
configProperty (ErrorBandStyle mps) = oldMprops_ "errorband" mps
configProperty (ErrorBarStyle mps) = oldMprops_ "errorbar" mps
configProperty (FacetStyle cps) = "facet" .=~ object (map compConfigProperty cps)
configProperty (FieldTitleStyle ftp) = "fieldTitle" .=~ fieldTitleLabel ftp
configProperty (FontStyle font) = "font" .=~ font
configProperty (GeoshapeStyle mps) = oldMprops_ "geoshape" mps
configProperty (HeaderStyle hps) = header_ "" hps
configProperty (HeaderColumnStyle hps) = header_ "Column" hps
configProperty (HeaderFacetStyle hps) = header_ "Facet" hps
configProperty (HeaderRowStyle hps) = header_ "Row" hps
configProperty (ImageStyle mps) = oldMprops_ "image" mps
configProperty (LegendStyle lcs) = "legend" .=~ object (map legendConfigProperty lcs)
configProperty (LineStyle mps) = oldMprops_ "line" mps

configProperty (LineBreakStyle s) = "lineBreak" .=~ s

configProperty (MarkStyle mps) = oldMprops_ "mark" mps
-- configProperty (MarkNamedStyles [(nme, mps)]) = "style" .=~ object [mprops_ nme mps]
configProperty (MarkNamedStyles styles) =
  let toStyle = uncurry oldMprops_
  in "style" .=~ toObject (map toStyle styles)

configProperty (NumberFormatStyle fmt) = "numberFormat" .=~ fmt
configProperty (PaddingStyle pad) = "padding" .=~ paddingSpec pad
configProperty (PointStyle mps) = oldMprops_ "point" mps
configProperty (ProjectionStyle pps) = "projection" .=~ object (map projectionProperty pps)
configProperty (RangeStyle rcs) = "range" .=~ object (map rangeConfigProperty rcs)
configProperty (RectStyle mps) = oldMprops_ "rect" mps
configProperty (RepeatStyle cps) = "repeat" .=~ object (map compConfigProperty cps)
configProperty (RuleStyle mps) = oldMprops_ "rule" mps
configProperty (ScaleStyle scs) = scaleConfig_ scs
configProperty (SelectionStyle selConfig) =
  let selProp (sel, sps) = selectionLabel sel .=~ object (concatMap selectionProperties sps)
  in "selection" .=~ toObject (map selProp selConfig)
configProperty (SquareStyle mps) = oldMprops_ "square" mps
configProperty (TextStyle mps) = oldMprops_ "text" mps
configProperty (TickStyle mps) = oldMprops_ "tick" mps
configProperty (TimeFormatStyle fmt) = "timeFormat" .=~ fmt
configProperty (TitleStyle tcs) = "title" .=~ object (map titleConfigSpec tcs)
configProperty (TrailStyle mps) = oldMprops_ "trail" mps
configProperty (ViewStyle vcs) = "view" .=~ object (concatMap viewConfigProperties vcs)

-- deprecated aliases
configProperty (Autosize aus) = "autosize" .=~ object (map autosizeProperty aus)
configProperty (Background bg) = "background" .=~ bg
configProperty (CountTitle ttl) = "countTitle" .=~ ttl
configProperty (FieldTitle ftp) = "fieldTitle" .=~ fieldTitleLabel ftp
configProperty (Legend lcs) = "legend" .=~ object (map legendConfigProperty lcs)
configProperty (NumberFormat fmt) = "numberFormat" .=~ fmt
configProperty (Padding pad) = "padding" .=~ paddingSpec pad
configProperty (Projection pps) = "projection" .=~ object (map projectionProperty pps)
configProperty (Range rcs) = "range" .=~ object (map rangeConfigProperty rcs)
configProperty (Scale scs) = scaleConfig_ scs
configProperty (TimeFormat fmt) = "timeFormat" .=~ fmt
configProperty (View vcs) = "view" .=~ object (concatMap viewConfigProperties vcs)

configProperty (NamedStyle nme mps) = "style" .=~ toObject [oldMprops_ nme mps]
configProperty (NamedStyles styles) =
  let toStyle = uncurry oldMprops_
  in "style" .=~ toObject (map toStyle styles)

{-|

Scale configuration property. These are used to configure all scales
with 'ScaleStyle'. For more details see the
<https://vega.github.io/vega-lite/docs/scale.html#scale-config Vega-Lite documentation>.

Version @0.5.0.0@ removed the @SCRangeStep@ and @SCTextXRangeStep@
constructors. The new 'ViewStep' constructor of 'ViewConfig' should
be used instead.
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
    | SCRound Bool
      -- ^ Are numeric values are rounded to integers when scaling? Useful
      --   for snapping to the pixel grid.
    | SCUseUnaggregatedDomain Bool
      -- ^ Whether or not to use the source data range before aggregation.
    | SCXReverse Bool
      -- ^ Reverse the X scale (useful for right-to-left charts).
      --
      --   @since 0.6.0.0

scaleConfig_ :: [ScaleConfig] -> LabelledSpec
-- scaleConfig_ [] = "scale" .= A.Null  -- not sure here
scaleConfig_ scs = "scale" .=~ object (map scaleConfigProperty scs)


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


{-|

Legend configuration options, set with the 'LegendStyle' constructor.
For more detail see the
<https://vega.github.io/vega-lite/docs/legend.html#config Vega-Lite documentation>.

In @0.9.0.0@ the 'LeTickCountTime' constructor was added.

In @0.8.0.0@ the @LeTitle@ constructor was removed as there is no way
to set the default text for a legend title in Vega-Lite ('LeNoTitle'
remains as this is used to turn off legend titles).

In @0.6.0.0@ the following constructors were added (all from Vega-Lite 4.0):
'LeSymbolLimit', 'LeTickCount', 'LeTitleLineHeight', and
'LeUnselectedOpacity'.

In @0.5.0.0@ the @LeShortTimeLabels@ constructor was removed (Vega-Lite 4.0).

This data type has seen significant changes in the @0.4.0.0@ release:

- the @EntryPadding@, @GradientHeight@, @GradientLabelBaseline@, @GradientWidth@
  and @SymbolColor@ constructors were removed;

- the constructors were removed;

- the remaining constructors that did not begin with @Le@ were renamed (for
  example @Orient@ was changed to 'LeOrient');

- and new constructors were added.

-}

-- based on schema 3.3.0 #/definitions/LegendConfig

data LegendConfig
    = LeAria Bool
      -- ^ A boolean flag indicating if
      --   [ARIA attributes](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)
      --   should be included (SVG output only).
      --
      --   If False, the \"aria-hidden\" attribute will be set on the output SVG group,
      --   removing the legend from the ARIA accessibility tree.
      --
      --   __Default value:__ True
      --
      --   @since 0.9.0.0
    | LeAriaDescription T.Text
      -- ^ A text description of this legend for
      --   [ARIA accessibility](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)
      --   (SVG output only).
      --
      --   If the 'LeAria' property is true, for SVG output the
      --   [\"aria-label\" attribute](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_aria-label_attribute)
      --   will be set to this description.
      --
      --   If the description is unspecified it will be automatically generated.
      --
      --   @since 0.9.0.0
    | LeClipHeight Double
      -- ^ The height in pixels at which to clip symbol legend entries.
      --
      --   @since 0.4.0.0
    | LeColumnPadding Double
      -- ^ The horizontal padding, in pixels, between symbol legend entries.
      --
      --   @since 0.4.0.0
    | LeColumns Int
      -- ^ The number of columns in which to arrange symbol legend entries. A value
      --   of @0@ or lower indicates a single row with one column per entry.
      --
      --   @since 0.4.0.0
    | LeCornerRadius Double
      -- ^ The corner radius for the full legend.
    | LeDirection Orientation
      -- ^ The direction for the legend.
      --
      --   @since 0.8.0.0
    | LeDisable Bool
      -- ^ Disable the legend by default?
      --
      --   Added in Vega-Lite 4.8.
      --
      --   @since 0.8.0.0
    | LeFillColor Color
      -- ^ The background fill color for the full legend.
    | LeGradientDirection Orientation
      -- ^ The default direction for gradient legends.
      --
      --   @since 0.4.0.0
    | LeGradientHorizontalMaxLength Double
      -- ^ The maximum legend length for a horizontal gradient.
      --
      --   @since 0.4.0.0
    | LeGradientHorizontalMinLength Double
      -- ^ The minimum legend length for a horizontal gradient.
      --
      --   @since 0.4.0.0
    | LeGradientLabelLimit Double
      -- ^ The maximum allowed length, in pixels, of color-ramp gradient labels.
    | LeGradientLabelOffset Double
      -- ^ The vertical offset in pixels for color-ramp gradient labels.
    | LeGradientLength Double
      -- ^ The length in pixels of the primary axis of a color gradient.
      --   See also 'LeGradientThickness'.
      --
      --   @since 0.4.0.0
    | LeGradientOpacity Opacity
      -- ^ The opacity of the color gradient.
      --
      --   @since 0.4.0.0
    | LeGradientStrokeColor Color
      -- ^ The color of the gradient stroke.
    | LeGradientStrokeWidth Double
      -- ^ The width of the gradient stroke, in pixels.
    | LeGradientThickness Double
      -- ^ The thickness in pixels of the color gradient. See also 'LeGradientLength'.
      --
      --   @since 0.4.0.0
    | LeGradientVerticalMaxLength Double
      -- ^ The maximum legend length for a vertical gradient.
      --
      --   @since 0.4.0.0
    | LeGradientVerticalMinLength Double
      -- ^ The minimum legend length for a vertical gradient.
      --
      --   @since 0.4.0.0
    | LeGridAlign CompositionAlignment
      -- ^ The alignment to apply to symbol legends rows and columns.
      --
      --    @since 0.4.0.0
    | LeLabelAlign HAlign
      -- ^ The alignment of the legend label.
    | LeLabelBaseline VAlign
      -- ^ The position of the baseline of the legend label.
    | LeLabelColor Color
      -- ^ The color of the legend label.
    | LeLabelFont T.Text
      -- ^ The font of the legend label.
    | LeLabelFontSize Double
      -- ^ The font of the legend label.
    | LeLabelFontStyle T.Text
      -- ^ The font style of the legend label.
      --
      --   @since 0.4.0.0
    | LeLabelFontWeight FontWeight
      -- ^ The font weight of the legend label.
      --
      --   @since 0.4.0.0
    | LeLabelLimit Double
      -- ^ The maxumum allowed pixel width of the legend label.
    | LeLabelOffset Double
      -- ^ The offset of the legend label.
    | LeLabelOpacity Opacity
      -- ^ The opacity of the legend label.
      --
      --   @since 0.4.0.0
    | LeLabelOverlap OverlapStrategy
      -- ^ How to resolve overlap of labels in gradient legends.
      --
      --   @since 0.4.0.0
    | LeLabelPadding Double
      -- ^ The passing in pixels between the legend and legend labels.
      --
      --   @since 0.4.0.0
    | LeLabelSeparation Double
      -- ^ The minimum separation between label bounding boxes for them
      --   to be considered non-overlapping (ignored if 'Graphics.Vega.VegaLite.ONone' is the
      --   chosen overlap strategy).
      --
      --   @since 0.4.0.0
    | LeLayout [LegendLayout]  -- TODO: schema for this is odd; check it is meaningful
      -- ^ Layout parameters for the legend orient group.
      --
      --   It is not clear if this is used in Vega Lite 4.2 or later.
      --
      --   @since 0.4.0.0
     | LeLeX Double
      -- ^ Custom x position for a legend with orientation 'Graphics.Vega.VegaLite.LONone'.
      --
      --   @since 0.4.0.0
     | LeLeY Double
      -- ^ Custom y position for a legend with orientation 'Graphics.Vega.VegaLite.LONone'.
      --
      --   @since 0.4.0.0
    | LeOffset Double
      -- ^ The offset in pixels between the legend and the data rectangle
      --   and axes.
    | LeOrient LegendOrientation
      -- ^ The orientation of the legend, which determines how the legend is positioned
      --   within the scene.
    | LePadding Double
      -- ^ The padding between the border and content of the legend group.
    | LeRowPadding Double
      -- ^ The vertical padding in pixels between symbol legend entries.
      --
      --   @since 0.4.0.0
    | LeStrokeColor Color
      -- ^ The border stoke color for the full legend.
    | LeStrokeDash DashStyle
      -- ^ The border stroke dash pattern for the full legend.
    | LeStrokeWidth Double
      -- ^ The border stroke width for the full legend.
    | LeSymbolBaseFillColor Color
      -- ^ The fill color for legend symbols. This is only applied if
      --   there is no \"fill\" scale color encoding for the legend.
      --
      --   @since 0.4.0.0
    | LeSymbolBaseStrokeColor Color
      -- ^ The stroke color for legend symbols. This is only applied if
      --   there is no \"fill\" scale color encoding for the legend.
      --
      --   @since 0.4.0.0
    | LeSymbolDash DashStyle
      -- ^ The pattern for dashed symbol strokes.
      --
      --   @since 0.4.0.0
    | LeSymbolDashOffset DashOffset
      -- ^ The offset at which to start drawing the symbol dash pattern.
      --
      --   @since 0.4.0.0
    | LeSymbolDirection Orientation
      -- ^ The default direction for symbol legends.
      --
      --   @since 0.4.0.0
    | LeSymbolFillColor Color
      -- ^ The color of the legend symbol.
      --
      --   @since 0.4.0.0
    | LeSymbolLimit Int  -- it may be that negative entries allow you to say "drop last 2"
      -- ^ The maximum number of allowed entries for a symbol legend. Any additional entries
      --   will be dropped.
      --
      --   @since 0.6.0.0
    | LeSymbolOffset Double
      -- ^ The horizontal pixel offset for legend symbols.
      --
      --   @since 0.4.0.0
    | LeSymbolOpacity Opacity
      -- ^ The opacity of the legend symbols.
      --
      --   @since 0.4.0.0
    | LeSymbolSize Double
      -- ^ The size of the legend symbol, in pixels.
    | LeSymbolStrokeColor Color
      -- ^ The stroke color for legend symbols.
      --
      --   @since 0.4.0.0
    | LeSymbolStrokeWidth Double
      -- ^ The width of the symbol's stroke.
    | LeSymbolType Symbol
      -- ^ The default shape type for legend symbols.
    | LeTickCount Int
      -- ^ The desired number of tick values for quantitative legends.
      --
      --   The 'LeTickCountTime' option can instead be used for \"time\"
      --   or \"utc\" scales.
      --
      --   @since 0.6.0.0
    | LeTickCountTime ScaleNice
      -- ^ A specialised version of 'LeTickCount' for \"time\" and \"utc\"
      --   time scales.
      --
      --   The 'Graphics.Vega.VegaLite.IsNice' and 'Graphics.Vega.VegaLte.NTickCount'
      --   options should not be used as they generate invalid VegaLite.
      --
      --   @since 0.9.0.0
    | LeNoTitle
      -- ^ Do not add a title for the legend.
      --
      --   @since 0.4.0.0
    | LeTitleAlign HAlign
      -- ^ The horizontal text alignment for legend titles.
    | LeTitleAnchor APosition
      -- ^ The text anchor position for legend titles.
      --
      --   @since 0.4.0.0
    | LeTitleBaseline VAlign
      -- ^ The vertical text alignment for legend titles.
    | LeTitleColor Color
      -- ^ The color of the legend title.
    | LeTitleFont T.Text
      -- ^ The font of the legend title.
    | LeTitleFontSize Double
      -- ^ The font size of the legend title.
    | LeTitleFontStyle T.Text
      -- ^ The font style for the legend title.
      --
      --   @since 0.4.0.0
    | LeTitleFontWeight FontWeight
      -- ^ The font weight of the legend title.
    | LeTitleLimit Double
      -- ^ The maxmimum pixel width of the legend title.
    | LeTitleLineHeight Double
      -- ^ The line height, in pixels, for multi-line title text.
      --
      --   @since 0.6.0.0
    | LeTitleOpacity Opacity
      -- ^ The opacity of the legend title.
      --
      --   @since 0.4.0.0
    | LeTitleOrient Side
      -- ^ The orientation of the legend title.
      --
      --   @since 0.4.0.0
    | LeTitlePadding Double
      -- ^ The padding, in pixels, between title and legend.
    | LeUnselectedOpacity Opacity
      -- ^ The opacity of unselected legend entries.
      --
      --   The default is 0.35.
      --
      --   @since 0.6.0.0
    | LeZIndex ZIndex
      -- ^ The z-index indicating the layering of the legend group relative
      --   to the other axis, mark, and legend groups.
      --
      --   @since 0.9.0.0

legendConfigProperty :: LegendConfig -> Pair
legendConfigProperty (LeAria b) = "bool" .= b
legendConfigProperty (LeAriaDescription t) = "description" .= t
legendConfigProperty (LeClipHeight x) = "clipHeight" .= x
legendConfigProperty (LeColumnPadding x) = "columnPadding" .= x
legendConfigProperty (LeColumns n) = "columns" .= n
legendConfigProperty (LeCornerRadius x) = "cornerRadius" .= x
legendConfigProperty (LeDirection o) = "direction" .= orientationSpec o
legendConfigProperty (LeDisable b) = "disable" .= b
legendConfigProperty (LeFillColor s) = "fillColor" .= fromColor s
legendConfigProperty (LeGradientDirection o) = "gradientDirection" .= orientationSpec o
legendConfigProperty (LeGradientHorizontalMaxLength x) = "gradientHorizontalMaxLength" .= x
legendConfigProperty (LeGradientHorizontalMinLength x) = "gradientHorizontalMinLength" .= x
legendConfigProperty (LeGradientLabelLimit x) = "gradientLabelLimit" .= x
legendConfigProperty (LeGradientLabelOffset x) = "gradientLabelOffset" .= x
legendConfigProperty (LeGradientLength x) = "gradientLength" .= x
legendConfigProperty (LeGradientOpacity x) = "gradientOpacity" .= x
legendConfigProperty (LeGradientStrokeColor s) = "gradientStrokeColor" .= fromColor s
legendConfigProperty (LeGradientStrokeWidth x) = "gradientStrokeWidth" .= x
legendConfigProperty (LeGradientThickness x) = "gradientThickness" .= x
legendConfigProperty (LeGradientVerticalMaxLength x) = "gradientVerticalMaxLength" .= x
legendConfigProperty (LeGradientVerticalMinLength x) = "gradientVerticalMinLength" .= x
legendConfigProperty (LeGridAlign ga) = "gridAlign" .= compositionAlignmentSpec ga
legendConfigProperty (LeLabelAlign ha) = "labelAlign" .= hAlignLabel ha
legendConfigProperty (LeLabelBaseline va) = "labelBaseline" .= vAlignLabel va
legendConfigProperty (LeLabelColor s) = "labelColor" .= fromColor s
legendConfigProperty (LeLabelFont s) = "labelFont" .= s
legendConfigProperty (LeLabelFontSize x) = "labelFontSize" .= x
legendConfigProperty (LeLabelFontStyle s) = "labelFontStyle" .= s
legendConfigProperty (LeLabelFontWeight fw) = "labelFontWeight" .= fontWeightSpec fw
legendConfigProperty (LeLabelLimit x) = "labelLimit" .= x
legendConfigProperty (LeLabelOffset x) = "labelOffset" .= x
legendConfigProperty (LeLabelOpacity x) = "labelOpacity" .= x
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
legendConfigProperty (LeStrokeColor s) = "strokeColor" .= fromColor s
legendConfigProperty (LeStrokeDash xs) = "strokeDash" .= fromDS xs
legendConfigProperty (LeStrokeWidth x) = "strokeWidth" .= x
legendConfigProperty (LeSymbolBaseFillColor s) = "symbolBaseFillColor" .= fromColor s
legendConfigProperty (LeSymbolBaseStrokeColor s) = "symbolBaseStrokeColor" .= fromColor s
legendConfigProperty (LeSymbolDash xs) = "symbolDash" .= fromDS xs
legendConfigProperty (LeSymbolDashOffset x) = "symbolDashOffset" .= x
legendConfigProperty (LeSymbolDirection o) = "symbolDirection" .= orientationSpec o
legendConfigProperty (LeSymbolFillColor s) = "symbolFillColor" .= fromColor s
legendConfigProperty (LeSymbolLimit n) = "symbolLimit" .= n
legendConfigProperty (LeSymbolOffset x) = "symbolOffset" .= x
legendConfigProperty (LeSymbolOpacity x) = "symbolOpacity" .= x
legendConfigProperty (LeSymbolSize x) = "symbolSize" .= x
legendConfigProperty (LeSymbolStrokeColor s) = "symbolStrokeColor" .= fromColor s
legendConfigProperty (LeSymbolStrokeWidth x) = "symbolStrokeWidth" .= x
legendConfigProperty (LeSymbolType s) = "symbolType" .= symbolLabel s
legendConfigProperty (LeTickCount n) = "tickCount" .= n
legendConfigProperty (LeTickCountTime sn) = "tickCount" .= scaleNiceSpec sn
legendConfigProperty LeNoTitle = "title" .= A.Null
legendConfigProperty (LeTitleAlign ha) = "titleAlign" .= hAlignLabel ha
legendConfigProperty (LeTitleAnchor anc) = "titleAnchor" .= anchorLabel anc
legendConfigProperty (LeTitleBaseline va) = "titleBaseline" .= vAlignLabel va
legendConfigProperty (LeTitleColor s) = "titleColor" .= fromColor s
legendConfigProperty (LeTitleFont s) = "titleFont" .= s
legendConfigProperty (LeTitleFontSize x) = "titleFontSize" .= x
legendConfigProperty (LeTitleFontStyle s) = "titleFontStyle" .= s
legendConfigProperty (LeTitleFontWeight fw) = "titleFontWeight" .= fontWeightSpec fw
legendConfigProperty (LeTitleLimit x) = "titleLimit" .= x
legendConfigProperty (LeTitleLineHeight x) = "titleLineHeight" .= x
legendConfigProperty (LeTitleOpacity x) = "titleOpacity" .= x
legendConfigProperty (LeTitleOrient orient) = "titleOrient" .= sideLabel orient
legendConfigProperty (LeTitlePadding x) = "titlePadding" .= x
legendConfigProperty (LeUnselectedOpacity x) = "unselectedOpacity" .= x
legendConfigProperty (LeZIndex z) = "zindex" .= z


{-|

Properties for customising the colors of a range. The parameter should be a
named color scheme such as @\"accent\"@ or @\"purpleorange-11\"@. For details see the
<https://vega.github.io/vega/docs/schemes/#scheme-properties Vega-Lite documentation>.

Used by 'RangeStyle'.

-}
data RangeConfig
    = RCategory T.Text
    | RDiverging T.Text
    | RHeatmap T.Text
    | ROrdinal T.Text
    | RRamp T.Text
    | RSymbol T.Text


rangeConfigProperty :: RangeConfig -> Pair
rangeConfigProperty rangeCfg =
  let (l, n) = case rangeCfg of
        RCategory nme -> ("category", nme)
        RDiverging nme -> ("diverging", nme)
        RHeatmap nme -> ("heatmap", nme)
        ROrdinal nme -> ("ordinal", nme)
        RRamp nme -> ("ramp", nme)
        RSymbol nme -> ("symbol", nme)

  in l .= object [schemeProperty n []]


scaleConfigProperty :: ScaleConfig -> Pair
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
scaleConfigProperty (SCRound b) = "round" .= b
scaleConfigProperty (SCUseUnaggregatedDomain b) = "useUnaggregatedDomain" .= b
scaleConfigProperty (SCXReverse b) = "xReverse" .= b


{-|

View configuration property. These are used to configure the style of a single
view within a visualization (via 'ViewStyle') such as its size and default fill and stroke colors.
For further details see the
<https://vega.github.io/vega-lite/docs/spec.html#config Vega-Lite documentation>.

In version @0.6.0.0@ the constructors that used to take an optional color,
namely 'ViewFill' and 'ViewStroke', were split out, so that they
now take a 'Color' argument and new constructors - 'ViewNoFill' and
'ViewNoStroke' - were added to replace the @Nothing@ versions.

In version @0.5.0.0@ the @ViewWidth@ and @ViewHeight@ constructors have
been deprecated, and replaced by
'ViewContinuousWidth', 'ViewContinuousHeight',
'ViewDiscreteWidth', and 'ViewDiscreteHeight'. The 'ViewBackgroundStyle'
constructor has been added.

This type has been changed in the @0.4.0.0@ release to use a consistent
naming scheme for the constructors (everything starts with @View@). Prior to
this release only @ViewWidth@ and @ViewHeight@ were named this way. There
are also five new constructors.

-}

-- based on schema 3.3.0 #/definitions/ViewConfig

{-# DEPRECATED ViewWidth "Please change ViewWidth to ViewContinuousWidth" #-}
{-# DEPRECATED ViewHeight "Please change ViewHeight to ViewContinuousHeight" #-}
data ViewConfig
    = ViewBackgroundStyle [ViewBackground]
      -- ^ The default single-view style.
      --
      --   @since 0.5.0.0
    | ViewClip Bool
      -- ^ Should the view be clipped?
    | ViewContinuousWidth Double
      -- ^ The default width of single views when the
      --   visualization has a continuous x field.
      --
      --   @since 0.5.0.0
    | ViewContinuousHeight Double
      -- ^ The default height of single views when the
      --   visualization has a continuous y field.
      --
      --   @since 0.5.0.0
    | ViewCornerRadius Double
      -- ^ The radius, in pixels, of rounded rectangle corners.
      --
      --   The default is @0@.
      --
      --   @since 0.4.0.0
    | ViewCursor Cursor
      -- ^ The default cursor for single views.
      --
      --   @since 0.6.0.0
    | ViewDiscreteWidth Double
      -- ^ The default width of single views when the
      --   visualization has a discrete x field.
      --
      --   @since 0.5.0.0
    | ViewDiscreteHeight Double
      -- ^ The default height of single views when the
      --   visualization has a discrete y field.
      --
      --   @since 0.5.0.0
    | ViewFill Color
      -- ^ The fill color. See also 'ViewNoFill'.
      --
      --   This was changed to use the @Color@ type alias in version @0.5.0.0@
      --   and removed the @Maybe@ type in version @0.6.0.0@.
    | ViewNoFill
      -- ^ Do not use a fill. See also 'ViewFill'.
      --
      --   @since 0.6.0.0
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
    | ViewStep Double
      -- ^ Default step size for discrete fields.
      --
      --   This replaces @SCRangeStep@ and @SCTextXRangeStep@ from
      --   'ScaleConfig'.
      --
      --   @since 0.5.0.0
    | ViewStroke Color
      -- ^ The stroke color. See also 'ViewNoStroke'.
      --
      --   This was changed to use the @Color@ type alias in version @0.5.0.0@
      --   and removed the @Maybe@ type in version @0.6.0.0@.
    | ViewNoStroke
      -- ^ Do not use a stroke color. See also 'ViewStroke'.
      --
      --   @since 0.6.0.0
    | ViewStrokeCap StrokeCap
      -- ^ The stroke cap for line-ending style.
      --
      --   @since 0.4.0.0
    | ViewStrokeDash DashStyle
      -- ^ The stroke dash pattern.
    | ViewStrokeDashOffset DashOffset
      -- ^ The offset for the dash pattern.
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
    | ViewWidth Double
      -- ^ As of version @0.5.0.0@ this is deprecated and 'ViewContinuousWidth' should
      --   be used instead.
    | ViewHeight Double
      -- ^ As of version @0.5.0.0@ this is deprecated and 'ViewContinuousHeight' should
      --   be used instead.


viewConfigProperties :: ViewConfig -> [Pair]
viewConfigProperties (ViewBackgroundStyle bs) = map viewBackgroundSpec bs
viewConfigProperties (ViewClip b) = ["clip" .= b]
viewConfigProperties (ViewWidth x) = ["continuousWidth" .= x]
viewConfigProperties (ViewHeight x) = ["continuousHeight" .= x]
viewConfigProperties (ViewContinuousWidth x) = ["continuousWidth" .= x]
viewConfigProperties (ViewContinuousHeight x) = ["continuousHeight" .= x]
viewConfigProperties (ViewCornerRadius x) = ["cornerRadius" .= x]
viewConfigProperties (ViewCursor c) = ["cursor" .= cursorLabel c]
viewConfigProperties (ViewDiscreteWidth x) = ["discreteWidth" .= x]
viewConfigProperties (ViewDiscreteHeight x) = ["discreteHeight" .= x]
viewConfigProperties (ViewFill ms) = ["fill" .= fromColor ms]
viewConfigProperties ViewNoFill = ["fill" .= A.Null]
viewConfigProperties (ViewFillOpacity x) = ["fillOpacity" .= x]
viewConfigProperties (ViewOpacity x) = ["opacity" .= x]
viewConfigProperties (ViewStep x) = ["step" .= x]
viewConfigProperties (ViewStroke ms) = ["stroke" .= fromColor ms]
viewConfigProperties ViewNoStroke = ["stroke" .= A.Null]
viewConfigProperties (ViewStrokeCap sc) = ["strokeCap" .= strokeCapLabel sc]
viewConfigProperties (ViewStrokeDash xs) = ["strokeDash" .= fromDS xs]
viewConfigProperties (ViewStrokeDashOffset x) = ["strokeDashOffset" .= x]
viewConfigProperties (ViewStrokeJoin sj) = ["strokeJoin" .= strokeJoinLabel sj]
viewConfigProperties (ViewStrokeMiterLimit x) = ["strokeMiterLimit" .= x]
viewConfigProperties (ViewStrokeOpacity x) = ["strokeOpacity" .= x]
viewConfigProperties (ViewStrokeWidth x) = ["strokeWidth" .= x]


{-|

Axis configuration options for customising all axes. See the
<https://vega.github.io/vega-lite/docs/axis.html#general-config Vega-Lite documentation>
for more details.

This is used by 'ConfigurationProperty'.

In @0.5.0.0@ the @ShortTimeLabels@ constructor was removed.

The @TitleMaxLength@ constructor was removed in release @0.4.0.0@. The
@TitleLimit@ constructor should be used instead.

-}
data AxisConfig
    = Aria Bool
      -- ^ A boolean flag indicating if
      --   [ARIA attributes](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)
      --   should be included (SVG output only).
      --
      --   If False, the \"aria-hidden\" attribute will be set on the output SVG group, removing
      --   the axis from the ARIA accessibility tree.
      --
      --   __Default value:__ True
      --
      --   @since 0.9.0.0
    | AriaDescription T.Text
      -- ^ A text description of this axis for
      --   [ARIA accessibility](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)
      --   (SVG output only).
      --
      --   If the 'Aria' property is True, for SVG output the
      --   [\"aria-label\" attribute](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_aria-label_attribute)
      --   will be set to this description.
      --
      --   If the description is unspecified it will be automatically generated.
      --
      --   @since 0.9.0.0
    | AStyle [StyleLabel]
      -- ^ The named styles - generated with 'AxisNamedStyles' - to apply to the
      --   axis or axes.
      --
      --   Added in Vega-Lite 4.7.0 (although accidentally supported in @hvega@
      --   before this release).
      --
      --   @since 0.6.0.0
    | BandPosition Double
      -- ^ The default axis band position.
    | Disable Bool
      -- ^ Disable the axis?
      --
      --   Added in Vega-Lite 4.8.0.
      --
      --  @since 0.8.0.0
    | Domain Bool
      -- ^ Should the axis domain be displayed?
    | DomainCap StrokeCap
      -- ^ The stroke cap for the domain lines' ending style.
      --
      --   @since 0.9.0.0
    | DomainColor Color
      -- ^ The axis domain color.
    | DomainDash DashStyle
      -- ^ The dash pattern of the domain.
      --
      --   @since 0.4.0.0
    | DomainDashOffset DashOffset
      -- ^ The offset for the dash pattern.
      --
      --   @since 0.4.0.0
    | DomainOpacity Opacity
      -- ^ The axis domain opacity.
      --
      --   @since 0.4.0.0
    | DomainWidth Double
      -- ^ The width of the axis domain.
    | Format T.Text
      -- ^ [Formatting pattern](https://vega.github.io/vega-lite/docs/format.html) for
      --   axis values. To distinguish between formatting as numeric values
      --   and data/time values, additionally use 'FormatAsNum', 'FormatAsTemporal',
      --   or 'FormatAsCustom'.
      --
      --   When used with a [custom formatType](https://vega.github.io/vega-lite/docs/config.html#custom-format-type),
      --   this value will be passed as \"format\" alongside \"datum.value\" to the
      --   registered function.
      --
      --   @since 0.9.0.0
    | FormatAsNum
      -- ^ Facet headers should be formatted as numbers. Use a
      --   [d3 numeric format string](https://github.com/d3/d3-format#locale_format)
      --   with 'Format'.
      --
      --   @since 0.9.0.0
    | FormatAsTemporal
      -- ^ Facet headers should be formatted as dates or times. Use a
      --   [d3 date/time format string](https://github.com/d3/d3-time-format#locale_format)
      --   with 'Format'.
      --
      --   @since 0.9.0.0
    | FormatAsCustom T.Text
      -- ^ The [custom format type](https://vega.github.io/vega-lite/docs/config.html#custom-format-type)
      --   for use with with 'Format'.
      --
      --   @since 0.9.0.0
    | Grid Bool
      -- ^ Should an axis grid be displayed?
    | GridCap StrokeCap
      -- ^ The stroke cap for the grid lines' ending style.
      --
      --   @since 0.9.0.0
    | GridColor Color
      -- ^ The color for the grid.
    | GridDash DashStyle
      -- ^ The dash pattern of the grid.
    | GridDashOffset DashOffset
      -- ^ The offset for the dash pattern.
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
    | LabelLineHeight Double
      -- ^ The line height, in pixels, for multi-line label text.
      --
      --   Added in Vega-Lite 4.6.0.
      --
      --   @since 0.7.0.0
    | LabelOffset Double
      -- ^ The pixel offset for labels, in addition to 'TickOffset'.
      --
      --   @since 0.6.0.0
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
    | Ticks Bool
      -- ^ Should tick marks be drawn on an axis?
    | TickBand BandAlign
      -- ^ For band scales, indicates if ticks and grid lines should be
      --   placed at the center of a band (the default) or at the band
      --   extents to indicate intervals.
      --
      --   @since 0.5.0.0
    | TickCap StrokeCap
      -- ^ The stroke cap for the grid lines' ending style.
      --
      --   @since 0.9.0.0
    | TickColor Color
      -- ^ The color of the ticks.
    | TickCount Int
      -- ^ The desired number of ticks for axes visualizing quantitative scales.
      --   This is a hint to the system, and the actual number used will be
      --   adjusted to be \"nice\" (multiples of 2, 5, or 10) and lie within the
      --   underlying scale's range.
      --
      --   The 'TickCountTime' option can instead be used for \"time\" or
      --   \"utc\" scales.
      --
      --   @since 0.9.0.0
    | TickCountTime ScaleNice
      -- ^ A specialised version of 'TickCount' for \"time\" and \"utc\"
      --   time scales.
      --
      --   The 'Graphics.Vega.VegaLite.IsNice' and 'Graphics.Vega.VegaLte.NTickCount'
      --   options should not be used as they generate invalid VegaLite.
      --
      --   @since 0.9.0.0
    | TickDash DashStyle
      -- ^ The dash pattern of the ticks.
    | TickDashOffset DashOffset
      -- ^ The offset for the dash pattern.
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
      --   See also 'LabelOffset'.
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
    | TitleLineHeight Double
      -- ^ Line height, in pixels, for multi-line title text.
      --
      --   @since 0.5.0.0
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
    | TranslateOffset Double
      -- ^ The translation offset in pixels applied to the axis group
      --   mark x and y. If specified it overrides the default value
      --   of a 0.5 offset to pixel-align stroked lines.
      --
      --   @since 0.5.0.0


axisConfigProperty :: AxisConfig -> Pair
axisConfigProperty (AStyle [s]) = "style" .= s
axisConfigProperty (AStyle s) = "style" .= s

axisConfigProperty (Aria b) = "aria" .= b
axisConfigProperty (AriaDescription t) = "description" .= t

axisConfigProperty (BandPosition x) = "bandPosition" .= x
axisConfigProperty (Disable b) = "disable" .= b
axisConfigProperty (Domain b) = "domain" .= b
axisConfigProperty (DomainCap c) = "domainCap" .= strokeCapLabel c
axisConfigProperty (DomainColor c) = "domainColor" .= fromColor c
axisConfigProperty (DomainDash ds) = "domainDash" .= fromDS ds
axisConfigProperty (DomainDashOffset x) = "domainDashOffset" .= x
axisConfigProperty (DomainOpacity x) = "domainOpacity" .= x
axisConfigProperty (DomainWidth w) = "domainWidth" .= w

axisConfigProperty (Format fmt) = "format" .= fmt
axisConfigProperty FormatAsNum = "formatNum" .= fromT "number"
axisConfigProperty FormatAsTemporal = "formatNum" .= fromT "type"
axisConfigProperty (FormatAsCustom c) = "formatType" .= c

axisConfigProperty (Grid b) = "grid" .= b
axisConfigProperty (GridCap c) = "gridCap" .= strokeCapLabel c
axisConfigProperty (GridColor c) = "gridColor" .= fromColor c
axisConfigProperty (GridDash ds) = "gridDash" .= fromDS ds
axisConfigProperty (GridDashOffset x) = "gridDashOffset" .= x
axisConfigProperty (GridOpacity o) = "gridOpacity" .= o
axisConfigProperty (GridWidth x) = "gridWidth" .= x
axisConfigProperty (LabelAlign ha) = "labelAlign" .= hAlignLabel ha
axisConfigProperty (LabelAngle angle) = "labelAngle" .= angle
axisConfigProperty (LabelBaseline va) = "labelBaseline" .= vAlignLabel va
axisConfigProperty LabelNoBound = "labelBound" .= False
axisConfigProperty LabelBound = "labelBound" .= True
axisConfigProperty (LabelBoundValue x) = "labelBound" .= x
axisConfigProperty (LabelColor c) = "labelColor" .= fromColor c
axisConfigProperty LabelNoFlush = "labelFlush" .= False
axisConfigProperty LabelFlush = "labelFlush" .= True
axisConfigProperty (LabelFlushValue x) = "labelFlush" .= x
axisConfigProperty (LabelFlushOffset x) = "labelFlushOffset" .= x
axisConfigProperty (LabelFont f) = "labelFont" .= f
axisConfigProperty (LabelFontSize x) = "labelFontSize" .= x
axisConfigProperty (LabelFontStyle s) = "labelFontStyle" .= s
axisConfigProperty (LabelFontWeight fw) = "labelFontWeight" .= fontWeightSpec fw
axisConfigProperty (LabelLimit x) = "labelLimit" .= x
axisConfigProperty (LabelLineHeight x) = "labelLineHeight" .= x
axisConfigProperty (LabelOffset x) = "labelOffset" .= x
axisConfigProperty (LabelOpacity x) = "labelOpacity" .= x
axisConfigProperty (LabelOverlap strat) = "labelOverlap" .= overlapStrategyLabel strat
axisConfigProperty (LabelPadding pad) = "labelPadding" .= pad
axisConfigProperty (LabelSeparation x) = "labelSeparation" .= x
axisConfigProperty (Labels b) = "labels" .= b
axisConfigProperty (MaxExtent n) = "maxExtent" .= n
axisConfigProperty (MinExtent n) = "minExtent" .= n
axisConfigProperty (Orient orient) = "orient" .= sideLabel orient
axisConfigProperty (TickBand band) = "tickBand" .= bandAlignLabel band
axisConfigProperty (TickCap c) = "tickCap" .= strokeCapLabel c
axisConfigProperty (TickColor c) = "tickColor" .= fromColor c
axisConfigProperty (TickCount n) = "tickCount" .= n
axisConfigProperty (TickCountTime sn) = "tickCount" .= scaleNiceSpec sn
axisConfigProperty (TickDash ds) = "tickDash" .= fromDS ds
axisConfigProperty (TickDashOffset x) = "tickDashOffset" .= x
axisConfigProperty (TickExtra b) = "tickExtra" .= b
axisConfigProperty (TickOffset x) = "tickOffset" .= x
axisConfigProperty (TickOpacity x) = "tickOpacity" .= x
axisConfigProperty (TickRound b) = "tickRound" .= b
axisConfigProperty (TickSize x) = "tickSize" .= x
axisConfigProperty (TickWidth x) = "tickWidth" .= x
axisConfigProperty (Ticks b) = "ticks" .= b
axisConfigProperty NoTitle = "title" .= A.Null
axisConfigProperty (TitleAlign algn) = "titleAlign" .= hAlignLabel algn
axisConfigProperty (TitleAnchor a) = "titleAnchor" .= anchorLabel a
axisConfigProperty (TitleAngle x) = "titleAngle" .= x
axisConfigProperty (TitleBaseline va) = "titleBaseline" .= vAlignLabel va
axisConfigProperty (TitleColor c) = "titleColor" .= fromColor c
axisConfigProperty (TitleFont f) = "titleFont" .= f
axisConfigProperty (TitleFontSize x) = "titleFontSize" .= x
axisConfigProperty (TitleFontStyle s) = "titleFontStyle" .= s
axisConfigProperty (TitleFontWeight w) = "titleFontWeight" .= fontWeightSpec w
axisConfigProperty (TitleLimit x) = "titleLimit" .= x
axisConfigProperty (TitleLineHeight x) = "titleLineHeight" .= x
axisConfigProperty (TitleOpacity x) = "titleOpacity" .= x
axisConfigProperty (TitlePadding x) = "titlePadding" .= x
axisConfigProperty (TitleX x) = "titleX" .= x
axisConfigProperty (TitleY x) = "titleY" .= x
axisConfigProperty (TranslateOffset x) = "translate" .= x


-- | Specifies how the title anchor is positioned relative to the frame.
--
--   @since 0.4.0.0
data TitleFrame
    = FrBounds
      -- ^ The position is relative to the full bounding box.
    | FrGroup
      -- ^ The pistion is relative to the group width / height.

titleFrameSpec :: TitleFrame -> VLSpec
titleFrameSpec FrBounds = "bounds"
titleFrameSpec FrGroup = "group"


{-|

Title configuration properties. These are used to configure the default style
of all titles within a visualization with 'title' or 'TitleStyle'.

For further details see the
<https://vega.github.io/vega-lite/docs/title.html#config Vega-Lite documentation>.

-}

-- NOTES:
--   do not have a 'TTitle' field because this is handled by the first
--   argument of title, so there's no point in having it here (also, would
--   have to be called something different than TTitle as we already have
--   this).
--
--   could move TSubtitle out too, but the ergonomics aren't great
--   either way

data TitleConfig
    = TAlign HAlign
      -- ^ The horizontal text alignment for title text.
      --
      --   @since 0.5.0.0
    | TAnchor APosition
      -- ^ The anchor position when placing titles.
    | TAngle Angle
      -- ^ The angle when orientating titles.
    | TAria Bool
      -- ^ A boolean flag indicating if
      --   [ARIA attributes](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)
      --   should be included (SVG output only).
      --
      --   If False, the \"aria-hidden\" attribute will be set on the output SVG group,
      --   removing the title from the ARIA accessibility tree.
      --
      --   __Default value:__ True
      --
      --   @since 0.9.0.0
    | TBaseline VAlign
      -- ^ The vertical alignment when placing titles.
    | TColor Color  -- this allows for null as a color
      -- ^ The color of title text.
    | TdX Double
      -- ^ The offset, in pixels, for the x coordinate of title and subtitle text.
      --
      --   @since 0.5.0.0
    | TdY Double
      -- ^ The offset, in pixels, for the x coordinate of title and subtitle text.
      --
      --   @since 0.5.0.0
    | TFont T.Text
      -- ^ Default font when showing titles.
    | TFontSize Double
      -- ^ Default font size when showing titles.
    | TFontStyle T.Text
      -- ^ Defaylt font style when showing titles.
      --
      --   @since 0.4.0.0
    | TFontWeight FontWeight
      -- ^ Default font weight when showing titles.
    | TFrame TitleFrame
      -- ^ Default title position anchor.
      --
      --   @since 0.4.0.0
    | TLimit Double
      -- ^ The maximum length, in pixels, of title and subtitle text.
    | TLineHeight Double
      -- ^ Line height, in pixels, for multi-line title text.
      --
      --   @since 0.5.0.0
    | TOffset Double
      -- ^ Default offset, in pixels, of titles relative to the chart body.
    | TOrient Side
      -- ^ Default placement of titles relative to the chart body.
    | TStyle [StyleLabel]
      -- ^ A list of named styles to apply. A named style can be specified
      --   via 'Graphics.Vega.VegaLite.MarkNamedStyles'. Later styles in the list will
      --   override earlier ones if there is a conflict in any of the
      --   properties.
      --
      --   @since 0.4.0.0
    | TSubtitle T.Text
      -- ^ Subtitle text. This is placed below the title text. Use \n
      --   to insert line breaks into the subtitle.
      --
      --   This should only be used with 'title' and not 'TitleConfig'.
      --
      --   @since 0.5.0.0
    | TSubtitleColor Color
      -- ^ Subtitle color.
      --
      --   @since 0.5.0.0
    | TSubtitleFont T.Text
      -- ^ Subtitle font.
      --
      --   @since 0.5.0.0
    | TSubtitleFontSize Double
      -- ^ Subtitle font size, in pixels.
      --
      --   @since 0.5.0.0
    | TSubtitleFontStyle T.Text
      -- ^ Subtitle font style.
      --
      --   @since 0.5.0.0
    | TSubtitleFontWeight FontWeight
      -- ^ Subtitle font weight.
      --
      --   @since 0.5.0.0
    | TSubtitleLineHeight Double
      -- ^ Subtitle line height, in pixels.
      --
      --   @since 0.5.0.0
    | TSubtitlePadding Double
      -- ^ Padding, in pixels, between the title and Subtitle.
      --
      --   @since 0.5.0.0
    | TZIndex ZIndex
      -- ^ Drawing order of a title relative to the other chart elements.
      --
      --   @since 0.4.0.0


titleConfigSpec :: TitleConfig -> Pair
titleConfigSpec (TAlign ha) = "align" .= hAlignLabel ha
titleConfigSpec (TAnchor an) = "anchor" .= anchorLabel an
titleConfigSpec (TAngle x) = "angle" .= x
titleConfigSpec (TAria b) = "aria" .= b
titleConfigSpec (TBaseline va) = "baseline" .= vAlignLabel va
titleConfigSpec (TColor clr) = "color" .= fromColor clr
titleConfigSpec (TdX x) = "dx" .= x
titleConfigSpec (TdY x) = "dy" .= x
titleConfigSpec (TFont fnt) = "font" .= fnt
titleConfigSpec (TFontSize x) = "fontSize" .= x
titleConfigSpec (TFontStyle s) = "fontStyle" .= s
titleConfigSpec (TFontWeight w) = "fontWeight" .= fontWeightSpec w
titleConfigSpec (TFrame tf) = "frame" .= titleFrameSpec tf
titleConfigSpec (TLimit x) = "limit" .= x
titleConfigSpec (TLineHeight x) = "lineHeight" .= x
titleConfigSpec (TOffset x) = "offset" .= x
titleConfigSpec (TOrient sd) = "orient" .= sideLabel sd
titleConfigSpec (TStyle [style]) = "style" .= style  -- minor simplification
titleConfigSpec (TStyle styles) = "style" .= styles
titleConfigSpec (TSubtitle s) = "subtitle" .= splitOnNewline s
titleConfigSpec (TSubtitleColor s) = "subtitleColor" .= fromColor s
titleConfigSpec (TSubtitleFont s) = "subtitleFont" .= s
titleConfigSpec (TSubtitleFontSize x) = "subtitleFontSize" .= x
titleConfigSpec (TSubtitleFontStyle s) = "subtitleFontStyle" .= s
titleConfigSpec (TSubtitleFontWeight fw) = "subtitleFontWeight" .= fontWeightSpec fw
titleConfigSpec (TSubtitleLineHeight x) = "subtitleLineHeight" .= x
titleConfigSpec (TSubtitlePadding x) = "subtitlePadding" .= x
titleConfigSpec (TZIndex z) = "zindex" .= z


{-|

Configuration options for composition views, used with
'ConcatStyle', 'FacetStyle', and 'RepeatStyle'.

Prior to @0.6.0.0@ this information was made available in
two types - @ConcatConfig@ and @FacetConfig@ - which had
the same meaning.

@since 0.6.0.0

-}
data CompositionConfig
    = CompColumns Int
      -- ^ The number of columns to use. The default is to use a single
      --   row (an infinite number of columns).
      --
      --   Prior to @0.6.0.0@ this was either @ConcatColumns@ or @FColumns@.
    | CompSpacing Double
      -- ^ The spacing in pixels between sub-views. The default is 20.
      --
      --   Prior to @0.6.0.0@ this was either @ConcatSpacing@ or @FSpacing@.


compConfigProperty :: CompositionConfig -> Pair
compConfigProperty (CompColumns n) = "columns" .= n
compConfigProperty (CompSpacing x) = "spacing" .= x


{-|

Defines a single configuration option to be applied globally across the visualization.
The first parameter identifies the type of configuration, the second a list of previous
configurations to which this one may be added.

The result should be used with 'Graphics.Vega.VegaLite.configure'.

@
'configuration' ('Axis' [ 'DomainWidth' 4 ]) []
@
-}
configuration ::
  ConfigurationProperty
  -> BuildConfigureSpecs
  -- ^ Prior to version @0.5.0.0@ this was @BuildLabelledSpecs@.
configuration cfg ols = CS (configProperty cfg) : ols


{-|

Provide an optional title to be displayed in the visualization.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'title' "Population Growth" ['TColor' \"orange\"]
    , 'Graphics.Vega.VegaLite.dataFromUrl' \"data/population.json\" []
    , 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Bar' []
    , 'Graphics.Vega.VegaLite.encoding' ...
    ]
@

Prior to @0.4.0.0@ there was no way to set the title options
(other than using 'configuration' with 'TitleStyle').

-}
title ::
  T.Text
  -- ^ The title. Any @'\n'@ characters are taken to mean a multi-line
  --   string and indicate a line break.
  --
  --   In version @0.5.0.0@, support for line breaks was added.
  -> [TitleConfig]
  -- ^ Configure the appearance of the title.
  --
  --   @since 0.4.0.0
  -> PropertySpec
title s [] =
  (VLTitle, splitOnNewline s)
title s topts =
  (VLTitle,
    object ("text" .= splitOnNewline s : map titleConfigSpec topts))
