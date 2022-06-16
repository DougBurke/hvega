For the latest version of this document, please see
[https://github.com/DougBurke/hvega/blob/master/hvega/CHANGELOG.md](https://github.com/DougBurke/hvega/blob/master/hvega/CHANGELOG.md).

## 0.12.0.3

Bump to support aeson version 2.1.

## 0.12.0.2

Bump to support text version 2.0. All hail our new UTF-8 rulers.

## 0.12.0.1

Bump to support bytestring version 0.11 for testing `hvega`. There
are no changes to the code.

## 0.12.0.0

Support aeson version 2.0. There is currently no significant change to
the API but it is expected that in a future release there will be when
we wil require a minimum version of 2.0 for aeson.

At the time of release of 0.12.0.0 the tests will fail when using
aeson >= 2.0 because `aeson-pretty` has not yet been updated (see
https://github.com/informatikr/aeson-pretty/issues/36).

## 0.11.0.1

There is no change in the code, only the tests, which should now
pass when using hashable 0.3.1.0.

## 0.11.0.0

The Vega-Lite tests are now validated against version 4.15 of the
Vega-Lite schema.

Note that `hvega` does __not__ provide any information to help users
take advantage of the (new to 4.14) ability to omit the type of a
field when [it can be inferred](https://vega.github.io/vega-lite/docs/type.html).
As the type is currently optional in `hvega` users can just not give a type.
The example [IHaskell notebooks](https://github.com/DougBurke/hvega/tree/master/notebooks)
have been updated to show off the optional support.

Similarly, Vega-Lite 4.14 allows you to share the type, scale, axis,
and legend in a shared encoding. There is no explicit support added in
0.11.0.0 because `hvega` already allowed you to create the specification.

### New Constructors

The `OrderChannel` type has gained `OBand`, `OTitle`/`ONoTitle`,
and conditional-predicate support with `ODataCondition`,
`OSelectionCondition`, and `ONumber` constructors.

The `MarkChannel` type has gained the `MNullValue` constructor.

The `ScaleRange` type has gained `RField`, `RMax`, and `RMin`
constructors.

### Breaking Changes

Domain settings in `ScaleProperty` and associated types have been
changed to better match the Vega-Lite schema: `SDomain` now takes
a new type (`DomainLimits`) which actually contains many of the
orignal symbols (so hopefully will require no changes), and a new
constructor has been added (`SDomainOpt`) which takes the
`ScaleDomain` type, which has seen new constructors - `DMax`,
`DMaxTime`, `DMid`, `DMin`, and `DMinTime` - as well as
some constructors moving to `DomainLimits`.

### Deprecated symbols

The `SDomainMid` constructor of `ScaleProperty` will be removed in a
future release as it has been replaced by the `DMid` constructor
in `ScaleDomain`.

## 0.10.0.0

The Vega-Lite tests are now validated against version 4.13 of the
Vega-Lite schema.

### Breaking Changes

The handling of time units (for both `TimeUnit` and `ScaleNice`) has
changed. The contents of these types have been split into two parts: a
"time unit" and the options that get applied to it (rather than having
a single type that combines both functions). This does mean that
setting time units has now become __more verbose__, but it has stopped
some problem cases (and, in the case of `ScaleNice`, fixed a logical
error on my part). The new time units are `BaseTimeUnit` and
`NTimeUnit`, and contain the "basic" constructors for the time
units. The `TimeUnit` and `ScaleNice` constructors now reference these
types rather than include them in their definition, so that `PTimeUnit
Month` has been changed to `PTimeUnit (TU Month)` and `SNice NMinute`
has changed to `SNice (NTU NMinute)`.

The `BaseTimeUnit` type has seen a number of additions: the `Week` and
`DayOfYear` time units added in Vega-Lite 4.13.0, along with the
associated composite units (such as `YearWeek`), and a number of
composite types that were missing (such as `MonthDateHours`).  The
`DateTime` type has added the `DTWeek` and `DTDayOfYear` constructors.

## 0.9.1.0

The tutorial has been expanded to add a section describing the
new pie chart support (the Arc mark).

## 0.9.0.1

The introductory text of the module has been updated to start
with the "cars" example (from the README) before rampng up to
the Betelgeuse example.

## 0.9.0.0

The Vega-Lite tests are now validated against version 4.12 of the
Vega-Lite schema.

### New Constructors

Support for arcs has been added: the `Arc` type has been added to
`Mark`; `Theta`, `Theta2`, `R`, and `R2` have been added to
`Position`; and `MInnerRadius`, `MOuterRadius`, `MPadAngle`,
`MRadius2`, `MRadiusOffset`, `MRadius2Offset`, `MTheta2`,
`MThetaOffset`, and `MTheta2Offset` added to `MarkProperty`.
`ArcStyle` has been added to `ConfigurationProperty`.

Support for ARIA attributes has been added to a number of features
(e.g. `Aria` and `AriaDescription` for `AxisConfig` and `MAria`,
`MAriaDescription`, `MAriaRole`, `MAriaRoleDescription` for
`MarkProperty`, `AriaStyle` for `ConfigurationProperty`).  The
`ariaDescrption` encoding has been added, along with the
`AriaDescriptionChannel`.

The `angle` encoding channel has been added for text and point marks.

The `Channel` type has gained `ChAngle`, `ChTheta`, `ChTheta2`,
`ChRadius`, `ChRadius`, `ChDescription`, and `ChURL`.

Layers have been added to `Arrangement` (`Layer`) and to `RepeatFields`
(`LayerFields`).

The `MRepeatDatum` and `MDatum`, `PRepeatDatum` and `PDatum`, and
`TRepeatDatum` and `TDatum` pairs have been added to `MarkChannel`,
`PositionChannel`, and `TextChannel` respectively.

The `MarkProperty` now has support for labelling the X (or X2)
coordinate as the "width" of the plot and Y (or Y2) as the "height" of
the plot. See `MXWidth`, `MX2Width`, `MYHeight`, and `MY2Height`.

Improved support for tick scales: `TickCount` and `TickCountTime` have
been added to `AxisConfig`, `AxTickCountTime` has been added to
`AxisProperty`, `LTickCountTime` has been added to `LegendProperty`,
and `LeTickCountTime` has been added to `LegendConfig`.

Add support for width and height to scale's range, with the `RHeight` and
`RWidth` constructors. The `RPair` constructor has been added as an
experimental means for specifying just the minimum and maximum values.

`AxisProperty` has gained `AxFormatAsCustom`. `AxisConfig` has gained
`Format`, `FormatAsNum`, `FormatAsTemporal`, and `FormatAsCustom`.
`LegendProperty` has gained `LFormatAsCustom`. `HeaderProperty` has
gained `HFormatAsCustom`. `TextChannel` has gained `TFormatAsCustom`.
The `ConfigurationProperty` type has a new option to configure support
for custom format types (`CustomFormatStyle`).

`AxisConfig` and `AxisProperty` have gained new cap styles:
`DomainCap`, `GridCap`, `TickCap` and `AxDomainCap`, `AxGridCap`,
`AxTickCap` respectively.

The `TZIndex` option of `TitleConfig` can now be used with `TitleStyle`
(prior to Vega-Lite 4.12 it was only supported when used with
`title`). The `LeZIndex` type has been added to `LegendConfig`.

The `HyperlinkChannel` has gained a number of constructors it was
missing: `HyBand`, `HyFormat`, `HyFormatAsNum`, `HyFormatAsTemporal`,
`HyFormatAsCustom`, `HyLabelExpr`, `HyTitle`, and `HyNoTitle`.  A
similar update has been made to `TextChannel`, which has gained
`TBand` and `TLabelExpr`.

## 0.8.0.0

The Vega-Lite tests are now validated against version 4.8 of the
Vega-Lite Sschema.

The `RepeatStyle` constructor for `ConfigurationProperty` should not
be used, as its functionality has been moved to `ConcatStyle` in
Vega-Lite 4.8. This constructor will be removed at some point in the
future but is still available (as support for Vega-Lite 4.8 is
limited).

### Breaking Changes

The `HTitleFontWeight` constructor (a member of `HeaderProperty`)
now takes a `FontWeight` argument rather than `Text`.

The `LeTitle` constructor from `LegendConfig` was removed as it is not
supported in Vega-Lite (`LeNoTitle` remains, as it is used to remove
legend titles from a visualization).

`ScBinLinear` was removed from `Scale` as it is not used by Vega-Lite.

### New constructors

The `HeaderProperty` type has gained the following constructors from
Vega-Lite 4.8: `HLabelBaseline`, `HLabelFontWeight`,
`HLabelLineHeight`, and `HOrient`.

The `AxisConfig` type has gained the `Disable` constructor from
Vega-Lite 4.8.

The `LegendConfig` type has gained the `LeDirection` and (from
Vega-Lite 4.8) `LeDisable` constructors. The `LegendProperty` type has
gained `LLabelExpr`, `LSymbolLimit`, and `LTitleLineHeight`
constructors.

## 0.7.0.1

Minor documentation fixes (typos and fixing links).

## 0.7.0.0

The Vega-Lite tests are now validated against version 4.7 of the
Vega-Lite schema.

### New functionality

The `BlendMode` type has been added for controlling how marks blend
with their background. This is used with the new `MBlend` constructor
for marks.

### Breaking Change

The axis style options for specific data- or mark- types (`AxisBand`,
`AxisDiscrete`, `AxisPoint`, `AxisQuantitative`, and `AxisTemporal`)
have been changed to accept an additional argument (the new
`AxisChoice` type) which defines which axis (X, Y, or both) the
configuration should be applied to. This is to support new axis
configuration options added in Vega-Lite 4.7.0.

The `ChTooltip` `Channel` constructor has been removed as support
for this channel type was dropped in Vega-Lite 4.

### New constructors

The `ScaleDomain` type has gained `DSelectionField` and
`DSelectionChannel` constructors, which allow you to link a scale
(e.g. an axis) to a selection that is projected over multiple fields
or encodings.

The `Operation` type has gained the `Product` specifier from Vega-Lite
4.6.0.

The `TextChannel` has gained `TStrings` to support multi-line labels.

The `VAlign` type has gained `AlignLineTop` and `AlignLineBottom`
(Vega-Lite 4.6.0).

`LineBreakStyle` has been added to `ConfigurationProperty`.

The height of multi-line axis labels can now be set with the
`LabelLineHeight` and `AxLabelLineHeight` properties of the
`AxisConfig` and `AxisProperty` types (Vega-Lite 4.6.0).

Numeric filter ranges, specified with `FRange`, can now be lower- or
upper-limits - `NumberRange` and `NumberRange` respectively - added to
the `FilterRange` type.

## 0.6.0.0

The Vega-Lite tests are now validated against version 4.5 of the
Vega-Lite schema.

### New functionality

New function for use with `encoding`: `strokeDash`. The `ChStrokeDash`
constructor has been added to the `Channel` type, and `RNumberLists`
(Vega-Lite 4.4) to `ScaleRange`.

Named styles have been added for axes as well as marks. As mentioned
below, this involves deprecating the previous constructors for naming
styles, as there are now separate configuration options:
`AxisNamedStyles` and `MarkNamedStyles`. The `AStyle` and `AxStyle`
options have been added to `AxisConfig` and `AxisProperty`
respectively.  The `StyleLabel` type alias has been added to help the
documentation, but provides no extra type safety.

### Breaking changes

The `ConcatStyle` and `FacetStyle` constructors for
`ConfigurationProperty` now accept a common type,
`CompositionConfig`, rather than having separate
`ConcatConfig` and `FacetConfig` types with the same meaning.
So `ConcatColumns` and `FColumns` have been replaced by `CompColumns`,
and `CompSpacing` and `FSpacing` by `CompSpacing`.

The `ViewFill` and `ViewStroke` constructors of `ViewConfig` no longer
take an optional `Color` argument. The Nothing case has been replaced
by new constructors: `ViewNoFill` and `ViewNoStroke`.

The `VBFill` and `VBStroke` constructors of `ViewBackground` no longer
take an optional `Color` argument. The Nothing case has been replaced
by new constructors: `VBNoFill` and `VBNoStroke`.

### New constructors

`FacetChannel` has gained the following constructors: `FAlign`,
`FCenter`, and `FSpacing`. The last one would have collides with
the `FacetStyle` option, but this has fortuitously been renamed to
`CompSpacing`.

`MSymbol` has been added to `MarkChannel` which can be used to make the
`shape` encoding conditional on a data or selection condition.

The `TUStep` and `TUMaxBins` constructors have been added to `TimeUnit`
for controlling how time values are binned.

The `MarkProperty` type has gained the `MCornerRadiusEnd` constructor,
which is used to draw rounded histogram bars, and `MTexts` for
specifying multiple text values.

Error box and band properties (constructors in `MarkProperty`) can now
be turned off with explicit `No` variants: `MNoBorders`, `MNoBox`,
`MNoMedian`, `MNoRule`, and `MNoTicks`. These join the `MNoOutliers`
constructor.

The `ScaleProperty` type has gained `SDomainMid`, useful for
asymmetric diverging color scales, and `SReverse` from Vega-Lite v4.5.
The `ScaleDomain` type has gained the `DUnionWith` option from
Vega-Lite v4.3. The `ScaleConfig` type has gained `SCXReverse`
from Vega-Lite v4.5.

Labels can now be vertically aligned to their baseline with the
`AlignBaseline` constructor of the `VAlign` type.

Headers (`HeaderProperty`) have gained the following constructors:
`HLabel`, `HLabelExpr`, `HLabelFontStyle`, `HTitleFontStyle`,
and `HTitleLineHeight`.

Conditional axis (`ConditionalAxisProperty`) has gained the following
constructors for features added in Vega-Lite v4.2 and v4.5:
`CAxLabelOffset`, `CAxLabelPadding`, and `CAxTickSize`.

Cursor handling has been enhanced (to match Vega-Lite 4.1):
`ViewCursor` has been added to `ViewConfig` and `SMCursor` to
`SelectionMarkProperty`.

The legend configuration has been updated (to match Vega-Lite 4.0)
with the addition of `LeSymbolLimit`, `LeTickCount`, `LeTitleLineHeight`,
and `LeUnselectedOpacity` constructors.

The axis configuration and property types (`AxisConfig` and
`AxisProperty`) have gained the Vega-Lite 4.4 `LabelOffset` and
`AxLabelOffset` constructors.  Note that version 4.4.0 of the
Vega-Lite specification has these fields as strings but this is fixed
in version 4.5.0.

`ConfigurationProperty` has added new constructors: `AxisDiscrete` and
`AxisPoint` from Vega-Lite 4.5, `AxisQuantitative` and `AxisTemporal`
from Vega-Lite 4.4, `BoxplotStyle`, `ErrorBandStyle`, `ErrorBarStyle`,
`FontStyle` (Vega-Lite 4.3), `HeaderColumnStyle`, `HeaderFacetStyle`,
`HeaderRowStyle`, `ImageStyle`, and `RepeatStyle`.

### Deprecated symbols

`ConfigurationProperty` has seen a large number of deprecations, as a
number of constructors have been renamed:

 - The `NamedStyle` and `NamedStyles` have been replaced by `MarkNamedStyles`;

 - `Autosize`, `Background`, `CountTitle`, `FieldTitle`, `Legend`,
   `NumberFormat`, `Padding`, `Projection`, `Range`, `Scale`,
   `TimeFormat`, and `View` constructors have been replaced by
   `AutosizeStyle`, `BackgroundStyle`, `CountTitleStyle`,
   `FieldTitleStyle`, `LegendStyle`, `NUmberFormatStyle`,
   `PaddingStyle`, `ProjectionStyle`, `RangeStyle`, `ScaleStyle`,
   `TimeFormatStyle`, `ViewStyle` respectively.

## 0.5.0.0

Update to version 4.0 of the Vega-Lite specification (tested against
version 4.0.2). There are several changes in default behavior due
to this (tooltips are now disabled by default, the background now defaults
to white rather than transparent), although this is also controlled by
how the Vega-Lite visualization is rendered, which can make tracking
down why something has changed a bit awkward.

There is more-extensive use of type aliases, such as `Color`,
and the introduction of several more (e.g. `DashStyle` and `FieldName`).
These do not add any type safety, but help the documentation (as they provide
a single place to explain the meaning and any constraints on a
particular value). There are some changes that do improve type safety,
discussed in the "Breaking changes" section below.

Documentation improvements, including a new section in the
tutorial on choropleths contributed by Adam Conner-Sax, and
plots using an Aitoff projection contributed by Jo Wood.

### New functionality

Title (and subtitle) strings can now be split across multiple lines:
use '\n' to indicate a line break.

Colors are now stripped of extraneous white space, and if empty
converted to the JSON null value rather than an empty string.

The `pivot` transform has been added, along with the `PivotProperty`
preferences type. This is the inverse of `fold`.

The `density` transform has been added, along with the `DensityProperty`
configuration type, to support kernel density estimation (e.g. to
generate a continuous distribtion from a discrete one).

The `loess` transform has been added, along with the `LoessProperty`
configuration type, to support scatterplot smoothing.

The `regression` transform has been added, along with the `RegressionProperty`
and `RegressionMethod` configuration types, to support regression
analysis.

The `quantile` transform has been added, along with the `QuantileProperty`
type, to support quantile analysis.

The `url` encoding has been added, which allows you to view images (e.g.
PNG) via the (new) `Image` mark type. The `MAspect` `MarkProperty` has
been added as a configuration option.

The `lookupSelection` transform has been added to support joining
data via a selection. The `SelectionLabel` type alias has been added
to help the documentation.

The `heightOfContainer` and `widthOfContainer` functions
have been added to support responsive sizing, although I have not had
much success in getting them to work!

The `tooltip` encoding will now turn off tooltips if given an empty
list (although note that tooltips are now off by default in Vega-Lite 4).

### Breaking changes

The `combineSpecs` function has been removed.

In an attempt to provide some type safety, the `encoding`,
`transform`, `resolve`, `selection`, and `configure` functions now
take specialised types (`EncodingSpec`, `TransformSpec`,
`ResolveSpec`, `SelectSpec`, and `ConfigureSpec` respectively) rather
than the generic `LabelledSpec` type. Simple visualizations should
remain unchanged, but helper functions may need to have their type
signatures updated.

The `lookup` transform has been changed so that the list of fields
stored when the keys match is now specified by the `LookupFields`
type (rather than a list of field names). This supports providing
aliases and handling of default values, and now subsumes the `lookupAs`
encoding, which has been marked as deprecated.

The `RemoveInvalid` constructor has removed from `ConfigurationProperty`,
and has been replaced by the `MRemoveInvalid` constructor of
`MarkProperty`. The `Stack` constructor was removed.

The `SRangeStep` constructor from `ScaleProperty` has been removed.
The `widthStep` and `heightStep` functions should be used instead.

The `ViewWidth` and `ViewHeight` constuctors of `ViewConfig` have
been replaced by `ViewContinuousWidth`, `ViewContinuousHeight`,
`ViewDiscreteWidth`, and `ViewDiscreteHeight` (well, the symbols
remain but have been deprecated for the continous-named versions).

The `SCRangeStep` and `SCTextXRangeStep` constructors of `ScaleConfig`
have been removed. The new `ViewStep` constructor of `ViewConfig` should be
used as a replacement.

The `ShortTimeLabels`, `LeShortTimeLabels`, and `MShortTimeLabels`
constructors - from `AxisConfig`, `LegendConfg`, and `MarkProperty`
respectively - have been removed.

### New constructors

This section does not repeat names mentioned above.

`AxisProperty` has gained the `AxDataCondition` constructor for
marking a subset of axis properties as being conditional on their
position, and the `ConditionalAxisProperty` for defining which
properties (grid, label, and tick) can be used in this way. It has
also gained the `AxLabelExpr` constructor, which allows you to
change the content of axis labels, `AxTickBand` for positioning the
labels for band scales (and the associated `BandAlign` type),
`AxTitleLineHeight` to specify the line height, and `AxTranslateOffset`
for applying a translation offset to the axis group.

`AxisConfig` has gained `TickBand`, `TitleLineHeight`, and
`TranslateOffset`, matching the additions to `AxisProperty`.

The `ViewBackgroundStyle` constructor has been added to `ViewConfig`.

The `TitleConfig` type gained the following constructors:
`TAlign`, `TdX`, `TdY`, `TLineHeight`, `TSubtitle`, `TSubtitleColor`,
`TSubtitleFont`, `TSubtitleFontSize`, `TSubtitleFontStyle`,
`TSubtitleFontWeight`, `TSubtitleLineHeight`, and `TSubtitlePadding`.

Added `AFitX` and `AFitY` constructors to the `Autosize` type.

The `SelectionProperty` type has gained the `BindLegend` constructor,
and the associated `BindLegendProperty` type, to allow selection of
a legend (categorical data only).

The `TextChannel` type has gained the `TString` constructor, which
lets you specify the text content as a literal.

Two new projections - `EqualEarth` and `NaturalEarth1` - have been
added to the `Projection` type.

Support for color gradients has been added for marks via the
`MColorGradient`, `MFillGradient`, and `MStrokeGradient` constructors of
`MarkProperty`, along with the new `ColorGradient` and
`GradientProperty` types for defining the appearance of the
gradient. The `GradientCoord` and `GradientStops` type aliases
have also been added (although they provides no type safety).

The `MCornerRadius`, `MCornerRadiusTL`, `MCornerRadiusTR`, `MCornerRadiusBL`,
and `MCornerRadiusBR` constructors have been added to `MarkProperty` to
set the corner radii of rectangular marks.

The `MDir`, `MEllipsis`, and `MLimit` constructors have been added to
`MarkProperty` to control how text is truncated. The `TextDirection` type
has been added for use with `MDir`.

The `MarkProperty` type has gained `MLineBreak` and `MLineHeight` constructors
for controlling how multi-line labels are displayed. Note that `hvega` will
always split on the newline character (`\n`), which will over-ride the
`MLineBreak` setting.

The `DTMonthNum` and `DTDayNum` constructors have been added to `DateTime`.

The `BinProperty` type has gained the `SelectionExtent`
constructor, for defining a bin range via an interval selection.

The `PositionChannel` type has gained the `PBand` constructor,
for defining the size of a mark relative to a band, and `MarkProperty`
has added `MTimeUnitBand` and `MTimeUnitBandPosition`.

### Bug fixes

The selection property `SInitInterval Nothing Nothing` is now a
no-op (as it does nothing), rather than generating invalid JSON.

The following options or symbols generated incorrect JSON output:
`ONone`, `LSymbolStrokeWidth`, `LeLabelOpacity`.

## 0.4.1.2

Documentation fix (rendering of a URL), provided by Alexey Kuleshevich
(lehins). The tests should now build without warning in GHC 8.8.1 (CPP
to the rescue again!).

## 0.4.1.1

Avoid a build warning about importing <> from Data.Monoid in GHC 8.8.1
by using the CPP sledge-hammer. Relaxed the bounds on containers so
that the tests can be built on stack LTS 9.21 (GHC 8.0.2).

## 0.4.1.0

Updated the tutorial, adding in a new plot to introduce the addition
of a second axis to the visualization (`parallaxBreakdown`).

## 0.4.0.0

Thanks to (in no order): Matthew Pickering (mpickering),
Adam Conner-Sax (adamConnerSax), and Jo Wood (jwoLondon).

This is a large release, in that it greatly-improves the functionality
of hvega (more-closely aligning it with version 3.4.0 of the Vega-Lite
specification), but does provide a number of **breaking changes** (a
number of functions and constructors have either been removed or had
some combination of being renamed, argument types have changed, or the
number of arguments has been changed). The documentation has also
seen a number of additions and improvements.

A large number of functions, data types, and constructors for data
types have been added, based on version 1.12.0 (and the development
version of the next release) of the elm-vegalite module. Thanks to Jo
Wood for doing all the work! The Haddock documentation indicates new
symbols with the `since 0.4.0.0` label.

### Bug fixes

Corrected the serialization of the `datasets` function, reported by
Matthew Pickering as [issue
29](https://github.com/DougBurke/hvega/issues/17).

Improved the output to better-match the Vega Lite 3.4.0 specification.
Note that hvega does not guarantee that it always creates valid output,
in part because this would complicate the API, but also because the
Vega-Lite specification is changing (e.g. I reported several issues with
version 3.3.0 of the specification during development of this release,
some of which have been addressed in the 3.4.0 version).

### New functions, symbols, and types

The error-related types and functions discussed below are based on
changes provided by Adam Conner-Sax.

`toVegaLiteSchema` has been added to allow you to specify a different
Vega-Lite schema. `toVegaLite` uses version 3 but version 4 is being
worked on as I type this. The `vlSchema` function has been added,
along with `vlSchema4`, `vlSchema3`, and `vlSchema2` values. The
`toHtmlWith` and `toHtmlFileWith` functions have been added to support
more control over the embedding of the Vega-Lite visualizations, and
the versions of the required Javascript libraries used by the
`toHtmlXXX` routines has been updated.

The `VLProperty` type now exports its constructors, to support users
who may need to tweak or augment the JSON Vega-Lite specification
created by `hvega` (see [issue
17](https://github.com/DougBurke/hvega/issues/17)). It has also gained
several new constructors and associated functions, which are given in
brackets after the constructor: `VLAlign` (`align`); `VLBounds`
(`bounds`); `VLCenter` (`center`, `centerRC`); `VLColumns`
(`columns`); `VLConcat` (`vlConcat`); `VLSpacing` (`alignRC`,
`spacing`, `spacingRC`); `VLUserMetadata` (`usermetadata`); and
`VLViewBackground` (`viewBackground`). It is expected that you will be
using the functions rather the constructors!

The `ZIndex` type has been added: this provides constructors for the
common options - `ZFront` and `ZBack` - and a fall-through (`ZValue`)
as a protection against future changes to the Vega-Lite specification.

Four new type aliases have been added: `Angle`, `Color`, `Opacity`,
and `ZIndex`. These do not provide any new functionality but do
document intent.

The `noData` function has been added to let compositions define the
source of the data (whether it is from the parent or not), and data
sources can be named with `dataName`. Data can be created with
`dataSequence`, `dataSequenceAs`, and `sphere`. Graticules can be
created with `graticule`.  The `NullValue` type has been added to
`DataValue` to support data sources that are missing elements, but for
more-complex cases it is suggested that you create your data as an
Aeson Value and then use `dataFromJson`. Support for data imputation
(creating new values based on existing data) has been added, as
discussed below.

The alignment, size, and composition of plots can be defined and
changed with `align`, `alignRC`, `bounds`, `center`, `centerRC`,
`columns`, `spacing`, and `spacingRC`.

Plots can be combined and arranged with: `facet`, `facetFlow`,
`repeat`, `repeatFlow`, and `vlConcat`

New functions for use in a `transform`: `flatten`, `flattenAs`,
`fold`, `foldAs`, `impute`, and `stack`.

New functions for use with `encoding`: `fillOpacity`, `strokeOpacity`,
`strokeWidth`,

The ability to arrange specifications has added the "flow" option
(aka "repeat"). This is seen in the addition of the `Flow` constructor
to the `Arrangement` type - which is used with `ByRepeatOp`,
`HRepeat`, `MRepeat`, `ORepeat`, `PRepeat`, and `TRepeat`.

The `Mark` type has gained `Boxplot`, `ErrorBar`, `ErrorBand`, and
`Trail` constructors. The `MarkProperty` type has gained `MBorders`,
`MBox`, `MExtent`, `MHeight`, `MHRef`, `MLine`, `MMedian`, `MOrder`,
`MOutliers`, `MNoOutliers`, `MPoint`, `MRule`, `MStrokeCap`, `MStrokeJoin`,
`MStrokeMiterLimit`, `MTicks`, `MTooltip`, `MWidth`, `MX`, `MX2`,
`MXOffset`, `MX2Offset`, `MY`, `MY2`, `MYOffset`, and `MY2Offset`
constructors.

The `Position` type has added `XError`, `XError2`, `YError`, and
`YError2` constructors.

The `MarkErrorExtent` type was added.

The `BooleanOp` type has gained the `FilterOp` and `FilterOpTrans`
constructors which lets you use `Filter` expressions as part of a
boolean operation. The `Filter` type has also gained expresiveness,
with the `FLessThan`, `FLessThanEq`, `FGreaterThan`, `FGreaterThanEq`,
and `FValid`.

The `Format` type has gained the `DSV` constructor, which allow you
to specify the separator character for column data.

The MarkChannel type has been expanded to include: `MBinned`, `MSort`,
`MTitle`, and `MNoTitle`. The PositionChannel type has added
`PHeight`, `PWidth`, `PNumber`, `PBinned`, `PImpute`, `PTitle`, and
`PNoTitle` constructors.

The LineMarker and PointMarker types have been added for use with
`MLine` and `MPoint` respectively (both from `MarkProperty`).

The ability to define the binning property with
`binAs`, `DBin`, `FBin`, `HBin`, `MBin`, `OBin`, `PBin`, and `TBin` has
been expanded by adding the `AlreadyBinned` and `BinAnchor`
constructors to `BinProperty`, as well as changing the `Divide`
constructor (as described below).

The `StrokeCap` and `StrokeJoin` types has been added. These are used
with `MStrokeCap`, `VBStrokeCap`, and `ViewStrokeCap` and
`MStrokeJoin`, `VBStrokeJoin`, and `ViewStrokeJoin` respectively.

The `StackProperty` constructor has been added with the `StOffset`
and `StSort` constructors. As discussed below this is a breaking change
since the old StackProperty type has been renamed to `StackOffset`.

The `ScaleProperty` type has seen significant enhancement, by adding
the constructors: `SAlign`, `SBase`, `SBins`, `SConstant` and
`SExponent`.  THe `Scale` tye has added `ScSymLog` `ScQuantile`,
`ScQuantize`, and `ScThreshold`.

The `SortProperty` type has new constructors: `CustomSort`,
`ByRepeatOp`, `ByFieldOp`, and `ByChannel`. See the breaking-changes
section below for the constructors that were removed.

The `AxisProperty` type has seen significant additions, including:
`AxBandPosition`, `AxDomainColor`, `AxDomainDash`,
`AxDomainDashOffset`, `AxDomainOpacity`, `AxDomainWidth`,
`AxFormatAsNum`, `AxFormatAsTemporal`, `AxGridColor`, `AxGridDash`,
`AxGridDashOffset`, `AxGridOpacity`, `AxGridWidth`, `AxLabelAlign`,
`AxLabelBaseline`, `AxLabelNoBound`, `AxLabelBound`,
`AxLabelBoundValue`, `AxLabelColor`, `AxLabelNoFlush`, `AxLabelFlush`,
`AxLabelFlushValue`, `AxLabelFlushOffset`, `AxLabelFont`,
`AxLabelFontSize`, `AxLabelFontStyle`, `AxLabelFontWeight`,
`AxLabelLimit`, `AxLabelOpacity`, `AxLabelSeparation`, `AxTickColor`,
`AxTickDash`, `AxTickDashOffset`, `AxTickExtra`, `AxTickMinStep`,
`AxTickOffset`, `AxTickOpacity`, `AxTickRound`, `AxTickWidth`,
`AxNoTitle`, `AxTitleAnchor`, `AxTitleBaseline`, `AxTitleColor`,
`AxTitleFont`, `AxTitleFontSize`, `AxTitleFontStyle`,
`AxTitleFontWeight`, `AxTitleLimit`, `AxTitleOpacity`, `AxTitleX`, and
`AxTitleY`.

The `AxisConfig` has seen a similar enhancement, and looks similar
to the above apart from the constructors do not start with 'Ax'.

The `LegendConfig` type has been significantly expanded and, as
discussed in the Breaking Changes section, changed. It has gained:
`LeClipHeight`, `LeColumnPadding`, `LeColumns`, `LeGradientDirection`,
`LeGradientHorizontalMaxLength`, `LeGradientHorizontalMinLength`,
`LeGradientLength`, `LeGradientOpacity`, `LeGradientThickness`,
`LeGradientVerticalMaxLength`, `LeGradientVerticalMinLength`,
`LeGridAlign`, `LeLabelFontStyle`, `LeLabelFontWeight`,
`LeLabelOpacity`, `LeLabelOverlap`, `LeLabelPadding`,
`LeLabelSeparation`, `LeLayout`, `LeLeX`, `LeLeY`, `LeRowPadding`,
`LeSymbolBaseFillColor`, `LeSymbolBaseStrokeColor`, `LeSymbolDash`,
`LeSymbolDashOffset`, `LeSymbolDirection`, `LeSymbolFillColor`,
`LeSymbolOffset`, `LeSymbolOpacity`, `LeSymbolStrokeColor`, `LeTitle`,
`LeNoTitle`, `LeTitleAnchor`, `LeTitleFontStyle`, `LeTitleOpacity`,
and `LeTitleOrient`.

The `LegendOrientation` type has gained `LOTop` and `LOBottom`.

The `LegendLayout` and `BaseLegendLayout` types are new, and used
with `LeLayout` to define the legent orient group.

The `LegendProperty` type gained: `LClipHeight`, `LColumnPadding`,
`LColumns`, `LCornerRadius`, `LDirection`, `LFillColor`,
`LFormatAsNum`, `LFormatAsTemporal`, `LGradientLength`,
`LGradientOpacity`, `LGradientStrokeColor`, `LGradientStrokeWidth`,
`LGradientThickness`, `LGridAlign`, `LLabelAlign`, `LLabelBaseline`,
`LLabelColor`, `LLabelFont`, `LLabelFontSize`, `LLabelFontStyle`,
`LLabelFontWeight`, `LLabelLimit`, `LLabelOffset`, `LLabelOpacity`,
`LLabelOverlap`, `LLabelPadding`, `LLabelSeparation`, `LRowPadding`,
`LStrokeColor`, `LSymbolDash`, `LSymbolDashOffset`,
`LSymbolFillColor`, `LSymbolOffset`, `LSymbolOpacity`, `LSymbolSize`,
`LSymbolStrokeColor`, `LSymbolStrokeWidth`, `LSymbolType`,
`LTickMinStep`, `LNoTitle`, `LTitleAlign`, `LTitleAnchor`,
`LTitleBaseline`, `LTitleColor`, `LTitleFont`, `LTitleFontSize`,
`LTitleFontStyle`, `LTitleFontWeight`, `LTitleLimit`, `LTitleOpacity`,
`LTitleOrient`, `LTitlePadding`, `LeX`, and `LeY`.

`Projection` has gained the `Identity` constructor. The
`ProjectionProperty` type has gained `PrScale`, `PrTranslate`,
`PrReflectX`, and `PrReflectY`. The `GraticuleProperty` type was
added to configure the appearance of graticules created with
`graticule`.

The `CompositionAlignment` type was added and is used with `align`,
`alignRC`, `LeGridAlign`, and `LGridAlign`.

The `Bounds` type was added for use with `bounds`.

The `ImputeProperty` and `ImMethod` types were added for use with
`impute` and `PImpute`.

The `ScaleConfig` type has gained `SCBarBandPaddingInner`,
`SCBarBandPaddingOuter`, `SCRectBandPaddingInner`, and
`SCRectBandPaddingOuter`.

The `SelectionProperty` type has gained `Clear`, `SInit`, and
`SInitInterval`.

The Channel type has gained: `ChLongitude`, `ChLongitude2`,
`ChLatitude`, `ChLatitude2`, `ChFill`, `ChFillOpacity`, `ChHref`,
`ChKey`, `ChStroke`, `ChStrokeOpacity`.  `ChStrokeWidth`, `ChText`,
and `ChTooltip`.

The `TitleConfig` type has gained: `TFontStyle`, `TFrame`, `TStyle`,
and `TZIndex`.

The `TitleFrame` type is new and used with `TFrame` from `TitleConfig`.

The `ViewBackground` type is new and used with `viewBackground`.

The `ViewConfig` type has gained `ViewCornerRadius`, `ViewOpacity`,
`ViewStrokeCap`, `ViewStrokeJoin`, and `ViewStrokeMiterLimit`.

The `ConfigurationProperty` type, used with `configuration`, has
gained `ConcatStyle`, `FacetStyle`, `GeoshapeStyle`, `HeaderStyle`,
`NamedStyles`, and `TrailStyle` constructors.

The `ConcatConfig` type was added for use with the `ConcatStyle`,
and the `FacetConfig` type for the `FacetStyle`
configuration settings.

The `HeaderProperty` type has gained: `HFormatAsNum`,
`HFormatAsTemporal`, `HNoTitle`, `HLabelAlign`, `HLabelAnchor`,
`HLabelAngle`, `HLabelColor`, `HLabelFont`, `HLabelFontSize`,
`HLabelLimit`, `HLabelOrient`, `HLabelPadding`, `HTitleAlign`,
`HTitleAnchor`, `HTitleAngle`, `HTitleBaseline`, `HTitleColor`,
`HTitleFont`, `HTitleFontSize`, `HTitleFontWeight`, `HTitleLimit`,
`HTitleOrient`, and `HTitlePadding`.

The `HyperlinkChannel` type has gained `HBinned`.

The `FacetChannel` type has gained `FSort`, `FTitle`, and `FNoTitle`.

The `TextChannel` type has gained `TBinned`, `TFormatAsNum`,
`TFormatAsTemporal`, `TTitle`, and `TNoTitle`.

The `TooltipContent` type was added, for use with `MTooltip`.

The `Symbol` type has gained: `SymArrow`, `SymStroke`, `SymTriangle`,
`SymTriangleLeft`, `SymTriangleRight`, and `SymWedge`.

### Breaking Changes

Some of these are repeated from above.

The `title` function now takes a second argument, a list of `TitleConfig`
values for configuring the appearance of the title.

The `SReverse` construtor was removed from `ScaleProperty` as it
represented a Vega, rather than Vega-Lite, property. The `xSort`
constructors are used to change the order of an item (e.g. `PSort`,
`MSort`).

The `ScSequential` constructor was removed from `Scale` as
`ScLinear` should be used.

The `SortProperty` type has had a number of changes: the `Op`,
`ByField`, and `ByRepeat` constructors have been removed, and
`ByRepeatOp`, `ByFieldOp`, and `ByChannel` constructors have been
added.

The `AxTitleMaxLength` and `TitleMaxLength` constructors have been
removed (from `AxisProperty` and `AxisConfig` respectively) as they
are invalid. The `AxTitleLimit` (new in this release) and
`TitleLimit` constructors should be used instead.

`AxisProperty`: the `AxValues` constructor has been changed from
accepting a list of doubles to `DataValues`. The `AxDates`
constructor has been deprecated and `AxValues` should be used
instead.

There have been significant changes to the `LegendConfig` type: the
`EntryPadding`, `GradientHeight`, `GradientLabelBaseline`,
`GradientWIdth`, and `SymbolColor` constructors have been removed;
the renaming constructors have been renamed so they all begin with
`Le` (e.g. `Orient` is now `LeOrient`, and `Orient` has been added
to `AxisConfig`); and new constructors have been added.

The `StackProperty` type has been renamed to `StackOffset` and its
constructors have changed, and a new `StackProperty`
type has been added (that references the `StackOffset` type).

The `Average` constructor of `Operation` was removed, and `Mean`
should be used instead.

The `LEntryPadding` constructor of `LegendProperty` was removed.

The arguments to the `MDataCondition`, `TDataCondition`, and
`HDataCondition` constructors - of `MarkChannel`, `TextChannel`,
and `HyperlinkChannel` respectively - have changed to support
accepting multiple expressions.

The `MarkOrientation` type has been renamed `Orientation`.

The constructors of the `ViewConfig` type have been renamed so they
all begin with `View` (to match `ViewWidth` and `ViewHeight`).

The constructors of the `ProjectionProperty` type have been renamed
so that they begin with `Pr` rather than `P` (to avoid conflicts
with the `PositionChannel` type).

The `Divide` constructor of `BinProperty` now takes a list of
Doubles rather than two.

The `TitleConfig` type has gained the following constructors:
`TFontStyle`, `TFrame`, `TStyle`, and `TZIndex`. The `TitleFrame`
type was added for use with `TFrame`.

The `ArgMax` and `ArgMin` constructors of `Operation` now take an
optional field name, to allow them to be used as part of an encoding
aggregation (e.g. with `PAggregate`).

The "z index" value has changed from an `Int` to the `ZIndex` type.

The constructors for the `Symbol` type now all start with `Sym`, so
`Cross`, `Diamond`, `TriangleUp`, `TriangleDown`, and `Path` have been
renamed to `SymCross`, `SymDiamond`, `SymTriangleUp`,
`SymTriangleDown`, and `SymPath`, respectively.

The `Legend` type has been renamed `LegendType` and its constructors
have been renamed `GradientLegend` and `SymbolLegend`.

### Improved testing

Added a test suite based on the Elm Vega-Lite tests (based entirely on
the work of Jo Wood).

The IPython notebooks have been expanded to cover recent changes in the
[Vega-Lite gallery](https://vega.github.io/vega-lite/examples/), and
include validation of the output (to check against the expected output).

## 0.3.0.1

The minimum base version has been bumped from 4.7 to 4.9, which
means ghc 8.0 or later. This is because the 0.2.0.1 release
does not appear to build with ghc 7.10. If this restriction is
a problem then please comment on the issues list.

There have been minor documentation updates, adding `@since`
annotations.

## 0.3.0.0

The `Channel` type has been extended to include `ChFill` and `ChStroke`
constructors and the `tooltips` function allows you to provide
multiple tooltips for a channel. The schema version has been changed
from 2 to 3, but there has been limited checking to see if the API
correctly reflects the new schema.

This functionality was provided by Adam Massmann (massma) and
BinderDavid.

## 0.2.1.0

Added the `toHtml` and `toHtmlFile` functions which create the necessary
HTML to view the  Vega-Lite visualization using
[Vega Embed](https://vega.github.io/vega-lite/usage/embed.html)
(this is similar to how
[ihaskell-hvega](https://hackage.haskell.org/package/ihaskell-hvega)
works).

This functionality was provided by Gregory Schwartz; apologies for
taking so long to get it released.

## 0.2.0.0

The constructors for the LegendOrientation type have been renamed (by
adding the prefix `LO`) which avoids the name clash with `Left` and `Right`
seen in earlier releases. This is a breaking API change.

Clarify how to use the library in the main `hvega` module.

Thanks to contributions from Nicolas Mattia (nmattia) and Marco Zocca (ocramz).

## 0.1.0.3

The only change is to the cabal file, where `cabal-version: >=1.18` has
been changed to `cabal-version: 1.18`.

## 0.1.0.2

Updated the upper bounds of `aeson` to really allow v1.4. This was not
released on Hackage.

## 0.1.0.1

Updated the upper bounds of `aeson` from v1.2 to v1.4.

The cabal package now includes `stack.yaml` and `default.nix` (although
the latter is *not* guaranteed to be correct), as well as a
change log and an extra image.

## 0.1.0.0

This is the initial version of `hvega`, which is based on
version 2.2.1 of the
[Elm Vega library](http://package.elm-lang.org/packages/gicentre/elm-vega/2.2.1/VegaLite).
