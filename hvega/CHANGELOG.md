For the latest version of this document, please see
[https://github.com/DougBurke/hvega/blob/master/hvega/CHANGELOG.md](https://github.com/DougBurke/hvega/blob/master/hvega/CHANGELOG.md).

## 0.6.0.0

Update to version 4.1 of the Vega-Lite specification (tested against
version 4.1.1).

The `MarkProperty` type has gained the `MCornerRadiusEnd` constructor.

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

The "z index" value has changed from an 'Int' to the 'ZIndex' type.

The constructors for the `Symbol` type now all start with `Sym`, so
`Cross`, `Diamond`, `TriangleUp`, `TriangleDown`, and `Path` have been
renamed to `SymCross`, `SymDiamond`, `SymTriangleUp`,
`SymTriangleDown`, and `SymPath`, respectively.

The `Legend` type has been renamed `LegendType` and its constructors
have been renamed 'GradientLegend' and 'SymbolLegend'.

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
