For the latest version of this document, please see
[https://github.com/DougBurke/hvega/blob/master/hvega/CHANGELOG.md](https://github.com/DougBurke/hvega/blob/master/hvega/CHANGELOG.md).

## 0.4.0.0

Thanks to (in no order): Matthew Pickering (mpickering),
Adam Conner-Sax (adamConnerSax), and Jo Wood (jwoLondon).

This is a large release, in that it greatly-improves the functionality
of hvega (more-closely aligning it with version 3.3.0 of the Vega-Lite
specification), but does provide a number of **breaking changes** (a
number of functions and constructors have either been removed or had
some combination of being renamed, argument types have changed, or the
number of arguments has been changed). The documentation has also
seen a number of additions and improvements.

### Added functionality

A large number of functions, data types, and constructors for data
types have been added, based on version 1.12.0 (and the development
version of the next release) of the elm-vegalite module. Thanks to Jo
Wood for doing all the work! The documentation indicates new symbols
with the `since 0.4.0.0` label. Some of the changes are listed below,
and in the 'Breaking Changes' section below.

The `Mark` type has gained `Boxplot`, `ErrorBar`, `ErrorBand`, and
`Trail` constructors. The `MarkProperty` type has gained `MBorders`,
`MBox`, `MExtent`, `MHRef`, `MLine`, `MMedian`, `MOrder`, `MOutliers`,
`MPoint`, `MRule`, `MStrokeCap`, `MStrokeJoin`, `MStrokeMiterLimit`,
`MTicks`, `MTooltip`, `MX`, `MY`, `MX2`, `MY2`, `MXOffset`,
`MYOffset`, `MX2Offset`, and `MY2Offset` constructors. The `Position`
type has added `XError`, `XError2`, `YError`, and `YError2`
constructors. The `MarkErrorExtent` type was added.  Some of these
changes were provided by Adam Conner-Sax.

The `BooleanOp` type has gained the `FilterOp` and `FilterOpTrans`
constructors which lets you use `Filter` expressions as part of a
boolean operation.

The `VLProperty` type now exports its constructors, to support users
who may need to tweak or augment the JSON Vega-Lite specification
created by `hvega` (see [issue
17](https://github.com/DougBurke/hvega/issues/17)).

### Improved testing

Added a test suite based on the Elm Vega-Lite tests (based entirely on
the work of Jo Wood).

The IPython notebooks have been expanded to cover recent changes in the
[Vega-Lite gallery](https://vega.github.io/vega-lite/examples/), and
include validation of the output (to check against the expected output).

### Bug fixes

Corrected the serialization of the `datasets` function, reported by
Matthew Pickering as [issue
29](https://github.com/DougBurke/hvega/issues/17).

Improved the output to better-match the Vega Lite 3.3.0 specification.
Note that hvega does not guarantee that it always creates valid output,
in part because this would complicate the API, but also because the
Vega-Lite specification is changing (e.g. I reported several issues with
specification during development of this release).

### New functions, symbols, and types

`toVegaLiteSchema` has been added to allow you to specify a
different Vega-Lite schema. `toVegaLite` uses version 3 but
version 4 is being worked on as I type this. The `vlSchema`
function has been added, along with `vlSchema4`, `vlSchema3`,
and `vlSchema2` values.

### Breaking Changes

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

The `title` function now takes a second argument, a list of `TitleConfig`
values for configuring the appearance of the title.

The `ArgMax` and `ArgMin` constructors of `Operation` now take an
optional field name, to allow them to be used as part of an encoding
aggregation (e.g. with `PAggregate`).

The `ZIndex` type has been added: this provides constructors for the
common options - `ZFront` and `ZBack` - and a fall-through (`ZValue`)
as a protection against future changes to the Vega-Lite specification.

Three new type aliases have been added: `Angle`, `Color`, and
`Opacity`. These do not provide any new functionality - other than
documentation - but may clash with symbols defined in other modules.

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
