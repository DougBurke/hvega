For the latest version of this document, please see
[https://github.com/DougBurke/hvega/blob/master/hvega/CHANGELOG.md](https://github.com/DougBurke/hvega/blob/master/hvega/CHANGELOG.md).

## 0.4.0.0

Corrected the serialization of the `datasets` function (#20), reported
by Matthew Pickering (mpickering).

Corrected the serialization of `Function` to match the Vega Lite 3.3.0
specification.

Added the `MarkErrorExtent` type, to indicate the extent of the rule
used for error bars and added the `ErrorBar` and `ErrorBand` marks.
The `MarkProperty` type has gained `MBorders` and `MExtent` constructors,
and the `Position` type has added `XError`, `XError2`, `YError`, and
`YError2` constructors as part of this change.

This functionality was provided by Adam Conner-Sax (adamConnerSax).

A large number of functions, data types, and constructors for data
types have been added, based on version 1.12.0 of the elm-vegalite
module. Thanks again to Jo Wood for doing all the work! The documentation
indicates new symbols with the `since 0.4.0.0` label.

The `VLProperty` type now exports its constructors, to support users
who may need to tweak or augment the JSON Vega-Lite specification
created by `hvega`.

Added a test suite based on the Elm Vega-Lite tests (again, thanks to
Jo Wood).

### Breaking Changes

The `SReverse` construtor was removed from `ScaleProperty` as it
represented a Vega, rather than Vega-Lite, property. The `PSort`
constructor is used to change the order of an axis.

The `ScSequential` constructor was removed from `Scale` as
`ScLinear` should be used.

The `AxTitleMaxLength` and `TitleMaxLength` constructors have been
removed (from `AxisProperty` and `AxisConfig` respectively) as they
are invalid. The `AxTitleLimit` (new in this release) and
`TitleLimit` constructors should be used instead.

There have been a number of changes to the `LegendConfig` type: the
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

The `Divide` constructor of `BinProperty` now takes a list of
Doubles rather than two.

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
