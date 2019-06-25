For the latest version of this document, please see
[https://github.com/DougBurke/hvega/blob/master/hvega/CHANGELOG.md](https://github.com/DougBurke/hvega/blob/master/hvega/CHANGELOG.md).

## 0.4.0.0

Corrected the serialization of `Function` to match the Vega Lite 3.3.0
specification.

Added the `MarkErrorExtent` type, to indicate the extent of the rule
used for error bars and added the `ErrorBar` and `ErrorBand` marks.
The `MarkProperty` type has gained `MBorders` and `MExtent` constructors,
and the `Position` type has added `XError`, `XError2`, `YError`, and
`YError2` constructors as part of this change.

This functionality was provided by Adam Conner-Sax (adamConnerSax).

The `dataSequence, `dataSequenceAs`, `sphere`, and `graticule` functions
have been added, as well as associated properties (`GraticuleProperty`).
The `Format` type has been extended to support delimeter-separated
values (`DSV`).

The `SReverse` construtor was removed from `ScaleProperty` as it
represented a Vega, rather than Vega-Lite, property. The `PSort`
constructor is used to change the order of an axis.

Added a test suite based on the Elm Vega-Lite tests.

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
