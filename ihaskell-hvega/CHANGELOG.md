For the latest version of this document, please see
[https://github.com/DougBurke/hvega/blob/master/ihaskell-hvega/CHANGELOG.md](https://github.com/DougBurke/hvega/blob/master/ihaskell-hvega/CHANGELOG.md).

## 0.5.0.2

Improve links in the haddock documentation. There are no code changes.

## 0.5.0.1

Support text 2.0.

## 0.5.0.0

Bump minimum supported IHaskell version to 0.10.

## 0.4.0.0

Bump to support version hvega-0.12.0.0 and version 2.0 of aeson,
although it will need updates to IHaskell components such as
ipython-kernel before you can combine all the pieces.

## 0.3.2.0

Bump to support version hvega-0.11.0.0.

## 0.3.1.0

Bump to support version hvega-0.10.0.0.

## 0.3.0.0

Support for the IHaskell notebook has been updated to version 4 of the
Vega-Lite schema. Finally.

Support for the Jupyter Lab interface remains stuck in a maze of
very-twisty paths...

## 0.2.2.0 - 0.2.6.0

Bump the maximum version of hvega.

## 0.2.1.0

The module now exports the `VegaLiteLab` type (provided by Alexey
Kuleshevich (lehins). This type is used to support display in Jupyter
Lab as well as notebooks, and is a somewhat experimental feature.

The module now builds without warnings on GHC 8.8.1.

This release has been marked as compatible with version 0.4 of `hvega`,
but **care should be taken** as it is possible to create visualizations
that either do not display correctly, or do not display at all: these
are visualizations that take advantages of Vega-Lite functionality
not supported by the Javascript display code (presumably because
of our use of an old Vega-Lite mimetype version in IHaskell; this
will hopefuly be addressed once a version of IHaskell is released
with support for
[custom mimetypes](https://github.com/gibiansky/IHaskell/issues/1089)).

## 0.2.0.2

Updated the upper bounds of `ihaskell` to allow version 0.10.

## 0.2.0.1

Updated the supported `hvega` range to include version 0.3.

## 0.2.0.0

Added the `vlShow` helper to allow Vega-Lite visualizations to be
viewed directly in Jupyter lab (rather than Jupyter notebook).

Try it out in [Tweag's jupyterWith environment](https://github.com/tweag/jupyterWith).

## 0.1.0.3

The only change is to the cabal file, where `cabal-version: >=1.10` has
been changed to `cabal-version: 1.18`. I honestly don't know what the
minimum-supported version actually is (the version bump is to match
that used by `hvega`.

## 0.1.0.2

Updated the upper bounds of `aeson` to really allow v1.4. This was not
released on Hackage.

## 0.1.0.1

The source code has been moved to the `src` sub-directory to match the
layout used by `hvega`.

Several incorrect links in the cabal file have been fixed (as my GitHub
user name is not actually `githubuser`).

The cabal package now includes `stack.yaml` and `default.nix` (although
the latter is *not* guaranteed to be correct), as well as a
change log.

Since `ihaskell` is now in Stackage, the LTS version has been updated
to reflect this (to LTS 11.19), and the extra dependency removed.

## 0.1.0.0

This is the initial version of `ihaskell-hvega`, and is released with
`hvega` version 0.1.0.0.
