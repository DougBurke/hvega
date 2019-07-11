For the latest version of this document, please see
[https://github.com/DougBurke/hvega/blob/master/ihaskell-hvega/CHANGELOG.md](https://github.com/DougBurke/hvega/blob/master/ihaskell-hvega/CHANGELOG.md).

## 0.2.0.3

Updated the supported `hvega` range to include version 0.4.

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
