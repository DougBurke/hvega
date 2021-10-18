# hvega

Contains the `hvega` module for creating
[Vega-Lite](https://vega.github.io/vega-lite/) visualizations
in Haskell, and the `ihaskell-hvega` module for viewing these
visualizations in
[IHaskell notebooks](https://github.com/gibiansky/IHaskell),
although circa June 2021 the IHaskell integration is not as seamless
as I'd like.

This code is released under the BSD3 license.

## Package: hvega

See the [hvega README](https://github.com/DougBurke/hvega/tree/main/hvega#readme)
for more details than given here.

[![Hackage](https://img.shields.io/hackage/v/hvega.svg)](https://hackage.haskell.org/package/hvega)
[![vega-lite version](https://img.shields.io/badge/Vega--Lite-v4.15-purple.svg)](https://vega.github.io/vega-lite/)
[![GitHub CI](https://github.com/DougBurke/hvega/workflows/hvega-CI/badge.svg)](https://github.com/DougBurke/hvega/actions)
[![Dependencies status](https://img.shields.io/hackage-deps/v/hvega.svg)](http://packdeps.haskellers.com/feed?needle=hvega)

Create [Vega-Lite](https://vega.github.io/vega-lite/) visualizations in
Haskell. It targets version 4.15 of the Vega-Lite specification. Note that
the module does not include a viewer for these visualizations (which are
JSON files), but does provide several helper functions, such as
[toHtmlFile](https://hackage.haskell.org/package/hvega/docs/Graphics-Vega-VegaLite.html#v:toHtmlFile),
which create HTML that can be viewed
with a browser to display the visualization. Other approaches include
automatic display in IHaskell notebooks - with the
[ihaskell-vega](https://hackage.haskell.org/package/ihaskell-hvega)
package - or use of external viewers such as
[Vega View](https://hackage.haskell.org/package/vega-view) and
[Vega-Desktop](https://github.com/vega/vega-desktop).

The `hvega` package started of as an almost-direct copy of version 2.2.1 of the
[Elm Vega library](http://package.elm-lang.org/packages/gicentre/elm-vega/2.2.1/VegaLite),
which was released under a BSD3 license by Jo Wood of the giCentre at the
City University of London. The two have diverged somewhat since (mainly
that `hvega` still uses data types as its primary control structure
whilst the Elm version has moved to functions).

### Example

The [Vega-Lite example gallery](https://vega.github.io/vega-lite/examples/) contain
a number of visualizations of the `cars.json` dataset, which has a number of
columns to display, such as "Horsepower", "Miles_per_Gallon", and "Origin". The
following code will create a visualization that plots the efficiency of the
cars (the "mpg") as a function of its Horsepower, and color-code by the
origin of the car:

```Haskell
let cars =  dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []

    enc = encoding
            . position X [ PName "Horsepower", PmType Quantitative ]
            . position Y [ PName "Miles_per_Gallon", PmType Quantitative, PTitle "Miles per Gallon" ]
            . color [ MName "Origin" ]

    bkg = background "rgba(0, 0, 0, 0.05)"

in toVegaLite [ bkg, cars, mark Circle [MTooltip TTEncoding], enc [] ]
```

When the JSON is viewed with a Vega-Lite aware viewer, the resultant plot
can be interacted with (e.g. to use the tooltip support) using the
[interactive version](https://vega.github.io/editor/#/url/vega-lite/N4IgtghgTg1iBcoAuB7FAbJBLADg0AxigHZICmpCIFRAJlsQOYgC+ANCEgJ45lUFYoBdH3YhaEJBHwgArlHRUAFkiQ4AzvAD0WgG5lGEAHSMsSJbIBGRrCj0GIAWglT1ZJOq0uIWgtHVGAFbqJKwcACTqBEpkkMqqGtr2hiZmFta2WlExkMlO6GZkegAsQSHEIBw0KPRMMkToKFAyAGZYZOi0VADyUFimFWIAHq3tnVQAEk1uOCgA7mTNHNy8VACOshCkZpJY+mEgXKMdXfAgALJYIuoA+rxQNwDiEOiNFctmIlSX1wAE979nq9QsseHwzhsttgpNh9iwxJYIAQYIwoChZMRTiAoIxEQAKAAMbF+RJJxIJRgJAFYAJSsIA).
It can also be viewed as a PNG:

![Simple scatterplot](https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/images/example-car.png "Simple scatterplot")

## Package: ihaskell-hvega

See the [ihaskell-hvega README](https://github.com/DougBurke/hvega/tree/main/ihaskell-hvega#readme)
for more details than given here.

View [Vega-Lite](https://vega.github.io/vega-lite/) visualizations
created by the `hvega` package in IHaskell notebooks.

When used with Jupyter notebooks it relies on
[Vega-Embed](https://vega.github.io/vega-lite/usage/embed.html) to
do the hard work of parsing and displaying the Vega Lite specification.

If run in a Jupyter Lab then the native Vega support is used for
displaying the Vega Lite specifications. I recommend using
[Tweag I/O's jupyterWith environment](https://github.com/tweag/jupyterWith)
to set this up, and have a rudimentary
[`shell.nix` example](https://github.com/DougBurke/hvega/blob/master/notebooks/shell.nix)
in the
[notebooks directory](https://github.com/DougBurke/hvega/tree/master/notebooks).

# How do I create a visualization?

The `hvega` package allows you to create a visualization but this is
just a JSON file - you still need a way to view it. There are a number
of ways:

- view it in the [Vega Editor](https://vega.github.io/editor/) (although this
  is easiest for small JSON files)
- embed the file into a HTML page and use the [Vega Embed](https://github.com/vega/vega-embed)
  Javascript library to view it
- the `hvega` package contains helper routines that will create a
  HTML page that includes `Vega Embed` for viewing in a web browser
  (see [toHtmlFile](https://hackage.haskell.org/package/hvega/docs/Graphics-Vega-VegaLite.html#v:toHtmlFile)
  and related routines)
- use a "Vega viewer" - I have a Haskell-based version at
  [Vega View](https://hackage.haskell.org/package/vega-view) but there are
  alternatives, such as 
  [vega desktop](https://github.com/vega/vega-desktop).

The [Vega-Lite ecosystem](https://vega.github.io/vega-lite/ecosystem.html)
page contains a miriad useful links.

# Installation

The packages are available on [hackage](https://hackage.haskell.org/):

- [hvega](https://hackage.haskell.org/package/hvega)
- [ihaskell-hvega](https://hackage.haskell.org/package/ihaskell-hvega)

There is a top-level `stack.yaml` which builds both `hvega` and
`ihaskell-hvega` using [Stack](https://docs.haskellstack.org/en/stable/README/).
There is also a `default.nix` file for development with
[Nix](https://nixos.org/nix/) - you can try

```bash
% nix-build
... wait an indeterminite time
% ./result/bin/ihaskell-notebook --notebook-dir notebooks
```

but it (currently) doesn't use the on-disk versions of `hvega` and
`ihaskell-hvega`.  The `notebooks/shell.nix` file contains a version
that uses [Tweag I/O's jupyterWith
environment](https://github.com/tweag/jupyterWith) environment but
that unfortunately seems to be using "old" versions of IHaskell and
ghc.

# Testing

There is basic testing of the output of `hvega`, and the module also
contains a
[tutorial](https://hackage.haskell.org/package/hvega/docs/Graphics-Vega-Tutorials-VegaLite.html)
which contains plots you can view; e.g. with
[Vega View](https://hackage.haskell.org/package/vega-view) or
[Vega-Desktop](https://github.com/vega/vega-desktop).

The [`notebooks/` directory](https://github.com/DougBurke/hvega/tree/master/notebooks) contains a (very small, very random) sample
of notebooks experimenting with `hvega`. I recommend using
[Tweag I/O's jupyterWith environment](https://github.com/tweag/jupyterWith)
to view these. At present there is support for `Jupyter notebook`,
whereas the support for `Jupyter lab` is unfortunately limited (due
to limitations on the current IHaskell environment and differences
between the Jupyter Lab versions).

There is also a
[Data Haskell example notebook](https://github.com/DataHaskell/data-glue/blob/master/tutorials/jlab_hvega.ipynb),
and the Monad-Bayes blog series by tweag.io
which [starts here](https://www.tweag.io/posts/2019-09-20-monad-bayes-1.html).

# Development

I created this as a piece of procrastination, while procrastinating with
a different task. At present I am not using it, due to lack of time. Please
pop on over to
[GitHub](https://github.com/DougBurke/hvega/), or ping me on Twitter
[@doug_burke](https://twitter.com/doug_burke) if you would like to help.
