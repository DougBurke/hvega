# hvega

Contains the `hvega` module for creating
[Vega-Lite](https://vega.github.io/vega-lite/) visualizations
in Haskell, and the `ihaskell-hvega` module for viewing these
visualizations in
[IHaskell notebooks](https://github.com/gibiansky/IHaskell).

This code is released under the BSD3 license.

The `hvega` package started of as an almost-direct copy of version 2.2.1 of the
[Elm Vega library](http://package.elm-lang.org/packages/gicentre/elm-vega/2.2.1/VegaLite),
which was released under a BSD3 license by Jo Wood of the giCentre at the
City University of London. The two have diverged somewhat since (mainly
that `hvega` still uses data types as its primary control structure
whilst the Elm version has moved to functions).

# How do I create a visualization?

The `hvega` package allows you to create a visualization but this is
just a JSON file - you still need a way to view it. There are a number
of ways:

- view it in the [Vega Editor](https://vega.github.io/editor/)
- embed the file into a HTML page and use the [Vega Embed](https://github.com/vega/vega-embed)
  Javascript library to view it
- the `hvega` package contains helper routines that will create a
  HTML page that includes `Vega Embed` for viewing in a web browser
  (see [toHtmlFile](https://hackage.haskell.org/package/hvega/docs/Graphics-Vega-VegaLite.html#v:toHtml)
  and related routines)
- use a "Vega viewer" - I have a Haskell-based solution at
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
