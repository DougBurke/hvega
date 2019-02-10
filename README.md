# hvega

Contains the `hvega` module for creating
[Vega-Lite](https://vega.github.io/vega-lite/) visualizations
in Haskell, and the `ihaskell-hvega` module for viewing these
visualizations in
[IHaskell notebooks](https://github.com/gibiansky/IHaskell).

This code is released under the BSD3 license.

It is an almost-direct copy of version 2.2.1 of the
[Elm Vega library](http://package.elm-lang.org/packages/gicentre/elm-vega/2.2.1/VegaLite),
which is released under a BSD3 license by Jo Wood of the giCentre at the
City University of London.

# Installation

The packages are available on [hackage](https://hackage.haskell.org/):

- [hvega](https://hackage.haskell.org/package/hvega)
- [ihaskell-hvega](https://hackage.haskell.org/package/ihaskell-hvega)

There is a top-level `stack.yaml` which builds both `hvega` and
`ihaskell-hvega` using [Stack](https://docs.haskellstack.org/en/stable/README/).
There is also a `shell.nix` file for development with
[Nix](https://nixos.org/nix/), although this is not guaranteed to work
as I have not yet fully integrated Nix into my life.

# Testing

This would be nice. The IHaskell notebook
`notebooks/VegaLiteGallery.ipynb` is used as a manual test case, but
automated tests would be ideal.

There is also a
[Data Haskell example notebook](https://github.com/DataHaskell/data-glue/blob/master/tutorials/jlab_hvega.ipynb).

# Development

I created this as a piece of procrastination, while procrastinating with
a different task. At present I am not using it, due to lack of time. Please
pop on over to
[GitHub](https://github.com/DougBurke/hvega/), or ping me on Twitter
[@doug_burke](https://twitter.com/doug_burke) if you would like to help.
