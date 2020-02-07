# ihaskell-hvega

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

This code is released under the BSD3 license.

# Current status

If your visualization uses anything more modern than version 2 of the Vega-Lite
specification (and possibly even version 2 features) then there is a good chance
that it will not display correctly in IHaskell. This can differ depending on
whether you are using the notebook or lab interface.

For the lab interface I am waiting for support for custom mime-types to be
released (it is in the master branch of IHaskell). For the notebook interface
I gave up on trying to work through the JavaScript issues I came across.
