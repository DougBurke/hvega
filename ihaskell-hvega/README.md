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

The display should work correctly when using the IHaskell `notebook`
interface, supporting features included in Vega-Lite 4 (this is new to
version 0.3, as earlier versions were limited to features in Vega-Lite
3).

Support for the Jupyter lab interface is **very limited** since there
is a curious combination of IHaskell and Jupyter lab issues (including
whether you are using v1.2 or v2). This is very annoying.
