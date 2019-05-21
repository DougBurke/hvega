# hvega

Support the creation of [Vega-Lite](https://vega.github.io/vega-lite/) visualizations
in Haskell. This code is released under the BSD3 license.

It is an almost-direct copy of version 2.2.1 of the
[Elm Vega library](http://package.elm-lang.org/packages/gicentre/elm-vega/2.2.1/VegaLite),
which is released under a BSD3 license by Jo Wood of the giCentre at the
City University of London.

## Example

```Haskell
let cars =  dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []

    enc = encoding
            . position X [ PName "Horsepower", PmType Quantitative ]
            . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
            . color [ MName "Origin", MmType Nominal ]

    bkg = background "rgba(0, 0, 0, 0.05)"

in toVegaLite [ bkg, cars, mark Circle [], enc [] ]
```

When viewed with a Vega-Lite aware viewer, the resultant plot is

![Simple scatterplot](https://raw.githubusercontent.com/DougBurke/hvega/master/hvega/images/intro.png "Simple scatterplot")

## Documentation

The [Elm Vega documentation](http://package.elm-lang.org/packages/gicentre/elm-vega/2.2.1)
can be used as a guide to using this module. The
[Vega-Lite Example Gallery](https://vega.github.io/vega-lite/examples/) has
been converted to an
[IHaskell notebook](https://github.com/DougBurke/hvega/blob/master/notebooks/VegaLiteGallery.ipynb)
Uunfortunately the plots created by VegaEmbed **do not appear**
in the notebook when viewed with either GitHub's viewer or
[ipynb viewer](http://nbviewer.jupyter.org/github/DougBurke/hvega/blob/master/notebooks/VegaLiteGallery.ipynb),
but things seem much better when using Jupyter Lab (rather than
notebook) to create the notebooks (since Vega is natively
supported in this environment). The notebooks have been re-created
using Jupyter Lab (thanks to Tweag I/O's
[JupyterWith environment](https://www.tweag.io/posts/2019-02-28-jupyter-with.html)), which should make the plots appear on GitHub (you may need
to reload the notebooks as I find they don't display on the
first try).

The [notebooks directory](https://github.com/DougBurke/hvega/tree/master/notebooks)
contains a poorly-curated set of examples and experiments with hvega.

## Differences to Elm Vega

The main changes to version 2.2.1 of
[Elm Vega](http://package.elm-lang.org/packages/gicentre/elm-vega/2.2.1)
are:

- Replace `Spec` by `VLSpec` (although both are synonyms for the underlying
  JSON representation).

- Add a type for the output of `toVegaLite` (`VegaLite`) that is separate from
  `VLSpec`, which is usefull for integration with IHaskell. The JSON
  specification is retrieved with `fromVL`.

- Take advantage of the lack of backwards compatibality requirements to remove or
  replace several symbols (such as add the `Utc` constructor to `TimeUnit`, remove the `bin`
  function, and use `Data` rather than `(VLProperty, VLSpec)` in function
  signatures).

- In version 0.2.0.0, the constructors for the `LegendOrientation` type
  have gained a `LO` prefix, which avoids clashing with the Prelude's
  `Either` type.
