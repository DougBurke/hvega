{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : IHaskell.Display.HvegaLab
Copyright   : (c) Douglas Burke, 2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings

Allow VegaLite visualizations to be displayed directly in Jupyter Lab.
There is also @IHaskell.Display.Hvega@ which is for Jupyter notebooks,
but that is unlikely to see much development as JupyterLab has in-built
support for Vega and Vega-Lite visualizations, which makes this module
a *lot* simpler to maintain.

Jupyter Lab can be installed with nix using the
<https://github.com/tweag/jupyterWith jupyterWith> setup.

== Example

If we have the following Vega-Lite definition:

@
\{\-\# language OverloadedStrings \#\-\}

import Graphics.Vega.VegaLite

vl1 = 'toVegaLite' ['description' desc, 'background' "white", 'dat' [], 'mark' 'Bar' 'barOpts', 'enc' []] where
    desc = "A very exciting bar chart"

    dat = 'dataFromRows' ['Parse' [("start", 'FoDate' "%Y-%m-%d")]]
          . 'dataRow' [("start", 'Str' "2011-03-25"), ("count", 'Number' 23)]
          . dataRow [("start", Str "2011-04-02"), ("count", Number 45)]
          . dataRow [("start", Str "2011-04-12"), ("count", Number 3)]

    barOpts = ['MOpacity' 0.4, 'MColor' "teal"]

    enc = 'encoding'
          . 'position' 'X' ['PName' "start", 'PmType' 'Temporal', 'PAxis' ['AxTitle' "Inception date"]]
          . position Y [PName "count", PmType Quantitative]
@

then it can be displayed automatically in Jupyter Lab by

> vl1

-}

module IHaskell.Display.HvegaLab where

import qualified Data.Text.Lazy as LT

import Data.Aeson.Text (encodeToLazyText)

import Graphics.Vega.VegaLite (VegaLite, fromVL)

import IHaskell.Display (IHaskellDisplay(..), Display(..), vegalite)


-- ^ Display Vega-Lite visualizations in an IHaskell notebook when
--   using Jupyter Lab.
--
--   Use the @IHaskell.Display.Hvega@ module when using IHaskell from
--   a Jupyter notebook.
--
--   Note that local file access is __not__ guaranteed to work - e.g.
--   @dataFromUrl@ where the file name refers to a local file -
--   since the JavaScript @fs@ module may not be loaded.
--
instance IHaskellDisplay VegaLite where
  display vl = let js = LT.unpack (encodeToLazyText (fromVL vl))
               in pure (Display [vegalite js])
  
