{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : IHaskell.Display.Hvega
Copyright   : (c) Douglas Burke 2018, 2019, 2020, 2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

Allow VegaLite visualizations to be displayed directly in Jupyter notebooks
or Jupyter Lab. For the moment they are handled separately, in that the Jupyter
Lab version requires use of the `vlShow` routine.

Jupyter Lab can be installed with nix using the
<https://github.com/tweag/jupyterWith jupyterWith> setup.

== Example

If we have the following Vega-Lite definition:

@
\{\-\# language OverloadedStrings \#\-\}

import Graphics.Vega.VegaLite

vl1 = 'Graphics.Vega.VegaLite.toVegaLite' ['Graphics.Vega.VegaLite.description' desc, 'Graphics.Vega.VegaLite.background' "white", dat [], 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Bar' barOpts, enc []] where
    desc = "A very exciting bar chart"

    dat = 'Graphics.Vega.VegaLite.dataFromRows' ['Graphics.Vega.VegaLite.Parse' [("start", 'Graphics.Vega.VegaLite.FoDate' "%Y-%m-%d")]]
          . 'Graphics.Vega.VegaLite.dataRow' [("start", 'Graphics.Vega.VegaLite.Str' "2011-03-25"), ("count", 'Graphics.Vega.VegaLite.Number' 23)]
          . dataRow [("start", Str "2011-04-02"), ("count", Number 45)]
          . dataRow [("start", Str "2011-04-12"), ("count", Number 3)]

    barOpts = ['Graphics.Vega.VegaLite.MOpacity' 0.4, 'Graphics.Vega.VegaLite.MColor' "teal"]

    enc = 'Graphics.Vega.VegaLite.encoding'
          . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.X' ['Graphics.Vega.VegaLite.PName' "start", 'Graphics.Vega.VegaLite.PmType' 'Graphics.Vega.VegaLite.Temporal', 'Graphics.Vega.VegaLite.PAxis' ['Graphics.Vega.VegaLite.AxTitle' "Inception date"]]
          . position Y [PName "count", PmType Quantitative]
@

then it can be displayed automatically in Jupyter Lab by

> vlShow vl1

where 'vlShow' should be imported automatically by IHaskell.
-}

module IHaskell.Display.Hvega (vlShow, VegaLiteLab) where

import qualified Data.Text.Lazy as LT

import Data.Aeson.Text (encodeToLazyText)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Graphics.Vega.VegaLite (VegaLite, fromVL)

import IHaskell.Display (IHaskellDisplay(..), Display(..)
                        , javascript, vegalite)


-- ^ View a Vega-Lite visualization in a Jupyter *notebook*. Use 'vlShow'
--   instead if you are using Jupyter *lab*.
--
--   There is currently no way to pass
--   <https://github.com/vega/vega-embed#options options>
--   to the @vegaEmbed@ call.
--
--   Note that local file access is __not__ guaranteed to work - e.g.
--   'Graphics.Vega.VegaLite.dataFromUrl' where the file name refers to a local file -
--   since the JavaScript @fs@ module may not be loaded.
--
instance IHaskellDisplay VegaLite where
  display vl =

    let -- does https://github.com/vega/vega-embed/issues/8 help?
        config = "requirejs.config({"
                 <> "baseUrl: 'https://cdn.jsdelivr.net/npm/',"
                 <> "paths: {"
                 <> "'vega-embed': 'vega-embed@6?noext',"
                 <> "'vega-lib': 'vega-lib?noext',"
                 <> "'vega-lite': 'vega-lite@4?noext',"
                 <> "'vega': 'vega@5?noext'"
                 <> "}});"

        -- rely on the element variable being set up; it appears to be an array
        -- so use the first element to add the div to.
        makeDiv = "var ndiv = document.createElement('div');"
                   <> "ndiv.innerHTML = "
                   <> "'Awesome Vega-Lite visualization to appear here';"
                   <> "element[0].appendChild(ndiv);"

        js = LT.unpack (encodeToLazyText (fromVL vl))

        -- Use the div element we have just created for the plot.
        -- More options could be passed to vegaEmbed.
        --
        plot = "require(['vega-embed'],function(vegaEmbed){"
               <> "vegaEmbed(ndiv," <> js <> ").then("
               <> "function (result) { console.log(result); }).catch("
               <> "function (error) { ndiv.innerHTML = "
               <> "'There was an error: ' + error; });"
               <> "});"

        ds = [javascript (config <> makeDiv <> plot)]

    in pure (Display ds)

-- | A wrapper around 'VegaLite' so that we can write a 'Display'
--   instance for JupyterLab and not end up with "overlapping
--   instances".
--
--   Is there a better way to do this (other than drop support for
--   Jupyter notebook users)?
--
newtype VegaLiteLab = VLL VegaLite

-- | Convert a VegaLite visualization so that it can be auto-displayed
--   in Jupyter Lab.
--
vlShow :: VegaLite -> VegaLiteLab
vlShow = VLL

-- ^ Display Vega-Lite visualizations in an IHaskell notebook when
--   using Jupyter Lab.
--
--   Use the @IHaskell.Display.Hvega@ module when using IHaskell from
--   a Jupyter notebook.
--
--   Note that local file access is __not__ guaranteed to work - e.g.
--   'Graphics.Vega.VegaLite.dataFromUrl' where the file name refers to a local file -
--   since the JavaScript @fs@ module may not be loaded.
--
--   It would be nice to create a PNG version for non-browser viewers,
--   but I am not sure how to get Jupyter to do this (and if it would
--   even do what I hope it does).
--
instance IHaskellDisplay VegaLiteLab where
  display (VLL vl) = let js = LT.unpack (encodeToLazyText (fromVL vl))
                     in pure (Display [vegalite js])
