{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : IHaskell.Display.HvegaLab
Copyright   : (c) Douglas Burke 2018, 2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings

Allow VegaLite visualizations to be displayed directly in Jupyter notebooks.
There is also @IHaskell.Display.HvegaLab@ which is for Jupyter Lab,
and it is suggested that you migrate to that environment, since Jupyter Lab
has in-built support for Vega and Vega-Lite visualizations.

Jupyter Lab can be installed with nix using the
<https://github.com/tweag/jupyterWith jupyterWith> setup.

-}

module IHaskell.Display.Hvega where

import qualified Data.Text.Lazy as LT

import Data.Aeson.Text (encodeToLazyText)
import Data.Monoid ((<>))

import Graphics.Vega.VegaLite (VegaLite, fromVL)

import IHaskell.Display (IHaskellDisplay(..), Display(..), javascript)


-- ^ Use <https://vega.github.io/vega-lite/usage/embed.html Vega-Embed>
--   to include the Vega-Lite visualization in an IHaskell notebook.
--
--   There is currently no way to pass
--   <https://github.com/vega/vega-embed#options options>
--   to the @vegaEmbed@ call.
--
--   Note that local file access is __not__ guaranteed to work - e.g.
--   @dataFromUrl@ where the file name refers to a local file -
--   since the JavaScript @fs@ module may not be loaded.
--
instance IHaskellDisplay VegaLite where
  display vl = 
  
    let -- Note: need to look in the package.json files for these packages
        -- to find the "full name" (the contents of the jsdelivr key),
        -- since requirejs does seem to like appending .js to everything.
        --
        jsname n v = "'https://cdn.jsdelivr.net/npm/"
                     <> n
                     <> "@" <> v
                     <> "/build/"
                     <> n <> ".min'"

        -- Should the config be set on require or requirejs?
        --
        -- These versions are known to work; later versions
        -- do not work but I have not tried to work out the
        -- latest version that does work.
        --
        config = "requirejs({paths:{vg:" <> jsname "vega" "3.2.1"
                 <> ",vl:" <> jsname "vega-lite" "2.3.0"
                 <> ",vge:" <> jsname "vega-embed" "3.5.2"
                 <> "},shim:{vge:{deps:['vg.global','vl.global']}"
                 <> ",vl:{deps:['vg']}"
                 <> "}});"
                 <> "define('vg.global',['vg'],function(g){window.vega = g;});"
                 <> "define('vl.global',['vl'],function(g){window.vl = g;});"

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
        plot = "require(['vge'],function(vegaEmbed){"
               <> "vegaEmbed(ndiv," <> js <> ").then("
               <> "function (result) { console.log(result); }).catch("
               <> "function (error) { ndiv.innerHTML = "
               <> "'There was an error: ' + error; });"
               <> "});"
                      
        ds = [javascript (config <> makeDiv <> plot)]
             
    in pure (Display ds)
