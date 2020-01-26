{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Output
Copyright   : (c) Douglas Burke, 2018-2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

Write out the VegaLite specification.

-}

module Graphics.Vega.VegaLite.Output
       ( toHtml
       , toHtmlFile
       , toHtmlWith
       , toHtmlFileWith
       ) where

import qualified Data.Aeson.Text as A
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Data.Aeson (Value)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Graphics.Vega.VegaLite.Specification (VegaLite, fromVL)


{-|

Converts VegaLite to html Text. Uses Vega-Embed with the
default options. See 'toHtmlWith' for more control.

@since 0.2.1.0
-}
toHtml :: VegaLite -> TL.Text
toHtml = toHtmlWith Nothing

{-|

Converts VegaLite to an html file. Uses Vega-Embed with the
default options. See 'toHtmlFileWith' for more control.

@since 0.2.1.0
-}
toHtmlFile :: FilePath -> VegaLite -> IO ()
toHtmlFile file = TL.writeFile file . toHtml

{-|

Converts VegaLite to html Text. Uses Vega-Embed and is for when
some control is needed over the output: 'toHtml' is a simpler
form which just uses the default Vega-Embed options.

The render you use to view the output file must support Javascript,
since it is needed to create the visualization from the Vega-Lite
specification. The Vega and Vega-Lite Javascript versions are pegged
to 5 and 4, but no limit is applied to the Vega-Embed library.

@since 0.4.0.0
-}
toHtmlWith ::
  Maybe Value
  -- ^ The options to pass to the Vega-Embed @embed@ routine. See
  --   <https://github.com/vega/vega-embed#options> for the
  --   supported options.
  -> VegaLite
  -- ^ The Vega-Lite specification to display.
  -> TL.Text
toHtmlWith mopts vl =
  let spec = A.encodeToLazyText (fromVL vl)
      opts = maybe "" (\o -> "," <> A.encodeToLazyText o) mopts

  in TL.unlines
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<head>"
      -- versions are fixed at vega 5, vega-lite 4
    , "  <script src=\"https://cdn.jsdelivr.net/npm/vega@5\"></script>"
    , "  <script src=\"https://cdn.jsdelivr.net/npm/vega-lite@4\"></script>"
    , "  <script src=\"https://cdn.jsdelivr.net/npm/vega-embed\"></script>"
    , "</head>"
    , "<body>"
    , "<div id=\"vis\"></div>"
    , "<script type=\"text/javascript\">"
    , "  var spec = " <> spec <> ";"
    , "  vegaEmbed(\'#vis\', spec" <> opts <> ").then(function(result) {"
    , "  // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view"
    , "  }).catch(console.error);"
    , "</script>"
    , "</body>"
    , "</html>"
    ]

{-|

Converts VegaLite to an html file. Uses Vega-Embed and is for when
some control is needed over the output: 'toHtmlFile' is a simpler
form which just uses the default Vega-Embed options.

@since 0.4.0.0
-}
toHtmlFileWith ::
  Maybe Value
  -- ^ The options to pass to the Vega-Embed @embed@ routine. See
  --   <https://github.com/vega/vega-embed#options> for the
  --   supported options.
  -> FilePath
  -- ^ The output file name (it will be over-written if it already exists).
  -> VegaLite
  -- ^ The Vega-Lite specification to display.
  -> IO ()
toHtmlFileWith mopts file = TL.writeFile file . toHtmlWith mopts

