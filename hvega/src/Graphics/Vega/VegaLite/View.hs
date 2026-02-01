{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Graphics.Vega.VegaLite.Output
Copyright   : (c) Douglas Burke, 2018-2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

Write out the VegaLite specification.
-}
module Graphics.Vega.VegaLite.View (view) where

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

import Control.Monad (void)
import System.Directory (
    createDirectoryIfMissing,
    getHomeDirectory,
 )
import System.FilePath ((</>))
import System.IO (hClose, hSetEncoding, utf8)
import System.IO.Temp (openTempFile)
import System.Process (
    StdStream (NoStream),
    createProcess,
    proc,
    std_err,
    std_in,
    std_out,
    waitForProcess,
 )

import Graphics.Vega.VegaLite.Output (toHtml)
import Graphics.Vega.VegaLite.Specification (VegaLite)

{- |

Opens a VegaLite plot in the operating system's default browser.

@since 0.12.9.0
-}
view :: VegaLite -> IO ()
view v = do
    dir <- fmap ((</> "hvega" </> "cache")) getHomeDirectory
    createDirectoryIfMissing True dir

    (file, h) <- openTempFile dir "plot-.html"
    hSetEncoding h utf8
    T.hPutStr h (TL.toStrict (toHtml v))
    hClose h

    openFileSilently openProgramForHostOS file

openProgramForHostOS :: String
#if defined(mingw32_HOST_OS)
openProgramForHostOS = "start"
#elif defined(darwin_HOST_OS)
openProgramForHostOS = "open"
#else
openProgramForHostOS = "xdg-open"
#endif

openFileSilently :: FilePath -> FilePath -> IO ()
openFileSilently program path = do
    (_, _, _, ph) <-
        createProcess
            (proc program [path])
                { std_in = NoStream
                , std_out = NoStream
                , std_err = NoStream
                }
    void (waitForProcess ph)
