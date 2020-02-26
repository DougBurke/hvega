{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite ProjctionTests.elm as of version 1.12.0
--
module ProjectionTests (testSpecs) where

import qualified Data.Text as T

import Graphics.Vega.VegaLite

import Prelude hiding (filter)


testSpecs :: [(String, VegaLite)]
testSpecs = standardProjs
            ++ [ configExample
               , reflectExample False False
               , reflectExample True False
               , reflectExample False True
               , reflectExample True True
               ]
            ++ d3Projections


{- Some relevant data sources:

   https://github.com/deldersveld/topojson
   https://github.com/topojson/world-atlas

   graticule.json produced with mapshaper.org:
     open console and type -graticule then export as topojson.
-}


worldMapTemplate :: String -> [ProjectionProperty] -> (String, VegaLite)
worldMapTemplate tText projProps =
    ( tText
    , toVegaLite
        [ width 500
        , height 300
        , title (T.pack tText) []
        , background "#c1e7f5"
        , projection projProps

        --, dataFromUrl "https://vega.github.io/vega-lite/data/world-110m.json" [ TopojsonFeature "countries" ]
        , dataFromUrl "https://vega.github.io/vega-lite/data/graticule.json" [ TopojsonFeature "graticule" ]
        , mark Geoshape [ MFillOpacity 0.01, MStroke "#411", MStrokeWidth 0.5 ]
        ]
    )

standardProjs :: [(String, VegaLite)]
standardProjs =
    [ worldMapTemplate "Albers" [ PrType Albers ]
    , worldMapTemplate "AzimuthalEqualArea" [ PrType AzimuthalEqualArea ]
    , worldMapTemplate "AzimuthalEquidistant" [ PrType AzimuthalEquidistant ]
    , worldMapTemplate "ConicConformal" [ PrType ConicConformal, PrClipAngle (Just 65) ]
    , worldMapTemplate "ConicEqualArea" [ PrType ConicEqualArea ]
    , worldMapTemplate "ConicEquidistant" [ PrType ConicEquidistant ]
    , worldMapTemplate "EqualEarth" [ PrType EqualEarth ]
    , worldMapTemplate "Equirectangular" [ PrType Equirectangular ]
    , worldMapTemplate "Gnomonic" [ PrType Gnomonic ]
    , worldMapTemplate "Identity" [ PrType Identity ]
    , worldMapTemplate "Mercator" [ PrType Mercator ]
    , worldMapTemplate "NaturalEarth1" [ PrType NaturalEarth1 ]
    , worldMapTemplate "Orthographic" [ PrType Orthographic ]
    , worldMapTemplate "Stereographic" [ PrType Stereographic ]
    , worldMapTemplate "TransverseMercator" [ PrType TransverseMercator ]
    ]


d3Projections :: [(String, VegaLite)]
d3Projections =
    -- Note these require registering via JavaScript in the hosting page.
    let
        customSpec pText =
            worldMapTemplate pText [ PrType (Custom (T.pack pText))
                                   , PrClipAngle (Just 179.999)
                                   , PrRotate 20 (-90) 0
                                   , PrPrecision 0.1 ]
    in
    map customSpec [ "airy", "aitoff", "armadillo", "august", "baker", "berghaus", "bertin1953", "boggs", "bonne", "bottomley", "collignon", "craig", "craster", "cylindricalequalarea", "cylindricalstereographic", "eckert1", "eckert2", "eckert3", "eckert4", "eckert5", "eckert6", "eisenlohr", "fahey", "foucaut", "gingery", "winkel3" ]


configExample :: (String, VegaLite)
configExample =
    let
        cfg =
            configure
                . configuration (BackgroundStyle "rgb(251,247,238)")
                . configuration (TitleStyle [ TFont "Roboto", TFontWeight W600, TFontSize 18 ])
                . configuration (ViewStyle [ ViewContinuousWidth 500
                                           , ViewContinuousHeight 300
                                           , ViewNoStroke ])
                . configuration (AutosizeStyle [ AFit ])
                . configuration (ProjectionStyle [ PrType Orthographic, PrRotate 0 0 0 ])

        globeSpec =
            asSpec
                [ dataFromUrl "data/globe.json" [ TopojsonFeature "globe" ]
                , mark Geoshape [ MColor "#c1e7f5" ]
                ]

        graticuleSpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/graticule.json" [ TopojsonFeature "graticule" ]
                , mark Geoshape [ MFillOpacity 0.01, MStroke "#411", MStrokeWidth 0.1 ]
                ]

        countrySpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/world-110m.json" [ TopojsonFeature "countries" ]
                , mark Geoshape [ MColor "#708E71" ]
                ]
    in
    ( "configExample"
    , toVegaLite [ title "Hello, World!" [], cfg [], layer [ globeSpec, graticuleSpec, countrySpec ] ]
    )


reflectExample :: Bool -> Bool -> (String, VegaLite)
reflectExample rx ry =
    let
        tname =
            if not rx && not ry then
                "identityExample"

            else
                "reflect"
                    ++ (if rx then
                            "X"

                        else
                            ""
                       )
                    ++ (if ry then
                            "Y"

                        else
                            ""
                       )
                    ++ "Example"

        globeSpec =
            asSpec
                [ dataFromUrl "data/globe.json" [ TopojsonFeature "globe" ]
                , mark Geoshape [ MColor "#c1e7f5" ]
                , projection [ PrType Identity, PrReflectX rx, PrReflectY ry ]
                ]

        graticuleSpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/graticule.json" [ TopojsonFeature "graticule" ]
                , mark Geoshape [ MFillOpacity 0.01, MStroke "#411", MStrokeWidth 0.1 ]
                , projection [ PrType Identity, PrReflectX rx, PrReflectY ry ]
                ]

        countrySpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/world-110m.json" [ TopojsonFeature "countries" ]
                , mark Geoshape [ MColor "#708E71" ]
                , projection [ PrType Identity, PrReflectX rx, PrReflectY ry ]
                ]
    in
    ( tname, toVegaLite [ width 500, height 250, layer [ globeSpec, graticuleSpec, countrySpec ] ] )
