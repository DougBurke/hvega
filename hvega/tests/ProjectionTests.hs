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
        , title (T.pack tText)
        , background "#c1e7f5"
        , projection projProps

        --, dataFromUrl "https://vega.github.io/vega-lite/data/world-110m.json" [ TopojsonFeature "countries" ]
        , dataFromUrl "https://vega.github.io/vega-lite/data/graticule.json" [ TopojsonFeature "graticule" ]
        , mark Geoshape [ MFillOpacity 0.01, MStroke "#411", MStrokeWidth 0.5 ]
        ]
    )

standardProjs :: [(String, VegaLite)]
standardProjs =
    [ worldMapTemplate "Albers" [ PType Albers ]
    , worldMapTemplate "AzimuthalEqualArea" [ PType AzimuthalEqualArea ]
    , worldMapTemplate "AzimuthalEquidistant" [ PType AzimuthalEquidistant ]
    , worldMapTemplate "ConicConformal" [ PType ConicConformal, PClipAngle (Just 65) ]
    , worldMapTemplate "ConicEqualArea" [ PType ConicEqualArea ]
    , worldMapTemplate "ConicEquidistant" [ PType ConicEquidistant ]
    , worldMapTemplate "Equirectangular" [ PType Equirectangular ]
    , worldMapTemplate "Gnomonic" [ PType Gnomonic ]
    , worldMapTemplate "Identity" [ PType Identity ]
    , worldMapTemplate "Mercator" [ PType Mercator ]
    , worldMapTemplate "Orthographic" [ PType Orthographic ]
    , worldMapTemplate "Stereographic" [ PType Stereographic ]
    , worldMapTemplate "TransverseMercator" [ PType TransverseMercator ]
    ]


d3Projections :: [(String, VegaLite)]
d3Projections =
    -- Note these require registering via JavaScript in the hosting page.
    let
        customSpec pText =
            worldMapTemplate pText [ PType (Custom (T.pack pText))
                                   , PClipAngle (Just 179.999)
                                   , PRotate 20 (-90) 0
                                   , PPrecision 0.1 ]
    in
    map customSpec [ "airy", "aitoff", "armadillo", "august", "baker", "berghaus", "bertin1953", "boggs", "bonne", "bottomley", "collignon", "craig", "craster", "cylindricalequalarea", "cylindricalstereographic", "eckert1", "eckert2", "eckert3", "eckert4", "eckert5", "eckert6", "eisenlohr", "fahey", "foucaut", "gingery", "winkel3" ]


configExample :: (String, VegaLite)
configExample =
    let
        cfg =
            configure
                . configuration (Background "rgb(251,247,238)")
                . configuration (TitleStyle [ TFont "Roboto", TFontWeight W600, TFontSize 18 ])
                . configuration (View [ ViewWidth 500, ViewHeight 300, ViewStroke Nothing ])
                . configuration (Autosize [ AFit ])
                . configuration (Projection [ PType Orthographic, PRotate 0 0 0 ])

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
    , toVegaLite [ title "Hello, World!", cfg [], layer [ globeSpec, graticuleSpec, countrySpec ] ]
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
                , projection [ PType Identity, PReflectX rx, PReflectY ry ]
                ]

        graticuleSpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/graticule.json" [ TopojsonFeature "graticule" ]
                , mark Geoshape [ MFillOpacity 0.01, MStroke "#411", MStrokeWidth 0.1 ]
                , projection [ PType Identity, PReflectX rx, PReflectY ry ]
                ]

        countrySpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/world-110m.json" [ TopojsonFeature "countries" ]
                , mark Geoshape [ MColor "#708E71" ]
                , projection [ PType Identity, PReflectX rx, PReflectY ry ]
                ]
    in
    ( tname, toVegaLite [ width 500, height 250, layer [ globeSpec, graticuleSpec, countrySpec ] ] )
