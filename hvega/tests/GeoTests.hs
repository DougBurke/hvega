{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GeoTests.elm as of version 1.12.0
--
module GeoTests (testSpecs) where

import qualified Data.Text as T

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Graphics.Vega.VegaLite

import Prelude hiding (filter)


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("defaultSize1", defaultSize1)
            , ("defaultSize2", defaultSize2)
            , ("choropleth1", choropleth1)
            , ("choropleth2", choropleth2)
            , ("linear1", tubeLines1)
            , ("linear2", tubeLines2)
            , ("linear3", tubeLines3)
            , ("sphere1", sphere1)
            , ("sphere2", sphere2)
            , ("graticule1", graticule1)
            , ("graticule2", graticule2)
            , ("graticule3", graticule3)
            , ("graticule4", graticule4)
            , ("scale1", scale1)
            , ("translate1", translate1)
            , ("mapComp1", mapComp1)
            , ("mapComp2", mapComp2)
            , ("mapComp3", mapComp3)
            , ("mapComp4", mapComp4)
            , ("dotMap1", dotMap1)
            , ("scribbleMap1", scribbleMap1)
            , ("scribbleMap2", scribbleMap2)
            , ("map1d", map1d)
            , ("geo_constant_value", geo_constant_value)
            ]


{- Some relevant data sources:

   https://github.com/deldersveld/topojson
   https://github.com/topojson/world-atlas
-}


noStroke :: PropertySpec
noStroke = configure $ configuration (ViewStyle [ ViewNoStroke ]) []


defaultSize1 :: VegaLite
defaultSize1 =
    toVegaLite
        [ description "Default map size"
        , projection [ PrType AlbersUsa ]
        , dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json" [ TopojsonFeature "counties" ]
        , mark Geoshape []
        , encoding $ color [ MString "black" ] []
        ]


defaultSize2 :: VegaLite
defaultSize2 =
    toVegaLite
        [ description "Default map size with view width and height specified in config."
        , configure $ configuration (ViewStyle [ ViewContinuousWidth 500
                                               , ViewContinuousHeight 300 ]) []
        , projection [ PrType AlbersUsa ]
        , dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json" [ TopojsonFeature "counties" ]
        , mark Geoshape []
        , encoding $ color [ MString "black" ] []
        ]


choropleth1 :: VegaLite
choropleth1 =
    toVegaLite
        [ width 900
        , height 500
        , noStroke
        , dataFromUrl "https://vega.github.io/vega-lite/data/londonBoroughs.json" [ TopojsonFeature "boroughs" ]
        , mark Geoshape [ MStrokeOpacity 0 ]
        , encoding $ color [ MName "id", MmType Nominal ] []
        ]


choropleth2 :: VegaLite
choropleth2 =
    let
        trans =
            transform
                . calculateAs "indexof (datum.name,' ') > 0  ? substring(datum.name,0,indexof(datum.name, ' ')) : datum.name" "bLabel"

        polyEnc =
            encoding
                . color [ MName "id", MmType Nominal, MScale boroughColors, MLegend [] ]
                . opacity [ MNumber 1 ]

        polySpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/londonBoroughs.json" [ TopojsonFeature "boroughs" ]
                , mark Geoshape [ MStroke "rgb(251,247,238)", MStrokeWidth 2 ]
                , polyEnc []
                ]

        labelEnc =
            encoding
                . position Longitude [ PName "cx", PmType Quantitative ]
                . position Latitude [ PName "cy", PmType Quantitative ]
                . text [ TName "bLabel", TmType Nominal ]

        labelSpec =
            asSpec [ dataFromUrl "https://vega.github.io/vega-lite/data/londonCentroids.json" [], trans [], mark Text [], labelEnc [] ]
    in
    toVegaLite
        [ width 1200
        , height 700
        , noStroke
        , layer [ polySpec, labelSpec ]
        ]


tubeLines1 :: VegaLite
tubeLines1 =
    toVegaLite
        [ width 700
        , height 500
        , dataFromUrl "https://vega.github.io/vega-lite/data/londonTubeLines.json" [ TopojsonFeature "line" ]
        , mark Geoshape [ MFilled False ]
        , encoding $ color [ MName "id", MmType Nominal ] []
        ]


tubeLines2 :: VegaLite
tubeLines2 =
    let
        enc =
            encoding
                . color
                    [ MName "id"
                    , MmType Nominal
                    , MLegend [ LNoTitle, LOrient LOBottomRight ]
                    , MScale tubeLineColors
                    ]
    in
    toVegaLite
        [ width 700
        , height 500
        , noStroke
        , dataFromUrl "https://vega.github.io/vega-lite/data/londonTubeLines.json" [ TopojsonFeature "line" ]
        , mark Geoshape [ MFilled False, MStrokeWidth 2 ]
        , enc []
        ]

tubeLines3 :: VegaLite
tubeLines3 =
    let
        polySpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/londonBoroughs.json" [ TopojsonFeature "boroughs" ]
                , mark Geoshape [ MStroke "rgb(251,247,238)", MStrokeWidth 2 ]
                , encoding $ color [ MString "#ddc" ] []
                ]

        labelEnc =
            encoding
                . position Longitude [ PName "cx", PmType Quantitative ]
                . position Latitude [ PName "cy", PmType Quantitative ]
                . text [ TName "bLabel", TmType Nominal ]
                . size [ MNumber 8 ]
                . opacity [ MNumber 0.6 ]

        trans =
            transform
                . calculateAs "indexof (datum.name,' ') > 0  ? substring(datum.name,0,indexof(datum.name, ' ')) : datum.name" "bLabel"

        labelSpec =
            asSpec [ dataFromUrl "https://vega.github.io/vega-lite/data/londonCentroids.json" [], trans [], mark Text [], labelEnc [] ]

        tubeEnc =
            encoding
                . color
                    [ MName "id"
                    , MmType Nominal
                    , MLegend [ LNoTitle, LOrient LOBottomRight, LOffset 0 ]
                    , MScale tubeLineColors
                    ]

        routeSpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/londonTubeLines.json" [ TopojsonFeature "line" ]
                , mark Geoshape [ MFilled False, MStrokeWidth 2 ]
                , tubeEnc []
                ]
    in
    toVegaLite
        [ width 700
        , height 500
        , noStroke
        , layer [ polySpec, labelSpec, routeSpec ]
        ]


boroughColors :: [ScaleProperty]
boroughColors =
    categoricalDomainMap
        [ ( "Kingston upon Thames", "#9db7b1" )
        , ( "Croydon", "#d4b4e5" )
        , ( "Bromley", "#afb9cb" )
        , ( "Hounslow", "#b2add6" )
        , ( "Ealing", "#e2f8ca" )
        , ( "Havering", "#a1bde6" )
        , ( "Hillingdon", "#e8aa95" )
        , ( "Harrow", "#8bd0eb" )
        , ( "Brent", "#dfb89b" )
        , ( "Barnet", "#a2e7ed" )
        , ( "Lambeth", "#e3aba7" )
        , ( "Southwark", "#86cbd1" )
        , ( "Lewisham", "#ecb1c2" )
        , ( "Greenwich", "#acd8ba" )
        , ( "Bexley", "#e4bad9" )
        , ( "Enfield", "#9bd6ca" )
        , ( "Waltham Forest", "#cec9f3" )
        , ( "Redbridge", "#c9d2a8" )
        , ( "Sutton", "#d1c1d9" )
        , ( "Richmond upon Thames", "#ddcba2" )
        , ( "Merton", "#a2acbd" )
        , ( "Wandsworth", "#deefd6" )
        , ( "Hammersmith and Fulham", "#b5d7a7" )
        , ( "Kensington and Chelsea", "#f6d4c9" )
        , ( "Westminster", "#add4e0" )
        , ( "Camden", "#d9b9ad" )
        , ( "Tower Hamlets", "#c6e1db" )
        , ( "Islington", "#e0c7ce" )
        , ( "Hackney", "#a6b79f" )
        , ( "Haringey", "#cbd5e7" )
        , ( "Newham", "#c2d2ba" )
        , ( "Barking and Dagenham", "#ebe2cf" )
        , ( "City of London", "#c7bfad" )
        ]


tubeLineColors :: [ScaleProperty]
tubeLineColors =
    categoricalDomainMap
        [ ( "Bakerloo", "rgb(137,78,36)" )
        , ( "Central", "rgb(220,36,30)" )
        , ( "Circle", "rgb(255,206,0)" )
        , ( "District", "rgb(1,114,41)" )
        , ( "DLR", "rgb(0,175,173)" )
        , ( "Hammersmith & City", "rgb(215,153,175)" )
        , ( "Jubilee", "rgb(106,114,120)" )
        , ( "Metropolitan", "rgb(114,17,84)" )
        , ( "Northern", "rgb(0,0,0)" )
        , ( "Piccadilly", "rgb(0,24,168)" )
        , ( "Victoria", "rgb(0,160,226)" )
        , ( "Waterloo & City", "rgb(106,187,170)" )
        ]


sphere1 :: VegaLite
sphere1 =
    let
        dataVals =
            dataFromUrl "https://gicentre.github.io/data/geoTutorials/world-110m.json"
                [ TopojsonFeature "countries1" ]

        proj =
            projection [ PrType Orthographic ]
    in
    toVegaLite [ width 300, height 300, dataVals, proj, mark Geoshape [ MFill "rgb(149,181,146)" ] ]


sphere2 :: VegaLite
sphere2 =
    let
        countryData =
            dataFromUrl "https://gicentre.github.io/data/geoTutorials/world-110m.json"
                [ TopojsonFeature "countries1" ]

        proj =
            projection [ PrType Orthographic ]

        sphereSpec =
            asSpec [ sphere, mark Geoshape [ MFill "aliceblue" ] ]

        countrySpec =
            asSpec [ countryData, mark Geoshape [ MFill "rgb(149,181,146)" ] ]
    in
    toVegaLite [ width 300, height 300, proj, layer [ sphereSpec, countrySpec ] ]


graticule1 :: VegaLite
graticule1 =
    let
        proj =
            projection [ PrType Orthographic, PrRotate (-42) (-30) 0 ]

        sphereSpec =
            asSpec [ sphere, mark Geoshape [ MFill "aliceblue" ] ]

        gratSpec =
            asSpec [ graticule [], mark Geoshape [ MFilled False, MStrokeWidth 0.3 ] ]
    in
    toVegaLite [ width 300, height 300, proj, layer [ sphereSpec, gratSpec ] ]


graticule2 :: VegaLite
graticule2 =
    let
        proj =
            projection [ PrType Orthographic, PrRotate (-42) (-30) 0 ]

        sphereSpec =
            asSpec [ sphere, mark Geoshape [ MFill "aliceblue" ] ]

        gratSpec =
            asSpec
                [ graticule [ GrExtent ( 0, 0 ) ( 90, 90 )
                            , GrStep ( 2, 2 )
                            , GrPrecision 2 ]
                , mark Geoshape [ MFilled False, MStrokeWidth 0.3 ]
                ]
    in
    toVegaLite [ width 300, height 300, proj, layer [ sphereSpec, gratSpec ] ]


graticule3 :: VegaLite
graticule3 =
    let
        proj =
            projection [ PrType Orthographic, PrRotate (-42) (-30) 0 ]

        sphereSpec =
            asSpec [ sphere, mark Geoshape [ MFill "aliceblue" ] ]

        gratSpec =
            asSpec
                [ graticule
                    [ GrExtentMajor ( 0, 0 ) ( 90, 90 )
                    , GrExtentMinor ( 0, 0 ) ( 90, 75.01 )
                    , GrStepMinor ( 2, 2 )
                    ]
                , mark Geoshape [ MFilled False, MStrokeWidth 0.3 ]
                ]
    in
    toVegaLite [ width 300, height 300, proj, layer [ sphereSpec, gratSpec ] ]


graticule4 :: VegaLite
graticule4 =
    let
        proj =
            projection [ PrType Orthographic, PrRotate (-42) (-30) 0 ]

        sphereSpec =
            asSpec [ sphere, mark Geoshape [ MFill "aliceblue" ] ]

        gratSpec =
            asSpec
                [ graticule [ GrStepMinor ( 15, 30 ) ]
                , mark Geoshape [ MFilled False, MStrokeWidth 0.3 ]
                ]
    in
    toVegaLite [ width 300, height 300, proj, layer [ sphereSpec, gratSpec ] ]

scale1 :: VegaLite
scale1 =
    let
        dataVals =
            dataFromUrl "https://gicentre.github.io/data/geoTutorials/world-110m.json"
                [ TopojsonFeature "countries1" ]

        proj =
            projection [ PrType Orthographic, PrScale 470 ]

        countrySpec =
            asSpec [ dataVals, mark Geoshape [ MFill "rgb(149,181,146)" ] ]

        gratSpec =
            asSpec
                [ graticule [ GrStepMinor ( 5, 5 ) ]
                , mark Geoshape [ MFilled False, MStrokeWidth 0.3 ]
                ]
    in
    toVegaLite [ width 300, height 300, proj, layer [ countrySpec, gratSpec ] ]


translate1 :: VegaLite
translate1 =
    let
        dataVals =
            dataFromUrl "https://gicentre.github.io/data/geoTutorials/world-110m.json"
                [ TopojsonFeature "countries1" ]

        proj =
            projection [ PrType Orthographic, PrTranslate 0 100 ]

        countrySpec =
            asSpec [ dataVals, mark Geoshape [ MFill "rgb(149,181,146)" ] ]

        gratSpec =
            asSpec
                [ graticule [ GrStepMinor ( 5, 5 ) ]
                , mark Geoshape [ MFilled False, MStrokeWidth 0.3 ]
                ]
    in
    toVegaLite [ width 300, height 300, proj, layer [ countrySpec, gratSpec ] ]


mapComp1 :: VegaLite
mapComp1 =
    let
        globe =
            asSpec
                [ width 300
                , height 300
                , dataFromUrl "https://vega.github.io/vega-lite/data/graticule.json" [ TopojsonFeature "graticule" ]
                , projection [ PrType Orthographic ]
                , mark Geoshape [ MFilled False ]
                ]
    in
    toVegaLite [ hConcat [ globe, globe, globe ] ]


mapComp2 :: VegaLite
mapComp2 =
    let
        globe =
            let
                graticuleSpec =
                    asSpec
                        [ dataFromUrl "https://vega.github.io/vega-lite/data/graticule.json" [ TopojsonFeature "graticule" ]
                        , mark Geoshape [ MFilled False, MStroke "#411", MStrokeWidth 0.1 ]
                        ]

                countrySpec =
                    asSpec
                        [ dataFromUrl "https://vega.github.io/vega-lite/data/world-110m.json" [ TopojsonFeature "land" ]
                        , mark Geoshape [ MFill "black", MFillOpacity 0.7 ]
                        ]
            in
            asSpec [ width 300, height 300, projection [ PrType Orthographic ], layer [ graticuleSpec, countrySpec ] ]
    in
    toVegaLite
        [ noStroke
        , hConcat [ globe, globe, globe ]
        ]


mapComp3 :: VegaLite
mapComp3 =
    let
        rotatedSpec rot =
            let
                graticuleSpec =
                    asSpec
                        [ projection [ PrType Orthographic, PrRotate rot 0 0 ]
                        , dataFromUrl "https://vega.github.io/vega-lite/data/graticule.json" [ TopojsonFeature "graticule" ]
                        , mark Geoshape [ MFilled False, MStroke "#411", MStrokeWidth 0.1 ]
                        ]

                countrySpec =
                    asSpec
                        [ projection [ PrType Orthographic, PrRotate rot 0 0 ]
                        , dataFromUrl "https://vega.github.io/vega-lite/data/world-110m.json" [ TopojsonFeature "countries" ]
                        , mark Geoshape [ MStroke "white", MFill "black", MStrokeWidth 0.5 ]
                        ]
            in
            asSpec [ width 300, height 300, layer [ graticuleSpec, countrySpec ] ]
    in
    toVegaLite
        [ noStroke
        , hConcat [ rotatedSpec (-65), rotatedSpec 115, rotatedSpec 10 ]
        ]


mapComp4 :: VegaLite
mapComp4 =
    let
        rotatedSpec rot =
            let
                seaSpec =
                    asSpec
                        [ projection [ PrType Orthographic, PrRotate 0 0 0 ]
                        , dataFromUrl "data/globe.json" [ TopojsonFeature "globe" ]
                        , mark Geoshape [ MFill "#c1e7f5", MStrokeOpacity 0 ]
                        ]

                graticuleSpec =
                    asSpec
                        [ projection [ PrType Orthographic, PrRotate rot 0 0 ]
                        , dataFromUrl "https://vega.github.io/vega-lite/data/graticule.json" [ TopojsonFeature "graticule" ]
                        , mark Geoshape [ MFilled False, MStroke "#411", MStrokeWidth 0.1 ]
                        ]

                countrySpec =
                    asSpec
                        [ projection [ PrType Orthographic, PrRotate rot 0 0 ]
                        , dataFromUrl "https://vega.github.io/vega-lite/data/world-110m.json" [ TopojsonFeature "countries" ]
                        , mark Geoshape [ MStroke "white", MFill "#242", MStrokeWidth 0.1 ]
                        ]
            in
            asSpec [ width 300, height 300, layer [ seaSpec, graticuleSpec, countrySpec ] ]
    in
    toVegaLite
        [ noStroke
        , hConcat [ rotatedSpec 0, rotatedSpec (-40) ]
        ]


dotMap1 :: VegaLite
dotMap1 =
    let
        enc =
            encoding
                . position Longitude [ PName "longitude", PmType Quantitative ]
                . position Latitude [ PName "latitude", PmType Quantitative ]
                . size [ MNumber 1 ]
                . color [ MName "digit", MmType Nominal ]
    in
    toVegaLite
        [ description "US zip codes: One dot per zipcode colored by first digit"
        , width 500
        , height 300
        , projection [ PrType AlbersUsa ]
        , dataFromUrl "https://vega.github.io/vega-lite/data/zipcodes.csv" []
        , transform $ calculateAs "substring(datum.zip_code, 0, 1)" "digit" []
        , mark Circle []
        , enc []
        ]


scribbleMap1 :: VegaLite
scribbleMap1 =
    let
        stateCondition =
          T.pack $
            concatMap (\s -> "&& datum.state !='" <> s <> "'") [ "AS", "FM", "PW", "MH", "GU", "MP", "VI", "PR" ]

        config =
            configure
                . configuration (TitleStyle [ TFont "Roboto", TFontWeight W300, TFontSize 28 ])
                . configuration (ViewStyle [ ViewNoStroke ])

        trans =
            transform
                . filter (FExpr ("datum.latitude != '' && datum.county != 'Honolulu' " <> stateCondition))
                . calculateAs "datum.state == 'HI' ? 'hi' : (datum.state == 'AK' ? 'ak' : 'continent')" "conterminous"

        enc =
            encoding
                . position Longitude [ PName "longitude", PmType Quantitative ]
                . position Latitude [ PName "latitude", PmType Quantitative ]
                . order [ OName "zip_code", OmType Quantitative ]
                . color [ MString "#666" ]
                . detail [ DName "conterminous", DmType Nominal ]
    in
    toVegaLite
        [ title "US connected zip codes" []
        , config []
        , width 1000
        , height 600
        , projection [ PrType AlbersUsa ]
        , dataFromUrl "https://vega.github.io/vega-lite/data/zipcodes.csv" []
        , trans []
        , mark Line [ MStrokeWidth 0.2, MInterpolate Monotone ]
        , enc []
        ]

scribbleMap2 :: VegaLite
scribbleMap2 =
    let
        stateCondition =
          T.pack $
            concatMap (\s -> "&& datum.state !='" <> s <> "'") [ "AS", "FM", "PW", "MH", "GU", "MP", "VI", "PR" ]

        config =
            configure
                . configuration (TitleStyle [ TFont "Roboto", TFontWeight W300, TFontSize 28 ])
                . configuration (ViewStyle [ ViewNoStroke ])

        trans =
            transform
                . filter (FExpr ("datum.latitude != '' && datum.county != 'Honolulu' " <> stateCondition))
                . calculateAs "substring(datum.zip_code, 0, 3)" "digit3"
                . calculateAs "length(datum.zip_code+' ')" "ziplen"

        enc =
            encoding
                . position Longitude [ PName "longitude", PmType Quantitative ]
                . position Latitude [ PName "latitude", PmType Quantitative ]
                . order [ OName "zip_code", OmType Quantitative ]
                . color [ MName "digit3", MmType Nominal, MLegend [] ]
                . detail [ DName "ziplen", DmType Nominal ]
    in
    toVegaLite
        [ title "US connected zip codes, coloured by first three digits" []
        , config []
        , width 1000
        , height 600
        , projection [ PrType AlbersUsa ]
        , dataFromUrl "https://vega.github.io/vega-lite/data/zipcodes.csv" []
        , trans []
        , mark Line [ MStrokeWidth 0.2, MInterpolate Monotone ]
        , enc []
        ]

map1d :: VegaLite
map1d =
    let
        geoData =
            dataFromUrl "https://gicentre.github.io/data/geoTutorials/londonBoroughs.json"
                [ TopojsonFeature "boroughs" ]

        centroidData =
            dataFromUrl "https://gicentre.github.io/data/geoTutorials/londonCentroids.csv"

        backgroundSpec =
            asSpec [ geoData, mark Geoshape [ MFill "lightgrey", MStroke "white" ] ]

        cEnc =
            encoding
                . position Longitude [ PName "cx", PmType Quantitative ]
                . position Latitude [ PName "cy", PmType Quantitative ]

        hEnc =
            encoding
                . position Longitude [ PName "cx", PmType Quantitative ]
                . position Latitude [ PNumber 51.28 ]

        vEnc =
            encoding
                . position Longitude [ PNumber (-0.52) ]
                . position Latitude [ PName "cy", PmType Quantitative ]

        cSpec =
            asSpec [ centroidData [], mark Circle [], cEnc [] ]

        hSpec =
            asSpec [ centroidData [], mark Circle [], hEnc [] ]

        vSpec =
            asSpec [ centroidData [], mark Circle [], vEnc [] ]
    in
    toVegaLite
        [ width 500, height 400
        , layer [ backgroundSpec, cSpec, hSpec, vSpec ] ]


-- geo_constant_value.vl.json
geo_constant_value :: VegaLite
geo_constant_value =
  let lyr1 = [ mark Square []
             , encoding
               . position Longitude [PName "longitude", PmType Quantitative]
               . position Latitude [PName "latitude", PmType Quantitative]
               . size [MNumber 1]
               . color [MString "gray"]
               $ []
             ]

      lyr2 = [ mark Square []
             , encoding
               . position Longitude [PDatum (Number (-122.335167))]
               . position Latitude [PName "latitude", PmType Quantitative]
               . size [MNumber 1]
               . color [MString "steelblue"]
               $ []
             ]

      lyr3 = [ mark Square []
             , encoding
               . position Longitude [PName "longitude", PmType Quantitative]
               . position Latitude [PDatum (Number 47.608013)]
               . size [MNumber 1]
               . color [MString "firebrick"]
               $ []
             ]

  in toVegaLite [ width 500
                , height 300
                , dataFromUrl "https://vega.github.io/vega-lite/data/airports.csv" []
                , projection [PrType AlbersUsa]
                , layer (map asSpec [lyr1, lyr2, lyr3])
                ]
