{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryGeo.elm (from development of version
-- 1.13.0)
--
module Gallery.Geo (testSpecs) where

import Graphics.Vega.VegaLite

-- import qualified Data.Text as T

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("geo1", geo1)
            , ("geo2", geo2)
            , ("geo3", geo3)
            , ("geo4", geo4)
            , ("geo5", geo5)
            , ("geo6", geo6)
            , ("geo7", geo7)
            , ("geo8", geo8)
            , ("geo9", geo9)
            ]


{-| No borders around maps
-}
cfg :: [ConfigureSpec] -> PropertySpec
cfg =
    configure
        . configuration (ViewStyle [ ViewNoStroke ])


geo1 :: VegaLite
geo1 =
  let countyData =
        dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json"
        [ TopojsonFeature "counties" ]

      unemploymentData =
        dataFromUrl "https://vega.github.io/vega-lite/data/unemployment.tsv" []

      trans =
        transform
        . lookup "id" unemploymentData "id" (LuFields [ "rate" ])

      proj = projection [ PrType AlbersUsa ]

      enc = encoding
            . color [ MName "rate", MmType Quantitative, MSort [ Descending ] ]

  in toVegaLite [ cfg [], width 500, height 300, countyData
                , proj, trans [], enc [], mark Geoshape [] ]


geo2 :: VegaLite
geo2 =
    let
        enc =
            encoding
                . position Longitude [ PName "longitude", PmType Quantitative ]
                . position Latitude [ PName "latitude", PmType Quantitative ]
                . size [ MNumber 1 ]
                . color [ MName "digit", MmType Nominal ]
    in
    toVegaLite
        [ cfg []
        , description "US zip codes: One dot per zipcode colored by first digit"
        , width 500
        , height 300
        , projection [ PrType AlbersUsa ]
        , dataFromUrl "https://vega.github.io/vega-lite/data/zipcodes.csv" []
        , transform (calculateAs "substring(datum.zip_code, 0, 1)" "digit" [])
        , mark Circle []
        , enc []
        ]


geo3 :: VegaLite
geo3 =
    let
        des =
            description "One dot per airport in the US overlayed on geoshape"

        backdropSpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json" [ TopojsonFeature "states" ]
                , mark Geoshape []
                , encoding (color [ MString "#eee" ] [])
                ]

        overlayEnc =
            encoding
                . position Longitude [ PName "longitude", PmType Quantitative ]
                . position Latitude [ PName "latitude", PmType Quantitative ]
                . size [ MNumber 5 ]
                . color [ MString "steelblue" ]

        overlaySpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/airports.csv" []
                , mark Circle []
                , overlayEnc []
                ]
    in
    toVegaLite
        [ cfg []
        , des
        , width 500
        , height 300
        , projection [ PrType AlbersUsa ]
        , layer [ backdropSpec, overlaySpec ]
        ]


geo4 :: VegaLite
geo4 =
  let backdropSpec =
        asSpec
        [ dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json" [ TopojsonFeature "states" ]
        , mark Geoshape []
        , encoding (color [ MString "#eee" ] [])
        ]

      airportsEnc =
        encoding
        . position Longitude [ PName "longitude", PmType Quantitative ]
        . position Latitude [ PName "latitude", PmType Quantitative ]
        . size [ MNumber 5 ]
        . color [ MString "gray" ]

      airportData =
        dataFromUrl "https://vega.github.io/vega-lite/data/airports.csv" []
        
      airportsSpec =
        asSpec
        [ airportData
        , mark Circle []
        , airportsEnc []
        ]

      trans =
        transform
        . filter (FEqual "origin" (Str "SEA"))
        . lookup "origin" airportData "iata" (LuAs "o")
        . lookup "destination" airportData "iata" (LuAs "d")

      flightsEnc = 
        encoding
        . position Longitude [ PName "o.longitude", PmType Quantitative ]
        . position Latitude [ PName "o.latitude", PmType Quantitative ]
        . position Longitude2 [ PName "d.longitude" ]
        . position Latitude2 [ PName "d.latitude" ]

      flightsSpec =
        asSpec
        [ dataFromUrl "https://vega.github.io/vega-lite/data/flights-airport.csv" []
        , trans []
        , mark Rule []
        , flightsEnc []
        ]

  in toVegaLite
        [ cfg []
        , description "Rules (line segments) connecting SEA to every airport reachable via direct flight"
        , width 800
        , height 500
        , projection [ PrType AlbersUsa ]
        , layer [ backdropSpec, airportsSpec, flightsSpec ]
        ]


geo5 :: VegaLite
geo5 =
    let
        enc =
            encoding
                . shape [ MName "geo", MmType GeoFeature ]
                . color [ MRepeat Row, MmType Quantitative, MSort [ Descending ] ]

        statesData =
          dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json"
                      [ TopojsonFeature "states" ]

        spec =
            asSpec
                [ width 500
                , height 300
                , dataFromUrl "https://vega.github.io/vega-lite/data/population_engineers_hurricanes.csv" []
                , transform (lookup "id" statesData "id" (LuAs "geo") [])
                , projection [ PrType AlbersUsa ]
                , mark Geoshape []
                , enc []
                ]
    in
    toVegaLite
        [ cfg []
        , description "Population per state, engineers per state, and hurricanes per state"
        , repeat [ RowFields [ "population", "engineers", "hurricanes" ] ]
        , resolve (resolution (RScale [ ( ChColor, Independent ) ]) [])
        , specification spec
        ]


geo6 :: VegaLite
geo6 =
    let
        des =
            description "US state capitals overlayed on map of the US"

        backdropSpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json" [ TopojsonFeature "states" ]
                , mark Geoshape []
                , encoding (color [ MString "#ccc" ] [])
                ]

        overlayEnc =
            encoding
                . position Longitude [ PName "lon", PmType Quantitative ]
                . position Latitude [ PName "lat", PmType Quantitative ]
                . text [ TName "city", TmType Nominal ]

        overlaySpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/us-state-capitals.json" []
                , mark Text []
                , overlayEnc []
                ]
    in
    toVegaLite
        [ cfg []
        , des
        , width 800
        , height 500
        , projection [ PrType AlbersUsa ]
        , layer [ backdropSpec, overlaySpec ]
        ]


geo7 :: VegaLite
geo7 =
  let backdropSpec =
        asSpec
        [ dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json" [ TopojsonFeature "states" ]
        , mark Geoshape []
        , encoding (color [ MString "#eee" ] [])
        ]

      airportsEnc =
        encoding
        . position Longitude [ PName "longitude", PmType Quantitative ]
        . position Latitude [ PName "latitude", PmType Quantitative ]
        . size [ MNumber 5 ]
        . color [ MString "gray" ]

      airportData =
        dataFromUrl "https://vega.github.io/vega-lite/data/airports.csv" []

      airportsSpec =
        asSpec
        [ airportData
        , mark Circle []
        , airportsEnc []
        ]

      itinerary =
        dataFromColumns []
        . dataColumn "airport" (Strings [ "SEA", "SFO", "LAX", "LAS", "DFW", "DEN", "ORD", "JFK", "ATL" ])
        . dataColumn "order" (Numbers [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])

      trans =
        transform
        . lookup "airport" airportData "iata"
            (LuFields [ "latitude", "longitude" ])

      flightsEnc =
        encoding
        . position Longitude [ PName "longitude", PmType Quantitative ]
        . position Latitude [ PName "latitude", PmType Quantitative ]
        . order [ OName "order" ]

      flightsSpec =
        asSpec
        [ itinerary []
        , trans []
        , mark Line []
        , flightsEnc []
        ]

  in toVegaLite
        [ cfg []
        , description "Line drawn between airports in the U.S. simulating a flight itinerary"
        , width 800
        , height 500
        , projection [ PrType AlbersUsa ]
        , layer [ backdropSpec, airportsSpec, flightsSpec ]
        ]


geo8 :: VegaLite
geo8 =
    let geoData =
          dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json" [ TopojsonFeature "states" ]

        enc =
            encoding
                . shape [ MName "geo", MmType GeoFeature ]
                . color [ MName "pct", MmType Quantitative, MSort [ Descending ] ]
                . row [ FName "group", FmType Nominal ]
    in
    toVegaLite
        [ cfg []
        , description "Income in the U.S. by state, faceted over income brackets"
        , width 500
        , height 300
        , dataFromUrl "https://vega.github.io/vega-lite/data/income.json" []
        , transform (lookup "id" geoData "id" (LuAs "geo") [])
        , projection [ PrType AlbersUsa ]
        , mark Geoshape []
        , enc []
        ]


geo9 :: VegaLite
geo9 =
    let
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

        polySpec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/londonBoroughs.json" [ TopojsonFeature "boroughs" ]
                , mark Geoshape [ MStroke "rgb(251,247,238)", MStrokeWidth 2 ]
                , encoding (color [ MString "#ddc" ] [])
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
        [ cfg []
        , description "Geographic position of London underground lines"
        , width 700
        , height 500
        , layer [ polySpec, labelSpec, routeSpec ]
        ]
