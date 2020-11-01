{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryMulti.elm (from development of version
-- 1.13.0)
--
module Gallery.Multi (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("multi1", multi1)
            , ("multi2", multi2)
            , ("multi3", multi3)
            , ("multi4", multi4)
            , ("multi5", multi5)
            , ("multi6", multi6)
            , ("multi7", multi7)
            , ("multi8", multi8)
            , ("selectandzoom", selectAndZoom)
            ]


multi1 :: VegaLite
multi1 =
    let
        des =
            description "Overview and detail."

        sel =
            selection . select "myBrush" Interval [ Encodings [ ChX ] ]

        enc1 =
            encoding
                . position X
                    [ PName "date"
                    , PmType Temporal
                    , PScale [ SDomainOpt (DSelection "myBrush") ]
                    , PAxis [ AxNoTitle ]
                    ]
                . position Y [ PName "price", PmType Quantitative ]

        spec1 =
            asSpec [ width 500, mark Area [], enc1 [] ]

        enc2 =
            encoding
                . position X [ PName "date", PmType Temporal, PAxis [ AxFormat "%Y" ] ]
                . position Y
                    [ PName "price"
                    , PmType Quantitative
                    , PAxis [ AxTickCount 3, AxGrid False ]
                    ]

        spec2 =
            asSpec [ width 480, height 60, sel [], mark Area [], enc2 [] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/sp500.csv" []
        , vConcat [ spec1, spec2 ]
        ]


multi2 :: VegaLite
multi2 =
    let
        des =
            description "Cross-filter."

        trans =
            transform
                . calculateAs "hours(datum.date)" "time"

        sel =
            selection . select "myBrush" Interval [ Encodings [ ChX ] ]

        selTrans =
            transform
                . filter (FSelection "myBrush")

        encPosition =
            encoding
                . position X
                    [ PRepeat Column
                    , PmType Quantitative
                    , PBin [ MaxBins 20 ]
                    ]
                . position Y [ PAggregate Count, PmType Quantitative ]

        spec1 =
            asSpec [ sel [], mark Bar [] ]

        spec2 =
            asSpec [ selTrans [], mark Bar [], encoding (color [ MString "goldenrod" ] []) ]

        spec =
            asSpec
                [ des
                , dataFromUrl "https://vega.github.io/vega-lite/data/flights-2k.json" [ Parse [ ( "date", FoDate "" ) ] ]
                , trans []
                , encPosition []
                , layer [ spec1, spec2 ]
                ]
    in
    toVegaLite
        [ repeat [ ColumnFields [ "distance", "delay", "time" ] ]
        , specification spec
        ]


multi3 :: VegaLite
multi3 =
    let
        des =
            description "Scatterplot matrix"

        sel =
            selection
                . select "myBrush"
                    Interval
                    [ On "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!"
                    , Translate "[mousedown[event.shiftKey], window:mouseup] > window:mousemove!"
                    , Zoom "wheel![event.shiftKey]"
                    , ResolveSelections Union
                    ]
                . select "grid"
                    Interval
                    [ BindScales
                    , Translate "[mousedown[!event.shiftKey], window:mouseup] > window:mousemove!"
                    , Zoom "wheel![event.shiftKey]"
                    , ResolveSelections Global
                    ]

        enc =
            encoding
                . position X [ PRepeat Column, PmType Quantitative ]
                . position Y [ PRepeat Row, PmType Quantitative ]
                . color
                    [ MSelectionCondition (SelectionName "myBrush")
                        [ MName "Origin", MmType Nominal ]
                        [ MString "grey" ]
                    ]

        spec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
                , mark Point []
                , sel []
                , enc []
                ]
    in
    toVegaLite
        [ des
        , repeat
            [ RowFields [ "Horsepower", "Acceleration", "Miles_per_Gallon" ]
            , ColumnFields [ "Miles_per_Gallon", "Acceleration", "Horsepower" ]
            ]
        , specification spec
        ]


multi4 :: VegaLite
multi4 =
    let
        des =
            description "A dashboard with cross-highlighting"

        selTrans =
            transform
                . filter (FSelection "myPts")

        encPosition =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PBin [ MaxBins 10 ] ]
                . position Y [ PName "Rotten_Tomatoes_Rating", PmType Quantitative, PBin [ MaxBins 10 ] ]

        colLegend = [ LTitle "All Movies"
                    , LDirection Horizontal
                    , LGradientLength 120
                    ]
        enc1 =
            encoding
                . color [ MAggregate Count, MmType Quantitative, MLegend colLegend ]

        -- removed width as not valid in Vega-Lite 4.1.1
        spec1 =
            asSpec [ {- width 300, -} mark Rect [], enc1 [] ]

        enc2 =
            encoding
                . size [ MAggregate Count, MmType Quantitative, MLegend [ LTitle "In Selected Category" ] ]
                . color [ MString "#666" ]

        spec2 =
            asSpec [ selTrans [], mark Point [], enc2 [] ]

        heatSpec =
            asSpec [ encPosition [], layer [ spec1, spec2 ] ]

        sel =
            selection . select "myPts" Single [ Encodings [ ChX ] ]

        barSpec =
            asSpec [ width 330, height 120, mark Bar [], sel [], encBar [] ]

        encBar =
            encoding
                . position X [ PName "Major_Genre", PmType Nominal, PAxis [ AxLabelAngle (-40) ] ]
                . position Y [ PAggregate Count, PmType Quantitative ]
                . color
                    [ MSelectionCondition (SelectionName "myPts")
                        [ MString "steelblue" ]
                        [ MString "grey" ]
                    ]

        config =
            configure . configuration (RangeStyle [ RHeatmap "greenblue" ])

        res =
            resolve
                . resolution (RLegend [ ( ChColor, Independent ), ( ChSize, Independent ) ])
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []
        , vConcat [ heatSpec, barSpec ]
        , res []
        , config []
        ]


multi5 :: VegaLite
multi5 =
    let
        des =
            description "A dashboard with cross-highlighting"

        spec1 =
            asSpec
                [ width 600, height 300, mark Point [], sel1 [], trans1 [], enc1 [] ]

        sel1 =
            selection . select "myBrush" Interval [ Encodings [ ChX ] ]

        trans1 =
            transform . filter (FSelection "myClick")

        weatherColors =
            categoricalDomainMap
                [ ( "sun", "#e7ba52" )
                , ( "fog", "#c7c7c7" )
                , ( "drizzle", "#aec7ea" )
                , ( "rain", "#1f77b4" )
                , ( "snow", "#9467bd" )
                ]

        enc1 =
            encoding
                . position X
                    [ PName "date"
                    , PmType Temporal
                    , PTimeUnit (TU MonthDate)
                    , PAxis [ AxTitle "Date", AxFormat "%b" ]
                    ]
                . position Y
                    [ PName "temp_max"
                    , PmType Quantitative
                    , PScale [ SDomain (DNumbers [ -5, 40 ]) ]
                    , PAxis [ AxTitle "Maximum Daily Temperature (C)" ]
                    ]
                . color
                    [ MSelectionCondition (SelectionName "myBrush")
                        [ MName "weather"
                        , MTitle "Weather"
                        , MmType Nominal
                        , MScale weatherColors
                        ]
                        [ MString "#cfdebe" ]
                    ]
                . size
                    [ MName "precipitation"
                    , MmType Quantitative
                    , MScale [ SDomain (DNumbers [ -1, 50 ]) ]
                    ]

        spec2 =
            asSpec [ width 600, mark Bar [], sel2 [], trans2 [], enc2 [] ]

        sel2 =
            selection . select "myClick" Multi [ Encodings [ ChColor ] ]

        trans2 =
            transform . filter (FSelection "myBrush")

        enc2 =
            encoding
                . position X [ PAggregate Count, PmType Quantitative ]
                . position Y [ PName "weather", PmType Nominal ]
                . color
                    [ MSelectionCondition (SelectionName "myClick")
                        [ MName "weather"
                        , MmType Nominal
                        , MScale weatherColors
                        ]
                        [ MString "#acbf98" ]
                    ]
    in
    toVegaLite
        [ title "Seattle Weather, 2012-2015" []
        , des
        , dataFromUrl "https://vega.github.io/vega-lite/data/seattle-weather.csv" []
        , vConcat [ spec1, spec2 ]
        ]


multi6 :: VegaLite
multi6 =
    let
        desc =
            description "Drag a rectangular brush to show (first 20) selected points in a table."

        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json"

        trans =
            transform
                . window [ ( [ WOp RowNumber ], "rowNumber" ) ] []

        sel =
            selection
                . select "brush" Interval []

        encPoint =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]
                . color
                    [ MSelectionCondition (SelectionName "brush")
                        [ MName "Cylinders", MmType Ordinal ]
                        [ MString "grey" ]
                    ]

        specPoint =
            asSpec [ sel [], mark Point [], encPoint [] ]

        tableTrans =
            transform
                . filter (FSelection "brush")
                . window [ ( [ WOp Rank ], "rank" ) ] []
                . filter (FLessThan "rank" (Number 20))

        encHPText =
            encoding
                . position Y [ PName "rowNumber", PmType Ordinal, PAxis [] ]
                . text [ TName "Horsepower", TmType Nominal ]

        specHPText =
            asSpec [ title "Engine power" [], tableTrans [], mark Text [], encHPText [] ]

        encMPGText =
            encoding
                . position Y [ PName "rowNumber", PmType Ordinal, PAxis [] ]
                . text [ TName "Miles_per_Gallon", TmType Nominal ]

        specMPGText =
            asSpec [ title "Efficiency (mpg)" [], tableTrans [], mark Text [], encMPGText [] ]

        encOriginText =
            encoding
                . position Y [ PName "rowNumber", PmType Ordinal, PAxis [] ]
                . text [ TName "Origin", TmType Nominal ]

        specOriginText =
            asSpec [ title "Country of origin" [], tableTrans [], mark Text [], encOriginText [] ]

        res =
            resolve
                . resolution (RLegend [ ( ChColor, Independent ) ])

        cfg =
            configure
                . configuration (ViewStyle [ ViewNoStroke ])
    in
    toVegaLite
        [ desc, cfg [], dvals [], trans [], res [], hConcat [ specPoint, specHPText, specMPGText, specOriginText ] ]


multi7 :: VegaLite
multi7 =
  let des = description "One dot per airport in the US overlayed on geoshape"

      cfg = configure
            . configuration (ViewStyle [ ViewNoStroke ])

      backdropSpec =
        asSpec
        [ dataFromUrl "https://vega.github.io/vega-lite/data/us-10m.json" [ TopojsonFeature "states" ]
        , mark Geoshape [ MFill "#ddd", MStroke "#fff" ]
        ]

      airportData =
        dataFromUrl "https://vega.github.io/vega-lite/data/airports.csv" []
      flightData =
        dataFromUrl "https://vega.github.io/vega-lite/data/flights-airport.csv" []

      lineTrans =
        transform
        . filter (FSelection "mySelection")
        . lookup "origin" airportData "iata" (LuAs "o")
        . lookup "destination" airportData "iata" (LuAs "d")

      lineEnc =
        encoding
        . position Longitude [ PName "o.longitude", PmType Quantitative ]
        . position Latitude [ PName "o.latitude", PmType Quantitative ]
        . position Longitude2 [ PName "d.longitude" ]
        . position Latitude2 [ PName "d.latitude" ]

      lineSpec =
        asSpec
        [ flightData
        , lineTrans []
        , lineEnc []
        , mark Rule [ MColor "black", MOpacity 0.35 ]
        ]

      airportTrans =
        transform
        . aggregate [ opAs Count "" "routes" ] [ "origin" ]
        . lookup "origin" airportData "iata"
              (LuFields [ "name", "state", "latitude", "longitude" ])
        . filter (FExpr "datum.state !== 'PR' && datum.state !== 'VI'")

      airportEnc =
        encoding
        . position Longitude [ PName "longitude", PmType Quantitative ]
        . position Latitude [ PName "latitude", PmType Quantitative ]
        . size [ MName "routes", MmType Quantitative, MScale [ SRange (RNumbers [ 0, 1000 ]) ], MLegend [] ]
        . order [ OName "routes", OmType Quantitative, OSort [ Descending ] ]
        . tooltip [ TName "name", TmType Nominal ]

      sel =
        selection
        . select "mySelection" Single [ On "mouseover", Nearest True, Empty, Fields [ "origin" ] ]

      airportSpec =
        asSpec
        [ flightData
        , airportTrans []
        , sel []
        , mark Circle []
        , airportEnc []
        ]

  in toVegaLite
        [ des
        , cfg []
        , width 900
        , height 500
        , projection [ PrType AlbersUsa ]
        , layer [ backdropSpec, lineSpec, airportSpec ]
        ]


multi8 :: VegaLite
multi8 =
  let dvals = dataFromUrl "https://vega.github.io/vega-lite/data/flights-5k.json"
              [ Parse [ ( "date", FoDate "" ) ] ]

      trans = transform
              . calculateAs "hours(datum.date) + minutes(datum.date) / 60" "time"
      sel = selection
            . select "brush" Interval [ Encodings [ ChX ]
                                      , SInitInterval
                                        (Just (Number 6, Number 12))
                                        Nothing
                                      ]

      enc1 = encoding
             . position X [ PName "time"
                          , PBin [ MaxBins 30 ]
                          , PmType Quantitative ]
             . position Y [ PAggregate Count, PmType Quantitative ]
      spec1 = asSpec [ width 963, height 100, sel [], enc1 [], mark Bar [] ]

      enc2 = encoding
             . position X [ PName "time"
                          , PBin [ MaxBins 30, SelectionExtent "brush" ]
                          , PmType Quantitative ]
             . position Y [ PAggregate Count, PmType Quantitative ]
      spec2 = asSpec [ width 963, height 100, enc2 [], mark Bar [] ]

  in toVegaLite [ dvals, trans [], vConcat [ spec1, spec2 ] ]


-- interval selection in one plot to zoom in in second plot.
--
selectAndZoom :: VegaLite
selectAndZoom =
  let plot1 = asSpec [ encoding
                       . position X [ PName "theta", PmType Quantitative, PAxis [] ]
                       . position Y [ PName "y", PmType Quantitative, PAxis [] ]
                       $ []
                     , mark Line []
                     , selection
                       . select "brush" Interval [ Encodings [ChX, ChY]
                                                 , SInitInterval (Just xr) (Just yr)
                                                 ]
                       $ []
                     ]

      xr = (Number 0.2, Number 6)
      yr = (Number (-0.8), Number 0.8)

      xscale = [ SDomainOpt (DSelectionField "brush" "theta") ]
      yscale = [ SDomainOpt (DSelectionChannel "brush" ChY) ]

      plot2 = asSpec [ encoding
                       . position X [ PName "theta", PmType Quantitative, PScale xscale ]
                       . position Y [ PName "y", PmType Quantitative, PScale yscale ]
                       $ []
                     , mark Circle []
                     ]

  in toVegaLite [ dataSequenceAs 0 6.28 0.1 "theta"
                , transform
                  . calculateAs "cos(datum.theta)" "y"
                  $ []
                , vConcat [plot1, plot2]
                ]
