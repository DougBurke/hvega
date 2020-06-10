{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite GalleryRepeat.elm (from development of version
-- 1.13.0)
--
module Gallery.Repeat (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter, lookup, repeat)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("repeat1", repeat1)
            , ("repeat2", repeat2)
            , ("repeat3", repeat3)
            , ("repeat4", repeat4)
            , ("repeat5", repeat5)
            , ("nested_concat_align", nestedConcatAlign)
            ]


movieData :: Data
movieData = dataFromUrl "https://vega.github.io/vega-lite/data/movies.json" []


repeat1 :: VegaLite
repeat1 =
    let
        des =
            description "Monthly weather information for individual years and overall average for Seatle and New York"

        enc1 =
            encoding
                . position X [ PName "date", PmType Ordinal, PTimeUnit (TU Month) ]
                . position Y [ PRepeat Column, PmType Quantitative, PAggregate Mean ]
                . detail [ DName "date", DmType Temporal, DTimeUnit (TU Year) ]
                . color [ MName "location", MmType Nominal ]
                . opacity [ MNumber 0.2 ]

        spec1 =
            asSpec [ mark Line [], enc1 [] ]

        enc2 =
            encoding
                . position X [ PName "date", PmType Ordinal, PTimeUnit (TU Month) ]
                . position Y [ PRepeat Column, PmType Quantitative, PAggregate Mean ]
                . color [ MName "location", MmType Nominal ]

        spec2 =
            asSpec [ mark Line [], enc2 [] ]

        spec =
            asSpec [ layer [ spec1, spec2 ] ]
    in
    toVegaLite
        [ des
        , dataFromUrl "https://vega.github.io/vega-lite/data/weather.csv" []
        , repeat [ ColumnFields [ "temp_max", "precipitation", "wind" ] ]
        , specification spec
        ]


repeat2 :: VegaLite
repeat2 =
    let
        desc =
            description "Two vertically concatenated charts that show a histogram of precipitation in Seattle and the relationship between min and max temperature"

        trans =
            transform
                . filter (FExpr "datum.location === 'Seattle'")

        enc1 =
            encoding
                . position X [ PName "date", PTimeUnit (TU Month), PmType Ordinal ]
                . position Y [ PName "precipitation", PmType Quantitative, PAggregate Mean ]

        spec1 =
            asSpec [ mark Bar [], enc1 [] ]

        enc2 =
            encoding
                . position X [ PName "temp_min", PmType Quantitative, PBin [] ]
                . position Y [ PName "temp_max", PmType Quantitative, PBin [] ]
                . size [ MAggregate Count, MmType Quantitative ]

        spec2 =
            asSpec [ mark Point [], enc2 [] ]
    in
    toVegaLite
        [ desc
        , trans []
        , dataFromUrl "https://vega.github.io/vega-lite/data/weather.csv" []
        , vConcat [ spec1, spec2 ]
        ]


repeat3 :: VegaLite
repeat3 =
    let
        des =
            description "Horizontally repeated charts that show the histograms of different parameters of cars in different countries"

        enc =
            encoding
                . position X [ PRepeat Column, PmType Quantitative, PBin [] ]
                . position Y [ PmType Quantitative, PAggregate Count ]
                . color [ MName "Origin", MmType Nominal ]

        spec =
            asSpec
                [ dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []
                , mark Bar []
                , enc []
                ]
    in
    toVegaLite
        [ des
        , repeat [ ColumnFields [ "Horsepower", "Miles_per_Gallon", "Acceleration" ] ]
        , specification spec
        ]


repeat4 :: VegaLite
repeat4 =
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


repeat5 :: VegaLite
repeat5 =
    let
        des =
            description "Marginal histograms show the counts along the x and y dimension"

        config =
            configure
                . configuration (RangeStyle [ RHeatmap "greenblue" ])
                . configuration (ViewStyle [ ViewNoStroke ])

        enc1 =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PAxis [], PBin [] ]
                . position Y
                    [ PAggregate Count
                    , PmType Quantitative
                    , PScale [ SDomain (DNumbers [ 0, 1000 ]) ]
                    , PAxis [ AxNoTitle ]
                    ]

        spec1 =
            asSpec [ height 60, mark Bar [], enc1 [] ]

        spec2 =
            asSpec [ spacing 15, bounds Flush, hConcat [ spec2_1, spec2_2 ] ]

        enc2_1 =
            encoding
                . position X [ PName "IMDB_Rating", PmType Quantitative, PBin [] ]
                . position Y [ PName "Rotten_Tomatoes_Rating", PmType Quantitative, PBin [] ]
                . color [ MAggregate Count, MmType Quantitative ]

        spec2_1 =
            asSpec [ mark Rect [], enc2_1 [] ]

        enc2_2 =
            encoding
                . position Y
                    [ PName "Rotten_Tomatoes_Rating"
                    , PmType Quantitative
                    , PBin []
                    , PAxis []
                    ]
                . position X
                    [ PAggregate Count
                    , PmType Quantitative
                    , PScale [ SDomain (DNumbers [ 0, 1000 ]) ]
                    , PAxis [ AxNoTitle ]
                    ]

        spec2_2 =
            asSpec [ width 60, mark Bar [], enc2_2 [] ]
    in
    toVegaLite
        [ des
        , spacing 15
        , bounds Flush
        , config []
        , movieData
        , vConcat [ spec1, spec2 ]
        ]


--https://vega.github.io/vega-lite/examples/nested_concat_align.html
nestedConcatAlign :: VegaLite
nestedConcatAlign =
  let desc = "Nested concatenation aligned by setting axis minExtent"

      plot1 = [ title "Ratings" []
              , repeat [ColumnFields ["Rotten_Tomatoes_Rating", "IMDB_Rating"]]
              , specification (asSpec spec)
              ]
              
      plot2 = [ title "Gross" []
              , repeat [ColumnFields ["US_Gross", "Worldwide_Gross"]]
              , specification (asSpec spec)
              ]

      spec = [ width 150
             , height 50
             , mark Bar []
             , encoding
               . position X [ PRepeat Column
                            , PBin [MaxBins 20]
                            , PmType Quantitative
                            ]
               . position Y [ PAggregate Count
                            , PmType Quantitative
                            ]
               $ []
             ]

  in toVegaLite [ description desc
                , movieData
                , vConcat [asSpec plot1, asSpec plot2]
                , configure
                  . configuration (CountTitleStyle "Count")
                  . configuration (AxisX [TitleLimit 150])
                  . configuration (AxisY [MinExtent 40])
                  $ []
                ]
