{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite InteractionTests.elm as of version 1.12.0
-- Note that the numbering / naming has diverged from Elm over
-- time (I want to keep the old tests the same name)
--
module InteractionTests (testSpecs) where

import Graphics.Vega.VegaLite

import Prelude hiding (filter)


testSpecs :: [(String, VegaLite)]
testSpecs = [ ("interaction1", interaction1)
            , ("interaction2", interaction2)
            , ("interaction3", interaction3)
            , ("interaction4", interaction4)
            , ("interaction5", interaction5)
            , ("interaction6", interaction6)
            , ("interaction7", interaction7)
            , ("interaction8", interaction8)
            , ("initinterval", initInterval)
            , ("initintervalxy", initIntervalXY)
            , ("initintervaly", initIntervalY)
            , ("initintervalx", initIntervalX)
            , ("bindlegend", bindLegend)
            , ("bindlegend2", bindLegendDouble)
            , ("bindlegendboth", bindLegendBoth)
            , ("lookupSelection1", lookupSelection1)
            , ("curpointer", curPointer)
            , ("curhelp", curHelp)
            ]


crimeData :: Data
crimeData = dataFromUrl "https://gicentre.github.io/data/westMidlands/westMidsCrimesAggregated.tsv" []


cScale :: [ScaleProperty]
cScale =
    categoricalDomainMap
        [ ( "Anti-social behaviour", "rgb(59,118,175)" )
        , ( "Burglary", "rgb(81,157,62)" )
        , ( "Criminal damage and arson", "rgb(141,106,184)" )
        , ( "Drugs", "rgb(239,133,55)" )
        , ( "Robbery", "rgb(132,88,78)" )
        , ( "Vehicle crime", "rgb(213,126,190)" )
        ]


month, crimes :: BuildEncodingSpecs
month = position X [ PName "month", PmType Temporal, PNoTitle ]
crimes = position Y [ PName "reportedCrimes", PmType Quantitative, PTitle "Reported crimes" ]

enc, encHighlight :: [EncodingSpec] -> PropertySpec
enc =
    encoding
        . month
        . crimes
        . color [ MName "crimeType", MmType Nominal, MScale cScale ]


encHighlight =
    encoding
        . month
        . crimes
        . color
            [ MSelectionCondition (SelectionName "mySelection")
                [ MName "crimeType", MmType Nominal, MScale cScale ]
                [ MString "black" ]
            ]
        . opacity
            [ MSelectionCondition (SelectionName "mySelection")
                [ MNumber 1 ]
                [ MNumber 0.1 ]
            ]


iPlot :: ([SelectSpec] -> PropertySpec) -> Mark -> VegaLite
iPlot sel markType =
  toVegaLite [ width 540, crimeData, sel [], encHighlight [], mark markType [] ]


interaction1 :: VegaLite
interaction1 =
    let sel = selection . select "mySelection" Single []
    in iPlot sel Line


interaction2 :: VegaLite
interaction2 =
    let sel = selection . select "mySelection" Multi []
    in iPlot sel Line


interaction3 :: VegaLite
interaction3 =
    let sel = selection
                . select "mySelection" Single [ Nearest True, Fields [ "crimeType" ] ]
    in iPlot sel Circle


interaction4 :: VegaLite
interaction4 =
    let sel = selection
                . select "mySelection" Interval [ Empty, Encodings [ ChX ] ]
    in iPlot sel Circle


interaction5 :: VegaLite
interaction5 =
    let sel = selection
                . select "mySelection" Interval [ BindScales, Encodings [ ChX ] ]
    in iPlot sel Circle


interaction6 :: VegaLite
interaction6 =
    let sel = selection
                . select "mySelection" Interval [ BindScales ]
    in iPlot sel Circle


interaction7 :: VegaLite
interaction7 =
    let sel = selection
                . select "mySelection"
                    Single
                    [ Fields [ "crimeType" ]
                    , Nearest True
                    , Bind
                        [ IRadio "crimeType"
                            [ InName " "
                            , InOptions
                                [ "Anti-social behaviour"
                                , "Criminal damage and arson"
                                , "Drugs"
                                , "Robbery"
                                , "Vehicle crime"
                                ]
                            ]
                        ]
                    ]

    in iPlot sel Circle


interaction8 :: VegaLite
interaction8 =
    let sel = selection
                . select "maxSlider"
                    Single
                    [ SInit [ ( "maxReported", Number 14000 ) ]
                    , Bind [ IRange "maxReported" [ InName "Max", InMin 400, InMax 14000 ] ]
                    ]
                . select "minSlider"
                    Single
                    [ SInit [ ( "minReported", Number 0 ) ]
                    , Bind [ IRange "minReported" [ InName "Min", InMax 12800 ] ]
                    ]

        trans = transform
                . filter (FExpr "datum.reportedCrimes >= minSlider_minReported && maxSlider_maxReported >= datum.reportedCrimes")

    in toVegaLite [ width 540, crimeData, trans [], sel [], enc [], mark Circle [] ]


-- Note that an interval of Nothing Nothing can not be encoded in Vega-Lite 4.0.2,
-- so this is a check we don't create invalid output.
--
initInterval :: VegaLite
initInterval =
  let sel = selection
              . select "mySelection"
                  Interval
                  [ SInitInterval Nothing Nothing ]

  in iPlot sel Circle


initIntervalXY :: VegaLite
initIntervalXY =
  let sel = selection
              . select "mySelection"
                  Interval
                  [ SInitInterval
                    (Just ( DateTime [ DTYear 2013 ], DateTime [ DTYear 2015 ] ))
                    (Just ( Number 4000, Number 8000 ))
                  ]

  in iPlot sel Circle


initIntervalY :: VegaLite
initIntervalY =
  let sel = selection
              . select "mySelection"
                  Interval
                  [ SInitInterval
                    Nothing
                    (Just ( Number 4000, Number 8000 ))
                  ]

  in iPlot sel Circle


initIntervalX :: VegaLite
initIntervalX =
  let sel = selection
              . select "mySelection"
                  Interval
                  [ Encodings [ ChX ]
                  , SInitInterval
                    (Just ( DateTime [ DTYear 2013 ], DateTime [ DTYear 2015 ] ))
                    Nothing
                  ]

  in iPlot sel Circle


bindLegend :: VegaLite
bindLegend =
  let sel = selection
             . select "mySelection" Single [ BindLegend (BLField "crimeType") ]

  in iPlot sel Circle


bindLegendDouble :: VegaLite
bindLegendDouble =
  let sel = selection
             . select "mySelection" Single
               [ BindLegend (BLChannelEvent ChColor "dblclick") ]

  in iPlot sel Circle


bindLegendBoth :: VegaLite
bindLegendBoth =
  let sel = selection
             . select "mySelection"
               Multi
               [ On "click"
               , BindLegend (BLFieldEvent "crimeType" "dblclick")
               ]

  in iPlot sel Circle


-- lookup into a selection
-- based on https://vega.github.io/vega-lite/docs/lookup.html#lookup-selection
--

csvStocks :: Data
csvStocks = dataFromUrl "https://vega.github.io/vega-lite/data/stocks.csv"
                 [ CSV, Parse [("date", FoDate "")] ]


lookupSelection1 :: VegaLite
lookupSelection1 =
  let xaxis = position X [PName "date", PmType Temporal, PAxis []]

      t0 = DateTime [DTYear 2005, DTMonthNum 1, DTDate 1]
      selPoint = selection
                 . select "index" Single
                          [ On "mouseover"
                          , Encodings [ChX]
                          , Nearest True
                          , SInit [("x", t0)]
                          ]
      encPoint = encoding
                 . xaxis
                 . opacity [MNumber 0]
      pointSpec = [ encPoint [], selPoint [], mark Point [] ]

      encLine = encoding
                . xaxis
                . position Y [ PName "indexed_price"
                             , PmType Quantitative
                             , PAxis [AxFormat "%"]
                             ]
                . color [ MName "symbol", MmType Nominal]
      transLine = transform
                  . lookupSelection "symbol" "index" "symbol"
                  . calculateAs
                    "datum.index && datum.index.price > 0 ? (datum.price - datum.index.price)/datum.index.price : 0"
                    "indexed_price"
      lineSpec = [ encLine [], transLine [], mark Line [] ]

      encRule = encoding
                . xaxis
                . color [MString "firebrick"]
      transRule = transform
                  . filter (FSelection "index")
      layerRule = layer [ asSpec [ mark Rule [MStrokeWidth 0.5] ]
                        , asSpec [ mark Text [ MAlign AlignCenter
                                             , MFontWeight W100 ]
                                 , encoding
                                   . text [ TName "date"
                                          , TmType Temporal
                                          , TTimeUnit (TU YearMonth)
                                          ]
                                   . position Y [PNumber 310]
                                   $ []
                                 ]
                        ]
      ruleSpec = [ encRule [], transRule [], layerRule ]

      layers = layer (map asSpec [pointSpec, lineSpec, ruleSpec])

  in toVegaLite
     [ csvStocks
     , width 650
     , height 300
     , layers
     ]


showCursor :: Cursor -> VegaLite
showCursor cur =
  let cfg = configure
            . configuration (ViewStyle [ ViewCursor CText ])

      trans = transform
              . filter (FExpr "datum.symbol==='GOOG'")

      sel = selection
            . select "myBrush"
                     Interval
                     [ Encodings [ ChX ]
                     , SelectionMark [ SMCursor cur ]
                     ]

      encLine = encoding
                . position X [PName "date", PmType Temporal]
                . position Y [PName "price", PmType Quantitative]

  in toVegaLite [ width 400
                , cfg []
                , csvStocks
                , trans []
                , sel []
                , encLine []
                , mark Line []
                ]


curPointer, curHelp :: VegaLite
curPointer = showCursor CPointer
curHelp = showCursor CHelp
