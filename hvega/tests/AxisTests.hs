{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite AxisTests.elm in the 2.0 development version
--
module AxisTests (testSpecs) where

import qualified Data.Text as T

import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("axis1", axis1)
            , ("axis1c", axis1c)
            , ("axis2", axis2)
            , ("axis2c", axis2c)
            , ("axis3", axis3)
            , ("axis3c", axis3c)
            , ("axis4", axis4)
            , ("axis4c", axis4c)
            , ("axis5", axis5)
            , ("axis5c", axis5c)
            , ("axis6", axis6)
            , ("axis6c", axis6c)
            , ("axis7", axis7)
            , ("axis7c", axis7c)
            , ("axis8", axis8)
            , ("axis8c", axis8c)
            , ("axisOverlapNone", axisOverlapNone)
            , ("axisOverlapParity", axisOverlapParity)
            , ("axisOverlapGreedy", axisOverlapGreedy)
            , ("zorder", zorder)
            , ("responsiveWidth", responsiveWidth)
            , ("responsiveHeight", responsiveHeight)
            , ("axisstyleempty", axisStyleEmpty)
            , ("axisstyleemptyx", axisStyleEmptyX)
            , ("axisstylex", axisStyleX)
            , ("axisstylexastyle", axisStyleXAStyle)
            , ("axisstylexy", axisStyleXY)
            , ("singleline", singleLine)
            , ("multiline", multiLine)
            ]


-- We do not provide these in hvega, so define them here to make copying
-- the Elm tests over easier.
--
pOrdinal, pQuant, pTemporal :: PositionChannel
pOrdinal = PmType Ordinal
pQuant = PmType Quantitative
pTemporal = PmType Temporal

pName :: T.Text -> PositionChannel
pName = PName


simpleData :: Data
simpleData =
  let xvals = map fromIntegral xs
      xs = [1::Int .. 100]
  in dataFromColumns []
       . dataColumn "x" (Numbers xvals)
       . dataColumn "catX" (Strings (map (T.pack . show) xs))
       . dataColumn "y" (Numbers xvals)
       $ []


temporalData ::  Data
temporalData =
  let dates = [ "2019-01-01 09:00:00"
              , "2019-01-02 09:00:00"
              , "2019-01-03 09:00:00"
              , "2019-01-04 09:00:00"
              , "2019-01-05 09:00:00"
              , "2019-01-06 09:00:00"
              , "2019-01-07 09:00:00"
              , "2019-01-08 09:00:00"
              , "2019-01-09 09:00:00"
              , "2019-01-10 09:00:00"
              , "2019-01-11 09:00:00"
              , "2019-01-12 09:00:00"
              ]

      xs = map fromIntegral [1::Int .. 12]

  in dataFromColumns []
     . dataColumn "date" (Strings dates)
     . dataColumn "y" (Numbers xs)
     $ []

xQuant, yQuant, catX, xDate :: [PositionChannel]
xQuant = [pName "x", pQuant]
yQuant = [pName "y", pQuant]
catX = [pName "catX", pOrdinal]
xDate = [pName "date", pTemporal]


axisBase :: Data -> [ConfigurationProperty] -> [PositionChannel] -> [PositionChannel] -> VegaLite
axisBase plotData confOpts xOpts yOpts =
  let enc = encoding . position X xOpts . position Y yOpts

      conf = case confOpts of
               [] -> []
               _ -> [configure (foldr configuration [] confOpts)]
      vs = conf ++ [ plotData, enc [], mark Line [ MPoint (PMMarker []) ] ]

  in toVegaLite vs

plotCfg :: [ConfigurationProperty]
plotCfg = [ AxisQuantitative AxXY [ DomainColor "orange"
                                  , GridColor "seagreen"
                                  , LabelFont "Comic Sans MS"
                                  , LabelOffset 10
                                  , TickOffset 10
                                  ]
          , AxisTemporal AxXY [ DomainColor "brown"
                              , DomainDash [4, 2]
                              , Grid False
                              , LabelColor "purple"
                              ]
          , PointStyle [ MStroke "black"
                       , MStrokeOpacity 0.4
                       , MStrokeWidth 1
                       , MFill "yellow"
                       ]
          , LineStyle [ MStroke "gray"
                      , MStrokeWidth 2
                      ]
          ]


axis1, axis1c, axis2, axis2c, axis3, axis3c, axis4, axis4c,
  axis5, axis5c, axis6, axis6c, axis7, axis7c, axis8, axis8c :: VegaLite
axis1 = axisBase simpleData [] xQuant yQuant
axis1c = axisBase simpleData plotCfg xQuant yQuant
axis2 = axisBase simpleData [] catX yQuant
axis2c = axisBase simpleData plotCfg catX yQuant
axis3 = axisBase simpleData [] xDate yQuant
axis3c = axisBase simpleData plotCfg xDate yQuant
axis4 =
  let x = PAxis [AxValues (Numbers [1, 25, 39, 90])] : xQuant
  in axisBase simpleData [] x yQuant
axis4c =
  let x = PAxis [AxValues (Numbers [1, 25, 39, 90])] : xQuant
  in axisBase simpleData plotCfg x yQuant
axis5 =
  let x = PAxis [AxValues (Strings ["1", "25", "39", "dummy", "90"])] : catX
  in axisBase simpleData [] x yQuant
axis5c =
  let x = PAxis [AxValues (Strings ["1", "25", "39", "dummy", "90"])] : catX
  in axisBase simpleData plotCfg x yQuant
axis6 =
  let x = PAxis [AxValues (DateTimes axDates)] : xDate

      axDates = [ [DTYear 2019, DTMonth Jan, DTDate 4 ]
                , [DTYear 2019, DTMonth Jan, DTDate 8 ]
                , [DTYear 2019, DTMonth Jan, DTDate 20 ]
                ]

  in axisBase temporalData [] x yQuant
axis6c =
  let x = PAxis [AxValues (DateTimes axDates)] : xDate

      axDates = [ [DTYear 2019, DTMonth Jan, DTDate 4 ]
                , [DTYear 2019, DTMonth Jan, DTDate 8 ]
                , [DTYear 2019, DTMonth Jan, DTDate 20 ]
                ]

  in axisBase temporalData plotCfg x yQuant
axis7 =
  let x = PAxis [AxLabelExpr "datum.value / 100"] : xQuant
  in axisBase simpleData [] x yQuant
axis7c =
  let x = PAxis [AxLabelExpr "datum.value / 100"] : xQuant
  in axisBase simpleData plotCfg x yQuant
axis8 =
  let x = PAxis [AxLabelExpr "'number' + datum.label"] : catX
  in axisBase simpleData [] x yQuant
axis8c =
  let x = PAxis [AxLabelExpr "'number' + datum.label"] : catX
  in axisBase simpleData plotCfg x yQuant


overlap :: OverlapStrategy -> VegaLite
overlap strat =
  let dvals = dataFromColumns []
              . dataColumn "x" (Numbers [ 0.1, 0.11, 0.2, 0.21, 0.5 ])
              . dataColumn "y" (Numbers [ 100, 101, 102, 103, 101 ])

      axisOpts = PAxis [ AxLabelOverlap strat
                       , AxLabelFontSize 20
                       ]

      enc = encoding
            . position X [ PName "x", PmType Quantitative, axisOpts ]
            . position Y [ PName "y", PmType Quantitative, axisOpts ]

  in toVegaLite [ dvals [], enc [], mark Circle [] ]


axisOverlapNone :: VegaLite
axisOverlapNone = overlap ONone

axisOverlapParity :: VegaLite
axisOverlapParity = overlap OParity

axisOverlapGreedy :: VegaLite
axisOverlapGreedy = overlap OGreedy


-- From
-- https://github.com/gicentre/elm-vegalite/issues/15#issuecomment-524527125
--
zorder :: VegaLite
zorder =
  let dcols = dataFromColumns []
              . dataColumn "x" (Numbers [ 20, 10 ])
              . dataColumn "y" (Numbers [ 10, 20 ])
              . dataColumn "cat" (Strings [ "a", "b" ])

      axis lbl z = [ PName lbl, PmType Quantitative, PAxis [ AxZIndex z ] ]
      enc = encoding
            . position X (axis "x" 2)
            . position Y (axis "y" 1)
            . color [ MName "cat", MmType Nominal, MLegend [] ]

      cfg = configure
            . configuration (Axis [ GridWidth 8 ])
            . configuration (AxisX [ GridColor "red" ])
            . configuration (AxisY [ GridColor "blue" ])

  in toVegaLite [ cfg []
                , dcols []
                , enc []
                , mark Circle [ MSize 5000, MOpacity 1 ]
                ]


responsive :: PropertySpec -> VegaLite
responsive prop =
  let enc = encoding
            . position X [PName "x", PmType Quantitative]
            . position Y [PName "y", PmType Quantitative]

  in toVegaLite [ prop, simpleData, enc []
                , mark Line [MPoint (PMMarker [])] ]

responsiveWidth, responsiveHeight :: VegaLite
responsiveWidth = responsive widthOfContainer
responsiveHeight = responsive heightOfContainer


carData :: Data
carData = dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []


carEnc :: [AxisProperty] -> [AxisProperty] -> PropertySpec
carEnc xOpts yOpts =
  let toAxis n l o = position n ([ PName l, PmType Quantitative ]
                                 ++ if null o
                                    then []
                                    else [PAxis o])

  in encoding
     . toAxis X "Horsepower" xOpts
     . toAxis Y "Miles_per_Gallon" yOpts
     . color [ MName "Origin", MmType Nominal, MLegend [] ]
     $ []

axisStyleEmpty :: VegaLite
axisStyleEmpty =
  let cfg = configure
            . configuration (AxisNamedStyles [])

  in toVegaLite [ cfg []
                , carData
                , carEnc [] []
                , mark Point []
                ]

axisStyleEmptyX :: VegaLite
axisStyleEmptyX =
  let cfg = configure
            . configuration (AxisNamedStyles [("x-style", [])])

  in toVegaLite [ cfg []
                , carData
                , carEnc [AxStyle ["x-style"]] []
                , mark Point []
                ]


axisStyleX :: VegaLite
axisStyleX =
  let cfg = configure
            . configuration (AxisNamedStyles [("x-style", [ AxDomainColor "orange"
                                                          , AxGridColor "lightgreen"
                                                          , AxLabelExpr xexpr ])])

      xexpr = "if (datum.value <= 100, 'low:' + datum.label, 'high:' + datum.label)"

  in toVegaLite [ cfg []
                , carData
                , carEnc [AxStyle ["x-style"]] []
                , mark Point []
                ]


-- check AStyle; should give same look as axisStyleX
axisStyleXAStyle :: VegaLite
axisStyleXAStyle =
  let cfg = configure
            . configuration (AxisNamedStyles [("x-style", [ AxDomainColor "orange"
                                                          , AxGridColor "lightgreen"
                                                          , AxLabelExpr xexpr ])])
            . configuration (AxisX [AStyle ["x-style"]])

      xexpr = "if (datum.value <= 100, 'low:' + datum.label, 'high:' + datum.label)"

  in toVegaLite [ cfg []
                , carData
                , carEnc [] []
                , mark Point []
                ]


axisStyleXY :: VegaLite
axisStyleXY =
  let cfg = configure
            . configuration (AxisNamedStyles [ ("x-style", [ AxDomainColor "orange"
                                                           , AxGridColor "lightgreen"
                                                           , AxLabelExpr xexpr ])
                                             , ("y-style", [ AxDomain False
                                                           , AxGrid False
                                                           , AxLabels False
                                                           , AxTicks False
                                                           , AxNoTitle ])
                                             ])

      xexpr = "if (datum.value <= 100, 'low:' + datum.label, 'high:' + datum.label)"

  in toVegaLite [ cfg []
                , carData
                , carEnc [AxStyle ["x-style"]] [AxStyle ["y-style"]]
                , mark Point []
                ]


singleLine :: VegaLite
singleLine =
  let xOpts = [ AxLabelExpr "datum.label + ' horses'" ]
      yOpts = [ AxLabelExpr "datum.label+' mpg'" ]

  in toVegaLite [ carData
                , carEnc xOpts yOpts
                , mark Point []
                ]


multiLine :: VegaLite
multiLine =
  let xOpts = [ AxLabelExpr "datum.label + ' horses'"
              , AxLabelLineHeight 22
              , AxLabelFontSize 11
              ]
      yOpts = [ AxLabelExpr "datum.label+' mpg'"
              , AxLabelFontSize 22 ]

  in toVegaLite [ carData
                , carEnc xOpts yOpts
                , mark Point []
                , configure
                  . configuration (LineBreakStyle " ")
                  . configuration (Axis [LabelLineHeight 20])
                  $ []
                ]
