{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite DataTests.elm as of version 1.12.0
--
module DataTests (testSpecs) where

import qualified Data.Aeson as A
import qualified Data.Text as T

import Data.Aeson ((.=))
import Data.Function ((&))
import Graphics.Vega.VegaLite

import Prelude hiding (filter)

testSpecs :: [(String, VegaLite)]
testSpecs = [ ("data1", data1)
            , ("data2", data2)
            , ("data3", data3)
            , ("data4", data4)
            , ("data5", data5)
            , ("data6", data6)
            , ("data7", data7)
            , ("data8", data8)
            , ("data9", data9)
            , ("data10", data10)
            -- , ("data11", data11)
            , ("namedData1", namedData1)
            , ("namedData2", namedData2)
            , ("namedData3", namedData3)
            , ("geodata1", geodata1)
            , ("geodata2", geodata2)
            , ("flatten1", flatten1)
            , ("fold1", fold1)
            , ("pivot1", pivot1)
            , ("pivotDocs", pivotDocs)
            , ("impute1", impute1)
            , ("impute2", impute2)
            , ("impute3", impute3)
            , ("impute4", impute4)
            , ("impute5", impute5)
            , ("impute6", impute6)
            , ("impute7", impute7)
            , ("impute8", impute8)
            , ("sample1", sample1)
            , ("bin1", bin1)
            , ("bin2", bin2)
            , ("bin3", bin3)
            , ("sequence1", sequence1)
            , ("sequence2", sequence2)
            , ("filter1", filter1)
            , ("filter2", filter2)
            , ("annotate1", annotate1)
            , ("null1", null1)
            , ("domain1", domain1)
            , ("domain2", domain2)
            , ("domain3", domain3)
            -- , ("key1", key1)
            , ("argmin_spaces", argminSpaces)
            ]


-- We do not provide these in hvega, so define them here to make copying
-- the Elm tests over easier.
--
pNominal, pOrdinal, pQuant :: PositionChannel
pNominal = PmType Nominal
pOrdinal = PmType Ordinal
pQuant = PmType Quantitative

pName :: T.Text -> PositionChannel
pName = PName


showData :: (VLProperty, VLSpec) -> VegaLite
showData dvals =
    let
        enc =
            encoding
                . position X [ PName "cat", PmType Nominal ]
                . position Y [ PName "val", PmType Quantitative ]
    in
    toVegaLite [ dvals, enc [], mark Bar [] ]

data1 :: VegaLite
data1 =
    let
        dvals =
            dataFromColumns []
                . dataColumn "cat" (Strings [ "a", "b", "c" ])
                . dataColumn "val" (Numbers [ 10, 18, 12 ])
    in
    showData (dvals [])


data2 :: VegaLite
data2 =
    let
        dvals =
            dataFromRows []
                . dataRow [ ( "cat", Str "a" ), ( "val", Number 10 ) ]
                . dataRow [ ( "cat", Str "b" ), ( "val", Number 18 ) ]
                . dataRow [ ( "cat", Str "c" ), ( "val", Number 12 ) ]
    in
    showData (dvals [])


json :: A.Value
json =
  let cv :: String -> Int -> A.Value
      cv c v = A.object [ "cat" .= c, "val" .= v]
      
  in A.toJSON [ cv "a" 10
              , cv "b" 18
              , cv "c" 12 ]

     
data3 :: VegaLite
data3 = showData (dataFromJson json [])


data4 :: VegaLite
data4 =
    showData (dataFromUrl "data/dataTest.csv" [])


data5 :: VegaLite
data5 =
    showData (dataFromUrl "data/dataTest.tsv" [])


data6 :: VegaLite
data6 =
    showData (dataFromUrl "data/dataTest.csv" [ DSV ',' ])


data7 :: VegaLite
data7 =
    showData (dataFromUrl "data/dataTest.json" [])

dataSource :: T.Text -> VegaLite
dataSource dname =
    let
        dataColumns =
            dataFromColumns []
                . dataColumn "cat" (Strings [ "a", "b", "c" ])
                . dataColumn "val" (Numbers [ 10, 18, 12 ])

        dataRows =
            dataFromRows []
                . dataRow [ ( "cat", Str "a" ), ( "val", Number 10 ) ]
                . dataRow [ ( "cat", Str "b" ), ( "val", Number 18 ) ]
                . dataRow [ ( "cat", Str "c" ), ( "val", Number 12 ) ]

        enc =
            encoding
                . position X [ PName "cat", PmType Nominal ]
                . position Y [ PName "val", PmType Quantitative ]
    in
    toVegaLite
        [ datasets
            [ ( "myData1", dataRows [] )
            , ( "myData2", dataColumns [] )
            , ( "myData3", dataFromJson json [] )
            ]
        , dataFromSource dname []
        , enc []
        , mark Bar []
        ]


data8 :: VegaLite
data8 = dataSource "myData1"


data9 :: VegaLite
data9 = dataSource "myData2"


data10 :: VegaLite
data10 = dataSource "myData3"


{-
-- TODO no arrow support

data11 =
    let
        pollData =
            dataFromUrl "https://gicentre.github.io/data/euPolls.arrow" [ arrow ]

        enc =
            encoding
                . position X [ PName "Answer", PmType Nominal ]
                . position Y [ PName "Percent", PmType Quantitative, pAggregate opMean ]
                . color [ mName "Pollster", MmType Nominal ]
                . column [ fName "Pollster", fMType Nominal ]
    in
    toVegaLite [ pollData, enc [], mark Bar [] ]

-}


namedData1, namedData2, namedData3 :: VegaLite
namedData1 =
    let
        dvals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C", "D", "E", "F", "G", "H", "I" ])
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        enc =
            encoding
                . position X [ PName "a", PmType Ordinal ]
                . position Y [ PName "b", PmType Quantitative ]
    in
    toVegaLite [ dataName "source" (dvals []), enc [], mark Bar [] ]


namedData2 =
    let
        dvals =
            dataName "myName" (dataFromUrl "data/dataTest.tsv" [])

        enc =
            encoding
                . position X [ PName "cat", PmType Nominal ]
                . position Y [ PName "val", PmType Quantitative ]
    in
    toVegaLite [ dvals, enc [], mark Bar [] ]

namedData3 =
    let
        enc =
            encoding
                . position X [ PName "cat", PmType Nominal ]
                . position Y [ PName "val", PmType Quantitative ]
    in
    toVegaLite [ dataName "source" (dataFromColumns [] []), enc [], mark Bar [] ]


geodata1 :: VegaLite
geodata1 =
    toVegaLite
        [ width 700
        , height 500
        , configure $ configuration (ViewStyle [ ViewNoStroke ]) []
        , dataFromUrl "https://vega.github.io/vega-lite/data/londonBoroughs.json" [ TopojsonFeature "boroughs" ]
        , mark Geoshape []
        , encoding $ color [ MName "id", MmType Nominal ] []
        ]

geodata2 :: VegaLite
geodata2 =
    let
        geojson =
            geoFeatureCollection
                [ geometry (GeoPolygon [ [ ( -3, 52 ), ( 4, 52 ), ( 4, 45 ), ( -3, 45 ), ( -3, 52 ) ] ]) [ ( "Region", Str "Southsville" ) ]
                , geometry (GeoPolygon [ [ ( -3, 59 ), ( 4, 59 ), ( 4, 52 ), ( -3, 52 ), ( -3, 59 ) ] ]) [ ( "Region", Str "Northerton" ) ]
                ]
    in
    toVegaLite
        [ width 300
        , height 400
        , configure $ configuration (ViewStyle [ ViewNoStroke ]) []
        , dataFromJson geojson [ JSON "features" ]
        , projection [ PrType Orthographic ]
        , encoding (color [ MName "properties.Region", MmType Nominal, MLegend [ LNoTitle ] ] [])
        , mark Geoshape []
        ]

flatten1 :: VegaLite
flatten1 =
    let
        dvals =
            dataFromJson
                (A.toJSON (map A.object
                    [ [ "key" .= ("alpha" :: String)
                      , "foo" .= [ 1 :: Int, 2 ]
                      , "bar" .= [ "A" :: String, "B" ]
                      ]
                    , [ "key" .= ("beta" :: String)
                      , "foo" .= [ 3 :: Int, 4, 5 ]
                      , "bar" .= [ "C" :: String, "D" ]
                      ]
                    ])
                )

        trans =
            transform
                . flattenAs [ "foo", "bar" ] [ "quant", "cat" ]

        enc =
            encoding
                . position X [ PName "quant", PmType Quantitative ]
                . position Y [ PName "cat", PmType Nominal ]
                . color [ MName "key", MmType Nominal ]
    in
    toVegaLite [ dvals [], trans [], mark Circle [], enc [] ]


fold1 :: VegaLite
fold1 =
    let
        dvals =
            dataFromColumns []
                . dataColumn "country" (Strings [ "USA", "Canada" ])
                . dataColumn "gold" (Numbers [ 10, 7 ])
                . dataColumn "silver" (Numbers [ 20, 26 ])

        trans =
            transform
                . foldAs [ "gold", "silver" ] "k" "v"

        enc =
            encoding
                . column [ FName "k", FmType Nominal ]
                . position X [ PName "country", PmType Nominal ]
                . position Y [ PName "v", PmType Quantitative ]
                . color [ MName "country", MmType Nominal ]
    in
    toVegaLite [ dvals [], trans [], mark Bar [], enc [] ]


pivot1 :: VegaLite
pivot1 =
  let dvals = dataFromColumns []
              . dataColumn "country" (Strings [ "USA", "USA", "Canada", "Canada" ])
              . dataColumn "medalType" (Strings [ "gold", "silver", "gold", "silver" ])
              . dataColumn "count" (Numbers [ 10, 20, 7, 26 ])

      trans = transform
              . pivot "medalType" "count" [ PiGroupBy [ "country" ] ]

      enc = encoding
            . position X [ pName "country", pNominal ]
            . position Y [ PRepeat Flow, pQuant ]
            . color [ MName "country", MmType Nominal ]

      spec = asSpec [ dvals [], trans [], enc [], mark Bar [] ]

  in toVegaLite [ repeatFlow [ "gold", "silver" ], specification spec ]


-- example from the pivot documentation
pivotDocs :: VegaLite
pivotDocs =
  let dvals = dataFromColumns []
              . dataColumn "city" (Strings [ "Bristol", "Bristol", "Sheffield", "Sheffield", "Glasgow", "Glasgow" ])
              . dataColumn "temperature" (Numbers [ 12, 14, 11, 13, 7, 10 ])
              . dataColumn "year" (Numbers [ 2017, 2018, 2017, 2018, 2017, 2018 ])

      trans = transform
              . pivot "year" "temperature" [ PiGroupBy [ "city" ] ]

      enc = encoding
            . position X [ pName "2017", pQuant ]
            . position Y [ pName "city", pNominal ]

  in toVegaLite [ dvals [], trans [], enc [], mark Circle [] ]


imputeData :: [DataColumn] -> Data
imputeData =
    dataFromColumns []
        . dataColumn "a" (Numbers [ 0, 0, 1, 1, 2, 2, 3 ])
        . dataColumn "b" (Numbers [ 28, 91, 43, 55, 81, 53, 19 ])
        . dataColumn "c" (Numbers [ 0, 1, 0, 1, 0, 1, 0 ])


impute1, impute2, impute3, impute4, impute5, impute6, impute7, impute8 :: VegaLite

impute1 =
    let
        trans =
            transform
                . impute "b" "a" [ ImNewValue (Number 0), ImGroupBy [ "c" ] ]

        enc =
            encoding
                . position X [ PName "a", PmType Quantitative, PScale [ SNice (NTickCount 1) ] ]
                . position Y [ PName "b", PmType Quantitative ]
                . color [ MName "c", MmType Nominal ]
    in
    toVegaLite [ imputeData [], trans [], mark Line [], enc [] ]


impute2 =
    let
        trans =
            transform
                . impute "b" "a" [ ImMethod ImMean, ImGroupBy [ "c" ], ImFrame (Just (-2)) (Just 2) ]

        enc =
            encoding
                . position X [ PName "a", PmType Quantitative, PScale [ SNice (NTickCount 1) ] ]
                . position Y [ PName "b", PmType Quantitative ]
                . color [ MName "c", MmType Nominal ]
    in
    toVegaLite [ imputeData [], trans [], mark Line [], enc [] ]


impute3 =
    let
        trans =
            transform
                . impute "b" "a" [ ImNewValue (Number 100), ImGroupBy [ "c" ], ImKeyValSequence 1 4 1 ]

        enc =
            encoding
                . position X [ PName "a", PmType Quantitative, PScale [ SNice (NTickCount 1) ] ]
                . position Y [ PName "b", PmType Quantitative ]
                . color [ MName "c", MmType Nominal ]
    in
    toVegaLite [ imputeData [], trans [], mark Line [], enc [] ]


impute4 =
    let
        enc =
            encoding
                . position X [ PName "a", PmType Quantitative, PScale [ SNice (NTickCount 1) ] ]
                . position Y [ PName "b", PmType Quantitative, PImpute [ ImNewValue (Number 0) ] ]
                . color [ MName "c", MmType Nominal ]
    in
    toVegaLite [ imputeData [], mark Line [], enc [] ]


impute5 =
    let
        enc =
            encoding
                . position X [ PName "a", PmType Quantitative, PScale [ SNice (NTickCount 1) ] ]
                . position Y [ PName "b", PmType Quantitative, PImpute [ ImMethod ImMean ] ]
                . color [ MName "c", MmType Nominal ]
    in
    toVegaLite [ imputeData [], mark Line [], enc [] ]


impute6 =
    let
        enc =
            encoding
                . position X [ PName "a", PmType Quantitative, PScale [ SNice (NTickCount 1) ] ]
                . position Y [ PName "b", PmType Quantitative, PImpute [ ImMethod ImMean, ImFrame (Just (-2)) (Just 2) ] ]
                . color [ MName "c", MmType Nominal ]
    in
    toVegaLite [ imputeData [], mark Line [], enc [] ]


impute7 =
    let
        enc =
            encoding
                . position X [ PName "a", PmType Quantitative, PScale [ SNice (NTickCount 1) ] ]
                . position Y [ PName "b", PmType Quantitative, PImpute [ ImNewValue (Number 100)
                                                                       , ImKeyVals (Numbers [ 4 ]) ] ]
                . color [ MName "c", MmType Nominal ]
    in
    toVegaLite [ imputeData [], mark Line [], enc [] ]


impute8 =
    let
        enc =
            encoding
                . position X [ PName "a", PmType Quantitative, PScale [ SNice (NTickCount 1) ] ]
                . position Y [ PName "b", PmType Quantitative, PImpute [ ImNewValue (Number 100)
                                                                       , ImKeyValSequence 4 6 1 ] ]
                . color [ MName "c", MmType Nominal ]
    in
    toVegaLite [ imputeData [], mark Line [], enc [] ]


sample1 :: VegaLite
sample1 =
    let
        dvals =
            dataFromUrl "https://vega.github.io/vega-lite/data/cars.json" []

        trans =
            transform
                . sample 200

        enc =
            encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon", PmType Quantitative ]

        spec1 =
            asSpec [ mark Point [], enc [] ]

        spec2 =
            asSpec [ mark Point [], trans [], enc [] ]
    in
    toVegaLite [ dvals, hConcat [ spec1, spec2 ] ]


bin1 :: VegaLite
bin1 =
    let
        dvals =
            dataFromColumns []
                . dataColumn "bin_start" (Numbers [ 8, 10, 12, 14, 16, 18, 20, 22 ])
                . dataColumn "bin_end" (Numbers [ 10, 12, 14, 16, 18, 20, 22, 24 ])
                . dataColumn "count" (Numbers [ 7, 29, 71, 127, 94, 54, 17, 5 ])

        enc =
            encoding
                . position X [ PName "bin_start", PmType Quantitative
                             , PBinned
                             , PAxis [ AxTickMinStep 2 ] ]
                . position X2 [ PName "bin_end" ]
                . position Y [ PName "count", PmType Quantitative ]
    in
    toVegaLite [ dvals [], enc [], mark Bar [] ]


bin2 :: VegaLite
bin2 =
    let
        dvals =
            dataFromColumns []
                . dataColumn "x" (Numbers [ 10.6, 12.1, 9.4, 11.5, 12.6, 10.7, 11.6, 7.7, 12, 10.6, 16.5, 8.7, 7.6, 10.2, 10, 9.8, 11, 9, 10.4, 11.6, 11.2, 11.1, 11.7, 12.1, 9.9, 8.9, 10.9, 14.6, 11.4, 12.1 ])

        enc =
            encoding
                . position X [ PName "x", pQuant, PBin [] ]
                . position Y [ PAggregate Count, pQuant ]
    in
    toVegaLite [ width 300, dvals [], enc [], mark Bar [] ]


bin3 :: VegaLite
bin3 =
    let
        dvals =
            dataFromColumns []
                . dataColumn "x" (Numbers [ 10.6, 12.1, 9.4, 11.5, 12.6, 10.7, 11.6, 7.7, 12, 10.6, 16.5, 8.7, 7.6, 10.2, 10, 9.8, 11, 9, 10.4, 11.6, 11.2, 11.1, 11.7, 12.1, 9.9, 8.9, 10.9, 14.6, 11.4, 12.1, 12.2, 11.3, 13.1, 14.3, 9.8, 12.7, 9.2, 8.7, 11.3, 6.5, 11.1, 8.9, 11.8, 10.5, 12.8, 11.1, 11.2, 7, 12.4, 11.3, 8.3, 12.4, 12.1, 9.4, 8.6, 11.1, 8.9, 8.4, 10.5, 9.9, 6.5, 8.2, 12.7, 7.7, 11.1, 8.1, 8.1, 10.7, 9.8, 11.2, 11.2 ])

        enc =
            encoding
                . position X [ PName "x", pQuant, PBin [ BinAnchor 0.5 ] ]
                . position Y [ PAggregate Count, pQuant ]
    in
    toVegaLite [ width 300, dvals [], enc [], mark Bar [] ]


sequence1 :: VegaLite
sequence1 =
    let
        dvals =
            dataSequence 0 12.7 0.1

        trans =
            transform
                . calculateAs "sin(datum.data)" "v"

        enc =
            encoding
                . position X [ PName "data", PmType Quantitative ]
                . position Y [ PName "v", PmType Quantitative ]
    in
    toVegaLite [ dvals, trans [], enc [], mark Line [] ]


sequence2 :: VegaLite
sequence2 =
    let
        dvals =
            dataSequenceAs 0 12.7 0.1 "u"

        trans =
            transform
                . calculateAs "sin(datum.u)" "v"

        enc =
            encoding
                . position X [ PName "u", PmType Quantitative ]
                . position Y [ PName "v", PmType Quantitative ]
    in
    toVegaLite [ dvals, trans [], enc [], mark Line [] ]


filter1 :: VegaLite
filter1 =
    let
        dvals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C", "D", "E", "F", "G", "H", "I" ])
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        trans =
            transform
                . filter (FExpr "datum.a == 'A' || datum.a == 'C' || datum.a == 'E'")

        enc =
            encoding
                . position X [ PName "a", PmType Ordinal ]
                . position Y [ PName "b", PmType Quantitative ]
    in
    toVegaLite [ dvals [], trans [], enc [], mark Bar [] ]


filter2 :: VegaLite
filter2 =
    let
        dvals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C", "D", "E", "F", "G", "H", "I" ])
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81, 53, 19, 87, 52 ])

        trans =
            transform
                . filter
                    (Or
                        (Or (FEqual "a" (Str "A") & FilterOp)
                            (FEqual "a" (Str "C") & FilterOp)
                        )
                        (FEqual "a" (Str "E") & FilterOp)
                        & FCompose
                    )

        enc =
            encoding
                . position X [ PName "a", PmType Ordinal ]
                . position Y [ PName "b", PmType Quantitative ]
    in
    toVegaLite [ dvals [], trans [], enc [], mark Bar [] ]


annotate1 :: VegaLite
annotate1 =
    let
        dvals =
            dataFromColumns []
                . dataColumn "a" (Strings [ "A", "B", "C", "D", "E" ])
                . dataColumn "b" (Numbers [ 28, 55, 43, 91, 81 ])

        enc =
            encoding
                . position X [ pName "a", pOrdinal ]
                . position Y [ pName "b", pQuant ]

        specBars =
            asSpec [ enc [], mark Bar [] ]

        specText =
            asSpec [ noData, mark Text [ MText "Test" ] ]
    in
    toVegaLite [ dvals [], layer [ specBars, specText ] ]


-- See https://vega.github.io/vega-lite/examples/line_skip_invalid_mid_overlay.html
null1 :: VegaLite
null1 =
  toVegaLite [ mark Line [MPoint (PMMarker [])]
             , encoding
                 . position X [pName "x", pQuant]
                 . position Y [pName "y", pQuant]
                 $ []
             , dataFromRows []
                 . dataRow [("x", Number 1), ("y", Number 10)]
                 . dataRow [("x", Number 2), ("y", Number 30)]
                 . dataRow [("x", Number 3), ("y", NullValue)]
                 . dataRow [("x", Number 4), ("y", Number 15)]
                 . dataRow [("x", Number 5), ("y", NullValue)]
                 . dataRow [("x", Number 6), ("y", Number 40)]
                 . dataRow [("x", Number 7), ("y", Number 20)]
                 $ []
             ]


domainData :: Data
domainData = dataFromColumns []
             . dataColumn "x" (Numbers [ 1, 2, 3, 4 ])
             . dataColumn "y" (Numbers [ 95, 97, 100, 105 ])
             $ []


domain :: [ScaleProperty] -> VegaLite
domain yExtra =
  let enc = encoding
            . position X [PName "x", PmType Quantitative]
            . position Y [PName "y", PmType Quantitative, PScale (SZero False : yExtra)]
            $ []

      mopts = [MPoint (PMMarker [])]

  in toVegaLite [domainData, enc, mark Line mopts]


domain1, domain2, domain3 :: VegaLite
domain1 = domain []
domain2 = domain [SDomain (DNumbers [90, 100])]
domain3 = domain [SDomainOpt (DUnionWith (DNumbers [90, 100]))]


{-

In adding this test I found the output did not validate against
version 4.0.2, so I've left out for now

key1 :: VegaLite
key1 =
  let dvals = dataFromColumns []
                . dataColumn "x1" (Numbers [ 4, 5, 6 ])
                . dataColumn "x2" (Numbers [ 5, 6, 8 ])
                . dataColumn "y" (Numbers [ 2, 2, 5 ])
                . dataColumn "flag" (Strings [ "good", "bad", "good" ])

      enc = encoding
             . position X [ PName "x1", PmType Quantitative ]
             . position Y [ PName "y", PmType Quantitative ]
             . keyChannel "flag"

  in toVegaLite [ dvals [], enc [], mark Line [] ]

-}


-- From https://github.com/vega/vega-lite/pull/6475/files
argminSpaces :: VegaLite
argminSpaces =
  let desc = "An example showing how to use argmin in tooltips with fields with spaces"

      dvals = dataFromColumns []
              . dataColumn "Fighter Name" (Strings [ "liukang", "liukang", "liukang"
                                                   , "johnnycage", "johnnycage", "johnnycage"
                                                   , "raided", "raiden", "raiden"
                                                   ])
              . dataColumn "Place Rank" (Numbers [1, 10, 3, 6, 5, 4, 8, 12, 2])
              . dataColumn "Fighting Style" (Strings [ "tiger", "crane", "shaolin"
                                                     , "tiger", "crane", "shaolin"
                                                     , "tiger", "crane", "shaolin"
                                                     ])
              $ []

  in toVegaLite [ description desc
                , dvals
                , mark Point []
                , encoding
                  . position X [PName "Place Rank", PAggregate Min, PmType Quantitative]
                  . position Y [PName "Fighter Name", PmType Nominal]
                  . tooltips [ [TName "Fighter Name", TmType Nominal]
                             , [TName "Place Rank", TAggregate Min, TmType Quantitative]
                             , [ TName "Fighting Style"
                               , TmType Nominal
                               , TAggregate (ArgMin (Just "Place Rank"))
                               ]
                             ]
                  $ []
                ]
