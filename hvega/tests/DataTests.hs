{-# LANGUAGE OverloadedStrings #-}

--
-- Based on the Elm VegaLite DataTests.elm as of version 1.12.0
--
module DataTests (testSpecs) where

import qualified Data.Aeson as A
import qualified Data.Text as T

import Data.Aeson ((.=))
import Graphics.Vega.VegaLite hiding (filter, repeat)

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
            -- , ("flatten1", flatten1)
            -- , ("fold1", fold1)
            {-
            , ("impute1", impute1)
            , ("impute2", impute2)
            , ("impute3", impute3)
            , ("impute4", impute4)
            , ("impute5", impute5)
            , ("impute6", impute6)
            , ("impute7", impute7)
            , ("impute8", impute8)
            -}
            -- , ("sample1", sample1)
            , ("bin1", bin1)
            , ("sequence1", sequence1)
            , ("sequence2", sequence2)
            ]

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
-- no arrow support

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
        , configure $ configuration (View [ Stroke Nothing ]) []
        , dataFromUrl "https://vega.github.io/vega-lite/data/londonBoroughs.json" [ TopojsonFeature "boroughs" ]
        , mark Geoshape []
        , encoding $ color [ MName "id", MmType Nominal ] []
        ]

-- TODO: add LNoTitle

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
        , configure $ configuration (View [ Stroke Nothing ]) []
        , dataFromJson geojson [ JSON "features" ]
        , projection [ PType Orthographic ]
        , encoding (color [ MName "properties.Region", MmType Nominal, MLegend [ LTitle "" ] ] [])
        , mark Geoshape []
        ]

{- TODO: add flattenAs

flatten1 =
    let
        dvals =
            dataFromJson
                (A.toJSON (map A.object
                    [ [ ( "key" .= "alpha" )
                      , ( "foo" .= [ 1, 2 ] )
                      , ( "bar" .= [ "A", "B" ] )
                      ]
                    , [ ( "key" .= "beta" )
                      , ( "foo" .= [ 3, 4, 5 ] )
                      , ( "bar" .= [ "C", "D" ] )
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
-}

{- TODO: add foldAs

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
-}


{- TODO: add support for impute

imputeData :: [DataColumn] -> Data
imputeData =
    dataFromColumns []
        . dataColumn "a" (Numbers [ 0, 0, 1, 1, 2, 2, 3 ])
        . dataColumn "b" (Numbers [ 28, 91, 43, 55, 81, 53, 19 ])
        . dataColumn "c" (Numbers [ 0, 1, 0, 1, 0, 1, 0 ])


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
                . impute "b" "a" [ ImNewValue (Num 100), ImGroupBy [ "c" ], ImKeyValSequence 1 4 1 ]

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

-}


{- TODO: add sample

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

-}


-- TODO: add PBinned

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
                             {- , PBinned -}
                             , PAxis [ AxTickMinStep 2 ] ]
                . position X2 [ PName "bin_end" ]
                . position Y [ PName "count", PmType Quantitative ]
    in
    toVegaLite [ dvals [], enc [], mark Bar [] ]

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
