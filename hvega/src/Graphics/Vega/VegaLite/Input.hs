{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Graphics.Vega.VegaLite.Input
Copyright   : (c) Douglas Burke, 2018-2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings, TupleSections

Data input (from file or created inline).

-}

module Graphics.Vega.VegaLite.Input
       ( Data
       , DataColumn
       , DataRow

       , Format(..)
       , DataType(..)

       , dataFromUrl
       , dataFromColumns
       , dataFromRows
       , dataFromJson
       , dataFromSource
       , dataName
       , datasets
       , dataColumn
       , dataRow
       , noData

       , dataSequence
       , dataSequenceAs

       ) where

import qualified Data.Aeson as A

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Control.Arrow (second)

import Data.Aeson ((.=), Value, decode, encode, object, toJSON)
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe, mapMaybe)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Graphics.Vega.VegaLite.Data
  ( DataValue(..)
  , DataValues(..)
  , dataValueSpec
  )
import Graphics.Vega.VegaLite.Foundation
  ( FieldName
  , toObject
  )
import Graphics.Vega.VegaLite.Specification
  ( VLProperty(VLData, VLDatasets)
  , VLSpec
  , LabelledSpec
  )
import Graphics.Vega.VegaLite.Time (dateTimeSpec)


{-|

Indicates the type of data to be parsed when reading input data. For @FoDate@
and @FoUtc@, the formatting specification can be specified using
<https://vega.github.io/vega-lite/docs/data.html#format D3's formatting specifiers>
or left as an empty string if default date formatting is to be applied. Care should
be taken when assuming default parsing of dates because different browsers can
parse dates differently. Being explicit about the date format is usually safer.

-}
data DataType
    = FoNumber
      -- ^ Indicate numeric data type to be parsed when reading input data.
    | FoBoolean
      -- ^ Indicate Boolean data type to be parsed when reading input data.
    | FoDate T.Text
      -- ^ Date format for parsing input data using
      --   [D3's formatting specifiers](https://vega.github.io/vega-lite/docs/data.html#format)
      --   or left as an empty string for default formatting.
    | FoUtc T.Text
      -- ^ Similar to 'FoDate' but for UTC format dates.


{-|

Specifies the type of format a data source uses. If the format is indicated by
the file name extension (@".tsv"@, @".csv"@, @".json"@) there is no need to indicate the
format explicitly. However this can be useful if the filename extension does not
indicate type (e.g. @".txt"@) or you wish to customise the parsing of a file. For
example, when specifying the @JSON@ format, its parameter indicates the name of
property field containing the attribute data to extract. For details see the
<https://vega.github.io/vega-lite/docs/data.html#format Vega-Lite documentation>.
-}
data Format
    = JSON T.Text
      -- ^ Property to be extracted from some JSON when it has some surrounding structure.
      --   e.g., specifying the property @values.features@ is equivalent to retrieving
      --   @json.values.features@ from a JSON object with a custom delimiter.
    | CSV
      -- ^ Comma-separated (CSV) data file format.
    | TSV
      -- ^ Tab-separated (TSV) data file format
    | DSV Char
      -- ^ The fields are separated by the given character (which must be a
      --   single 16-bit code unit).
      --
      --   @since 0.4.0.0
      {- This isn't in the current vega-lite v3 schema as far as I can see
    | Arrow
      -- ^ <https://observablehq.com/@theneuralbit/introduction-to-apache-arrow Apache Arrow> format.
      --
      -- @since 0.4.0.0
      -}
    | TopojsonFeature T.Text
      -- ^ A topoJSON feature format containing an object with the given name. For example:
      --
      -- @
      -- 'dataFromUrl' \"londonBoroughs.json\" ['TopojsonFeature' \"boroughs\"]
      -- @
    | TopojsonMesh T.Text
      -- ^ A topoJSON mesh format containing an object with the given name. Unlike
      --   'TopojsonFeature', the corresponding geo data are returned as a single unified mesh,
      --   not as individual GeoJSON features.
    | Parse [(FieldName, DataType)]
      -- ^ Parsing rules when processing some data text, specified as
      --   a list of tuples in the form @(fieldname,
      --   datatype)@. Useful when automatic type inference needs to
      --   be overridden, for example when converting integers representing
      --   years into dates and strings into numbers:
      --
      -- @
      -- 'dataFromUrl' \"myDataFile.csv\"
      --    [ 'Parse' [ ( \"year\", 'FoDate' \"%Y\" ), ( "\y\", 'FoNumber' ) ] ]
      -- @


{-|

Represents a single column of data. Used when generating inline data with
'dataColumn' and 'dataFromColumns'.
-}
type DataColumn = [LabelledSpec]

{-|

Represents a single row of data. Used when generating inline data with
'dataRow' and 'dataFromRows'.
-}
type DataRow = VLSpec

{-|

Convenience type-annotation label for use with data generation functions.

@
myRegion : ['DataColumn'] -> 'Data'
myRegion =
    'dataFromColumns' []
        . 'dataColumn' "easting" ('Graphics.Vega.VegaLite.Numbers' [ -3, 4, 4, -3, -3 ])
        . 'dataColumn' "northing" ('Graphics.Vega.VegaLite.Numbers' [ 52, 52, 45, 45, 52 ])
@

It is the same as 'Graphics.Vega.VegaLite.PropertySpec' (which was added in @0.4.0.0@),
but kept separate to help better-document code.

-}
type Data = (VLProperty, VLSpec)


formatProperty :: Format -> [Pair]
formatProperty (JSON js) =
  let ps = [("type", "json")]
           <> if T.null (T.strip js) then [] else [("property", js)]
  in map (second toJSON) ps

formatProperty CSV = [("type", "csv")]
formatProperty TSV = [("type", "tsv")]
formatProperty (DSV delim) = [("type", "dsv"), "delimiter" .= delim]
-- formatProperty Arrow = [("type", "arrow")]
formatProperty (TopojsonFeature os) = [("type", "topojson")
                                      , "feature" .= os
                                      ]
formatProperty (TopojsonMesh os) = [("type", "topojson")
                                   , "mesh" .= os
                                   ]
formatProperty (Parse fmts) =
  let pObj = toObject (map (second dataTypeSpec) fmts)
  in [("parse", pObj)]


dataTypeSpec :: DataType -> VLSpec
dataTypeSpec dType =
  let s = case dType of
        FoNumber -> "number"
        FoBoolean -> "boolean"
        FoDate fmt | T.null fmt -> "date"
                   | otherwise -> "date:'" <> fmt <> "'"
        FoUtc fmt | T.null fmt -> "utc"
                  | otherwise -> "utc:'" <> fmt <> "'"
  in toJSON s


{-|

Create a row of data. A row comprises a list of (columnName, value) pairs.
The final parameter is the list of any other rows to which this is added.

This is expected to be used with 'dataFromRows'.

@
'dataRow' [(\"Animal\", 'Graphics.Vega.VegaLite.Str' \"Fish\"), (\"Age\", 'Graphics.Vega.VegaLite.Number' 28), (\"Year\", 'Graphics.Vega.VegaLite.Str' "2010")] []
@
-}
dataRow :: [(FieldName, DataValue)] -> [DataRow] -> [DataRow]
dataRow rw = (toObject (map (second dataValueSpec) rw) :)


{-|

Create a dataset comprising a collection of named 'Data' items. Each data item
can be created with normal data generating functions such as 'dataFromRows' or
'dataFromJson'. These can be later referred to using 'dataFromSource'.

@
let toJS = Data.Aeson.toJSON
    obj = Data.Aeson.object

    dvals = 'dataFromRows' []
            . 'dataRow' [ ( "cat", 'Graphics.Vega.VegaLite.Str' "a" ), ( "val", 'Graphics.Vega.VegaLite.Number' 10 ) ]
            . 'dataRow' [ ( "cat", 'Graphics.Vega.VegaLite.Str' "b" ), ( "val", 'Graphics.Vega.VegaLite.Number' 18 ) ]
    json = toJS
            [ obj [ ( "cat", toJS "a" ), ( "val", toJS 120 ) ]
            , obj [ ( "cat", toJS "b" ), ( "val", toJS 180 ) ]
            ]

    enc = ...

in 'Graphics.Vega.VegaLite.toVegaLite'
    [ 'datasets' [ ( \"myData\", dvals [] ),  ( \"myJson\", 'dataFromJson' json [] ) ]
    , 'dataFromSource' \"myData\" []
    , 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Bar' []
    , enc []
    ]
@
-}
datasets :: [(T.Text, Data)] -> Data
datasets namedData =
  -- Follow Elm in parsing the structure to get what we want, but the code is
  -- written rather differently.
  --
  -- The input is expected to be a singleton list containing a pair.
  let converted = extract . snd
      specs = map (second converted) namedData

      convert :: Value -> Maybe [(T.Text, Value)]
      convert v = HM.toList <$> decode (encode v)

      extract din =
        let extract' [(_, v)] = Just v
            extract' _ = Nothing

        in fromMaybe din (convert din >>= extract')

  in (VLDatasets, toObject specs)


-- | This is for composed specifications, and it tells the visualization to
--   ignore the data from the parent.
--
--   @since 0.4.0.0
noData :: Data
noData = (VLData, A.Null)


{-|

Name to give a data source. Useful when a specification needs to reference a
data source, such as one generated via an API call.

@
dvals = 'dataName' \"myName\" ('dataFromUrl' \"myData.json\" [])
@

@since 0.4.0.0

-}

-- TODO: can we restrict this to only those with a VLProperty of VLData?

dataName ::
  T.Text
  -- ^ The name to give the data source
  -> Data
  -- ^ The data source to be named.
  -> Data
  -- ^ If the input @Data@ argument is not a data source then
  --   this is just the input value.
dataName s odata@(_, dataSpec) =
  -- follow Elm in parsing the structure to get what we want, but the code is
  -- written rather differently. This is based on the code used in datasets.
  --
  -- The input is expected to be a singleton list containing a pair.
  --
  let converted = convert >>= extract

      -- Aeson's objects are wrappers around a hash map, so this should be
      -- a relatively easy conversion. The type annotation isn't needed
      -- but left in for reference.
      --
      convert :: Maybe [Pair]
      convert = HM.toList <$> decode (encode dataSpec)

      extract [v] = Just v
      extract _ = Nothing

  in case converted of
       Just v -> (VLData, object [ "name" .= s, v ])
       _ -> odata


{-|

Declare a data source from a list of column values. Each column has a
specific type (e.g. 'Number' or 'String'), but different columns can have
different types.

Note that the columns are truncated to match the length of the shortest column.

@
'dataFromColumns' [ 'Parse' [ ( \"Year\", 'FoDate' "%Y" ) ] ]
  . 'dataColumn' \"Animal\" ('Strings' [ \"Fish\", \"Dog\", \"Cat\" ])
  . 'dataColumn' \"Age\" ('Numbers' [ 28, 12, 6 ])
  . 'dataColumn' \"Year\" ('Strings' [ "2010", "2014", "2015" ])
@
-}
dataFromColumns ::
  [Format]
  -- ^ An optional list of formatting instructions for the columns.
  --
  --   Simple numbers and strings do not normally need formatting, but it is
  --   good practice to explicitly declare date-time formats as handling of
  --   these values can vary between different viewers (e.g. browsers).
  --
  --   See the
  --   <https://vega.github.io/vega-lite/docs/data.html#format Vega-Lite documentation>
  --   for more details.
  -> [DataColumn]
  -- ^ The columns to add. This is expected to be created with one or more
  --   calls to 'dataColumn'.
  -> Data
dataFromColumns fmts cols =
  let dataArray = map toObject (transpose cols)

      vals = [("values", toJSON dataArray)]
             <> if null fmts
                then []
                else [("format", toJSON fmtObject)]

      fmtObject = object (concatMap formatProperty fmts)

  in (VLData, object vals)


transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs) : xss) =
  let heads = filterMap elmHead xss --
      tails = filterMap elmTail xss

      elmHead (h:_) = Just h
      elmHead [] = Nothing

      elmTail [] = Nothing
      elmTail (_:ts) = Just ts

      filterMap = mapMaybe

  in (x : heads) : transpose (xs : tails)


{-|

Declare a data source from a provided json specification. The most likely use-case
for specifying json inline is when creating <http://geojson.org geojson> objects,
when 'Graphics.Vega.VegaLite.geometry', 'Graphics.Vega.VegaLite.geometryCollection', and 'Graphics.Vega.VegaLite.geoFeatureCollection' functions
may be used. For more general cases of json creation, consider 'Data.Aeson.encode'.

@
let geojson =
        'Graphics.Vega.VegaLite.geometry' ('Graphics.Vega.VegaLite.GeoPolygon' [ [ ( -3, 59 ), ( 4, 59 ), ( 4, 52 ), ( -3, 59 ) ] ]) []
in 'Graphics.Vega.VegaLite.toVegaLite'
    [ 'Graphics.Vega.VegaLite.width' 200
    , 'Graphics.Vega.VegaLite.height' 200
    , 'dataFromJson' geojson []
    , 'Graphics.Vega.VegaLite.projection' [ 'Graphics.Vega.VegaLite.PrType' 'Graphics.Vega.VegaLite.Orthographic' ]
    , 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Geoshape' []
    ]
@
-}
dataFromJson :: VLSpec -> [Format] -> Data
dataFromJson vlspec fmts =
  let js = if null fmts
           then object [("values", vlspec)]
           else object [ ("values", vlspec)
                       , ("format",
                          object (concatMap formatProperty fmts)) ]
  in (VLData, js)


{-|

Create a column of data. A column has a name and a list of values. The final
parameter is the list of any other columns to which this is added.

This is expected to be used with 'dataFromColumns'.

@
'dataColumn' \"Animal\" ('Strings' [ \"Cat\", \"Dog\", \"Mouse\"]) []
@
-}
dataColumn :: FieldName -> DataValues -> [DataColumn] -> [DataColumn]
dataColumn colName dVals xs =
  let col = case dVals of
        Booleans cs -> map toJSON cs
        DateTimes cs -> map dateTimeSpec cs
        Numbers cs -> map toJSON cs
        Strings cs -> map toJSON cs

      x = map (colName,) col

  in x : xs


{-|

Declare a data source from a provided list of row values. Each row
contains a list of tuples where the first value is a string
representing the column name, and the second the column value for that
row. Each column can have a value of a different type but
__you must ensure__
that when subsequent rows are added, they match the types of
previous values with shared column names.

Note though that generally if you are creating data inline (as opposed
to reading from a file), adding data by column is more efficient and
less error-prone.

@
dataFromRows [ 'Parse' [ ( \"Year\", 'FoDate' "%Y" ) ] ]
  . 'dataRow' [ ( \"Animal\", 'Str' \"Fish\" ), ( \"Age\", 'Number' 28 ), ( \"Year\", 'Str' "2010" ) ]
  . 'dataRow' [ ( \"Animal\", 'Str' \"Dog\" ), ( \"Age\", 'Number' 12 ), ( \"Year\", 'Str' "2014" ) ]
  . 'dataRow' [ ( \"Animal\", 'Str' \"Cat\" ), ( \"Age\", 'Number' 6 ), ( \"Year\", 'Str' "2015" ) ]
@
-}
dataFromRows ::
  [Format]
  -- ^ An optional list of formatting instructions for the rows.
  --
  --   Simple numbers and strings do not normally need formatting, but it is
  --   good practice to explicitly declare date-time formats as handling of
  --   these values can vary between different viewers (e.g. browsers).
  --
  --   See the
  --   <https://vega.github.io/vega-lite/docs/data.html#format Vega-Lite documentation>
  --   for more details.
  -> [DataRow]
  -- ^ The rows to add. This is expected to be created with one or more
  --   calls to 'dataRow'.
  -> Data
dataFromRows fmts rows =
  let kvs = ("values", toJSON rows)
            : if null fmts
              then []
              else [("format", object (concatMap formatProperty fmts))]
  in (VLData, object kvs)


{-|

Declare data from a named source. The source may be from named 'datasets' within
a specification or a named data source created via the
<https://vega.github.io/vega/docs/api/view/#data Vega View API>.
An optional list of field formatting instructions can be provided as the second
parameter or an empty list to use the default formatting. See the
<https://vega.github.io/vega-lite/docs/data.html#named Vega-Lite documentation>
for details.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'datasets' [ ( "myData", dvals [] ),  ( "myJson", 'dataFromJson' json [] ) ]
    , 'dataFromSource' "myData" []
    , 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Bar' []
    , ...
    ]
@
-}
dataFromSource :: T.Text -> [Format] -> Data
dataFromSource sourceName fmts =
  let kvs = ("name" .= sourceName)
            : if null fmts
              then []
              else [("format", object (concatMap formatProperty fmts))]
  in (VLData, object kvs)


{-|

Declare data source from a url. The url can be a local path on a web server
or an external http(s) url. Used to create a data ( property, specification ) pair.
An optional list of field formatting instructions can be provided as the second
parameter or an empty list to use the default formatting. See the
<https://vega.github.io/vega-lite/docs/data.html#format Vega-Lite documentation>
for details.

@
'dataFromUrl' "data/weather.csv" [ 'Parse' [ ( "date", 'FoDate' "%Y-%m-%d %H:%M" ) ] ]
@
-}
-- TODO: should this use a URL type?
dataFromUrl :: T.Text -> [Format] -> Data
dataFromUrl url fmts =
  let kvs = ("url" .= url)
            : if null fmts
              then []
              else [("format", object (concatMap formatProperty fmts))]
  in (VLData, object kvs)


{-|

Generate a sequence of numbers as a data source. The resulting
sequence will have the name @\"data\"@. To give it an alternative name use
'dataSequenceAs'.

@
myData = 'dataSequence' 0 6.28 0.1
@

@since 0.4.0.0

-}
dataSequence ::
  Double     -- ^ start of the sequence (inclusive)
  -> Double  -- ^ end of the sequence (exclusive)
  -> Double  -- ^ step size
  -> Data
dataSequence start stop step =
  let vals = [("sequence", object svals)]
      svals = [ "start" .= start
              , "stop" .= stop
              , "step" .= step
              ]

  in (VLData, object vals)


{-|

Generate a sequence of numbers as a named data source. This extends
'dataSequence' by allowing you to name the data source.

@
myTheta = 'dataSequenceAs' 0 6.28 0.1 \"theta\"
@

@since 0.4.0.0

-}
dataSequenceAs ::
  Double     -- ^ start of the sequence (inclusive)
  -> Double  -- ^ end of the sequence (exclusive)
  -> Double  -- ^ step size
  -> FieldName  -- ^ The name of the data source
  -> Data
dataSequenceAs start stop step outName =
  let vals = [ "sequence" .= object svals ]
      svals = [ "start" .= start
              , "stop" .= stop
              , "step" .= step
              , "as" .= outName
              ]

  in (VLData, object vals)
