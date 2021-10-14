{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Transform
Copyright   : (c) Douglas Burke, 2018-2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

Types related to data transformation.

Note that this does not include some of the \"obvious\" types,
in particular @Operation@ and @Filter@ as these types are
inter-related and end up requiring a number of other types unrelated
to transformations.

-}

module Graphics.Vega.VegaLite.Transform
       ( Operation(..)
       , Window(..)
       , WOperation(..)
       , BinProperty(..)
       , WindowProperty(..)
       , ImputeProperty(..)
       , ImMethod(..)

         -- not for external export
       , aggregate_
       , op_
       , binned_
       , impute_
       , bin
       , binProperty
       , operationSpec
       , windowTS
       , joinAggregateTS
       , imputeTS

       ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

import Data.Aeson ((.=), object, toJSON)
import Data.Aeson.Types (Pair)
import Data.Maybe (mapMaybe)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

import Graphics.Vega.VegaLite.Data
  ( DataValue
  , DataValues
  , dataValueSpec
  , dataValuesSpecs
  )
import Graphics.Vega.VegaLite.Foundation
  ( FieldName
  , SortField
  , sortFieldSpec
  -- , field_
  , fromT
  , allowNull
  )
import Graphics.Vega.VegaLite.Specification
  ( VLSpec
  , TransformSpec(..)
  , SelectionLabel
  )


{-|

Type of aggregation operation. See the
<https://vega.github.io/vega-lite/docs/aggregate.html#ops Vega-Lite documentation>
for more details.

The @Average@ constructor was removed in version @0.4.0.0@; use 'Mean' instead.

-}
data Operation
    = ArgMax (Maybe FieldName)
      -- ^ An input data object containing the maximum field value to be used
      --   in an aggregation operation.
      --
      --   If supplied as part of an encoding aggregation, the parameter
      --   should be 'Just' the name of the field to maximise. When used
      --   as part of a transform the parameter should be 'Nothing' as the
      --   field is specified in the 'Graphics.Vega.VegaLite.aggregate' call.
      --
      --   Encoding example, to find the production budget for the maximum
      --   US grossing film in each genre:
      --
      --   @
      --   'Graphics.Vega.VegaLite.encoding'
      --     . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.X'
      --                [ 'Graphics.Vega.VegaLite.PName' \"Production_Budget\"
      --                , 'Graphics.Vega.VegaLite.PmType' 'Graphics.Vega.VegaLite.Quantitative'
      --                , 'Graphics.Vega.VegaLite.PAggregate' ('ArgMax' ('Just' \"US_Gross\"))
      --                ]
      --     . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.Y' ['Graphics.Vega.VegaLite.PName' \"Major_Genre\", 'Graphics.Vega.VegaLite.PmType' 'Graphics.Vega.VegaLite.Nominal']
      --   @
      --
      --   An example of its use as part of an 'Graphics.Vega.VegaLite.aggregate' call:
      --
      --   @
      --   'Graphics.Vega.VegaLite.transform'
      --     . 'Graphics.Vega.VegaLite.aggregate'
      --         [ 'Graphics.Vega.VegaLite.opAs' ('ArgMax' 'Nothing') \"US_Gross\" \"amUSGross\"]
      --         [\"Major_Genre\"]
      --   @
      --
      --   The optional field name was added in the @0.4.0.0@ release.
    | ArgMin (Maybe FieldName)
      -- ^ An input data object containing the minimum field value to be used
      --   in an aggregation operation. See 'ArgMax' for a discussion of the
      --   optional argument.
      --
      --   The optional field name was added in the @0.4.0.0@ release.
    | CI0
      -- ^ Lower 95% confidence interval to be used in an aggregation operation.
    | CI1
      -- ^ Upper 95% confidence interval to be used in an aggregation operation.
    | Count
      -- ^ Total count of data objects to be used in an aggregation operation.
    | Distinct
      -- ^ Count of distinct data objects to be used in an aggregation operation.
    | Max
      -- ^ Maximum field value to be used in an aggregation operation.
    | Mean
      -- ^ Mean field value to be used in an aggregation operation.
    | Median
      -- ^ Median field value to be used in an aggregation operation.
    | Min
      -- ^ Minimum field value to be used in an aggregation operation.
    | Missing
      -- ^ Count of @null@ or @undefined@ field value to be used in an aggregation operation.
    | Product
      -- ^ Product of field values to be used in an aggregate operation.
      --
      --   This was added in Vega-Lite 4.6.0.
      --
      --   @since 0.7.0.0
    | Q1
      -- ^ Lower quartile boundary of field values to be used in an aggregation operation.
    | Q3
      -- ^ Upper quartile boundary of field values to be used in an aggregation operation.
    | Stderr
      -- ^ Standard error of field values to be used in an aggregate operation.
    | Stdev
      -- ^ Sample standard deviation of field values to be used in an aggregate operation.
    | StdevP
      -- ^ Population standard deviation of field values to be used in an aggregate operation.
    | Sum
      -- ^ Sum of field values to be used in an aggregate operation.
    | Valid
      -- ^ Count of values that are not @null@, @undefined@, or @NaN@ to be used in an
      -- aggregation operation.
    | Variance
      -- ^ Sample variance of field values to be used in an aggregate operation.
    | VarianceP
      -- ^ Population variance of field values to be used in an aggregate operation.


-- Unlike Elm, not checking if the string is empty for ArgMin/Max

operationSpec :: Operation -> VLSpec
operationSpec (ArgMax Nothing) = "argmax"
operationSpec (ArgMax (Just s)) = object ["argmax" .= s]
operationSpec (ArgMin Nothing) = "argmin"
operationSpec (ArgMin (Just s)) = object ["argmin" .= s]
operationSpec CI0 = "ci0"
operationSpec CI1 = "ci1"
operationSpec Count = "count"
operationSpec Distinct = "distinct"
operationSpec Max = "max"
operationSpec Mean = "mean"
operationSpec Median = "median"
operationSpec Min = "min"
operationSpec Missing = "missing"
operationSpec Product = "product"
operationSpec Q1 = "q1"
operationSpec Q3 = "q3"
operationSpec Stderr = "stderr"
operationSpec Stdev = "stdev"
operationSpec StdevP = "stdevp"
operationSpec Sum = "sum"
operationSpec Valid = "valid"
operationSpec Variance = "variance"
operationSpec VarianceP = "variancep"


aggregate_ :: Operation -> Pair
aggregate_ op = "aggregate" .= operationSpec op

op_ :: Operation -> Pair
op_ op = "op" .= operationSpec op


-- | Window transformations.
--
--   @since 0.4.0.0

data Window
    = WAggregateOp Operation
      -- ^ An aggregrate operation to be used in a window transformation.
    | WOp WOperation
      -- ^ Window-specific operation to be used in a window transformation.
    | WParam Int
      -- ^ Numeric parameter for window-only operations that can be parameterised
      --   ('Ntile', 'Lag', 'Lead' and 'NthValue').
    | WField FieldName
      -- ^ Field for which to compute a window operation. Not needed for operations
      --   that do not apply to fields such as 'Count', 'Rank', and 'DenseRank'.


windowFieldProperty :: Window -> Pair
windowFieldProperty (WAggregateOp op) = "op" .= operationSpec op
windowFieldProperty (WOp op) = "op" .= wOperationLabel op
windowFieldProperty (WParam n) = "param" .= n
windowFieldProperty (WField f) = "field" .= f  -- was "field_ f"


-- | Window-specific operation for transformations (for use with 'WOp').
--
--   @since 0.4.0.0

data WOperation
    = RowNumber
      -- ^ Assign consecutive row number to values in a data object to be applied in a window transform.
    | Rank
      -- ^ Rank function to be applied in a window transform.
    | DenseRank
      -- ^ Dense rank function to be applied in a window transform.
    | PercentRank
      -- ^ Percentile of values in a sliding window to be applied in a window transform.
    | CumeDist
      -- ^ Cumulative distribution function to be applied in a window transform.
    | Ntile
      -- ^ Value preceding the current object in a sliding window to be applied in a window transform.
    | Lag
      -- ^ Value preceding the current object in a sliding window to be applied in a window transform.
    | Lead
      -- ^ Value following the current object in a sliding window to be applied in a window transform.
    | FirstValue
      -- ^ First value in a sliding window to be applied in a window transform.
    | LastValue
      -- ^ Last value in a sliding window to be applied in a window transform.
    | NthValue
      -- ^ Nth value in a sliding window to be applied in a window transform.


wOperationLabel :: WOperation -> T.Text
wOperationLabel RowNumber = "row_number"
wOperationLabel Rank = "rank"
wOperationLabel DenseRank = "dense_rank"
wOperationLabel PercentRank = "percent_rank"
wOperationLabel CumeDist = "cume_dist"
wOperationLabel Ntile = "ntile"
wOperationLabel Lag = "lag"
wOperationLabel Lead = "lead"
wOperationLabel FirstValue = "first_value"
wOperationLabel LastValue = "last_value"
wOperationLabel NthValue = "nth_value"

{-|

Type of binning property to customise. See the
<https://vega.github.io/vega-lite/docs/bin.html Vega-Lite documentation> for
more details.

This is used with: 'Graphics.Vega.VegaLite.binAs', 'Graphics.Vega.VegaLite.DBin', 'Graphics.Vega.VegaLite.FBin', 'Graphics.Vega.VegaLite.HBin', 'Graphics.Vega.VegaLite.MBin', 'Graphics.Vega.VegaLite.OBin',
'Graphics.Vega.VegaLite.PBin', and 'Graphics.Vega.VegaLite.TBin'.

-}

-- based on schema 3.3.0 #/definitions/BinParams

data BinProperty
    = AlreadyBinned Bool
      -- ^ Should the input data be treated as already binned?
      --
      --   @since 0.4.0.0
    | BinAnchor Double
      -- ^ A value in the binned domain at which to anchor the bins, shifting the bin
      --   boundaries if necessary to ensure that a boundary aligns with the anchor
      --   value.
      --
      --   @since 0.4.0.0
    | Base Double
      -- ^ The number base to use for automatic bin determination.
      --
      --   Default is @10@.
    | Divide [Double]
      -- ^ Scale factors indicating allowable subdivisions.
      --
      --   Default is @[5, 2]@.
      --
      --   Prior to @0.4.0.0@ the @Divide@ constructor took two numbers.
    | Extent Double Double
      -- ^ The range (minimum, maximum) of the desired bin values.
    | MaxBins Int
      -- ^ The maxium number of bins.
      --
      --   Default is @6@ for 'Graphics.Vega.VegaLite.row', 'Graphics.Vega.VegaLite.column', and 'Graphics.Vega.VegaLite.shape' channels,
      --   @10@ otherwise.
    | MinStep Double
      -- ^ A minimum allowable step size.
    | Nice Bool
      -- ^ If @True@, the bin boundaries are adjusted to use human-friendly values,
      --   such as multiples of ten.
      --
      --   Default is @True@.
    | SelectionExtent SelectionLabel
      -- ^ Set the range based on an interactive selection. The label
      --   must reference an interval selection, but this constraint is
      --   /not enforced/ at compile or run time.
      --
      --   @
      --   sel = 'Graphics.Vega.VegaLite.selection'
      --         . 'Graphics.Vega.VegaLite.select' \"brush\" 'Graphics.Vega.VegaLite.Interval' [ 'Graphics.Vega.VegaLite.Encodings' [ 'Graphics.Vega.VegaLite.ChX' ] ]
      --   enc = 'Graphics.Vega.VegaLite.encoding'
      --         . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.X' [ 'Graphics.Vega.VegaLite.PName' \"temperature\"
      --                      , 'Graphics.Vega.VegaLite.PmType' 'Graphics.Vega.VegaLite.Quantitative'
      --                      , 'Graphics.Vega.VegaLite.PBin' [ 'SelectionExtent' \"brush\" ]
      --                      ]
      --   @
      --
      --   @since 0.5.0.0
    | Step Double
      -- ^ The step size to use between bins.
      --
      --   If specified, 'MaxBins' and other related options are ignored.
    | Steps [Double]
      -- ^ Pick the step size from this list.


binProperty :: BinProperty -> Pair
binProperty (AlreadyBinned b) = "binned" .= b
binProperty (BinAnchor x) = "anchor" .= x
binProperty (Base x) = "base" .= x
binProperty (Divide xs) = "divide" .= xs
binProperty (Extent mn mx) = "extent" .= [ mn, mx ]
binProperty (SelectionExtent s) = "extent" .= object [ "selection" .= s ]
binProperty (MaxBins n) = "maxbins" .= n
binProperty (MinStep x) = "minstep" .= x
binProperty (Nice b) = "nice" .= b
binProperty (Step x) = "step" .= x
binProperty (Steps xs) = "steps" .= xs


bin :: [BinProperty] -> Pair
bin [] = "bin" .= True
bin xs = "bin" .= object (map binProperty xs)

binned_ :: Pair
binned_ = "bin" .= fromT "binned"


-- | Properties for a window transform.
--
--   @since 0.4.0.0

data WindowProperty
    = WFrame (Maybe Int) (Maybe Int)
      -- ^ Moving window for use by a window transform. When a number is
      --   given, via @Just@, then it indicates the offset from the current
      --   data object. A @Nothing@ indicates an un-bounded number of rows
      --   preceding or following the current data object.
    | WIgnorePeers Bool
      -- ^ Should the sliding window in a window transform ignore peer
      --   values (those considered identical by the sort criteria).
    | WGroupBy [FieldName]
      -- ^ The fields for partitioning data objects in a window transform
      --   into separate windows. If not specified, all points will be in a
      --   single group.
    | WSort [SortField]
      -- ^ Comparator for sorting data objects within a window transform.


-- This is different to how Elm's VegaLite handles this (as of version 1.12.0)
-- Helpers for windowPropertySpec

wpFrame , wpIgnorePeers, wpGroupBy, wpSort :: WindowProperty -> Maybe VLSpec
wpFrame (WFrame m1 m2) = Just (toJSON [allowNull m1, allowNull m2])
wpFrame _ = Nothing

wpIgnorePeers (WIgnorePeers b) = Just (toJSON b)
wpIgnorePeers _ = Nothing

wpGroupBy (WGroupBy fs) = Just (toJSON fs)
wpGroupBy _ = Nothing

wpSort (WSort sfs) = Just (toJSON (map sortFieldSpec sfs))
wpSort _ = Nothing

windowTS ::
  [([Window], FieldName)]
  -> [WindowProperty]
  -> TransformSpec
windowTS wss wps =
  let addField n a = case mapMaybe a wps of
                       [x] -> [n .= x]
                       _ -> []

      winFieldDef (ws, out) = object ("as" .= out : map windowFieldProperty ws)

      fields = [ "window" .= map winFieldDef wss ]
               <> addField "frame" wpFrame
               <> addField "ignorePeers" wpIgnorePeers
               <> addField "groupby" wpGroupBy
               <> addField "sort" wpSort

  in TS (object fields)


joinAggregateTS :: [VLSpec] -> [WindowProperty] -> TransformSpec
joinAggregateTS ops wps =
  let addField n a = case mapMaybe a wps of
                       [x] -> [n .= x]
                       _ -> []

      fields = [ "joinaggregate" .= ops ]
               <> addField "frame" wpFrame
               <> addField "ignorePeers" wpIgnorePeers
               <> addField "groupby" wpGroupBy
               <> addField "sort" wpSort

  in TS (object fields)


-- | This is used with 'Graphics.Vega.VegaLite.impute' and 'Graphics.Vega.VegaLite.PImpute'.
--
--   @since 0.4.0.0

data ImputeProperty
    = ImFrame (Maybe Int) (Maybe Int)
      -- ^ 1d window over which data imputation values are generated. The two
      --   parameters should either be @Just@ a number indicating the offset from the current
      --   data object, or @Nothing@ to indicate unbounded rows preceding or following the
      --   current data object.
    | ImKeyVals DataValues
      -- ^ Key values to be considered for imputation.
    | ImKeyValSequence Double Double Double
      -- ^ Key values to be considered for imputation as a sequence of numbers between
      --   a start (first parameter), to less than an end (second parameter) in steps of
      --   the third parameter.
    | ImMethod ImMethod
      -- ^ How is the imputed value constructed.
      --
      --   When using @ImMethod 'ImValue'@, the replacement value is
      --   set with 'ImNewValue'.
    | ImGroupBy [FieldName]
      -- ^ Allow imputing of missing values on a per-group basis. For use with the impute
      --   transform only and not a channel encoding.
    | ImNewValue DataValue
      -- ^ The replacement value (when using @ImMethod 'ImValue'@).


imputeProperty :: ImputeProperty -> Pair
imputeProperty (ImFrame m1 m2) = "frame" .= map allowNull [m1, m2]
imputeProperty (ImKeyVals dVals) = "keyvals" .= dataValuesSpecs dVals
imputeProperty (ImKeyValSequence start stop step) =
  "keyvals" .= object ["start" .= start, "stop" .= stop, "step" .= step]
imputeProperty (ImMethod method) = "method" .= imMethodLabel method
imputeProperty (ImNewValue dVal) = "value" .= dataValueSpec dVal
imputeProperty (ImGroupBy _) = "groupby" .= A.Null


imputePropertySpecFrame, imputePropertySpecKeyVals,
  imputePropertySpecKeyValSequence, imputePropertySpecGroupBy,
  imputePropertySpecMethod, imputePropertySpecValue :: ImputeProperty -> Maybe VLSpec

imputePropertySpecFrame (ImFrame m1 m2) = Just (toJSON (map allowNull [m1, m2]))
imputePropertySpecFrame _ = Nothing

imputePropertySpecKeyVals (ImKeyVals dVals) = Just (toJSON (dataValuesSpecs dVals))
imputePropertySpecKeyVals _ = Nothing

imputePropertySpecKeyValSequence (ImKeyValSequence start stop step) =
  let obj = ["start" .= start, "stop" .= stop, "step" .= step]
  in Just (object obj)
imputePropertySpecKeyValSequence _ = Nothing

imputePropertySpecGroupBy (ImGroupBy fields) = Just (toJSON fields)
imputePropertySpecGroupBy _ = Nothing

imputePropertySpecMethod (ImMethod method) = Just (toJSON (imMethodLabel method))
imputePropertySpecMethod _ = Nothing

imputePropertySpecValue (ImNewValue dVal) = Just (dataValueSpec dVal)
imputePropertySpecValue _ = Nothing


impute_ :: [ImputeProperty] -> Pair
impute_ ips = "impute" .= object (map imputeProperty ips)


imputeTS ::
  FieldName
  -- ^ The data field to process.
  -> FieldName
  -- ^ The key field to uniquely identify data objects within a group.
  -> [ImputeProperty]
  -- ^ Define how the imputation works.
  -> TransformSpec
imputeTS field key imProps =
  let addField n a = case mapMaybe a imProps of
                       [x] -> [n .= x]
                       _ -> []

      fields = [ "impute" .= field
               , "key" .= key ]
               <> addField "frame" imputePropertySpecFrame
               -- TODO: can we combine the keyvals options?
               <> addField "keyvals" imputePropertySpecKeyVals
               <> addField "keyvals" imputePropertySpecKeyValSequence
               <> addField "method" imputePropertySpecMethod
               <> addField "groupby" imputePropertySpecGroupBy
               <> addField "value" imputePropertySpecValue

  in TS (object fields)


-- | Imputation method to use when replacing values.
--
--   @since 0.4.0.0

data ImMethod
  = ImMin
    -- ^ Use the minimum value.
  | ImMax
    -- ^ Use the maximum value.
  | ImMean
    -- ^ Use the mean value.
  | ImMedian
    -- ^ Use the median value.
  | ImValue
    -- ^ Use a replacement value (set with @ImNewValue@).


imMethodLabel :: ImMethod -> T.Text
imMethodLabel ImMin = "min"
imMethodLabel ImMax = "max"
imMethodLabel ImMean = "mean"
imMethodLabel ImMedian = "median"
imMethodLabel ImValue = "value"
