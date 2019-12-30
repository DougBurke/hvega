{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Transform
Copyright   : (c) Douglas Burke, 2018-2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : OverloadedStrings

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

         -- not for extgernal export
       , aggregate_
       , op_
       , bin
       , binned_
       , binProperty
       , windowFieldProperty

       ) where

import qualified Data.Text as T

import Data.Aeson ((.=), object)


import Graphics.Vega.VegaLite.Specification (VLSpec, LabelledSpec)
import Graphics.Vega.VegaLite.Foundation
  ( field_
  , fromT
  )


{-|

Type of aggregation operation. See the
<https://vega.github.io/vega-lite/docs/aggregate.html#ops Vega-Lite documentation>
for more details.

The @Average@ constructor was removed in version @0.4.0.0@; use 'Mean' instead.

-}
data Operation
    = ArgMax (Maybe T.Text)
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
    | ArgMin (Maybe T.Text)
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
operationSpec Q1 = "q1"
operationSpec Q3 = "q3"
operationSpec Stderr = "stderr"
operationSpec Stdev = "stdev"
operationSpec StdevP = "stdevp"
operationSpec Sum = "sum"
operationSpec Valid = "valid"
operationSpec Variance = "variance"
operationSpec VarianceP = "variancep"


aggregate_ :: Operation -> LabelledSpec
aggregate_ op = "aggregate" .= operationSpec op

op_ :: Operation -> LabelledSpec
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
    | WField T.Text
      -- ^ Field for which to compute a window operation. Not needed for operations
      --   that do not apply to fields such as 'Count', 'Rank', and 'DenseRank'.


windowFieldProperty :: Window -> LabelledSpec
windowFieldProperty (WAggregateOp op) = "op" .= operationSpec op
windowFieldProperty (WOp op) = "op" .= wOperationLabel op
windowFieldProperty (WParam n) = "param" .= n
windowFieldProperty (WField f) = field_ f


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
    | Step Double
      -- ^ The step size to use between bins.
      --
      --   If specified, 'MaxBins' and other related options are ignored.
    | Steps [Double]
      -- ^ Pick the step size from this list.


binProperty :: BinProperty -> LabelledSpec
binProperty (AlreadyBinned b) = "binned" .= b
binProperty (BinAnchor x) = "anchor" .= x
binProperty (Base x) = "base" .= x
binProperty (Divide xs) = "divide" .= xs
binProperty (Extent mn mx) = "extent" .= [ mn, mx ]
binProperty (MaxBins n) = "maxbins" .= n
binProperty (MinStep x) = "minstep" .= x
binProperty (Nice b) = "nice" .= b
binProperty (Step x) = "step" .= x
binProperty (Steps xs) = "steps" .= xs


bin :: [BinProperty] -> LabelledSpec
bin [] = "bin" .= True
bin xs = "bin" .= object (map binProperty xs)

binned_ :: LabelledSpec
binned_ = "bin" .= fromT "binned"
