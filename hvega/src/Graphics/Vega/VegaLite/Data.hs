{-|
Module      : Graphics.Vega.VegaLite.Data
Copyright   : (c) Douglas Burke, 2018-2019
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable

Representing data values (excluding base time
types).

-}

module Graphics.Vega.VegaLite.Data
       ( DataValue(..)
       , DataValues(..)

       -- not for external export
       , dataValueSpec
       , dataValuesSpecs

       ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

import Data.Aeson (toJSON)


import Graphics.Vega.VegaLite.Specification (VLSpec)
import Graphics.Vega.VegaLite.Time
  ( DateTime
  , dateTimeSpec
  )


{-|

A single data value. This is used when a function or constructor
can accept values of different types (e.g. either a number or a string),
such as:
'Graphics.Vega.VegaLite.dataRow', 'Graphics.Vega.VegaLite.geometry', many constructors of the 'Graphics.Vega.VegaLite.Filter' type,
'Graphics.Vega.VegaLite.ImNewValue', and 'Graphics.Vega.VegaLite.SInit'.

-}
data DataValue
    = Boolean Bool
    | DateTime [DateTime]
    | Number Double
    | Str T.Text
    | NullValue
      -- ^ Create a JavaScript @null@ value. This can be useful when
      --   explictly recoding a value as undefined, such as in the following
      --   example:
      --
      --   @
      --   'Graphics.Vega.VegaLite.dataFromRows' []
      --     . 'Graphics.Vega.VegaLite.dataRow' [("x", 'Number' 1), ("y", 'Str' "good")]
      --     . 'Graphics.Vega.VegaLite.dataRow' [("x", 'Number' 2), ("y", 'NullValue')]
      --     . 'Graphics.Vega.VegaLite.dataRow' [("x", 'Number' 3), ("y", 'String' "bad")]
      --   @
      --
      --   For more-complex data sources - such as lists of defined
      --   and un-specified values, it is suggested that 'Graphics.Vega.VegaLite.dataFromJson'
      --   be used rather than 'Graphics.Vega.VegaLite.dataFromRows' or 'Graphics.Vega.VegaLite.dataFromColumns'.
      --
      --   @since 0.4.0.0

dataValueSpec :: DataValue -> VLSpec
dataValueSpec (Boolean b) = toJSON b
dataValueSpec (DateTime dt) = dateTimeSpec dt
dataValueSpec (Number x) = toJSON x
dataValueSpec (Str t) = toJSON t
dataValueSpec NullValue = A.Null


{-|

A list of data values. This is used when a function or constructor
can accept lists of different types (e.g. either a list of numbers
or a list of strings), such as:
'Graphics.Vega.VegaLite.dataColumn', 'Graphics.Vega.VegaLite.CustomSort', 'Graphics.Vega.VegaLite.FOneOf', or 'Graphics.Vega.VegaLite.ImKeyVals'.

If your data contains undefined values then it is suggested that
you convert it to JSON (e.g. 'A.Value') and then use 'Graphics.Vega.VegaLite.dataFromJson'.

-}
data DataValues
    = Booleans [Bool]
    | DateTimes [[DateTime]]
    | Numbers [Double]
    | Strings [T.Text]


dataValuesSpecs :: DataValues -> [VLSpec]
dataValuesSpecs (Booleans bs) = map toJSON bs
dataValuesSpecs (DateTimes dtss) = map dateTimeSpec dtss
dataValuesSpecs (Numbers xs) = map toJSON xs
dataValuesSpecs (Strings ss) = map toJSON ss

