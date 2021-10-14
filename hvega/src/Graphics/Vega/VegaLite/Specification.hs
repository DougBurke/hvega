{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Specification
Copyright   : (c) Douglas Burke, 2018-2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

The structure of the Vega-Lite specification.

-}

module Graphics.Vega.VegaLite.Specification
       ( toVegaLite
       , toVegaLiteSchema
       , vlSchema2, vlSchema3, vlSchema4, vlSchema
       , fromVL
       , VLProperty(..)
       , VLSpec
       , VegaLite
       , PropertySpec
       , LabelledSpec
       , EncodingSpec(..)
       , toEncodingSpec
       , fromEncodingSpec
       , BuildEncodingSpecs
       , TransformSpec(..)
       , toTransformSpec
       , fromTransformSpec
       , BuildTransformSpecs
       , ResolveSpec(..)
       , toResolveSpec
       , fromResolveSpec
       , BuildResolveSpecs
       , SelectSpec(..)
       , toSelectSpec
       , fromSelectSpec
       , BuildSelectSpecs
       , ConfigureSpec(..)
       , toConfigureSpec
       , fromConfigureSpec
       , BuildConfigureSpecs
       , asSpec
       , specification
       , SelectionLabel
       )
    where

#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.Key as Key
#endif

-- VegaLite uses these symbols.
import Prelude hiding (filter, lookup, repeat)

import qualified Data.Text as T

import Control.Arrow (first)

-- Aeson's Value type conflicts with the Number type
#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson (Value, object, toJSON)
#else
import Data.Aeson (Value, object, (.=))
#endif

import Data.Maybe (catMaybes, fromMaybe)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

-- added in base 4.8.0.0 / ghc 7.10.1
import Numeric.Natural (Natural)


-- | A Vega Lite visualization, created by
--   'toVegaLite'. The contents can be extracted with 'fromVL'.
--
newtype VegaLite = VL (T.Text, [LabelledSpec])


-- | The Vega-Lite specification is represented as JSON.
type VLSpec = Value

baseSchema :: T.Text
baseSchema = "https://vega.github.io/schema/vega-lite/"

-- | The latest version 2 Vega-Lite schema (equivalent to
--   @'vlSchema' 2 Nothing Nothing Nothing@).
vlSchema2 :: T.Text
vlSchema2 = vlSchema 2 Nothing Nothing Nothing

-- | The latest version 3 Vega-Lite schema (equivalent to
--   @'vlSchema' 3 Nothing Nothing Nothing@).
vlSchema3 :: T.Text
vlSchema3 = vlSchema 3 Nothing Nothing Nothing

-- | The latest version 4 Vega-Lite schema (equivalent to
--   @'vlSchema' 4 Nothing Nothing Nothing@).
vlSchema4 :: T.Text
vlSchema4 = vlSchema 4 Nothing Nothing Nothing

-- | Create the Vega-Lite schema for an arbitrary version. See
--   <https://github.com/vega/schema> for more information on naming
--   and availability.
--
--   There is no validation of the input values.
--
--   Alpha and Beta releases can be specified by setting
--   the last argument; for instance to get the \"beta.0\"
--   version of version 4 you would use
--
--   @vlSchema 4 (Just 0) (Just 0) (Just "-beta.0")@
--
--   whereas
--
--   @vlSchema 4 Nothing Nothing Nothing@
--
--   refers to the latest release of version 4.
--
vlSchema ::
  Natural
  -- ^ The major version
  -> Maybe Natural
  -- ^ The minor version
  -> Maybe Natural
  -- ^ The \"micro\" version
  -> Maybe T.Text
  -- ^ Anything beyond "major.minor.micro" (e.g. \"-beta.0").
  -> T.Text
  -- ^ The Vega-Lite Schema
vlSchema major mminor mmicro mextra =
  let versions = [ pure (showNat major)
                 , showNat <$> mminor
                 , showNat <$> mmicro
                 ]
      version = T.intercalate "." (catMaybes versions)

  in baseSchema <> "v" <> version <> fromMaybe "" mextra <> ".json"


showNat :: Natural -> T.Text
showNat = T.pack . show


{-|

Convert a list of Vega-Lite specifications into a single JSON object
that may be passed to Vega-Lite for graphics generation. Commonly
these will include at least a data, mark, and encoding specification.

While simple properties like 'Graphics.Vega.VegaLite.mark' may be provided directly, it is
usually clearer to label more complex ones such as encodings as
separate expressions. This becomes increasingly helpful for
visualizations that involve composition of layers, repeats and facets.

Specifications can be built up by chaining a series of functions (such
as 'Graphics.Vega.VegaLite.dataColumn' or 'Graphics.Vega.VegaLite.position' in the example below). Functional
composition using the '.' operator allows this to be done compactly.

@
let dat = 'Graphics.Vega.VegaLite.dataFromColumns' []
          . 'Graphics.Vega.VegaLite.dataColumn' "a" ('Graphics.Vega.VegaLite.Strings' [ \"C", \"C", \"D", \"D", \"E", \"E" ])
          . 'Graphics.Vega.VegaLite.dataColumn' "b" ('Graphics.Vega.VegaLite.Numbers' [ 2, 7, 1, 2, 6, 8 ])

    enc = 'Graphics.Vega.VegaLite.encoding'
          . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.X' [ 'Graphics.Vega.VegaLite.PName' "a", 'Graphics.Vega.VegaLite.PmType' 'Graphics.Vega.VegaLite.Nominal' ]
          . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.Y' [ 'Graphics.Vega.VegaLite.PName' "b", 'Graphics.Vega.VegaLite.PmType' 'Graphics.Vega.VegaLite.Quantitative', 'Graphics.Vega.VegaLite.PAggregate' 'Graphics.Vega.VegaLite.Mean' ]

in 'toVegaLite' [ dat [], 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Bar' [], enc [] ]
@

The schema used is <https://github.com/vega/schema version 4 of Vega-Lite>,
and please [report an issue](https://github.com/DougBurke/hvega/issues) if
you find a problem with the output of @hvega@. Use 'toVegaLiteSchema' if you
need to create a Vega-Lite specification which uses a different version of
the schema.

-}

toVegaLite :: [PropertySpec] -> VegaLite
toVegaLite = toVegaLiteSchema vlSchema4


{-|
A version of 'toVegaLite' that allows you to change the Vega-Lite
schema version of the visualization.

@
'toVegaLiteSchema' 'vlSchema3' props
@

Note that the schema is __only used__ to fill in the @\"$schema\"@
field of the JSON structure. It __does not__ change the JSON
encoding of the visualization.

-}

toVegaLiteSchema ::
  T.Text
  -- ^ The schema to use (e.g. 'vlSchema4' or created with 'vlSchema').
  --
  --   There is __no check__ that this schema represents Vega-Lite,
  --   and is just treated as a value added to the output JSON.
  --
  -> [PropertySpec]
  -- ^ The visualization.
  -> VegaLite
toVegaLiteSchema schema props = VL (schema, unProperty props)


unProperty :: [PropertySpec] -> [LabelledSpec]
unProperty = map (first vlPropertyLabel)


{-|

Obtain the Vega-Lite JSON (i.e. specification) for passing to a
Vega-Lite visualizer.

> let vlSpec = fromVL vl
> Data.ByteString.Lazy.Char8.putStrLn (Data.Aeson.Encode.Pretty.encodePretty vlSpec)

Note that there is __no__ validation done to ensure that the
output matches the Vega Lite schema. That is, it is possible to
create an invalid visualization with this module (e.g. missing a
data source or referring to an undefined field).

-}

fromVL ::
  VegaLite
  -> Value
  -- ^ Prior to version @0.5.0.0@ this was labelled as returning a 'VLSpec'
  --   value. It has been changed to 'Value' to indicate that this is the
  --   JSON representation of the visualization (noting that @VLSpec@ is
  --   an alias for @Value@).
fromVL (VL (schema, specs)) =
#if MIN_VERSION_aeson(2, 0, 0)
  let kvals = map (first Key.fromText) (("$schema", toJSON schema) : specs)
#else
  let kvals = ("$schema" .= schema) : specs
#endif
  in object kvals


{-|

Create a specification sufficient to define an element in a composed visualization
such as a superposed layer or juxtaposed facet. Typically a layer will contain a
full set of specifications that define a visualization with
the exception of the data specification which is usually defined outside of any one
layer. Whereas for repeated and faceted specs, the entire specification is provided.

@
spec1 = asSpec [ enc1 [], 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Line' [] ]
@
-}
asSpec :: [PropertySpec] -> VLSpec
#if MIN_VERSION_aeson(2, 0, 0)
asSpec = object . map (first Key.fromText) . unProperty
#else
asSpec = object . unProperty
#endif

{-|

Defines a specification object for use with faceted and repeated small multiples.

@
'toVegaLite'
    [ 'Graphics.Vega.VegaLite.facet' [ 'Graphics.Vega.VegaLite.RowBy' [ 'Graphics.Vega.VegaLite.FName' \"Origin\", 'Graphics.Vega.VegaLite.FmType' 'Graphics.Vega.VegaLite.Nominal' ] ]
    , 'specification' spec
    ]
@
-}
specification :: VLSpec -> PropertySpec
specification spec = (VLSpecification, spec)


{-|

A convenience type-annotation label. It is the same as 'Graphics.Vega.VegaLite.Data'.

@since 0.4.0.0
-}
type PropertySpec = (VLProperty, VLSpec)

{-|

Represents a named Vega-Lite specification, usually generated by a
function in this module. You shouldn't need to create @LabelledSpec@
tuples directly, but they can be useful for type annotations.
-}
type LabelledSpec = (T.Text, VLSpec)

{-|

Represent an encoding (input to 'Graphics.Vega.VegaLite.encoding').

It is expected that routines like 'Graphics.Vega.VegaLite.position'
and 'Graphics.Vega.VegaLite.color' are used to create values with this
type, but they can also be constructed and deconstructed manually
with 'toEncodingSpec' and 'fromEncodingSpec'.

@since 0.5.0.0

-}

newtype EncodingSpec = ES { unES :: (T.Text, VLSpec) }


{-|

This function is provided in case there is any need to inject
JSON into the Vega-Lite document that @hvega@ does not support
(due to changes in the Vega-Lite specification or missing
functionality in this module). If you find yourself needing
to use this then please
<https://github.com/DougBurke/hvega/issues report an issue>.

See also 'fromEncodingSpec'.

@since 0.5.0.0
-}

toEncodingSpec ::
  T.Text
  -- ^ The key to use for these settings (e.g. @\"color\"@ or @\"position\"@).
  -> VLSpec
  -- ^ The value of the key. This is expected to be an object, but there
  --   is no check on the value.
  --
  --   See the <https://github.com/vega/schema/tree/master/vega-lite Vega-Lite schema>
  --   for information on the supported values.
  -> EncodingSpec
toEncodingSpec lbl spec = ES (lbl, spec)


{-|

Extract the contents of an encoding specification. This may be
needed when the Vega-Lite specification adds or modifies settings
for a particular encoding, and @hvega@ has not been updated
to reflect this change. If you find yourself needing
to use this then please
<https://github.com/DougBurke/hvega/issues report an issue>.

See also 'toEncodingSpec'.

@since 0.5.0.0
-}

fromEncodingSpec ::
  EncodingSpec
  -> (T.Text, VLSpec)
  -- ^ The key for the settings (e.g. \"detail\") and the value of the
  --   key.
fromEncodingSpec = unES


{-|
Represent the functions that can be chained together and sent to
'Graphics.Vega.VegaLite.encoding'.

@since 0.5.0.0
-}

type BuildEncodingSpecs = [EncodingSpec] -> [EncodingSpec]

{-|

Represent a transformation (input to 'Graphics.Vega.VegaLite.transform').

It is expected that routines like 'Graphics.Vega.VegaLite.calculateAs'
and 'Graphics.Vega.VegaLite.filter' are used to create values with this
type, but they can also be constructed and deconstructed manually
with 'toTransformSpec' and 'fromTransformSpec'.

@since 0.5.0.0

-}

newtype TransformSpec = TS { unTS :: VLSpec }

{-|

This function is provided in case there is any need to inject
JSON into the Vega-Lite document that @hvega@ does not support
(due to changes in the Vega-Lite specification or missing
functionality in this module). If you find yourself needing
to use this then please
<https://github.com/DougBurke/hvega/issues report an issue>.

See also 'fromTransformSpec'.

@since 0.5.0.0
-}

toTransformSpec ::
  VLSpec
  -- ^ The tranform value, which is expected to be an object, but there
  --   is no check on this.
  --
  --   See the <https://github.com/vega/schema/tree/master/vega-lite Vega-Lite schema>
  --   for information on the supported values.
  -> TransformSpec
toTransformSpec = TS


{-|

Extract the contents of a transformation specification. This may be
needed when the Vega-Lite specification adds or modifies settings
for a particular encoding, and @hvega@ has not been updated
to reflect this change. If you find yourself needing
to use this then please
<https://github.com/DougBurke/hvega/issues report an issue>.

See also 'toTransformSpec'.

@since 0.5.0.0
-}

fromTransformSpec ::
  TransformSpec
  -> VLSpec
  -- ^ The transformation data.
fromTransformSpec = unTS

{-|
Represent the functions that can be chained together and sent to
'Graphics.Vega.VegaLite.transform'.

@since 0.5.0.0
-}

type BuildTransformSpecs = [TransformSpec] -> [TransformSpec]


{-|

Represent a set of resolution properties
(input to 'Graphics.Vega.VegaLite.resolve').

It is expected that 'Graphics.Vega.VegaLite.resolution' is used
to create values with this type, but they can also be constructed and
deconstructed manually with 'toResolveSpec' and 'fromResolveSpec'.

@since 0.5.0.0

-}

newtype ResolveSpec = RS { unRS :: (T.Text, VLSpec) }


{-|

This function is provided in case there is any need to inject
JSON into the Vega-Lite document that @hvega@ does not support
(due to changes in the Vega-Lite specification or missing
functionality in this module). If you find yourself needing
to use this then please
<https://github.com/DougBurke/hvega/issues report an issue>.

See also 'fromResolveSpec'.

@since 0.5.0.0
-}

toResolveSpec ::
  T.Text
  -- ^ The key to use for these settings (e.g. @\"axis\"@ or @\"scale\"@).
  -> VLSpec
  -- ^ The value of the key. This is expected to be an object, but there
  --   is no check on the value.
  --
  --   See the <https://github.com/vega/schema/tree/master/vega-lite Vega-Lite schema>
  --   for information on the supported values.
  -> ResolveSpec
toResolveSpec lbl spec = RS (lbl, spec)


{-|

Extract the contents of an resolve specification. This may be
needed when the Vega-Lite specification adds or modifies settings
for a particular resolve, and @hvega@ has not been updated
to reflect this change. If you find yourself needing
to use this then please
<https://github.com/DougBurke/hvega/issues report an issue>.

See also 'toResolveSpec'.

@since 0.5.0.0
-}

fromResolveSpec ::
  ResolveSpec
  -> (T.Text, VLSpec)
  -- ^ The key for the settings (e.g. \"legend\") and the value of the
  --   key.
fromResolveSpec = unRS


{-|
Represent the functions that can be chained together and sent to
'Graphics.Vega.VegaLite.resolve'.

@since 0.5.0.0
-}

type BuildResolveSpecs = [ResolveSpec] -> [ResolveSpec]


{-|

Represent a set of resolution properties
(input to 'Graphics.Vega.VegaLite.selection').

It is expected that 'Graphics.Vega.VegaLite.select' is used
to create values with this type, but they can also be constructed and
deconstructed manually with 'toSelectSpec' and 'fromSelectSpec'.

@since 0.5.0.0

-}

newtype SelectSpec = S { unS :: (T.Text, VLSpec) }


{-|

This function is provided in case there is any need to inject
JSON into the Vega-Lite document that @hvega@ does not support
(due to changes in the Vega-Lite specification or missing
functionality in this module). If you find yourself needing
to use this then please
<https://github.com/DougBurke/hvega/issues report an issue>.

See also 'fromSelectSpec'.

@since 0.5.0.0
-}

toSelectSpec ::
  SelectionLabel
  -- ^ The name given to the selection.
  -> VLSpec
  -- ^ The value of the key. This is expected to be an object, but there
  --   is no check on the value.
  --
  --   See the <https://github.com/vega/schema/tree/master/vega-lite Vega-Lite schema>
  --   for information on the supported values.
  -> SelectSpec
toSelectSpec lbl spec = S (lbl, spec)


{-|

Extract the contents of a select specification. This may be
needed when the Vega-Lite specification adds or modifies settings
for a particular select, and @hvega@ has not been updated
to reflect this change. If you find yourself needing
to use this then please
<https://github.com/DougBurke/hvega/issues report an issue>.

See also 'toSelectSpec'.

@since 0.5.0.0
-}

fromSelectSpec ::
  SelectSpec
  -> (SelectionLabel, VLSpec)
  -- ^ The name for the selection and its settings.
fromSelectSpec = unS


{-|
Represent the functions that can be chained together and sent to
'Graphics.Vega.VegaLite.selection'.

@since 0.5.0.0
-}

type BuildSelectSpecs = [SelectSpec] -> [SelectSpec]


{-|

Represent a set of configuration properties
(input to 'Graphics.Vega.VegaLite.configuration').

It is expected that 'Graphics.Vega.VegaLite.configuration' is used
to create values with this type, but they can also be constructed and
deconstructed manually with 'toConfigureSpec' and 'fromConfigureSpec'.

@since 0.5.0.0

-}

newtype ConfigureSpec = CS { unCS :: (T.Text, VLSpec) }


{-|

This function is provided in case there is any need to inject
JSON into the Vega-Lite document that @hvega@ does not support
(due to changes in the Vega-Lite specification or missing
functionality in this module). If you find yourself needing
to use this then please
<https://github.com/DougBurke/hvega/issues report an issue>.

See also 'fromConfigureSpec'.

@since 0.5.0.0
-}

toConfigureSpec ::
  T.Text
  -- ^ The key to use for these settings (e.g. @\"axis\"@ or @\"background\"@).
  -> VLSpec
  -- ^ The value of the key.
  --
  --   See the <https://github.com/vega/schema/tree/master/vega-lite Vega-Lite schema>
  --   for information on the supported values.
  -> ConfigureSpec
toConfigureSpec lbl spec = CS (lbl, spec)


{-|

Extract the contents of a configuration specification. This may be
needed when the Vega-Lite specification adds or modifies settings
for a particular configure, and @hvega@ has not been updated
to reflect this change. If you find yourself needing
to use this then please
<https://github.com/DougBurke/hvega/issues report an issue>.

See also 'toConfigureSpec'.

@since 0.5.0.0
-}

fromConfigureSpec ::
  ConfigureSpec
  -> (T.Text, VLSpec)
  -- ^ The key for the settings (e.g. \"numberFormat\") and the value of the
  --   key.
fromConfigureSpec = unCS


{-|
Represent the functions that can be chained together and sent to
'Graphics.Vega.VegaLite.configure'.

@since 0.5.0.0
-}

type BuildConfigureSpecs = [ConfigureSpec] -> [ConfigureSpec]

{-|

Top-level Vega-Lite properties. These are the ones that define the core of the
visualization grammar. All properties are created by functions which can be
arranged into seven broad groups:

[Data Properties] These relate to the input data to be visualized. Generated by
'Graphics.Vega.VegaLite.dataFromColumns', 'Graphics.Vega.VegaLite.dataFromRows', 'Graphics.Vega.VegaLite.dataFromUrl', 'Graphics.Vega.VegaLite.dataFromSource',
'Graphics.Vega.VegaLite.dataFromJson', 'Graphics.Vega.VegaLite.dataSequence', 'Graphics.Vega.VegaLite.sphere', and 'Graphics.Vega.VegaLite.graticule'.

[Transform Properties] These indicate that some transformation of input data should
be applied before encoding them visually. Generated by 'Graphics.Vega.VegaLite.transform'
and 'Graphics.Vega.VegaLite.projection' they can include data transformations such as 'Graphics.Vega.VegaLite.filter',
'Graphics.Vega.VegaLite.binAs' and 'Graphics.Vega.VegaLite.calculateAs' and geo transformations of longitude, latitude coordinates
used by marks such as 'Graphics.Vega.VegaLite.Geoshape', 'Graphics.Vega.VegaLite.Point', and 'Graphics.Vega.VegaLite.Line'.

[Mark Properties] These relate to the symbols used to visualize data items. They
are generated by 'Graphics.Vega.VegaLite.mark', and include types such as 'Graphics.Vega.VegaLite.Circle', 'Graphics.Vega.VegaLite.Bar', and 'Graphics.Vega.VegaLite.Line'.

[Encoding Properties] These specify which data elements are mapped to which mark characteristics
(known as /channels/). Generated by 'Graphics.Vega.VegaLite.encoding', they include encodings
such as 'Graphics.Vega.VegaLite.position', 'Graphics.Vega.VegaLite.color', 'Graphics.Vega.VegaLite.size', 'Graphics.Vega.VegaLite.shape', 'Graphics.Vega.VegaLite.text', 'Graphics.Vega.VegaLite.hyperlink', and
'Graphics.Vega.VegaLite.order'.

[Composition Properties] These allow visualization views to be combined to form more
complex visualizations. Generated by 'Graphics.Vega.VegaLite.layer', 'Graphics.Vega.VegaLite.repeat', 'Graphics.Vega.VegaLite.repeatFlow', 'Graphics.Vega.VegaLite.facet', 'Graphics.Vega.VegaLite.facetFlow',
'Graphics.Vega.VegaLite.vlConcat', 'Graphics.Vega.VegaLite.columns', 'Graphics.Vega.VegaLite.hConcat', 'Graphics.Vega.VegaLite.vConcat', 'Graphics.Vega.VegaLite.asSpec', and 'Graphics.Vega.VegaLite.resolve'.

[Interaction Properties] These allow interactions such as clicking, dragging and others
generated via a GUI or data stream to influence the visualization. Generated by
'Graphics.Vega.VegaLite.selection'.

[Supplementary and Configuration Properties] These provide a means to add metadata and
styling to one or more visualizations. Generated by
'Graphics.Vega.VegaLite.name', 'Graphics.Vega.VegaLite.title',
'Graphics.Vega.VegaLite.description', 'Graphics.Vega.VegaLite.background',
'Graphics.Vega.VegaLite.height', 'Graphics.Vega.VegaLite.heightStep',
'Graphics.Vega.VegaLite.width', 'Graphics.Vega.VegaLite.widthStep',
'Graphics.Vega.VegaLite.padding', 'Graphics.Vega.VegaLite.autosize',
'Graphics.Vega.VegaLite.viewBackground', and
'Graphics.Vega.VegaLite.configure'.

Prior to @0.4.0.0@ this was an opaque data type, as the constructors
were not exported. It is suggested that you do not import the
constructors to @VLProperty@ unless you need to transform the
Vega-Lite code in some manner (e.g. because @hvega@ is missing needed
functionality or is buggy).

Note that there is only a very-limited attempt to enforce the Vega-Lite
Schema (e.g. to ensure the required components are provided).

-}

-- based on schema 3.3.0 #/definitions/TopLevelSpec
-- which accepts one of
--    #/definitions/TopLevelUnitSpec
--    #/definitions/TopLevelFacetSpec
--    #/definitions/TopLevelLayerSpec
--    #/definitions/TopLevelRepeatSpec
--    #/definitions/TopLevelConcatSpec
--    #/definitions/TopLevelVConcatSpec
--    #/definitions/TopLevelHConcatSpec

data VLProperty
    = VLAlign
      -- ^ See 'Graphics.Vega.VegaLite.align'.
      --
      --   @since 0.4.0.0
    | VLAutosize
      -- ^ See 'Graphics.Vega.VegaLite.autosize'.
    | VLBackground
      -- ^ See 'Graphics.Vega.VegaLite.background'.
    | VLBounds
      -- ^ See 'Graphics.Vega.VegaLite.bounds'.
      --
      --   @since 0.4.0.0
    | VLCenter
      -- ^ See 'Graphics.Vega.VegaLite.center' and 'Graphics.Vega.VegaLite.centerRC'.
      --
      --   @since 0.4.0.0
    | VLColumns
      -- ^ See 'Graphics.Vega.VegaLite.columns'.
      --
      --   @since 0.4.0.0
    | VLConcat
      -- ^ See 'Graphics.Vega.VegaLite.vlConcat'.
      --
      --   @since 0.4.0.0
    | VLConfig
      -- ^ See 'Graphics.Vega.VegaLite.configure'.
    | VLData
      -- ^ See 'Graphics.Vega.VegaLite.dataFromColumns', 'Graphics.Vega.VegaLite.dataFromJson', 'Graphics.Vega.VegaLite.dataFromRows',
      --   'Graphics.Vega.VegaLite.dataFromSource', 'Graphics.Vega.VegaLite.dataFromUrl', 'Graphics.Vega.VegaLite.dataName', 'Graphics.Vega.VegaLite.dataSequence',
      --   'Graphics.Vega.VegaLite.dataSequenceAs', 'Graphics.Vega.VegaLite.graticule', 'Graphics.Vega.VegaLite.noData', and 'Graphics.Vega.VegaLite.sphere'.
    | VLDatasets
      -- ^ See 'Graphics.Vega.VegaLite.datasets'.
    | VLDescription
      -- ^ See 'Graphics.Vega.VegaLite.description'.
    | VLEncoding
      -- ^ See 'Graphics.Vega.VegaLite.encoding'.
    | VLFacet
      -- ^ See 'Graphics.Vega.VegaLite.facet' and 'Graphics.Vega.VegaLite.facetFlow'.
    | VLHConcat
      -- ^ See 'Graphics.Vega.VegaLite.hConcat'.
    | VLHeight
      -- ^ See 'Graphics.Vega.VegaLite.height' and
      --   'Graphics.Vega.VegaLite.heightStep'.
    | VLLayer
      -- ^ See 'Graphics.Vega.VegaLite.layer'.
    | VLMark
      -- ^ See 'Graphics.Vega.VegaLite.mark'.
    | VLName
      -- ^ See 'Graphics.Vega.VegaLite.name'.
    | VLPadding
      -- ^ See 'Graphics.Vega.VegaLite.padding'.
    | VLProjection
      -- ^ See 'Graphics.Vega.VegaLite.projection'.
    | VLRepeat
      -- ^ See 'Graphics.Vega.VegaLite.repeat' and 'Graphics.Vega.VegaLite.repeatFlow'.
    | VLResolve
      -- ^ See 'Graphics.Vega.VegaLite.resolve'.
    | VLSelection
      -- ^ See 'Graphics.Vega.VegaLite.selection'.
    | VLSpacing
      -- ^ See 'Graphics.Vega.VegaLite.alignRC', 'Graphics.Vega.VegaLite.spacing', and 'Graphics.Vega.VegaLite.spacingRC'.
      --
      --   @since 0.4.0.0
    | VLSpecification
      -- ^ See 'Graphics.Vega.VegaLite.specification'.
    | VLTitle
      -- ^ See 'Graphics.Vega.VegaLite.title'.
    | VLTransform
      -- ^ See 'Graphics.Vega.VegaLite.transform'.
    | VLUserMetadata
      -- ^ see 'Graphics.Vega.VegaLite.usermetadata'.
      --
      --   @since 0.4.0.0
    | VLVConcat
      -- ^ See 'Graphics.Vega.VegaLite.vConcat'.
    | VLViewBackground
      -- ^ See 'Graphics.Vega.VegaLite.viewBackground'.
      --
      --   @since 0.4.0.0
    | VLWidth
      -- ^ See 'Graphics.Vega.VegaLite.width' and
      --   'Graphics.Vega.VegaLite.widthStep'.


vlPropertyLabel :: VLProperty -> T.Text
vlPropertyLabel VLAlign = "align"
vlPropertyLabel VLAutosize = "autosize"
vlPropertyLabel VLBackground = "background"
vlPropertyLabel VLBounds = "bounds"
vlPropertyLabel VLCenter = "center"
vlPropertyLabel VLColumns = "columns"
vlPropertyLabel VLConcat = "concat"
vlPropertyLabel VLConfig = "config"
vlPropertyLabel VLData = "data"
vlPropertyLabel VLDatasets = "datasets"
vlPropertyLabel VLDescription = "description"
vlPropertyLabel VLEncoding = "encoding"
vlPropertyLabel VLFacet = "facet"
vlPropertyLabel VLHConcat = "hconcat"
vlPropertyLabel VLHeight = "height"
vlPropertyLabel VLLayer = "layer"
vlPropertyLabel VLMark = "mark"
vlPropertyLabel VLName = "name"
vlPropertyLabel VLPadding = "padding"
vlPropertyLabel VLProjection = "projection"
vlPropertyLabel VLRepeat = "repeat"
vlPropertyLabel VLResolve = "resolve"
vlPropertyLabel VLSelection = "selection"
vlPropertyLabel VLSpacing = "spacing"
vlPropertyLabel VLSpecification = "spec"
vlPropertyLabel VLTitle = "title"
vlPropertyLabel VLTransform = "transform"
vlPropertyLabel VLUserMetadata = "usermeta"
vlPropertyLabel VLVConcat = "vconcat"
vlPropertyLabel VLViewBackground = "view"
vlPropertyLabel VLWidth = "width"


{-|

Convenience type-annotation label to indicate the name, or label,
of a selection. It is expected to be a non-empty string, but there
is __no attempt__ to validate this.

@since 0.5.0.0
-}

type SelectionLabel = T.Text
