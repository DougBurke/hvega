{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Specification
Copyright   : (c) Douglas Burke, 2018-2019
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
       , BuildLabelledSpecs
       , Angle
       , Color
       , Opacity
       , ZIndex
       , combineSpecs
       , asSpec
       , specification
       )
    where

-- VegaLite uses these symbols.
import Prelude hiding (filter, lookup, repeat)

import qualified Data.Text as T

import Control.Arrow (first)

-- Aeson's Value type conflicts with the Number type
import Data.Aeson (Value, object, (.=))
import Data.Maybe (catMaybes, fromMaybe)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif

-- added in base 4.8.0.0 / ghc 7.10.1
import Numeric.Natural (Natural)


-- | A Vega Lite visualization, created by
--   'toVegaLite'. The contents can be extracted with 'fromVL'.
--
newtype VegaLite =
  VL {
  fromVL :: VLSpec
  -- ^ Extract the specification for passing to a VegaLite visualizer.
  --
  --   > let vlSpec = fromVL vl
  --   > Data.ByteString.Lazy.Char8.putStrLn (Data.Aeson.Encode.Pretty.encodePretty vlSpec)
  --
  --   Note that there is __no__ validation done to ensure that the
  --   output matches the Vega Lite schema. That is, it is possible to
  --   create an invalid visualization with this module (e.g. missing a
  --   data source or referring to an undefined field).
  }

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
--   At the time of writing the latest version 4 schema - which
--   is @https://vega.github.io/schema/vega-lite/v4.0.0-beta.0.json@ -
--   can be specified as
--
--   @vlSchema 4 (Just 0) (Just 0) (Just "-beta.0")@
--
--   whereas
--
--   @vlSchema 4 Nothing Nothing Nothing@
--
--   refers to the latest version.
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

The schema used is <https://github.com/vega/schema version 3 of Vega-Lite>,
although there are some differences, in part because of bugs in @hvega@ -
in which case please [report an issue](https://github.com/DougBurke/hvega/issues) - but also because of issues with the Vega-Lite spec (for instance there
are several minor issues I have reported against version 3.3.0 of the
Vega-Lite schema).

Use 'toVegaLiteSchema' if you need to create a Vega-Lite specification
which uses a different version of the schema.

-}

-- TODO:
--    Should the input data, or converted to VLSpec, be stored
--    without the $schema key, so it can be easily combined with
--    other visualizations?
--
--    Could we make VegaLite a Semigroup so you can easily combine
--    specifications? However, what would that mean (concatenation,
--    if so what direction, anything else?)

toVegaLite :: [PropertySpec] -> VegaLite
toVegaLite = toVegaLiteSchema vlSchema3


{-|
A version of 'toVegaLite' that allows you to change the Vega-Lite
schema version of the visualization.

@
'toVegaLiteSchema' 'vlSchema4' props
@
-}

toVegaLiteSchema ::
  T.Text
  -- ^ The schema to use (e.g. 'vlSchema4' or created with 'vlSchema').
  -> [PropertySpec]
  -- ^ The visualization.
  -> VegaLite
toVegaLiteSchema schema props =
  let kvals = ("$schema" .= schema) : map toProp props
      toProp = first vlPropertyLabel

  in VL { fromVL = object kvals }


{-|

Combines a list of labelled specifications into a single specification.
This is useful when you wish to create
a single page with multiple visulizualizations.

@
'combineSpecs'
    [ ( "vis1", myFirstVis )
    , ( "vis2", mySecondVis )
    , ( "vis3", myOtherVis )
    ]
@
-}
combineSpecs :: [LabelledSpec] -> VLSpec
combineSpecs = object


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
asSpec = object . map (first vlPropertyLabel)


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

Represent those functions which can be chained together using function
composition to append new specifications onto an existing list.
-}
type BuildLabelledSpecs = [LabelledSpec] -> [LabelledSpec]


{-|

Convenience type-annotation label to indicate a color value.
There is __no attempt__ to validate that the user-supplied input
is a valid color.

Any supported HTML color specification can be used, such as:

@
\"#eee\"
\"#734FD8\"
\"crimson\"
\"rgb(255,204,210)\"
\"hsl(180, 50%, 50%)\"
@

@since 0.4.0.0
-}

type Color = T.Text


{-|

Convenience type-annotation label to indicate an opacity value, which
lies in the range 0 to 1 inclusive. There is __no attempt__ to validate
that the user-supplied value falls in this range.

A value of 0 indicates fully transparent (see through), and 1 is
fully opaque (does not show anything it is on top of).

@since 0.4.0.0
-}

type Opacity = Double


{-|

Convenience type-annotation label to indicate an angle, which is measured
in degrees from the horizontal (so anti-clockwise).

The value should be in the range 0 to 360, inclusive, but __no attempt__ is
made to enforce this.

@since 0.4.0.0
-}

type Angle = Double


{-|

At what "depth" (z index) is the item to be drawn (a relative depth
for items in the visualization). The standard values are @0@ for
back and @1@ for front, but other values can be used if you want
to ensure a certain layering of items.

The following example is taken from a discussion with
<https://github.com/gicentre/elm-vegalite/issues/15#issuecomment-524527125 Jo Wood>:

@
let dcols = 'Graphics.Vega.VegaLite.dataFromColumns' []
              . 'Graphics.Vega.VegaLite.dataColumn' "x" ('Graphics.Vega.VegaLite.Numbers' [ 20, 10 ])
              . 'Graphics.Vega.VegaLite.dataColumn' "y" ('Graphics.Vega.VegaLite.Numbers' [ 10, 20 ])
              . 'Graphics.Vega.VegaLite.dataColumn' "cat" ('Graphics.Vega.VegaLite.Strings' [ "a", "b" ])

    axis lbl z = [ 'Graphics.Vega.VegaLite.PName' lbl, 'Graphics.Vega.VegaLite.PmType' 'Graphics.Vega.VegaLite.Quantitative', 'Graphics.Vega.VegaLite.PAxis' [ 'Graphics.Vega.VegaLite.AxZIndex' z ] ]
    enc = 'Graphics.Vega.VegaLite.encoding'
            . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.X' (axis "x" 2)
            . 'Graphics.Vega.VegaLite.position' 'Graphics.Vega.VegaLite.Y' (axis "y" 1)
            . 'Graphics.Vega.VegaLite.color' [ 'Graphics.Vega.VegaLite.MName' "cat", 'Graphics.Vega.VegaLite.MmType' 'Graphics.Vega.VegaLite.Nominal', 'Graphics.Vega.VegaLite.MLegend' [] ]

    cfg = 'Graphics.Vega.VegaLite.configure'
            . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.Axis' [ 'Graphics.Vega.VegaLite.GridWidth' 8 ])
            . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.AxisX' [ 'Graphics.Vega.VegaLite.GridColor' "red" ])
            . 'Graphics.Vega.VegaLite.configuration' ('Graphics.Vega.VegaLite.AxisY' [ 'Graphics.Vega.VegaLite.GridColor' "blue" ])

in 'toVegaLite' [ cfg []
              , dcols []
              , enc []
              , 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Circle' [ 'Graphics.Vega.VegaLite.MSize' 5000, 'Graphics.Vega.VegaLite.MOpacity' 1 ]
              ]
@

<<images/zindex.png>>

<https://vega.github.io/editor/#/url/vega-lite/N4KABBYEQMYPYDsBmBLA5lAXGUk-QEMAPFAZwE0sdx9ao0AnFAEwGE4AbOBqqAIw4BXAKZQatAL4AacfijEyADSq5acxi3Zce2KA2HMxasNNl55JUirNr6TZgHUWAFwAWVABw2IE2afMAtgQMANbWxlCkKABeotgArAAMyTIRcAAOBDAozgCeVACMqbZ56XHQ2QwwHKJ+xRBQzATOBOG2AG4EQsJW2ADa3viqxnQwzbyt9SPmRFQATIlT0w352AWJg3j+y8PLDWPOvHxQS8tQs2uLm7arYAvXPoMAunWyUAAkpDCuwkG8rs5nOlSJgAPSg9rCNAEAB0aByrkEfBhKDgoK+PyCEKhBAAtBwcsIIQBmGEAK1IiBOb2ECHgzBQCAw2F25ng2ja0ygqGEHEMugO1L2UFK5SgCDgAUZXSFZxqaFp-LACEEHA4g22dAu1GFPL5vFmpzoot4AEdBAQEM4cs0UJDZVyFL0dXtIFBoozmMJtXMHiYNUboLdWbY9UqoPlA+YTbpzZbrS1rfao26nZzXe7Pd7Cn7fMY85BfL4gA View the visualization in the Vega Editor>

@since 0.4.0.0
-}

type ZIndex = Natural


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
styling to one or more visualizations. Generated by 'Graphics.Vega.VegaLite.name', 'Graphics.Vega.VegaLite.title', 'Graphics.Vega.VegaLite.description',
'Graphics.Vega.VegaLite.background', 'Graphics.Vega.VegaLite.height', 'Graphics.Vega.VegaLite.width', 'Graphics.Vega.VegaLite.padding', 'Graphics.Vega.VegaLite.autosize', 'Graphics.Vega.VegaLite.viewBackground',
and 'Graphics.Vega.VegaLite.configure'.

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
      -- ^ See 'Graphics.Vega.VegaLite.height'.
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
      -- ^ See 'Graphics.Vega.VegaLite.width'.


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
