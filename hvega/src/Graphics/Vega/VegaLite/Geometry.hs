{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Graphics.Vega.VegaLite.Geometry
Copyright   : (c) Douglas Burke, 2018-2021
License     : BSD3

Maintainer  : dburke.gw@gmail.com
Stability   : unstable
Portability : CPP, OverloadedStrings

Geometric shapes and projections.

-}

module Graphics.Vega.VegaLite.Geometry
       ( geometry
       , geoFeatureCollection
       , geometryCollection
       , Geometry(..)

       , sphere
       , graticule
       , GraticuleProperty(..)

       , projection
       , ProjectionProperty(..)
       , Projection(..)
       , ClipRect(..)

       -- not for external export
       , projectionProperty

       ) where

import qualified Data.Aeson as A

import qualified Data.Text as T

import Control.Arrow (second)

import Data.Aeson ((.=), object, toJSON)
import Data.Aeson.Types (Pair)

#if !(MIN_VERSION_base(4, 12, 0))
import Data.Monoid ((<>))
#endif


import Graphics.Vega.VegaLite.Data
  ( DataValue
  , dataValueSpec
  )
import Graphics.Vega.VegaLite.Foundation
  ( fromT
  , toObject
  )
import Graphics.Vega.VegaLite.Specification
  ( VLProperty(VLData, VLProjection)
  , VLSpec
  , PropertySpec
  )
import Graphics.Vega.VegaLite.Input
  ( Data
  )


type_ :: T.Text -> Pair
type_ t = "type" .= t


{-|

Specifies a list of geo features to be used in a geoShape specification.
Each feature object in this collection can be created with the 'geometry'
function.

@
'geoFeatureCollection'
    [ 'geometry' ('GeoPolygon' [ [ ( -3, 59 ), ( -3, 52 ), ( 4, 52 ), ( -3, 59 ) ] ])
        [ ( "myRegionName", 'Graphics.Vega.VegaLite.Str' "Northern region" ) ]
    , 'geometry' ('GeoPolygon' [ [ ( -3, 52 ), ( 4, 52 ), ( 4, 45 ), ( -3, 52 ) ] ])
        [ ( "myRegionName", 'Graphics.Vega.VegaLite.Str' "Southern region" ) ]
    ]
@
-}
geoFeatureCollection :: [VLSpec] -> VLSpec
geoFeatureCollection geoms =
  object [ type_ "FeatureCollection"
         , "features" .=  geoms
         ]


{-|

Specifies a list of geometry objects to be used in a geoShape specification.
Each geometry object in this collection can be created with the 'geometry'
function.

@
'geometryCollection'
    [ 'geometry' ('GeoPolygon' [ [ ( -3, 59 ), ( 4, 59 ), ( 4, 52 ), ( -3, 59 ) ] ]) []
    , 'geometry' ('GeoPoint' -3.5 55.5) []
    ]
@
-}
geometryCollection :: [VLSpec] -> VLSpec
geometryCollection geoms =
  object [ type_ "GeometryCollection"
         , "geometries" .= geoms
         ]


{-|

Types of geographic map projection. These are based on a subset of those provided
by the <https://github.com/d3/d3-geo d3-geo library>. For details of available
projections see the
<https://vega.github.io/vega-lite/docs/projection.html#projection-types Vega-Lite documentation>.
-}

-- based on schema 3.3.0 #/definitions/ProjectionType

data Projection
    = Albers
      -- ^ An Albers equal-area conic map projection.
    | AlbersUsa
      -- ^ An Albers USA map projection that combines continental USA with
      --   Alaska and Hawaii. Unlike other projection types, this remains
      --   unaffected by 'PrRotate'.
    | AzimuthalEqualArea
      -- ^ An azimuthal equal area map projection.
    | AzimuthalEquidistant
      -- ^ An azimuthal equidistant map projection.
    | ConicConformal
      -- ^ A conformal conic map projection.
    | ConicEqualArea
      -- ^ An equal area conic map projection.
    | ConicEquidistant
      -- ^ An equidistant conic map projection.
    | Custom T.Text
      -- ^ Specify the name of the custom D3 prohection to use. See the
      --   <https://vega.github.io/vega/docs/projections/#register Vega API>
      --   for more information.
      --
      --   An example: @Custom "winkle3"@
    | EqualEarth
      -- ^ An <https://github.com/d3/d3-geo#equal-earth Equal Earth map projection>
      --   that provides a reasonable shape approximation while retaining relative areas.
      --
      --   @since 0.5.0.0
    | Equirectangular
      -- ^ An equirectangular map projection that maps longitude to x and latitude to y.
      --   While showing less area distortion towards the poles than the default 'Mercator'
      --   projection, it is neither equal-area nor conformal.
    | Gnomonic
      -- ^ A gnomonic map projection.
    | Identity
      -- ^ The identiy projection. This can be combined with 'PrReflectX' and
      --   'PrReflectY' in the list of projection properties.
      --
      --   @since 0.4.0.0
    | Mercator
      -- ^ A Mercator map projection. This is the default projection of longitude, latitude
      --   values if no projection is set explicitly. It preserves shape (local angle) and
      --   lines of equal angular bearing remain parallel straight lines. The area is
      --   /significantly/ enlarged towards the poles.
    | NaturalEarth1
      -- ^ The <https://github.com/d3/d3-geo#geoNaturalEarth1 Natural Earth projection>
      --   is neither conformal nor equal-area, but is designed to be \"appealing to the
      --   eye\" for small-scale maps of the whole world.
      --
      --   @since 0.5.0.0
    | Orthographic
      -- ^ An orthographic map projection.
    | Stereographic
      -- ^ A stereographic map projection.
    | TransverseMercator
      -- ^ A transverse Mercator map projection.


projectionLabel :: Projection -> T.Text
projectionLabel Albers = "albers"
projectionLabel AlbersUsa = "albersUsa"
projectionLabel AzimuthalEqualArea = "azimuthalEqualArea"
projectionLabel AzimuthalEquidistant = "azimuthalEquidistant"
projectionLabel ConicConformal = "conicConformal"
projectionLabel ConicEqualArea = "conicEqualarea"
projectionLabel ConicEquidistant = "conicEquidistant"
projectionLabel (Custom pName) = pName
projectionLabel EqualEarth = "equalEarth"
projectionLabel Equirectangular = "equirectangular"
projectionLabel Gnomonic = "gnomonic"
projectionLabel Identity = "identity"
projectionLabel Mercator = "mercator"
projectionLabel NaturalEarth1 = "naturalEarth1"
projectionLabel Orthographic = "orthographic"
projectionLabel Stereographic = "stereographic"
projectionLabel TransverseMercator = "transverseMercator"


-- | Specifies a clipping rectangle for defining
--   the clip extent of a map projection.

data ClipRect
    = NoClip
      -- ^ No clipping it to be applied.
    | LTRB Double Double Double Double
      -- ^ The left, top, right, and bottom extents, in pixels,
      --   of a rectangular clip.


{-|

Properties for customising a geospatial projection that converts longitude,latitude
pairs into planar @(x,y)@ coordinate pairs for rendering and query. For details see the
<https://vega.github.io/vega-lite/docs/projection.html Vega-Lite documentation>.

This type has been changed in the @0.4.0.0@ release so that all constructors
start with @Pr@ rather than @P@ (and so provide some differentiation to the
'Graphics.Vega.VegaLite.PositionChannel' constructors).

-}

-- based on schema 3.3.0 #/definitions/Projection

data ProjectionProperty
    = PrType Projection
      -- ^ The type of the map projection.
    | PrClipAngle (Maybe Double)
      -- ^ The clipping circle angle in degrees. A value of @Nothing@ will switch to
      --   antimeridian cutting rather than small-circle clipping.
    | PrClipExtent ClipRect
      -- ^ Projection’s viewport clip extent to the specified bounds in pixels.
    | PrCenter Double Double
      -- ^ Projection’s center as longitude and latitude in degrees.
    | PrScale Double
      -- ^ The projection's zoom scale, which if set, overrides automatic scaling of a
      --   geo feature to fit within the viewing area.
      --
      --   @since 0.4.0.0
    | PrTranslate Double Double
      -- ^ A projection’s panning translation, which if set, overrides automatic positioning
      --   of a geo feature to fit within the viewing area
      --
      --   Note that the prefix is @Pr@ and not @P@, to match the Elm API.
      --
      --   @since 0.4.0.0
    | PrRotate Double Double Double
      -- ^ A projection’s three-axis rotation angle. The order is @lambda@ @phi@ @gamma@,
      --   and specifies the rotation angles in degrees about each spherical axis.
    | PrPrecision Double
      -- ^ Threshold for the projection’s adaptive resampling in pixels, and corresponds to the
      --   Douglas–Peucker distance. If precision is not specified, the projection’s current
      --   resampling precision of 0.707 is used.
      --
      --   Version 3.3.0 of the Vega-Lite spec claims this should be output as a string,
      --   but it is written out as a number since the
      --   [spec is in error](https://github.com/vega/vega-lite/issues/5190).
    | PrReflectX Bool
      -- ^ Reflect the x-coordinates after performing an identity projection. This
      --   creates a left-right mirror image of the geoshape marks when subject to an
      --   identity projection with 'Identity'.
      --
      -- @since 0.4.0.0
    | PrReflectY Bool
      -- ^ Reflect the y-coordinates after performing an identity projection. This
      --   creates a left-right mirror image of the geoshape marks when subject to an
      --   identity projection with 'Identity'.
      --
      -- @since 0.4.0.0
    | PrCoefficient Double
      -- ^ The @Hammer@ map projection coefficient.
    | PrDistance Double
      -- ^ The @Satellite@ map projection distance.
    | PrFraction Double
      -- ^ The @Bottomley@ map projection fraction.
    | PrLobes Int
      -- ^ Number of lobes in lobed map projections such as the @Berghaus star@.
    | PrParallel Double
      -- ^ Parallel for map projections such as the @Armadillo@.
    | PrRadius Double
      -- ^ Radius value for map projections such as the @Gingery@.
    | PrRatio Double
      -- ^ Ratio value for map projections such as the @Hill@.
    | PrSpacing Double
      -- ^ Spacing value for map projections such as the @Lagrange@.
    | PrTilt Double
      -- ^ @Satellite@ map projection tilt.


projectionProperty :: ProjectionProperty -> Pair
projectionProperty (PrType proj) = "type" .= projectionLabel proj
projectionProperty (PrClipAngle numOrNull) = "clipAngle" .= maybe A.Null toJSON numOrNull
projectionProperty (PrClipExtent rClip) =
  ("clipExtent", case rClip of
    NoClip -> A.Null
    LTRB l t r b -> toJSON (map toJSON [l, t, r, b])
  )
projectionProperty (PrCenter lon lat) = "center" .= [lon, lat]
projectionProperty (PrScale sc) = "scale" .= sc
projectionProperty (PrTranslate tx ty) = "translate" .= [tx, ty]
projectionProperty (PrRotate lambda phi gamma) = "rotate" .= [lambda, phi, gamma]
projectionProperty (PrPrecision pr) = "precision" .= pr  -- the 3.3.0 spec says this is a string, but that's wrong,  See https://github.com/vega/vega-lite/issues/5190
projectionProperty (PrReflectX b) = "reflectX" .= b
projectionProperty (PrReflectY b) = "reflectY" .= b
projectionProperty (PrCoefficient x) = "coefficient" .= x
projectionProperty (PrDistance x) = "distance" .= x
projectionProperty (PrFraction x) = "fraction" .= x
projectionProperty (PrLobes n) = "lobes" .= n
projectionProperty (PrParallel x) = "parallel" .= x
projectionProperty (PrRadius x) = "radius" .= x
projectionProperty (PrRatio x) = "ratio" .= x
projectionProperty (PrSpacing x) = "spacing" .= x
projectionProperty (PrTilt x) = "tilt" .= x


{-|

Sets the cartographic projection used for geospatial coordinates. A projection
defines the mapping from @(longitude,latitude)@ to an @(x,y)@ plane used for rendering.
This is useful when using the 'Graphics.Vega.VegaLite.Geoshape' mark. For further details see the
<https://vega.github.io/vega-lite/docs/projection.html Vega-Lite documentation>.

@
'projection' [ 'PrType' 'Orthographic', 'PrRotate' (-40) 0 0 ]
@
-}
projection :: [ProjectionProperty] -> PropertySpec
projection pProps = (VLProjection, object (map projectionProperty pProps))


{-|

Specifies the type and content of geometry specifications for programatically
creating GeoShapes. These can be mapped to the
<https://tools.ietf.org/html/rfc7946#section-3.1 GeoJson geometry object types>
where the pluralised type names refer to their @Multi@ prefixed equivalent in the
GeoJSON specification.
-}
data Geometry
    = GeoPoint Double Double
    -- ^ The GeoJson geometry @point@ type.
    | GeoPoints [(Double, Double)]
    -- ^ The GeoJson geometry @multi-point@ type.
    | GeoLine [(Double, Double)]
    -- ^ The GeoJson geometry @line@ type.
    | GeoLines [[(Double, Double)]]
    -- ^ The GeoJson geometry @multi-line@ type.
    | GeoPolygon [[(Double, Double)]]
    -- ^ The GeoJson geometry @polygon@ type.
    | GeoPolygons [[[(Double, Double)]]]
    -- ^ The GeoJson geometry @multi-polygon@ type.


{-|

Specifies a geometric object to be used in a geoShape specification. The first
parameter is the geometric type, the second an optional list of properties to be
associated with the object.

@
'geometry' ('GeoPolygon' [ [ ( -3, 59 ), ( 4, 59 ), ( 4, 52 ), ( -3, 59 ) ] ]) []
@
-}
geometry :: Geometry -> [(T.Text, DataValue)] -> VLSpec
geometry gType properties =
  object ([ ("type", fromT "Feature")
          , ("geometry", geometryTypeSpec gType) ]
          <> if null properties
             then []
             else [("properties",
                    toObject (map (second dataValueSpec) properties))]
         )


geometryTypeSpec :: Geometry -> VLSpec
geometryTypeSpec gType =
  let toCoords :: [(Double, Double)] -> VLSpec
      toCoords = toJSON -- rely on Aeson converting a pair to a 2-element list

      toCoordList :: [[(Double, Double)]] -> VLSpec
      toCoordList = toJSON . map toCoords  -- this is just toJSON isn't it?

      -- can we replace this infinite tower of toJSON calls with one toJSON call?
      (ptype, cs) = case gType of
        GeoPoint x y -> ("Point", toJSON [x, y])
        GeoPoints coords -> ("MultiPoint", toCoords coords)
        GeoLine coords -> ("LineString", toCoords coords)
        GeoLines coords -> ("MultiLineString", toCoordList coords)
        GeoPolygon coords -> ("Polygon", toCoordList coords)
        GeoPolygons ccoords -> ("MultiPolygon", toJSON (map toCoordList ccoords))

  in object [("type", ptype), ("coordinates", cs)]


{-|

Generate a data source that is a sphere for bounding global geographic data.
The sphere will be subject to whatever projection is specified for the view.

@
'Graphics.Vega.VegaLite.toVegaLite'
    [ 'sphere'
    , 'projection' [ 'PrType' 'Orthographic' ]
    , 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Geoshape' [ 'Graphics.Vega.VegaLite.MFill' "aliceblue" ]
    ]
@

@since 0.4.0.0
-}
sphere :: Data
sphere = (VLData, object ["sphere" .= True])


{-|

Generate a grid of lines of longitude (meridians) and latitude
(parallels).

@
let proj = 'projection' [ 'PrType' 'Orthographic' ]
    sphereSpec = 'Graphics.Vega.VegaLite.asSpec' [ 'sphere'
                        , 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Geoshape' [ 'Graphics.Vega.VegaLite.MFill' "aliceblue" ] ]
    gratSpec =
        'Graphics.Vega.VegaLite.asSpec'
            [ 'graticule' [ 'GrStep' (5, 5) ]
            , 'Graphics.Vega.VegaLite.mark' 'Graphics.Vega.VegaLite.Geoshape' [ 'Graphics.Vega.VegaLite.MFilled' False, 'Graphics.Vega.VegaLite.MStrokeWidth' 0.3 ]
            ]
in 'Graphics.Vega.VegaLite.toVegaLite' [ proj, 'Graphics.Vega.VegaLite.layer' [ sphereSpec, gratSpec ] ]
@

@since 0.4.0.0

-}
graticule ::
  [GraticuleProperty] -- ^ An empty list uses the default parameters
  -> Data
graticule [] = (VLData, object ["graticule" .= True])
graticule grProps =
  (VLData, object ["graticule" .= object (map graticuleProperty grProps)])


{-|

Determine the properties of graticules. See the
<https://vega.github.io/vega-lite/docs/data.html#graticule Vega-Lite documentation> for details.

@since 0.4.0.0

-}
data GraticuleProperty
    = GrExtent (Double, Double) (Double, Double)
    -- ^ Define the extent of both the major and minor graticules.
    --   The range is given as longitude, latitude pairs of the
    --   minimum and then maximum extent. The units are degrees.
    | GrExtentMajor (Double, Double) (Double, Double)
    -- ^ As @GrExtent@ but for the major graticule lines only.
    | GrExtentMinor (Double, Double) (Double, Double)
    -- ^ As @GrExtent@ but for the minor graticule lines only.
    | GrStep (Double, Double)
    -- ^ The step angles for the graticule lines, given as a longitude,
    --   latitude pair defining the EW and NS intervals respectively.
    --   The units are degrees.
    --
    --   By default major graticule lines extend to both poles but the
    --   minor lines stop at ±80 degrees latitude.
    | GrStepMajor (Double, Double)
    -- ^ As @GrStep@ but for the major graticule lines only.
    --
    --   The default is @(90, 360)@.
    | GrStepMinor (Double, Double)
    -- ^ As @GrStep@ but for the minor graticule lines only.
    --
    --   The default is @(10, 10)@.
    | GrPrecision Double
    -- ^ The precision of the graticule. The units are degrees.
    --   A smaller value reduces visual artifacts (steps) but takes longer
    --   to render.
    --
    --   The default is @2.5@.


graticuleProperty :: GraticuleProperty -> Pair
graticuleProperty (GrExtent (lng1, lat1) (lng2, lat2)) =
  "extent" .= [[lng1, lat1], [lng2, lat2]]
graticuleProperty (GrExtentMajor (lng1, lat1) (lng2, lat2)) =
  "extentMajor" .= [[lng1, lat1], [lng2, lat2]]
graticuleProperty (GrExtentMinor (lng1, lat1) (lng2, lat2)) =
  "extentMinor" .= [[lng1, lat1], [lng2, lat2]]
graticuleProperty (GrStep (lng, lat)) = "step" .= [lng, lat]
graticuleProperty (GrStepMajor (lng, lat)) = "stepMajor" .= [lng, lat]
graticuleProperty (GrStepMinor (lng, lat)) = "stepMinor" .= [lng, lat]
graticuleProperty (GrPrecision x) = "precision" .= x
