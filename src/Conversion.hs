module Conversion (
  -- * Full conversion functions
    osgb36EastingsNorthingsToWGS84LatLon
  , wgs84LatLonToOSGB36EastingsNorthings
  -- * Coordinate system conversion functions
  , ellipsoidToGrid
  , gridToEllipsoid
  -- * Datum conversion functions
  , wgs84ToOSGB36
  , osgb36ToWGS84
  , readOSTNShifts
  , OSTNShifts
  ) where

import Data.Angle

import Conversion.Constants
import Conversion.ETRS89OSGB36
import Conversion.EllipsoidGrid

osgb36EastingsNorthingsToWGS84LatLon :: (RealFrac a, Floating a, Ord a, Show a) => OSTNShifts -> (a, a) -> (Degrees a, Degrees a)
osgb36EastingsNorthingsToWGS84LatLon o = gridToEllipsoid wgs84 . osgb36ToWGS84 o

wgs84LatLonToOSGB36EastingsNorthings :: (RealFrac a, Floating a, Ord a, Show a) => OSTNShifts -> (Degrees a, Degrees a) -> (a, a)
wgs84LatLonToOSGB36EastingsNorthings o = wgs84ToOSGB36 o . ellipsoidToGrid wgs84
