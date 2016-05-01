module Lib (
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

import Conversion
