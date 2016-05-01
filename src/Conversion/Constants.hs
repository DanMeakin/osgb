module Conversion.Constants where

import Data.Angle

data EllipsoidConstants a = EllipsoidConstants { _semiMajorAxisA :: a
                                               , _semiMinorAxisB :: a
                                               } deriving (Show, Eq)

_eccentricitySquared :: Floating a => EllipsoidConstants a -> a
_eccentricitySquared c = (a² - b²) / a²
  where a² = _semiMajorAxisA c ** 2
        b² = _semiMinorAxisB c ** 2

data ProjectionConstants a = ProjectionConstants { _scaleFactor :: a
                                                 , _trueOriginLatitude :: Degrees a
                                                 , _trueOriginLongitude :: Degrees a
                                                 , _trueOriginEastings :: a
                                                 , _trueOriginNorthings :: a
                                                 } deriving (Show, Eq)

data Constants a = Constants { _ellipsoid :: EllipsoidConstants a
                             , _projection :: ProjectionConstants a
                             } deriving (Show, Eq)

semiMajorAxisA = _semiMajorAxisA . _ellipsoid
semiMinorAxisB = _semiMinorAxisB . _ellipsoid

eccentricitySquared :: Floating a => Constants a -> a
eccentricitySquared = _eccentricitySquared . _ellipsoid

scaleFactor = _scaleFactor . _projection
trueOriginLatitude = _trueOriginLatitude . _projection
trueOriginLongitude = _trueOriginLongitude . _projection
trueOriginEastings = _trueOriginEastings . _projection
trueOriginNorthings = _trueOriginNorthings . _projection

-------------------------
-- Constants Sets (UK) --
-------------------------

-- | osgb36 defines the constants to use when making calculations on the OSGB36
--   datum.
osgb36 = Constants { _ellipsoid = airy1830Ellipsoid
                   , _projection = nationalGridProjection 
                   }

-- | wgs84 defines the constants to use when making calculations on the WGS84
--   datum.
wgs84 :: Floating a => Constants a 
wgs84 = Constants { _ellipsoid = wgs84Ellipsoid
                  , _projection = nationalGridProjection 
                  }

------------------------------
-- Ellipsoid Constants (UK) --
------------------------------

airy1830Ellipsoid = EllipsoidConstants { _semiMajorAxisA = 6377563.396 
                                       , _semiMinorAxisB = 6356256.909
                                       }

airy1830EllipsoidModified = EllipsoidConstants { _semiMajorAxisA = 6377340.189
                                               , _semiMinorAxisB = 6356034.447
                                               }

international1924Ellipsoid = EllipsoidConstants { _semiMajorAxisA = 6378388.000
                                                , _semiMinorAxisB = 6356911.946 
                                                }

wgs84Ellipsoid :: Floating a => EllipsoidConstants a
wgs84Ellipsoid = EllipsoidConstants { _semiMajorAxisA = 6378137.000 
                                    , _semiMinorAxisB = 6356752.3141 
                                    }

------------------------------------------
-- Transverse Mercator Projections (UK) --
------------------------------------------

nationalGridProjection :: Floating a => ProjectionConstants a
nationalGridProjection = ProjectionConstants { _scaleFactor = 0.9996012717 
                                             , _trueOriginLatitude = Degrees 49
                                             , _trueOriginLongitude = Degrees (-2)
                                             , _trueOriginEastings = 400000
                                             , _trueOriginNorthings = -100000
                                             }

itmGridProjection = ProjectionConstants { _scaleFactor = 0.99982
                                        , _trueOriginLatitude = Degrees 53.5
                                        , _trueOriginLongitude = Degrees (-8)
                                        , _trueOriginEastings = 600000
                                        , _trueOriginNorthings = 750000
                                        }

irishNationalGridProjection = ProjectionConstants { _scaleFactor = 1.000035
                                                  , _trueOriginLatitude = Degrees 53.5
                                                  , _trueOriginLongitude = Degrees (-8)
                                                  , _trueOriginEastings = 200000
                                                  , _trueOriginNorthings = 250000
                                                  }

utmZone29Projection = ProjectionConstants { _scaleFactor = 0.9996
                                          , _trueOriginLatitude = 0
                                          , _trueOriginLongitude = Degrees (-9)
                                          , _trueOriginEastings = 500000
                                          , _trueOriginNorthings = 0
                                          }

utmZone30Projection = ProjectionConstants { _scaleFactor = 0.9996
                                          , _trueOriginLatitude = 0
                                          , _trueOriginLongitude = Degrees (-3)
                                          , _trueOriginEastings = 500000
                                          , _trueOriginNorthings = 0
                                          }

utmZone31Projection = ProjectionConstants { _scaleFactor = 0.9996
                                          , _trueOriginLatitude = 0
                                          , _trueOriginLongitude = Degrees 3
                                          , _trueOriginEastings = 500000
                                          , _trueOriginNorthings = 0
                                          }
