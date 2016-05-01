{-|
This module contains functions for converting between the WGS84 and OSGB36
coordinate reference systems. The 'wgs84ToOSGB36' and 'osgb36ToWGS84' functions
are used to carry out the required conversions.

An accurate conversion requires the use of OSTN02 tables, which show the shift
to be applied to a given WGS84 coordinate to determine the OSGB36 coordinate.
The 
-}
module Conversion.ETRS89OSGB36 where

import Control.Applicative (liftA2)
import Data.Array
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lex.Fractional as B

data OSTNRecord = OSTNRecord { easting :: Integer
                             , northing :: Integer
                             , eastShift :: Double
                             , northShift :: Double 
                             , geoidHeight :: Double
                             , geoidDatumFlag :: Integer
                             } deriving (Show, Eq, Ord)

type OSTNShifts = Array Integer OSTNRecord

type ReadError = String
 
-- | Converts a pair of WGS84 eastings, northings coordinates to a pair.of
--   OSGB36 eastings, northings coordinates.
--
--   This function requires an OSTNShifts array to be passed to it, to carry
--   out the calculation of shifts between WGS84 and OSGB36 coordinates. It
--   then applies the shifts to the WGS84 coordinates, resulting in a set of
--   OSGB36 coordinates.
--
--   Full details of the conversion can be found on page 19 of the document at
--   http://www.leica-geosystems.co.uk/downloads123/gb/gps/gps1200/other/OS%20transformations%20and%20OSGM02%20user%20guide_en.pdf
wgs84ToOSGB36 :: RealFrac a => OSTNShifts -> (a, a) -> (a, a)
wgs84ToOSGB36 os p@(x, y) = (x + se, y + sn)
  where (se, sn) = calculateShifts os p

-- | Converts a pair of OSGB36 eastings, northings coordinates to a pair of
--   WGS84 eastings, northings coordinates.
--
--   
osgb36ToWGS84 :: (RealFrac a, Show a) => OSTNShifts -> (a, a) -> (a, a)
osgb36ToWGS84 os p@(x, y) = (x - finalEs, y - finalNs)
  where initialEstimate = doShift fs
        doShift sft@(es, ns) = (x - es, y - ns)
        fs = nextShift p
        (finalEs, finalNs) = getShift fs initialEstimate
        getShift prevShift currP = if shiftBelowThreshold currShift prevShift
                                      then currShift
                                      else getShift currShift (doShift currShift)
                                        where currShift = nextShift currP
        nextShift = calculateShifts os
        shiftBelowThreshold (es1, ns1) (es2, ns2) = abs (es2 - es1) < 0.0001 && abs (ns2 - ns1) < 0.0001

-- | Calculates the shifts required to convert a pair of WGS84 eastings,
--   northings coordinates to OSGB36 eastings, northings.
calculateShifts :: RealFrac a => OSTNShifts -> (a, a) -> (a, a)
calculateShifts os p@(x, y) = (se, sn)
  where se = (1 - t)*(1 - u)*se₀ + t*(1 - u)*se₁ + t*u*se₂ + (1 - t)*u*se₃
        sn = (1 - t)*(1 - u)*sn₀ + t*(1 - u)*sn₁ + t*u*sn₂ + (1 - t)*u*sn₃
        s₀ = os ! recordNumberFromIndices (eastIndex p, northIndex p)
        s₁ = os ! recordNumberFromIndices (succ $ eastIndex p, northIndex p)
        s₂ = os ! recordNumberFromIndices (succ $ eastIndex p, succ $ northIndex p)
        s₃ = os ! recordNumberFromIndices (eastIndex p, succ $ northIndex p)
        se₀ = realToFrac . eastShift $ s₀
        sn₀ = realToFrac . northShift $ s₀
        se₁ = realToFrac . eastShift $ s₁
        sn₁ = realToFrac . northShift $ s₁
        se₂ = realToFrac . eastShift $ s₂
        sn₂ = realToFrac . northShift $ s₂
        se₃ = realToFrac . eastShift $ s₃
        sn₃ = realToFrac . northShift $ s₃
        dx = x - (realToFrac . easting) s₀
        dy = y - (realToFrac . northing) s₀
        t = dx / 1000
        u = dy / 1000

-- | Reads and parses the OSTN shifts datafile.
--
--   This function expects the path to the OSGB02_GB file, to populate a series
--   of OSTNShifts values.
readOSTNShifts :: FilePath -> IO OSTNShifts
readOSTNShifts path = do
  ls <- B.readFile path 
  return $ parseOSTNData ls

-- | Parses the ByteString contents of the OSTN shifts datafile.
parseOSTNData :: B.ByteString -> OSTNShifts
parseOSTNData = array (1, 876951) . zip [1..] . map createOSTNRecord . processedLines
    where processedLines = map processLine . B.lines
          createOSTNRecord xs = OSTNRecord { easting = floor $ xs !! 1
                                           , northing = floor $ xs !! 2
                                           , eastShift = xs !! 3
                                           , northShift = xs !! 4
                                           , geoidHeight = xs !! 5
                                           , geoidDatumFlag = floor $ xs !! 6
                                           }

-- | Processes one line from a OSTN datafile. 
--
--   A line contains a series of signed decimal numbers, separated by commas.
--   This function splits the line at each comma, and then reads each entry
--   as a decimal.
processLine :: Fractional a => B.ByteString -> [a]
processLine = map (fst . fromMaybe (error "") . B.readSigned B.readDecimal) . B.split ','

-- | Determines the index of the relevant cell within the OSTNShifts array for
--   a given set of OSGB eastings, northings coordinates. Accepts a pair of
--   eastings, northings values.
recordNumber :: RealFrac a => (a, a) -> Integer
recordNumber = recordNumberFromIndices . liftA2 (,) eastIndex northIndex

-- | Determines the index of the relevant cell based on the east and north
--   indices passed thereto.
recordNumberFromIndices :: (Integer, Integer) -> Integer
recordNumberFromIndices (ei, ni) = ei + ni * 701 + 1

-- | Determines the east index of the grid cell in which a particular point
--   lies.
eastIndex :: RealFrac a => (a, a) -> Integer
eastIndex (e, _) = floor (e / 1000)

-- | Determines the north index of the grid cell in which a particular point
--   lies.
northIndex :: RealFrac a => (a, a) -> Integer
northIndex (_, n) = floor (n / 1000)
