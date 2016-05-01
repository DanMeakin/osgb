module Main where

import Control.Monad
import Data.Angle
import qualified Data.ByteString.Char8 as B

import Lib

data MapPoint = MapPoint { mapX :: Integer
                         , mapY :: Integer 
                         , pixelX :: Double
                         , pixelY :: Double
                         , enable :: Integer 
                         } deriving (Show, Read)

main :: IO ()
main = do
  osgn <- osgns
  pf <- B.readFile "/home/daniel/Dropbox/Work/March Stones/PLAN FREEDOM 1929.jpg.points"
  let pts = readPoints pf
      latLonStr p = show (getDegs . snd $ llpt) ++ "," ++ show (getDegs . fst $ llpt) 
        where llpt = latLonPoint osgn p
              getDegs (Degrees x) = x
      printPoint p = putStrLn $ latLonStr p ++ "," ++ show (pixelX p) ++ "," ++ show (pixelY p) ++ "," ++ show (enable p)
  forM_ pts printPoint

readPoints :: B.ByteString -> [MapPoint]
readPoints = map (makePoints . map B.unpack . B.split ',') . drop 1 . B.lines
  where makePoints x = MapPoint { mapX = read (head x)
                                , mapY = read (x !! 1)
                                , pixelX = read (x !! 2)
                                , pixelY = read (x !! 3)
                                , enable = read (x !! 4)
                                }

latLonPoint :: OSTNShifts -> MapPoint -> (Degrees Double, Degrees Double)
latLonPoint o = osgb36EastingsNorthingsToWGS84LatLon o . liftM2 (,) (fromInteger . mapX) (fromInteger . mapY)

osgns :: IO OSTNShifts
osgns = readOSTNShifts "data/OSTN02_OSGM02_GB.txt"
