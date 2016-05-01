module Conversion.EllipsoidGrid (
    ellipsoidToGrid
  , gridToEllipsoid
  ) where

import Data.Angle

import Conversion.Constants

-- | Converts a set of ellipsoid coordinates (latitude, longitude), to
--   a set of grid coordinates (eastings, northings) on the same datum.
--
--   This function follows the procedure laid out at p. 40 of the Ordnance
--   Survey document "A guide to coordinate systems in Great Britain", found
--   at https://www.ordnancesurvey.co.uk/docs/support/guide-coordinate-systems-great-britain.pdf
ellipsoidToGrid :: Floating a 
                => Constants a -- ^ Constants for the relevant datum
                -> (Degrees a, Degrees a) -- ^ Pair of lat/lon coordinates
                -> (a, a) -- ^ Pair of eastings/northings coordinates
ellipsoidToGrid cs (φ⁰, λ⁰) = (es, ns)
  where φ = getRadians φ⁰
        λ = getRadians λ⁰
        -- Extract Constants
        a = semiMajorAxisA cs
        b = semiMinorAxisB cs
        φ₀ = getRadians . trueOriginLatitude $ cs
        λ₀ = getRadians . trueOriginLongitude $ cs
        n₀ = trueOriginNorthings cs
        e₀ = trueOriginEastings cs
        -- Calculations
        δφ = φ - φ₀
        σφ = φ + φ₀
        δλ = λ - λ₀
        n = c1 cs
        ν = nu cs φ
        ρ = rho cs φ
        η² = c2 cs φ
        m = c3 cs φ
        ns = fI + fII * δλ**2 + fIII * δλ**4 + fIIIA * δλ**6
          where fI = m + n₀
                fII = ν/2 * sin φ * cos φ
                fIII = ν/24 * sin φ * cos φ ** 3 * (5 - tan φ ** 2 + 9 * η²)
                fIIIA = ν/720 * sin φ * cos φ ** 5 * (61 - 58 * tan φ ** 2 + tan φ ** 4)
        es = e₀ + fIV*δλ + fV*δλ**3 + fVI*δλ**5
          where fIV = ν * cos φ
                fV = ν/6 * cos φ ** 3 * (ν/ρ - tan φ ** 2)
                fVI = ν/120 * cos φ ** 5 * (5 - 18 * tan φ ** 2 + tan φ ** 4 + 14 * η² - 58 * η² * tan φ ** 2)

-- | Converts a set of grid coordinations (eastings, northings) into a set of
--   ellipsoid coordinations (latitude, longitude) on the same datum.
--
--   This function follows the procedure laid out at p. 41 of the Ordnance
--   Survey document "A guide to coordinate systems in Great Britain", found
--   at https://www.ordnancesurvey.co.uk/docs/support/guide-coordinate-systems-great-britain.pdf
gridToEllipsoid :: (Floating a, Ord a) 
                => Constants a -- ^ Constants for the relevant datum
                -> (a, a) -- ^ Pair of eastings/northings coordinates
                -> (Degrees a, Degrees a) -- ^ Pair of lat/lon coordinates
gridToEllipsoid cs (es, ns) = (φ⁰, λ⁰)
  where φ⁰ = degrees φ
        λ⁰ = degrees λ
        -- Extract Constants
        a = semiMajorAxisA cs
        b = semiMinorAxisB cs
        e² = eccentricitySquared cs
        φ₀ = getRadians . trueOriginLatitude $ cs
        λ₀ = getRadians . trueOriginLongitude $ cs
        e₀ = trueOriginEastings cs
        n₀ = trueOriginNorthings cs
        f₀ = scaleFactor cs
        -- Calculations
        δE = es - e₀
        φ' = phi' ns
        ρ = rho cs φ'
        ν = nu cs φ'
        η² = c2 cs φ'
        φ = Radians $ φ' - fVII * δE**2 + fVIII * δE**4 - fIX * δE**6
          where fVII = tan φ' / (2 * ρ * ν)
                fVIII = tan φ' / (24 * ρ * ν**3) * (5 + 3 * tan φ' ** 2 + η² - 9 * tan φ' ** 2 * η²)
                fIX = tan φ' / (720 * ρ * ν**5) * (61 + 90 * tan φ' ** 2 + 45 * tan φ' ** 4)
        λ = Radians $ λ₀ + fX * δE - fXI * δE**3 + fXII * δE**5 - fXIIA * δE**7
          where fX = sec φ' / ν
                fXI = sec φ' / (6 * ν**3) * (ν/ρ + 2 * tan φ' ** 2)
                fXII = sec φ' / (120 * ν**5) * (5 + 28 * tan φ' ** 2 + 24 * tan φ' ** 4)
                fXIIA = sec φ' / (5040 * ν**7) * (61 + 662 * tan φ' ** 2 + 1320 * tan φ' ** 4 + 720 * tan φ' ** 6)
        phi' ns = if (ns - n₀ - m φ₁) < 0.00001 
                     then φ₁
                     else φ'new φ₁
          where m = c3 cs
                φ₁ = (ns - n₀)/(a * f₀) + φ₀
                φ'new φ = if (ns - n₀ - m φ') < 0.00001
                               then φ'
                               else φ'new φ'
                            where φ' = (ns - n₀ - m φ)/(a * f₀) + φ

-- | Calculates the value of C1 as used in conversions (see p. 40).
c1 :: Floating a => Constants a -> a
c1 cs = (a - b) / (a + b)
  where a = semiMajorAxisA cs
        b = semiMinorAxisB cs

-- | Calculates the value of C2 as used in conversions (see p. 40).
--
--   This function expects a value of radians as input.
c2 :: Floating a => Constants a -> a -> a
c2 cs φ = nu cs φ / rho cs φ - 1

-- | Calculates the value of ν as used in conversions (see p. 40).
--
--   This function expects a value of radians as input.
nu :: Floating a => Constants a -> a -> a
nu cs = fst . calcNuAndRho cs

-- | Calculates the value of ρ as used in conversions (see p. 40).
--
--   This function expects a value of radians as input.
rho :: Floating a => Constants a -> a -> a
rho cs = snd . calcNuAndRho cs

-- | Calculates the values of ν and ρ as used in conversions. Returns the
--   pair of values in a tuple containing (ν, ρ) in that order.
calcNuAndRho :: Floating a => Constants a -> a -> (a, a)
calcNuAndRho cs φ = (ν, ρ)
  where ν = a * f₀ * (1 - e² * sin φ ** 2) ** (-0.5)
        ρ = a * f₀ * (1 - e²) * (1 - e² * sin φ ** 2) ** (-1.5)
        a = semiMajorAxisA cs
        f₀ = scaleFactor cs
        e² = eccentricitySquared cs


-- | Calculates the value of C3 as used in conversions (see p. 40).
--
--   This function expects a value of radians as input.
c3 :: Floating a => Constants a -> a -> a
c3 cs φ = b * f₀ * (f1 - f2 + f3 - f4) 
  where b = semiMinorAxisB cs
        f₀ = scaleFactor cs
        φ₀ = getRadians . trueOriginLatitude $ cs
        δφ = φ - φ₀
        σφ = φ + φ₀
        n = c1 cs
        f1 = (1 + n + 5/4 * n**2 + 5/4 * n**3) * δφ
        f2 = (3 * n + 3 * n**2 + 21/8 * n**3) * sin δφ * cos σφ
        f3 = (15/8 * n**2 + 15/8 * n**3) * sin (2*δφ) * cos (2*σφ)
        f4 = 35/24 * n**3 * sin (3*δφ) * cos (3*σφ)

getRadians :: Floating a => Degrees a -> a
getRadians = unradians . radians
  where unradians (Radians x) = x

sec :: Floating a => a -> a
sec x = 1 / cos x
