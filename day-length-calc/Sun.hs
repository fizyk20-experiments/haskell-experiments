module Sun where

import Data.Time.Calendar
import Data.Time.Clock
    
-- J2000 epoch
j2000 :: UTCTime -> Double
j2000 dt = fromIntegral (toModifiedJulianDay $ utctDay dt) + realToFrac (utctDayTime dt) / 86400 - 51544.5

-- helper
deg2rad x = (x/180) * pi
rad2deg x = (x/pi) * 180

-- now astronomical parameters

-- mean longitude in degrees
meanLongitude :: Double -> Double
meanLongitude n = 280.46 + 0.9856474*n

-- mean anomaly in degrees
meanAnomaly :: Double -> Double
meanAnomaly n = 357.528 + 0.9856003*n

-- ecliptic longitude of the Sun
eclipticLong :: Double -> Double
eclipticLong n =
    let g = deg2rad $ meanAnomaly n
    in meanLongitude n + 1.915 * sin g + 0.02 * sin (2*g)
    
-- Sun-Earth distance
sunR :: Double -> Double
sunR n = 
    let g = deg2rad $ meanAnomaly n
    in (1.00014 - 0.01671 * cos g - 0.00014 * cos (2*g)) * 149597870700

-- Earth obliquity
obliquity n = 23.439 - 0.0000004 * n

-- Sun right ascension
sunRA :: Double -> Double
sunRA n
    | ra < 0 = ra * 12 / pi + 24
    | otherwise = ra * 12 / pi
    where
        eps = deg2rad $ obliquity n
        l = deg2rad $ eclipticLong n
        x = cos eps * sin l
        y = cos l
        ra = atan2 x y
    
-- Sun declination
sunDec :: Double -> Double
sunDec n =
    let eps = deg2rad $ obliquity n
        l = deg2rad $ eclipticLong n
    in rad2deg $ asin (sin eps * sin l)
    
-- location-specific values
data Location = Loc { lat :: Double, lon :: Double }

-- Greenwich Mean Sidereal Time
gmst :: Double -> Double
gmst n = 18.697374558 + 24.06570982441908 * n

-- local sidereal time
lst :: Location -> Double -> Double
lst l n = gmst n + lon l / 15

-- Sun local hour angle
sunLHA :: Location -> Double -> Double
sunLHA l n = (lst l n - sunRA n) * 15

-- Solar zenith angle
sunZenith :: Location -> Double -> Double
sunZenith l n =
    let d = deg2rad $ sunDec n
        h = deg2rad $ sunLHA l n
        lat' = deg2rad $ lat l
    in rad2deg $ acos (sin lat' * sin d + cos lat' * cos d * cos h)
    
sunElev :: Location -> Double -> Double
sunElev l n = 90 - sunZenith l n

-- Solar azimuth angle
sunAzim :: Location -> Double -> Double
sunAzim l n
    | az < 0 = az + 360
    | otherwise = az
    where
        lat' = deg2rad $ lat l
        dec = deg2rad $ sunDec n
        h = deg2rad $ sunLHA l n
        sinAz = - sin h * cos dec
        cosAz = (sin dec * cos lat') - (cos h * cos dec * sin lat')
        az = rad2deg $ atan2 sinAz cosAz
        
-- Sunrise, sunset
diffThreshold :: (Fractional a) => a
diffThreshold = 0.2

sunBinSearch :: Location -> UTCTime -> UTCTime -> Maybe DiffTime
sunBinSearch l time1 time2
    | elev1 * elev2 > 0 = Nothing
    | diffUTCTime time2 time1 < diffThreshold = Just (utctDayTime time1)
    | elev1 * elevMid <= 0 = sunBinSearch l time1 timeMid
    | elevMid * elev2 <= 0 = sunBinSearch l timeMid time2
    | otherwise = Nothing
    where elev1 = sunElev l (j2000 time1)
          elev2 = sunElev l (j2000 time2)
          timeMid = addUTCTime (diffUTCTime time2 time1 / 2) time1
          elevMid = sunElev l (j2000 timeMid)

sunrise :: Location -> Day -> Maybe DiffTime
sunrise l d = sunBinSearch l (UTCTime d (secondsToDiffTime 0)) (UTCTime d (secondsToDiffTime 43200))

sunset :: Location -> Day -> Maybe DiffTime
sunset l d = sunBinSearch l (UTCTime d (secondsToDiffTime 43200)) (UTCTime d (secondsToDiffTime 86400))

dayLength :: Location -> Day -> Maybe DiffTime
dayLength l d = maybeDiff time1 time2
    where
        maybeDiff Nothing _ = Nothing
        maybeDiff _ Nothing = Nothing
        maybeDiff (Just t1) (Just t2) = Just (t1 - t2)
        time1 = sunset l d
        time2 = sunrise l d