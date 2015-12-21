import Sun
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import System.Locale

showTime :: Maybe DiffTime -> String
showTime Nothing = "Nothing"
showTime (Just t) = show h ++ "h " ++ show m ++ "min " ++ show s ++ "s"
    where h = floor (t / 3600)
          m = floor ((t - 3600 * fromIntegral h) / 60)
          s = floor (t - 3600 * fromIntegral h - 60 * fromIntegral m)

showDateTime :: UTCTime -> String
showDateTime = formatTime defaultTimeLocale "%F %T"

here = Loc 52.2 20.9

main = do
    putStrLn $ show $ zip [1..] $ map (showTime . (sunset here) . (fromGregorian 2015 12)) [1..31] 