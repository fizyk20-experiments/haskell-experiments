import Sun
import Control.Monad
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Data.List
import System.Locale
import Options.Applicative

showInterval :: Maybe DiffTime -> String
showInterval Nothing = "Nothing"
showInterval (Just t) = show h ++ "h " ++ show m ++ "min " ++ show s ++ "s"
    where h = floor (t / 3600)
          m = floor ((t - 3600 * fromIntegral h) / 60)
          s = floor (t - 3600 * fromIntegral h - 60 * fromIntegral m)
          
showTime :: (FormatTime t) => Maybe t -> String
showTime Nothing = "N/A"
showTime (Just x) = formatTime defaultTimeLocale "%T" x

showDate :: (FormatTime t) => Maybe t -> String
showDate Nothing = "Nothing"
showDate (Just x) = formatTime defaultTimeLocale "%F" x

newtype MDay = MDay (Maybe Day)
    
instance Read MDay where
    readsPrec _ = map (\(x,y) -> (MDay $ Just x, y)) . reads
    
instance Show MDay where
    show (MDay (Just x)) = show x
    show _ = "Nothing"

data Options = Options {
    latitude :: Double,
    longitude :: Double,
    start :: Day,
    end :: MDay,
    timeZone :: TimeZone
}

latitudeParser :: Parser Double
latitudeParser = option auto
     ( long "latitude"
    <> short 'l'
    <> metavar "LAT"
    <> help "Location latitude (defaults to 52.2)"
    <> value 52.2 )
    
longitudeParser :: Parser Double
longitudeParser = option auto
     ( long "longitude"
    <> short 'g'
    <> metavar "LON"
    <> help "Location longitude (defaults to 20.9)"
    <> value 20.9 )
    
startParser :: Day -> Parser Day
startParser defDay = option auto
     ( long "start"
    <> short 's'
    <> metavar "YYYY-MM-DD"
    <> help "The starting date (defaults to current date)"
    <> value defDay )
    
endParser :: Parser MDay
endParser = option auto
     ( long "end"
    <> short 'e'
    <> metavar "YYYY-MM-DD"
    <> help "The ending date (defaults to the same as starting date)"
    <> value (MDay Nothing) )
    
timeZoneParser :: TimeZone -> Parser TimeZone
timeZoneParser defTZ = option (auto >>= \x -> return (hoursToTimeZone x))
     ( long "zone"
    <> short 'z'
    <> metavar "OFFSET"
    <> help "Time zone offset from UTC in hours (defaults to local time zone)"
    <> value defTZ )

opts :: Day -> TimeZone -> Parser Options   
opts defDay defTZ= Options
    <$> latitudeParser
    <*> longitudeParser
    <*> startParser defDay
    <*> endParser
    <*> timeZoneParser defTZ
    
genDayList :: Day -> MDay -> [Day]
genDayList x (MDay Nothing) = [x]
genDayList x my@(MDay (Just y))
    | x > y = []
    | otherwise = x : genDayList (addDays 1 x) my
    
formatLine :: TimeZone -> Location -> Day -> String
formatLine tz l d = showDate date ++ ": rise " ++ showTime rise
    ++ ", set " ++ showTime set ++ ", length = " ++ showInterval len
    where
        date = Just $ LocalTime d midnight
        rise = utcToLocalTime tz <$> sunrise l d
        set = utcToLocalTime tz <$> sunset l d
        len = dayLength l d

main = do
    localTZ <- getCurrentTimeZone
    curTime <- getCurrentTime
    options <- execParser (info (helper <*> opts (utctDay curTime) localTZ)
         ( fullDesc
        <> progDesc "Calculate the length of the given day"
        <> header "Day length calculator" ))
    let tz = timeZone options
    let list = genDayList (start options) (end options)
    let lat = latitude options
    let lon = longitude options
    putStrLn ("Location: Lat " ++ show lat ++ "; Lon " ++ show lon)
    putStrLn ("Time zone offset: " ++ timeZoneOffsetString tz)
    mapM_ (putStrLn . formatLine tz (Loc lat lon)) list
    