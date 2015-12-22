import Sun
import Control.Monad
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.List
import System.Locale
import Options.Applicative

showInterval :: Maybe DiffTime -> String
showInterval Nothing = "Nothing"
showInterval (Just t) = show h ++ "h " ++ show m ++ "min " ++ show s ++ "s"
    where h = floor (t / 3600)
          m = floor ((t - 3600 * fromIntegral h) / 60)
          s = floor (t - 3600 * fromIntegral h - 60 * fromIntegral m)
          
showTime :: Maybe UTCTime -> String
showTime Nothing = "N/A"
showTime (Just x) = formatTime defaultTimeLocale "%T" x

showDate :: Maybe UTCTime -> String
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
    end :: MDay
}

latitudeParser :: Parser Double
latitudeParser = option auto
     ( long "latitude"
    <> short 'l'
    <> metavar "LAT"
    <> help "Location latitude"
    <> value 52.2 )
    
longitudeParser :: Parser Double
longitudeParser = option auto
     ( long "longitude"
    <> short 'g'
    <> metavar "LON"
    <> help "Location longitude"
    <> value 20.9 )
    
startParser :: Parser Day
startParser = option auto
     ( long "start"
    <> short 's'
    <> metavar "YYYY-MM-DD"
    <> help "The starting date"
    <> value (fromGregorian 2015 3 21) )
    
endParser :: Parser MDay
endParser = option auto
     ( long "end"
    <> short 'e'
    <> metavar "YYYY-MM-DD"
    <> help "The ending date"
    <> value (MDay Nothing) )

opts :: Parser Options   
opts = Options
    <$> latitudeParser
    <*> longitudeParser
    <*> startParser
    <*> endParser
    
genDayList :: Day -> MDay -> [Day]
genDayList x (MDay Nothing) = [x]
genDayList x my@(MDay (Just y))
    | x > y = []
    | otherwise = x : genDayList (addDays 1 x) my
    
formatLine :: Location -> Day -> String
formatLine l d = showDate (Just $ UTCTime d 0) ++ ": rise " ++ showTime rise ++ ", set " ++ showTime set ++ ", length = " ++ showInterval len
    where
        rise = sunrise l d
        set = sunset l d
        len = dayLength l d

main = do
    options <- execParser (info (helper <*> opts)
         ( fullDesc
        <> progDesc "Calculate the length of the given day"
        <> header "Day length calculator" ))
    let list = genDayList (start options) (end options)
    let lat = latitude options
    let lon = longitude options
    putStrLn ("Location: Lat " ++ show lat ++ "; Lon " ++ show lon)
    mapM_ (putStrLn . formatLine (Loc lat lon)) list
    