import Sun
import Control.Monad
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.List
import System.Locale
import Options.Applicative

showTime :: Maybe DiffTime -> String
showTime Nothing = "Nothing"
showTime (Just t) = show h ++ "h " ++ show m ++ "min " ++ show s ++ "s"
    where h = floor (t / 3600)
          m = floor ((t - 3600 * fromIntegral h) / 60)
          s = floor (t - 3600 * fromIntegral h - 60 * fromIntegral m)

showDate :: UTCTime -> String
showDate = formatTime defaultTimeLocale "%F"

data Options = Options {
    latitude :: Double,
    longitude :: Double,
    year :: Integer,
    month :: Int,
    day :: Int
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
    
yearParser :: Parser Integer
yearParser = option auto
     ( long "year"
    <> short 'y'
    <> metavar "YEAR"
    <> help "The year part of the date"
    <> value 2015 )

monthParser :: Parser Int
monthParser = option auto
     ( long "month"
    <> short 'm'
    <> metavar "MONTH"
    <> help "The month part of the date"
    <> value 3 )
    
dayParser :: Parser Int
dayParser = option auto
     ( long "day"
    <> short 'd'
    <> metavar "DAY"
    <> help "The day part of the date"
    <> value 21 )

opts :: Parser Options   
opts = Options
    <$> latitudeParser
    <*> longitudeParser
    <*> yearParser
    <*> monthParser
    <*> dayParser

main = do
    options <- execParser (info (helper <*> opts)
         ( fullDesc
        <> progDesc "Calculate the length of the given day"
        <> header "Day length calculator" ))
    let here = Loc (latitude options) (longitude options)
    let date = fromGregorian (year options) (month options) (day options)
    putStrLn (showDate $ UTCTime date 0)
    putStrLn ("Sunrise: " ++ showTime (sunrise here date))
    putStrLn ("Sunset: " ++ showTime (sunset here date))
    putStrLn ("Day length: " ++ showTime (dayLength here date))