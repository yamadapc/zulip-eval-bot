module Logging where

import Control.Exception (SomeException)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Text.Printf (printf)
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleIntensity(..),
                            ConsoleLayer(..), SGR(..), setSGR)
import System.Locale (defaultTimeLocale)

logColor :: Color -> String -> IO ()
logColor c s = do
    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid c
           , SetColor Background Dull c
           ]
    date <- getCurrentTime
    printf "[%s]" (formatTime defaultTimeLocale "%H" date)

    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid White
           ]
    putStrLn s

logInfo :: String -> IO ()
logInfo = logColor Blue

logException :: SomeException -> IO ()
logException = logColor Red . show
