{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Control.AutoUpdate
-- import Control.Monad.Writer
-- -- import Data.List hiding (repeat)
-- import Data.Time (getCurrentTime)
-- import Data.Time.Format (formatTime, iso8601DateFormat, defaultTimeLocale)
-- import System.IO hiding (putStrLn)

-- type FormattedTime = String

-- data Color = GREEN | BLUE | YELLOW | RED | PURPLE | RESET
-- instance Show Color where
--     show GREEN  = "\x1b[32m"
--     show BLUE   = "\x1b[34m"
--     show YELLOW = "\x1b[33m"
--     show RED    = "\x1b[31m"
--     show PURPLE = "\x1b[35m"
--     show RESET  = "\x1b[0m"

-- data MessageLevel = OK | INFO | WARN | FAIL | CRIT
-- instance Show MessageLevel where
--     show OK     = "  OK  "
--     show INFO   = " INFO "
--     show WARN   = " WARN "
--     show FAIL   = " FAIL "
--     show CRIT   = " CRIT "

-- splotch :: MessageLevel -> String
-- splotch OK   = (show GREEN)  ++ "  o  " ++ (show RESET)
-- splotch INFO = (show BLUE)   ++ "  o  " ++ (show RESET)
-- splotch WARN = (show YELLOW) ++ "  o  " ++ (show RESET)
-- splotch FAIL = (show RED)    ++ "  o  " ++ (show RESET)
-- splotch CRIT = (show PURPLE) ++ "  o  " ++ (show RESET)

-- buildMessage :: MessageLevel -> String -> FormattedTime -> String
-- buildMessage lvl msg formattedTime = do
--     formattedTime ++ " || [" ++ (show lvl) ++ "]" ++ (splotch lvl) ++ msg

-- slog :: (IO String) -> MessageLevel -> String -> Handle -> IO ()
-- slog timeCache lvl msg f = do
--   t <- timeCache
--   let msg' = buildMessage lvl msg t
--   (hPutStrLn f msg')

-- bench1 :: IO ()
-- bench1 = do
--     f <- openFile "log.txt" AppendMode
--     timeCache <- mkAutoUpdate $ defaultUpdateSettings {
--       updateAction = getCurrentTime >>= pure . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))
--       }
--     replicateM_ 10000000 $ slog timeCache OK "Hello, world!" f
--     hClose f

-- main :: IO ()
-- main = bench1


import Control.Monad
import Data.Monoid
import System.Log.FastLogger.Date
import System.Log.FastLogger

main = toBench 10000000 OK "Hello, World!"

data MessageLevel = OK | INFO | WARN | FAIL | CRIT

instance Show MessageLevel where
    show OK     = "  OK  "
    show INFO   = " INFO "
    show WARN   = " WARN "
    show FAIL   = " FAIL "
    show CRIT   = " CRIT "

toBench :: Int -> MessageLevel -> String -> IO ()
toBench n lvl msg = do
    logFunction <- fst <$> fastLogger
    let toString msg t = toLogStr t <> toLogStr (mconcat msg)
    replicateM_ n $ (logFunction . toString) ([" || [", (show lvl), "] ", msg, "\n"])

fastLogger :: IO (TimedFastLogger, IO ())
fastLogger = do
    timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
    let logType = LogFileNoRotate "test.log" 4096
    newTimedFastLogger timeCache logType
