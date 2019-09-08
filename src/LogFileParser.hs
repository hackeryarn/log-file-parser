module LogFileParser
       ( someFunc
       , Log (..)
       , Activity (..)
       , Time (..)
       , timeByActivity
       , parseLog
       , parseDay
       , parseDate
       , parseComment
       , parseActivity
       ) where

import Control.Applicative
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Text.Trifecta
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

type Date = String

type Hour = Int
type Minute = Int
data Time = Time Hour Minute
  deriving (Eq, Show)

type ActivityDescription = String
data Activity = Activity Time ActivityDescription
  deriving (Eq, Show)

newtype Log =
  Log (Map Date [Activity])
  deriving (Eq, Show)


timeByActivity :: Log -> Map ActivityDescription Time
timeByActivity (Log days) = foldr (M.unionWith combineDays) mempty activitiesByDay
  where
    combineDays (Time h m) (Time h' m') = Time (h + h') (m + m')
    activitiesByDay = M.foldr go [] days
    go activities acc = M.fromList (zipWith timeSpent activities (tail activities)) : acc
    timeSpent (Activity (Time h m) description) (Activity (Time h' m') _) =
      (description, Time (h' - h) (m' - m))

parseLog :: Parser Log
parseLog = Log <$> days
  where days = M.fromList <$> manyTill (skipMany eol *> parseDay) eof

parseDay :: Parser (Date, [Activity])
parseDay = (,) <$> parseDate <*> some parseActivity

parseDate :: Parser Date
parseDate = char '#' *> spaces *> go <* (try eol <|> (whiteSpace *> eol))
  where go = do
          year <- count 4 digit
          _ <- char '-'
          month <- count 2 digit
          _ <- char '-'
          day <- count 2 digit
          return $ year ++ "-" ++ month ++ "-" ++ day

parseComment :: Parser ()
parseComment =
  () <$ (try (char '-') *> char '-' *> manyTill anyChar (try eol))

parseActivity :: Parser Activity
parseActivity = Activity <$> parseTime <*> parseActivityDescription

parseActivityDescription :: Parser ActivityDescription
parseActivityDescription =
  dropWhileEnd isSpace <$> manyTill anyChar (try eol)

parseTime :: Parser Time
parseTime = Time <$> (read <$> parseHour) <*> (read <$> parseMinute)
  where parseHour = count 2 digit <* char ':'
        parseMinute = count 2 digit <* spaces

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

eol :: Parser ()
eol = eof <|> parseComment <|> (() <$ newline)
