module Day4 where

import Text.Trifecta
import Data.Dates
import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (sortBy)
import Data.Ord (comparing)

data EventType = BeginShift Integer | FallAsleep | WakeUp deriving (Show)
data Event =
  Event {
      timestamp :: DateTime
    , eventType :: EventType
    } deriving (Show)

integral :: Parser Int
integral = fromIntegral <$> integer

date :: Parser DateTime
date = do
  year <- integral
  _ <- char '-'
  month <- integral
  _ <- char '-'
  day <- integral
  hour <- integral
  _ <- char ':'
  mins <- integral
  return $ DateTime { year=year, month=month, day=day, hour=hour, minute=mins, second=0 }

beginShift :: Parser EventType
beginShift = do
  _ <- string "Guard #"
  _id <- integer
  _ <- string "begins shift"
  return $ BeginShift _id

fallAsleep :: Parser EventType
fallAsleep = string "falls asleep" *> return FallAsleep

wakeUp :: Parser EventType
wakeUp = string "wakes up" *> return WakeUp

parseEvent :: Parser Event
parseEvent = do
  _ <- char '['
  d <- date
  _ <- char ']'
  _ <- char ' '
  e <- beginShift <|> fallAsleep <|> wakeUp
  return $ Event {timestamp=d, eventType=e}

sortedEvents :: [String] -> [Event]
sortedEvents xs =
  case sequenceA $ map (parseString parseEvent mempty) xs of
    Success a -> sortBy (comparing timestamp) a
    Failure _ -> error $ "could not parse input!"

exampleInput :: [String]
exampleInput =
  [
      "[1518-11-01 00:00] Guard #10 begins shift"
    , "[1518-11-01 00:05] falls asleep"
    , "[1518-11-01 00:25] wakes up"
    , "[1518-11-01 00:30] falls asleep"
    , "[1518-11-01 00:55] wakes up"
    , "[1518-11-01 23:58] Guard #99 begins shift"
    , "[1518-11-02 00:40] falls asleep"
    , "[1518-11-02 00:50] wakes up"
    , "[1518-11-03 00:05] Guard #10 begins shift"
    , "[1518-11-03 00:24] falls asleep"
    , "[1518-11-03 00:29] wakes up"
    , "[1518-11-04 00:02] Guard #99 begins shift"
    , "[1518-11-04 00:36] falls asleep"
    , "[1518-11-04 00:46] wakes up"
    , "[1518-11-05 00:03] Guard #99 begins shift"
    , "[1518-11-05 00:45] falls asleep"
    , "[1518-11-05 00:55] wakes up"
  ]
