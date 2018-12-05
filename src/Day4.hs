module Day4 (solution1, solution2) where

import Text.Trifecta
import Data.Dates
import Control.Applicative hiding (empty)
import Data.Traversable (sequenceA)
import Data.List (sortBy, maximumBy)
import Data.Ord (comparing)
import Data.Map
import Data.Maybe (fromMaybe)

-- Aliases
type Minute      = Int
type Guard       = Integer
type SleepRanges = Map Guard [(Minute, Minute)] -- timerange in which guard sleeps

-- Domain Types
data EventType = BeginShift Guard | FallAsleep | WakeUp deriving (Show)
data Event =
  Event {
      timestamp :: DateTime
    , eventType :: EventType
    } deriving (Show)

integral :: Parser Int
integral = fromIntegral <$> integer

date :: Parser DateTime
date = do
  y <- integral
  _ <- char '-'
  m <- integral
  _ <- char '-'
  d <- integral
  h <- integral
  _ <- char ':'
  mins <- integral
  return $ DateTime { year=y, month=m, day=d, hour=h, minute=mins, second=0 }

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
  case sequenceA $ Prelude.map (parseString parseEvent mempty) xs of
    Success a -> sortBy (comparing timestamp) a
    Failure _ -> error $ "could not parse input!"

sleepRanges :: Guard -> SleepRanges -> [Event] -> SleepRanges
sleepRanges _ acc [] = acc
sleepRanges _ acc ((Event {eventType=(BeginShift gid)}):es) = sleepRanges gid acc es
sleepRanges gid acc (Event {eventType=FallAsleep, timestamp=t1}:Event {eventType=WakeUp, timestamp=t2}:es) =
  sleepRanges gid (insertWith (++) gid [(minute t1, minute t2)] acc) es
sleepRanges _ _ _ = error "FallAsleep should be followed by WakeUp"

ranges :: [String] -> SleepRanges
ranges xs =
  let events = sortedEvents xs
  in sleepRanges 0 empty events

-- Find guard which sleeps for the maximum time
maxSleeper :: SleepRanges -> Guard
maxSleeper =
  fst . maximumBy (comparing snd) . toList . Data.Map.map sumRanges where
    sumRanges = Prelude.foldr (\(x1, x2) acc -> acc + (x2 - x1)) 0

-- number of times given guard slept each minute
sleepingMinutes :: [(Minute, Minute)] -> Map Minute Int
sleepingMinutes rs =
  let es = concat [[x1..(x2 - 1)] | (x1, x2) <- rs]
      calcRec = Prelude.foldr (\x acc -> insertWith (+) x 1 acc)
  in  calcRec empty es

maxOccurrence :: [(Minute, Int)] -> Minute
maxOccurrence = fst . maximumBy (comparing snd)

solution1 :: [String] -> Integer
solution1 xs =
  let rs = ranges xs
      i = maxSleeper rs
      mins = fromMaybe [] $ Data.Map.lookup i rs
  in i * (fromIntegral . maxOccurrence . toList $ sleepingMinutes mins)

maxSleptMinute :: SleepRanges -> (Guard, Minute)
maxSleptMinute ss =
  maximumBy (comparing snd) . toList $ Data.Map.map (\rs -> maxOccurrence (toList $ sleepingMinutes rs)) ss

solution2 :: [String] -> Integer
solution2 xs =
  let rs = ranges xs
      (guard, m) = maxSleptMinute rs
  in (fromIntegral guard) * (fromIntegral m)
