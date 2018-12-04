module Day4 (solution1) where

import Text.Trifecta
import Data.Dates
import Control.Applicative hiding (empty)
import Data.Traversable (sequenceA)
import Data.List (sortBy, maximumBy)
import Data.Ord (comparing)
import Data.Map
import Data.Maybe (fromMaybe)

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
  case sequenceA $ Prelude.map (parseString parseEvent mempty) xs of
    Success a -> sortBy (comparing timestamp) a
    Failure _ -> error $ "could not parse input!"

type SleepRanges = Map Integer [(Int, Int)]

maxFromMap :: Map Int Int -> Int
maxFromMap = undefined

sleepRanges :: Integer -> SleepRanges -> [Event] -> SleepRanges
sleepRanges currentId acc [] = acc
sleepRanges _ acc ((Event {eventType=(BeginShift i)}):es) = sleepRanges i acc es
sleepRanges i acc (Event {eventType=FallAsleep, timestamp=t1}:Event {eventType=WakeUp, timestamp=t2}:es) =
  sleepRanges i (insertWith (++) i [(minute t1, minute t2)] acc) es

maxSleeper :: SleepRanges -> Integer
maxSleeper =
  fst . maximumBy (comparing snd) . toList . Data.Map.map sumRanges where
    sumRanges = Prelude.foldr (\(x1, x2) acc -> acc + (x2 - x1)) 0

mostSleepingMinute :: SleepRanges -> Integer -> Integer
mostSleepingMinute ss i =
  let rs = fromMaybe [] $ Data.Map.lookup i ss
      es = concat [[x1..(x2 - 1)] | (x1, x2) <- rs]
      calcRec = Prelude.foldr (\x acc -> insertWith (+) x 1 acc)
  in fromIntegral . fst . maximumBy (comparing snd) . toList $ calcRec empty es

solution1 :: [String] -> Integer
solution1 xs =
  let events = sortedEvents xs
      ranges = sleepRanges 0 empty events
      i      = maxSleeper ranges
  in i * (mostSleepingMinute ranges i)
