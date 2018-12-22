module Day7 where

import Text.Trifecta
import Data.List ((\\), sort, nub, delete, findIndex)
import Data.Traversable (sequenceA)
import Data.Map hiding (map, (\\), union, singleton, toList, delete)
import Data.Set hiding (map, (\\), delete)
import Data.Char (ord)

type Prerequisite = (Char, Char) -- (after, before)

-- parse single prerequisite requirement
parsePrerequisite :: Parser Prerequisite
parsePrerequisite = do
  _ <- string "Step "
  before <- letter
  _ <- string " must be finished before step "
  after <- letter
  _ <- string " can begin."
  eof
  return $ (after, before)

-- parse multiple of prerequisites
prerequisites :: [String] -> [Prerequisite]
prerequisites xs =
  case sequenceA $ map (parseString parsePrerequisite mempty) xs of
    Success a -> a
    Failure _ -> error $ "could not parse input!"

afters :: [Prerequisite] -> [Char]
afters = nub . map fst

befores :: [Prerequisite] -> [Char]
befores = nub . map snd

roots :: [Prerequisite] -> [Char]
roots ps = nub $ befores ps \\ afters ps

-- Map of key = task & value = set of all prerequisite tasks
prerequisiteMap :: [Prerequisite] -> Map Char (Set Char)
prerequisiteMap ps =
  let ps' = (fmap . fmap) singleton ps -- (a, b) -> (a, Set b) - so that union can be applied
  in fromListWith union ps'

-- Returns next task (char) to be selected and unlocked tasks after selecting it
-- based on sorting alpabetically and also checking if all requirements are met
next :: Map Char (Set Char) -> [Char] -> [Char] -> Maybe (Char, [Char])
next _ [] _ = Nothing
next instrns available done =
  let (first:rest) = sort available
      nextAvailable = keys $ Data.Map.filter (elem first) instrns
  in case Data.Map.lookup first instrns of
    Nothing -> Just (first, nextAvailable) -- root element does not have any befores
    Just a  -> if toList a \\ done == []
               then Just (first, nextAvailable)
               else next instrns rest done

traverseGraph :: Map Char (Set Char) -> [Char] -> [Char] -> [Char]
traverseGraph _ [] done = reverse done
traverseGraph instrns available done =
  case next instrns available done of
    Nothing -> error "nothing left to take next" -- This case should not appear part 1
    Just (nxt, nextAvailable) ->
      traverseGraph instrns (nub $ delete nxt available ++ nextAvailable) (nxt:done)

order :: [String] -> [Char]
order xs =
  let is = prerequisites xs
  in traverseGraph (prerequisiteMap is) (roots is) []


-- Part 2

type Workers = [(Char, Int)] -- [(job, timeleft)]

decrement :: Workers -> Workers
decrement = (fmap . fmap) (\x -> if x > 0 then x - 1 else x)

timeReqd :: Char -> Int
timeReqd c = ord c - 64

assign :: Workers -> Int -> Char -> Workers
assign workers idx c = Prelude.take idx workers ++ [(c, timeReqd c)] ++ Prelude.drop (idx + 1) workers

getCompleted :: Workers -> [Char]
getCompleted ws = Prelude.map fst $ Prelude.filter (\w -> snd w == 1) ws

assignAll :: Workers -> [Char] -> Workers
assignAll ws [] = ws
assignAll ws available =
  case Data.List.findIndex (\w -> snd w == 0) ws of
    Nothing -> ws -- all workers busy
    Just i  ->
      let nxt = head $ sort available
          newWorkers = assign ws i nxt
      in assignAll newWorkers (delete nxt available)

jobCount :: Map Char (Set Char) -> Int
jobCount instrns =
  length $ (Data.Map.foldr union Data.Set.empty instrns) `union` Data.Set.fromList (keys instrns)

performConstruction :: Map Char (Set Char) -> [Char] -> [Char] -> Workers -> Int -> Int
performConstruction instrns available done workers counter
  -- All jobs done - halt
  | jobCount instrns == length done = counter
  -- jobs still pending completion - recurse
  | otherwise =
    let newDone       = getCompleted workers
        newWorkers    = assignAll (decrement workers) available
        nextAvailable = \j -> keys $ Data.Map.filter (elem j) instrns
        newAvailable  = nub $ concat . map nextAvailable $ newDone
    in performConstruction instrns newAvailable newDone newWorkers (counter + 1)

constructionTime :: [String] -> Int -> Int
constructionTime ps workerCount =
  let is = prerequisites ps
      initWorkers = Prelude.map ((,) '.') $ Prelude.take workerCount $ repeat 0
  in performConstruction (prerequisiteMap is) (roots is) [] initWorkers 0
