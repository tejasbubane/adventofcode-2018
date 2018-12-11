module Day7 where

import Text.Trifecta
import Data.List ((\\), sort, nub, delete)
import Data.Traversable (sequenceA)
import Data.Map hiding (map, (\\), union, singleton, toList, delete)
import Data.Set hiding (map, (\\), delete)

type Prerequisite = (Char, Char)

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
next :: Map Char (Set Char) -> [Char] -> [Char] -> (Char, [Char])
next _ [] _ = error "no available chars given"
next instrns available acc =
  let (first:rest) = sort available
      nextAvailable = keys $ Data.Map.filter (elem first) instrns
  in case Data.Map.lookup first instrns of
    Nothing -> (first, nextAvailable) -- root element does not have any befores
    Just a  -> if toList a \\ acc == []
               then (first, nextAvailable)
               else next instrns rest acc

traverseGraph :: Map Char (Set Char) -> [Char] -> [Char] -> [Char]
traverseGraph _ [] acc = reverse acc
traverseGraph instrns available acc =
  let (nxt, nextAvailable) = next instrns available acc
  in traverseGraph instrns (nub $ delete nxt available ++ nextAvailable) (nxt:acc)

order :: [String] -> [Char]
order xs =
  let is = prerequisites xs
  in traverseGraph (prerequisiteMap is) (roots is) []
