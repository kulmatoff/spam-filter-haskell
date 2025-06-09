module Main (main) where

import Data.Map.Strict (Map, fromListWith, size)
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (fromMaybe)

type WordCounts = Map String Int
type ClassModel = (WordCounts, Int)

-- base functions
tokenize :: String -> [String] -- Converts whole text into the list of separate low-case words (tokens)
tokenize text = words (map (\c -> if isAlphaNum c then toLower c else ' ') text)

train :: [String] -> ClassModel
train messages = 
  let tokens = concatMap tokenize messages
      freqs = fromListWith (+) [(w, 1) | w <- tokens]

  in (freqs, length tokens)

logProb :: ClassModel -> String -> Double
logProb (wc, total) word = 
  let count = Map.findWithDefault 0 word wc
  in log ((fromIntegral (count + 1)) / fromIntegral (total + size wc))

classify :: ClassModel -> ClassModel -> String -> [(String, Double)]
classify spamModel hamModel msg =
  let words = tokenize msg
      spamScore = sum [logProb spamModel w | w <- words]
      hamScore = sum [logProb hamModel w | w <- words]
  in [("spam", spamScore), ("ham", hamScore)]

sortByValue :: (Ord a) => [(String, a)] -> [(String, a)]
sortByValue [] = []
sortByValue [x] = [x]
sortByValue (mid:xs) = sortByValue lesser ++ [mid] ++ sortByValue greater
  where
    middle_val = snd mid
    lesser = filter (\n -> snd n < middle_val) xs
    greater = filter (\n -> snd n >= middle_val) xs

-- Spam filter in action                                                                                                                                                                                                                                                     

main :: IO ()
main = do
  spamTrain <- lines <$> readFile "spam.txt"
  hamTrain <- lines <$> readFile "ham.txt"
  testMsg <- getLine
  let spamModel = train spamTrain
      hamModel = train hamTrain
    
  -- print $ take 100 $ reverse $ sortByValue $ Map.toList $ fst hamModel

  print $ classify spamModel hamModel testMsg