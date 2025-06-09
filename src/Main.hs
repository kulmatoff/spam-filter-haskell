module Main (main) where

import Data.Map.Strict (Map, fromListWith, size)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char
import Data.Maybe (fromMaybe)

type WordCounts = Map String Int

-- DF (Documents Frequence) needed to calculate IDF.
-- Basically, it shows frequency of a Word across the Documents.
-- In compare with TF, that shows frequency across all the Words.
type DocumentCounts = Map String Int

type ClassDF = (DocumentCounts, Int)
type ClassModel = (WordCounts, Int)

-- base functions
tokenize :: String -> [String] -- Converts whole text into the list of separate low-case words (tokens)
tokenize text = words (map (\c -> if isAlphaNum c then toLower c else ' ') text)

train :: [String] -> ClassModel
train messages = 
  let tokens = concatMap tokenize messages
      freqs = fromListWith (+) [(w, 1) | w <- tokens]

  in (freqs, length tokens)

df_train :: [String] -> ClassDF -- calculating in how many documents the word occurs
df_train documents = 
  let 
    freq_map = [Map.fromListWith (+) [(word, 1) | word <- (Set.toList $ Set.fromList $ tokenize x)] | x <- documents]
    summed_freq_map = Map.unionsWith (+) freq_map
  in
    (summed_freq_map, length documents)

logProb :: ClassModel -> ClassDF -> String -> Double
logProb (wc, total) (df_map, total_docs) word = 
  let count = Map.findWithDefault 0 word wc
      df_count = Map.findWithDefault 0 word $ df_map
      tf_part = (fromIntegral (count + 1)) / fromIntegral (total + size wc)
      idf_part = log (fromIntegral(total_docs + 1) / fromIntegral(df_count + 1))
  in log tf_part * idf_part

classify :: ClassModel -> ClassModel -> ClassDF -> String -> [(String, Double)]
classify spamModel hamModel doc_count msg =
  let words = tokenize msg
      spamScore = sum [logProb spamModel doc_count w | w <- words]
      hamScore = sum [logProb hamModel doc_count w | w <- words]
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

  let allDocsTrain = spamTrain ++ hamTrain
   
  testMsg <- getLine
  let spamModel = train spamTrain
      hamModel = train hamTrain
      dfModel = df_train allDocsTrain
    
  -- print $ take 100 $ reverse $ sortByValue $ Map.toList $ fst hamModel

  print $ classify spamModel hamModel dfModel testMsg