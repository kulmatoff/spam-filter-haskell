module Main (main) where

import Data.Map.Strict (Map, fromListWith, size)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char

type TermFrequency = Map String Int

-- DF (Documents Frequency) needed to calculate IDF.
-- Basically, it shows frequency of a Word across the Documents.
-- In compare with TF, that shows frequency across all the Words.
type DocumentFrequency = Map String Int

type ClassDF = (DocumentFrequency, Int)
type ClassModel = (TermFrequency, Int)

-- base functions
tokenize :: String -> [String] -- Converts whole text into the list of separate low-case words (tokens)
tokenize text = words (map (\c -> if isAlphaNum c then toLower c else ' ') text)

train :: [String] -> ClassModel
train messages = 
  let tokens = concatMap tokenize messages
      freqs = fromListWith (+) [(w, 1) | w <- tokens]

  in (freqs, length tokens)

dfTrain :: [String] -> ClassDF -- calculating in how many documents the word occurs
dfTrain documents = 
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

classify :: ClassModel -> ClassModel -> ClassDF -> String -> String
classify spamModel hamModel doc_count msg =
  let tokens = tokenize msg
      spamScore = sum [logProb spamModel doc_count w | w <- tokens]
      hamScore = sum [logProb hamModel doc_count w | w <- tokens]
  in if spamScore > hamScore then "spam" else "ham"

sortByValue :: (Ord a) => [(String, a)] -> [(String, a)]
sortByValue [] = []
sortByValue [x] = [x]
sortByValue (mid:xs) = sortByValue lesser ++ [mid] ++ sortByValue greater
  where
    middle_val = snd mid
    lesser = filter (\n -> snd n < middle_val) xs
    greater = filter (\n -> snd n >= middle_val) xs

getMetrics :: (String -> Bool) -> [String] -> [String] -> Double
getMetrics binaryClassifier spamList hamList =
  let true_positive = sum [fromEnum $ not (binaryClassifier msg) | msg <- spamList]
      false_positive = sum [fromEnum $ binaryClassifier msg | msg <- spamList]
      true_negative = sum [fromEnum $ not (binaryClassifier msg) | msg <- hamList]
      false_negative = sum [fromEnum $ binaryClassifier msg | msg <- hamList]
      acc = fromIntegral(true_positive + true_negative) / fromIntegral(length (spamList ++ hamList))
  in acc

-- Spam filter in action                                                                                                                                                                                                                                                     

main :: IO ()
main = do
  spamTrain <- lines <$> readFile "spam.txt"
  hamTrain <- lines <$> readFile "ham.txt"

  let allDocsTrain = spamTrain ++ hamTrain
   
  -- testMsg <- getLine
  let spamModel = train spamTrain
      hamModel = train hamTrain
      dfModel = dfTrain allDocsTrain
  
  let get_class msg = classify spamModel hamModel dfModel msg == "spam"
      acc = getMetrics get_class spamTrain hamTrain

  
  print acc
    
  -- print $ take 100 $ reverse $ sortByValue $ Map.toList $ fst hamModel

  -- print $ classify spamModel hamModel dfModel testMsg
