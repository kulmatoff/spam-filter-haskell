module Main (main) where

bayesFunc :: Double -> Double -> Double -> Double
bayesFunc p_B_given_A p_A p_B = (p_B_given_A * p_A) / p_B

main :: IO ()

main = do
  print(bayesFunc (2/5) 0.5 (6/11))
