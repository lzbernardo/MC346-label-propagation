-- Projeto 1 : MC346
--
-- Bernardo Lanza de Assunção : RA 164836
-- Bianca Yoshie Itiroko : RA 164923
--

import Text.Read
import Data.List
import Data.Maybe

euclideanDistance :: Floating c => [c] -> [c] -> c
euclideanDistance a=sqrt.sum.map((^2).uncurry(flip(-))).zip a

-- Turns a subset of ["pointName", "x1", "x2", ... "xn"] into a tuple ("pointName", [x1, x2, ... xn])
buildTuple :: [String] -> (String,[Double])
buildTuple [] = ("NaN", [0.0])
buildTuple (x:xs) = (x,map (read::String->Double) xs)

-- Appeds a dictionary item
appendLabel :: Int -> String -> [(Int, [String])] -> [(Int, [String])]
appendLabel key s ((a,b):xs)
  | key == a = (a,b++[s]):xs
  | otherwise = (a,b):appendLabel key s xs

-- Formats the output dictionary
attDict :: [[String]] -> [(Int, [String])]
attDict l = attDict' l []
attDict' [] acc = acc
attDict' (s:ss) acc
  | label `elem` (map fst acc) = attDict' ss (appendLabel label (head s) acc)
  | otherwise = attDict' ss (acc++[(label, [head s])])
  where label = (read::String->Int) (last s)

-- Filters elements with (Bool 1) or without (Bool 0) labels
filterLabel :: Bool -> [String] -> [(String,[Double])] -> [(String,[Double])]
filterLabel _ _ [] = []
filterLabel _ [] l = l
filterLabel b s (t:ts)
  | ((fst t) `elem` s) == b = t : (filterLabel b s ts)
  | otherwise = filterLabel b s ts

-- Finds the minimal distance between a point and a list of points
findMinDist :: (String,[Double]) -> [(String,[Double])] -> (Double,[String])
findMinDist t dict = (minDistance, [fst t]++[fst (dict!!(fromJust (elemIndex minDistance mappedDistances)))])
  where minDistance = minimum mappedDistances
        mappedDistances = map (`euclideanDistance` (snd t)) (map snd dict)

-- Propagates labels one by one
propagate :: [(String,[Double])] -> [(String,[Double])] -> [[String]] -> [[String]]
propagate [] _ lp = lp
propagate sl cl lp = propagate (delete minPoint sl) (cl++[minPoint]) newlp
  where minDist = foldl1' (\ac it -> if (fst ac) < (fst it) then ac else it) (foldl (\ac it -> ac++[findMinDist it sl]) [] cl)
        minDistName = last $ snd minDist
        newlp = lp ++ [[last $ snd minDist]++[last $ head $ filter (\ls -> if (head ls) == (head $ snd minDist) then True else False) lp]]
        minPoint = foldl1 (\ac it -> if minDistName == (fst it) then it else ac) sl

main :: IO ()
main = do
    inputs <- fmap lines getContents

    let dictPoints = map (buildTuple.words) (fst $ break (== "") inputs)
    let labeledPoints = map words (tail $ snd $ break (== "") inputs)
    let dictWithLabel = filterLabel True (map head labeledPoints) dictPoints
    let dictWithoutLabel = filterLabel False (map head labeledPoints) dictPoints

    mapM_ print (attDict $ propagate dictWithoutLabel dictWithLabel labeledPoints)
