import Data.Trees.KdTree
import Data.List
import Data.Char
import Text.Read
-- import qualified Data.Map as Map

-- Structure of a k-dimensional point
data KPoint = KPoint { k :: Int, list :: [Double]} deriving (Eq, Ord, Show)
instance Point KPoint where
  dimension p = k p
  coord i p = (list p)!!i

-- This one reads each line into a string
readInputs :: IO [String]
readInputs = fmap lines getContents

-- Turns a subset of ["pointName", "x1", "x2", ... "xn"] into a tuple ("pointName", [x1, x2, ... xn])
buildTuple :: [String] -> (String,[Double])
buildTuple [] = ("NaN", [0.0])
buildTuple (x:xs) = (x,map (read::String->Double) xs)

-- Converts a list of doubles to a K-dimensional point
listToPoint :: [Double] -> KPoint
listToPoint k = KPoint (length k) k

main :: IO ()
main = do
    inputs <- readInputs
    -- print inputs

    -- Lista de listas [[String]]
    -- print (map words inputs)

    -- Dicionário [("Nome", [Double])]
    -- print (map buildTuple (map words inputs))

    -- Lista de pontos [[Double]]:
    -- print (map snd (map buildTuple (map words inputs)))
    -- print (map (snd.buildTuple.words) inputs)

    -- Lista de KPoints [KPoint]
    let kpl = map (listToPoint.snd.buildTuple.words) inputs
    print kpl

    -- Constrói a KDTree
    let kdt = fromList kpl

    -- Busca pelo ponto 4-dimensional mais próximo
    print (nearestNeighbor kdt (KPoint 4 [60.0,60.0,60.0,60.0]))

    -- mapM_ putStrLn inputs
