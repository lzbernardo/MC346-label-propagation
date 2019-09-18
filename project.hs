import Data.Trees.KdTree
import Text.Read
-- import Data.List
-- import Data.Char
-- import qualified Data.Map as Map

-- Structure of a k-dimensional point
data KPoint = KPoint { name :: String, k :: Int, list :: [Double]} deriving (Eq, Ord, Show)
instance Point KPoint where
  dimension p = k p
  coord i p = (list p)!!i

-- Turns a subset of ["pointName", "x1", "x2", ... "xn"] into a tuple ("pointName", [x1, x2, ... xn])
buildTuple :: [String] -> (String,[Double])
buildTuple [] = ("NaN", [0.0])
buildTuple (x:xs) = (x,map (read::String->Double) xs)

-- Converts a list of doubles to a K-dimensional point
listToPoint :: [Double] -> KPoint
listToPoint l = KPoint "unnamed" (length l) l

tupleToPoint :: (String, [Double]) -> KPoint
tupleToPoint (s,d) = KPoint s (length d) d

main :: IO ()
main = do

    inputs <- fmap lines getContents
    -- mapM_ putStrLn inputs
    print inputs
    -- Dicionário [("Nome", [Double])]
    let dict = map (buildTuple.words) inputs
    putStrLn "DICT ::: "
    print dict

    -- Lista de KPoints [KPoint]
    -- let kpl = map (listToPoint.snd) dict -- DEPRECATED
    let kpl = map tupleToPoint dict
    putStrLn "KPL ::: "
    print kpl

    -- Constrói a KDTree
    let kdt = fromList kpl

    -- Busca pelo ponto 4-dimensional mais próximo
    putStrLn "NN ::: "
    print $ nearestNeighbor kdt (KPoint "flamengo" 4 [5.0,1930.0,1990.0,2000.0])

 -- DUVIDA DE PARALELIZAÇÃO: um ponto por vez ou é possível paralelizar? ex: x     .   .  . . o - o ponto mais a esquerda se torna x ou o?
