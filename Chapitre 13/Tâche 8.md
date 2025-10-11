import System.IO
import qualified Data.List as List
import qualified Data.Map as Map

-- Fonction pour filtrer une liste avec Data.List.filter
filterList :: [Int] -> [Int]
filterList xs = List.filter (> 0) xs

-- Fonction pour filtrer une map avec Data.Map.filter (simulée avec une liste convertie)
filterMap :: [(Int, String)] -> Map.Map Int String
filterMap pairs = Map.fromList $ List.filter (\(k, _) -> k > 0) pairs

-- Programme principal
main :: IO ()
main = do
  let listData = [-1, 0, 1, 2, 3]
  let filteredList = filterList listData
  putStrLn $ "Liste filtrée (valeurs > 0) : " ++ show filteredList

  let mapData = [(-1, "neg"), (0, "zero"), (1, "one"), (2, "two")]
  let filteredMap = filterMap mapData
  putStrLn $ "Map filtrée (clés > 0) : " ++ show (Map.toList filteredMap)
