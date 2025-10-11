import System.IO
import System.Directory (listDirectory)
import Data.List (filter, sort)

-- Fonction pour rechercher et trier les fichiers contenant une sous-chaîne
searchAndSortFiles :: String -> IO [String]
searchAndSortFiles substring = do
  -- Liste tous les fichiers du répertoire courant
  files <- listDirectory "."
  -- Filtre les fichiers contenant la sous-chaîne
  let filteredFiles = filter (isInfixOf substring) files
  -- Trie les résultats
  return (sort filteredFiles)

-- Programme principal
main :: IO ()
main = do
  let searchTerm = "txt"  -- Sous-chaîne fixe pour tester
  results <- searchAndSortFiles searchTerm
  if null results
    then putStrLn $ "Aucun fichier ne contient la sous-chaîne : " ++ searchTerm
    else do
      putStrLn $ "Fichiers contenant '" ++ searchTerm ++ "' (triés) :"
      mapM_ putStrLn results
