 HC13T10 : Fonction principale multi-modules

```haskell
import System.IO
import System.Directory (listDirectory)
import Data.List (filter, sort)
import Control.Exception (try)
import System.IO.Error (isDoesNotExistError)

-- Fonction pour vérifier si une sous-chaîne est dans une chaîne
containsSubstring :: String -> String -> Bool
containsSubstring substring str = substring `elem` words str  -- Simplification : vérifie si la sous-chaîne est dans les mots

-- Fonction pour rechercher et trier les fichiers contenant une sous-chaîne
searchAndSortFiles :: String -> IO [String]
searchAndSortFiles substring = do
  -- Tente de lister les fichiers, gère les erreurs
  fileList <- try (listDirectory ".") :: IO (Either IOError [String])
  case fileList of
    Left err
      | isDoesNotExistError err -> return ["Erreur : Répertoire introuvable"]
      | otherwise -> return ["Erreur : Problème d'accès au répertoire"]
    Right files -> do
      let filteredFiles = filter (containsSubstring substring) files
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
```


### Explication :

1. **Fonction `containsSubstring`** :
   - `containsSubstring substring str = substring `elem` words str` : Vérifie si `substring` est l'un des mots dans `str` (séparés par des espaces). C'est une approximation ; pour une recherche plus précise, on pourrait utiliser une boucle ou une vérification caractère par caractère si nécessaire.

2. **Fonction `searchAndSortFiles`** :
   - `searchAndSortFiles :: String -> IO [String]` : Prend une sous-chaîne et retourne une liste de fichiers filtrés et triés.
   - Gère les erreurs avec `try` et procède au filtrage avec `containsSubstring` et au tri avec `sort`.

3. **Fonction `main`** :
   - Définit `searchTerm = "txt"` et appelle `searchAndSortFiles`.
   - Utilise `if null results` pour afficher un message si aucun fichier ne correspond, ou liste les résultats sinon.

  
