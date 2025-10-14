HC13T6 : Convertir des fichiers filtrés en map
```haskell
import System.Directory (listDirectory)
import Data.List (filter, isInfixOf)
import qualified Data.Map as Map

-- Fonction pour convertir une liste de fichiers filtrés en une map clé/valeur
filesToMap :: String -> IO (Map.Map Int String)
filesToMap substring = do
  -- Récupère la liste des fichiers dans le répertoire courant
  entries <- listDirectory "."
  -- Filtre les fichiers contenant la sous-chaîne
  let filtered = filter (\entry -> isInfixOf substring entry) entries
  -- Crée une map avec des indices comme clés et les noms de fichiers comme valeurs
  let indexed = zip [1..] filtered
  let indexedMap = Map.fromList indexed
  return indexedMap

-- Programme principal
main :: IO ()
main = do
  let substring = "txt"  -- Sous-chaîne fixe pour tester
  result <- filesToMap substring
  if Map.null result
    then putStrLn $ "Aucun fichier ne contient la sous-chaîne : " ++ substring
    else do
      putStrLn $ "Fichiers filtrés convertis en map (clé = indice, valeur = nom) :"
      mapM_ (\(k, v) -> putStrLn $ show k ++ " -> " ++ v) (Map.toList result)
```

### Explication :

1. **Importations** :
   - `import System.Directory (listDirectory)` : Importe uniquement la fonction `listDirectory` pour lister les fichiers du répertoire courant. Cela suit le conseil de ton cours d'importer seulement les éléments nécessaires, évitant la pollution de l'environnement.
   - `import Data.List (filter, isInfixOf)` : Importe `filter` (pour filtrer les listes) et `isInfixOf` (pour vérifier si une sous-chaîne est présente), comme utilisé dans l'exemple du cours pour chercher des fichiers.
   - `import qualified Data.Map as Map` : Importe le module `Data.Map` avec un espace de noms qualifié (`Map`) pour éviter les conflits avec d'autres fonctions (par exemple, `filter` d'un autre module), comme recommandé dans la section "Contrôle des espaces de noms" de ton cours.

2. **Fonction `filesToMap`** :
   - `filesToMap :: String -> IO (Map.Map Int String)` : Définit une fonction qui prend une sous-chaîne (`String`) et retourne une action `IO` produisant une map (`Map.Map Int String`). Les clés sont des entiers (`Int`) représentant des indices, et les valeurs sont des chaînes (`String`) pour les noms de fichiers. Le type `IO` indique que cette fonction effectue une opération d'entrée/sortie (lire les fichiers).
   - `entries <- listDirectory "."` : Utilise `listDirectory` pour obtenir la liste des fichiers et dossiers dans le répertoire courant (`.`) et la stocke dans `entries`. C'est une action `IO` qui renvoie une liste de `FilePath` (alias pour `String`).
   - `let filtered = filter (\entry -> isInfixOf substring entry) entries` : Filtre la liste `entries` pour ne garder que les noms de fichiers contenant la sous-chaîne. La fonction `isInfixOf` vérifie si `substring` est une sous-chaîne de `entry`, et `filter` applique ce critère, comme dans l'exemple du cours avec `find'`.
   - `let indexed = zip [1..] filtered` : Crée une liste de paires en associant chaque fichier filtré à un indice croissant (commençant à 1) avec `zip [1..]`. Cela suit l'approche du cours utilisant `zip` pour générer des clés.
   - `let indexedMap = Map.fromList indexed` : Convertit la liste de paires en une map avec `Map.fromList`, comme montré dans ton cours pour transformer une liste en `Map`.
   - `return indexedMap` : Retourne la map dans le contexte `IO`.

3. **Fonction `main`** :
   - `let substring = "txt"` : Définit une sous-chaîne fixe `"txt"` pour tester la fonction. Cela remplace une entrée utilisateur (`getLine`) pour simplifier le code dans ton éditeur mobile.
   
