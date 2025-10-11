HC13T3 : Trier et retourner les fichiers filtrés

```haskell
import System.Directory (listDirectory)
import Data.List (sort, filter, isInfixOf)
import Control.Monad (forM_)

main :: IO ()
main = do
  -- Liste les fichiers du répertoire courant
  contents <- listDirectory "."
  
  -- Filtre les éléments contenant la sous-chaîne "txt"
  let filtered = filter (isInfixOf "txt") contents
  
  -- Trie les éléments filtrés
  let sorted = sort filtered
  
  -- Affiche les résultats
  putStrLn "Fichiers triés contenant 'txt' :"
  forM_ sorted $ \item -> do
    putStrLn item
```

### Explication :

1. **Imports** :
   - `System.Directory (listDirectory)` : Pour lister les fichiers.
   - `Data.List (sort, filter, isInfixOf)` : Pour trier, filtrer et vérifier la sous-chaîne.
   - `Control.Monad (forM_)` : Pour itérer et afficher.

2. **Fonction `main`** :
   - `contents <- listDirectory "."` : Récupère la liste des fichiers/dossiers du répertoire courant.
   - `let filtered = filter (isInfixOf "txt") contents` : Filtre pour ne garder que les éléments contenant "txt".
   - `let sorted = sort filtered` : Trie la liste filtrée par ordre alphabétique.
   - `putStrLn "Fichiers triés contenant 'txt' :"` : Affiche un titre.
   - `forM_ sorted $ \item -> putStrLn item` : Affiche chaque élément trié.

  
