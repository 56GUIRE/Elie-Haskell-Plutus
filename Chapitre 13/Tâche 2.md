HC13T2 : Filtrer les fichiers par sous-chaîne

```haskell
import System.Directory (listDirectory)
import Data.List (isInfixOf)
import Control.Monad (forM_)

main :: IO ()
main = do
  -- Liste les fichiers du répertoire courant
  contents <- listDirectory "."
  let filtered = filter (isInfixOf "test") contents  -- Filtre avec une sous-chaîne fixe "test"
  
  -- Affiche les résultats
  putStrLn "Fichiers contenant 'test' :"
  forM_ filtered $ \item -> do
    putStrLn item
```

### Explication :

1. **Imports** :
   - `System.Directory (listDirectory)` : Importe la fonction `listDirectory`, qui retourne une liste des noms de fichiers et dossiers dans le répertoire courant (indiqué par `"."`).
   - `Data.List (isInfixOf)` : Importe `isInfixOf`, une fonction qui vérifie si une sous-chaîne (ici "test") est présente dans une autre chaîne (les noms de fichiers).
   - `Control.Monad (forM_)` : Importe `forM_`, qui permet d'exécuter une action `IO` (comme afficher un texte) pour chaque élément d'une liste, sans retourner de résultat.

2. **Fonction `main`** :
   - `contents <- listDirectory "."` : Utilise `listDirectory` pour obtenir la liste des fichiers et dossiers du répertoire courant. Le résultat est stocké dans la variable `contents`.
   - `let filtered = filter (isInfixOf "test") contents` : Crée une nouvelle liste `filtered` en filtrant `contents` pour ne garder que les éléments dont le nom contient la sous-chaîne "test". La fonction `isInfixOf` recherche cette sous-chaîne dans chaque nom de fichier.
   - `putStrLn "Fichiers contenant 'test' :"` : Affiche un titre pour indiquer les résultats.
   - `forM_ filtered $ \item -> do putStrLn item` : Itère sur chaque élément de `filtered` et affiche son nom avec `putStrLn`. Cela montre tous les fichiers ou dossiers contenant "test".

3. **Comportement** :
   - Le programme affiche uniquement les fichiers ou dossiers dont le nom contient "test" (par exemple, `testfile.txt` ou `mytestdir`). Si aucun fichier ne correspond, il affiche juste le titre sans liste.
   - Comme la sous-chaîne est fixée à "test", il n'y a pas besoin d'entrée utilisateur, ce qui simplifie l'exécution et évite les erreurs liées à `getLine`.

   
