Voici un programme Haskell qui liste tous les fichiers du répertoire courant en utilisant le module `System.Directory`. Le programme inclut une fonction `main` pour exécuter cette tâche :

```haskell
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_)

main :: IO ()
main = do
  -- Obtient la liste des fichiers et dossiers dans le répertoire courant
  contents <- listDirectory "."
  
  -- Affiche chaque élément de la liste
  putStrLn "Fichiers et dossiers dans le répertoire courant :"
  forM_ contents $ \item -> do
    putStrLn item
```

### Explication :

1. **Imports** :
   - `System.Directory (listDirectory)` : Importe la fonction `listDirectory` du module `System.Directory`, qui retourne une liste des entrées (fichiers et dossiers) dans un répertoire donné.
   - `System.FilePath ((</>))` : Importe l'opérateur `</>` pour manipuler les chemins de fichiers, bien qu'il ne soit pas utilisé directement ici (inclus pour une éventuelle extension).
   - `Control.Monad (forM_)` : Importe `forM_`, une fonction utilitaire qui applique une action `IO` à chaque élément d'une liste et ignore le résultat (utile pour l'affichage).

2. **Fonction `main`** :
   - `contents <- listDirectory "."` : Utilise `listDirectory` avec `"."` pour lister le contenu du répertoire courant. Le résultat est stocké dans `contents`, qui est une liste de `FilePath` (chaînes représentant les noms des fichiers/dossiers).
   - `putStrLn "Fichiers et dossiers dans le répertoire courant :"` : Affiche un titre pour la sortie.
   - `forM_ contents $ \item -> do ...` : Itère sur chaque élément de `contents` et affiche son nom avec `putStrLn item`.

3. **Sortie attendue** :
   - Le programme affichera une liste des fichiers et dossiers présents dans le répertoire courant où il est exécuté. Par exemple :
     ```
     Fichiers et dossiers dans le répertoire courant :
     file1.txt
     file2.hs
     dossier1
     dossier2
     ```

### Instructions pour exécuter :
1. Sauvegarde ce code dans un fichier, par exemple `listFiles.hs`.
2. Compile et exécute avec GHC :
   ```bash
   ghc listFiles.hs
   ./listFiles
   ```
   Ou utilise GHCi :
   ```bash
   ghci listFiles.hs
   *Main> main
   ```
3. Assure-toi d'avoir les permissions nécessaires pour accéder au répertoire courant.

### Remarques :
- Cette version liste uniquement les noms des fichiers et dossiers dans le répertoire courant, sans inclure les sous-dossiers récursivement.
- Si tu veux inclure les détails (comme la taille ou la date), tu peux étendre le programme en utilisant d'autres fonctions de `System.Directory` (par exemple, `getFileSize` ou `getModificationTime`).
- Si une erreur survient (par exemple, pas de permission), le programme pourrait échouer ; tu peux ajouter une gestion d'erreur avec `catch` si nécessaire.

Essaie ce code et dis-moi si tu as besoin d'ajustements !
