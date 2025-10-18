HC15T9 : Utiliser try pour intercepter les exceptions IO de fichier
```haskell
import Control.Exception (try)
import System.IO.Error (isDoesNotExistError, ioeGetErrorString)
import System.IO (readFile)

main :: IO ()
main = do
    let fileNames = ["test.txt", "nonexistent.txt", "empty.txt"] -- Fichiers à tester
    mapM_ processFile fileNames
  where
    processFile fileName = do
        result <- try (readFile fileName) :: IO (Either IOError String)
        case result of
            Left err ->
                if isDoesNotExistError err
                    then putStrLn $ "Erreur : Le fichier " ++ fileName ++ " n'existe pas."
                    else putStrLn $ "Erreur lors de la lecture de " ++ fileName ++ ": " ++ ioeGetErrorString err
            Right content -> putStrLn $ "Contenu de " ++ fileName ++ " : " ++ content
```

### Explication
1. **Importations** :
   - `import Control.Exception (try)` : Importe `try`, qui encapsule une opération IO dans un `Either IOError a`, capturant les exceptions.
   - `import System.IO.Error (isDoesNotExistError, ioeGetErrorString)` : Fournit des utilitaires pour identifier les erreurs spécifiques (comme "fichier inexistant") et extraire les messages d'erreur.
   - `import System.IO (readFile)` : Importe `readFile` pour lire le contenu d'un fichier.

2. **Fonction `main`** :
   - `let fileNames = ["test.txt", "nonexistent.txt", "empty.txt"]` : Définit une liste de noms de fichiers à tester. Ces noms sont simulés ; dans un environnement réel, "test.txt" ou "empty.txt" devraient exister pour que la lecture réussisse.
   - `mapM_ processFile fileNames` : Applique `processFile` à chaque nom de fichier et affiche les résultats.

3. **Fonction `processFile`** :
   - `result <- try (readFile fileName) :: IO (Either IOError String)` : Tente de lire le fichier avec `readFile`. Si une exception survient, `try` renvoie `Left err` ; sinon, `Right content`.
   - `case result of` :
     - `Left err` : Si une erreur survient :
       - `if isDoesNotExistError err` : Vérifie si l'erreur est due à l'absence du fichier et affiche un message spécifique.
       - Sinon, utilise `ioeGetErrorString err` pour afficher un message d'erreur générique avec les détails.
     - `Right content` : Si la lecture réussit, affiche le contenu du fichier avec son nom.

4. **Gestion élégante des exceptions** :
   - `try` capture les exceptions IO (par exemple, `IOError`) et les transforme en un type `Either`, permettant une gestion structurée.
   - Les messages d'erreur sont clairs et spécifiques, améliorant l'expérience utilisateur.
