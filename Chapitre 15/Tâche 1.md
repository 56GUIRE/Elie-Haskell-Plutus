HC15T1 : Gérer les exceptions lors de la lecture d'un fichier et du calcul de la vitesse
```haskell
import System.IO
import Control.Exception
import System.IO.Error
import Text.Read (readMaybe)

-- Fonction pour lire un fichier et retourner la distance
readDistanceFromFile :: FilePath -> IO (Either String Double)
readDistanceFromFile filePath = do
  result <- try (readFile filePath) :: IO (Either IOError String)
  case result of
    Left err -> return $ Left $ "Erreur lors de la lecture du fichier : " ++ show err
    Right content -> case readMaybe content :: Maybe Double of
      Just distance -> return $ Right distance
      Nothing -> return $ Left "Le contenu du fichier n'est pas un nombre valide"

-- Fonction pour lire le temps saisi par l'utilisateur
readTimeFromUser :: IO (Either String Double)
readTimeFromUser = do
  putStrLn "Entrez le temps (en secondes) : "
  input <- getLine
  case readMaybe input :: Maybe Double of
    Just time -> if time > 0
                 then return $ Right time
                 else return $ Left "Le temps doit être supérieur à 0"
    Nothing -> return $ Left "La saisie n'est pas un nombre valide"

-- Fonction pour calculer la vitesse
calculateSpeed :: Double -> Double -> Double
calculateSpeed distance time = distance / time

-- Fonction principale
main :: IO ()
main = do
  let filePath = "distance.txt" -- Nom du fichier à lire
  distanceResult <- readDistanceFromFile filePath
  case distanceResult of
    Left err -> putStrLn err
    Right distance -> do
      timeResult <- readTimeFromUser
      case timeResult of
        Left err -> putStrLn err
        Right time -> do
          let speed = calculateSpeed distance time
          putStrLn $ "La vitesse est : " ++ show speed ++ " m/s"
```

### Explications :
1. **Gestion des exceptions pour la lecture du fichier** :
   - La fonction `readDistanceFromFile` utilise `try` pour capturer les erreurs d'entrée/sortie (comme un fichier manquant ou non lisible).
   - Si la lecture réussit, elle vérifie si le contenu du fichier peut être converti en `Double` avec `readMaybe`.
   - Les erreurs possibles (fichier non trouvé, contenu non numérique) sont renvoyées sous forme de `Left String`.

2. **Gestion des exceptions pour la saisie utilisateur** :
   - La fonction `readTimeFromUser` lit l'entrée utilisateur et utilise `readMaybe` pour vérifier si l'entrée est un nombre valide.
   - Elle vérifie également que le temps est positif (non nul).
   - Les erreurs (saisie non numérique, temps négatif ou nul) sont renvoyées sous forme de `Left String`.

3. **Calcul de la vitesse** :
   - La fonction `calculateSpeed` effectue le calcul simple `distance / temps`.
   - Comme les vérifications ont été faites en amont, cette fonction est sûre.

4. **Fonction `main`** :
   - Coordonne l'exécution en enchaînant les appels aux fonctions `readDistanceFromFile` et `readTimeFromUser`.
   - Affiche les messages d'erreur ou le résultat du calcul selon les cas.


