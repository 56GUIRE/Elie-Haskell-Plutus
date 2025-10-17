Voici un programme en Haskell qui définit et utilise une exception personnalisée pour gérer les erreurs liées à un système de feux tricolores. Le programme inclut un `main` pour démontrer le lancement et la capture de l'exception.

```haskell
module Main where

import Control.Exception
import System.IO

-- Définition de l'exception personnalisée
data TrafficLightError = InvalidLightState String
  deriving Show

-- Faire en sorte que TrafficLightError soit une instance de Exception
instance Exception TrafficLightError

-- État possible des feux tricolores
data LightState = Red | Yellow | Green
  deriving (Show, Eq)

-- Fonction pour valider l'état du feu tricolore
validateLightState :: String -> IO LightState
validateLightState state = case state of
  "Red"    -> return Red
  "Yellow" -> return Yellow
  "Green"  -> return Green
  _        -> throwIO $ InvalidLightState $ "État invalide du feu tricolore : " ++ state

-- Fonction pour changer l'état du feu
changeLightState :: LightState -> String -> IO LightState
changeLightState currentState newStateStr = do
  newState <- validateLightState newStateStr
  case (currentState, newState) of
    (Red, Green)    -> return Green
    (Green, Yellow) -> return Yellow
    (Yellow, Red)   -> return Red
    _ -> throwIO $ InvalidLightState $ "Transition invalide de " ++ show currentState ++ " à " ++ newStateStr

-- Main pour tester l'exception
main :: IO ()
main = do
  putStrLn "Démarrage du système de feux tricolores..."
  let initialState = Red
  putStrLn $ "État initial : " ++ show initialState

  -- Essayer une transition valide
  result <- try (changeLightState initialState "Green") :: IO (Either TrafficLightError LightState)
  case result of
    Left err -> putStrLn $ "Erreur : " ++ show err
    Right newState -> putStrLn $ "Nouvel état : " ++ show newState

  -- Essayer une transition invalide
  result2 <- try (changeLightState initialState "Yellow") :: IO (Either TrafficLightError LightState)
  case result2 of
    Left err -> putStrLn $ "Erreur : " ++ show err
    Right newState -> putStrLn $ "Nouvel état : " ++ show newState

  -- Essayer un état invalide
  result3 <- try (validateLightState "Blue") :: IO (Either TrafficLightError LightState)
  case result3 of
    Left err -> putStrLn $ "Erreur : " ++ show err
    Right newState -> putStrLn $ "Nouvel état : " ++ show newState
```

### Explication :
1. **Exception personnalisée** :
   - `TrafficLightError` est défini comme un type d'exception personnalisé avec un message d'erreur.
   - Il est rendu instance de la classe `Exception` grâce à `Control.Exception`.

2. **État des feux tricolores** :
   - `LightState` est un type de données énuméré représentant les états valides : `Red`, `Yellow`, `Green`.

3. **Validation de l'état** :
   - La fonction `validateLightState` vérifie si une chaîne correspond à un état valide. Si ce n'est pas le cas, elle lance une exception `InvalidLightState`.

4. **Transition d'état** :
   - La fonction `changeLightState` gère les transitions valides entre les états des feux (par exemple, de `Red` à `Green`, de `Green` à `Yellow`, etc.). Toute transition invalide déclenche une exception.

5. **Main** :
   - Le programme commence avec un feu à l'état `Red`.
   - Il teste une transition valide (`Red` → `Green`), une transition invalide (`Red` → `Yellow`), et un état invalide (`Blue`).
   - Les erreurs sont capturées avec `try` et affichées.

### Sortie attendue :
Lorsque vous exécutez ce programme, vous obtiendrez quelque chose comme :

```
Démarrage du système de feux tricolores...
État initial : Red
Nouvel état : Green
Erreur : InvalidLightState "Transition invalide de Red à Yellow"
Erreur : InvalidLightState "État invalide du feu tricolore : Blue"
```

### Notes :
- Le programme utilise `throwIO` pour lancer des exceptions dans le monade `IO`.
- Les erreurs sont gérées de manière sécurisée avec `try`, qui retourne un `Either` contenant soit l'erreur (`Left`), soit le résultat (`Right`).
- Vous pouvez compiler et exécuter ce code avec GHC (par exemple, `ghc -o traffic_light traffic_light.hs` et `./traffic_light`).

Si vous avez besoin de modifications ou d'explications supplémentaires, faites-le-moi savoir !
