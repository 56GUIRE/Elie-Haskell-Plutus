HC15T4 : Utiliser une fonction de gestion d'exception pour les feux
```haskell
module Main where

import Control.Exception
import System.IO

-- Type pour représenter les états des feux tricolores
data TrafficLight = Red | Green | Orange deriving (Show, Eq)

-- Type d'exception personnalisé pour les erreurs liées aux feux tricolores
data TrafficLightException = InvalidState String deriving Show

instance Exception TrafficLightException

-- Fonction pure pour changer l'état du feu tricolore
changeLight :: TrafficLight -> Either TrafficLightException TrafficLight
changeLight Red = Right Green
changeLight Green = Right Orange
changeLight Orange = Right Red
changeLight state = Left (InvalidState $ "État du feu tricolore non valide : " ++ show state)

-- Fonction gestionnaire d'exceptions
handleTrafficLightException :: TrafficLightException -> IO TrafficLight
handleTrafficLightException (InvalidState msg) = do
    putStrLn $ "Erreur capturée : " ++ msg
    putStrLn "Retour à l'état par défaut : Rouge"
    return Red

-- Fonction pour simuler le fonctionnement du feu tricolore
simulateTrafficLight :: TrafficLight -> IO TrafficLight
simulateTrafficLight state = do
    putStrLn $ "État actuel du feu : " ++ show state
    putStrLn "Changement d'état..."
    case changeLight state of
        Right nextState -> do
            putStrLn $ "Nouvel état : " ++ show nextState
            return nextState
        Left err -> handleTrafficLightException err

-- Fonction principale
main :: IO ()
main = do
    -- Test avec un état valide
    putStrLn "Test avec un état valide (Vert) :"
    _ <- simulateTrafficLight Green
    
    -- Test avec un état limite
    putStrLn "\nTest avec un état limite :"
    _ <- simulateTrafficLight Orange
    
    putStrLn "\nProgramme terminé."
```

### Explication détaillée

#### 1. **Objectif du programme**
Le programme simule un système de feux tricolores avec trois états (`Red`, `Green`, `Orange`) et gère les exceptions potentielles liées à des transitions d'état non valides. Une fonction gestionnaire d'exceptions (`handleTrafficLightException`) est utilisée pour intercepter et traiter ces erreurs, en affichant un message et en revenant à un état par défaut (`Red`). Le programme inclut une fonction `main` pour démontrer son fonctionnement.

#### 2. **Structure du code**
- **Type `TrafficLight`** : Définit les états possibles des feux tricolores (`Red`, `Green`, `Orange`) avec les dérivations `Show` (pour l'affichage) et `Eq` (pour la comparaison).
- **Type d'exception `TrafficLightException`** : Un type personnalisé pour représenter les erreurs, avec un constructeur `InvalidState` contenant un message d'erreur. Il est rendu instance de la classe `Exception` pour être utilisable avec le mécanisme d'exceptions de Haskell.
- **Fonction `changeLight`** : Une fonction pure qui calcule l'état suivant d'un feu tricolore. Elle retourne un `Either TrafficLightException TrafficLight` pour indiquer soit un succès (`Right` avec le nouvel état), soit une erreur (`Left` avec une exception).
- **Fonction `handleTrafficLightException`** : Le gestionnaire d'exceptions qui affiche un message d'erreur et retourne l'état par défaut `Red`.
- **Fonction `simulateTrafficLight`** : Simule une transition d'état en affichant l'état actuel, en appelant `changeLight`, et en gérant le résultat (succès ou erreur).
- **Fonction `main`** : Exécute deux tests : un avec un état valide (`Green`) et un avec un état limite (`Orange`), pour montrer le fonctionnement normal.

