HC1T4 - Tâche 4 : Composer une fonction pour traiter des données de joueurs:

```haskell
module Main where

import Data.List (sortBy)
import Data.Ord (comparing)

-- Extrait les noms des joueurs d'une liste de tuples
extractPlayers :: [(String, Int)] -> [String]
extractPlayers xs = [name | (name, _) <- xs]

-- Trie la liste par score décroissant
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (comparing (negate . snd))

-- Retourne les trois meilleurs joueurs
topThree :: [(String, Int)] -> [(String, Int)]
topThree = take 3

-- Compose les fonctions pour obtenir les trois meilleurs joueurs
getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- Fonction principale pour tester
main :: IO ()
main = do
  let players = [("Alice", 100), ("Bob", 85), ("Charlie", 95), ("David", 70), ("Eve", 90)]
  print $ getTopThreePlayers players
```

### Corrections et améliorations :
1. **Utilisation de `Data.List` et `Data.Ord`** :
   - J'ai ajouté les imports `Data.List (sortBy)` et `Data.Ord (comparing)` pour utiliser les fonctions standard de tri, ce qui est plus idiomatique en Haskell.
   - Cela remplace l'implémentation manuelle de `sortBy` qui était inutile et potentiellement moins performante.

2. **Simplification de `sortByScore`** :
   - La fonction utilise maintenant `sortBy (comparing (negate . snd))` pour trier par score décroissant.
   - `comparing` transforme la deuxième composante du tuple (le score) en une comparaison ordinale.
   - `negate` inverse l'ordre pour obtenir un tri décroissant (du plus grand au plus petit score).

3. **Autres fonctions inchangées** :
   - `extractPlayers` reste identique, car elle est correcte et concise.
   - `topThree` est simplifiée en utilisant directement `take 3`, qui est déjà idiomatique.
   - `getTopThreePlayers` conserve la composition des fonctions, qui est correcte.

4. **Main conservé** :
   - Le `main` reste inchangé, car il teste correctement le programme avec une liste d'exemple.

### Sortie attendue :
Pour la liste `players = [("Alice", 100), ("Bob", 85), ("Charlie", 95), ("David", 70), ("Eve", 90)]`, la sortie sera :
```haskell
["Alice","Charlie","Eve"]
```
