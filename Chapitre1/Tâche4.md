HC1T4 - Tâche 4 : Composer une fonction pour traiter des données de joueurs:
```haskell
module Main where

import Data.List (sortBy) -- Importation pour utiliser sortBy

-- Extrait les noms des joueurs d'une liste de tuples (nom, score)
extractPlayers :: [(String, Int)] -> [String]
extractPlayers players = map fst players

-- Trie les joueurs par score décroissant
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore players = sortBy (\(_, score1) (_, score2) -> compare score2 score1) players

-- Retourne les trois premiers joueurs
topThree :: [(String, Int)] -> [(String, Int)]
topThree players = take 3 players

-- Compose les fonctions pour obtenir les noms des trois meilleurs joueurs
getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- Fonction principale pour tester le programme
main :: IO ()
main = do
    let players = [("Alice", 100), ("Bob", 85), ("Charlie", 90), ("David", 95), ("Eve", 80)]
    putStrLn "Liste initiale des joueurs :"
    print players
    putStrLn "\nNoms des joueurs extraits :"
    print $ extractPlayers players
    putStrLn "\nJoueurs triés par score décroissant :"
    print $ sortByScore players
    putStrLn "\nTop 3 joueurs (avec scores) :"
    print $ topThree $ sortByScore players
    putStrLn "\nNoms des 3 meilleurs joueurs :"
    print $ getTopThreePlayers players
```
### Explication détaillée du code
Voici une explication claire et structurée de chaque partie du code, en évitant les répétitions inutiles et en restant concise :

1. **extractPlayers** :
   - **Signature** : `[(String, Int)] -> [String]`
   - **Rôle** : Prend une liste de tuples où chaque tuple contient un nom (`String`) et un score (`Int`), et extrait uniquement les noms.
   - **Mécanisme** : Utilise `map fst`, où `fst` extrait le premier élément d'un tuple. `map` applique `fst` à chaque tuple de la liste.
   - **Exemple** : Pour `[("Alice", 100), ("Bob", 85)]`, retourne `["Alice", "Bob"]`.

2. **sortByScore** :
   - **Signature** : `[(String, Int)] -> [(String, Int)]`
   - **Rôle** : Trie la liste de tuples par score en ordre décroissant.
   - **Mécanisme** : Utilise `sortBy` (du module `Data.List`) avec une fonction de comparaison `\(_, score1) (_, score2) -> compare score2 score1`. Cette fonction compare les scores en inversant l'ordre (`score2` avant `score1`) pour un tri décroissant.
   - **Exemple** : Pour `[("Alice", 100), ("Bob", 85), ("Charlie", 90)]`, retourne `[("Alice", 100), ("Charlie", 90), ("Bob", 85)]`.

3. **topThree** :
   - **Signature** : `[(String, Int)] -> [(String, Int)]`
   - **Rôle** : Extrait les trois premiers éléments d'une liste de tuples.
   - **Mécanisme** : Utilise `take 3` pour limiter la liste aux trois premiers éléments. Si la liste a moins de trois éléments, elle retourne la liste entière.
   - **Exemple** : Pour `[("Alice", 100), ("Charlie", 90), ("Bob", 85)]`, retourne `[("Alice", 100), ("Charlie", 90), ("Bob", 85)]`.

4. **getTopThreePlayers** :
   - **Signature** : `[(String, Int)] -> [String]`
   - **Rôle** : Combine les trois fonctions pour retourner les noms des trois joueurs ayant les meilleurs scores.
   - **Mécanisme** : Utilise la composition de fonctions avec l'opérateur `(.)`. L'ordre d'exécution est :
     1. `sortByScore` trie la liste par score décroissant.
     2. `topThree` prend les trois premiers tuples de la liste triée.
     3. `extractPlayers` extrait les noms des trois tuples.
   - **Exemple** : Pour `[("Alice", 100), ("Bob", 85), ("Charlie", 90)]`, retourne `["Alice", "Charlie", "Bob"]`.

5. **main** :
   - **Rôle** : Fonction principale pour tester le programme.
   - **Mécanisme** :
     - Définit une liste de test `players` avec cinq joueurs et leurs scores.
     - Affiche la liste initiale, les résultats intermédiaires (noms extraits, liste triée, top 3 avec scores), et le résultat final (noms des trois meilleurs joueurs).
     - Utilise `putStrLn` pour afficher du texte et `print` pour afficher les structures de données.
   - **Sortie** (pour la liste donnée) :
     ```
     Liste initiale des joueurs :
     [("Alice",100),("Bob",85),("Charlie",90),("David",95),("Eve",80)]

     Noms des joueurs extraits :
     ["Alice","Bob","Charlie","David","Eve"]

     Joueurs triés par score décroissant :
     [("Alice",100),("David",95),("Charlie",90),("Bob",85),("Eve",80)]

     Top 3 joueurs (avec scores) :
     [("Alice",100),("David",95),("Charlie",90)]

     Noms des 3 meilleurs joueurs :
     ["Alice","David","Charlie"]
     ```

### Points clés
- **Style fonctionnel** : Le code utilise des concepts fonctionnels de Haskell, comme la composition de fonctions (`(.)`) et l'application de fonctions pures (`map`, `sortBy`, `take`).
- **Robustesse** : Le code gère correctement les cas où la liste d'entrée est vide ou contient moins de trois joueurs (retourne une liste vide ou partielle).
- **Clarté** : Les noms des fonctions sont explicites, et les commentaires expliquent leur rôle.
