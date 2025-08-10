HC1T4 - Tâche 4 : Composer une fonction pour traiter des données de joueurs:
Voici un code Haskell complet avec un main qui fait exactement ce que tu demandes :

import Data.List (sortBy)
import Data.Ord (comparing)

-- Fonction qui extrait uniquement les noms des joueurs
extractPlayers :: [(String, Int)] -> [String]
extractPlayers joueurs = [nom | (nom, _) <- joueurs]

-- Fonction qui trie les joueurs par score décroissant
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (flip (comparing snd))

-- Fonction qui retourne les trois meilleurs joueurs
topThree :: [(String, Int)] -> [(String, Int)]
topThree joueurs = take 3 joueurs

-- Fonction composée : récupère le top 3 des noms des joueurs
getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- Programme principal
main :: IO ()
main = do
    let joueurs = [("Alice", 120), ("Bob", 90), ("Charlie", 150), ("David", 80), ("Eve", 110)]
    putStrLn "Les trois meilleurs joueurs sont :"
    print (getTopThreePlayers joueurs)

💡 Explication rapide :

extractPlayers utilise une compréhension de liste pour ne garder que les noms.

sortByScore utilise sortBy et comparing snd pour trier selon le score (snd = deuxième élément du tuple).

topThree prend les 3 premiers après tri.

getTopThreePlayers compose les trois fonctions en un seul pipeline.


Si tu veux, je peux aussi te faire le même code mais avec des symboles Haskell expliqués en détail comme on avait fait pour doubleThenIncrement.
Veux-tu que je te fasse ça ?

