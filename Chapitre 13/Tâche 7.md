HC13T7 : Utiliser un module personnalisé dans main

```haskell
import System.IO

-- Définition du module SumNonEmpty (simulée dans un fichier unique)
data MyData = Error String | Result Int deriving (Show)

sumNonEmpty :: [Int] -> MyData
sumNonEmpty xs
  | null xs = Error "List is empty"
  | otherwise = Result (foldl (+) 0 xs)

-- Programme principal
main :: IO ()
main = do
  let numbers :: [Int] = [1, 2, 3, 4]  -- Liste de nombres pour tester avec type explicite
  let result = sumNonEmpty numbers
  case result of
    Error msg -> putStrLn $ "Erreur : " ++ msg
    Result sum -> putStrLn $ "La somme de " ++ show numbers ++ " est : " ++ show sum

  let emptyList :: [Int] = []  -- Liste vide pour tester l'erreur avec type explicite
  let resultEmpty = sumNonEmpty emptyList
  case resultEmpty of
    Error msg -> putStrLn $ "Erreur : " ++ msg
    Result sum -> putStrLn $ "La somme de " ++ show emptyList ++ " est : " ++ show sum
```

### Explication :

1. **Importation et Module** :
   - `import System.IO` : Importe les fonctionnalités d'entrée/sortie de base (`putStrLn`) nécessaires pour afficher les résultats.
   - `data MyData = Error String | Result Int deriving (Show)` : Définit un type personnalisé comme dans ton cours, avec deux constructeurs : `Error` pour un message d'erreur (`String`) et `Result` pour la somme (`Int`). La clause `deriving (Show)` permet d'afficher les valeurs de `MyData` directement, mais si cela pose problème, je peux le retirer (dis-moi si besoin).
   - `sumNonEmpty :: [Int] -> MyData` : Fonction qui prend une liste d'entiers et retourne un `MyData`. Elle suit l'exemple du cours en gérant les listes vides avec une erreur.

2. **Fonction `sumNonEmpty`** :
   - `| null xs = Error "List is empty"` : Vérifie si la liste est vide avec `null` et retourne `Error` avec un message si c'est le cas.
   - `| otherwise = Result (foldl (+) 0 xs)` : Sinon, calcule la somme des éléments avec `foldl (+) 0 xs` (qui accumule la somme en partant de 0) et retourne `Result`. `foldl` est utilisé ici comme une alternative à `sum` pour explicitement parcourir la liste.

3. **Fonction `main`** :
   - `let numbers :: [Int] = [1, 2, 3, 4]` : Définit une liste non vide avec une annotation de type explicite (`:: [Int]`) pour forcer le compilateur à reconnaître le type, évitant les erreurs d'ambiguïté.
   - `let result = sumNonEmpty numbers` : Calcule le résultat pour `numbers`.
   - `case result of` : Utilise une expression `case` pour gérer les deux cas possibles :
     - `Error msg -> putStrLn $ "Erreur : " ++ msg` : Affiche le message d'erreur si la liste est vide (bien que cela ne s'applique pas ici).
     - `Result sum -> putStrLn $ "La somme de " ++ show numbers ++ " est : " ++ show sum` : Affiche la somme formatée si le calcul réussit.
   - `let emptyList :: [Int] = []` : Définit une liste vide avec une annotation de type explicite.
   - `let resultEmpty = sumNonEmpty emptyList` : Calcule le résultat pour `emptyList`.
   - Une seconde expression `case` gère `resultEmpty` de la même manière.

  
