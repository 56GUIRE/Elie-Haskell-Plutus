HC13T4 : Module SumNonEmpty

```haskell
import System.IO

sumNonEmpty :: [Int] -> Maybe Int
sumNonEmpty xs
  | null xs = Nothing
  | otherwise = Just (foldl (+) 0 xs)

-- Fonction utilitaire pour construire la chaîne de sortie
showSum :: [Int] -> Maybe Int -> String
showSum xs result = case result of
  Nothing -> "Erreur : La liste est vide."
  Just res -> "Somme de " ++ show xs ++ " = " ++ show res

main :: IO ()
main = do
  let test1 :: [Int] = [1, 2, 3, 4]  -- Liste non vide avec type explicite
      test2 :: [Int] = []             -- Liste vide avec type explicite
  
  -- Test avec une liste non vide
  putStrLn $ showSum test1 (sumNonEmpty test1)
  
  -- Test avec une liste vide
  putStrLn $ showSum test2 (sumNonEmpty test2)
```

### Explication :

1. **Imports** :
   - `import System.IO` : Importe des fonctionnalités d'entrée/sortie de base, nécessaires pour `putStrLn`. Cela garantit que le programme peut afficher des résultats dans la console.

2. **Fonction `sumNonEmpty`** :
   - `sumNonEmpty :: [Int] -> Maybe Int` : Définit une fonction qui prend une liste d'entiers (`[Int]`) et retourne un type `Maybe Int`. Le type `Maybe` permet de gérer deux cas : `Nothing` pour une liste vide, et `Just` avec la somme pour une liste non vide.
   - `| null xs = Nothing` : Si la liste `xs` est vide (vérifié avec `null`), retourne `Nothing` pour indiquer qu'aucune somme ne peut être calculée.
   - `| otherwise = Just (foldl (+) 0 xs)` : Sinon, utilise `foldl (+) 0 xs` pour calculer la somme des éléments en parcourant la liste de gauche à droite avec une initialisation à 0, et retourne `Just` avec le résultat. (Note : `foldl` est équivalent à `sum` ici, mais explicite.)

3. **Fonction `showSum`** :
   - `showSum :: [Int] -> Maybe Int -> String` : Une fonction utilitaire qui prend une liste `[Int]` et un `Maybe Int`, et retourne une chaîne (`String`) formatée.
   - `case result of` : Utilise une expression `case` pour gérer les deux possibilités de `Maybe` :
     - `Nothing -> "Erreur : La liste est vide."` : Si `result` est `Nothing`, retourne un message d'erreur.
     - `Just res -> "Somme de " ++ show xs ++ " = " ++ show res` : Si `result` est `Just res`, construit une chaîne avec la liste (`show xs`), un signe égal, et le résultat (`show res`). La fonction `show` convertit les valeurs en chaînes pour les concaténer avec `++`.

4. **Fonction `main`** :
   - `let test1 :: [Int] = [1, 2, 3, 4]` : Définit une liste non vide avec un type explicite `[Int]` pour éviter les ambiguïtés de type.
   - `let test2 :: [Int] = []` : Définit une liste vide avec un type explicite.
   - `putStrLn $ showSum test1 (sumNonEmpty test1)` : Calcule la somme de `test1`, passe le résultat à `showSum`, et affiche la chaîne générée.
   - `putStrLn $ showSum test2 (sumNonEmpty test2)` : Fait de même pour `test2`, ce qui donnera le message d'erreur car la liste est vide.

  
