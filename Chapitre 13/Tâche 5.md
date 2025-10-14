HC13T5 : Restreindre la visibilité dans le module

```haskell
import System.IO

-- Fonction principale pour calculer la somme d'une liste non vide
sumNonEmpty :: [Int] -> Maybe Int
sumNonEmpty xs
  | null xs = Nothing
  | otherwise = Just (foldl (+) 0 xs)

-- Fonction utilitaire privée pour construire la chaîne de sortie
showSum :: [Int] -> Maybe Int -> String
showSum xs result = case result of
  Nothing -> "Erreur : La liste est vide."
  Just res -> "Somme de " ++ show xs ++ " = " ++ show res

-- Programme principal
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

1. **Fonction `sumNonEmpty`** :
   - `sumNonEmpty :: [Int] -> Maybe Int` : Retourne `Nothing` pour une liste vide, `Just` avec la somme sinon.
   - `| null xs = Nothing` et `| otherwise = Just (foldl (+) 0 xs)` : Logique inchangée.

2. **Fonction `showSum`** :
   - `showSum :: [Int] -> Maybe Int -> String` : Construit la chaîne de sortie en fonction du résultat de `sumNonEmpty`.

3. **Fonction `main`** :
   - Teste `test1` et `test2` avec `showSum` pour afficher les résultats.

   
