HC5T1 : Utiliser applyTwice
Voici le code en Haskell pour une fonction qui prend une fonction et un entier, puis applique la fonction trois fois à l'entier, avec un `main` pour démontrer son fonctionnement :

```haskell
-- Définition de la fonction qui applique une fonction trois fois à un entier
applyThreeTimes :: (Int -> Int) -> Int -> Int
applyThreeTimes f x = f (f (f x))

-- Fonction principale pour tester
main :: IO ()
main = do
    -- Exemple avec une fonction qui incrémente
    let inc x = x + 1
    print $ applyThreeTimes inc 5  -- Affiche 8 (5 + 1 + 1 + 1)
    -- Exemple avec une fonction qui double
    let double x = x * 2
    print $ applyThreeTimes double 3  -- Affiche 24 (3 * 2 * 2 * 2)
```

### Explications :
- `applyThreeTimes` prend une fonction `f` (de type `Int -> Int`) et un entier `x`, puis applique `f` trois fois à `x` en utilisant la composition : `f (f (f x))`.
- Dans le `main`, on teste avec deux fonctions :
  - `inc`, qui incrémente l'entier de 1 (par exemple, `applyThreeTimes inc 5` donne `5 + 1 + 1 + 1 = 8`).
  - `double`, qui double l'entier (par exemple, `applyThreeTimes double 3` donne `3 * 2 * 2 * 2 = 24`).
- Le `main` utilise `print` pour afficher les résultats dans la console.

Ce code est concis, fonctionnel et respecte les conventions de Haskell. Vous pouvez l'exécuter dans un environnement comme GHC pour voir les résultats.
