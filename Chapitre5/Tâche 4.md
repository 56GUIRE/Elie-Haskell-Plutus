HC5T4 : Utiliser les fonctions lambda
Voici le code en Haskell qui réécrit la fonction `biggerThan10` en utilisant une fonction lambda, avec un `main` pour tester :

```haskell
-- Réécriture de biggerThan10 avec une lambda
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- Fonction principale
main :: IO ()
main = do
    print $ biggerThan10 15  -- Affiche True
    print $ biggerThan10 5   -- Affiche False
```

### Explications :
- La fonction `biggerThan10` est réécrite comme une lambda : `\x -> x > 10`, qui prend un entier `x` et retourne `True` si `x > 10`, sinon `False`.
- Dans le `main`, on teste la fonction avec deux valeurs :
  - `biggerThan10 15` retourne `True` car 15 > 10.
  - `biggerThan10 5` retourne `False` car 5 n'est pas > 10.
- `print` affiche les résultats dans la console.

Ce code est concis, respecte les conventions de Haskell, et peut être exécuté dans GHC pour voir les résultats.
