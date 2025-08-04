Voici le code Haskell demandé avec une fonction `maxOfThree` utilisant `let` pour stocker les valeurs intermédiaires maximales et un `main` pour tester les cas spécifiés :

```haskell
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
    let maxAB = if a > b then a else b  -- Maximum entre a et b
        maxABC = if maxAB > c then maxAB else c  -- Maximum entre maxAB et c
    in maxABC

main :: IO ()
main = do
    putStrLn $ "maxOfThree 10 20 15: " ++ show (maxOfThree 10 20 15)
    putStrLn $ "maxOfThree 5 25 10: " ++ show (maxOfThree 5 25 10)
```

### Explication :
- La fonction `maxOfThree` prend trois entiers (`Int`) et retourne le plus grand d'entre eux.
- À l'intérieur d'un `let`, on calcule :
  - `maxAB` : le maximum entre `a` et `b` en utilisant `if-then-else`.
  - `maxABC` : le maximum entre `maxAB` et `c` en utilisant `if-then-else`.
- La fonction retourne `maxABC`, qui est le maximum des trois nombres.
- Le `main` teste la fonction avec les triplets `(10, 20, 15)` et `(5, 25, 10)`, et affiche les résultats avec `show` pour convertir les entiers en chaînes.

### Calculs pour vérification :
1. **Pour `maxOfThree 10 20 15`** :
   - `maxAB = if 10 > 20 then 10 else 20` → `20`
   - `maxABC = if 20 > 15 then 20 else 15` → `20`
   - Résultat : `20`
2. **Pour `maxOfThree 5 25 10`** :
   - `maxAB = if 5 > 25 then 5 else 25` → `25`
   - `maxABC = if 25 > 10 then 25 else 10` → `25`
   - Résultat : `25`

### Sortie attendue :
```
maxOfThree 10 20 15: 20
maxOfThree 5 25 10: 25
```

### Remarque :
- La fonction suppose que les entrées sont des entiers valides. Si des validations supplémentaires sont nécessaires (par exemple, pour gérer des cas spécifiques), elles pourraient être ajoutées.
- Une alternative serait d'utiliser la fonction intégrée `max` de Haskell (par exemple, `max a (max b c)`), mais l'énoncé demande explicitement d'utiliser `let` et des comparaisons intermédiaires, ce qui est respecté ici.
