HC5T7 : L'opérateur $

```haskell
main :: IO ()
main = print $ result [1..10]

result :: [Int] -> Int
result xs = sum $ map (*2) $ filter (>3) xs
```

### Corrections et explications :
1. **Généralisation de la fonction** : J'ai ajouté un paramètre explicite `xs` à la fonction `result` pour rendre clair qu'elle prend une liste en entrée, plutôt que d'incorporer `[1..10]` directement dans la définition. Cela rend la fonction plus générique et réutilisable.
2. **Utilisation correcte de `$`** : L'opérateur `$` est utilisé pour remplacer les parenthèses, comme demandé, en écrivant `sum $ map (*2) $ filter (>3) xs` au lieu de `sum (map (*2) (filter (>3) xs))`. Cela fonctionne correctement et est plus lisible.
3. **Test dans `main`** : La fonction `main` applique `result` à `[1..10]` et affiche le résultat (98), comme dans l'expression originale.

### Vérification du résultat :
Pour la liste `[1..10]` :
- `filter (>3)` donne `[4, 5, 6, 7, 8, 9, 10]`.
- `map (*2)` donne `[8, 10, 12, 14, 16, 18, 20]`.
- `sum` donne `8 + 10 + 12 + 14 + 16 + 18 + 20 = 98`.


