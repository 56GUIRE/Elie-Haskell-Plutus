HC5T9 : Fonction d'ordre supérieur pour transformer une liste


```haskell
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4]
    print $ transformList (+5) numbers  -- Affiche [11,12,13,14]

transformList :: (a -> a) -> [a] -> [a]
transformList f xs = map (f . f) xs
```

### Explications :
- **Fonction d'ordre supérieur** : `transformList` prend une fonction `f` (de type `a -> a`) et une liste `xs` (de type `[a]`), et applique `f` deux fois à chaque élément de la liste.
- **Composition** : L'expression `(f . f)` utilise l'opérateur de composition `.` pour créer une fonction qui applique `f` deux fois consécutivement (i.e., `f (f x)`).
- **Application** : `map (f . f) xs` applique la fonction composée à chaque élément de la liste `xs`.
- **Type** : La signature `(a -> a) -> [a] -> [a]` indique que `f` est une fonction unitaire, et la liste d'entrée et de sortie est du même type générique `a`.
- **Main** : On teste `transformList` avec la fonction `(+5)` sur la liste `[1, 2, 3, 4]` :
  - Pour chaque élément `x`, `(+5)` est appliqué deux fois : `x + 5 + 5 = x + 10`.
  - Ainsi, `[1, 2, 3, 4]` devient `[1+10, 2+10, 3+10, 4+10] = [11, 12, 13, 14]`.
- **Résultat** : Le programme affiche `[11, 12, 13, 14]`.

