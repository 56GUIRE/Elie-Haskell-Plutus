Voici le code Haskell demandé avec une fonction `triangleType` utilisant des gardes pour classer un triangle en fonction de ses côtés, et un `main` pour tester les cas spécifiés :

```haskell
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Équilatéral"
    | a == b || b == c || a == c = "Isocèle"
    | otherwise = "Scalène"

main :: IO ()
main = do
    putStrLn $ "triangleType 3 3 3: " ++ triangleType 3 3 3
    putStrLn $ "triangleType 5 5 8: " ++ triangleType 5 5 8
    putStrLn $ "triangleType 6 7 8: " ++ triangleType 6 7 8
```

### Explication :
- La fonction `triangleType` prend trois `Float` représentant les longueurs des côtés `a`, `b` et `c` d'un triangle.
- Les gardes (`|`) sont utilisées pour classer le triangle :
  - Si tous les côtés sont égaux (`a == b && b == c`), le triangle est **Équilatéral**.
  - Si exactement deux côtés sont égaux (`a == b || b == c || a == c`), le triangle est **Isocèle**.
  - Sinon (`otherwise`), le triangle est **Scalène**.
- Le `main` teste la fonction avec les triplets `(3, 3, 3)`, `(5, 5, 8)` et `(6, 7, 8)`, et affiche les résultats.

### Sortie attendue :
```
triangleType 3 3 3: Équilatéral
triangleType 5 5 8: Isocèle
triangleType 6 7 8: Scalène
```

### Remarque :
- Les valeurs d'entrée sont supposées valides (c.-à-d. formant un triangle réel selon l'inégalité triangulaire). Si une validation est nécessaire (par exemple, vérifier que `a + b > c`, `b + c > a`, et `a + c > b`), cela pourrait être ajouté avec des conditions supplémentaires.
- Les comparaisons d'égalité sur des `Float` peuvent être sensibles aux erreurs d'arrondi dans des cas réels. Pour une robustesse accrue, une tolérance pourrait être introduite, mais ici, les entrées sont des nombres entiers, donc cela ne pose pas de problème.
