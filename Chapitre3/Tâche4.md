Voici le code Haskell demandé avec une fonction `triangleArea` utilisant `let` pour calculer le demi-périmètre et la formule de Héron, ainsi qu'un `main` pour tester les cas spécifiés :

```haskell
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2  -- Demi-périmètre
        area = sqrt (s * (s - a) * (s - b) * (s - c))  -- Formule de Héron
    in area

main :: IO ()
main = do
    putStrLn $ "triangleArea 3 4 5: " ++ show (triangleArea 3 4 5)
    putStrLn $ "triangleArea 7 8 9: " ++ show (triangleArea 7 8 9)
```

### Explication :
- La fonction `triangleArea` prend trois `Float` représentant les longueurs des côtés `a`, `b`, et `c` d'un triangle.
- À l'intérieur d'un `let`, on calcule :
  - Le demi-périmètre `s = (a + b + c) / 2`.
  - L'aire du triangle avec la formule de Héron : `sqrt(s * (s - a) * (s - b) * (s - c))`.
- La fonction retourne l'aire calculée.
- Le `main` teste la fonction avec les triplets `(3, 4, 5)` et `(7, 8, 9)`, en affichant les résultats avec `show` pour convertir les `Float` en chaînes.

### Sortie attendue :
```
triangleArea 3 4 5: 6.0
triangleArea 7 8 9: 26.832815729997478
```

### Remarque :
- Le triplet `(3, 4, 5)` correspond à un triangle rectangle, et son aire est exactement `6.0` (car `(1/2) * 3 * 4 = 6`).
- Pour `(7, 8, 9)`, l'aire est approximativement `26.8328`, calculée précisément par la formule de Héron.
- Les valeurs d'entrée sont supposées valides (formant un triangle réel, c.-à-d. respectant l'inégalité triangulaire). Si une validation est nécessaire, il faudrait ajouter des vérifications supplémentaires.
