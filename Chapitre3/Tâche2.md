Voici le code Haskell demandé avec une fonction `grade` utilisant des gardes et un `main` pour tester les cas spécifiés :

```haskell
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise   = "F"

main :: IO ()
main = do
    putStrLn $ "grade 95: " ++ grade 95
    putStrLn $ "grade 72: " ++ grade 72
    putStrLn $ "grade 50: " ++ grade 50
```

Explication :
- La fonction `grade` prend un entier (`Int`) représentant le score et retourne une chaîne (`String`) correspondant à la note.
- Les gardes (`|`) vérifient les plages de scores pour attribuer les notes : "A" (≥90), "B" (80-89), "C" (70-79), "D" (60-69), et "F" (<60).
- Le `main` teste la fonction avec les valeurs 95, 72 et 50, et affiche les résultats.

Sortie attendue lors de l'exécution :
```
grade 95: A
grade 72: C
grade 50: F
```
