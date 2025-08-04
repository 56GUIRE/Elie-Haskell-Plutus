Voici le code Haskell demandé avec une fonction `season` utilisant des gardes pour déterminer la saison en fonction du mois, et un `main` pour tester les cas spécifiés :

```haskell
season :: Int -> String
season month
    | month == 12 || month == 1 || month == 2  = "Hiver"
    | month == 3  || month == 4 || month == 5  = "Printemps"
    | month == 6  || month == 7 || month == 8  = "Été"
    | month == 9  || month == 10 || month == 11 = "Automne"
    | otherwise                                = "Mois invalide"

main :: IO ()
main = do
    putStrLn $ "season 3: " ++ season 3
    putStrLn $ "season 7: " ++ season 7
    putStrLn $ "season 11: " ++ season 11
```

### Explication :
- La fonction `season` prend un entier (`Int`) représentant un mois (1 à 12) et retourne une chaîne (`String`) correspondant à la saison.
- Les gardes (`|`) vérifient les mois pour associer la saison correspondante :
  - Mois 12, 1, 2 → "Hiver"
  - Mois 3, 4, 5 → "Printemps"
  - Mois 6, 7, 8 → "Été"
  - Mois 9, 10, 11 → "Automne"
  - Tout autre valeur → "Mois invalide" (pour gérer les entrées hors de la plage 1-12).
- Le `main` teste la fonction avec les mois `3`, `7` et `11`, et affiche les résultats.

### Sortie attendue :
```
season 3: Printemps
season 7: Été
season 11: Automne
```

### Remarque :
- La fonction inclut une clause `otherwise` pour gérer les mois invalides (par exemple, <1 ou >12), bien que cela ne soit pas strictement requis par l'énoncé, pour rendre le programme plus robuste.
- Les mois sont supposés être des entiers. Si des validations supplémentaires sont nécessaires (par exemple, pour des types d'entrée incorrects), elles pourraient être ajoutées.
