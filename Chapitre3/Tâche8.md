HC3T8 - Tâche avancée 8 : Calculer l'IMC et retourner la catégorie avec où:

```haskell
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5          = "Insuffisance pondérale"
    | bmi >= 18.5 && bmi <= 24.9 = "Normal"
    | bmi >= 25 && bmi <= 29.9   = "Surpoids"
    | bmi >= 30           = "Obésité"
    where bmi = weight / (height * height)

main :: IO ()
main = do
    putStrLn $ "bmiCategory 70 1.75: " ++ bmiCategory 70 1.75
    putStrLn $ "bmiCategory 90 1.8: " ++ bmiCategory 90 1.8
```

### Explication :
- La fonction `bmiCategory` prend deux `Float` : `weight` (poids en kg) et `height` (taille en mètres).
- Dans une clause `where`, l'IMC est calculé avec la formule : `bmi = weight / (height * height)`.
- Les gardes (`|`) classifient l'IMC selon les catégories :
  - `< 18.5` → "Insuffisance pondérale"
  - `18.5` à `24.9` → "Normal"
  - `25` à `29.9` → "Surpoids"
  - `≥ 30` → "Obésité"
- Le `main` teste la fonction avec les paires `(70, 1.75)` et `(90, 1.8)`, et affiche les résultats.

### Calculs pour vérification :
1. **Pour `bmiCategory 70 1.75`** :
   - IMC = `70 / (1.75 * 1.75) = 70 / 3.0625 ≈ 22.857`
   - `18.5 ≤ 22.857 ≤ 24.9` → "Normal"
2. **Pour `bmiCategory 90 1.8`** :
   - IMC = `90 / (1.8 * 1.8) = 90 / 3.24 ≈ 27.778`
   - `25 ≤ 27.778 ≤ 29.9` → "Surpoids"

### Sortie attendue :
```
bmiCategory 70 1.75: Normal
bmiCategory 90 1.8: Surpoids
```

### Remarque :
- Les entrées sont supposées valides (poids et taille positifs, taille non nulle). Si une validation est nécessaire (par exemple, pour vérifier que `height > 0`), cela pourrait être ajouté avec des conditions supplémentaires.
- Les comparaisons sur les `Float` sont utilisées directement, mais pour des applications réelles, une tolérance pourrait être envisagée pour gérer les imprécisions des flottants.
