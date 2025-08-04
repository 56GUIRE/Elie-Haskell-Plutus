:HC2T2 - Tâche 2 : Signatures de fonctions

```haskell
-- Signatures des fonctions
add :: Int -> Int -> Int
isEven :: Int -> Bool
concatStrings :: String -> String -> String

-- Implémentations des fonctions
add x y = x + y
-- Additionne deux entiers

isEven n = n `mod` 2 == 0
-- Vérifie si un nombre est pair

concatStrings s1 s2 = s1 ++ s2
-- Concatène deux chaînes

-- Fonction principale avec tests
main :: IO ()
main = do
    -- Test de add
    let sum1 = add 5 3
        sum2 = add (-2) 7
    putStrLn $ "Somme de 5 et 3 : " ++ show sum1
    putStrLn $ "Somme de -2 et 7 : " ++ show sum2
    
    -- Test de isEven
    let even1 = isEven 4
        even2 = isEven 7
    putStrLn $ "4 est pair ? : " ++ show even1
    putStrLn $ "7 est pair ? : " ++ show even2
    
    -- Test de concatStrings
    let concat1 = concatStrings "Bonjour, " "Haskell!"
        concat2 = concatStrings "" "Vide"
    putStrLn $ "Concaténation 1 : " ++ concat1
    putStrLn $ "Concaténation 2 : " ++ concat2
```

**Sortie attendue :**
```
Somme de 5 et 3 : 8
Somme de -2 et 7 : 5
4 est pair ? : True
7 est pair ? : False
Concaténation 1 : Bonjour, Haskell!
Concaténation 2 : Vide
```

**Vérifications effectuées :**
1. Les signatures des fonctions sont correctes :
   - `add :: Int -> Int -> Int` prend deux `Int` et retourne un `Int`.
   - `isEven :: Int -> Bool` prend un `Int` et retourne un `Bool`.
   - `concatStrings :: String -> String -> String` prend deux `String` et retourne un `String`.
2. Les implémentations sont conformes aux spécifications :
   - `add` utilise l'opérateur `+` pour l'addition.
   - `isEven` utilise `mod` pour vérifier la parité.
   - `concatStrings` utilise `++` pour la concaténation.
3. Le `main` est une action `IO` qui teste les fonctions avec des cas variés (positifs, négatifs, chaîne vide).
