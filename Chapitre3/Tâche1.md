Voici le code Haskell demandé avec une fonction `checkNumber` utilisant une instruction if-then-else et un `main` pour tester les cas spécifiés :

```haskell
checkNumber :: Int -> String
checkNumber n = 
    if n > 0 then "Positif"
    else if n < 0 then "Négatif"
    else "Zéro"

main :: IO ()
main = do
    putStrLn $ "checkNumber 5: " ++ checkNumber 5
    putStrLn $ "checkNumber (-3): " ++ checkNumber (-3)
    putStrLn $ "checkNumber 0: " ++ checkNumber 0
```

Explication :
- La fonction `checkNumber` prend un entier (`Int`) et retourne une chaîne (`String`).
- Elle utilise une structure `if-then-else` imbriquée pour vérifier si le nombre est positif (`n > 0`), négatif (`n < 0`) ou nul (`n == 0`).
- Dans le `main`, on teste la fonction avec les valeurs 5, -3 et 0, et on affiche les résultats.

Sortie attendue lors de l'exécution :
```
checkNumber 5: Positif
checkNumber (-3): Négatif
checkNumber 0: Zéro
```
