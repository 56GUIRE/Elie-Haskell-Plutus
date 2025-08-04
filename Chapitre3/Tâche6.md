Voici le code Haskell demandé avec une fonction `isLeapYear` utilisant `if-then-else` pour déterminer si une année est bissextile, et un `main` pour tester le cas spécifié :

```haskell
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then True
    else if year `mod` 100 == 0 then False
    else if year `mod` 4 == 0 then True
    else False

main :: IO ()
main = do
    putStrLn $ "isLeapYear 2000: " ++ show (isLeapYear 2000)
```

### Explication :
- La fonction `isLeapYear` prend un entier (`Int`) représentant une année et retourne un booléen (`Bool`).
- Elle utilise une structure `if-then-else` imbriquée pour vérifier les règles des années bissextiles :
  - Si l'année est divisible par 400 (`year mod 400 == 0`), elle est bissextile (`True`).
  - Sinon, si elle est divisible par 100 (`year mod 100 == 0`), elle n'est pas bissextile (`False`).
  - Sinon, si elle est divisible par 4 (`year mod 4 == 0`), elle est bissextile (`True`).
  - Sinon, elle n'est pas bissextile (`False`).
- Le `main` teste la fonction avec l'année `2000` et affiche le résultat, en utilisant `show` pour convertir le booléen en chaîne.

### Sortie attendue :
```
isLeapYear 2000: True
```

### Remarque :
- L'année 2000 est bissextile car elle est divisible par 400, ce qui satisfait la première condition.
- La fonction suppose que l'entrée est une année valide (entier positif). Si une validation supplémentaire est nécessaire (par exemple, pour exclure les années négatives), cela pourrait être ajouté.
