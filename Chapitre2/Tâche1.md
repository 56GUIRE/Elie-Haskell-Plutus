HC2T1 - Tâche 1 : Vérification des types dans GHCi

### Types attendus (avant vérification dans GHCi) :
1. `42` : Entier, type attendu : `Int` ou `Integer` (en général, GHCi choisit `Integer` pour les littéraux entiers sans contrainte spécifique).
2. `3.14` : Nombre à virgule flottante, type attendu : `Double` ou `Float` (GHCi choisit souvent `Double` pour les littéraux à virgule).
3. `"Haskell"` : Chaîne de caractères, type attendu : `String` (qui est un alias pour `[Char]`).
4. `'Z'` : Caractère unique, type attendu : `Char`.
5. `Vrai et faux` : En Haskell, les booléens sont `True` et `False`, et l'opération `et` correspond à `&&`. Type attendu pour `True && False` : `Bool`.

### Code Haskell :
```haskell
module Main where

main :: IO ()
main = do
  putStrLn "Expression 42 : type attendu = Integer"
  putStrLn $ "Valeur : " ++ show 42
  putStrLn "Expression 3.14 : type attendu = Double"
  putStrLn $ "Valeur : " ++ show 3.14
  putStrLn "Expression \"Haskell\" : type attendu = String"
  putStrLn $ "Valeur : " ++ show "Haskell"
  putStrLn "Expression 'Z' : type attendu = Char"
  putStrLn $ "Valeur : " ++ show 'Z'
  putStrLn "Expression True && False : type attendu = Bool"
  putStrLn $ "Valeur : " ++ show (True && False)
```

### Explications :
- Le programme définit un `main` de type `IO ()` qui affiche chaque expression avec son type attendu et sa valeur.
- Les expressions sont : `42`, `3.14`, `"Haskell"`, `'Z'`, et `True && False`.
- Pour vérifier les types dans GHCi, vous pouvez charger ce programme ou entrer directement les expressions avec la commande `:t`. Voici ce que vous obtiendriez dans GHCi :
  ```haskell
  :t 42
  42 :: Num p => p  -- En pratique, souvent Integer
  :t 3.14
  3.14 :: Fractional p => p  -- En pratique, souvent Double
  :t "Haskell"
  "Haskell" :: String
  :t 'Z'
  'Z' :: Char
  :t True && False
  True && False :: Bool
  ```

### Instructions pour GHCi :
1. Ouvrez GHCi dans votre terminal avec la commande `ghci`.
2. Entrez chaque expression précédée de `:t` pour vérifier son type, par exemple `:t 42`.
3. Comparez les types affichés par GHCi avec les types attendus ci-dessus.

### Résultat attendu lors de l'exécution du programme :
Si vous compilez et exécutez le programme (par exemple, avec `ghc fichier.hs` puis `./fichier`), la sortie sera :
```
Expression 42 : type attendu = Integer
Valeur : 42
Expression 3.14 : type attendu = Double
Valeur : 3.14
Expression "Haskell" : type attendu = String
Valeur : "Haskell"
Expression 'Z' : type attendu = Char
Valeur : 'Z'
Expression True && False : type attendu = Bool
Valeur : False
```
