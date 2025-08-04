HC2T3 - Tâche 3 : Variables immuables:

```haskell
-- Définir les variables immuables
myAge :: Int
myAge = 30

piValue :: Double
piValue = 3.14159

salut :: String
salut = "Bonjour"

isHaskellFun :: Bool
isHaskellFun = True

-- Fonction principale
main :: IO ()
main = do
  -- Afficher les valeurs initiales
  putStrLn $ "myAge: " ++ show myAge
  putStrLn $ "piValue: " ++ show piValue
  putStrLn $ "salut: " ++ salut
  putStrLn $ "isHaskellFun: " ++ show isHaskellFun
  
  -- Tentative de modification de myAge (création d'une nouvelle variable)
  let myNewAge = myAge + 1
  putStrLn $ "myNewAge: " ++ show myNewAge
  putStrLn $ "myAge original: " ++ show myAge
  -- Une tentative de réassignation directe comme "myAge = myAge + 1" causerait une erreur de compilation
```

### Explications
- **Variables immuables** : En Haskell, les variables (`myAge`, `piValue`, `salut`, `isHaskellFun`) sont immuables, c'est-à-dire qu'elles ne peuvent pas être modifiées après leur définition.
- **Types** :
  - `myAge` est un `Int` (entier).
  - `piValue` est un `Double` (nombre à virgule flottante).
  - `salut` est un `String` (chaîne de caractères).
  - `isHaskellFun` est un `Bool` (booléen).
- **Tentative de modification** : Une réassignation directe comme `myAge = myAge + 1` dans le même scope provoquerait une erreur de compilation ("multiple declarations"). À la place, une nouvelle variable `myNewAge` est créée avec `let` pour démontrer que l'original reste inchangé.
- **Sortie** : L'exécution affiche les valeurs initiales, puis montre que `myNewAge` vaut 31, tandis que `myAge` reste 30, illustrant l'immutabilité.

### Résultat attendu
En exécutant ce programme (par exemple avec GHC), la sortie sera :

```
myAge: 30
piValue: 3.14159
salut: Bonjour
isHaskellFun: True
myNewAge: 31
myAge original: 30
```

### Observation
La tentative de modification via une nouvelle variable montre que `myAge` reste immuable. Une réassignation directe est impossible en Haskell, ce qui garantit la sécurité et la prévisibilité du code, conformément aux principes de la programmation fonctionnelle pure.
