HC1T1 - Tâche 1 : Composition de fonctions:

```haskell
-- Définition des fonctions
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

-- Fonction main pour tester
main :: IO ()
main = do
    let x = 5
    print $ double x           -- Affiche 10
    print $ doubleThenIncrement x  -- Affiche 11
```

### Explications :
- `double` : Multiplie un nombre par 2.
- `increment` : Augmente un nombre de 1.
- `doubleThenIncrement` : Utilise la composition de fonctions (avec l'opérateur `.`) pour appliquer `double` puis `increment`.
- `main` : Teste les fonctions avec une valeur d'entrée (ici `5`). Affiche `10` (5 * 2) et `11` ((5 * 2) + 1).
