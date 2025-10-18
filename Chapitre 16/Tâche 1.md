HC16T1� : Inverser une chaîne
```haskell
main :: IO ()
main = do
    let testStrings = ["Hello", "World", "123", ""]
    mapM_ (putStrLn . reverseString) testStrings
  where
    reverseString :: String -> String
    reverseString = reverse
```

### Explication
1. **Fonction `main`** :
   - `let testStrings = ["Hello", "World", "123", ""]` : Une liste de chaînes de test, y compris une chaîne vide pour vérifier la robustesse.
   - `mapM_ (putStrLn . reverseString) testStrings` : Applique `reverseString` à chaque chaîne, affiche le résultat avec `putStrLn`, et utilise `mapM_` pour exécuter les actions IO dans l'ordre.

2. **Fonction `reverseString`** :
   - `reverseString :: String -> String` : Définit une fonction qui prend une chaîne (`String`) et renvoie sa version inversée.
   - `reverse` : Utilise la fonction intégrée `reverse` de Haskell, qui inverse une liste (et donc une chaîne, car `String` est un synonyme de `[Char]`).
