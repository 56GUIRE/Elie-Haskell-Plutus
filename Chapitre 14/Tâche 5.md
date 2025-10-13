HC14T5 : Type Résultat a et pattern matching avec @
```haskell
module Main where

-- Définition du type personnalisé Result
data Result a = Success a | Failure String deriving (Show)

-- Fonction utilisant le pattern matching avec @
processResult :: Result Int -> String
processResult result@(Success n) = "Succès avec la valeur " ++ show n ++ " (structure complète : " ++ show result ++ ")"
processResult result@(Failure msg) = "Échec avec le message : " ++ msg ++ " (structure complète : " ++ show result ++ ")"

main :: IO ()
main = do
    let successCase = Success 42
        failureCase = Failure "Une erreur s'est produite"
    
    putStrLn $ processResult successCase
    putStrLn $ processResult failureCase
```

### Explications
- **Type personnalisé `Result a` :**
  - Déclaré avec `data Result a = Success a | Failure String`.
  - `Result` est un type paramétrique qui peut contenir une valeur de type `a` dans le cas `Success`, ou un message d'erreur de type `String` dans le cas `Failure`.
  - L'option `deriving (Show)` permet d'afficher les valeurs de `Result` avec `show`.

- **Pattern matching avec `@` (as-pattern) :**
  - Dans la fonction `processResult`, le symbole `@` est utilisé pour lier la structure complète du `Result` à la variable `result`, tout en décomposant ses parties.
  - Pour `result@(Success n)`, `result` contient toute la structure (par exemple, `Success 42`), et `n` contient la valeur `42`.
  - Pour `result@(Failure msg)`, `result` contient toute la structure (par exemple, `Failure "Une erreur..."`), et `msg` contient le message d'erreur.
  - Cela permet d'accéder à la fois à la structure entière et à ses composants dans le pattern matching.

- **Programme principal (`main`) :**
  - Crée deux exemples : `Success 42` et `Failure "Une erreur s'est produite"`.
  - Appelle `processResult` pour chaque cas et affiche les résultats.

