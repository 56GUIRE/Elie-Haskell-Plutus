HC14T8 : Fonction de comptage de fréquence de caractères
```haskell
module Main where

import Data.List (sort, group)

-- Fonction counts qui retourne la fréquence des caractères sous forme de liste de tuples
counts :: String -> [(Char, Int)]
counts str = [(head g, length g) | g <- group (sort str)]

-- Fonction main pour tester counts
main :: IO ()
main = do
    let testString = "hello"
    print $ counts testString  -- Affiche [( 'e', 1), ('h', 1), ('l', 2), ('o', 1)]
```

### Explications :
1. **Fonction `counts`** :
   - `sort str` trie la chaîne pour regrouper les caractères identiques.
   - `group` regroupe les caractères consécutifs identiques en sous-listes.
   - La compréhension de liste `[(head g, length g) | g <- group (sort str)]` crée des tuples où :
     - `head g` prend le premier caractère de chaque groupe.
     - `length g` compte le nombre d'occurrences dans chaque groupe.
   - Le résultat est une liste de tuples `(caractère, fréquence)`.

2. **Fonction `main`** :
   - Teste la fonction `counts` avec la chaîne `"hello"`.
   - Affiche le résultat `[( 'e', 1), ('h', 1), ('l', 2), ('o', 1)]`.
`
