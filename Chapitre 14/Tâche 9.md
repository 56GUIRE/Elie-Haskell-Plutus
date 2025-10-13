HC14T9 : Extension de signatures de type partiel

```haskell
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.List (sort, group)

-- Fonction counts avec une signature partielle utilisant un joker
counts :: String -> [(_, Int)]
counts str = [(head g, length g) | g <- group (sort str)]

-- Fonction main pour tester counts
main :: IO _
main = do
    let testString = "hello"
    print $ counts testString  -- Affiche [('e',1),('h',1),('l',2),('o',1)]
```

### Explications :
1. **Extension `PartialTypeSignatures`** :
   - Activée avec `{-# LANGUAGE PartialTypeSignatures #-}` en haut du fichier.
   - Permet d'utiliser des types jokers (`_`) dans les signatures de fonctions, laissant le compilateur inférer les parties manquantes.
   - Dans la signature de `counts :: String -> [(_, Int)]`, le premier élément du tuple est un joker (`_`), que GHC infère comme étant `Char`.
   - Dans `main :: IO _`, le type de retour est un joker, inféré comme `IO ()`.

2. **Fonction `counts`** :
   - Identique à l'exemple précédent : elle prend une chaîne, trie ses caractères, les regroupe, et retourne une liste de tuples `(caractère, fréquence)`.
   - La signature partielle `[(_, Int)]` indique que le résultat est une liste de tuples où le second élément est un `Int`, et le premier est inféré comme `Char`.

3. **Fonction `main`** :
   - Teste `counts` avec la chaîne `"hello"`.
   - Affiche le résultat `[('e',1),('h',1),('l',2),('o',1)]`.


