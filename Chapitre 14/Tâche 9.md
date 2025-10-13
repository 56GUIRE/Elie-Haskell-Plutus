Voici un exemple de programme Haskell qui utilise l'extension `PartialTypeSignatures` pour permettre des types jokers (`_`) dans la signature de fonction, avec une fonction `counts` (comme dans votre demande précédente) et un `main` pour tester :

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

### Compilation et exécution :
Pour compiler ce programme avec GHC :
```bash
ghc -o counts counts.hs
./counts
```

Sortie attendue :
```
[('e',1),('h',1),('l',2),('o',1)]
```

### Notes :
- L'extension `PartialTypeSignatures` est utile pour prototyper ou lorsque le type exact d'une partie de la signature n'est pas critique.
- GHC peut émettre des avertissements si les types jokers ne sont pas assez contraints, mais ici, le contexte est suffisant pour inférer correctement `Char` et `IO ()`.
- Si vous voulez voir les types inférés par GHC, vous pouvez compiler avec l'option `-Wpartial-type-signatures` pour obtenir des avertissements détaillant les inférences.
