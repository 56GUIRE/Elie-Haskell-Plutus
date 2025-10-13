HC14T4 : Type d'extensionApplications

```haskell
{-# LANGUAGE TypeApplications #-}

module Main where

-- Fonction pour convertir une String en Int
stringToInt :: String -> Int
stringToInt s = read @Int s

main :: IO ()
main = do
    let input = "42"  -- Exemple de chaîne
    let result = stringToInt input
    putStrLn $ "Conversion de \"" ++ input ++ "\" en Int : " ++ show result
```

### Explications
- **Extension `TypeApplications` :**
  - Activée avec la pragma `{-# LANGUAGE TypeApplications #-}`.
  - Permet de spécifier explicitement le type attendu pour une fonction polymorphe comme `read` en utilisant la syntaxe `@Type`.
  - Ici, `read @Int s` indique que `read` doit convertir la chaîne `s` en un `Int`.

- **Fonction `stringToInt` :**
  - Prend une `String` en entrée et utilise `read @Int` pour la convertir en `Int`.
  - La syntaxe `@Int` clarifie que le résultat de `read` doit être un `Int`, évitant l'ambiguïté de type.

- **Programme principal (`main`) :**
  - Définit une chaîne d'exemple (`"42"`).
  - Appelle `stringToInt` pour convertir la chaîne en `Int`.
  - Affiche le résultat avec un message descriptif.

