HC14T1 : Initialiser un projet Cabal:

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello, Cabal!"
```

### Explications :
- Ce code est placé dans le dossier `app` de ton projet Haskell, créé avec `cabal init`.
- `module Main where` : Déclare le module principal, requis pour un exécutable.
- `main :: IO ()` : La fonction `main` est le point d'entrée du programme.
- `putStrLn "Hello, Cabal!"` : Affiche le texte « Hello, Cabal ! » dans la console.


