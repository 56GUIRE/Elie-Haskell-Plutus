HC14T6 : Structure du projet avec src et app

### 1. Fichier `.cabal` (`random-number.cabal`)

Le fichier `.cabal` configure le projet et spécifie les dossiers `app` et `src`.

```cabal
cabal-version:       3.0
name:                random-number
version:             0.1.0.0
synopsis:            A simple Haskell project with src and app structure
license:             BSD-3-Clause
author:              Votre Nom
maintainer:          votre.email@example.com
build-type:          Simple

library
  exposed-modules:     MyLib
  build-depends:       base ^>= 4.16,
                       random >= 1.2
  hs-source-dirs:      src
  default-language:    Haskell2010

executable random-number
  main-is:             Main.hs
  build-depends:       base ^>= 4.16,
                       random >= 1.2,
                       random-number
  hs-source-dirs:      app
  default-language:    Haskell2010
```

**Explications :**
- **Section `library` :** Définit une bibliothèque avec le module `MyLib` dans `src`. Elle dépend de `base` et `random`.
- **Section `executable` :** Définit l'exécutable avec `Main.hs` dans `app`. Il dépend de `base`, `random`, et de la bibliothèque interne `random-number` (pour accéder à `MyLib`).
- `hs-source-dirs` spécifie les dossiers `src` pour la bibliothèque et `app` pour l'exécutable.

### 2. Module dans `src` (`src/MyLib.hs`)

Ce module contient une fonction simple qui utilise la bibliothèque `random` pour générer un nombre aléatoire.

```haskell
module MyLib where

import System.Random (randomRIO)

-- Fonction qui génère un nombre aléatoire entre 1 et 100
generateRandom :: IO Int
generateRandom = randomRIO (1, 100)
```

**Explications :**
- Le module `MyLib` exporte la fonction `generateRandom`.
- `generateRandom` utilise `randomRIO` pour générer un nombre aléatoire entre 1 et 100.

### 3. Module principal dans `app` (`app/Main.hs`)

Le module principal importe `MyLib` et affiche un nombre aléatoire.

```haskell
module Main where

import MyLib (generateRandom)

main :: IO ()
main = do
    randomNum <- generateRandom
    putStrLn $ "Nombre aléatoire généré : " ++ show randomNum
```

**Explications :**
- `Main` importe `generateRandom` depuis `MyLib`.
- Le `main` appelle `generateRandom` et affiche le résultat.

