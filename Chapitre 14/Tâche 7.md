HC14T7 : Composant bibliothèque dans le .cabal
### 1. Fichier `.cabal` (`random-number.cabal`)

Le fichier `.cabal` est configuré pour inclure un composant bibliothèque (`library`) et un exécutable (`executable`), avec une dépendance sur le paquet `random`.

```cabal
cabal-version:       3.0
name:                random-number
version:             0.1.0.0
synopsis:            A Haskell project with a library and executable
license:             BSD-3-Clause
author:              Votre Nom
maintainer:          votre.email@example.com
build-type:          Simple

-- Composant bibliothèque
library
  exposed-modules:     MyLib
  build-depends:       base ^>= 4.16,
                       random >= 1.2
  hs-source-dirs:      src
  default-language:    Haskell2010

-- Composant exécutable
executable random-number
  main-is:             Main.hs
  build-depends:       base ^>= 4.16,
                       random >= 1.2,
                       random-number
  hs-source-dirs:      app
  default-language:    Haskell2010
```

**Explications :**
- **Section `library` :** Définit une bibliothèque nommée `random-number` (par défaut, le même nom que le projet). Elle expose le module `MyLib` situé dans `src`. Elle dépend de `base` et `random`.
- **Section `executable` :** Définit l'exécutable nommé `random-number`, avec `Main.hs` comme point d'entrée dans `app`. Il dépend de `base`, `random`, et de la bibliothèque interne `random-number` (pour accéder à `MyLib`).
- `hs-source-dirs` spécifie `src` pour la bibliothèque et `app` pour l'exécutable.

### 2. Module de la bibliothèque (`src/MyLib.hs`)

Ce module définit une fonction qui génère un nombre aléatoire entre 1 et 100.

```haskell
module MyLib where

import System.Random (randomRIO)

-- Fonction pour générer un nombre aléatoire entre 1 et 100
generateRandom :: IO Int
generateRandom = randomRIO (1, 100)
```

**Explications :**
- Le module `MyLib` exporte la fonction `generateRandom`.
- `randomRIO` est utilisé pour générer un nombre aléatoire dans la plage spécifiée.

### 3. Module principal (`app/Main.hs`)

Le module principal importe `MyLib` et utilise la fonction `generateRandom` pour afficher un nombre aléatoire.

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

