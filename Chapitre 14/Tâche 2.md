HC14T2 : Ajouter une dépendance et afficher un nombre aléatoire
### 1. Code Haskell (fichier `Main.hs`)

```haskell
module Main where

import System.Random (randomRIO)

main :: IO ()
main = do
    randomNum <- randomRIO (1 :: Int, 100 :: Int) -- Génère un nombre aléatoire entre 1 et 100
    putStrLn $ "Nombre aléatoire : " ++ show randomNum
```

**Explications :**
- `System.Random` fournit des fonctions pour la génération de nombres aléatoires.
- `randomRIO` est utilisé pour générer un nombre aléatoire dans une plage donnée (ici, 1 à 100).
- Le type `Int` est explicitement spécifié pour éviter toute ambiguïté.
- Le programme affiche le nombre aléatoire avec un message.

### 2. Fichier `.cabal`

Voici un exemple de fichier `.cabal` configuré pour inclure la dépendance sur le paquet `random`. Supposons que votre projet s'appelle `random-number`.

```cabal
cabal-version:       3.0
name:                random-number
version:             0.1.0.0
synopsis:            A simple program to generate a random number
license:             BSD-3-Clause
author:              Votre Nom
maintainer:          votre.email@example.com
build-type:          Simple

executable random-number
  main-is:             Main.hs
  build-depends:       base ^>= 4.16,
                       random >= 1.2
  hs-source-dirs:      src
  default-language:    Haskell2010
```

**Explications :**
- La dépendance `random >= 1.2` est ajoutée dans la section `build-depends`.
- `base ^>= 4.16` est inclus pour la compatibilité avec la bibliothèque standard de Haskell.
- `main-is: Main.hs` indique que le fichier principal est `Main.hs`.
- `hs-source-dirs: src` suppose que `Main.hs` est dans un dossier `src`.

