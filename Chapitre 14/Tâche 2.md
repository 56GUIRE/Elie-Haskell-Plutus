Je vais vous fournir un programme Haskell qui génère un nombre aléatoire entre 1 et 100, avec le code source et les modifications nécessaires pour le fichier `.cabal`.

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

### 3. Instructions pour exécuter le programme

1. **Structure du projet :**
   Créez un dossier pour votre projet avec la structure suivante :
   ```
   random-number/
   ├── random-number.cabal
   ├── src/
   │   └── Main.hs
   ```

2. **Initialisation et exécution :**
   - Assurez-vous que Cabal et GHC sont installés (via GHCup ou Stack).
   - Dans le dossier du projet, exécutez :
     ```bash
     cabal update
     cabal build
     cabal run random-number
     ```
   - Cela téléchargera la dépendance `random`, compilera le programme, et affichera un nombre aléatoire entre 1 et 100, par exemple : `Nombre aléatoire : 42`.

3. **Alternative avec Stack :**
   Si vous utilisez Stack, créez un fichier `stack.yaml` ou initialisez un projet avec `stack new random-number`, puis ajoutez `random` dans la section `dependencies` du fichier `package.yaml` :
   ```yaml
   dependencies:
   - base >= 4.7 && < 5
   - random >= 1.2
   ```
   Ensuite, exécutez :
   ```bash
   stack build
   stack exec random-number
   ```

### Remarques
- Le paquet `random` est une bibliothèque standard pour la génération de nombres aléatoires en Haskell.
- Si vous rencontrez des problèmes avec les versions, vérifiez la compatibilité de `random` avec votre version de GHC via `cabal info random` ou sur Hackage.
- Pour une génération de nombres aléatoires plus complexe, vous pouvez explorer des bibliothèques comme `mwc-random` ou `tf-random`, mais `random` est suffisant ici.

Si vous avez besoin d'aide supplémentaire (par exemple, pour configurer l'environnement ou ajouter d'autres fonctionnalités), faites-le-moi savoir !
