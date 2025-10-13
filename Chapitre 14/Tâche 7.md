Je vais vous fournir un programme Haskell structuré avec un projet Cabal qui inclut à la fois un composant bibliothèque et un exécutable principal. Le composant bibliothèque contiendra une fonction simple (par exemple, générer un nombre aléatoire entre 1 et 100), et l'exécutable principal l'utilisera. Le fichier `.cabal` sera modifié pour supporter cette structure, avec une dépendance sur le paquet `random`.

### Structure du projet
Voici l'organisation du projet :

```
random-number/
├── random-number.cabal
├── app/
│   └── Main.hs
├── src/
│   └── MyLib.hs
```

- Le dossier `src` contient les modules de la bibliothèque.
- Le dossier `app` contient le module principal de l'exécutable.
- Le fichier `.cabal` définit à la fois la bibliothèque et l'exécutable.

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

### 4. Instructions pour exécuter le programme

1. **Créer la structure du projet :**
   - Créez un dossier `random-number`.
   - Créez les sous-dossiers `app` et `src`.
   - Placez les fichiers comme décrit :
     - `random-number.cabal` à la racine.
     - `app/Main.hs`.
     - `src/MyLib.hs`.

2. **Exécution avec Cabal :**
   - Dans le dossier `random-number`, exécutez :
     ```bash
     cabal update
     cabal build
     cabal run random-number
     ```
   - Cela télécharge la dépendance `random`, compile la bibliothèque et l'exécutable, puis exécute le programme. Vous verrez une sortie comme :
     ```
     Nombre aléatoire généré : 42
     ```
     (Le nombre variera à chaque exécution.)

3. **Exécution avec Stack (optionnel) :**
   - Si vous utilisez Stack, initialisez un projet avec :
     ```bash
     stack new random-number
     ```
   - Modifiez `package.yaml` pour refléter la structure :
     ```yaml
     name:                random-number
     version:             0.1.0.0
     synopsis:            A Haskell project with a library and executable
     license:             BSD-3-Clause
     author:              Votre Nom
     maintainer:          votre.email@example.com

     dependencies:
     - base >= 4.7 && < 5
     - random >= 1.2

     library:
       source-dirs: src
       exposed-modules:
       - MyLib

     executables:
       random-number:
         main: Main.hs
         source-dirs: app
         dependencies:
         - random-number
     ```
   - Exécutez :
     ```bash
     stack build
     stack exec random-number
     ```

### Sortie attendue
La sortie sera similaire à :
```
Nombre aléatoire généré : 42
```
(Le nombre change à chaque exécution.)

### Remarques
- **Dépendance `random` :** Le paquet `random` est inclus pour permettre l'utilisation de `randomRIO`. Assurez-vous que votre environnement Haskell (Cabal ou Stack) est configuré correctement.
- **Composant bibliothèque :** La bibliothèque `random-number` peut être réutilisée dans d'autres projets ou par d'autres exécutables dans le même projet. Vous pouvez ajouter d'autres modules dans `src` et les exposer via `exposed-modules`.
- **Structure modulaire :** Séparer la logique (dans `src/MyLib.hs`) de l'exécutable (dans `app/Main.hs`) est une pratique courante pour organiser les projets Haskell.
- **Personnalisation :** Vous pouvez enrichir `MyLib` avec d'autres fonctions, types personnalisés, ou dépendances. Par exemple, vous pourriez intégrer des fonctionnalités des requêtes précédentes (comme `NumericUnderscores` ou `TypeApplications`).

Si vous avez besoin d'ajouter des modules, des fonctionnalités, ou des explications supplémentaires, faites-le-moi savoir !
