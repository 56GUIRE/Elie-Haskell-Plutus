Je vais vous fournir un programme Haskell structuré avec un projet Cabal, incluant les dossiers `src` et `app`, où le module principal est placé dans `app` et les autres modules dans `src`. Le programme démontrera une fonctionnalité simple, par exemple, une fonction dans un module `src` qui est appelée depuis le `main` dans `app`. Pour l'exemple, je vais créer un module qui effectue une opération simple (par exemple, doubler un nombre) et l'utiliser dans le `main`.

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

### 4. Instructions pour exécuter le programme

1. **Créer la structure du projet :**
   - Créez un dossier `random-number`.
   - Créez les sous-dossiers `app` et `src`.
   - Placez les fichiers comme décrit ci-dessus :
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
   - Si vous préférez Stack, initialisez un projet avec :
     ```bash
     stack new random-number
     ```
   - Modifiez `package.yaml` pour inclure :
     ```yaml
     dependencies:
     - base >= 4.7 && < 5
     - random >= 1.2

     library:
       source-dirs: src

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

### Remarques
- **Dépendance `random` :** Le paquet `random` est inclus dans le fichier `.cabal` pour permettre l'utilisation de `randomRIO`. Assurez-vous que votre environnement Haskell est configuré avec Cabal ou Stack.
- **Structure modulaire :** Cette organisation avec `src` pour les modules de bibliothèque et `app` pour le programme principal est typique des projets Haskell. Vous pouvez ajouter d'autres modules dans `src` et les importer dans `Main.hs`.
- **Personnalisation :** Vous pouvez enrichir `MyLib` avec d'autres fonctions ou types personnalisés. Par exemple, vous pourriez ajouter un type `Result` comme dans une requête précédente ou d'autres fonctionnalités.

Si vous avez besoin d'ajouter des modules supplémentaires, d'autres fonctionnalités, ou des explications sur la configuration, faites-le-moi savoir !
