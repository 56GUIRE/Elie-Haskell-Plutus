HC14T2 : Ajouter une dépendance et afficher un nombre aléatoire
1. Fichier app/Main.hs
```haskell
module Main where

import System.Random (randomRIO)

main :: IO ()
main = do
    randomNumber <- randomRIO (1 :: Int, 100 :: Int)
    putStrLn $ "Nombre aléatoire : " ++ show randomNumber
```

#### 2. Fichier `hello-cabal.cabal`
```cabal
cabal-version:      2.4
name:               hello-cabal
version:            0.1.0.0

executable hello-cabal
    main-is:          Main.hs
    build-depends:    base ^>= 4.14,
                      random ^>= 1.2
    hs-source-dirs:   app
    default-language: Haskell2010
```

### Explication détaillée

#### Fichier `app/Main.hs`
- **`module Main where`** :
  - Déclare le module principal nommé `Main`, qui est requis pour un exécutable Haskell. La fonction `main` dans ce module sert de point d'entrée pour le programme.
- **`import System.Random (randomRIO)`** :
  - Importe la fonction `randomRIO` du module `System.Random`, qui fait partie du paquet `random`. Cette fonction est utilisée pour générer un nombre aléatoire dans une plage donnée (ici, 1 à 100).
- **`main :: IO ()`** :
  - La fonction `main` est le point d'entrée du programme. Son type `IO ()` indique qu'elle effectue des opérations d'entrée/sortie (comme générer un nombre aléatoire ou afficher du texte) et ne retourne aucune valeur (`()`).
- **`randomNumber <- randomRIO (1 :: Int, 100 :: Int)`** :
  - `randomRIO` prend un tuple `(a, b)` représentant la plage des nombres aléatoires (inclusivement). Ici, `(1 :: Int, 100 :: Int)` spécifie une plage de 1 à 100, avec des annotations de type `:: Int` pour s'assurer que les bornes sont des entiers.
  - L'opérateur `<-` extrait le résultat de l'action `IO` (le nombre aléatoire) et l'assigne à `randomNumber`.
- **`putStrLn $ "Nombre aléatoire : " ++ show randomNumber`** :
  - `show randomNumber` convertit le nombre aléatoire (de type `Int`) en une chaîne de caractères (`String`).
  - `"Nombre aléatoire : " ++ show randomNumber` concatène le texte `"Nombre aléatoire : "` avec la représentation textuelle du nombre.
  - `putStrLn` affiche la chaîne résultante dans la console, suivie d'un saut de ligne.

#### Fichier `hello-cabal.cabal`
- **`cabal-version: 2.4`** :
  - Spécifie la version du format du fichier `.cabal`. La version 2.4 est standard et compatible avec les outils modernes de Cabal.
- **`name: hello-cabal`** :
  - Définit le nom du projet comme `hello-cabal`, qui est le nom par défaut généré par `cabal init`.
- **`version: 0.1.0.0`** :
  - Indique la version initiale du projet, générée par `cabal init`.
- **`executable hello-cabal`** :
  - Définit une section pour l'exécutable nommé `hello-cabal`, qui sera généré à partir du code source.
- **`main-is: Main.hs`** :
  - Indique que le fichier `Main.hs` (situé dans le dossier spécifié par `hs-source-dirs`) contient la fonction `main`, point d'entrée de l'exécutable.
- **`build-depends: base ^>= 4.14, random ^>= 1.2`** :
  - Liste les dépendances du projet :
    - `base ^>= 4.14` : La bibliothèque standard de Haskell, compatible avec GHC 8.10. La contrainte `^>= 4.14` accepte la version 4.14.x ou toute version mineure compatible.
    - `random ^>= 1.2` : Le paquet `random`, qui fournit le module `System.Random` pour la génération de nombres aléatoires. La version 1.2 est récente et largement compatible.
- **`hs-source-dirs: app`** :
  - Indique que les fichiers source Haskell, comme `Main.hs`, se trouvent dans le dossier `app`.
- **`default-language: Haskell2010`** :
  - Spécifie que le langage utilisé est Haskell 2010, une version standard du langage Haskell.

### Instructions pour exécuter
1. **Configurer le projet** :
   - Si tu n'as pas encore créé le projet, exécute dans un terminal :
     ```bash
     cabal init --non-interactive --exe -p hello-cabal
     ```
     Cela crée la structure du projet avec les dossiers `app` et le fichier `hello-cabal.cabal`.
   - Remplace le contenu de `app/Main.hs` par le code ci-dessus.
   - Remplace le contenu de `hello-cabal.cabal` par le code ci-dessus.

2. **Compiler et exécuter** :
   - Dans le terminal, à la racine du projet (où se trouve `hello-cabal.cabal`), exécute :
     ```bash
     cabal update
     cabal build
     cabal run hello-cabal
     ```
   - `cabal update` : Met à jour l'index des paquets pour inclure `random`.
   - `cabal build` : Compile le projet, en téléchargeant automatiquement la dépendance `random` si nécessaire.
   - `cabal run hello-cabal` : Exécute l'exécutable, affichant un résultat comme :
     ```
     Nombre aléatoire : 42
     ```
     (Le nombre sera différent à chaque exécution, car il est aléatoire.)

