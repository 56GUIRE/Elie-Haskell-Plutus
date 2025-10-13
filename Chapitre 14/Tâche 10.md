HC14T10 : Suite de tests Cabal pour counts

### Structure du projet
1. **Module principal** : Contient la fonction `counts`.
2. **Module de tests** : Utilise la bibliothèque de test `HUnit` pour vérifier le bon fonctionnement de `counts`.
3. **Fichier Cabal** : Configure le projet et la suite de tests.
4. **Main** : Un programme simple pour démontrer l'utilisation de `counts`.

### Étape 1 : Code du module principal (`Counts.hs`)
Créez un fichier `src/Counts.hs` avec le code suivant :

```haskell
module Counts where

import Data.List (sort, group)

-- Fonction counts qui retourne la fréquence des caractères
counts :: String -> [(Char, Int)]
counts str = [(head g, length g) | g <- group (sort str)]
```

### Étape 2 : Code du module de tests (`Tests.hs`)
Créez un fichier `test/Tests.hs` avec une suite de tests utilisant `HUnit` :

```haskell
module Main where

import Test.HUnit
import Counts (counts)

-- Tests pour la fonction counts
tests :: Test
tests = TestList [
    TestLabel "Empty string" $
      TestCase $ assertEqual "counts of empty string" [] (counts ""),
    TestLabel "Single character" $
      TestCase $ assertEqual "counts of single character" [('a', 1)] (counts "a"),
    TestLabel "Multiple characters with duplicates" $
      TestCase $ assertEqual "counts of hello" [('e', 1), ('h', 1), ('l', 2), ('o', 1)] (counts "hello"),
    TestLabel "All same characters" $
      TestCase $ assertEqual "counts of aaaa" [('a', 4)] (counts "aaaa"),
    TestLabel "Mixed case and special characters" $
      TestCase $ assertEqual "counts of Aa!a" [('!', 1), ('A', 1), ('a', 2)] (counts "Aa!a")
  ]

-- Main pour exécuter les tests
main :: IO ()
main = do
  countsResult <- runTestTT tests
  putStrLn $ show countsResult
```

### Étape 3 : Fichier Cabal (`counts.cabal`)
Créez un fichier `counts.cabal` à la racine du projet pour configurer le projet et les tests :

```cabal
cabal-version: 1.12
name:           counts
version:        0.1.0.0
build-type:     Simple
license:        BSD3

library
  exposed-modules: Counts
  hs-source-dirs: src
  build-depends:  base >= 4.7 && < 5
  default-language: Haskell2010

executable counts-exe
  main-is: Main.hs
  hs-source-dirs: app
  build-depends:  base >= 4.7 && < 5, counts
  default-language: Haskell2010

test-suite counts-test
  type: exitcode-stdio-1.0
  main-is: Tests.hs
  hs-source-dirs: test
  build-depends:  base >= 4.7 && < 5,
                  counts,
                  HUnit >= 1.6
  default-language: Haskell2010
```

### Étape 4 : Programme principal (`Main.hs`)
Créez un fichier `app/Main.hs` pour démontrer l'utilisation de `counts` :

```haskell
module Main where

import Counts (counts)

main :: IO ()
main = do
  let testString = "hello"
  putStrLn $ "Counts for \"hello\": " ++ show (counts testString)
```

### Étape 5 : Structure du répertoire
Votre répertoire devrait ressembler à ceci :
```
counts/
├── src/
│   └── Counts.hs
├── test/
│   └── Tests.hs
├── app/
│   └── Main.hs
└── counts.cabal
```



### Explications
- **Fonction `counts`** : Comme dans les réponses précédentes, elle calcule la fréquence des caractères dans une chaîne en utilisant `sort` et `group` de `Data.List`.
- **Tests HUnit** :
  - Les tests vérifient différents cas : chaîne vide, caractère unique, caractères avec doublons, caractères identiques, et caractères variés (majuscules, minuscules, spéciaux).
  - Chaque test utilise `assertEqual` pour comparer le résultat de `counts` avec la sortie attendue.
- **Fichier Cabal** :
  - Définit une bibliothèque (`Counts`), un exécutable (`counts-exe`), et une suite de tests (`counts-test`).
  - Inclut `HUnit` comme dépendance pour les tests.
- **Main** :
  - Le programme principal dans `app/Main.hs` montre un exemple d'utilisation de `counts`.
  - Le `main` des tests dans `test/Tests.hs` exécute la suite de tests avec `runTestTT`.

### Notes
- **Dépendances** : Assurez-vous que `HUnit` est disponible. Si nécessaire, ajoutez-le à votre environnement avec `cabal install --lib HUnit`.
- **Extension `PartialTypeSignatures`** : Vous n'avez pas explicitement demandé son utilisation ici, mais si vous voulez l'intégrer, ajoutez `{-# LANGUAGE PartialTypeSignatures #-}` dans `Counts.hs` ou `Tests.hs` et utilisez des signatures comme `counts :: String -> [(_, Int)]`.
- **Personnalisation** : Vous pouvez ajouter plus de tests pour couvrir d'autres cas, comme des chaînes avec des espaces ou des caractères Unicode.
- **Sortie des tests** : La sortie de `cabal test` indique si les tests passent ou échouent, avec des détails en cas d'erreur.

Si vous avez besoin d'ajustements ou de tests supplémentaires, faites-le-moi savoir !
